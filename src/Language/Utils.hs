module Language.Utils where

import Language.Types
{-import LangDef.Blossom-}
import Language.Program
import Data.List (intersect)
import Data.Maybe (isJust)
import Control.Monad (msum, liftM2)
import qualified Data.Map as M

super :: ClassEnv -> Id -> [Id]
super ce i = case M.lookup i ce of 
    Just (is,_,_) -> is
    _ -> error "Super class not in class env"

insts :: ClassEnv -> Id -> [Inst]
insts ce i = case M.lookup i ce of 
    Just (_,ins,_) -> ins
    _ -> error "Instance not in class env"

defined :: Maybe a -> Bool
defined = isJust

enumId :: Int -> String
enumId n = "..v" ++ show n

nullSubst :: Subst
nullSubst = []

(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u,t)]

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u,t) <- s2] ++ s1

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree 
                then return (s1 ++ s2) else fail "merge failed"
    where agree = all p (map fst s1 `intersect` map fst s2)
          p v = apply s1 (TVar v) == apply s2 (TVar v)

mgu :: (Monad m, Show a) => a -> Type -> Type -> m Subst
mgu a (TAp l r) (TAp l' r') = do
    s1 <- mgu a l l'
    s2 <- mgu a (apply s1 r) (apply s1 r') 
    return (s2 @@ s1)
mgu a (TVar u) t = varBind a u t
mgu a t (TVar u) = varBind a u t
mgu _ (TCons t1) (TCons t2) 
    | t1 == t2 = return nullSubst
mgu e a b = fail $  "Types could not be unified " ++ show a ++ ", "++ show b ++ " In expresssion: " ++ show e

mguPred :: Monad m => Pred -> Pred -> m Subst
mguPred = liftPred (mgu "Pred")

varBind :: (Monad m, Show a) => a -> Tyvar -> Type -> m Subst
varBind a u@(Tyvar i _) t | t == TVar u = return nullSubst
            | u `elem` tv t = fail $ i ++ " Occurs check failed" ++ show a
            | kind u /= kind t = fail $ "Kinds do not match " ++ show u ++ " " ++ show t ++ " " ++ show a
            | otherwise = return (u +-> t)

match :: Monad m => Type -> Type -> m Subst
match (TAp l r) (TAp l' r') = (match l l' >>= merge) =<< match r r'
match (TVar u) t | kind u == kind t = return (u +-> t)
match (TCons tc1) (TCons tc2)
    | tc1 == tc2 = return nullSubst
match t1 t2 = fail $ "Types do not match " ++ show t1 ++ ", " ++ show t2

matchPred :: Monad m => Pred -> Pred -> m Subst
matchPred = liftPred match

liftPred :: Monad m => (Type -> Type -> m Subst) -> Pred -> Pred -> m Subst
liftPred m (IsIn i ts) (IsIn i' ts')
    | i == i' = subUnion <$> foldFail ts ts'
    | otherwise = fail "Classes Differ"
    where foldFail (x:xs) (y:ys) = liftM2 (:) (m x y) (foldFail xs ys)
          foldFail []     []     = return []
          foldFail _      _      = fail "Classes have different numbers of parameters"

subUnion :: [Subst] -> Subst
subUnion = foldl (@@) nullSubst

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) = p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i _) = msum [tryInst it | it <- insts ce i]
    where tryInst (ps :=> h) = do u <- matchPred h p
                                  Just (map (apply u) ps)

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
    case byInst ce p of
        Nothing -> False
        Just qs -> all (entail ce ps) qs

simplify   :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
    where loop rs [] = rs
          loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
                         | otherwise = loop (p:rs) ps

reduce :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)

elimTauts :: ClassEnv -> [Pred] -> [Pred]
elimTauts ce ps = [ p | p <- ps, not (entail ce [] p) ]

scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)


--context reduction garbage
isInHnf :: Pred -> Bool
isInHnf (IsIn _ ts) = all hnf ts
    where hnf (TVar _) = True
          hnf (TCons _) = False
          hnf (TAp t _) = hnf t
          hnf _ = error "Generic Types have no HNF"

toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | isInHnf p = return [p]
           | otherwise = case byInst ce p of
                            Nothing -> fail $ "Context reduction failed " ++show p
                            Just ps -> toHnfs ce ps
           
quantQual :: [Pred] -> Type -> Scheme
quantQual ps t = quantify (tv t) (ps :=> t)

quantAll :: Qual Type -> Scheme
quantAll t = quantify (tv t) t

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
    where vs' = [v | v <- tv qt , v `elem` vs]
          ks = map kind vs'
          s = zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

find :: Monad m => Id -> [Assump] -> m Scheme
find i ((i':>:sc) : as) | i == i' = return sc
                        | otherwise = find i as
find i _ = fail $ "Unbound variable " ++ show i

assume :: [Tyvar] -> Id -> Type -> Assump
assume tv' i t = i :>: quantify tv' ([] :=> t)

mkQualFn :: [Qual Type] -> Qual Type -> Qual Type
mkQualFn qts qt = foldr qualFn qt qts

qualFn :: Qual Type -> Qual Type -> Qual Type
qualFn (q1 :=> t1) (q2 :=> t2) = (q1 ++ q2) :=> (t1 `func` t2)

mkCons :: Id -> Kind -> Type
mkCons n k = TCons $ Tycon n k

mkVar :: Id -> Kind -> Type
mkVar n k = TVar $ Tyvar n k

class Data d where
    dQual ::  d -> [Pred]
    dTCons :: d -> Tycon
    dCstrs :: d -> [(Id, Scheme)]
    dNames :: d -> [Id]

instance Data Adt where
    dQual = aqual
    dTCons (Adt _ t _) = getCons t
    dNames (Adt _ t cs) = let (Tycon n _) = getCons t
                              cNames = map fst cs
                          in n : cNames
    dCstrs (Adt q t cs) = map toCstr cs
         where toCstr (n,[]) = (n, quantQual q t)
               toCstr (n,ts) = (n, quantQual q $ ts `mkFun` t)

getCons :: Type -> Tycon
getCons (TCons t) = t
getCons (TAp t _) = getCons t
getCons _ = error "getCons applied to something without a constructor"

instance Data Rec where
    dQual = rqual
    dTCons (Rec _ t _ _)= getCons t
    dNames (Rec _ t _ _)= let (Tycon n _) = getCons t
                          in [n]
    dCstrs (Rec q t _ fs) = 
        let (Tycon n _) = getCons t
            ts = map snd fs
            ft = if null ts then t else ts `mkFun` t
                
        in [(n, quantQual q ft)]

func :: Type -> Type -> Type
a `func` b = TAp (TAp tArrow a) b

mkFun :: [Type] -> Type -> Type
mkFun ts t = foldr func t ts
