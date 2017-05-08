module Language.Utils where

import Language.Types
import Language.Expressions
import Language.Program
import Data.List (intersect)
import Data.Maybe (isJust)
import Control.Monad (msum, liftM2)
import qualified Data.Map as M

super :: ClassEnv -> Id -> [Id]
super ce i = case M.lookup i ce of 
    Just (Class is _ _) -> is
    _ -> error "Super class not in class env"

insts :: ClassEnv -> Id -> [Inst]
insts ce i = case M.lookup i ce of 
    Just (Class _ ins _) -> map imInst ins
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
          p (Tyvar n k) = apply s1 (TVar n k) == apply s2 (TVar n k)

mgu :: (Monad m, Show a) => a -> Type -> Type -> m Subst
mgu a (TAp l r) (TAp l' r') = do
    s1 <- mgu a l l'
    s2 <- mgu a (apply s1 r) (apply s1 r') 
    return (s2 @@ s1)
mgu a (TVar n k) t = varBind a (Tyvar n k) t
mgu a t (TVar n k) = varBind a (Tyvar n k) t
mgu _ t1@(TCons _ _) t2@(TCons _ _) 
    | t1 == t2 = return nullSubst
mgu e a b = fail $  "Types could not be unified " ++ show a ++ ", "++ show b ++ " In expresssion: " ++ show e

mguPred :: Monad m => Pred -> Pred -> m Subst
mguPred = liftPred (mgu "Pred")

varBind :: (Monad m, Show a) => a -> Tyvar -> Type -> m Subst
varBind a u@(Tyvar n k) t | t == TVar n k = return nullSubst
            | u `elem` tv t = fail $ n ++ " Occurs check failed" ++ show a
            | k /= kind t = fail $ "Kinds do not match " ++ n ++ " " ++ show t ++ " " ++ show a
            | otherwise = return (u +-> t)

match :: Monad m => Type -> Type -> m Subst
match (TAp l r) (TAp l' r') = (match l l' >>= merge) =<< match r r'
match (TVar n k) t | k == kind t = return (Tyvar n k +-> t)
match tc1@(TCons _ _) tc2@(TCons _ _)
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
    where hnf (TVar _ _) = True
          hnf (TCons _ _) = False
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

class Data d where
    dName :: d -> Id

instance Data Adt where
    dName (Adt (_:=>t) _) = getTName t
instance Data Rec where
    dName (Rec (_:=>t) _ _) = getTName t

getTName :: Type -> Id
getTName (TCons n _) = n
getTName (TAp t _) = getTName t
getTName _ = error "getTName applied to something without a constructor"

func :: Type -> Type -> Type
a `func` b = TAp (TAp tArrow a) b

mkFun :: [Type] -> Type -> Type
mkFun ts t = foldr func t ts

blossomAssumps :: [Assump]
blossomAssumps = [
    "+UN"    :>: unary "Num"
    ,"-UN"    :>: unary "Num"
    ,"+"      :>: binary "Num"
    ,"-"      :>: binary "Num"
    ,"/"      :>: binary "Fractional"
    ,"*"      :>: binary "Num"
    ,"//"     :>: binary "Integral"
    ,"%"      :>: binary "Integral"
    ,"<"      :>: relational "Ord"
    ,">"      :>: relational "Ord"
    ,">="     :>: relational "Ord"
    ,"<="     :>: relational "Ord"
    ,"=="     :>: relational "Eq"
    ,"and"    :>: bbool
    ,"or"     :>: bbool
    ,"xor"    :>: bbool
    ,"not"    :>: ubool
    ,"!seq"   :>: Forall [Star,Star] ([] :=> mkFun [g0, g1] g1)
    ,"print"  :>: sc1 ([IsIn "Showable" [g0]] :=> func g0 tNull)
    ,"[nil]"  :>:sc1 ([] :=> tList g0)
    ,"[cons]" :>: sc1 ([] :=> mkFun [g0, tList g0] (tList g0))
    ] ++ tupleAssumps
    where binary q = sc1 ([IsIn q [g0]] :=> mkFun [g0,g0] g0)
          relational q = sc1 $ [IsIn q [g0]] :=> mkFun [g0,g0] tBool
          unary q = sc1 $ [IsIn q [g0]] :=> func g0 g0
          bbool = Forall [] ([]:=> mkFun [tBool,tBool] tBool)
          ubool = Forall [] ([] :=> func tBool tBool)
          g0 = TGen 0
          g1 = TGen 1
          sc1 = Forall [Star]
   
tupleAssumps :: [Assump]
tupleAssumps = map tplAsmp [2..20]
    where tplAsmp n = 
            let ks = replicate n Star
                ts = map TGen [0..n]
                name = tplName n
           in (name :>: Forall ks ([] :=> tTuple ts))
