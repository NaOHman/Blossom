module Types.Utils where

import Models.Types
import Models.Program
import Models.Expressions
import Text.Megaparsec
import Data.List (nub, union, intersect)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (msum, liftM, ap, liftM2)
import qualified Data.Map as M

super :: ClassEnv -> Id -> [Id]
super ce i = case M.lookup i ce of Just (is,_) -> is

insts :: ClassEnv -> Id -> [Inst]
insts ce i = case M.lookup i ce of Just (_,ins) -> ins

defined = isJust

enumId n = "..v" ++ show n

nullSubst :: Subst
nullSubst = []

(+->) :: Tyvar -> Type' -> Subst
u +-> t = [(u,t)]

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u,t) <- s2] ++ s1

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree 
                then return (s1 ++ s2) else fail "merge failed"
    where agree = all p (map fst s1 `intersect` map fst s2)
          p v = apply s1 (TVar v) == apply s2 (TVar v)

mgu :: Monad m => Type' -> Type' -> m Subst
mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r') 
    return (s2 @@ s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCons t1) (TCons t2) 
    | t1 == t2 = return nullSubst
mgu a b = fail $  "Types could not be unified " ++ show a ++ ", "++ show b

mguPred :: Monad m => Pred -> Pred -> m Subst
mguPred = liftPred mgu

varBind :: Monad m => Tyvar -> Type' -> m Subst
varBind u@(Tyvar i _) t | t == TVar u = return nullSubst
            | u `elem` tv t = fail $ i ++ " Occurs check failed"
            | kind u /= kind t = fail "Kinds do not match"
            | otherwise = return (u +-> t)

match :: Monad m => Type' -> Type' -> m Subst
match (TAp l r) (TAp l' r') = (match l l' >>= merge) =<< match r r'
match (TVar u) t | kind u == kind t = return (u +-> t)
match (TCons tc1) (TCons tc2)
    | tc1 == tc2 = return nullSubst
match t1 t2 = fail $ "Types do not match " ++ show t1 ++ ", " ++ show t2

matchPred :: Monad m => Pred -> Pred -> m Subst
matchPred = liftPred match

liftPred :: Monad m => (Type' -> Type' -> m Subst) -> Pred -> Pred -> m Subst
liftPred m (IsIn i ts) (IsIn i' ts')
    | i == i' = subUnion <$> foldFail ts ts'
    | otherwise = fail "Classes Differ"
    where foldFail (x:xs) (y:ys) = liftM2 (:) (m x y) (foldFail xs ys)
          foldFail []     []     = return []
          foldFail _      _      = fail "Classes have different numbers of parameters"

subUnion = foldl (@@) nullSubst

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) = 
    p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [tryInst it | it <- insts ce i]
    where tryInst (ps:=>h) = do u <- matchPred h p
                                Just (map (apply u) ps)

--does a list of predicates imply this predicate?
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = scEntail ce ps p || case byInst ce p of
                          Nothing -> False
                          Just qs -> all (entail ce ps) qs

scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

--context reduction garbage
isInHnf :: Pred -> Bool
isInHnf (IsIn _ ts) = all hnf ts
    where hnf (TVar _) = True
          hnf (TCons _) = False
          hnf (TAp t _) = hnf t

toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | isInHnf p = return [p]
           | otherwise = case byInst ce p of
                            Nothing -> fail "Context reduction failed"
                            Just ps -> toHnfs ce ps

-- takes a list of predicates and removes all unnecessary ones
-- note that this isn't quite a fold
simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
    where loop rs [] = rs 
          loop rs (p:ps) | entail ce (rs ++ ps) p = loop rs ps
                         | otherwise = loop (p:rs) ps

-- put a context into the simplest form
reduceCtx :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduceCtx ce ps = do qs <- toHnfs ce ps
                     return (simplify ce qs)
            
quantify :: [Tyvar] -> Qual Type' -> Scheme
quantify vs qt = Forall ks (apply s qt)
    where vs' = [v | v <- tv qt , v `elem` vs]
          ks = map kind vs'
          s = zip vs' (map TGen [0..])

toScheme :: Type' -> Scheme
toScheme t = Forall [] ([] :=> t)

find :: Monad m => Id -> [Assump] -> m Scheme
find i [] = fail $ "Unbound variable " ++ show i
find i ((i':>:sc) : as) | i == i' = return sc
                        | otherwise = find i as

kGen :: Id -> Int -> Type'
kGen id k = foldl tAp' (TCons $ Tycon id (kAry k)) [0..k]
    where tAp' t i = TAp t (TGen i)

a `func` b = TAp (TAp tArrow a) b
tUnit = TCons (Tycon "()" Star)
tNull = TCons (Tycon "Null" Star)
tType = TCons (Tycon "Type" Star)
tString = TAp tList tChar
tChar = TCons (Tycon "Char" Star)
tInt = TCons (Tycon "Int" Star)
tBool = TCons (Tycon "Bool" Star)
tFloat = TCons (Tycon "Float" Star)
tList = TCons (Tycon "[]" (KFun Star Star))
tArrow = TCons (Tycon "->" (KFun Star (KFun Star Star)))
tcons n k = TCons (Tycon n k)

tProduct :: [Type'] -> Type'
tProduct ps = foldl TAp (TCons $ Tycon name (kAry len)) ps
    where name = show len ++ "PROD"
          len = length ps

assume :: [Tyvar] -> Id -> Type' -> Assump
assume tv id t = id :>: quantify tv ([] :=> t)

mkFun ts t = tProduct (map unwrap ts) `func` t
mkFun' ts t = tProduct ts `func` t

kAry 0 = Star
kAry n = KFun Star (kAry (n-1))
