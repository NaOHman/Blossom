module Models.Types 
    ( module Models.Core
    , Subst
    , Kind(..)
    , Type(..)
    , Tyvar(..)
    , Tycon(..)
    , Qual(..)
    , Pred(..)
    , Scheme(..)
    , Assump(..)
    , Inst(..)
    , HasKind(..)
    , Types(..)
    , Instantiate(..)
    ) where

import Models.Core
import Data.List (nub, union,intercalate)
import Data.Maybe (fromMaybe)

type Subst = [(Tyvar, Type)]

data Type = TVar Tyvar 
          | TCons Tycon
          | TAp Type Type
          | TGen Int
    deriving Eq

data Kind = Star | KFun Kind Kind
    deriving Eq

data Tyvar = Tyvar Id Kind
    deriving Eq

data Tycon = Tycon Id Kind
    deriving (Eq, Show)

data Qual t = [Pred] :=> t
    deriving (Eq, Show)

data Pred = IsIn Id [Type]
    deriving (Eq, Show)

data Scheme = Forall [Kind] (Qual Type)
    deriving (Eq, Show)

data Assump = Id :>: Scheme
    deriving (Eq, Show)

type Inst = Qual Pred

class HasKind t where
    kind :: t -> Kind

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [Tyvar]

{-instance Types a => Types (Lex a) where-}
    {-apply s (Lex p t) = Lex p (apply s t)-}
    {-tv (Lex _ t) = tv t-}

instance Types Type where
    apply s (TVar u) = fromMaybe (TVar u) (lookup u s)
    apply s (TAp l r) = TAp (apply s l) (apply s r)
    apply s t = t

    tv (TVar u) = [u]
    tv (TAp l r) = tv l `union` tv r
    tv t = []

instance Types t => Types (Qual t) where
    apply s (ps:=>t) = apply s ps :=> apply s t
    tv (ps:=>t) = tv ps `union` tv t

instance Types Pred where
    apply s (IsIn i t) = IsIn i (apply s t)
    tv (IsIn i t) = tv t

instance Types a => Types [a] where
    apply s = map (apply s)
    tv = nub . concatMap tv

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    tv (Forall _ qt) = tv qt

instance Types Assump where
    apply s (id:>:sc) = id :>: apply s sc
    tv (id:>:sc) = tv sc

instance HasKind a => HasKind (Lex a) where
    kind (Lex _ a) = kind a

instance HasKind Tyvar where
    kind (Tyvar _ k) = k
 
instance HasKind Tycon where
    kind (Tycon _ k) = k

instance HasKind Type where
    kind (TVar tv) = kind tv
    kind (TCons tc) = kind tc
    kind (TAp t _)  = case kind t of
                           (KFun _ k) -> k

instance Ord Tyvar where
    (Tyvar v1 k1) <= (Tyvar v2 k2) 
        | v1 == v2 = k1 <= k2
        | otherwise = v1 <= v2

instance Ord Kind where
    Star <= Star = True
    (KFun _ _ ) <= Star = True
    Star <= (KFun _ _ ) = False
    (KFun _ k1) <= (KFun _ k2) = k1 >= k2

instance Show Kind where 
    show Star = "*"
    show (KFun k1 k2) = show k1 ++ " -> " ++ show k2

instance Show Type where
    show (TAp (TAp (TCons (Tycon "->" _)) t1) t2) = 
        show t1 ++ " -> " ++ show t2
    show (TAp t1 t2) = let (i,ts) = bottom t1 [t2]
                       in i ++ "<" ++ intercalate "," (map show ts) ++ ">"

    show (TCons (Tycon i _ )) = i
    show (TVar (Tyvar i _)) = i
    show (TGen i) = "G" ++ show i

instance Show Tyvar where
    show (Tyvar v k) = v ++ "(" ++ show k ++ ")"

class Instantiate t where
    inst :: [Type] -> t -> t

instance Instantiate Type where
    inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
    inst ts (TGen n)  = ts !! n
    inst ts t         = t
instance Instantiate a => Instantiate [a] where
    inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
    inst ts (ps:=>t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
    inst ts (IsIn c t) = IsIn c (inst ts t)


bottom (TAp t1 t2) ts = bottom t1 (t2:ts)
bottom t ts = (show t, ts)
