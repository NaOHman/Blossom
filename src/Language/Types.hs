module Language.Types 
    ( module Language.Core
    , Subst
    , Kind(..)
    , Type(..)
    , Tyvar(..)
    , Tycon(..)
    , Qual(..)
    , Pred(..)
    , Scheme(..)
    , Assump(..)
    , Inst
    , HasKind(..)
    , Types(..)
    , Instantiate(..)
    , tChar
    , tUnit
    , tInt
    , tFloat
    , tBool
    , tNull
    , tArrow
    , tTuple
    , tList
    , tString
    , tplName
    ) where

import Language.Core
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
    apply _ t = t

    tv (TVar u) = [u]
    tv (TAp l r) = tv l `union` tv r
    tv _ = []

instance Types t => Types (Qual t) where
    apply s (ps:=>t) = apply s ps :=> apply s t
    tv (ps:=>t) = tv ps `union` tv t

instance Types Pred where
    apply s (IsIn i t) = IsIn i (apply s t)
    tv (IsIn _ t) = tv t

instance Types a => Types [a] where
    apply s = map (apply s)
    tv = nub . concatMap tv

instance Types Scheme where
    apply s (Forall ks qt) = Forall ks (apply s qt)
    tv (Forall _ qt) = tv qt

instance Types Assump where
    apply s (i:>:sc) = i :>: apply s sc
    tv (_:>:sc) = tv sc

instance HasKind Tyvar where
    kind (Tyvar _ k) = k
 
instance HasKind Tycon where
    kind (Tycon _ k) = k

instance HasKind Type where
    kind (TVar v) = kind v
    kind (TCons tc) = kind tc
    kind (TAp t _)  = case kind t of
                           (KFun _ k) -> k
                           _ -> error "Bad function application"
    kind _ = error "Tried to find the kind of a Generic Type"

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
    inst _ t         = t
instance Instantiate a => Instantiate [a] where
    inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
    inst ts (ps:=>t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
    inst ts (IsIn c t) = IsIn c (inst ts t)


bottom :: Type -> [Type] -> (String, [Type])
bottom (TAp t1 t2) ts = bottom t1 (t2:ts)
bottom t ts = (show t, ts)
 
tChar :: Type
tChar = tcons "Char" Star

tArrow :: Type
tArrow = TCons $ Tycon "->" (KFun Star (KFun Star Star))

tInt :: Type
tInt = tcons "Int" Star

tFloat :: Type
tFloat = tcons "Float" Star

tBool :: Type
tBool = tcons "Bool" Star

tNull :: Type
tNull = tcons "Null" Star

tUnit :: Type
tUnit = tcons "()" Star

tList :: Type -> Type
tList = TAp (tcons "List" (KFun Star Star))

tString :: Type
tString = tList tChar

tplName :: Int -> String 
tplName n = "(" ++ replicate (n-1) ',' ++ ")"

tcons :: Id -> Kind -> Type
tcons n k = TCons $ Tycon n k

tTuple :: [Type] -> Type
tTuple ts = 
    let len = length ts
        name = tplName len 
        ks = kAry len
        tc = tcons name ks
    in foldl TAp tc ts

kAry :: Int -> Kind
kAry 0 = Star
kAry n = KFun Star (kAry (n-1))
