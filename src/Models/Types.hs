module Models.Types 
    ( module Models.Core
    , Subst
    , Kind(..)
    , Type'(..)
    , Type(..)
    , Tyvar(..)
    , Tycon(..)
    , Qual(..)
    , Pred(..)
    , Scheme(..)
    , Assump(..)
    {-, ClassEnv(..)-}
    , Class(..)
    , Inst(..)
    , HasKind(..)
    , Types(..)
    ) where

import Models.Core
import Data.List (nub, union)
import Data.Maybe (fromMaybe)

type Subst = [(Tyvar, Type')]
type Type = Lex Type'

data Type' = TVar Tyvar 
          | TFun [Type'] Type'
          | TCons Tycon
          | TAp Type' Type'
          | TGen Int
    deriving (Eq, Show)

data Kind = Star | KFun Kind Kind
    deriving (Eq, Show)

data Tyvar = Tyvar Id Kind
    deriving (Eq, Show)

data Tycon = Tycon Id Kind
    deriving (Eq, Show)

data Qual t = [Pred] :=> t
    deriving (Eq, Show)

data Pred = IsIn Id [Type']
    deriving (Eq, Show)

data Scheme = Forall [Kind] (Qual Type')
    deriving (Eq, Show)

data Assump = Id :>: Scheme
    deriving (Eq, Show)

type Class = ([Id], [Inst])

type Inst = Qual Pred

class HasKind t where
    kind :: t -> Kind

class Types t where
    apply :: Subst -> t -> t
    tv :: t -> [Tyvar]

instance Types a => Types (Lex a) where
    apply s (Lex p t) = Lex p (apply s t)
    tv (Lex _ t) = tv t

instance Types Type' where
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

instance HasKind Type' where
    kind (TVar tv) = kind tv
    kind (TCons tc) = kind tc
    kind (TAp t _)  = case kind t of
                           (KFun _ k) -> k
