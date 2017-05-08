-- {-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}
module Language.Bindings 
    ( module Language.Expressions
    , module Language.Types
    , Expr (..) , Literal (..)
    , Pat (..), NamedExpr (..)
    , Alt
    , Stub
    , Expl (..)
    , Impl (..)
    , BindGroup
    , Binding(..)
    , Nameable(..)
    , namedToBinding
    ) where

import Language.Types
import Language.Expressions
    
data Expl = Expl Id Scheme Expr
    deriving Show

data Impl = Impl Id Expr
    deriving Show

class NamedExpr t where
    nname :: t -> Id
    nexpr :: t -> Expr

instance NamedExpr Expl where
    nname (Expl i _ _) = i
    nexpr (Expl _ _ e) = e

instance NamedExpr Impl where
    nname (Impl i _) = i
    nexpr (Impl _ e) = e

type Alt = ([Pat], Expr)
type BindGroup = ([Expl], [[Impl]])
type Stub = (Id, Scheme, [(Scheme, Expr)])

class Nameable a where
    nameOf :: a -> Id

data Binding = ExpB Expl
             | ImpB Impl
    deriving Show

instance NamedExpr Binding where
    nname (ExpB e) = nname e
    nname (ImpB i) = nname i

    nexpr (ExpB e) = nexpr e
    nexpr (ImpB i) = nexpr i

instance Types Expl where
    tv e = tv $ nexpr e
    apply s (Expl i sc e) = Expl i sc $ apply s e

instance Types Impl where
    tv i = tv $ nexpr i
    apply s (Impl i e) = Impl i $ apply s e

instance Types Binding where
    tv (ExpB e) = tv $ nexpr e
    tv (ImpB i) = tv $ nexpr i

    apply s (ExpB e) = ExpB $ apply s e
    apply s (ImpB i) = ImpB $ apply s i

class Prod a where
    prod :: [a] -> a

instance Prod a => Prod (Qual a) where 
    prod qs = let (ps,as) = unzip [(p,a) | p :=> a <- qs]
              in concat ps :=> prod as

instance Prod Pat where 
    prod [as] = as
    prod as = PCons (prodName as) as

instance Prod Expr where 
    prod [as] = as
    prod as = foldl Ap (Var $ prodName as) as

instance Prod Type where 
    prod [t] = t
    prod ts = foldl TAp (TCons (prodName ts) ks) ts
        where ks =  foldr KFun Star (replicate (length ts) Star)

namedToBinding :: Id -> Expr -> Binding
namedToBinding n (Annot (ex :-: qt)) = ExpB $ Expl n (quantAll qt) ex
namedToBinding n ex = ImpB $ Impl n ex

prodName :: [a] -> String
prodName ls = show (length ls) ++ "PROD"
