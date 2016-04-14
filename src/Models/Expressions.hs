{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}
module Models.Expressions 
    ( module Models.Core
    , module Models.Types
    , Expr (..)
    , Literal (..)
    , Pat (..)
    , Alt(..)
    , Expl(..)
    , Impl(..)
    , OBind(..)
    , Class(..)
    , BindGroup(..)
    , Binding(..)
    , Nameable(..)
    , Prod(..)
    ) where

import Models.Core
import Models.Types
import Parser.Core
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate)
    
type Expl = (Id, Scheme, Expr)
type Impl = (Id, Expr)

data OBind = OBind Id (Type -> Type) Expr

instance Show OBind where
    show (OBind i _ e) = show (i,e)

type Alt = (Pat, Expr)
type BindGroup = ([Expl], [Impl])
type Class = ([Id], [Inst], [OBind])

class Nameable a where
    nameOf :: a -> Id

data Binding = Expl Expl
             | Impl Impl
    deriving Show

data Expr = Lit Literal
          | Var  Id
          | Abs  Alt
          | Ap   Expr Expr
          | Let  BindGroup Expr
          | Case Expr [Alt]
          | Annot Expr Scheme
          | Over [(Qual Type, Expr)]

instance Show Expr where
    show (Lit l) = show l
    show (Var v) = "{" ++ v ++ "}"
    show (Abs a) = showAlt a
    show (Ap e1 e2) = show e1 ++  "(" ++ show e2 ++ ")"
    show (Case e as) =  "Case " ++ show e ++ " of" ++ indentedAlt as
    show (Annot e s) = show e ++ " : " ++ show s
    show (Let bg ex) = show "Let " ++ showBG bg ++ " in " ++ show ex
    show (Over os)   = show "Overload " ++ concatMap (\(qt,e) -> "\n  " ++ show qt ++  " => " ++ show e) os

showBG (es, is) = indented es ++  indented is
showAlt (p,ex) = "(" ++ show p ++ ")" ++ " -> " ++ show ex

indentedAlt = concatMap (\a -> "\n   " ++ showAlt a)
indented :: Show a => [a] -> String
indented = concatMap (\a -> "\n   " ++ show a)

data Literal = LChar Char
             | LInt    Integer
             | LFloat  Double
             | LNull

instance Show Literal where
    show (LChar  c) = show c
    show (LInt   i) = show i
    show (LFloat d) = show d
    show LNull      = "Null"

data Pat = PCons Id [Pat]
         | PAs   Id Pat
         | PLit  Literal
         | PVar  Id
         | PNil

instance Show Pat where
    show PNil     = "[_]"
    show (PLit l) = "[" ++ show l ++ "]"
    show (PVar v) = "[" ++ v ++ "]"
    show (PCons v ps) = "[" ++ v ++ " " ++ subPats ++ "]"
        where subPats = intercalate ", " $ map show ps

class Prod a where
    prod :: [a] -> a

instance Prod a => Prod (Qual a) where 
    prod qs = let (ps,as) = unzip [(p,a) | p :=> a <- qs]
              in concat ps :=> prod as

instance Nameable a => Nameable (Lex a) where
    nameOf = nameOf . unwrap

instance Prod Pat where 
    prod [as] = as
    prod as = PCons (prodName as) as

instance Prod Expr where 
    prod [as] = as
    prod as = foldl Ap (Var $ prodName as) as

instance Prod Type where 
    prod [t] = t
    prod ts = foldl TAp (TCons $ Tycon (prodName ts) ks) ts
        where ks =  foldr KFun Star (replicate (length ts) Star)

{-lProd as = LCons (prodName as) as-}
prodName :: [a] -> String
prodName ls = show (length ls) ++ "PROD"
