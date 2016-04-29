{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}
module Models.Expressions 
    ( module Models.Core
    , module Models.Types
    , Expr (..) , Literal (..)
    , Pat (..)
    , Alt
    , Stub
    , Expl
    , Impl
    , Class
    , BindGroup
    , Binding(..)
    , Nameable(..)
    ) where

import Models.Core
import Models.Types
import Data.List (intercalate)
import Control.Arrow (second)
    
type Expl = (Id, Scheme, Expr)
type Impl = (Id, Expr)

type Alt = ([Pat], Expr)
type BindGroup = ([Expl], [[Impl]])
type Stub = (Id, Scheme, [(Scheme, Expr)])
type Class = ([Id], [Inst], [Stub])

class Nameable a where
    nameOf :: a -> Id

data Binding = Expl Expl
             | Impl Impl
    deriving Show

data Expr = Lit Literal
          | Var  Id
          | Abs  Alt
          | Ap   Expr Expr
          | Let  [Binding] Expr
          | Case Expr [Alt]
          | Annot Expr Scheme
          | Over Id Type [(Scheme, Expr)]

instance Types Binding where
    tv (Expl (_,_,e)) = tv e
    tv (Impl (_,e)) = tv e

    apply s (Expl (i,sc,e)) = Expl (i,sc, apply s e)
    apply s (Impl (i,e)) = Impl (i, apply s e)

instance Types Expr where
    tv (Abs (_,e)) = tv e
    tv (Let bg e) = tv bg ++ tv e
    tv (Ap e1 e2) = tv e1 ++ tv e2
    tv (Case e as) = tv e ++ concatMap (tv . snd) as
    tv (Annot e _) = tv e 
    tv (Over _ (TVar t) _) = [t] 
    tv _ = []

    apply s (Abs (p,e)) = Abs (p, apply s e)
    apply s (Let bg e) = Let (apply s bg) (apply s e)
    apply s (Ap e1 e2) = Ap (apply s e1) (apply s e2)
    apply s (Case e as) = Case (apply s e) $ map (second (apply s)) as
    apply s (Annot e sc) = Annot (apply s e) sc
    apply s (Over i t e) = Over i (apply s t) e
    apply _ e = e


instance Show Expr where
    show (Lit l) = show l
    show (Var v) = "{" ++ v ++ "}"
    show (Abs a) = showAlt a
    show (Ap e1 e2) = "(AP" ++ show e1 ++  " " ++ show e2 ++ ")"
    show (Case e as) =  "Case " ++ show e ++ " of" ++ indentedAlt as
    show (Annot e s) = show e ++ " : " ++ show s
    show (Let bg ex) = show "Let " ++ show bg ++ " in " ++ show ex
    show (Over i t os)   = show "Overload " ++ i ++ " :" ++ show t ++ ":" ++ concatMap (\(qt,e) -> "\n  " ++ show qt ++  " => " ++ show e) os


showAlt :: Alt -> String
showAlt (p,ex) = "(" ++ show p ++ ")" ++ " -> " ++ show ex

indentedAlt :: [Alt] -> String
indentedAlt = concatMap (\a -> "\n   " ++ showAlt a)

data Literal = LChar Char
             | LInt    Integer
             | LFloat  Double
             | LBool  Bool
             | LNull

instance Show Literal where
    show (LChar  c) = show c
    show (LInt   i) = show i
    show (LFloat d) = show d
    show (LBool b) = show b
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
    show (PAs v p) = "[" ++ v ++ "#" ++ show p ++ "]"

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
    prod ts = foldl TAp (TCons $ Tycon (prodName ts) ks) ts
        where ks =  foldr KFun Star (replicate (length ts) Star)

prodName :: [a] -> String
prodName ls = show (length ls) ++ "PROD"
