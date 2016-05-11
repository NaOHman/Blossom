module Language.Expressions 
    ( module Language.Core
    , module Language.Types
    , Expr (..) , Literal (..)
    , Pat (..)
    , Expl(..)
    , Class(..)
    , BindGroup
    , Annotation(..)
    , Implementation(..)
    , Bind(..)
    , explName
    , imInst
    ) where

import Language.Core
import Language.Types
import Data.List (intercalate)
import Control.Arrow (second)
    
type BindGroup = ([Expl], [[Bind]])

data Expl = Expl Id Annotation
    deriving Show

data Implementation = Im Inst [Expl]
    deriving Show

data Class = Class [Id] [Implementation] [Id]

data Bind = Bind Id Expr
    deriving Show

data Expr = Lit Literal
          | Var  Id
          | Abs  [Pat] Expr
          | Ap   Expr Expr
          | Let  [Bind] Expr
          | Case Expr [(Pat, Expr)]
          | Annot Annotation
          | Over Id Type [(Scheme, Expr)]

data Annotation = Expr :-: Scheme
    deriving Show

instance Types Bind where
    tv (Bind _ e) = tv e
    apply s (Bind i e) = Bind i (apply s e)

instance Types Expr where
    tv (Abs _ e) = tv e
    tv (Let bg e) = tv bg ++ tv e
    tv (Ap e1 e2) = tv e1 ++ tv e2
    tv (Case e as) = tv e ++ concatMap (tv . snd) as
    tv (Annot (e:-:_)) = tv e 
    tv (Over _ (TVar t) _) = [t] 
    tv _ = []

    apply s (Abs ps e) = Abs ps (apply s e)
    apply s (Let bg e) = Let (apply s bg) (apply s e)
    apply s (Ap e1 e2) = Ap (apply s e1) (apply s e2)
    apply s (Case e as) = Case (apply s e) $ map (second (apply s)) as
    apply s (Annot (e:-:sc)) = Annot (apply s e :-: sc)
    apply s (Over i t e) = Over i (apply s t) e
    apply _ e = e


instance Show Expr where
    show (Lit l) = show l
    show (Var v) = "{" ++ v ++ "}"
    show (Abs ps ex) = show ps ++ " -> " ++ show ex
    show (Ap e1 e2) = "(AP" ++ show e1 ++  " " ++ show e2 ++ ")"
    show (Case e as) =  "Case " ++ show e ++ " of" ++ concatMap show as
    show (Annot (e :-: s)) = show e ++ " : " ++ show s
    show (Let bg ex) = show "Let " ++ show bg ++ " in " ++ show ex
    show (Over i t os)   = show "Overload " ++ i ++ " :" ++ show t ++ ":" ++ concatMap (\(qt,e) -> "\n  " ++ show qt ++  " => " ++ show e) os

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

explName :: Expl -> Id
explName (Expl i _) = i

imInst :: Implementation -> Inst
imInst (Im i _) = i

prodName :: [a] -> String
prodName ls = show (length ls) ++ "PROD"
