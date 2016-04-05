{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}
module Models.Expressions 
    ( module Models.Core
    , module Models.Types
    {-, Lex (..)-}
    {-, PExpr' (..)-}
    {-, Expr' (..)-}
    {-, Literal' (..)-}
    {-, ArgDec' (..)-}
    {-, Arg' (..)-}
    {-, Pat' (..)-}
    , PExpr (..)
    , Expr (..)
    , Literal (..)
    , Pat (..)
    , Alt(..)
    , Expl(..)
    , Impl(..)
    , BindGroup(..)
    , Nameable(..)
    , Prod(..)
    ) where

import Models.Core
import Models.Types
import Parser.Core
import qualified Data.Map.Strict as M
import qualified Data.Set as S
    
type Expl = (Id, Scheme, Expr)
type Impl = (Id, Expr)
type Alt = (Pat, Expr)
type BindGroup = ([Expl], [Impl])

{-type PExpr = Lex PExpr'-}
{-type Expr = Lex Expr'-}
{-type Literal = Lex Literal'-}
{-type ArgDec = Lex ArgDec'-}
{-type Arg = Lex Arg'-}
{-type Pat = Lex Pat'-}

class Nameable a where
    nameOf :: a -> Id

data PExpr = ELit Literal
           | ECons Id [PExpr]
           | EVar  Id
           | EAbs  [(Id, Maybe Type)] PExpr
           | EAp   PExpr [PExpr]
           | ELet  [(Pat,PExpr)] PExpr
           | ECase PExpr [(Pat,PExpr)]
           | EAnnot PExpr Type
           | EUnit
    deriving Show

data Expr = Lit Literal
          | Var Id
          | Let Id Expr Expr
          | Ap Expr Expr
          | Abs Pat Expr
          | Case Expr [Alt] 
    deriving Show

data Literal = LChar Char
             | LInt    Integer
             | LFloat  Double
             {-| LCons   Id [Literal']-}
             {-| LDict   [(a,a)]-}
             {-| LSet    [a]-}
             | LType   Id
             | LNull
    deriving Show

data Pat = PCons Id [Pat]
         | PAs   Id Pat
         | PLit  Literal
         | PVar  Id
         | PNil
    deriving Show

class Prod a where
    prod :: [a] -> a

instance Nameable a => Nameable (Lex a) where
    nameOf = nameOf . unwrap

instance Prod Pat where 
    prod as = PCons (prodName as) as

instance Prod Expr where 
    prod as = foldl Ap (Var $ prodName as) as

instance Prod Type where 
    prod ts = foldl TAp (TCons $ Tycon (prodName ts) ks) ts
        where ks =  foldr KFun Star (replicate (length ts) Star)

{-lProd as = LCons (prodName as) as-}
prodName :: [a] -> String
prodName ls = show (length ls) ++ "PROD"
