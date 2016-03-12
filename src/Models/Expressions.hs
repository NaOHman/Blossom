{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}
module Models.Expressions 
    ( module Models.Core
    , module Models.Types
    , Lex (..)
    , Expr' (..)
    , Expr (..)
    , Literal' (..)
    , Literal (..)
    , ArgDec' (..)
    , ArgDec (..)
    , Arg' (..)
    , Arg (..)
    , Pat' (..)
    , Pat (..)
    , Alt(..)
    , Expl(..)
    , Impl(..)
    , BindGroup(..)
    , Nameable(..)
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
type Expr = Lex Expr'
type Literal = Lex Literal'
type ArgDec = Lex ArgDec'
type Arg = Lex Arg'
type Pat = Lex Pat'

class Nameable a where
    nameOf :: a -> Id

-- TODO remove ArgDec, change EAbs to 'EAbs Alt'
data Expr' = ELit Literal
           | EVar  Id
           | EAbs  [ArgDec] Expr
           | EAp   Expr [Arg]
           | ELet  Id Expr
           | ECase Expr [Alt]
           | EAnnot Expr Type
           | EUnit
    deriving Show

data Literal' = LChar Char
              | LInt    Integer
              | LFloat  Double
              | LDict   [(Expr,Expr)]
              | LSet    [Expr]
              | LCons   Id [Expr] --constructors are functions too
              | LType   Id
              | LNull
    deriving Show

instance Nameable a => Nameable (Lex a) where
    nameOf = nameOf . unwrap

instance Nameable ArgDec' where
    nameOf (PDec n _) = n
    nameOf (KDec n _ _) = n
    
data ArgDec' = PDec Id (Maybe Type) 
             | KDec Id Literal (Maybe Type)
    deriving Show

data Arg' = PArg Expr | KArg Id Expr
    deriving Show

data Pat' = PCons Id [Pat']
          | PAs   Id Pat'
          | PLit  Literal
          | PVar  Id
          | PNil
    deriving Show
