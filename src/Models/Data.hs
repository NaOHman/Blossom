{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Models.Data 
    ( module Models.Core
    , Stub(..)
    , Binding(..)
    , Data(..)
    , Behavior(..)
    , Implementation(..)
    {-, Instance'(..)-}
    {-, ClassDec'(..)-}
    {-, Data'(..)-}
    )where

import Models.Expressions
import Models.Core
import Models.Types
import Text.Megaparsec.Pos

type Stub = (Id, Scheme)
type Binding = (Id, PExpr, Scheme)

data Data = ADT Scheme [Stub]
          | Rec Scheme (Maybe Type) [Stub]
    deriving Show

data Behavior = Bhvr Scheme Id [Stub]
    deriving Show

data Implementation = Imp Scheme Id [Binding]
    deriving Show
