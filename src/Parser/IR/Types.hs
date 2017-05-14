module Parser.IR.Types
    ( Type (..)
    , Pred(..)
    ) where

data Type = Given [Pred] Type
          | TFunc [Type] Type
          | TPoly String [Type]
          | TMono String
          | TUnit
          | TList Type
          | TTuple [Type]

data Pred = IsIn String [Type]
