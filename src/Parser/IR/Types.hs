module Parser.IR.Types
    ( Type (..)
    , Pred(..)
    ) where

import Parser.IR.Pretty

data Type = Given [Pred] Type
          | TFunc [Type] Type
          | TPoly String [Type]
          | TMono String
          | TUnit
          | TList Type
          | TTuple [Type]
    deriving Eq

data Pred = IsIn String [Type]
    deriving Eq

instance Show Type where
    show = pretty

instance Pretty Type where
    pp i (Given ps t) = block i ps ++ " " ++ pp i t
    pp i (TFunc as rt) = "<" ++ csl i as ++ "> -> " ++ pp i rt
    pp i (TPoly n as) =  n ++ "<" ++ csl i as ++ ">"
    pp _ (TMono n) =  n
    pp _ TUnit =  "()"
    pp i (TList t) = "[" ++ pp i t ++ "]"
    pp i (TTuple ts) = "(" ++ csl i ts ++ ")"

instance Show Pred where
    show = pretty

instance Pretty Pred where
    pp i (IsIn bhvr ts) = "Given " ++ csl i ts ++ " in " ++ bhvr ++ "."
