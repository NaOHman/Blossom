module Parser.IR.Literals
    ( Literal (..)
    ) where

data Literal = LChar Char
             | LInt Integer
             | LFloat Double
             | LBool Bool
             | LString String
             | LNull
    deriving Eq
