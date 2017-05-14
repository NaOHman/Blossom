module Parser.IR.Literals
    ( Literal (..)
    ) where

import Parser.IR.Pretty

data Literal = LChar Char
             | LInt Integer
             | LFloat Double
             | LBool Bool
             | LString String
             | LNull
    deriving Eq

instance Show Literal where
    show = pretty

instance Pretty Literal where
    pp _ (LChar c) = c:""
    pp _ (LInt i) = show i
    pp _ (LFloat f) = show f
    pp _ (LBool b) = show b
    pp _ (LString b) = '"' : b ++ "\"" --TODO(naohman) add escaping
    pp _ LNull = "()"
