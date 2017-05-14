module Parser.IR.Patterns
    ( Pat(..)
    ) where 

import Parser.IR.Literals
import Parser.IR.Pretty

data Pat = PCons String [Pat]
         | PAs   String Pat
         | PLit  Literal
         | PVar  String
         | PNil
    deriving Eq

instance Show Pat where
    show = pretty

instance Pretty Pat where
    pp i (PCons ctr pats) = ctr ++ parenCsl i pats
    pp i (PAs var pat) = var ++ "#" ++ pp i pat
    pp i (PLit lit) = pp i lit
    pp _ (PVar var) = var
    pp _ (PNil) = "_"
