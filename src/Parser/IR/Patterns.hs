module Parser.IR.Patterns
    ( Pat(..)
    ) where 

import Parser.IR.Literals
import Parser.IR.Pretty

data Pat = PCons String [Pat]
         | PAs   String Pat
         | PLit  Literal
         | PVar  String
         | PListComp [Pat] Pat
         | PArray [Pat]
         | PSingleton Pat
         | PEmptyList
         | PTuple [Pat]
         | PNil
    deriving Eq

instance Show Pat where
    show = pretty

instance Pretty Pat where
    pp i (PCons ctr pats) = ctr ++ parenCsl i pats
    pp i (PAs var pat) = var ++ "#" ++ pp i pat
    pp i (PTuple pats) = parenCsl i pats
    pp i (PListComp hds tl) = "[" ++ csl i hds ++ " | " ++ pp i tl ++ "]"
    pp i (PArray ps) = "[" ++ csl i ps ++ "]"
    pp i (PSingleton p) = "[" ++ pp i p ++ "]"
    pp _ PEmptyList = "[]"
    pp i (PLit lit) = pp i lit
    pp _ (PVar var) = var
    pp _ (PNil) = "_"
