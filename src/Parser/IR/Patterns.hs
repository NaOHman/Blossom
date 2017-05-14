module Parser.IR.Patterns
    ( Pat(..)
    ) where 
import Parser.IR.Literals

data Pat = PCons String [Pat]
         | PAs   String Pat
         | PLit  Literal
         | PVar  String
         | PNil
    deriving Eq
