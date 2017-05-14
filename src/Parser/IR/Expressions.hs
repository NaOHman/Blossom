module Parser.IR.Expressions
    ( module X
    , Expr(..)
    , ParseExpr
    , Function(..)
    , Branch(..)
    ) where 
import Parser.IR.Literals as X
import Parser.IR.Patterns as X
import Parser.IR.Types

type ParseExpr = Expr Type

data Function a = Function
    { qual       :: Maybe [Pred]
    , name       :: String
    , args       :: [(String, Maybe a)]
    , returnType :: Maybe a
    , body       :: [Expr a]
    }

data Branch a = Branch Pat [Expr a]

data Expr a = Lit Literal
            | Var String
            | Lambda [(String, Maybe a)] (Maybe a) [Expr a]
            | Func (Function a)
            | Call (Expr a) [Expr a]
            | Binding String (Expr a)
            | Case (Expr a) [Branch a]
            | Field (Expr a) String
            | Annotation (Expr a) a
            | EList [Expr a]
            | ETuple [Expr a]

