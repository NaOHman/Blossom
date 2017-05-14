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
import Parser.IR.Pretty
import Data.List (intercalate)

type ParseExpr = Expr Type

data Function a = Function
    { qual       :: Maybe [Pred]
    , name       :: String
    , args       :: [(String, Maybe a)]
    , returnType :: Maybe a
    , body       :: [Expr a]
    }
    deriving Eq

data Branch a = Branch Pat [Expr a]
    deriving Eq

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
    deriving Eq

instance Pretty a => Show (Branch a) where
    show = pretty

instance Pretty a => Pretty (Branch a) where
    pp i (Branch p es) = pp i p ++ " -> " ++ newBlock (i+2) es

instance Pretty a => Show (Expr a) where
    show = pretty

instance Pretty a => Pretty (Expr a) where
    pp i (Lit l) = pp i l
    pp _ (Var v) = v
    pp i (EList es) = "[" ++ csl i es ++ "]"
    pp i (ETuple es) = "(" ++ csl i es ++ ")"
    pp i (Annotation e t) = pp i e ++ " : " ++ pp i t
    pp i (Field e s) = pp i e ++ "." ++ s
    pp i (Binding n e) = n ++ " = " ++ pp i e
    pp i (Call (Var v) es) = v ++ parenCsl i es
    pp i (Call e@(Field _ _) es) = pp i e ++ parenCsl i es
    pp i (Call e es) = "(" ++ pp i e ++ ")" ++ parenCsl i es
    pp i (Case e bs) = "case " ++ pp i e ++ " of" ++ newBlock (i+2) bs
    pp i (Lambda as rt es) = showLambda i as rt es
    pp i (Func (Function ps n as rt es)) = showPreds i ps ++ "fun " ++ n ++ showLambda i as rt es

showLambda :: Pretty a => Int -> [(String, Maybe a)] -> Maybe a -> [Expr a] -> String
showLambda i as rt es = showArgs i as ++ showMT i rt ++ " -> " ++ newBlock (i+2) es

showArgs :: Pretty a => Int -> [(String, Maybe a)] -> String
showArgs i as = "(" ++ intercalate ", " (map showArg as) ++ ")"
    where showArg (a, mt) = a ++ showMT i mt

showMT :: Pretty a => Int -> Maybe a -> String
showMT i (Just t) = " : " ++ pp i t
showMT _ _ = ""

showPreds :: Pretty a => Int -> Maybe [a] -> String
showPreds i (Just ps) = block i ps ++ indent i
showPreds _ _ = ""
