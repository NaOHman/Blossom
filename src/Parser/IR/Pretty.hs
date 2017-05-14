module Parser.IR.Pretty
    ( Pretty(..)
    , pretty
    ) where

import Data.List (intercalate)
import Parser.IR.Patterns
import Parser.IR.Expressions
import Parser.IR.Module
import Parser.IR.Types

class Pretty a where
    pp :: Int -> a -> String

pretty :: Pretty a => a -> String
pretty = pp 0

instance Pretty Literal where
    pp _ (LChar c) = c:""
    pp _ (LInt i) = show i
    pp _ (LFloat f) = show f
    pp _ (LBool b) = show b
    pp _ LNull = "()"

instance Pretty Pat where
    pp i (PCons ctr pats) = ctr ++ parenCsl i pats
    pp i (PAs var pat) = var ++ "#" ++ pp i pat
    pp i (PLit lit) = pp i lit
    pp _ (PVar var) = var
    pp _ (PNil) = "_"
    
instance Pretty Pred where
    pp i (IsIn bhvr ts) = "Given " ++ csl i ts ++ " in " ++ bhvr ++ "."

instance Pretty Type where
    pp i (Given ps t) = block i ps ++ " " ++ pp i t
    pp i (TFunc as rt) = "<" ++ csl i as ++ "> -> " ++ pp i rt
    pp i (TPoly n as) =  n ++ "<" ++ csl i as ++ ">"
    pp _ (TMono n) =  n
    pp _ TUnit =  "()"
    pp i (TList t) = "[" ++ pp i t ++ "]"
    pp i (TTuple ts) = "(" ++ csl i ts ++ ")"

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

instance Pretty a => Pretty (Branch a) where
    pp i (Branch p es) = pp i p ++ " -> " ++ newBlock (i+2) es

instance Pretty Implementation where
    pp i (Implementation ps ts bhvr es) = block i ps ++ csl i ts ++ " is " ++ bhvr ++ " because" ++ newBlock (i+2) es

instance Pretty Bhvr where
    pp i (Bhvr ps ts n ss) = block i ps ++ csl i ts ++ " is " ++ n ++ " when" ++ block' (i+2) ppStub ss
        where ppStub i' (sn, [], rt) = sn ++ " : " ++ pp i' rt
              ppStub i' (sn, sts, rt) = sn ++ csl i' sts ++ " : " ++ pp i' rt

instance Pretty Adt where
    pp i (Adt ps t ctrs) = block i ps ++ "data " ++ pp i t ++ " where" ++ block' (i+2) ppCons ctrs
        where ppCons _ (s, []) = s
              ppCons i' (s, ts) = s ++ csl i' ts

instance Pretty Rec where
    pp i (Rec ps t [] fs) = block i ps ++ "record " ++ pp i t ++ " where" ++ block' (i+2) ppField fs
    pp i (Rec ps t ss fs) = block i ps ++ "record " ++ pp i t ++ " inherits " ++ csl i ss ++  " where" ++ block' (i+2) ppField fs

instance Pretty Module where
    pp i (Module bs is as rs bhvs) = block i rs ++ "\n" ++ block i as ++ "\n" ++ block i bhvs ++ "\n" ++ block i is ++ "\n" ++ block i bs

ppField :: Int -> (String, Type) -> String
ppField i (f, t) = '.':f ++ " : " ++ pp i t

block' :: Int -> (Int -> a -> String) -> [a] -> String
block' i f = concatMap (\s -> indent i ++ f i s)

indent :: Int -> String
indent i = '\n' : replicate i ' '

showPreds :: Pretty a => Int -> Maybe [a] -> String
showPreds i (Just ps) = block i ps ++ indent i

showLambda :: Pretty a => Int -> [(String, Maybe a)] -> Maybe a -> [Expr a] -> String
showLambda i as rt es = showArgs i as ++ showMT i rt ++ " -> " ++ newBlock (i+2) es

showArgs :: Pretty a => Int -> [(String, Maybe a)] -> String
showArgs i as = "(" ++ intercalate ", " (map showArg as) ++ ")"
    where showArg (a, mt) = a ++ showMT i mt

showMT :: Pretty a => Int -> Maybe a -> String
showMT i (Just t) = " : " ++ pp i t
showMT _ _ = ""

newBlock :: Pretty a => Int -> [a] -> String
newBlock i es = indent i ++ block i es

block :: Pretty a => Int -> [a] -> String
block i = intercalate (indent i) . map (pp i) 

csl :: Pretty a => Int -> [a] -> String
csl i = intercalate ", " . map (pp i) 

parenCsl :: Pretty a => Int -> [a] -> String
parenCsl i ps =  "(" ++ csl i ps ++ ")"
