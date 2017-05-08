module Language.Expressions 
    ( module Language.Core
    , module Language.Types
    , Expr (..) , Literal (..)
    , Pat (..)
    , Annotated(..)
    , prettyPrint
    ) where

import Language.Core
import Language.Types
import Data.List (intercalate)
import Control.Arrow (second)
    
data Expr = Lit Literal
          | Var  Id
          | Abs  [Pat] Expr
          | Ap   Expr Expr
          | Let Id Expr Expr
          | LetRec Id Expr Expr
          | Case Expr [(Pat, Expr)]
          | Annot (Annotated Expr)
    deriving Eq -- used for testing only

-- Quantification is defered until after parsing.
data Annotated a = a :-: Qual Type
    deriving (Show, Eq)

instance Types Expr where
    tv (Abs _ e) = tv e
    tv (Let _ e1 e2) = tv e1 ++ tv e2
    tv (LetRec _ e1 e2) = tv e1 ++ tv e2
    tv (Ap e1 e2) = tv e1 ++ tv e2
    tv (Case e as) = tv e ++ concatMap (tv . snd) as
    tv (Annot (e :-: _)) = tv e 
    tv _ = []

    apply s (Abs ps e) = Abs ps (apply s e)
    apply s (Let i e1 e2) = Let i (apply s e1) (apply s e2)
    apply s (LetRec i e1 e2) = LetRec i (apply s e1) (apply s e2)
    apply s (Ap e1 e2) = Ap (apply s e1) (apply s e2)
    apply s (Case e as) = Case (apply s e) $ map (second (apply s)) as
    apply s (Annot (e :-: sc)) = Annot (apply s e :-: sc)
    apply _ e = e

-- TODO move
prettyPrint :: Expr -> String
prettyPrint = prettyPrint' 0

prettyPrint' :: Int -> Expr -> String
prettyPrint' _ (Lit l) = show l
prettyPrint' _ (Var v) = v
prettyPrint' i (Let ix e1 e2) = ix ++ " = " ++ prettyPrint' (i+4) e1 ++ indent (i+2) ++ prettyPrint' (i+2) e2
prettyPrint' i (LetRec ix e1 e2) = "func " ++ ix ++ " = " ++ prettyPrint' (i+4) e1 ++ indent (i+2) ++ prettyPrint' (i+2) e2 
prettyPrint' i (Ap (Ap (Var "!seq") e1) e2) = prettyPrint' i e1 ++ ";" ++ indent i ++ prettyPrint' i e2 
prettyPrint' _ e@(Ap (Ap (Var "[cons]") (Lit (LChar _))) _) = "\"" ++ prettyString e ++ "\""
prettyPrint' i (Ap e1 e2) = prettyPrint' i e1 ++ "(" ++ prettyPrint' i e2 ++ ")" 
prettyPrint' i (Annot (e :-: s)) = prettyPrint' i e ++ " : " ++ show s
prettyPrint' i (Abs ps e) = show ps ++ " ->" ++ indent (i+2) ++ prettyPrint' (i+2) e
prettyPrint' i (Case ps cs) = "Case " ++ show ps ++ " of " ++ concatMap (ppCase (i+2)) cs
    where ppCase ind (p, e) = indent ind ++ show p ++ " -> " ++ prettyPrint' ind e

prettyString :: Expr -> String
prettyString (Ap (Ap (Var "[cons]") (Lit (LChar c))) e) = c : prettyString e
prettyString _ = ""

indent :: Int -> String
indent i = '\n' : replicate i ' '


instance Show Expr where
    show (Lit l) = show l
    show (Var v) = "{" ++ v ++ "}"
    show (Abs ps ex) = show ps ++ " -> " ++ show ex
    show (Ap e1 e2) = "(AP" ++ show e1 ++  " " ++ show e2 ++ ")"
    show (Case e as) =  "Case " ++ show e ++ " of" ++ concatMap show as
    show (Annot (e :-: s)) = show e ++ " : " ++ show s
    show (Let i e1 e2) = show "Let " ++ i ++ " = "++ show e1 ++ " in " ++ show e2
    show (LetRec i e1 e2) = show "LetRec " ++ i ++ " = "++ show e1 ++ " in " ++ show e2

data Literal = LChar Char
             | LInt    Integer
             | LFloat  Double
             | LBool  Bool
             | LNull
    deriving Eq

instance Show Literal where
    show (LChar  c) = show c
    show (LInt   i) = show i
    show (LFloat d) = show d
    show (LBool b) = show b
    show LNull      = "Null"

data Pat = PCons Id [Pat]
         | PAs   Id Pat
         | PLit  Literal
         | PVar  Id
         | PNil
    deriving Eq

instance Show Pat where
    show PNil     = "[_]"
    show (PLit l) = "[" ++ show l ++ "]"
    show (PVar v) = "[" ++ v ++ "]"
    show (PCons v ps) = "[" ++ v ++ " " ++ subPats ++ "]"
        where subPats = intercalate ", " $ map show ps
    show (PAs v p) = "[" ++ v ++ "#" ++ show p ++ "]"

class Prod a where
    prod :: [a] -> a

instance Prod a => Prod (Qual a) where 
    prod qs = let (ps,as) = unzip [(p,a) | p :=> a <- qs]
              in concat ps :=> prod as

instance Prod Pat where 
    prod [as] = as
    prod as = PCons (prodName as) as

instance Prod Expr where 
    prod [as] = as
    prod as = foldl Ap (Var $ prodName as) as

instance Prod Type where 
    prod [t] = t
    prod ts = foldl TAp (TCons (prodName ts) ks) ts
        where ks =  foldr KFun Star (replicate (length ts) Star)

prodName :: [a] -> String
prodName ls = show (length ls) ++ "PROD"
