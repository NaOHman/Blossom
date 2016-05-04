module Interpretor.Values where

import Language.Expressions
import Control.Monad
import Data.List (intercalate, unionBy)
import Data.Char (isUpper)

data Value = VInt Integer
           | VFloat Double
           | VBool Bool
           | VChar Char
           | VNull
           | VOver Expr
           | VCons String [Value]
           | VLambda [Pat] Expr
           | BuiltIn Id Int ([Value] -> IO Value)

instance Show Value where
   show (VInt i) = show i 
   show (VFloat i) = show i 
   show (VBool i) = show i 
   show (VChar i) = show i 
   show (VCons i vs) = i ++ "(" ++ intercalate ","  (map show vs) ++ ")"
   show (VOver ex) = "Over" ++ show ex
   show (BuiltIn n i _) = show n ++ show i
   show (VLambda ps e) = show ps ++ "(" ++ intercalate ","  (map show ps) ++ ")" ++ show e
   show VNull = "Null"

data Scope = Scope [(Id, Value)]
    deriving Show

add :: Scope -> Scope -> Scope
add (Scope b1) (Scope b2) = Scope (unionBy f b1  b2)
    where f (i,_) (i2,_) = i == i2

lookupScope :: Id -> Scope -> Maybe Value
lookupScope ('#':i) _ = Just $ BuiltIn "Access" 1 (\[VCons _ vs] -> return (vs !! read i))
lookupScope i@(c:_) (Scope sc) 
    | isUpper c || c == '[' || c == '(' = Just $ VCons i []
    | otherwise = lookup i sc
lookupScope _ _ = fail "Tried to look up empty string"

catScope :: [Scope] -> Scope
catScope = Scope . concatMap (\(Scope s) -> s) 

bind :: Id -> Value -> Scope -> Scope
bind i v (Scope s) = Scope $ (i,v):s

match :: Pat -> Value -> Maybe Scope
match PNil _ = Just $ Scope []
match (PLit l) v 
  | lit2Val l == v = Just $ Scope []
  | otherwise = Nothing
match (PVar n) v = Just $ Scope [(n,v)]
match (PCons n1 ps) (VCons n2 vs) 
  | n1 == n2 = matches ps vs
  | otherwise  = Nothing
match (PAs i p) v = bind i v <$> match p v
match _ _ = Nothing

matches :: [Pat] -> [Value] -> Maybe Scope
matches ps vs = catScope <$> zipWithM match ps vs

lit2Val :: Literal -> Value
lit2Val (LInt i) = VInt i
lit2Val (LFloat f) = VFloat f
lit2Val (LChar c) = VChar c
lit2Val (LBool b) = VBool b
lit2Val LNull = VNull

instance Eq Value where
    VInt   i == VInt   i' = i == i'
    VFloat f == VFloat f' = f == f'
    VChar  c == VChar  c' = c == c'
    VBool  b == VBool  b' = b == b'
    VNull    == VNull     = True
    _        == _         = False
