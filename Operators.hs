{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Exprs where

import ParserUtils
import Literals
import Functions
import Control.Monad (void, foldM, ap)
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

type Id = String

data Program = Program { 
      classes   :: [Class], 
      functions :: [Expr], 
      datatypes :: [Data]
    }
    -- todo type aliases and error types

data Pat where
    DName :: Id -> Pat
    DCons :: Id -> [Pat] -> Pat
    DNil  :: Pat

data Expr  where
    ELit    :: Literal -> Expr
    EVar    :: Id -> Expr
    ELambda :: Lambda Expr -> Expr
    EFCall  :: Call -> Expr
    ECons   :: Call -> Expr --special case of function application
    EFix    :: Id -> Lambda Expr -> Expr
    ELet    :: Pat -> Expr -> Expr -- EChain ~= in
    ECase   :: Expr -> [(Pat, Expr)] -> Expr
    EChain  :: Expr -> Expr -> Expr

data Call = Call {
    fname :: String,
    posArgs :: [Expr],
    kwArgs :: [(String, Expr)]
    }

------------------------- Basic Expr Parsing -------------------------------
--

call cons np = cons <$> do 
    name <- np
    genArgs [fArg, kwArg] (Call name [] [])
    where fArg (Call n p k ) = fmap (\a -> Call n (p++[a]) k) expr
          kwArg (Call n p k) = fmap (\a -> Call n p (k++[a])) kwTpl
          kwTpl = (,) <$> miLName <*> (miString "=" *> expr)

fcall    = call EFCall miLName
consCall = call ECons  miUName

elet = ELet <$> decons <*> (miString "=" *> expr)

decons = name' <|> dec' <|> nil' <|> parens decons
    where dec' = DCons <$> miUName <*> parens (decons `sepBy` comma')
          name' = DName <$> miLName
          nil' = miString "_" >> const DNil

term = parens expr <|> eLiteral <|> var <|> fcall <|> consCall

var = EVar <$> miLName

expr = makeExprParser term operators <?> "expression"

miLiteral = miLexeme (literal expr)
eLiteral = ELit <$> miLiteral
operators = [[miuOp "+", miuOp "-"],
         [mibOp "*", mibOp "/", mibOp "//", mibOp "%"],
         [mibOp "+", mibOp "%"],
         [mibOp "==", mibOp "<", mibOp "<=", mibOp ">", mibOp ">="],
         [iuOp "not"],
         [ibOp "and", ibOp "or", ibOp "xor"]]

bOp p s = InfixL (p s *> pure (\a b -> EFCall $ Call s [a,b] []))
uOp s = Prefix (p s *> pure (\a -> EFCall $ Call s [a] []))
mibOp = bOp miString 
ibOp = bOp iString 
miuOp = uOp miString 
iuOp = uOp iString 

{-instance Show Expr where-}
    {-show (ELit l) = show l-}
    {-show (EFCall f) = show f-}

{-instance Show Statement where-}
    {-show (Assign n e) = "assign " ++ show n ++ " " ++ show e-}

instance Show Pat where
    show (DCons n as) = n ++ intercalate "," (map show as)
    show (DName n) = n

instance Show Call where
    show (Call n p k) = "call " ++ n ++ "(" ++ showP p ++ "("++ showK k ++ "))"
        where showP = intercalate ", " . map show
              showK = intercalate ", " . map (\(n,v) -> n ++ "=" ++ show v)
