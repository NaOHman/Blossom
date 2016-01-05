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

data Statement where
    Assign  :: Decons -> Expr -> Statement
    StExpr  :: Expr -> Statement
    StFun   :: Function Statement -> Statement

data Decons where
    DName :: String -> Decons
    DCons :: String -> [Decons] -> Decons
    DNil  :: Decons

data Expr  where
    ELit    :: Literal -> Expr
    EFCall  :: Call -> Expr
    EVar    :: String -> Expr
    ECons   :: Call -> Expr
    OAdd    :: Expr -> Expr -> Expr
    OSub    :: Expr -> Expr -> Expr
    OMult   :: Expr -> Expr -> Expr
    ODiv    :: Expr -> Expr -> Expr
    OIntDiv :: Expr -> Expr -> Expr
    OMod    :: Expr -> Expr -> Expr
    OEq     :: Expr -> Expr -> Expr
    OLT     :: Expr -> Expr -> Expr
    OLTE    :: Expr -> Expr -> Expr
    OGT     :: Expr -> Expr -> Expr
    OGTE    :: Expr -> Expr -> Expr
    OOr     :: Expr -> Expr -> Expr
    OAnd    :: Expr -> Expr -> Expr
    OXor    :: Expr -> Expr -> Expr
    ONot    :: Expr -> Expr
    ONeg    :: Expr -> Expr
    OPos    :: Expr -> Expr
    

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

assign = Assign <$> decons <*> (miString "=" *> expr)

decons = name' <|> dec' <|> nil' <|> parens decons
    where dec' = DCons <$> miUName <*> parens (decons `sepBy` comma')
          name' = DName <$> miLName
          nil' = miString "_" >> const DNil

term = parens expr <|> eLiteral <|> var <|> fcall <|> consCall

var = EVar <$> miLName

expr = makeExprParser term table <?> "expression"

miLiteral = miLexeme (literal expr)
eLiteral = ELit <$> miLiteral
table = [[oPos, oNeg],
         [oMult, oDiv, oIntDiv, oMod],
         [oAdd, oSub],
         [oEq, oLT, oLTE, oGT, oGTE],
         [oNot],
         [oAnd, oOr, oXor]]


miExpr cstr s = InfixL (miString s *> pure cstr)
iExpr cstr s = InfixL (iString s *> pure cstr)

oAdd = miExpr OAdd "+"
oSub = miExpr OSub "-"
oMult = miExpr OMult "*"
oDiv = miExpr ODiv "/"
oIntDiv = miExpr OIntDiv "//"
oMod = miExpr OMod "%"
oEq = miExpr OEq "=="
oLT = miExpr OLT "<"
oLTE = miExpr OLTE "<="
oGT = miExpr OGT ">"
oGTE = miExpr OGTE "<="

oAnd = iExpr OAnd "and"
oOr = iExpr OOr "or"
oXor = iExpr OXor "xor"

oNot = Prefix (iString "not" *> pure ONot)
oNeg = Prefix (miString "-" *> pure ONeg)
oPos = Prefix (miString "+" *> pure OPos)

instance Show Expr where
    show (OAdd a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (OSub a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show (OMult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
    show (ODiv a b) = "(" ++ show a  ++ "/" ++ show b ++ ")"
    show (OIntDiv a b) = "(" ++ show a ++ "//" ++ show b ++ ")"
    show (OMod a b) = "(" ++ show a ++ "%" ++ show b ++ ")"
    show (OEq a b) = "(" ++ show a ++ "==" ++ show b ++ ")"
    show (OLT a b) = "(" ++ show a ++ "<" ++ show b ++ ")"
    show (OLTE a b) = "(" ++ show a ++ "<=" ++ show b ++ ")"
    show (OGT a b) = "(" ++ show a ++ ">" ++ show b ++ ")"
    show (OGTE a b) = "(" ++ show a ++ ">=" ++ show b ++ ")"
    show (OOr a b) = "(" ++ show a ++ " or " ++ show b ++ ")"
    show (OAnd a b) = "(" ++ show a ++ " and " ++ show b ++ ")"
    show (OXor a b) = "(" ++ show a ++ " xor " ++ show b ++ ")"
    show (ONot a) = "(not " ++ show a ++ ")"
    show (ONeg a) = "(-" ++ show a ++ ")"
    show (OPos a) = "(+" ++ show a ++ ")"
    show (ELit l) = show l
    show (EFCall f) = show f

instance Show Statement where
    show (Assign n e) = "assign " ++ show n ++ " " ++ show e

instance Show Decons where
    show (DCons n as) = n ++ intercalate "," (map show as)
    show (DName n) = n

instance Show Call where
    show (Call n p k) = "call " ++ n ++ "(" ++ showP p ++ "("++ showK k ++ "))"
        where showP = intercalate ", " . map show
              showK = intercalate ", " . map (\(n,v) -> n ++ "=" ++ show v)
