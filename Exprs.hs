{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Exprs where

import ParserUtils
import Control.Monad (void, foldM, ap)
import Constraints
import Models
import Data.List
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)


program :: MyParser Program 
program = chain' (Program [] [] [] []) <$> (sepBy topExprs (many spaceChar) <* eof)
    where topExprs = try (nonIndented exfix) <|> try (nonIndented exlet)
          chain' p []                = p
          chain' p (e@(Expr _ (EFix{})):es) = chain' (p {functions = e:functions p}) es
          chain' p (e@(Expr _ (ELet{})):es)  = chain' (p {globals = e:globals p}) es

------------------------- Basic Expr Parsing -------------------------------
-- TODO add opCons to the end of argDec
exfcall = efcall lName (parenCsl argDecs)
    where argDecs = tryList [pos, ks, kwS, posS]
          pos = posArg expr
          ks = kwArg (lName <* equals') expr
          posS = pSplat (symbol "*" *> expr)
          kwS = kwSplat (symbol "**" *> expr)

dataDec :: MyParser Data
dataDec = topdec (datap dataName. return) consDec
    where consDec = undefined
          dataName = data_ *> uName  <* where_

classDec :: MyParser Class
classDec = topdec cHead fStub 
    where cHead = classp lName (is' *> uName <* when') . return
          fStub = (,) <$> lName <*> argDec

exlet = elet decons (equals' *> expr)

excase :: MyParser Expr
excase = genDec header cases
    where header = ecase (between case_ of_ expr) . return
          cases = try inlineCase <|> try blockCase
          inlineCase = (,) <$> patArr <*> expr
          blockCase = genDec conCase expr 
          conCase es = (,) <$> patArr <*> return (chain es) 
          patArr = decons <* arrow'

exif = try inlineIf <|> try blockIf

inlineIf = if2Case <$> thenc <*> return [] <*> optional elsec
    where thenc = Then <$> (if_ *> expr <* arrow') <*> expr
          elsec = Else <$> (else_ *> arrow' *> expr)

blockIf = if2Case <$> thenc <*> many elifc <*> optional elsec
    where thenc = clause (\p -> Then <$> (if_ *> expr <* arrow') <*> p)
          elifc = clause (\p -> Elif <$> (elif *> expr <* arrow') <*> p)
          elsec = clause (\p -> Else <$> (else_ *> arrow' *> p))

clause :: (MyParser Expr -> MyParser a) -> MyParser a
clause h = try (h expr) <|> try (block h)
    where block h = genDec (h . return . chain) expr

if2Case :: Then -> [Elif] -> Maybe Else -> Expr
if2Case = undefined

--Todo implement binding syntax
decons = name' <|> dec' <|> nil' <|> match' <|> parens decons
    where dec' = dcons uName $ parens (decons `sepBy` comma')
          match' = dmatch cTimeLit
          name' = dname lName
          nil' = dnil (symbol "_")

exvar = evar lName 

exlambda = try blockLambda <|> try (elambda argArrow expr)
    where blockLambda = genDec conLam expr
          conLam = elambda argArrow . return . chain

exfix :: MyParser Expr
exfix = try blockFun <|> try (efix funName argArrow expr)
    where blockFun = genDec header expr
          header = efix funName argArrow . return . chain

chain :: [Expr] -> Expr
chain (e:es) = foldl conChain e es
    where conChain e1@(Expr p _) e2 = Expr p (EChain e1 e2)

expr = makeExprParser term operators <?> "expression"
term = tryList [exif, exlet, excase, exfcall, exliteral, exvar, parens expr]

operators =  [[uOp "+", uOp "-"],
         [bOp "*", bOp "/", bOp "//", bOp "%"],
         [bOp "+", bOp "-"],
         [bOp "==", bOp "<", bOp "<=", bOp ">", bOp ">="],
         [uOp "not"],
         [bOp "and", bOp "or", bOp "xor"]]

uOp s = Prefix (try (rword s) >> return (\e@(Expr p _) -> 
    Expr p (EFCall (s++"UN") [Arg p $ PosArg e])))

bOp s = InfixL (try (rword s) >> return (\e1@(Expr p _) e2 ->
    Expr p (EFCall s [Arg p $ PosArg e1, Arg p $ PosArg e2])))

--------------------- Literal Parsers ---------------------------------

exliteral :: MyParser Expr
exliteral = elit $ lexeme $ tryList [lFloat, lBool, lInt, lArray expr, lTuple expr,
                                lCons expr, lChar, lString, lDict expr, lSet expr]

cTimeLit :: MyParser Literal
cTimeLit = lexeme $ tryList [lFloat, lBool, lInt, lArray cLit, lTuple cLit,
                                lCons cLit, lChar, lString, lDict cLit, lSet cLit]
    where cLit = elit cTimeLit

-- Parses a Character literal
lChar :: MyParser Literal
lChar = lchar $ between (char '"') (char '"') myChar

-- Parses a String literal
lString = lstring $ between (char '"') (char '"') (many chars)
    where chars = escapedChar <|> noneOf "\""
    
myChar = escapedChar <|> noneOf "'\"\\\n\t\r"

escapedChar :: MyParser Char
escapedChar = char '\\' >> choice (zipWith escape codes reps) <?> "Bad escape code"
    where escape c r = char c >> return r
          codes = "ntr\\\"'" 
          reps = "\n\t\r\\\"'" 

-- Parses and Integer Literal
lInt = lint L.integer

-- Parses a float literal
lFloat = lfloat (try sufflt <|> flt)
    where flt    = L.float
          sufflt = fromIntegral <$> L.integer <* char 'f'

lCons p = try complexCons <|> try noArgs
    where noArgs = lcons uName (return [])
          complexCons = lcons uName (lStruct "(" ")" p)

-- Parses an array literal
lArray p = larray $ lStruct "[" "]" p

-- Parses a set literal, empty brackets are presumed to be Dicts
lSet p = lset $ lStruct "{" "}" p

lBool :: MyParser Literal
lBool = true <|> false
    where true = getPosition <* rword "True" >>= \p -> return (Literal p (LCons "True" []))
          false = getPosition <* rword "False" >>= \p -> return (Literal p (LCons "False" []))

-- Parses a Dict literal
lDict p = ldict $ lStruct "{" "}" (dictPair p)

-- Parses a Dictionary keyword pair
dictPair p = (,) <$> (expr <* colon') <*> p

-- Parses a Tuple literal 
lTuple p = getPosition >>= \pos -> parens $ do 
              head <- p <* comma'
              tail <- sepBy1 p comma'
              return $ Literal pos (LTuple (head:tail))

-- Parses a generic literal struct
lStruct s e p = between (symbol s) (symbol e) (sepBy p comma')

posAfterKw = "You can't define positional arugment that come after keyword arguments"
posAfterS = "You can't define positional argument after you've defined an argument that aggregates a list"
multiplePS = "You can't define multiple argument that aggregate lists"
multipleKS = "You can't define multiple arguments that aggregate keyword arugments"

{-defArgs = Args [] [] Nothing Nothing-}

------------------------- Function Declaration Parsers -------------------------

-- Parses function arguments
argArrow = argDec <* arrow'
argDec =  undefined
{-genArgs [addK, addP, addPS, addKS] defArgs-}
    
{-addK a = do-}
    {-n <- lName-}
    {-v <- equals' *> cTimeLit-}
    {-c <- opCons-}
    {-return  $ a {keyword = (n,v,c):keyword a}-}

{-addP a = do -}
    {-n <- lName-}
    {-c <- opCons-}
    {-case arrArgs a of-}
        {-Nothing -> case keyword a of-}
             {-[] -> return $ a {positional = (n,c):positional a}-}
             {-_  -> fail posAfterKw-}
        {-Just _  -> fail posAfterS-}

{-addPS a = do-}
    {-n <- symbol "*" *> lName-}
    {-c <- opCons-}
    {-case arrArgs a of-}
        {-Nothing -> return $ a {arrArgs = Just (n,c)}-}
        {-Just _  -> fail multiplePS-}

{-addKS a = do-}
    {-n <- symbol "**" *> lName-}
    {-c <- opCons-}
    {-case kwArgs a of-}
        {-Nothing -> return $ a {kwArgs = Just (n, c)}-}
        {-Just _  -> fail multipleKS-}
