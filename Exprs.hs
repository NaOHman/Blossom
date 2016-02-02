{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Exprs where

import ParserUtils
import Control.Monad (void, foldM, ap, liftM)
import Control.Monad.State
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

parseBlossom = parseFromFile (evalStateT program 0)

program :: MyParser Program 
program = toPrg <$> (many statement <* eof)
    where toPrg = foldl f (Program [] [] [] [])
          f p (SEx e) = p {globals   = e : globals p}
          f p (SDa d) = p {datatypes = d : datatypes p}
          f p (SCl c) = p {classes   = c : classes p}
          f p (SIn i) = p {instances = i : instances p}

statement = tryList
                [ SEx <$> (indentGuard 1 *> exfix) 
                , SEx <$> (indentGuard 1 *> exlet)
                , SDa <$> (indentGuard 1 *> dataDec) 
                , SCl <$> (indentGuard 1 *> classDec) 
                , SIn <$> (indentGuard 1 *> instnc)]

------------------------- Basic Expr Parsing -------------------------------
-- TODO add opCons to the end of argDec
exfcall = efcall lName (parenCsl argDecs)
    where argDecs = tryList [pos, ks, kwS, posS]
          pos = posArg expr
          ks = kwArg (lName <* equals') expr
          posS = pSplat (symbol "*" *> expr)
          kwS = kwSplat (symbol "**" *> expr)

dataDec :: MyParser Data
dataDec = Data <$> getPosition <*> dataName <*> constrs
    where constrs = block ((,) <$> uName <*> dataArgs)
          dataName = data_ *> decCons uName <* where_

classDec :: MyParser Class
classDec = Class <$> getPosition <*> consOrList lName <*> cheader <*> stubs
    where cheader = is' *> uName <* when'
          stubs = block $ (,,) <$> lName <*> csl justConsArg <*> recSufCons

instnc :: MyParser Instance
instnc = Instance <$> getPosition <*> consOrList uName 
                  <*> iheader <*> block exfix 
    where iheader = is' *> uName <* because'

consOrList p = do 
    c <- optional p
    case c of
        Just n -> DecCons <$> return n <*> params
        Nothing -> DecCons <$> return "" <*> angles constraint
    where params = fromMaybe [] <$> optional (angles constraint)

decCons :: MyParser Id -> MyParser DecCons
decCons p = DecCons <$> p <*> params
    where params = fromMaybe [] <$> optional (angles constraint)

exlet = elet decons (equals' *> expr)
-- TODO let cases ex.
--  a = case even a of Just a = a
--  == a = cast(x,Just a)

-- TODO Fix onecase def
excase :: MyParser Expr
excase = ecase header (inlineOrBlock aCase)
    where header = case_ *> expr <* of_
          aCase = (,) <$> (decons <* arrow') <*> exblock 


exblock ::  MyParser Expr
exblock = liftM chain (inlineOrBlock expr) 

--Todo implement binding syntax
decons = tryList [name',  dec', nil', match', parens decons]
    where dec' = dcons uName $ csl decons
          match' = dmatch cTimeLit
          name' = dname lName
          nil' = dnil (symbol "_")

exvar = evar lName 

exlambda = elambda argArrow exblock

exfix :: MyParser Expr
exfix = efix funName argArrow exblock

chain :: [Expr] -> Expr
chain [e] = e
chain (e:es) = foldl conChain e es
    where conChain e1@(Expr p _) e2 = Expr p (EChain e1 e2)

expr = makeExprParser term operators <?> "expression"
--todo exif
term = tryList [excase, exfcall, exlet, exliteral, exvar, parens expr]

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

--TODO Figure out how to data-tize sets/dicts
exliteral :: MyParser Expr
exliteral = elit $ lexeme $ tryList [lFloat, lBool, lInt, lArray expr, lTuple expr,
                                lCons expr, lChar, lString]
{-, lDict expr, lSet expr-}

cTimeLit :: MyParser Literal
cTimeLit = lexeme $ tryList [lFloat, lBool, lInt, lArray cLit, lTuple cLit,
                                lCons cLit, lChar, lString]
    where cLit = elit cTimeLit
{-, lDict cLit, lSet cLit-}

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
lTuple p = parens $ do 
              pos <- getPosition
              head <- p <* comma'
              tail <- sepBy1 p comma'
              ltuple (head:tail)

-- Parses a generic literal struct
lStruct s e p = between (symbol s) (symbol e) (sepBy p comma')

posAfterKw = "You can't define positional arugment that come after keyword arguments"
posAfterS = "You can't define positional argument after you've defined an argument that aggregates a list"
multiplePS = "You can't define multiple argument that aggregate lists"
multipleKS = "You can't define multiple arguments that aggregate keyword arugments"

{-defArgs = Args [] [] Nothing Nothing-}

------------------------- Function Declaration Parsers -------------------------

-- Parses function arguments
argArrow =  csl (argWithCons opSufCons) <* arrow'

dataArgs = csl (argWithCons recSufCons <|> try justConsArg)
                
justConsArg  = argdec (PosDec <$> return "") constraint

argWithCons c = tryList $ map ($ c) [kwsdec, psdec, kwdec, pdec]
    where pdec = argdec (PosDec <$> lName)
          kwdec = argdec (KWDec <$> lName <*> (equals' *> cTimeLit))
          kwsdec = argdec (KWSDec <$> (lName <* symbol "**"))
          psdec = argdec (PSDec <$> (lName <* symbol "*"))

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
