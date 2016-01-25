{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

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


program :: Parsec String Program 
program = condense (Program [] [] [] []) <$> sepBy topExprs (many spaceChar)
    where topExprs = try (nonIndented efix) <|> try (nonIndented elet)
          condense p []                = p
          condense p (e@EFix{} :es) = condense (p {functions = e:functions p}) es
          condense p (e@ELet{}:es)  = condense (p {globals = e:globals p}) es

------------------------- Basic Expr Parsing -------------------------------
-- TODO add opCons to the end of argDec
fcall = EFCall <$> do 
    name <- lName
    genArgs [fArg, kwArg] (Call name [] [])
    where fArg (Call n p k ) = fmap (\a -> Call n (p++[a]) k) expr
          kwArg (Call n p k) = fmap (\a -> Call n p (k++[a])) kwTpl
          kwTpl = (,) <$> lName <*> (equals' *> expr)

dataDec = nonIndented $ genDec dataName consDec Data
    where consDec = efix
          dataName = data' *> uName  <* where'

classDec = nonIndented $ genDec cDec fHeader ($)
    where cDec = Class <$> lName <*> (is' *> uName <* when')
          fHeader = (,) <$> lName <*> argDec

elet = ELet <$> decons <*> (equals' *> expr)

ecase = genDec header cases ECase
    where header = case' *> expr <* of'
          cases = try inlineCase <|> try blockCase
          inlineCase = (,) <$> (decons <* arrow') <*> expr
          blockCase = genDec (decons <* arrow') expr (\pat es -> (pat, condense es))

--Todo implement binding syntax
decons = name' <|> dec' <|> nil' <|> match' <|> parens decons
    where dec' = DCons <$> uName <*> parens (decons `sepBy` comma')
          match' = DMatch <$> cTimeLit
          name' = DName <$> lName
          nil' = symbol "_" >> return DNil

var = EVar <$> lName

elambda = try blockLambda <|> try (anonDec <*> expr)
    where anonDec = ELambda <$> argArrow
          blockLambda = genDec anonDec expr (. condense)

efix = try blockFun <|> try (namedDec <*> expr)
    where namedDec = EFix <$> funName <*> argArrow
          blockFun = genDec namedDec expr (. condense)

condense (e:es) = foldl EChain e es

expr = lexeme (makeExprParser term operators <?> "expression")
term = tryList [elet, ecase, fcall, eLiteral, var, parens expr]

eLiteral = ELit <$> literal
operators = 
{-[[uOp "+", uOp "-"],-}
        [[bOp "*", bOp "/", bOp "//", bOp "%"],
         [bOp "+", bOp "-"],
         [bOp "==", bOp "<", bOp "<=", bOp ">", bOp ">="],
         [ruOp "not"],
         [rbOp "and", rbOp "or", rbOp "xor"]]

uOp = op1 symbol
bOp = op2 symbol
ruOp = op1 rword
rbOp = op2 rword
op1 p s = Prefix (p s *> pure (\a -> EFCall $ Call (s++"UN") [a] []))
op2 p s = InfixL (p s *> pure (\a b -> EFCall $ Call s [b,a] []))

--------------------- Literal Parsers ---------------------------------

literal = lexeme $ tryList [lFloat, lBool, lInt, lArray expr, lTuple expr,
                                lCons expr, lChar, lString, lDict expr, lSet expr]
cTimeLit = lexeme $ tryList [lFloat, lBool, lInt, lArray cLit, lTuple cLit,
                                lCons cLit, lChar, lString, lDict cLit, lSet cLit]
    where cLit = ELit <$> cTimeLit

-- Parses a Character literal
lChar = LChar <$> between quote quote (escapedChar ecs ers)
    where ecs = zip "'\\trn" "'\\\t\r\n"
          ers = "\\\n\r"
          quote = char '\''

-- Parses a String literal
lString = LString <$> between (char '"') (char '"') (many (escapedChar ecs ers))
    where ecs = zip "\"\\trn" "\"\\\t\r\n"
          ers = "\\\n\r\""

-- Parses and Integer Literal
lInt = LInt <$> L.integer

-- Parses a float literal
lFloat = LFloat <$> (try sufflt <|> flt)
    where flt    = L.float
          sufflt = fromIntegral <$> L.integer <* char 'f'

lCons p = try complexCons <|> try noArgs
    where noArgs = LCons <$> uName <*> return []
          complexCons = LCons <$> uName <*> lStruct "(" ")" p

-- Parses an array literal
lArray p = LArray <$> lStruct "[" "]" p

-- Parses a set literal, empty brackets are presumed to be Dicts
lSet p = LSet <$> lStruct "{" "}" p

lBool = true <|> false
    where true = rword "True" >> return (LCons "True" [])
          false = rword "False" >> return (LCons "False" [])

-- Parses a Dict literal
lDict p = LDict <$> lStruct "{" "}" (dictPair p)

-- Parses a Dictionary keyword pair
dictPair p = (,) <$> (expr <* colon') <*> p

-- Parses a Tuple literal 
lTuple p = parens $ do 
              head <- p <* comma'
              tail <- sepBy1 p comma'
              return $ LTuple (head:tail)

-- Parses a generic literal struct
lStruct s e p = between (symbol s) (symbol e) (sepBy p comma')

-- Handles character escapes in literals
escapedChar escps errs = escape <|> noneOf errs
    where escape = do
                        char '\\'
                        c <- anyChar
                        case lookup c escps of
                            Just ec -> return ec
                            Nothing -> fail $ "invalid escape sequence \\" ++ show c

posAfterKw = "You can't define positional arugment that come after keyword arguments"
posAfterS = "You can't define positional argument after you've defined an argument that aggregates a list"
multiplePS = "You can't define multiple argument that aggregate lists"
multipleKS = "You can't define multiple arguments that aggregate keyword arugments"

defArgs = Args [] [] Nothing Nothing

------------------------- Function Declaration Parsers -------------------------

-- Parses function arguments
argArrow = argDec <* arrow'
argDec = genArgs [addK, addP, addPS, addKS] defArgs
    
addK a = do
    n <- lName
    v <- equals' *> cTimeLit
    c <- opCons
    return  $ a {keyword = (n,v,c):keyword a}

addP a = do 
    n <- lName
    c <- opCons
    case arrArgs a of
        Nothing -> case keyword a of
             [] -> return $ a {positional = (n,c):positional a}
             _  -> fail posAfterKw
        Just _  -> fail posAfterS

addPS a = do
    n <- symbol "*" *> lName
    c <- opCons
    case arrArgs a of
        Nothing -> return $ a {arrArgs = Just (n,c)}
        Just _  -> fail multiplePS

addKS a = do
    n <- symbol "**" *> lName
    c <- opCons
    case kwArgs a of
        Nothing -> return $ a {kwArgs = Just (n, c)}
        Just _  -> fail multipleKS
