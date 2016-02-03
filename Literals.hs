{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Literals where

import ParserUtils
import Models
import Control.Monad (void, foldM, ap)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

--------------------- Literal Parsers ---------------------------------
literal p = lexeme $ tryList 
    [lFloat, lBool, lInt, lArray p, lTuple p, 
      lType, lCons p, lChar, lString]

--TODO Figure out how to data-tize sets/dicts
{-cTimeLit = lexeme $ tryList [lFloat, lBool, lInt, lArray cLit, lTuple cLit,-}
                                {-lCons cLit, lChar, lString]-}
    {-where cLit = elit cTimeLit-}
{-, lDict cLit, lSet cLit-}

-- Parses a Character literal
lType :: MyParser Literal
lType = literal' (LType <$> (uName <* rword ".type"))

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
    where true = gp <* rword "True" >>= \p -> return (Literal p (LCons "True" []))
          false = gp <* rword "False" >>= \p -> return (Literal p (LCons "False" []))

-- Parses a Dict literal
lDict = ldict . lStruct "{" "}" . dictPair

-- Parses a Dictionary keyword pair
dictPair p = (,) <$> (p <* colon') <*> p

-- Parses a Tuple literal 
lTuple p = parens $ do 
              pos <- gp
              head <- p <* comma'
              tail <- sepBy1 p comma'
              ltuple (head:tail)

-- Parses a generic literal struct
lStruct s e = between (symbol s) (symbol e) . (`sepBy` comma')
