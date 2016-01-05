{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Literals where

import ParserUtils
import Control.Monad (void, foldM, ap)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

data Literal where
    LChar   :: Char -> Literal
    LString :: String -> Literal
    LInt    :: Integer -> Literal
    LFloat  :: Double -> Literal
    LBool   :: Bool -> Literal
    LArray  :: (Show a) => [a] -> Literal
    {-LTuple  :: [Literal] -> Literal-}
    LDict   :: (Show a) => [(a,a)]  -> Literal
    LSet    :: (Show a) => [a] -> Literal

--------------------- Literal Parsers ---------------------------------

literal ex = tryList [lFloat, lBool, lInt, lArray ex, lChar, lString, lDict ex, lSet ex]

-- Parses a Character literal
lChar = LChar <$> between (char '\'') (char '\'') (escapedChar ecs ers)
    where ecs = zip "'\\trn" "'\\\t\r\n"
          ers = "\\\n\r"

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

-- Parses an array literal
lArray = fmap LArray . lStruct '[' ']' 

-- Parses a Tuple literal 
-- TODO tuples must have more than one element
{-lTuple = LTuple <$> lStruct '(' ')' lValue -}

-- Parses a set literal, empty brackets are presumed to be Dicts
lSet = fmap LSet . lStruct '{' '}'

lBool = true <|> false
    where true = miLexeme (string "True") >> const (LBool True)
          false = miLexeme (string "False") >> const (LBool True)

-- Parses a Dict literal
lDict = fmap LDict . lStruct '{' '}' . dictPair

-- Parses a Dictionary keyword pair
dictPair ex = do
    key <- ex <* anySpace
    colon' 
    value <- ex <* anySpace
    return (key, value)

-- Parses a generic literal struct
lStruct s e p = between (char s) (char e) (sepBy p' comma')
    where p' = p <* anySpace

-- Handles character escapes in literals
escapedChar escps errs = escape <|> noneOf errs
    where escape = do
                        char '\\'
                        c <- anyChar
                        case lookup c escps of
                            Just ec -> return ec
                            Nothing -> fail $ "invalid escape sequence \\" ++ show c

instance Show Literal where
    show (LChar   c) = "Char: " ++ show c
    show (LString s) = "String \"" ++ s ++ "\""
    show (LInt    i) = "Int: " ++ show i
    show (LFloat  f) = "Float: " ++ show f
    show (LArray  a) = "Array: [" ++ showStruct a ++ "]"
    show (LSet    s) = "Set: {" ++ showStruct s ++ "}"
    show (LDict   d) = "Dict: {" ++ showDict d ++ "}"
    {-show (LTuple  t) = "Tuple: (" ++ showStruct t ++ "}"-}
