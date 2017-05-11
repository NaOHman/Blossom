{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-} 

{- |
Module      : Parser.Lexeme
Description : A Module that provides basic parsing functions, compare to Megaparsec.Lexer
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module provides all sorts of nice utility functions to help you parse things.
-}


module Parser.Lexeme 
   ( BParser()
   , getTV
   , putTV
   , symbol
   , lexeme
   , opList
   , block
   , inlineBlock
   , parens
   , singleQuotes
   , doubleQuotes
   , brackets
   , bracketList
   , lAngle
   , rAngle
   , angles
   , angles1
   , csl
   , opCsl
   , eol_
   , nonIndented
   , charChar
   , stringChar
   , C.digitChar
   ) where

import Text.Megaparsec as X
import Text.Megaparsec.Char as C
import qualified Data.Foldable as F
import Control.Monad.State
import qualified Text.Megaparsec.Lexer as L

--------------------------- General Declaration -----------------------

type BParser a = StateT (Int,Int) (Parsec String) a

getIndent :: BParser Int
getIndent = snd <$> get

savePos :: BParser ()
savePos = do
    (tv,_) <- get 
    pos <- getPosition
    put (tv, sourceColumn pos)

getTV :: BParser Int
getTV = fst <$> get

putTV :: Int -> BParser ()
putTV i = do 
    (_,p) <- get
    put (i,p)

symbol :: String -> BParser String
symbol = L.symbol sc 

opList :: BParser [a] -> BParser [a]
opList p = F.concat <$> optional p

lexeme :: BParser a -> BParser a
lexeme = L.lexeme sc

charChar :: BParser Char
charChar = escaped "ntr\\'" "\n\t\r\\'"  <|> noneOf "\n\t\r\\'"

stringChar :: BParser Char
stringChar = escaped "ntr\\\"" "\n\t\r\\\"" <|> noneOf "\n\t\r\\\""

escaped :: String -> String -> BParser Char
escaped codes replacements = char '\\' >> choice (zipWith escape codes replacements) <?> "Bad escape code"
    where escape c r = char c >> return r

sc :: BParser ()
sc = L.space (void $ oneOf " \t") lineCmnt blockCmnt

indentSC :: BParser ()
indentSC = L.space indentConsumer lineCmnt blockCmnt
    where indentConsumer = void (oneOf " \t") <|> void newIndent
          newIndent = eol_ >> many (oneOf " \t") >> savePos

lineCmnt :: BParser ()
lineCmnt  = L.skipLineComment inlineComment
    where inlineComment = "//"

blockCmnt :: BParser ()
blockCmnt = L.skipBlockComment commentStart commentEnd
    where commentStart = "/*"
          commentEnd = "*/"

nonIndented :: BParser a -> BParser a
nonIndented = L.nonIndented indentSC

eol_ :: BParser ()
eol_ = void $ try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r"

parens :: BParser a -> BParser a
parens = between (symbol "(") (symbol ")")

singleQuotes :: BParser a -> BParser a
singleQuotes = between (char '\'') (char '\'')

doubleQuotes :: BParser a -> BParser a
doubleQuotes = between (char '"') (char '"')

brackets :: BParser a -> BParser a
brackets = between (symbol "[") (symbol "]")

bracketList :: BParser a -> BParser [a]
bracketList p = brackets (sepBy p (symbol ","))

lAngle :: BParser() 
lAngle = void $ symbol "<"

rAngle :: BParser() 
rAngle = void $ symbol ">"

angles :: BParser a -> BParser [a]
angles p = between lAngle rAngle (sepBy p (symbol ","))

angles1 :: BParser a -> BParser [a]
angles1 p = between lAngle rAngle (sepBy1 p (symbol ","))

csl :: BParser a -> BParser [a]
csl p = parens (sepBy p (symbol ","))

opCsl :: BParser a -> BParser [a]
opCsl p = parens (sepBy p (symbol ",")) <|> return []

inlineBlock :: BParser a -> BParser [a]
inlineBlock item = try (return <$> item) <|> block item

block :: BParser b -> BParser [b] 
block item = do
    oldlvl <- getIndent
    indentSC
    newlvl <- getIndent
    if newlvl > oldlvl 
        then indentedItems oldlvl newlvl item
        else fail $ "Not indented old " ++ show oldlvl ++ " new " ++ show newlvl

indentedItems :: Int -> Int -> BParser a -> BParser [a]
indentedItems ref lvl p = re lvl
    where go = indentSC >> getIndent >>= re
          re pos
             | pos <= ref = return []
             | pos == lvl = (:) <$> p <*> go
             | otherwise  = do
                    done <- optional eof
                    case done of
                        Just _ ->  return []
                        _ -> fail "Incorrect Indentation"
