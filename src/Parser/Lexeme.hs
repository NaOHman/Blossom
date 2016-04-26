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
   , angles
   , angles1
   , csl
   , opCsl
   , eol_
   , nonIndented
   ) where

import Text.Megaparsec as X
import qualified Data.Foldable as F
import Control.Monad.State
import qualified Text.Megaparsec.Lexer as L

--------------------------- General Declaration -----------------------

type BParser a = StateT (Int,Int) (Parsec String) a

getIndent = snd <$> get

savePos :: BParser ()
savePos = do
    (tv,_) <- get 
    pos <- getPosition
    put (tv, sourceColumn pos)

getTV = fst <$> get
putTV i = do 
    (_,p) <- get
    put (i,p)

symbol :: String -> BParser String
symbol = L.symbol sc 


opList p = F.concat <$> optional p

lexeme :: BParser a -> BParser a
lexeme = L.lexeme sc

sc :: BParser ()
sc = L.space (void $ oneOf " \t") lineCmnt blockCmnt

indentSC :: BParser ()
indentSC = L.space indentConsumer lineCmnt blockCmnt
    where indentConsumer = void (oneOf " \t") <|> void newIndent
          newIndent = eol_ >> many (oneOf " \t") >> savePos

lineCmnt  = L.skipLineComment inlineComment
    where inlineComment = "//"

blockCmnt = L.skipBlockComment commentStart commentEnd
    where commentStart = "/*"
          commentEnd = "*/"

nonIndented = L.nonIndented indentSC
indentBlock = L.indentBlock indentSC

eol_ = void $ try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r"

parens = between (symbol "(") (symbol ")")
singleQuotes = between (char '\'') (char '\'')
doubleQuotes = between (char '"') (char '"')
brackets = between (symbol "[") (symbol "]")
angles p = between (symbol "<") (symbol ">") (sepBy p (symbol ","))
angles1 p = between (symbol "<") (symbol ">") (sepBy1 p (symbol ","))
csl p = parens (sepBy p (symbol ","))

opCsl p = parens (sepBy p (symbol ",")) <|> return []

inlineBlock item = try (return <$> item) <|> block item

block :: BParser b -> BParser [b] 
block item = do
    oldlvl <- getIndent
    indentSC
    newlvl <- getIndent
    if newlvl > oldlvl then
        indentedItems oldlvl newlvl item
    else 
        fail $ "Not indented old " ++ show oldlvl ++ " new " ++ show newlvl

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
