{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module ParserUtils where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

noBreakSpace = "\t "

commentStart = "/*"

commentEnd = "*/"

inlineComment = "//"

line p = p <* anySpace <* eol'

myReserves = ["if", "then", "else", "is", "while", "curry", "send", "send_wait", "MailBox", "fun", "when", "where", "because", "given", "and", "or", "not"]

opChars = "<>=+-/%@~*!?:.,"

eol' = try (string "\n\r") <|> try (string "\r\n") 
        <|> string "\n" <|> string "\r"

inlineSpace = try continuation <|> some (oneOf noBreakSpace)

maybeISpace = many inlineSpace

comma' = char ',' >> anySpace

miString = miLexeme . string
miChar  = miLexeme . char
miName = miLexeme name
miLName = miLexeme lowerName
miUName = miLexeme upperName

miLexeme :: MonadParsec s m Char => m a -> m a
miLexeme p = p <* maybeISpace

iString = iLexeme . string
iChar  = iLexeme . char
iLName = iLexeme lowerName
iUName = iLexeme upperName

iLexeme :: MonadParsec s m Char => m a -> m a
iLexeme p = p <* inlineSpace

colon' = miChar ':'

anySpace = void . many $
             void inlineSpace 
         <|> lineCmnt'
         <|> blockCmnt
         <|> void eol'

parens = between (miChar '(') (miChar ')')

aBrackets = between (miChar '<') (miChar '>')

lowerName = identifier lowerChar

name = identifier letterChar

funcName = lowerName

upperName = identifier upperChar

rword w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

identifier firstCharParser = p >>= rwcheck
    where p         = (:) <$> firstCharParser <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ "You can't use " ++ show x ++ " It's all mine"
                      else return x

nameChars = alphaNumChar <|> char '_'

continuation = do
    many $ oneOf noBreakSpace
    char '\\' 
    many $ oneOf noBreakSpace
    eol' 
    many $ oneOf noBreakSpace

spaceConsumer = L.space (void inlineSpace) lineCmnt blockCmnt

lineCmnt  = L.skipLineComment inlineComment
lineCmnt' = L.skipLineComment inlineComment >> void eol'
blockCmnt = L.skipBlockComment commentStart commentEnd

whiteLine = many inlineSpace *> eol'
indentGuard n = count n wsChar >> notFollowedBy wsChar
wsChar = oneOf noBreakSpace

indentBlock n p = do
    lvl <- nextIndentLevel
    if lvl <= n
    then fail "Expecting indented block, didn't find one"
    else (do  
        head <- p <* eol'
        tail <- sepBy (indentGuard lvl *> p) eol'
        return $ head:tail)
     
nextIndentLevel = do
    many $ try whiteLine
    firstWs <- many wsChar
    return $ length firstWs
    where notWs = notFollowedBy $ noneOf noBreakSpace
