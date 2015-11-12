{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module ParserUtils where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

noBreakSpace = "\t "

commentStart = "/*"

commentEnd = "*/"

inlineComment = "//"

myReserves = ["if", "then", "else", "is", "while", "curry", "send", "send_wait", "MailBox", "fun", "when", "where", "because", "given", "and", "or", "not"]

myOps = ["<",">","<=",">=","+","-","/","//","%", "=", "==", "and", "or", "not"]

myEOL = try (string "\n\r") <|> try (string "\r\n") 
        <|> string "\n" <|> string "\r"

inlineSpace = try continuation <|> some (oneOf noBreakSpace)

lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer

comma = symbol ","

parens = between (symbol "(") (symbol ")")

lowerName = identifier lowerChar

name = identifier letterChar

funcName = lowerName

upperName = identifier upperChar

rword w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

identifier firstCharParser = lexeme (p >>= rwcheck)
    where p         = (:) <$> firstCharParser <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ "You can't use " ++ show x ++ " It's all mine"
                      else return x

nameChars = alphaNumChar <|> char '_'

continuation = do
    many $ oneOf noBreakSpace
    char '\\' 
    many $ oneOf noBreakSpace
    myEOL 
    many $ oneOf noBreakSpace

spaceConsumer = L.space (void inlineSpace) lineCmnt blockCmnt
    where lineCmnt  = L.skipLineComment inlineComment
          blockCmnt = L.skipBlockComment commentStart commentEnd
