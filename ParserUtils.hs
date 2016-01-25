{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module ParserUtils where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import Data.List (intercalate) 
import qualified Text.Megaparsec.Lexer as L

--------------------------- General Declaration -----------------------
symbol = L.symbol sc

lexeme = L.lexeme sc

sc = L.space (void $ oneOf " \t") lineCmnt blockCmnt

indentSC = L.space (void spaceChar) lineCmnt blockCmnt

lineCmnt  = L.skipLineComment inlineComment
    where inlineComment = "//"

blockCmnt = L.skipBlockComment commentStart commentEnd
    where commentStart = "/*"
          commentEnd = "*/"

nonIndented = L.nonIndented indentSC
indentBlock = L.indentBlock indentSC

myReserves = ["if", "then", "else", "is", "while", "curry", "send", "send_wait", "MailBox", "fun", "when", "where", "because", "given", "and", "or", "not", "True", "False"]

is' = rword "is"
when' = rword "when"
where' = rword "where"
data' = rword "data"
case' = rword "case"
of' = rword "of"
arrow' = symbol "->"
equals' = symbol "="
comma' = symbol ","
colon' = symbol ":"

lName = identifier lowerChar
uName = identifier upperChar
aName = identifier letterChar
funName = rword "fun" *> lName
dataName = rword "data" *> uName
className = rword "is" *> uName

eol' = try (string "\n\r") <|> try (string "\r\n") 
        <|> string "\n" <|> string "\r"

parens = between (symbol "(") (symbol ")")

aBrackets = between (symbol "<") (symbol ">")

rword w = string w *> notFollowedBy alphaNumChar *> sc
identifier firstCharParser = p >>= rwcheck
    where p         = (:) <$> firstCharParser <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ "You can't use " ++ show x ++ " It's all mine"
                      else return x

nameChars = alphaNumChar <|> char '_'

tryList [p]    = try p
tryList (p:ps) = try p <|> tryList ps

topdec = nonIndented . genDec

genDec :: Parser a -> Parser b -> (a -> [b] -> c) -> Parser c
genDec header item cons = indentBlock $ do
    h <- header
    return $ L.IndentSome Nothing (return . cons h) item

indentSome :: MonadParsec s m Char => ([b] -> m a) -> m b -> m a
indentSome f p = indentBlock $ return $ 
                    L.IndentSome Nothing f p

genArgs ps = (lexeme . parens) . foldSepBy comma' (tryList' ps)
    where foldSepBy sep f acc = foldSepBy1 sep f acc <|> pure acc
          foldSepBy1 sep f acc = f acc >>= \a -> 
                    (sep *> foldSepBy1 sep f a) <|> pure a
          tryList' ps b = tryList (map ($b) ps)
