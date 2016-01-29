{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module ParserUtils where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import qualified Data.Text as T
import Data.List (intercalate) 
import Control.Monad.State
import Control.Monad.Trans
import qualified Text.Megaparsec.Lexer as L

--------------------------- General Declaration -----------------------

type MyParser a = StateT Int (Parsec String) a

{-symbol :: MyParser String-}

symbol :: String -> MyParser String
symbol = L.symbol sc 

lexeme :: MyParser a -> MyParser a
lexeme = L.lexeme sc

sc :: MyParser ()
sc = L.space (void $ oneOf " \t") lineCmnt blockCmnt

indentSC :: MyParser ()
indentSC = L.space indentConsumer lineCmnt blockCmnt
    where indentConsumer = void spaceChar

lineCmnt  = L.skipLineComment inlineComment
    where inlineComment = "//"

blockCmnt = L.skipBlockComment commentStart commentEnd
    where commentStart = "/*"
          commentEnd = "*/"

nonIndented = L.nonIndented indentSC
indentBlock = L.indentBlock indentSC

myReserves = ["if", "then", "else", "elif", "is", "while", "curry", "send", "send_wait", "MailBox", "fun", "when", "where", "because", "given", "and", "or", "not", "True", "False"]

is' = rword "is"
if_ = rword "if"
elif = rword "elif"
then_ = rword "then"
else_ = rword "else"
when' = rword "when"
where_ = rword "where"
data_ = rword "data"
case_ = rword "case"
of_ = rword "of"
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
identifier firstCharParser = lexeme (p >>= rwcheck)
    where p         = (:) <$> firstCharParser <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ "You can't use " ++ show x ++ " It's all mine"
                      else return x

nameChars = alphaNumChar <|> char '_'

tryList [p]    = try p
tryList (p:ps) = try p <|> tryList ps

topdec a b = nonIndented (genDec a b)

genDec :: ([b] -> MyParser a) -> MyParser b -> MyParser a
genDec a b = indentBlock $ return $ L.IndentSome Nothing a b

parenCsl p = lexeme $ parens $ sepBy p comma'
