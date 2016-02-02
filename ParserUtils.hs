{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module ParserUtils where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import qualified Data.Text as T
import Data.Maybe (maybeToList)
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
    where indentConsumer = void (oneOf " \t") <|>  newIndent
          newIndent = eol' >> many (oneOf " \t") >>= (put . length)

lineCmnt  = L.skipLineComment inlineComment
    where inlineComment = "//"

blockCmnt = L.skipBlockComment commentStart commentEnd
    where commentStart = "/*"
          commentEnd = "*/"

nonIndented = L.nonIndented indentSC
{-indentBlock = L.indentBlock indentSC-}

myReserves = ["if", "then", "else", "elif", "is", "while", "curry", "send", "send_wait", "MailBox", "fun", "when", "where", "because", "given", "and", "or", "not", "True", "False", "case"]

is' = rword "is"
because' = rword "because"
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
csl p = between (symbol "(") (symbol ")") (sepBy p comma')

angles p = between (symbol "<") (symbol ">") (sepBy p comma')

rword w = string w *> notFollowedBy alphaNumChar *> sc
identifier firstCharParser = lexeme (p >>= rwcheck)
    where p         = (:) <$> firstCharParser <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ "You can't use " ++ show x ++ " It's all mine"
                      else return x

nameChars = alphaNumChar <|> char '_'

tryList [p]    = try p
tryList (p:ps) = try p <|> tryList ps

{-topdec a b = nonIndented (genDec a b)-}

{-genDec :: ([b] -> MyParser a) -> MyParser b -> MyParser a-}
{-genDec a b = indentBlock $ return $ L.IndentSome Nothing a b-}

indentGuard i = getPosition >>= \p -> 
    when (sourceColumn p /= i) (fail "incorrect indentation")

inlineOrBlock p = try (block p) <|> try (onlyInline p)
    where onlyInline p = do
            oldlvl <- get 
            result <- p
            done <- optional eof
            case done of 
                Just _ -> return [result]
                Nothing -> do
                    indentSC
                    newlvl <- get
                    if newlvl <= oldlvl then
                        return [result]
                    else
                        fail "Improper indendation"


block :: MyParser b -> MyParser [b] 
block item = do
    oldlvl <- get
    indentSC
    newlvl <- get
    if newlvl > oldlvl then
        indentedItems oldlvl newlvl item
    else 
        fail $ "Not indented old " ++ show oldlvl ++ " new " ++ show newlvl

topParser :: MyParser b -> MyParser [b]
topParser =  indentedItems (-1) 0 

indentedItems :: Int -> Int -> MyParser a -> MyParser [a]
indentedItems ref lvl p = re lvl
    where go = indentSC >> get >>= re
          re pos
            | pos <= ref = return []
            | pos == lvl = (:) <$> p <*> go
            | otherwise  = do
                    done <- optional eof
                    case done of
                        Just _ ->  return []
                        otherwise -> fail "Incorrect Indentation"

parenCsl p = lexeme $ parens $ sepBy p comma'
