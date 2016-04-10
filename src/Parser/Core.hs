{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-} 
module Parser.Core where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import qualified Data.Text as T
import qualified Data.Foldable as F
import Data.Maybe (maybeToList)
import Data.List (intercalate) 
import Control.Monad.State
import Control.Monad.Trans
import qualified Text.Megaparsec.Lexer as L

--------------------------- General Declaration -----------------------

type BParser a = StateT Int (Parsec String) a

symbol :: String -> BParser String
symbol = L.symbol sc 

gp = getPosition

opList p = F.concat <$> optional p
lexeme :: BParser a -> BParser a
lexeme = L.lexeme sc

sc :: BParser ()
sc = L.space (void $ oneOf " \t") lineCmnt blockCmnt

indentSC :: BParser ()
indentSC = L.space indentConsumer lineCmnt blockCmnt
    where indentConsumer = void (oneOf " \t") <|>  newIndent
          newIndent = eol' >> many (oneOf " \t") >>= (put . length)

lineCmnt  = L.skipLineComment inlineComment
    where inlineComment = "//"

blockCmnt = L.skipBlockComment commentStart commentEnd
    where commentStart = "/*"
          commentEnd = "*/"

nonIndented = L.nonIndented indentSC

myReserves = ["if", "then", "else", "elif", "is", "while", "curry", "send", "send_wait", "MailBox", "fun", "when", "where", "because", "given", "and", "or", "not", "True", "False", "case", "type", "inherits", "Given"]

is' = rword "is"
because' = rword "because"
inherits = rword "inherits"
given = rword "Given"
if_ = rword "if"
elif = rword "elif"
then_ = rword "then"
else_ = rword "else"
when' = rword "when"
where_ = rword "where"
data_ = rword "data"
case_ = rword "case"
type_ = rword ".type"
of_ = rword "of"
arrow' = symbol "->"
equals' = symbol "="
comma' = symbol ","
dot' = symbol "."
colon' = symbol ":"

lName = identifier lowerChar
uName = identifier upperChar
aName = identifier letterChar
fun' = rword "fun" 
dataName = rword "data" *> uName
className = rword "is" *> uName

eol' = try (string "\n\r") <|> try (string "\r\n") 
        <|> string "\n" <|> string "\r"

parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
csl p = between (symbol "(") (symbol ")") (sepBy p comma')

angles p = between (symbol "<") (symbol ">") (sepBy p comma')
angles1 p = between (symbol "<") (symbol ">") (sepBy1 p comma')

rword w = string w *> notFollowedBy alphaNumChar *> sc
identifier = lexeme . rstring
rstring firstCharParser = p >>= rwcheck
    where p= (:) <$> firstCharParser <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ x ++ " is a reserved word"
                      else return x

nameChars = alphaNumChar <|> char '_'

tryList [p]    = try p
tryList (p:ps) = try p <|> tryList ps

indentGuard i = getPosition >>= \p -> 
    when (sourceColumn p /= i) (fail "incorrect indentation")

iBlock p = try (block p) <|> try (onlyInline p)
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


block :: BParser b -> BParser [b] 
block item = do
    oldlvl <- get
    indentSC
    newlvl <- get
    if newlvl > oldlvl then
        indentedItems oldlvl newlvl item
    else 
        fail $ "Not indented old " ++ show oldlvl ++ " new " ++ show newlvl

topParser :: BParser b -> BParser [b]
topParser p = re 1
    where go = indentSC >> get >>= re
          re pos
             | pos <= 1 = (:) <$> p <*> go
             | otherwise  = do
                    done <- optional eof
                    case done of
                        Just _ ->  return []
                        otherwise -> do
                            fail $ "Incorrect Indentation " ++ show pos

indentedItems :: Int -> Int -> BParser a -> BParser [a]
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
