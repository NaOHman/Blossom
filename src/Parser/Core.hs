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
{-type BParser a = Parser a-}

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
    where indentConsumer = void (oneOf " \t") <|> eol'

lineCmnt  = L.skipLineComment inlineComment
    where inlineComment = "//"

blockCmnt = L.skipBlockComment commentStart commentEnd
    where commentStart = "/*"
          commentEnd = "*/"

nonIndented = L.nonIndented indentSC
indentBlock = L.indentBlock indentSC

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

eol' = void $ tryList $ map string ["\n\r", "\r\n", "\n", "\r"]

parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
csl p = between (symbol "(") (symbol ")") (sepBy p comma')

angles p = between (symbol "<") (symbol ">") (sepBy p comma')
angles1 p = between (symbol "<") (symbol ">") (sepBy1 p comma')

rword w = string w *> notFollowedBy nameChars *> sc

identifier = lexeme . rstring

rstring fcp =lexeme (p >>= rwcheck)
    where p = (:) <$> fcp <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ x ++ " is a reserved word"
                      else return x

nameChars = alphaNumChar <|> char '_'

tryList [p]    = try p
tryList (p:ps) = try p <|> tryList ps

{-indentGuard i = getPosition >>= \p -> -}
    {-when (sourceColumn p /= i) (fail "incorrect indentation")-}

{-iBlock p = try (pure <$> p) <|> try (block p)-}
    {-where onlyInline p = do-}
            {-oldlvl <- get -}
            {-result <- p-}
            {-done <- optional eof-}
            {-case done of -}
                {-Just _ -> return [result]-}
                {-Nothing -> do-}
                    {-indentSC-}
                    {-newlvl <- get-}
                    {-if newlvl <= oldlvl then-}
                        {-return [result]-}
                    {-else-}
                        {-fail "Improper indendation"-}

{-block p = indentBlock (return $ L.IndentSome Nothing return p)-}
{-block header item = L.indentBlock indentSC p -- <?> "Indent failed"-}
    {-where p = return $ L.IndentSome Nothing header item-}

block f header item = L.indentBlock indentSC p 
    where p = do h <- header
                 return (L.IndentSome Nothing (return . f h) item)

{-block :: BParser b -> BParser [b] -}
{-block item = do-}
    {-oldlvl <- get-}
    {-indentSC-}
    {-newlvl <- get-}
    {-if newlvl > oldlvl then-}
        {-indentedItems oldlvl newlvl item-}
    {-else -}
        {-fail $ "Not indented old " ++ show oldlvl ++ " new " ++ show newlvl-}

{-indentedItems :: Int -> Int -> BParser a -> BParser [a]-}
{-indentedItems ref lvl p = re lvl-}
    {-where go = indentSC >> get >>= re-}
          {-re pos-}
             {-| pos <= ref = return []-}
             {-| pos == lvl = (:) <$> p <*> go-}
             {-| otherwise  = do-}
                    {-done <- optional eof-}
                    {-case done of-}
                        {-Just _ ->  return []-}
                        {-otherwise -> fail "Incorrect Indentation"-}

parenCsl p = lexeme $ parens $ sepBy p comma'
