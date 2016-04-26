{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-} 
module Parser.Lexeme 
   ( BParser()
   , symbol
   , lexeme
   , opList
   , block
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
    where indentConsumer = void (oneOf " \t") <|> eol_

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
