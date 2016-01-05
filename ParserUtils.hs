{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module ParserUtils where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import Data.List (intercalate) 
import qualified Text.Megaparsec.Lexer as L

--------------------------- General Declaration -----------------------
genDec header ending blk = h <*> indentBlock 0 blk
    where h = indentGuard 0 *> header <* endline (string ending)

genArgs :: (MonadParsec s m Char) => [b -> m b] -> b -> m b
genArgs ps = (miLexeme . parens) . foldSepBy comma' (tryList' ps)

foldSepBy sep f acc = foldSepBy1 sep f acc <|> pure acc

foldSepBy1 sep f acc = f acc >>= \a -> (sep *> foldSepBy1 sep f a) <|> pure a

tryList' ps b = tryList (map ($b) ps)

showCsl f = intercalate ", " . map f
showNsl f = intercalate "\n" . map f
showStruct = showCsl show
showDict = showCsl $ \(a,b) -> show a ++ ": " ++ show b

noBreakSpace = "\t "

commentStart = "/*"

commentEnd = "*/"

inlineComment = "//"

line p = p <* anySpace <* eol'

myReserves = ["if", "then", "else", "is", "while", "curry", "send", "send_wait", "MailBox", "fun", "when", "where", "because", "given", "and", "or", "not"]

is' = (iString "is" *>)
dta' = (iString "data" *>)
fun' = iString "fun"
arrow' = miString "->"

opChars = "<>=+-/%@~*!?:.,"

eol' = try (string "\n\r") <|> try (string "\r\n") 
        <|> string "\n" <|> string "\r"

inlineSpace = try continuation <|> some (oneOf noBreakSpace)

maybeISpace = many inlineSpace

comma' = char ',' >> anySpace

miColon = miChar ':'
miEquals = miChar '='

endline p = p <* maybeISpace <* (lineCmnt' <|> void eol')

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

p1 a b = a <$> b
p2 a b c = a <$> b <*> c
p3 a b c d = a <$> b <*> c <*> d
p4 a b c d e = a <$> b <*> c <*> d <*> e
p5 a b c d e f = a <$> b <*> c <*> d <*> e <*> f

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
        tail <- endBy (indentGuard lvl *> p) eol'
        return $ head:tail)
     
tryList [p]    = try p
tryList (p:ps) = try p <|> tryList ps

nextIndentLevel = do
    many $ try whiteLine
    firstWs <- many wsChar
    return $ length firstWs
    where notWs = notFollowedBy $ noneOf noBreakSpace
