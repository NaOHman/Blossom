{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

spaceConsumer = L.space (void inlineSpace) lineCmnt blockCmnt
    where lineCmnt  = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"

lexeme = L.lexeme spaceConsumer

symbol = L.symbol spaceConsumer

parens = between (symbol "(") (symbol ")")

nameChars = alphaNumChar <|> char '_'

upperName = lowerChar *> many nameChars

lowerName = upperChar *> many nameChars

code = stmt <* eof

stmt = parens

inlineSpace = try continuation <|> some (oneOf noBreakSpace)

continuation = do
    many $ oneOf noBreakSpace
    char '\\' 
    many $ oneOf noBreakSpace
    myEOL 
    many $ oneOf noBreakSpace

myEOL = try (string "\n\r") <|> try (string "\r\n") 
        <|> string "\n" <|> string "\r"

noBreakSpace = "\t "

{-parseCode :: String -> Either ParseError [String]-}
{-parseCode = parse code "(unknown)" -}

main = putStrLn "hello world"
{-main = do-}
    {-c <- getContents-}
    {-case parseCode c of-}
        {-Left e -> do putStrLn "Error Encountered:"-}
                     {-print e-}
        {-Right r -> mapM_ print r-}

commentStart = "/*"

commentEnd = "*/"

myReserves = ["if", "then", "else", "is", "while", "curry", "send", "send_wait", "MailBox", "fun", "when", "where", "because", "given"]

myOps = ["<",">","<=",">=","+","-","/","//","%", "=", "==", "and", "or", "not"]
