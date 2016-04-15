{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Parser.Literals (literal) where

import Parser.Core
import Models.Expressions
import qualified Text.Megaparsec.Lexer as L

--------------------- Literal Parsers ---------------------------------
literal = lexeme $ try lFloat <|> lInt <|> lChar <|> try lBool

lFloat = LFloat <$> (try sufflt <|> flt)
    where flt    = L.float
          sufflt = fromIntegral <$> L.integer <* char 'f'

lInt = LInt <$> L.integer

lChar = LChar <$> singleQuotes (escapedChar <|> noneOf "'\\\n\t\r")

lBool = try (LBool True <$ true_) <|> try (LBool False <$ false_)

lNull = LNull <$ symbol "?"

escapedChar :: BParser Char
escapedChar = char '\\' >> choice (zipWith escape codes reps) <?> "Bad escape code"
    where escape c r = char c >> return r
          codes = "ntr\\\"'" 
          reps = "\n\t\r\\\"'" 

--------------------- Sugar ------------------------------------------

lString :: BParser Expr
lString = toList <$> doubleQuotes (many stringChar)
    where stringChar = Lit . LChar <$> (escapedChar <|> noneOf "\"\\\n\t\r")

lArray p = toList <$> brackets (sepBy p comma_)

-- Parses a Tuple literal 
lTuple p = do 
    head <- p <* comma_
    tail <- sepBy1 p comma_
    let cstr = Var $ "(" ++ replicate (1 + length tail) ',' ++ ")"
    return $ foldl Ap cstr (head:tail)

toList = foldl cons (Var "[nil]") 
    where cons e = Ap (Ap (Var "[cons]") e )
