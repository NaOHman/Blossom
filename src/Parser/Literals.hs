{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Parser.Literals (literal, escapedChar) where

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
