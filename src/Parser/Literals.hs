{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

{- |
Module      : Parser.Literals
Description : A Module that provides parsers for literal values
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module parses parsers literals 
-}


module Parser.Literals (literal) where

import Parser.Core
import Language.Expressions
import qualified Text.Megaparsec.Lexer as L

--------------------- Literal Parsers ---------------------------------
literal :: BParser Literal
literal = lexeme $ try lFloat <|> lInt <|> lChar <|> lNull <|> try lBool

lFloat :: BParser Literal
lFloat = LFloat <$> (try sufflt <|> flt)
    where flt    = L.float
          sufflt = fromIntegral <$> L.integer <* char 'f'

lInt :: BParser Literal
lInt = LInt <$> L.integer

lChar :: BParser Literal
lChar = LChar <$> singleQuotes (escapedChar <|> noneOf "'\\\n\t\r")

lBool :: BParser Literal
lBool = try (LBool True <$ true_) <|> try (LBool False <$ false_)

lNull :: BParser Literal
lNull = LNull <$ symbol "?"
