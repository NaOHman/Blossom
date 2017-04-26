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
literal = lexeme $ choice [lFloat, lInt, lChar, lNull, lBool]

lFloat :: BParser Literal
lFloat = LFloat <$> negatable (choice [suffix, decimalFirst, L.float])
    where suffix = try (fromIntegral <$> L.integer <* char 'f')
          decimalFirst = do
            text <- dot_ *> some digitChar
            return $ read $ "0." ++ text

lInt :: BParser Literal
lInt = LInt <$> negatable L.integer

lChar :: BParser Literal
lChar = LChar <$> singleQuotes charChar

lBool :: BParser Literal
lBool = LBool True <$ true_ <|> LBool False <$ false_

lNull :: BParser Literal
lNull = LNull <$ symbol "?"

negatable :: Num a => BParser a -> BParser a
negatable parser = try $ do 
    doNegate <- optional minus_
    num <- parser
    case doNegate of
        Just _ -> return $ negate num
        _ -> return num
