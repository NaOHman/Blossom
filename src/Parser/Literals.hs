{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Parser.Literals where

import Parser.Core
import Models.Expressions
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

--------------------- Literal Parsers ---------------------------------
literal p = withPos $ lexeme $ tryList 
    [lFloat, lBool, lInt, lArray p, lTuple p, 
      lType, lCons p, lChar, lString]

-- Parses a Character literal
lType :: BParser Literal'
lType = LType <$> (uStr <* rword ".type")

lChar :: BParser Literal'
lChar = LChar <$> between (char '"') (char '"') myChar

-- Parses a String literal
lString :: BParser Literal'
lString = a2Cons <$> strChars
    where strChars = between (char '"') (char '"') (many posChar)
          posChar = lit2Expr <$> getPosition <*> (LChar <$> myChar)

lit2Expr :: SourcePos -> Literal' -> Expr
lit2Expr p l = Lex p $ ELit $ Lex p l

myChar = escapedChar <|> noneOf "'\"\\\n\t\r"

escapedChar :: BParser Char
escapedChar = char '\\' >> choice (zipWith escape codes reps) <?> "Bad escape code"
    where escape c r = char c >> return r
          codes = "ntr\\\"'" 
          reps = "\n\t\r\\\"'" 

-- Parses and Integer Literal
lInt = LInt <$> L.integer

-- Parses a float literal
lFloat = LFloat <$> (try sufflt <|> flt)
    where flt    = L.float
          sufflt = fromIntegral <$> L.integer <* char 'f'

-- TODO handle disambiguation e.g. List.Cons
lCons :: BParser Expr -> BParser Literal'
lCons p = LCons <$> uName <*> ps
    where ps = try (lStruct "(" ")" p) <|> return []

-- Parses an array literal
lArray p = a2Cons <$> lStruct "[" "]" p

-- Parses a set literal, empty brackets are presumed to be Dicts
{-lSet p = lset $ lStruct "{" "}" p-}
lSet = undefined

lBool :: BParser Literal'
lBool = true <|> false
    where true = rword "True" *> return (LCons "True" [])
          false = rword "False" *> return  (LCons "False" [])

-- Parses a Dict literal
{-lDict = ldict . lStruct "{" "}" . dictPair-}
lDict = undefined

-- Parses a Dictionary keyword pair
dictPair p = (,) <$> (p <* colon') <*> p

-- Parses a Tuple literal 
lTuple p = parens $ do 
    head <- p <* comma'
    tail <- sepBy1 p comma'
    let cstr = "(" ++ replicate (1 + length tail) ',' ++ ")"
    return $ LCons cstr (head:tail)

-- Parses a generic literal struct
lStruct s e = between (symbol s) (symbol e) . (`sepBy` comma')

a2Cons :: [Expr] -> Literal'
a2Cons = foldl cons nil 
    where cons l e@(Lex p _) = LCons "[cons]" [e, lit2Expr p l]
          nil = LCons "[nil]" []
