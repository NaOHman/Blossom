{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Statements where

import ParserUtils
import Models
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S

-- Literal Parsers

expr = literal

literal = try lFloat 
       <|> try lInt 
       <|> lArray 
       <|> lTuple 
       <|> lChar
       <|> lString
       <|> try lDict 
       <|> try lSet

lChar = LChar <$> between (char '\'') (char '\'') (escapedChar ecs ers)
    where ecs = zip "'\\trn" "'\\\t\r\n"
          ers = "\\\n\r"

lString = LString <$> between (char '"') (char '"') (many (escapedChar ecs ers))
    where ecs = zip "\"\\trn" "\"\\\t\r\n"
          ers = "\\\n\r\""

lInt = LInt <$> L.integer

lFloat = LFloat <$> (try sufflt <|> flt)
    where flt    = L.float
          sufflt = fromIntegral <$> L.integer <* char 'f'

lArray = LArray <$> lStruct '[' ']' expr

lTuple = LTuple <$> lStruct '(' ')' expr 

lSet = LSet <$> lStruct '{' '}' expr 

lDict = LDict <$> lStruct '{' '}' dictPair

dictPair = do
    key <- expr
    void $ symbol ":"
    value <- expr
    return (key, value)

lStruct s e p = between (char s) (char e) (sepBy p comma)

escapedChar escps errs = escape <|> noneOf errs
    where escape = do
                        char '\\'
                        c <- anyChar
                        case lookup c escps of
                            Just ec -> return ec
                            Nothing -> fail $ "invalid escape sequence \\" ++ show c
{-escpString delim escapes errs = do-}
    {-string delim-}
    {-str <- escapeParser escapes errs-}
    {-string delim-}


{-expression = functionCall <|> literal <|> name-}

{-vanillaFunctionCall = do-}
    {-func <- funcName-}
    {-args <- functionArgs-}
    {-void myEOL-}
    {-return FCall func args-}

{-infixFunctionCall = do-}
    {-firstArg <- lowerName-}
    {-void $ symbol "~"-}
    {-func <- funcName-}
    {-args <- oneArg <|> functionArgs <* myEOL-}
    {-return FCall func (firstArg:args)-}
    {-where oneArg = lowerName <* myEOL-}

{-rebindAssign = do-}
    {-assignVars <- lowerName-}
    {-void $ symbol "@"-}
    {-expr <- exprWithRebind assignVars-}
    {-return Assignment vars expr-}

{-vanillaAssign = do-}
    {-vars <- assignsVars-}
    {-void $ symbol "="-}
    {-expr <- expression-}
    {-return Assignment vars expr-}

{-parseCode :: String -> Either ParseError [String]-}
{-parseCode = parse code "(unknown)" -}

main = putStrLn "hello world"
{-main = do-}
    {-c <- getContents-}
    {-case parseCode c of-}
        {-Left e -> do putStrLn "Error Encountered:"-}
                     {-print e-}
        {-Right r -> mapM_ print r-}
