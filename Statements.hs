{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Statements where

import ParserUtils
import Models
import Control.Monad (void, foldM)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

---------------------------------- Top Level Parser ---------------------------
-- exp parses a statement which may have a value
expr = literal 

-- literal parses a literal eg integer, float, string etc.
literal = try lFloat 
       <|> try lInt 
       <|> lArray 
       <|> lTuple 
       <|> lChar
       <|> lString
       <|> try lDict 
       <|> try lSet

-- function declarations parse the headers of function 
functionDec = try namedFunction <|> try anonFunDec

{-statement = expr <|> assignment-}

{-assignment = line $ eqAssign <|> rebindAssignt-}
    
{-functionCall = infixCall <|> vanillaCall-}

------------------------- Function Declaration Parsers -------------------------

-- parses a named function declared with the keyword fun
namedFunction = do
    iString "fun"
    name <- miLName
    fun <- anonFunDec
    return $ fun {fName = Just name}

-- parses an anonymous function declared like so (arg1,arg2) -> code
anonFunDec = do
    args <- argDec
    cons <- constraint
    string "->"
    return $ anonFDec args cons

-- Parses function arguments
argDec = parens (sepBy argDec' comma') >>= reduceArgs
    where argDec' = try ksplat <|> try psplat <|> try kwArg <|> pArg
          ksplat = splat "**" KSplat
          psplat = splat "*" PSplat
          reduceArgs = foldM newArg defArgs

-- Parses a keyword argument eg count=0
kwArg = do
    n <- miLName
    miChar '='
    def <- miLexeme literal
    cns <- mConstraint
    return $ KArg n def cns 

-- parses a positional argument
pArg = do
    n <- miLName
    cns <- mConstraint
    return $ PArg n cns

-- parses a splat argument, either *arg or **arg
splat s cons = do 
    miString s 
    n <- miLName
    cns <- mConstraint
    return $ cons n cns

------------------------------ Constraint Parsing -----------------------------

-- parses a constraint if it exists, otherwise returns none
mConstraint = (miString "::" *> miLexeme constraint) <|> return None

-- parses a constraint of fails
constraint = try vanillaCons <|> funCons

-- parses a functional constraint ie <T1,T2>::T3
funCons = do
    args <- consParams
    rt <- optional constraint
    return $ makeFunCons args rt

-- parses a non functional constraint ie T1<T2,T3>
vanillaCons = do
    name <- miName
    ps <- optional consParams
    return $ makeCons name (concat ps)

-- Parses constraint parameters, ie the stuff between angle brackets
consParams = miLexeme $ aBrackets commaSepCs 
    where commaSepCs = constraint `sepBy` comma'

--------------------------- Data Declaration Parsing --------------------------

-- Parses a data declaration
dataDec = do
    indentGuard 0
    iString "data"
    name <- miUName
    params <- fromMaybe [] <$> optional consParams
    colon'
    eol'
    cons <- indentBlock 0 constructor
    return $ Data name params []
   
-- Parses a constructor
constructor = miUName

--------------------- Literal Parsers ---------------------------------

-- Parses a Character literal
lChar = LChar <$> between (char '\'') (char '\'') (escapedChar ecs ers)
    where ecs = zip "'\\trn" "'\\\t\r\n"
          ers = "\\\n\r"

-- Parses a String literal
lString = LString <$> between (char '"') (char '"') (many (escapedChar ecs ers))
    where ecs = zip "\"\\trn" "\"\\\t\r\n"
          ers = "\\\n\r\""

-- Parses and Integer Literal
lInt = LInt <$> L.integer

-- Parses a float literal
lFloat = LFloat <$> (try sufflt <|> flt)
    where flt    = L.float
          sufflt = fromIntegral <$> L.integer <* char 'f'

-- Parses an array literal
lArray = LArray <$> lStruct '[' ']' expr

-- Parses a Tuple literal TODO tuples must have more than one element
lTuple = LTuple <$> lStruct '(' ')' expr 

-- Parses a set literal, empty brackets are presumed to be Dicts
lSet = LSet <$> lStruct '{' '}' expr 

-- Parses a Dict literal
lDict = LDict <$> lStruct '{' '}' dictPair

-- Parses a Dictionary keyword pair
dictPair = do
    key <- expr <* anySpace
    colon' 
    value <- expr <* anySpace
    return (key, value)

-- Parses a generic literal struct
lStruct s e p = between (char s) (char e) (sepBy p' comma')
    where p' = p <* anySpace

-- Handles character escapes in literals
escapedChar escps errs = escape <|> noneOf errs
    where escape = do
                        char '\\'
                        c <- anyChar
                        case lookup c escps of
                            Just ec -> return ec
                            Nothing -> fail $ "invalid escape sequence \\" ++ show c

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


{-parseCode :: String -> Either ParseError [String]-}
{-parseCode = parse code "(unknown)" -}

test = runParser functionDec "cmdline"
main = putStrLn "hello world"
{-main = do-}
    {-c <- getContents-}
    {-case parseCode c of-}
        {-Left e -> do putStrLn "Error Encountered:"-}
                     {-print e-}
        {-Right r -> mapM_ print r-}
