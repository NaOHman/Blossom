{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Functions 
    ( FunctionDec(..)
    , Function
    , Args(..)
    , namedFunction
    , anonFunDec
    )
where

import ParserUtils
import Constraints
import Control.Monad 
import Data.List 
import Data.Maybe 
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import Text.Megaparsec.Char

posAfterKw = "You can't define positional arugment that come after keyword arguments"
posAfterS = "You can't define positional argument after you've defined an argument that aggregates a list"
multiplePS = "You can't define multiple argument that aggregate lists"
multipleKS = "You can't define multiple arguments that aggregate keyword arugments"

data FunctionDec l = FunctionDec 
                      { fName :: Maybe String
                      , args :: Args l
                      , returnType :: Constraint
                      }


data Function e l = Function (FunctionDec l) e

data Args l = Args { positional :: [(String, Constraint)]
                 , keyword :: [(String, l, Constraint)]
                 , arrArgs :: Maybe (String, Constraint)
                 , kwArgs :: Maybe (String, Constraint)
                 }

defArgs = Args [] [] Nothing Nothing

------------------------- Function Declaration Parsers -------------------------

function :: Monad m => m b -> m a -> (a -> a -> a) -> m (Function a b)
function lit ex c = Function <$> functionDec lit <*> fBody
    where  fBody = (try blockBody) <|> (try ex)
           blockBody = do
                eol'
                n <- nextIndentLevel
                condense <$> indentBlock n ex
           condense [x] = x
           condense (x:xs) = c x (condense xs)

functionDec lit = try (namedFunction lit) <|> try (anonFunDec lit)

-- parses a named function declared with the keyword fun
namedFunction lit = do
    name <- iString "fun" *> miLName
    fun <- anonFunDec lit
    return $ fun {fName = Just name}

-- parses an anonymous function declared like so (arg1,arg2) -> code
anonFunDec lit = FunctionDec Nothing <$> argDec lit <*> (opCons <* arrow')


-- Parses function arguments
argDec lit = genArgs [addK lit, addP, addPS, addKS] defArgs
    
addK lit a = do
    n <- miLName
    v <- miEquals *> lit
    c <- opCons
    return  $ a {keyword = (n,v,c):keyword a}

addP a = do 
    n <- miLName
    c <- opCons
    case arrArgs a of
        Nothing -> case keyword a of
             [] -> return $ a {positional = (n,c):positional a}
             _  -> fail posAfterKw
        Just _  -> fail posAfterS

addPS a = do
    n <- miString "*" *> miLName
    c <- opCons
    case arrArgs a of
        Nothing -> return $ a {arrArgs = Just (n,c)}
        Just _  -> fail multiplePS

addKS a = do
    n <- miString "**" *> miLName
    c <- opCons
    case kwArgs a of
        Nothing -> return $ a {kwArgs = Just (n, c)}
        Just _  -> fail multipleKS

{-instance Show FunctionDec where-}
    {-show (FunctionDec n args cons) = "Function: " ++ showName n ++-}
            {-"\nArgs:\n" ++ show args ++ "\nReturns: " ++ show cons-}
        {-where showName (Just n) = n-}
              {-showName Nothing = "(Anonymous)"-}

{-instance Show Args where-}
    {-show (Args pos kw sp sk) = intercalate "\n" $ catMaybes [ -}
        {-showP     "Positional Args: " pos,-}
        {-showK     "Keyword Args: " kw, -}
        {-showSplat "Array Agregator: " sp, -}
        {-showSplat "Keyword Aggregator: " sk]-}

{-showP _ [] = Nothing-}
{-showP d as = Just $ d ++ intercalate ", " (map showArg as)-}

{-showK _ [] = Nothing-}
{-showK d as = Just $ d ++ intercalate ", " (map showKArg as)-}
{-showKArg (n,v,c) = n ++ "=" ++ show v ++ " ::" ++ show c-}

{-showSplat d = fmap ((d++) . showArg)-}
{-showArg (n,c) = n ++ " :: " ++  show c-}
