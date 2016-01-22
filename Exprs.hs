{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Exprs where

import ParserUtils
import Control.Monad (void, foldM, ap)
import Constraints
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

type Id = String

data Program = Program { 
      functions :: [Expr], 
      globals   :: [Expr],
      datatypes :: [Data],
      classes   :: [Class] 
    }
    deriving Show
    -- todo type aliases and error types

data Data = Dta | NoData
    deriving Show
data Class = Cls | NCls
    deriving Show

data Pat where
    DName :: Id -> Pat
    DCons :: Id -> [Pat] -> Pat
    DNil  :: Pat

data Expr  where
    ELit    :: Literal -> Expr
    EVar    :: Id -> Expr
    ELambda :: Expr -> Expr
    EFCall  :: Call -> Expr
    ECons   :: Call -> Expr --special case of function application
    EFix    :: FunctionDec -> Expr -> Expr
    ELet    :: Pat -> Expr -> Expr -- EChain ~= in
    ECase   :: Expr -> [(Pat, Expr)] -> Expr
    EChain  :: Expr -> Expr -> Expr
    deriving Show

data Call = Call {
    fname :: String,
    cpArgs :: [Expr],
    ckwArgs :: [(String, Expr)]
    }


data FunctionDec = FunctionDec 
                      { fName :: Maybe String
                      , args :: Args
                      , returnType :: Constraint
                      }
  deriving Show

data Args = Args { positional :: [(String, Constraint)]
                 , keyword :: [(String, Literal, Constraint)]
                 , arrArgs :: Maybe (String, Constraint)
                 , kwArgs :: Maybe (String, Constraint)
                 }
    deriving Show


program = condense (Program [] [] [] []) <$> sepBy topExprs anySpace
    where topExprs = try function <|> try elet 
          condense p []                = p
          condense p (e@(EFix _ _):es) = condense (p {functions = e:functions p}) es
          condense p (e@(ELet _ _):es) = condense (p {globals = e:globals p}) es

------------------------- Basic Expr Parsing -------------------------------
--

fcall    = call EFCall miLName
consCall = call ECons  miUName
call cons np = cons <$> do 
    name <- np
    genArgs [fArg, kwArg] (Call name [] [])
    where fArg (Call n p k ) = fmap (\a -> Call n (p++[a]) k) expr
          kwArg (Call n p k) = fmap (\a -> Call n p (k++[a])) kwTpl
          kwTpl = (,) <$> miLName <*> (miString "=" *> expr)

whiteLine = many inlineSpace *> eol'

fblock n = do
    lvl <- nextIndentLevel
    if lvl <= n
    then fail "Expecting indented block, didn't find one"
    else (do  
        head <- expr <* eol'
        tail <- endBy (indentGuard lvl *> expr) eol'
        return (condense (head:tail)))
    where condense [e]    = e
          condense (e:es) = EChain e (condense es)
 
elet = ELet <$> decons <*> (miString "=" *> expr)

decons = name' <|> dec' <|> nil' <|> parens decons
    where dec' = DCons <$> miUName <*> parens (decons `sepBy` comma')
          name' = DName <$> miLName
          nil' = miString "_" >> return DNil

term = tryList [elet, fcall, consCall, eLiteral, var, parens expr]

var = EVar <$> miLName

expr = makeExprParser term operators <?> "expression"

miLiteral = miLexeme literal
eLiteral = ELit <$> miLiteral
operators = [[miuOp "+", miuOp "-"],
         [mibOp "*", mibOp "/", mibOp "//", mibOp "%"],
         [mibOp "+", mibOp "%"],
         [mibOp "==", mibOp "<", mibOp "<=", mibOp ">", mibOp ">="],
         [iuOp "not"],
         [ibOp "and", ibOp "or", ibOp "xor"]]

bOp p s = InfixL (p s *> pure (\a b -> EFCall $ Call s [a,b] []))
uOp p s = Prefix (p s *> pure (\a -> EFCall $ Call s [a] []))
mibOp = bOp miString 
ibOp = bOp iString 
miuOp = uOp miString 
iuOp = uOp iString 


instance Show Pat where
    show (DCons n as) = n ++ intercalate "," (map show as)
    show (DName n) = n
    show (DNil) = "_"

instance Show Call where
    show (Call n p k) = "call " ++ n ++ "(" ++ showP p ++ "("++ showK k ++ "))"
        where showP = intercalate ", " . map show
              showK = intercalate ", " . map (\(n,v) -> n ++ "=" ++ show v)

data Literal where
    LChar   :: Char -> Literal
    LString :: String -> Literal
    LInt    :: Integer -> Literal
    LFloat  :: Double -> Literal
    LBool   :: Bool -> Literal
    LArray  :: [Expr] -> Literal
    {-LTuple  :: [Literal] -> Literal-}
    LDict   :: [(Expr,Expr)]  -> Literal
    LSet    :: [Expr] -> Literal

--------------------- Literal Parsers ---------------------------------

literal = tryList [lFloat, lBool, lInt, lArray, lChar, lString, lDict, lSet]

-- Parses a Character literal
lChar = LChar <$> between quote quote (escapedChar ecs ers)
    where ecs = zip "'\\trn" "'\\\t\r\n"
          ers = "\\\n\r"
          quote = char '\''

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

-- Parses a Tuple literal 
-- TODO tuples must have more than one element
{-lTuple = LTuple <$> lStruct '(' ')' lValue -}

-- Parses a set literal, empty brackets are presumed to be Dicts
lSet = LSet <$> lStruct '{' '}' expr

lBool = true <|> false
    where true = miLexeme (string "True") >> return (LBool True)
          false = miLexeme (string "False") >> return (LBool False)

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

instance Show Literal where
    show (LChar   c) = "Char: " ++ show c
    show (LString s) = "String \"" ++ s ++ "\""
    show (LInt    i) = "Int: " ++ show i
    show (LFloat  f) = "Float: " ++ show f
    show (LArray  a) = "Array: [" ++ showStruct a ++ "]"
    show (LSet    s) = "Set: {" ++ showStruct s ++ "}"
    show (LDict   d) = "Dict: {" ++ showDict d ++ "}"
    {-show (LTuple  t) = "Tuple: (" ++ showStruct t ++ "}"-}

posAfterKw = "You can't define positional arugment that come after keyword arguments"
posAfterS = "You can't define positional argument after you've defined an argument that aggregates a list"
multiplePS = "You can't define multiple argument that aggregate lists"
multipleKS = "You can't define multiple arguments that aggregate keyword arugments"

defArgs = Args [] [] Nothing Nothing

------------------------- Function Declaration Parsers -------------------------

function = EFix <$> functionDec <*> fBody
    where  fBody = try (fblock 0) <|> try expr
           {-blockBody = do-}
                {-eol'-}
                {-n <- nextIndentLevel-}
                {-condense <$> indentBlock n expr-}
           {-condense [x] = x-}
           {-condense (x:xs) = EChain x (condense xs)-}

functionDec = try namedFunction <|> try anonFunDec

-- parses a named function declared with the keyword fun
namedFunction = do
    name <- iString "fun" *> miLName
    fun <- anonFunDec
    return $ fun {fName = Just name}

-- parses an anonymous function declared like so (arg1,arg2) -> code
anonFunDec = FunctionDec Nothing <$> argDec <*> (opCons <* arrow')


-- Parses function arguments
argDec = genArgs [addK, addP, addPS, addKS] defArgs
    
addK a = do
    n <- miLName
    v <- miEquals *> literal
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


