{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Models where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate) 
import Data.Maybe (catMaybes) 

-- Top level model for expressions
data Expr where
    EFCall :: FCall -> Expr
    ELiteral :: Literal -> Expr
    EName :: String -> Expr

-- Model for function calls
data FCall where
    FNamed :: String -> Args -> FCall
    FAnon :: Args -> FCall

-- Model for code blocks (Maybe not used)
data Block = Block { statements :: [Statement] 
                   , bType :: Constraint
                   , exceptions :: [Exception] 
                   , indentLevel :: Integer
                   }

type Statement = ()

type Constructor = ()

type Exception = ()

type Name = String

data Class = Class { cName :: String
                   , cParams :: [String]
                   , functions :: [FunctionDec]
                   }

data Constraint where
    None :: Constraint
    Partial :: Type -> Constraint
    Concrete :: Type -> Constraint

type Alias = (String, Type)

data Arg where 
    PArg   :: Name -> Constraint -> Arg
    KArg   :: Name -> Literal -> Constraint -> Arg
    KSplat :: Name -> Constraint -> Arg
    PSplat :: Name -> Constraint -> Arg


data Type = Type String [Constraint]

data Data = Data Name [Constraint] [FunctionDec]

makeCons n [] = Concrete $ Type n []
makeCons n cs = if concrete cs
                   then Concrete $ Type n cs
                   else Partial $ Type n cs

makeFunCons args rt = if concrete args 
                        then Concrete $ Type (f rt) args
                        else Partial $ Type (f rt) args
    where f (Just rt) = "Function to a " ++ show rt
          f _ = "Function to anything"

concrete = all p 
    where p (Concrete _) = True
          p _            = False

fType args rt = Type ("function to a " ++ show rt) args

data FunctionDec = FunctionDec 
                      { fName :: Maybe String
                      , args :: Args
                      , returnType :: Constraint
                      }

anonFDec = FunctionDec Nothing

data Function = Function FunctionDec Block

data Args = Args { positional :: [Arg]
                 , keyword :: [Arg]
                 , arrArgs :: Maybe Arg
                 , kwArgs :: Maybe Arg
                 }

newArg :: Monad m => Args -> Arg -> m Args
newArg a k@KArg{} = return $ a {keyword = k:keyword a}
newArg a p@PArg{} = case arrArgs a of
    Nothing -> case keyword a of
         [] -> return $ a {positional = p:positional a}
         _  -> fail "You can't define positional arugment that come after keyword arguments"
    Just _  -> fail "You can't define positional argument after you've defined an argument that aggregates a list"
newArg a ps@PSplat{} = case arrArgs a of
    Nothing -> return $ a {arrArgs = Just ps}
    Just _  -> fail "You can't define multiple argument that aggregate lists"
newArg a ks@KSplat{} = case kwArgs a of
    Nothing -> return $ a {kwArgs = Just ks}
    Just _  -> fail "You can't define multiple arguments that aggregate keyword arugments"

defArgs = Args { positional = []
               , keyword = []
               , arrArgs = Nothing
               , kwArgs = Nothing}

data Literal where
    LChar   :: Char -> Literal
    LString :: String -> Literal
    LInt    :: Integer -> Literal
    LFloat  :: Double -> Literal
    LArray  :: [Literal] -> Literal
    LTuple  :: [Literal] -> Literal
    LDict   :: [(Literal,Literal)]  -> Literal
    LSet    :: [Literal] -> Literal

instance Show Literal where
    show (LChar   c) = "Char: " ++ show c
    show (LString s) = "String \"" ++ s ++ "\""
    show (LInt    i) = "Int: " ++ show i
    show (LFloat  f) = "Float: " ++ show f
    show (LArray  a) = "Array: [" ++ showStruct a ++ "]"
    show (LSet    s) = "Set: {" ++ showStruct s ++ "}"
    show (LDict   d) = "Dict: {" ++ showDict d ++ "}"
    show (LTuple  t) = "Tuple: (" ++ showStruct t ++ "}"

instance Show FunctionDec where
    show (FunctionDec n args cons) = "Function: " ++ showName n ++
            "\nArgs:\n" ++ show args ++ "\nReturns: " ++ show cons
        where showName (Just n) = n
              showName Nothing = "(Anonymous)"

instance Show Args where
    show (Args pos kw sp sk) = intercalate "\n" $ catMaybes [ 
        showArgs "Positional Args: " pos,
        showArgs "Keyword Args: " kw, 
        showSplat "Array Agregator: " sp, 
        showSplat "Keyword Aggregator: " sk]
        where showSplat desc s = (\s -> desc ++ show s) <$> s
              showArgs desc [] = Nothing
              showArgs desc as = Just $ desc ++ showCsl show as

instance Show Arg where
    show (KArg n l c) = n ++ " def = " ++ show l ++ ' ':show c
    show (PArg n c) = n ++ " is " ++ show c
    show (KSplat n c) = n ++ " is " ++ show c
    show (PSplat n c) = n ++ " is " ++ show c

instance Show Constraint where
    show None = "Anything"
    show (Concrete t) = prettyType t
    show (Partial t) = prettyType t
    
instance Show Data where
    show (Data n cs cons) = prettyType (Type n cs) ++ "\n" ++ showCsl show cons

prettyType (Type name cs) = "Type: " ++ name ++ "<" ++ showCsl show cs ++ ">" 

showCsl f = intercalate ", " . map f
showStruct = showCsl show
showDict = showCsl $ \(a,b) -> show a ++ ": " ++ show b
