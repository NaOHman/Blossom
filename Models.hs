{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Models where

import Literals
import ParserUtils
import qualified Data.Map as M
import qualified Data.Set as S
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

data Constructor = Constructor { 
                   consName :: Name,
                   params :: [CArg], 
                   rtCons :: Constraint,
                   consBody :: Maybe Block }

data Stub = Stub { sName :: Name
                 , sargs :: [SArg]
                 , sRT :: Constraint
                 }

data Implementation = Implementation
    { implementing :: Constraint
    , isa :: Name
    , because :: [Function]}

data SArg where
    SNamed :: Name -> Constraint -> SArg
    SAnon :: Constraint -> SArg

data CArg where
    Named :: Name -> Constraint -> CArg
    Keyword :: Name -> Literal -> Constraint -> CArg
    AnonKey :: Literal -> Constraint -> CArg
    Anon :: Constraint -> CArg

type Exception = ()

type Name = String

data Class = Class { cConstraint :: Constraint
                   , cName :: String
                   , functions :: [Stub]
                   }

type Alias = (String, Type)


data Data = Data Name [Constraint] [Constructor]

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
              showArgs desc as = Just $ desc ++ showStruct as

instance Show Arg where
    show (KArg n l c) = n ++ " def = " ++ show l ++ ' ':show c
    show (PArg n c) = n ++ " is " ++ show c
    show (KSplat n c) = n ++ " is " ++ show c
    show (PSplat n c) = n ++ " is " ++ show c

   
instance Show Data where
    show (Data n cs cons) = "Data " ++ show n ++ "<" ++ showStruct cs ++ "> where \n" ++ showNsl show cons

instance Show Constructor where
    show (Constructor n ps c _) = "Constructor: " ++ n ++ "(" ++ showStruct ps ++ ")" ++ show c

instance Show CArg where
    show (Named n c) = n ++ " : " ++ show c 
    show (Keyword n d c) = n ++ " = "  ++ show d ++ " : " ++ show c 
    show (AnonKey d c) = "Anon def " ++ show d ++ " : " ++ " : " ++ show c 
    show (Anon c) = "Anon : " ++ show c

instance Show Class where
    show (Class cns n ss) = "Class: " ++ n ++ "\n with constraint: " ++ show cns ++ "\n" ++
        showNsl show ss

instance Show Stub where
    show (Stub n ps rt) = "Stub: " ++ n ++ "(" ++ showStruct ps ++ ")" ++ " : " ++ show rt

instance Show SArg where
    show (SNamed n c) = n ++ " : " ++ show c
    show (SAnon c) = "Anon : "  ++ show c

prettyType (Type name cs) = "Type: " ++ name ++ "<" ++ showStruct cs ++ ">" 

