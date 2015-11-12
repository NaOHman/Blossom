{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Models where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate) 

data Expr where
    EFCall :: FCall -> Expr
    ELiteral :: Literal -> Expr
    EName :: String -> Expr

data FCall where
    FNamed :: String -> Args -> FCall
    FAnon :: Args -> FCall

data Args = Args [Expr] (M.Map String Expr)

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
    show (LString s) = "String '" ++ s ++ "'"
    show (LInt    i) = "Int: " ++ show i
    show (LFloat  f) = "Float: " ++ show f
    show (LArray  a) = "Array: [" ++ showStruct a ++ "]"
    show (LSet    s) = "Set: {" ++ showStruct s ++ "}"
    show (LDict   d) = "Dict: {" ++ showDict d ++ "}"
    show (LTuple  t) = "Tuple: (" ++ showStruct t ++ "}"


showStruct = intercalate ", " . map show
showDict = intercalate ", " . map dshow
    where dshow (a,b) = show a ++ ": " ++ show b
