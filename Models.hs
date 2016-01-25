{-# LANGUAGE GADTs #-}

module Models where

import Constraints

type Id = String

data Program = Program 
    { functions :: [Expr] 
    , globals   :: [Expr]
    , datatypes :: [Data]
    , classes   :: [Class] 
    }
    deriving Show
    -- todo type aliases and error types

data Data = Data 
    { dName :: Id
    , constructors :: [Expr]
    }
    deriving Show

data Class = Class 
    { cVard :: Id
    , cName :: Id
    , implementations :: [(Id, Args)]
    }
    deriving Show

data Pat where
    DName  :: Id -> Pat
    DCons  :: Id -> [Pat] -> Pat
    DMatch :: Literal -> Pat
    DNil   :: Pat
    deriving Show

data Expr  where
    ELit    :: Literal -> Expr
    EVar    :: Id -> Expr
    ELambda :: Args -> Expr -> Expr
    EFCall  :: Call -> Expr
    EFix    :: String -> Args -> Expr -> Expr
    ELet    :: Pat -> Expr -> Expr -- EChain ~= in
    ECase   :: Expr -> [(Pat, Expr)] -> Expr
    EChain  :: Expr -> Expr -> Expr
    deriving Show

data Call = Call 
    { fname :: String
    , cpArgs :: [Expr]
    , ckwArgs :: [(String, Expr)]
    }
    deriving Show

data FDec = FDec 
    { fName :: Maybe String
    , args :: Args
    , returnType :: Constraint
    }
    deriving Show

data Args = Args 
    { positional :: [(String, Constraint)]
    , keyword :: [(String, Literal, Constraint)]
    , arrArgs :: Maybe (String, Constraint)
    , kwArgs :: Maybe (String, Constraint)
    }
    deriving Show

data Literal where
    LChar   :: Char -> Literal
    LString :: String -> Literal
    LInt    :: Integer -> Literal
    LFloat  :: Double -> Literal
    LArray  :: [Expr] -> Literal
    LTuple  :: [Expr] -> Literal
    LDict   :: [(Expr,Expr)]  -> Literal
    LSet    :: [Expr] -> Literal
    LCons   :: Id -> [Expr] -> Literal
    LNull   :: Literal
    deriving Show
