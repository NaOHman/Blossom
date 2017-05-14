-- {-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Parser.IR.Module 
    ( Module(..)
    , Implementation(..)
    , Rec(..)
    , Adt(..)
    , Bhvr(..)
    ) where

import Parser.IR.Expressions
import Parser.IR.Types

data Implementation = Implementation
    { imPred :: [Pred]
    , imTypes :: [Type]
    , imBhvr :: String
    , imDefs :: [ParseExpr]
    }

data Bhvr = Bhvr 
    { bhvrPred :: [Pred]
    , bhvrVars :: [Type]
    , bhvrName :: String
    , bhvrDefs :: [(String, [Type], Type)]
    }

data Module = Module
    { mBind :: [ParseExpr]
    , mImpl :: [Implementation]
    , mAdt :: [Adt]
    , mRdt :: [Rec]
    , mBvr :: [Bhvr]
    }

data Adt = Adt 
    { adtPred :: [Pred]
    , adtType :: Type
    , adtConstructors :: [(String, [Type])]
    }

data Rec = Rec 
    { recPred :: [Pred]
    , recType :: Type
    , recSupers :: [Type]
    , recFields :: [(String, Type)]
    }
