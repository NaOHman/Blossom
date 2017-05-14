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
import Parser.IR.Pretty

data Implementation = Implementation
    { imPred :: [Pred]
    , imTypes :: [Type]
    , imBhvr :: String
    , imDefs :: [ParseExpr]
    }
    deriving Eq

data Bhvr = Bhvr 
    { bhvrPred :: [Pred]
    , bhvrVars :: [Type]
    , bhvrName :: String
    , bhvrDefs :: [(String, [Type], Type)]
    }
    deriving Eq

data Module = Module
    { mBind :: [ParseExpr]
    , mImpl :: [Implementation]
    , mAdt :: [Adt]
    , mRdt :: [Rec]
    , mBvr :: [Bhvr]
    }
    deriving Eq

data Adt = Adt 
    { adtPred :: [Pred]
    , adtType :: Type
    , adtConstructors :: [(String, [Type])]
    }
    deriving Eq

data Rec = Rec 
    { recPred :: [Pred]
    , recType :: Type
    , recSupers :: [Type]
    , recFields :: [(String, Type)]
    }
    deriving Eq

instance Show Implementation where
    show = pretty

instance Pretty Implementation where
    pp i (Implementation ps ts bhvr es) = block i ps ++ csl i ts ++ " is " ++ bhvr ++ " because" ++ newBlock (i+2) es

instance Show Bhvr where
    show = pretty

instance Pretty Bhvr where
    pp i (Bhvr ps ts n ss) = block i ps ++ csl i ts ++ " is " ++ n ++ " when" ++ block' (i+2) ppStub ss
        where ppStub i' (sn, [], rt) = sn ++ " : " ++ pp i' rt
              ppStub i' (sn, sts, rt) = sn ++ csl i' sts ++ " : " ++ pp i' rt

instance Show Adt where
    show = pretty

instance Pretty Adt where
    pp i (Adt ps t ctrs) = block i ps ++ "data " ++ pp i t ++ " where" ++ block' (i+2) ppCons ctrs
        where ppCons _ (s, []) = s
              ppCons i' (s, ts) = s ++ csl i' ts

instance Show Rec where
    show = pretty

instance Pretty Rec where
    pp i (Rec ps t [] fs) = block i ps ++ "record " ++ pp i t ++ " where" ++ block' (i+2) ppField fs
    pp i (Rec ps t ss fs) = block i ps ++ "record " ++ pp i t ++ " inherits " ++ csl i ss ++  " where" ++ block' (i+2) ppField fs

instance Show Module where
    show = pretty

instance Pretty Module where
    pp i (Module bs is as rs bhvs) = block i rs ++ "\n" ++ block i as ++ "\n" ++ block i bhvs ++ "\n" ++ block i is ++ "\n" ++ block i bs

ppField :: Int -> (String, Type) -> String
ppField i (f, t) = '.':f ++ " : " ++ pp i t

block' :: Int -> (Int -> a -> String) -> [a] -> String
block' i f = concatMap (\s -> indent i ++ f i s)
