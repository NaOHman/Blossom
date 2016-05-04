-- {-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Language.Program 
    ( ClassEnv
    -- , module X
    , Program(..)
    , Rec(..)
    , Adt(..)
    , Behavior(..)
    , Implementation(..)
    ) where

import Language.Expressions as X
import qualified Data.Map.Strict as M


data Program = Program
    { pBind :: [Binding]
    , pImpl :: [Implementation]
    , pAdt :: [Adt]
    , pRdt :: [Rec]
    , pBvr :: [Behavior]
    }

data Adt = Adt 
    { aqual   :: [Pred]
    , atycon :: Type
    , acnstrs :: [(Id, [Type])]
    } deriving Show

data Rec = Rec 
    { rqual :: [Pred] 
    , rtycon :: Type
    , sups :: [Type]
    , rfields :: [(Id, Type)]
    } deriving (Show, Eq)

-- Type is Id when stubs
data Behavior = Bhvr [Pred] Type Id [(Id, Type)]
    deriving Show

data Implementation = Im [Pred] Type Id [Impl]
    deriving Show

type ClassEnv = M.Map Id Class
