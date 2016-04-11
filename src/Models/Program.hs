{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Models.Program 
    ( module X
    , ClassEnv(..)
    , Top(..)
    , Data(..)
    , Behavior(..)
    , Implementation(..)
    ) where

import Models.Expressions as X
import Models.Core as X
import Text.Megaparsec.Pos
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Top = Bind Binding
         | Imp Implementation
         | Dta Data
         | Bvr Behavior

data Data = ADT [Pred] Type [(Id, [Type])]
          | Rec [Pred] Type [Type] [(Id, Type)]
    deriving Show

-- Type is Id when stubs
data Behavior = Bhvr [Pred] Type Id [(Id, Type)]
    deriving Show

data Implementation = Im [Pred] Type Id [Binding]
    deriving Show


type ClassEnv = M.Map Id Class

instance Show Top where
    show (Bind b) = show b
    show (Imp b) = show b
    show (Dta b) = show b
    show (Bvr b) = show b
defClasses = M.fromList 
 [("Eq",        ([],[]))
 ,("Ord",       (["Eq"],[]))
 ,("Num",       (["Eq", "Showable"],[]))
 ,("Enum",      (["Eq", "Showable"],[]))
 ,("Real",      (["Num", "Ord"],[]))
 ,("Fractional",(["Num", "Ord"],[]))
 ,("Integral",  (["Real", "Enum"],[]))
 ,("RealFrac",  (["Real","Fractional"],[]))
 ,("Floating",  (["Fractional"],[]))
 ,("RealFloat", (["RealFrac", "Floating"],[""]))
 ,("Readable",  ([],[]))
 ]
