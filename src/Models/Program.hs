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
    deriving Show

data Data = ADT (Qual Type) [(Id, [Type])]
          | Rec (Qual Type) [Type] [(Id, Type)]
    deriving Show

-- Type is Id when stubs
data Behavior = Bhvr (Qual Type) Id [(Id, Type)]
    deriving Show

data Implementation = Im (Qual Type) Id [Binding]
    deriving Show


type ClassEnv = M.Map Id Class

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
