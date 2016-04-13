{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Models.Program 
    ( module X
    , ClassEnv(..)
    , Rec(..)
    , Adt(..)
    , Top(..)
    , Data(..)
    , Behavior(..)
    , Implementation(..)
    ) where

import Models.Expressions as X
import Text.Megaparsec.Pos
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Top = Bind Binding
         | Imp Implementation
         | ADT Adt
         | RDT Rec
         | Bvr Behavior

class Data d where
    dQual ::  d -> [Pred]
    dTCons :: d -> Tycon
    dCstrs :: d -> [(Id, Scheme)]
    dNames :: d -> [Id]

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
    } deriving Show

-- Type is Id when stubs
data Behavior = Bhvr [Pred] Type Id [(Id, Type)]
    deriving Show

data Implementation = Im [Pred] Type Id [Impl]
    deriving Show


type ClassEnv = M.Map Id Class

{-instance Show (M.Map Id Class) where-}
    {-show = show . M.elems-}

instance Show Top where
    show (Bind b) = show b
    show (Imp b) = show b
    show (ADT b) = show b
    show (RDT b) = show b
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
