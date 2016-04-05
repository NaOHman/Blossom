{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Models.Program 
    ( module X
    {-, Scope (..)-}
    {-, FArgs (..)-}
    {-, FDec (..)-}
    {-, Value(..)-}
    , ClassEnv(..)
    , DataMap(..)
    , Program(..)
    {-, defPrg-}
    ) where

import Models.Expressions as X
import Models.Core as X
import Models.Data as X
import Text.Megaparsec.Pos
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Program = Program
    { bnds  :: [Binding]
    , impls :: [Implementation]
    , dtaDs :: [Data]
    , bhvr  :: ClassEnv
    }
    deriving Show

{-defPrg = Program [] [] EUnit [] M.empty-}

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

{-data Scope = Scope-}
    {-{ global :: M.Map String Value-}
    {-, local  :: M.Map String Value-}
    {-, constr :: M.Map String (M.Map String Int)-}
    {-} -}
 
data DataMap = DataMap 
    { dta :: M.Map Id Type
    , cnstrs :: M.Map Id [Type]
    , ambig :: S.Set Id
    }
 
defDMap = DataMap M.empty M.empty S.empty
