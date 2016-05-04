-- {-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Language.Program 
    ( ClassEnv
    -- , module X
    , Program(..)
    , Rec(..)
    , Adt(..)
    , Behavior(..)
    , Implementation(..)
    , defClasses
    ) where

import Language.Expressions as X
import qualified Data.Map.Strict as M


data Program = Program
    { pBind :: [Bind]
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

data Implementation = Im [Pred] Type Id [Bind]
    deriving Show

type ClassEnv = M.Map Id Class

defClasses :: ClassEnv
defClasses = M.fromList $ map mkCls 
    [("Eq", [], [tInt, tChar, tFloat, tBool])
    ,("Showable", [], [tInt, tChar, tFloat, tBool, tString])
    ,("Ord", ["Eq"], [tInt, tChar, tFloat])
    ,("Num", ["Eq","Show"], [tInt, tFloat])
    ,("Real", ["Num","Ord"], [tInt, tFloat])
    ,("Fractional", ["Num"], [tFloat])
    ,("Floating", ["Fractional"], [tFloat])]

mkCls :: (Id, [Id], [Type]) -> (Id, ([Id], [Qual Pred], [Stub]))
mkCls (i,ss,is) = (i, (ss, ints, stubs))
    where ints = map (\t -> [] :=> IsIn i [t]) is
          stubs = []
