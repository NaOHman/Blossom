-- {-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Language.Program 
    ( ClassEnv
    -- , module X
    , Program(..)
    , Rec(..)
    , Adt(..)
    , defClasses
    ) where

import Language.Expressions as X
import qualified Data.Map.Strict as M


data Program = Program
    { pBind :: [Bind]
    , pImpl :: [Implementation]
    , pAdt :: [Adt]
    , pRdt :: [Rec]
    , pBvr :: [(Id,Class)]
    }

data Adt = Adt 
    { aType :: Qual Type 
    , acnstrs :: [Expl]
    } deriving Show

data Rec = Rec 
    { rType :: Qual Type
    , sups :: [Inst]
    , rfields :: [Expl]
    } deriving (Show)

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

mkCls :: (Id, [Id], [Type]) -> (Id, Class)
mkCls (i,ss,is) = (i, Class ss ints stubs)
    where ints = map (\t ->Im ([] :=> IsIn i [t]) []) is
          stubs = []
