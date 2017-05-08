-- {-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Language.Program 
    ( ClassEnv
    -- , module X
    , Program(..)
    , Implementation(..)
    , imInst
    , Rec(..)
    , Adt(..)
    , Class(..)
    , defClasses
    ) where

import Language.Expressions as X
import Language.Bindings as X
import qualified Data.Map.Strict as M

imInst :: Implementation -> Inst
imInst (Im i _) = i

data Implementation = Im Inst [Binding]
    deriving Show

data Class = Class [Id] [Implementation] [Id]
    deriving Show

data Program = Program
    { pBind :: [Binding]
    , pImpl :: [Implementation]
    , pAdt :: [Adt]
    , pRdt :: [Rec]
    , pBvr :: [(Id,Class)]
    }

data Adt = Adt 
    { aType :: Qual Type 
    , acnstrs :: [Annotated Id]
    } deriving Show

data Rec = Rec 
    { rType :: Qual Type
    , sups :: [Inst]
    , rfields :: [(Type, Id)]
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
