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

import Parser.IR.Expressions

imInst :: Implementation -> Inst
imInst (Im i _) = i

data Implementation = Im Inst [Binding]
    deriving Show

data Class = Class [Id] [Implementation] [Id]
    deriving Show

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
