{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Models.Data 
    ( module Models.Core
    , Data(..)
    , Data'(..)
    , dname
    , ClassDec(..)
    , ClassDec'(..)
    , Instance(..)
    , Instance'(..)
    )where

import Models.Expressions
import Models.Core
import Models.Types
import Text.Megaparsec.Pos

type Instance = Lex Instance'
type ClassDec = Lex ClassDec'
type Data = Lex Data'

data Data' = Data'
    { dCons :: Type
    , constructors :: [(Id, Maybe Type')]
    , fields :: [(Id, Type)]
    } deriving Show

dname (Data' (Lex _ t) _ _) = tname t
tname (TCons (Tycon n _)) = n
tname (TVar (Tyvar n _)) = n
tname (TAp t _) = tname t

data ClassDec' = ClassDec'
    { cVar :: Type
    , cname :: Id 
    , cstubs :: [(Id, Type)]
    } deriving Show

data Instance' = Instance'
    { icons  :: Type
    , iclass :: Id -- Predicate?
    , ifuns  :: [(Id,PExpr)]
    } deriving Show
