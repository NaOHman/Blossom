{- |
Module      : Parser.Parser
Description : A Module that provides top level parsers
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module contains parsers for all top level blossom syntactic constructs which are
Global Variables, Function Definitions, Behavior Declarations, Data Declarations, and
Behavior Implementations. 
-}

module Parser.Parser (parseBlossomFile, blossomParser) where

import Parser.Core
import Parser.Exprs
import Parser.IR.Module
import Parser.IR.Expressions
import Parser.IR.Types
import Parser.Types
import Control.Monad.State (evalStateT)

type TopParser = Module -> BParser Module

parseBlossomFile :: FilePath -> IO (Either ParseError Module)
parseBlossomFile = parseFromFile blossomParser


blossomParser :: Parsec String Module
blossomParser = evalStateT blossom (0,1)

top :: TopParser
top prg = nonIndented $ choice $ map ($ prg) [gVar, fBind, behavior, implem, adt, rdt]

blossom :: BParser Module
blossom = re $ Module [] [] [] [] []
    where go prg = top prg >>= re
          re prg = do done <- optional eof
                      case done of
                          Just _ -> return prg
                          _ -> go prg
 
gVar :: TopParser
gVar prg = addBinding prg <$> eLet

fBind :: TopParser
fBind prg = addBinding prg <$> eFunc

adt :: TopParser
adt prg = try $ do
    prd <- constraint 
    tname <- data_ *> ptype <* where_
    constructors <- block adtStub
    return $ addAdt prg $ Adt prd tname constructors

adtStub :: BParser (String, [Type])
adtStub = (,) <$> uName <*> opList (csl ptype)

rdt :: TopParser
rdt prg = try $ do 
    prd <- constraint 
    tname <- data_ *> ptype
    -- TODO generate instances
    supers <- opList (inherits_ *> sepBy1 ptype comma_)
    where_
    fields <- block field
    return $ addRecord prg $ Rec prd tname supers fields

field :: BParser (String, Type)
field = (,) <$> (dot_ *> lName) <*> (colon_ *> ptype) 

behavior :: TopParser
behavior prg = try $ do
      prd <- constraint
      vars <- csl ptype
      nme <- is_ *> uName <* when_
      defs <- block stub
      return $ addBehavior prg $ Bhvr prd vars nme defs

stub :: BParser (String, [Type], Type)
stub = do 
    nme <- lName 
    ts <- opList $ csl ptype
    rt <- colon_ *> ptype
    return (nme, ts, rt)

implem :: TopParser
implem prg = try $ do
      prd <- constraint
      ts <- csl ptype
      bhvr <- is_ *> uName <* because_
      impls <- block eFunc
      return $ addImplementation prg (Implementation prd ts bhvr impls)

addBinding :: Module -> ParseExpr -> Module
addBinding m b = m {mBind = b : mBind m}

addRecord :: Module -> Rec -> Module
addRecord m r = m {mRdt   = r : mRdt m}

addAdt :: Module -> Adt -> Module
addAdt m a = m {mAdt   = a : mAdt m}

addBehavior :: Module -> Bhvr -> Module
addBehavior m b = m {mBvr   = b : mBvr m}

addImplementation :: Module -> Implementation -> Module
addImplementation m i = m {mImpl  = i : mImpl m} 
