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
import Language.Utils
import Language.Expressions
import Language.Program
import Parser.Types
import Control.Monad.State (evalStateT)

type TopParser = Program -> BParser Program

parseBlossomFile :: FilePath -> IO (Either ParseError Program)
parseBlossomFile = parseFromFile blossomParser


blossomParser :: Parsec String Program
blossomParser = evalStateT blossom (0,1)

top :: TopParser
top prg = nonIndented $ choice $ map ($ prg) [gVar, fBind, behavior, implem, adt, rdt]

blossom :: BParser Program
blossom = re $ Program [] [] [] [] []
    where go prg = top prg >>= re
          re prg = do done <- optional eof
                      case done of
                          Just _ -> return prg
                          _ -> go prg
 
gVar :: TopParser
gVar prg = addBinding prg <$> eLet

fBind :: TopParser
fBind prg = addBinding prg <$> eLetRec

adt :: TopParser
adt prg = try $ do
    q <- constraint 
    tcon <- data_ *> ptype <* where_
    let qt = q :=> tcon
    stubs <- block (adtStub q tcon)
    return $ addAdt prg $ Adt qt stubs

adtStub :: [Pred] -> Type -> BParser (Annotated Id)
adtStub quals t = do
    constructorName <- uName
    argTypes <- opList (csl ptype)
    let constructorType = mkFun argTypes t
        qualedType = quals :=> constructorType
    return $ constructorName :-: qualedType

rdt :: TopParser
rdt prg = try $ do 
    q <- constraint 
    t <- data_ *> ptype
    let qt = q :=> t
    -- TODO generate instances
    supNs <- opList (inherits_ *> sepBy1 ptype comma_) <* where_
    let sups = map (\(TCons n _) -> q :=> (IsIn n [t])) supNs
    fs <- block (field qt)
    return $ addRecord prg $ Rec qt sups fs

field :: Qual Type -> BParser (Annotated Id)
field (quals :=> objType) = do 
    fieldName <- dot_  *> lName
    fieldType <- colon_ *> ptype
    return $ ('_':fieldName) :-: (quals :=> (objType `func` fieldType))

behavior :: TopParser
behavior prg = try $ do
      qs <- constraint
      t <- ptype
      let sups = map (\(IsIn n _) -> n) qs
      name <- is_ *> uName <* when_
      stubs <- block stub
      return $ addBehavior prg (name, Class sups [] (map fst stubs))

stub :: BParser (Id, Type)
stub = do 
    name <- lName 
    ts <- optional $ csl ptype
    rt <- colon_ *> ptype
    return (name, case ts of
                  Just ts' -> ts' `mkFun` rt
                  Nothing -> rt)

implem :: TopParser
implem prg = try $ do
      q <- constraint
      t <- ptype
      bhvr <- is_ *> uName <* because_
      let ins = q :=> (IsIn bhvr [t])
      impls <- block eLetRec
      return $ addImplementation prg (Im ins impls)

addBinding :: Program -> Expr -> Program
addBinding p b = p {pBind = b : pBind p}

addRecord :: Program -> Rec -> Program
addRecord p r = p {pRdt   = r : pRdt p}

addAdt :: Program -> Adt -> Program
addAdt p a = p {pAdt   = a : pAdt p}

addBehavior :: Program -> (Id, Class) -> Program
addBehavior p b = p {pBvr   = b : pBvr p}

addImplementation :: Program -> Implementation -> Program
addImplementation p i = p {pImpl  = i : pImpl p} 
