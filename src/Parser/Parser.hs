{-# LANGUAGE TupleSections #-}

module Parser.Parser where

import Parser.Core
import Parser.Exprs
import Types.Utils
import Models.Program
import Text.Megaparsec
import Models.Expressions
import Parser.Types
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (void, foldM, ap, liftM)
import Control.Monad.State (evalStateT)

parseBlossomFile = parseFromFile blossomParser 

test fname = do
    res <- parseFromFile blossomParser fname
    case res of 
        Right bs -> mapM_ print bs
        Left err -> print err

blossomParser = evalStateT blossom (0,1)

top = nonIndented $ choice [gVar, fBind, behavior, implem, adt, rdt]

blossom = re []
    where go prg = top >>= (\t -> return $ t : prg) >>= re
          re prg = do done <- optional eof
                      case done of
                          Just _ -> return prg
                          _ -> go prg
 
gVar = try $ Bind <$> do 
    name <- uName
    sch <- opSufCons
    ex <- equals_ *> expr
    return $ case sch of
        Just qt -> Expl (name, quantAll qt, ex)
        _      -> Impl (name, ex)
  
fBind = Bind <$> fDec
fDec = try $ do 
      q <- try topQual
      n <- fun_ *> lName
      (p, mqt) <- args
      ex <- exblock
      let lam = Abs (p, ex)
      return $ case mqt of
          Just (qs :=> t) -> let sch = quantUser ((q ++ qs) :=> t)
                             in (Expl (n, sch, lam))
          Nothing -> Impl (n, lam)

adt = try $ do
    q <- topQual 
    tcon <- (data_ *> vCons <* where_)
    stubs <- block ((,) <$> uName <*> opList (csl ptype))
    return $ ADT $ Adt q tcon stubs
          {-cStub = (,) <$> uName <*> opList (csl ptype)-}

rdt = try $ do 
    q <- topQual 
    t <- data_ *> vCons
    sts <- opList (inherits_ *> sepBy1 vCons comma_) <* where_
    fs <- block field
    return $ RDT $ Rec q t sts fs

field = do 
    fname <- dot_  *> lName
    t <- colon_ *> ptype
    return ('_':fname, t)

behavior = try $ do
      q <- topQual
      t <- vVar
      name <- is_ *> uName <* when_
      stubs <- block stub
      return $ Bvr (Bhvr q t name stubs)

stub = do 
    name <- lName 
    ts <- optional $ csl ptype
    rt <- colon_ *> ptype
    return (name, case ts of
                  Just ts -> ts `mkFun` rt
                  Nothing -> rt)

implem = try $ do
      q <- topQual
      t <- ptype
      bhvr <- is_ *> uName <* because_
      impls <- block impl
      return $ Imp (Im q t bhvr impls)

impl =  do b <- fDec
           case b of 
                Impl i -> return i
                _ -> fail "Don't specify the types of your implementations"
