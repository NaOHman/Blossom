{-# LANGUAGE TupleSections #-}

module Parser.Parser where

import Parser.Core
import Parser.Exprs
import Types.Utils
import Models.Program
import Text.Megaparsec
import Models.Expressions
import Parser.Constraints
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

blossomParser = evalStateT blossom 0

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
  
fBind = try $ Bind <$> fDec
fDec = exblock f header
    where header = do 
              q <- try topQual
              n <- fun_ *> lName
              (p, mqt) <- args
              return (p, case mqt of
                  Just (qs :=> t) -> let sch = quantUser ((q ++ qs) :=> t)
                                     in (Expl . (n, sch,))
                  Nothing -> Impl . (n,))
          f (p,fn) ex = fn (Abs (p, ex))

adt = try $ ADT <$> try (block ($) header cStub)
    where header = Adt <$> topQual <*> (data_ *> vCons <* where_)
          cStub = (,) <$> uName <*> opList (csl ptype)

rdt = RDT <$> try (block ($) header field)
    where header = do q <- topQual 
                      t <- data_ *> vCons
                      ss <- superTypes <* where_
                      return (Rec q t ss)
          field = do fname <- dot_  *> lName
                     t <- colon_ *> ptype
                     return ('_':fname, t)
          superTypes = opList (inherits_ *> sepBy1 vCons comma_)

behavior = Bvr <$> try (block ($) header stub)
    where header = try $ do 
              q <- topQual
              t <- vVar
              name <- is_ *> uName <* when_
              return (Bhvr q t name)
          stub = do 
              name <- lName 
              ts <- optional $ csl ptype
              rt <- colon_ *> ptype
              return (name, case ts of
                  Just ts -> ts `mkFun` rt
                  Nothing -> rt)

implem = Imp <$> try (block ($) header fDec')
    where fDec' = do bs <- fDec
                     return $ toImpl bs 
          toImpl (Expl (i,_,e)) = (i,e)
          toImpl (Impl i) = i
          header = try $ do 
              q <- topQual
              t <- ptype
              bhvr <- is_ *> uName <* because_
              return (Im q t bhvr)
