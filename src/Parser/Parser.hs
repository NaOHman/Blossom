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
        Right bs -> void $ mapM print bs
        Left err -> print err

blossomParser = evalStateT blossom 0

top = nonIndented $ tryList [gVar, fBind, behavior, implem, adt, rdt]

blossom = re []
    where go prg = top >>= (\t -> return $ t : prg) >>= re
          re prg = do done <- optional eof
                      case done of
                          Just _ -> return prg
                          otherwise -> go prg
 
gVar = Bind <$> do 
    name <- uName
    sch <- opSufCons
    ex <- equals' *> expr
    return $ case sch of
        Just qt -> Expl (name, quantAll qt, ex)
        _      -> Impl (name, ex)
  
fBind = Bind <$> fDec
fDec = try inline <|> try blk
    where blk = block f header expr
          inline = do (p, fn) <- header
                      ex <- expr
                      return $ fn (Abs (p,ex))
          header = do 
              q <- topQual
              n <- fun' *> lName
              (p, mqt) <- args
              {-p' = scrubZeroProd p-}
              return (p, case mqt of
                  Just (qs :=> t) -> 
                      let sch = quantUser ((q ++ qs) :=> t)
                      in (Expl . (n, sch,))
                  Nothing -> Impl . (n,))
          f (p,fn) exs = fn (Abs (p, chain exs))

adt = ADT <$> block ($) header cStub
    where header = Adt <$> topQual <*> (data_ *> vCons <* where_)
          cStub = (,) <$> uName <*> opList (parenCsl ptype)

rdt = RDT <$> block ($) header field 
    where header = do q <- topQual 
                      t <- data_ *> vCons
                      ss <- superTypes <* where_
                      return (Rec q t ss)
          field = do fname <- dot'  *> lName
                     t <- colon' *> ptype
                     return ('_':fname, t)
          superTypes = opList (inherits *> sepBy1 vCons comma')

behavior = Bvr <$> block ($) header stub 
    where header = do q <- topQual
                      t <- vVar
                      name <- is' *> uName <* when'
                      return (Bhvr q t name)
          stub = do 
                name <- lName 
                ts <- optional $ parenCsl ptype
                rt <- colon' *> ptype
                return (name, case ts of
                    Just ts -> ts `mkFun` rt
                    Nothing -> rt)

implem = Imp <$> block ($) header fDec'
    where fDec' = do bs <- fDec
                     return $ toImpl bs 
          toImpl (Expl (i,_,e)) = (i,e)
          toImpl (Impl i) = i
          header = do q <- topQual
                      t <- ptype
                      bhvr <- is' *> uName <* because'
                      return (Im q t bhvr)
