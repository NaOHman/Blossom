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

blossomParser = evalStateT blossom 0

blossom = indentedItems (-1) 0 top

top = nonIndented $ tryList [gVar, fBind, behavior, implem, adt, rdt]

gVar = Bind <$> do 
    name <- uName
    sch <- opSufCons
    ex <- equals' *> expr
    return $ case sch of
        Just qt -> Expl name (quantAll qt) ex
        _      -> Impl name ex
  
fBind = Bind <$> fDec

fDec = do
    q <- topQual
    f <- fun' *> lName
    (lam, mt) <- lambda
    return $ case mt of
        Just (qs :=> t) -> Expl f (quantQual (q ++ qs) t) lam
        _      -> Impl f lam

adt = Dta <$> do 
    q <- topQual
    newType <- data_ *> vCons <* where_
    cstrs <- block cStub
    return $ ADT (q :=> newType) cstrs
    where cStub = (,) <$> uName <*> opList (parenCsl ptype)

rdt = Dta <$> do 
    q <- topQual
    newType <- data_ *> vCons 
    superTypes <- opList (inherits *> sepBy1 vCons comma')
    fields <- where_ *> block field 
    return $ Rec (q :=> newType) superTypes fields
    where field = (,) <$> (dot' *> lName) <*> (colon' *> ptype)

behavior = Bvr <$> do 
    q <- topQual
    t <- vVar
    name <- is' *> uName <* when'
    stubs <- block stub
    return $ Bhvr (q :=> t) name stubs
    where stub = do 
                name <- lName 
                ts <- optional $ parenCsl ptype
                rt <- colon' *> ptype
                return (name, case ts of
                    Just ts -> ts `mkFun` rt
                    Nothing -> rt)

implem = Imp <$> do
    q <- topQual
    t <- ptype
    bhvr <- is' *> uName <* because'
    bindings <- block fDec
    return $ Im (q :=> t) bhvr bindings
