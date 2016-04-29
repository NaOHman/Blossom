module Parser.Parser where

import Parser.Core
import LangDef.Blossom (mkFun)
import Parser.Exprs
import Types.Utils
import Models.Program
import Parser.Types
import Control.Monad.State (evalStateT)

data Top = Bind Binding
         | Imp Implementation
         | ADT Adt
         | RDT Rec
         | Bvr Behavior

parseBlossomFile :: FilePath -> IO (Either ParseError Program)
parseBlossomFile = parseFromFile blossomParser


blossomParser :: Parsec String Program
blossomParser = evalStateT blossom (0,1)

top :: BParser Top
top = nonIndented $ choice [gVar, fBind, behavior, implem, adt, rdt]

blossom :: BParser Program
blossom = splitTops <$> re []
    where go prg = top >>= (\t -> return $ t : prg) >>= re
          re prg = do done <- optional eof
                      case done of
                          Just _ -> return prg
                          _ -> go prg
 
gVar :: BParser Top
gVar = try $ Bind <$> do 
    name <- uName
    sch <- opSufCons
    ex <- equals_ *> expr
    return $ case sch of
        Just qt -> Expl (name, quantAll qt, ex)
        _      -> Impl (name, ex)
  

fBind :: BParser Top
fBind = Bind <$> fDec

fDec :: BParser Binding
fDec = try $ do 
      q <- try topQual
      n <- fun_ *> lName
      (p, mqt) <- args
      ex <- exblock
      let lam = Abs (p, ex)
      return $ case mqt of
          Just (qs :=> t) -> let sch = quantUser ((q ++ qs) :=> t)
                             in Expl (n, sch, lam)
          Nothing -> Impl (n, lam)

adt :: BParser Top
adt = try $ do
    q <- topQual 
    tcon <- data_ *> vCons <* where_
    stubs <- block ((,) <$> uName <*> opList (csl ptype))
    return $ ADT $ Adt q tcon stubs

rdt :: BParser Top
rdt = try $ do 
    q <- topQual 
    t <- data_ *> vCons
    sts <- opList (inherits_ *> sepBy1 vCons comma_) <* where_
    fs <- block field
    return $ RDT $ Rec q t sts fs

field :: BParser (Id, Type)
field = do 
    fname <- dot_  *> lName
    t <- colon_ *> ptype
    return ('_':fname, t)

behavior :: BParser Top
behavior = try $ do
      q <- topQual
      t <- vVar
      name <- is_ *> uName <* when_
      stubs <- block stub
      return $ Bvr (Bhvr q t name stubs)

stub :: BParser (Id, Type)
stub = do 
    name <- lName 
    ts <- optional $ csl ptype
    rt <- colon_ *> ptype
    return (name, case ts of
                  Just ts' -> ts' `mkFun` rt
                  Nothing -> rt)

implem :: BParser Top
implem = try $ do
      q <- topQual
      t <- ptype
      bhvr <- is_ *> uName <* because_
      impls <- block impl
      return $ Imp (Im q t bhvr impls)

impl :: BParser (Id, Expr)
impl =  do b <- fDec
           case b of 
                Impl i -> return i
                _ -> fail "Don't specify the types of your implementations"

splitTops :: [Top] -> Program
splitTops = foldl split (Program [] [] [] [] [])
    where split p (Bind b) = p {pBind = b : pBind p}
          split p (RDT  r) = p {pRdt   = r : pRdt p}
          split p (ADT  a) = p {pAdt   = a : pAdt p}
          split p (Bvr  b) = p {pBvr   = b : pBvr p}
          split p (Imp  i) = p {pImpl  = i : pImpl p} 
