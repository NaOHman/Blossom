{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Parser.Exprs where

import Parser.Core
import Models.Expressions
import Parser.Literals
import Parser.Constraints
import Control.Monad (liftM)
import Text.Megaparsec
import Text.Megaparsec.Expr

expr = makeExprParser term operators <?> "expression"

term = tryList [eCase, eAp, eLet, eLit, eVar, parens expr]

eLit = ELit <$> literal

eVar = EVar <$> lName

eAbs = EAbs <$> argArrow <*> exblock

eCons = ECons <$> uName <*> (try parenExprs <|> return []) 
eLet =  do p <- pat <* equals'
           ex <- expr
           return $ ELet [(p, ex)] EUnit

eAp = regCall <|> subRef
    where subRef = do ex <- expr
                      field <- char '.' *> eVar
                      return $ EAp field [ex]
          regCall = EAp <$> expr <*> parenExprs

eCase = ECase <$> header <*> iBlock branch
    where header = case_ *> expr <* of_
          branch = (,) <$> (pat <* arrow') <*> exblock 
   
pat  =  try (PCons <$> uName <*> (try (csl pat) <|> return []))
    <|> try (PAs   <$> (lName <* char '#') <*> pat)
    <|> try (PLit  <$> literal)
    <|> try (PVar  <$> lName)
    <|> try (symbol "_" >> return PNil)

{-cTimeLit = literal (withPos (ELit <$> cTimeLit))-}

operators = [[uOp "+", uOp "-"],
         [bOp "*", bOp "/", bOp "//", bOp "%"],
         [bOp "+", bOp "-"],
         [bOp "==", bOp "<", bOp "<=", bOp ">", bOp ">="],
         [uOp "not"],
         [bOp "and", bOp "or", bOp "xor"]]

exblock ::  BParser PExpr
exblock = liftM chain (iBlock expr) 
    where chain [e] = e
          chain (e:es) = foldl conChain e es
          conChain (ELet bg _) e2 = ELet bg e2
          conChain e1 e2 = EAp (EVar "!seq") [e1, e2]

uOp s = Prefix $ try $
    rword s *> return (\e -> opAp (s++"UN") [e])

bOp s = InfixL $ try $
    rword s *> return (\e1 e2 -> opAp s [e1, e2])

opAp s = EAp (EVar s)

argArrow = csl arg <* arrow'
    where arg = (,) <$> lName <*> opSufCons

parenExprs = parenCsl expr
