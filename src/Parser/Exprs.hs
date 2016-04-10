{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Parser.Exprs where

import Parser.Core
import Data.Maybe
import Models.Expressions
import Parser.Literals
import Types.Utils
import Parser.Constraints
import Control.Monad (liftM, foldM)
import Text.Megaparsec
import Text.Megaparsec.Expr

expr = makeExprParser term operators <?> "expression"

term = tryList [eCase, eAp, eLet, eAbs, eLit, eVar, parens expr]

noApExpr = makeExprParser noApTerms operators <?> "expression"
    where noApTerms = tryList [eCase, eLet, eAbs, eLit, eVar, parens expr]

eLit = Lit <$> literal

{-eVar = Var <$> (aName <* notFollowedBy (symbol "(" <|> dot')) -}
eVar = Var <$> aName

-- lambdas with explicitly typed arguments are contained within annotations.
eAbs = do
    (lam, mt) <- lambda
    return $ case mt of
        Just t  -> Annot lam (quantAll t)
        Nothing -> lam

lambda = do
    (args, qts) <- args
    rt <- opSufCons
    ex <- arrow' *> expr
    let lambda = Abs (args, ex)
        mSch = fScheme qts rt
    return (lambda, mSch)
        
args = do 
    (as,qs) <- unzip <$> parenCsl arg 
    return (prod $ map PVar as, qs)
    where arg = (,) <$> lName <*> opSufCons

eAp = try regCall <|> subRef
    where subRef = do
                ex <- noApExpr
                field <- char '.' *> eVar
                return $ Ap field (prod [ex])
          regCall = Ap <$> noApExpr <*> fArgs

eLet =  do i <- lName 
           sch <- opSufCons 
           ex <- equals' *> expr
           return $ case sch of
              Just s -> Let ([(i, quantAll s, ex)], []) eUnit
              _ -> Let ([],[(i, ex)]) eUnit

eCase = Case <$> header <*> iBlock branch
    where header = case_ *> expr <* of_
          branch = (,) <$> (pat <* arrow') <*> exblock 
   
eAnnot = Annot <$> expr <*> (quantAll <$> sufCons)

pat  =  try (PCons <$> uName <*> (try (csl pat) <|> return []))
    <|> try (PAs   <$> (lName <* char '#') <*> pat)
    <|> try (PLit  <$> literal)
    <|> try (PVar  <$> lName)
    <|> try (symbol "_" >> return PNil)

operators = [[uOp "+", uOp "-"],
         [bOp "*", bOp "/", bOp "//", bOp "%"],
         [bOp "+", bOp "-"],
         [bOp "==", bOp "<", bOp "<=", bOp ">", bOp ">="],
         [uOp "not"],
         [bOp "and", bOp "or", bOp "xor"]]

exblock ::  BParser Expr
exblock = iBlock expr >>= chain
    where chain [e] = return e
          chain (e:es) = foldM conChain e es
          conChain (Let bg (Lit LNull)) e2 = return $ Let bg e2
          conChain (Let _ _ ) _ = fail "Something is horribly wrong"
          conChain e1 e2 = return $ Ap (Var "!seq") (prod [e1, e2])

uOp s = Prefix $ try $
    rword s *> return (\e -> opAp (s++"UN") (prod [e]))

bOp s = InfixL $ try $
    rword s *> return (\e1 e2 -> opAp s (prod [e1, e2]))

opAp s = Ap (Var s)

argArrow = csl arg <* arrow'
    where arg = (,) <$> lName <*> opSufCons

fArgs = prod <$> parenCsl expr

eUnit = Lit LNull

toVars = zipWith fromMaybe defVars

defVars = [[] :=> (TVar $ Tyvar ("!var" ++ show v) Star) | v <- [0..]]

fScheme ts Nothing 
    | all isNothing ts = Nothing
    | otherwise = fScheme ts rt
    where rt = Just ([] :=> (TVar $ Tyvar "%rt" Star))
fScheme ts (Just rt) = let t = prod (toVars ts) `qualFn` rt
                       in Just t
