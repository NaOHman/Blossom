{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Parser.Exprs where

import Parser.Core
import Data.Maybe
import Models.Expressions
import Parser.Literals
import Types.Utils
import Parser.Constraints
import Control.Monad (liftM, foldM)
import Control.Monad.State
import Text.Megaparsec
import Text.Megaparsec.Expr

expr = makeExprParser term operators <?> "expression"

term = tryList [eAbs, eCase, eAp, eLet, eLit, eVar, parens expr]

noApExpr = makeExprParser noApTerms operators <?> "expression"
    where noApTerms = tryList [eAbs, eCase, eLet, eLit, eVar, parens expr]

eLit = Lit <$> literal

eVar = Var <$> aName

-- lambdas with explicitly typed arguments are contained within annotations.
eAbs = do
    (lam, mt) <- lambda
    return $ case mt of
        Just sc  -> Annot (Abs lam) (quantUser sc)
        Nothing -> Abs lam

lambda :: BParser (Alt, Maybe (Qual Type))
lambda = do ((pat, mt), ex) <- exblock (,) args
            return ((pat, ex), mt)

args :: BParser (Pat, Maybe (Qual Type))
args = do 
    (as,qts) <- unzip <$> parenCsl arg 
    rt <- opSufCons <* arrow'
    mt <- funcAnnot qts rt
    let p = prod $ map PVar as
    return (p, mt)
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

eCase = block Case header branch
    where header = case_ *> expr <* of_
          branch = exblock (,) (pat <* arrow')
   
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

exblock ::  (a -> Expr -> b) -> BParser a -> BParser b 
exblock f h = try (f <$> h <*> expr) <|> try (block f' h expr)
    where f' d es = f d (chain es)

chain = foldl1 chain'
    where chain' (Let bg (Lit LNull)) e2 = Let bg e2
          chain' e1 e2 = Ap (Var "!seq") (prod [e1, e2])

uOp s = Prefix $ try $
    rword s *> notFollowedBy (symbol ">") *>
       return (\e -> opAp (s++"UN") (prod [e]))

bOp s = InfixL $ try $
    rword s *> notFollowedBy (symbol ">") *>
        return (\e1 e2 -> opAp s (prod [e1, e2]))

opAp s = Ap (Var s)

argArrow = csl arg <* arrow'
    where arg = (,) <$> lName <*> opSufCons

fArgs = prod <$> parenCsl expr

eUnit = Lit LNull

funcAnnot :: [Maybe (Qual Type)] -> Maybe (Qual Type) -> BParser (Maybe (Qual Type))
funcAnnot ts Nothing 
    | all isNothing ts = return Nothing
    | otherwise = do
        rt <- newTVar Star
        funcAnnot ts (Just $ [] :=> rt)
funcAnnot ts (Just rt) = do
    args <- prod <$> toVars ts
    return $ Just $ args `qualFn` rt

newTVar :: Kind -> BParser Type
newTVar k = do i <- get
               put (i+1)
               return (TVar $ Tyvar ("!var" ++ show i) k)

toVars :: [Maybe (Qual Type)]  -> BParser [Qual Type]
toVars [] = return []
toVars (Nothing:ts) = do tv <- newTVar Star
                         tss <- toVars ts
                         return $ ([] :=> tv) : tss
toVars (Just t :ts) = do tss <- toVars ts
                         return $ t:tss

quantUser :: Qual Type -> Scheme
quantUser qt = let tvs = filter userDef (tv qt)
               in quantify tvs qt
    where userDef (Tyvar ('!':_) _) = False
          userDef _ = True
