module Parser.Exprs 
   ( expr
   , lambda
   , args
   , pat
   , exblock
   , quantUser
   ) where

import Parser.Core
import PreProcessor.Bindings
import Parser.Patterns
import Parser.Sugar
import Data.Maybe
import Language.Expressions
import Parser.Literals
import Language.Utils
import Parser.Types
import Text.Megaparsec.Expr
import Control.Monad.State

expr :: BParser Expr
expr = makeExprParser term operators <?> "expression"

term :: BParser Expr
term = choice [eAbs, eCase, eAp, eLet, eLit, terminating eVar, parens expr]

terminating :: BParser a -> BParser a
terminating p = try p <* notFollowedBy (char '.' <|> char '(')

eLit :: BParser Expr
eLit = Lit <$> literal <|> lString <|> eList expr <|> eTup expr

eVar :: BParser Expr
eVar = Var <$> try aName

-- lambdas with explicitly typed arguments are contained within annotations.
eAbs :: BParser Expr
eAbs = do
    (lam, mt) <- lambda
    return $ case mt of
        Just sc  -> Annot (Abs lam) (quantUser sc)
        Nothing -> Abs lam

data ApDta = Field Id | RegAp [Expr]

eAp :: BParser Expr
eAp = try $ do 
    h <- parens expr <|> eVar
    tails <- some apdta
    return $ foldl makeAp h tails
    where makeAp ex (Field n) = Ap (Var $ '_':n) ex
          makeAp ex (RegAp es) = foldl Ap ex es

apdta :: BParser ApDta
apdta = (Field <$> (dot_ *> lName)) <|> (RegAp <$> csl expr)

eLet :: BParser Expr
eLet = try $ do 
           i <- lName 
           sch <- opSufCons 
           ex <- equals_ *> expr
           return $ case sch of
              Just s -> Let [Expl (i, quantAll s, expr2Alt ex)] eUnit
              _ -> Let [Impl (i, expr2Alt ex)] eUnit

eCase :: BParser Expr
eCase = Case <$> header <*> inlineBlock branch
    where header = case_ *> expr <* of_
          branch = do
            p <- pat <* arrow_
            ex <- exblock
            return ([p], ex)
   
{-eAnnot :: BParser Expr-}
{-eAnnot = Annot <$> expr <*> (quantAll <$> sufCons)-}

lambda :: BParser (Alt, Maybe (Qual Type))
lambda = do (p, mt) <- args
            ex <- exblock
            return ((p,ex), mt)

args :: BParser ([Pat], Maybe (Qual Type))
args = do 
    (as,qts) <- unzip <$> csl arg 
    rt <- opSufCons <* arrow_
    mt <- funcAnnot qts rt
    let p = map PVar as
    return (p, mt)
    where arg = (,) <$> lName <*> opSufCons

operators :: [[Operator (StateT (Int, Int) (Parsec String)) Expr]]
operators = [
{-[uOp "+", uOp "-"],-}
         [bOp "*", bOp "/", bOp "//", bOp "%"],
         [bOp "+", bOp "-"],
         [bOp "==", bOp "<=", bOp "<", bOp ">=", bOp ">"],
         [uOp "not"],
         [bOp "and", bOp "or", bOp "xor"]]
    where uOp s = Prefix $ try $
            rword s *> return (opAp (s++"UN"))
          bOp s = InfixL $ try $
              rword s *> notFollowedBy (symbol "=") *>
                  return (\e1 e2 -> Ap (opAp s e1) e2)


exblock ::  BParser Expr
exblock = chain <$> inlineBlock expr

chain :: [Expr] -> Expr
chain [e] = e 
chain (Let bg (Lit LNull):es) = Let bg (chain es)
chain (e1:e2) = Ap (Ap (Var "!seq") e1) (chain e2)
chain _ = error "Chain applied to an empty list"


opAp :: Id -> Expr -> Expr
opAp s = Ap (Var s)

{-argArrow :: BParser [(Id, Maybe (Qual Type))]-}
{-argArrow = csl arg <* arrow_-}
    {-where arg = (,) <$> lName <*> opSufCons-}

{-fArgs :: BParser [Expr]-}
{-fArgs = csl expr-}

eUnit :: Expr
eUnit = Lit LNull

funcAnnot :: [Maybe (Qual Type)] -> Maybe (Qual Type) -> BParser (Maybe (Qual Type))
funcAnnot ts Nothing 
    | all isNothing ts = return Nothing
    | otherwise = do
        rt <- newTVar Star
        funcAnnot ts (Just $ [] :=> rt)
funcAnnot ts (Just rt) = do
    as <- toVars ts
    return $ Just $ as `mkQualFn` rt

newTVar :: Kind -> BParser Type
newTVar k = do i <- getTV
               putTV (i+1)
               return (TVar $ Tyvar ("!var" ++ show i) k)

toVars :: [Maybe (Qual Type)]  -> BParser [Qual Type]
toVars [] = return []
toVars (Nothing:ts) = do t <- newTVar Star
                         tss <- toVars ts
                         return $ ([] :=> t) : tss
toVars (Just t :ts) = do tss <- toVars ts
                         return $ t:tss

quantUser :: Qual Type -> Scheme
quantUser qt = let tvs = filter userDef (tv qt)
               in quantify tvs qt
    where userDef (Tyvar ('!':_) _) = False
          userDef _ = True
