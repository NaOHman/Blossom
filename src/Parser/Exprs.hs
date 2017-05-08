{- |
Module      : Parser.Exprs
Description : Parsers for Expressions
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module defines a number of parsers which can be used to parse Blossom Expressions.
-}

module Parser.Exprs 
   ( expr
   , pat
   , eLet
   , eLetRec
   , exblock
   , quantUser
   , letBind
   ) where

import Parser.Core
import Parser.Patterns
import Parser.Sugar
import Data.Maybe
import Language.Expressions
import Language.Bindings
import Parser.Literals
import Language.Utils
import Parser.Types
import Text.Megaparsec.Expr
import Control.Monad.State

-- | A parser that parses an expression.
expr :: BParser Expr
expr = makeExprParser term operators <?> "expression"

-- | Parses a term. Basically any expression that isn't an operator application
term :: BParser Expr
term = choice [eAbs, eLetRec, eCase, annotationAllowed eAp, eLet, 
               annotationAllowed eLit, annotationAllowed (terminating eVar),
               annotationAllowed (parens expr)]

-- | Modifies a parser to reject incomplete expressions.
terminating :: BParser a -> BParser a
terminating parser = try parser <* notFollowedBy (char '.' <|> char '(')

-- | Parse a literal Expression
eLit :: BParser Expr
eLit = Lit <$> literal <|> lString <|> eList expr <|> eTup expr

-- | Parse a variable function
eVar :: BParser Expr
eVar = Var <$> try aName

-- | Parse a lambda abstraction lambdas with explicitly typed arguments are contained within annotations.
-- TODO defer quantifying types?
eAbs :: BParser Expr
eAbs = do
    (pats, maybeArgTypes) <- unzip <$> csl arg
    maybeReturnType <- typeAnnotation <* arrow_
    maybeType <- funcAnnot maybeArgTypes maybeReturnType
    ex <- exblock
    return $ maybeAnnotate (Abs pats ex) maybeType
    where arg = (,) <$> pVar <*> typeAnnotation

-- | Parse an application expression
eAp :: BParser Expr
eAp = fieldAp <|> functionAp

-- | Field function application obj.method is syntactic sugar for method(obj)
fieldAp :: BParser Expr
fieldAp = try $ do
    object <- parens expr <|> eVar
    myField <- dot_ *> lName
    return $ Ap (fieldToVar myField) object
    where fieldToVar f = Var $ '_':f

-- | Function application, classic method(obj)
functionAp :: BParser Expr
functionAp = try $ do
    function <- parens expr <|> eVar
    myArgs <- csl expr
    return $ foldl Ap function myArgs

-- | Parse a let binding.
eLet :: BParser Expr
eLet = try $ do 
    varName <- lName 
    maybeType <- typeAnnotation 
    myExpr <- equals_ *> expr
    return $ Let varName (maybeAnnotate myExpr maybeType) eUnit

letBind :: BParser Binding
letBind = try $ do 
    varName <- lName 
    maybeType <- typeAnnotation 
    myExpr <- equals_ *> expr
    return $ maybeBind myExpr varName maybeType


-- | Parses a recursive let binding (also known as a function).
eLetRec :: BParser Expr
eLetRec = try $ do 
      qual <- try constraint
      name <- fun_ *> lName
      lambda <- eAbs
      return $ case lambda of
          Annot (ex :-: (qs:=>t)) -> Annot $ 
                LetRec name ex eUnit :-: ((qual ++ qs):=>t)
          ex -> LetRec name ex eUnit
          --TODO fail on unbound qual

-- | Annotate an expression if given a qualified type. 
-- TODO somewhere else
maybeAnnotate :: Expr -> Maybe (Qual Type) -> Expr
maybeAnnotate ex (Just qt) = Annot (ex :-: qt)
maybeAnnotate ex _             = ex

-- | Annotate an expression if given a qualified type. 
-- TODO somewhere else
maybeBind :: Expr -> Id -> Maybe (Qual Type) -> Binding
maybeBind ex n (Just qt) = ExpB $ Expl n (quantAll qt) ex
-- Catch the case where the annotation is on the expression, not the variable 
maybeBind ex n _         = namedToBinding n ex


-- | Parse a case expression
eCase :: BParser Expr
eCase = Case <$> header <*> inlineBlock branch
    where header = case_ *> expr <* of_
          branch = do
            pattern <- pat <* arrow_
            expression <- exblock
            return (pattern, expression)
   
-- | Parse a potentially annotated expression
annotationAllowed :: BParser Expr -> BParser Expr
annotationAllowed parser = maybeAnnotate <$> parser <*> typeAnnotation

-- | A list of blossom builtin operators.
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


-- | Parse a block of expressions chained with the sequence operator.
exblock ::  BParser Expr
exblock = chain <$> inlineBlock expr

-- TODO fix chains for LetRecs and annotations
chain :: [Expr] -> Expr
chain [e] = e 
chain (Let i ex (Lit LNull):es) = Let i ex (chain es)
chain (e1:e2) = Ap (Ap (Var "!seq") e1) (chain e2)
chain _ = error "Chain applied to an empty list"


opAp :: Id -> Expr -> Expr
opAp s = Ap (Var s)

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
               return $ TVar ("!var" ++ show i) k

toVars :: [Maybe (Qual Type)]  -> BParser [Qual Type]
toVars [] = return []
toVars (Nothing:ts) = do t <- newTVar Star
                         tss <- toVars ts
                         return $ ([] :=> t) : tss
toVars (Just t :ts) = do tss <- toVars ts
                         return $ t:tss

-- TODO this belongs elsewhere
quantUser :: Qual Type -> Scheme
quantUser qt = let tvs = filter userDef (tv qt)
               in quantify tvs qt
    where userDef (Tyvar ('!':_) _) = False
          userDef _ = True
