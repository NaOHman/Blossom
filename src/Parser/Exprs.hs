{- |
Module      : Parser.Exprs
Description : Parsers for ParseExpressions
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module defines a number of parsers which can be used to parse Blossom ParseExpressions.
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
{-import Parser.Sugar-}
import Data.Maybe
import Parser.IR.Expressions
import Parser.IR.Types
{-import Language.Bindings-}
import Parser.Literals
{-import Language.Utils-}
import Parser.Types
import Text.Megaparsec.Expr
import Control.Monad.State

-- | A parser that parses an expression.
expr :: BParser ParseExpr
expr = makeExprParser term operators <?> "expression"

-- | Parses a term. Basically any expression that isn't an operator application
term :: BParser ParseExpr
term = choice [eAbs, eCase, annotationAllowed eAp, eLet, 
               annotationAllowed eLit, annotationAllowed (terminating eVar),
               annotationAllowed (parens expr)]

-- | Modifies a parser to reject incomplete expressions.
terminating :: BParser a -> BParser a
terminating parser = try parser <* notFollowedBy (char '.' <|> char '(')

-- | Parse a literal ParseExpression
eLit :: BParser ParseExpr
eLit = Lit <$> literal <|> eList <|> eTup

-- | Parse a variable function
eVar :: BParser ParseExpr
eVar = Var <$> try aName

-- | Parse a lambda abstraction lambdas with explicitly typed arguments are contained within annotations.
-- TODO defer quantifying types?
eAbs :: BParser ParseExpr
eAbs = Lambda <$> csl arg <*> (typeAnnotation <* arrow_) <*> exblock
    where arg = (,) <$> lName <*> typeAnnotation

-- | Parse an application expression
eAp :: BParser ParseExpr
eAp = fieldAp <|> functionAp

-- | Field function application obj.method is syntactic sugar for method(obj)
fieldAp :: BParser ParseExpr
fieldAp = try $ do
    object <- expr -- TODO longest matching parse.
    myField <- dot_ *> lName
    return $ Field object myField

-- | Function application, classic method(obj)
functionAp :: BParser ParseExpr
functionAp = try $ do
    function <- expr -- TODO longest matching parse.
    myArgs <- csl expr
    return $ Call function myArgs

-- | Parse a let binding.
eLet :: BParser ParseExpr
eLet = Binding <$> lName <*> (equals_ *>) expr


-- | Parses a recursive let binding (also known as a function).
{-eLetRec :: BParser ParseExpr-}
{-eLetRec = try $ do -}
      {-qual <- try constraint-}
      {-name <- fun_ *> lName-}
      {-lambda <- eAbs-}
      {-return $ case lambda of-}
          {-Annot (ex :-: (qs:=>t)) -> Annot $ -}
                {-LetRec name ex eUnit :-: ((qual ++ qs):=>t)-}
          {-ex -> LetRec name ex eUnit-}
          --TODO fail on unbound qual

-- | Annotate an expression if given a qualified type. 
-- TODO somewhere else
maybeAnnotate :: ParseExpr -> Maybe Type -> ParseExpr
maybeAnnotate ex (Just t) = Annotation t ex
maybeAnnotate ex _             = ex

-- | Parse a case expression
eCase :: BParser ParseExpr
eCase = Case <$> header <*> inlineBlock branch
    where header = case_ *> expr <* of_
          branch = Branch <$> (pat <* arrow_) <*> exblock
   
-- | Parse a potentially annotated expression
annotationAllowed :: BParser ParseExpr -> BParser ParseExpr
annotationAllowed parser = maybeAnnotate <$> parser <*> typeAnnotation

-- | A list of blossom builtin operators.
operators :: [[Operator (StateT (Int, Int) (Parsec String)) ParseExpr]]
operators = [
{-[uOp "+", uOp "-"],-}
         [bOp "*", bOp "/", bOp "//", bOp "%"],
         [bOp "+", bOp "-"],
         [bOp "==", bOp "<=", bOp "<", bOp ">=", bOp ">"],
         [uOp "not"],
         [bOp "and", bOp "or", bOp "xor"]]
    where uOp s = Prefix $ try $
            rword s *> return (\e -> opAp (s++"UN") [e])
          bOp s = InfixL $ try $
              rword s *> notFollowedBy (symbol "=") *>
                  return (\e1 e2 -> opAp s [e1, e2])


-- | Parse a block of expressions chained with the sequence operator.
exblock ::  BParser [ParseExpr]
exblock = inlineBlock expr

opAp :: String -> [ParseExpr] -> ParseExpr
opAp s = Call (Var s)

eUnit :: ParseExpr
eUnit = Lit LNull

eList :: BParser ParseExpr
eList = EList <$> bracketList expr

eTup :: BParser ParseExpr
eTup = ETuple <$> csl expr
