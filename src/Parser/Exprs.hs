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
   , statement
   , eFunc
   , exblock
   {-, quantUser-}
   {-, letBind-}
   ) where

import Parser.Core
import Parser.Patterns
{-import Parser.Sugar-}
import Parser.IR.Expressions
{-import Parser.IR.Types-}
{-import Language.Bindings-}
import Parser.Literals
{-import Language.Utils-}
import Parser.Types
import Text.Megaparsec.Expr
import Control.Monad.State

-- | A parser that parses an expression.
expr :: BParser ParseExpr
expr = makeExprParser term operators <?> "expression"

statement :: BParser ParseExpr
statement = eFunc <|> expr

-- | Parses a term. Basically any expression that isn't an operator application
term :: BParser ParseExpr
term = choice blockExpressions <|> longestMatch

longestMatch :: BParser ParseExpr
longestMatch = do
    lhs <- choice $ terminalExpressions ++ 
                    map parens blockExpressions ++
                    [parens expr]
    greedyParse lhs

greedyParse :: ParseExpr -> BParser ParseExpr
greedyParse lhs = (try continuation >>= greedyParse) <|> return lhs
    where continuation = choice $ map ($ lhs) complexExpressions

-- TODO(naohman) explain the rational for these different groups
-- | Expressions which usually require and indentation block. They create a new
-- scope, and require parentheses if used in a sub expression.
blockExpressions :: [BParser ParseExpr]
blockExpressions = [eAbs, eCase] 

-- | Expressions that do not conatain sub expressions.
terminalExpressions :: [BParser ParseExpr]
terminalExpressions = [eLit, eLet, eVar, eList, eTup]

-- | Expressions that require sub expressions. Each of these parsers takes a
-- left hand expression and returns a parser which can parse the full
-- expression. For instance the function call `max(myList)` is composed of 2
-- sub expressions, the variable `max`  and the variable `myList` the variable
-- max will have already been parsed and it will be passed to the funtcion call
-- parser which will combine the variable max with the argument myList
complexExpressions :: [ParseExpr -> BParser ParseExpr]
complexExpressions = [functionCall, annotation, fieldAccess]

-- | Parse a literal ParseExpression
eLit :: BParser ParseExpr
eLit = Lit <$> literal

-- | Parse a variable function
eVar :: BParser ParseExpr
eVar = Var <$> try aName

-- | Parse a lambda abstraction lambdas with explicitly typed arguments are contained within annotations.
-- TODO defer quantifying types?
eAbs :: BParser ParseExpr
eAbs = Lambda <$> try (csl arg) <*> (try (optional typeAnnotation) <* arrow_) <*> exblock
    where arg = (,) <$> lName <*> optional typeAnnotation

-- | Field function application obj.field is syntactic sugar for _field(obj)
fieldAccess :: ParseExpr -> BParser ParseExpr
fieldAccess objectExpr = Field objectExpr <$> (dot_ *> lName)

-- | Function application, ex. method(arg1, arg2)
functionCall :: ParseExpr -> BParser ParseExpr
functionCall functionExpr = Call functionExpr <$> csl expr

-- | Parse a let binding.
eLet :: BParser ParseExpr
eLet = Binding <$> lName <*> (equals_ *>) expr


-- | Parses a function definition
eFunc :: BParser ParseExpr
eFunc = do 
      cons <- optional constraint
      fname <- fun_ *> lName
      Lambda as rt bdy <- eAbs
      return $ Func $ Function cons fname as rt bdy

-- | Parse a case expression
eCase :: BParser ParseExpr
eCase = Case <$> header <*> inlineBlock branch
    where header = case_ *> expr <* of_
          branch = Branch <$> (pat <* arrow_) <*> exblock
   
-- | Parse a potentially annotated expression
annotation :: ParseExpr -> BParser ParseExpr
annotation annotatedExpr = Annotation annotatedExpr <$> (typeAnnotation <* notFollowedBy (char '(' <|> char '.'))

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
exblock = inlineBlock statement

opAp :: String -> [ParseExpr] -> ParseExpr
opAp s = Call (Var s)

eList :: BParser ParseExpr
eList = EList <$> bracketList expr

eTup :: BParser ParseExpr
eTup = ETuple <$> csl expr
