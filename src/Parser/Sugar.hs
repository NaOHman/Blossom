{- |
Module      : Parser.Sugar
Description : Parsers for Syntactic Sugar
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module defines a number of parsers which help parse sugary constructs such as Lists, Strings,
and Tuples. Sugar is defined in it's own module because the same sugar generally applies to types, 
patterns, and expressions.
-}

module Parser.Sugar where

import Parser.Core
import Parser.IR.Expressions
import Parser.IR.Types
import Control.Monad (void)

lArray :: BParser Expr -> BParser Expr
lArray p = toList <$> brackets (sepBy p comma_)

{-tup p = try $ parens $ do-}
    {-head <- p <* comma_-}
    {-tail <- sepBy1 p comma_-}
    {-return (head:tail)-}

lString :: BParser Expr
lString = toList <$> map Lit <$> stringChars

pString :: BParser Pat
pString = toPatList <$> map PLit <$> stringChars

stringChars :: BParser [Literal]
stringChars = doubleQuotes (many (LChar <$> stringChar))

-- Parses a Tuple literal 
eTup :: BParser Expr -> BParser Expr
eTup = tup (foldl Ap . Var)

tTup :: BParser Type -> BParser Type
tTup = tup (const tTuple)

pTup :: BParser Pat -> BParser Pat
pTup p = try $ tup PCons p

tup :: (String -> [a] -> a) -> BParser a -> BParser a
tup f p = parens $ do 
    h <- p <* comma_
    t <- sepBy1 p comma_
    let items = h:t
    let cstr =  tplName (length items)
    return $ f cstr items

list :: a -> (a -> a -> a) -> BParser a -> BParser a
list nil cns p = try (nilList nil) <|> list' nil cns p

toList :: [Expr] -> Expr
toList = foldr cons (Var "[nil]") 
    where cons e = Ap (Ap (Var "[cons]") e )

toPatList :: [Pat] -> Pat
toPatList = foldr cons (PCons "[nil]" [])
    where cons p1 p2 = PCons "[cons]" [p1, p2]

eList :: BParser Expr -> BParser Expr
eList = list nil cns
    where nil = Var "[nil]"
          cns e = Ap (Ap (Var "[cons]") e)

pList :: BParser Pat -> BParser Pat
pList = list nil cns
    where nil = PCons "[nil]" []
          cns a b = PCons "[cons]" [a,b]

list' :: a -> (a -> a -> a) -> BParser a -> BParser a
list' n c p = brackets $ do
    ss <- sepBy1 p (symbol ",")
    t <- try (vert c p)<|> return n
    return $ foldr c t ss

vert :: (a -> a -> a) -> BParser a -> BParser a
vert c p = do
    void $ symbol "|"
    ps <- sepBy1 p (symbol "|")
    return $ foldr1 c ps
        
nilList :: a -> BParser a
nilList n = n <$ (symbol "[" *> symbol "]")
