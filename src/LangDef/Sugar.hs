module LangDef.Sugar where

import LangDef.Blossom
import Parser.Core
import Parser.Literals
import Models.Expressions

lArray p = toList <$> brackets (sepBy p comma_)

{-tup p = try $ parens $ do-}
    {-head <- p <* comma_-}
    {-tail <- sepBy1 p comma_-}
    {-return (head:tail)-}

lString = toList <$> doubleQuotes (many stringChar)
    where stringChar = Lit . LChar <$> (escapedChar <|> noneOf "\"\\\n\t\r")


-- Parses a Tuple literal 
eTup = tup (foldl Ap . Var)

tTup = tup (const tTuple)

pTup = tup PCons

tup :: (Id -> [a] -> a) -> BParser a -> BParser a
tup f p = parens $ do 
    head <- p <* comma_
    tail <- sepBy1 p comma_
    let items = head:tail
    let cstr =  tplName (length items)
    return $ f cstr items

list nil cns p = try (nilList nil) <|> list' nil cns p

toList = foldr cons (Var "[nil]") 
    where cons e = Ap (Ap (Var "[cons]") e )

eList = list nil cns
    where nil = Var "[nil]"
          cns e = Ap (Ap (Var "[cons]") e)

pList = list nil cns
    where nil = PCons "[nil]" []
          cns a b = PCons "[cons]" [a,b]

list' n c p = brackets $ do
    ss <- sepBy1 p (symbol ",")
    t <- try (vert c p)<|> return n
    return $ foldr c t ss

vert c p = do
    symbol "|"
    ps <- sepBy1 p (symbol "|")
    return $ foldr1 c ps
        
nilList n = n <$ (symbol "[" *> symbol "]")
