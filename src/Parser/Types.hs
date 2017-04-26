{- |
Module      : Parser.Types
Description : Parsers for Types
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module defines a number of parsers which can be used to parse Blossom Types and qualifiers.
-}

module Parser.Types 
    ( constraint
    , typeAnnotation
    , qualType
    , ptype
    )

where

import Parser.Core
import Data.Char
import Language.Expressions
import Language.Utils
import Parser.Sugar

-- Given X is in Y, Z is in Y.
constraint :: BParser [Pred]
constraint = opList (given_ *> sepBy1 pred' comma_  <* dot_)
    where pred' = IsIn <$> uName <*> csl ptype

-- Annotation Parsing, always optional
typeAnnotation :: BParser (Maybe (Qual Type))
typeAnnotation = optional (colon_ *> qualType)

-- Inline Qualified type
qualType :: BParser (Qual Type)
qualType = (:=>) <$> constraint <*> ptype

-- Parses any kind of type
ptype :: BParser Type
ptype = choice [pAp, pCons, pVar, pFun, sugar]

-- Parses a type application
pAp :: BParser Type
pAp = try $ do
    name@(n:_) <- aName
    params <- angles1 ptype
    let k = foldl (\s _ -> KFun Star s) Star params
        tf = if isLower n 
                then TVar name k
                else TCons name k
    return $ foldl TAp tf params

-- Parses a Type variable with Kind *
pVar :: BParser Type
pVar = TVar <$> lName <*> return Star

-- Parses a Type constructor with Kind *
pCons :: BParser Type
pCons = TCons <$> uName <*> return Star

-- Parses a function type
pFun :: BParser Type
pFun = mkFun <$> angles ptype <*> (arrow_ *> ptype) 

sugar :: BParser Type
sugar = try unit <|> lst <|> tuple
    where unit = symbol "()" >> return tUnit
          lst = tList <$> brackets ptype
          tuple = tTup ptype
