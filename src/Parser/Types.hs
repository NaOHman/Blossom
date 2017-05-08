{- |
Module      : Parser.Types
Description : Parsers for Types
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module defines a number of parsers which can be used to parse Blossom Types and Type Schemes.
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

-- | Parses a type constraint (a list of predicates). Ex: 'Given Eq(a), Show(a).'
constraint :: BParser [Pred]
constraint = opList (given_ *> sepBy1 pred' comma_  <* dot_)
    where pred' = IsIn <$> uName <*> csl ptype

-- | Parses a type annotation. Ex: ': Thing<a>'
typeAnnotation :: BParser (Maybe (Qual Type))
typeAnnotation = optional (colon_ *> qualType)

-- | Parses a qualified type. Ex: 'Given Eq(a). <a, a> -> Bool'
qualType :: BParser (Qual Type)
qualType = (:=>) <$> constraint <*> ptype

-- | Parses an unqualified type. Ex: 'List<a>'
ptype :: BParser Type
ptype = choice [pAp, pCons, pVar, pFun, sugar]

-- | Parses an applied type.
pAp :: BParser Type
pAp = try $ do
    name@(n:_) <- aName
    params <- angles1 ptype
    let k = foldl (\s _ -> KFun Star s) Star params
        tf = if isLower n 
                then TVar name k
                else TCons name k
    return $ foldl TAp tf params

-- | Parses a type variable. The kind is assumed to be *.
pVar :: BParser Type
pVar = TVar <$> lName <*> return Star

-- | Parses a type constructor. The kind is assumed to be *
pCons :: BParser Type
pCons = TCons <$> uName <*> return Star

-- | Parses a function type. Ex: '<List<a>, a> -> Bool'
pFun :: BParser Type
pFun = mkFun <$> angles ptype <*> (arrow_ *> ptype) 

-- | Parses sugary types, the Unit type, and the list type and 
sugar :: BParser Type
sugar = try unit <|> lst <|> tuple
    where unit = symbol "()" >> return tUnit
          lst = tList <$> brackets ptype
          tuple = tTup ptype
