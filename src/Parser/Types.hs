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
import Parser.IR.Types
{-import Parser.Sugar-}

-- | Parses a type constraint (a list of predicates). Ex: 'Given Eq(a), Show(a).'
constraint :: BParser [Pred]
constraint = opList (given_ *> sepBy1 pred' comma_  <* dot_)
    where pred' = IsIn <$> uName <*> csl ptype

-- | Parses a type annotation. Ex: ': Thing<a>'
typeAnnotation :: BParser Type
typeAnnotation = colon_ *> ptype

-- | Parses a qualified type. Ex: 'Given Eq(a). <a, a> -> Bool'
qualType :: BParser Type
qualType = Given <$> constraint <*> ptype

-- | Parses an unqualified type. Ex: 'List<a>'
ptype :: BParser Type
ptype = choice [pMono, pPoly, pFun, sugar]

-- | Parses a Polymorphic type
pPoly :: BParser Type
pPoly = TPoly <$> aName <*> angles1 ptype

-- | Parses a Monomorphic type
pMono :: BParser Type
pMono = TMono <$> lName

-- | Parses a function type. Ex: '<List<a>, a> -> Bool'
pFun :: BParser Type
pFun = TFunc <$> angles ptype <*> (arrow_ *> ptype) 

-- | Parses sugary types, the Unit type, and the list type and 
sugar :: BParser Type
sugar = try unit <|> lst <|> tuple
    where unit = symbol "()" >> return TUnit
          lst = TList <$> brackets ptype
          tuple = TTuple <$> csl ptype
