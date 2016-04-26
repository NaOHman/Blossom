{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Parser.Types where

import Parser.Core
import Parser.Literals
import Models.Expressions
import Types.Utils
import LangDef.Blossom
import LangDef.Sugar

--Qualifier parsing
inlineQual = opList (qual <* dot_)

topQual = opList (qual <* (dot_ <|> eol_))

qual = given_ *> sepBy1 pred comma_ 
    where pred = IsIn <$> uName <*> csl ptype

-- Optional Annotation Parsing
opSufCons = optional sufCons

-- Mandatory Annotation Parsing
sufCons = colon_ *> qualType

-- Inline Qualified type
qualType = (:=>) <$> inlineQual <*> ptype

-- Parses any kind of type
ptype = choice [pcons, pvar, pfun]

genType n p c = do
    name <- n
    params <- opList (angles1 p)
    let k = foldl (\s _ -> KFun Star s) Star params
    return $ foldl TAp (c name k) params

-- Parses a type that may be applied to other types
pvar = genType lName ptype mkVar

-- Parses a Type constructor that may be applied to other type
pcons = genType uName ptype mkCons <|> sugar ptype

-- Parses a function type
pfun = mkFun <$> angles ptype <*> (arrow_ *> ptype) 

-- Variable with all variable parameters
vVar = genType lName vVar mkVar

-- Constructor with all variable parameters
vCons = genType uName vVar mkCons <|> sugar vVar

sugar p = try unit <|> list <|> tuple
    where unit = symbol "()" >> return tUnit
          list = tList <$> brackets p
          tuple = tTup p
