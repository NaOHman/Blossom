module Parser.Types where

import Parser.Core
import Language.Expressions
import Language.Utils
{-import LangDef.Blossom-}
import Parser.Sugar

--Qualifier parsing
inlineQual :: BParser [Pred]
inlineQual = opList (qual <* dot_)

topQual :: BParser [Pred]
topQual = opList (qual <* (dot_ <|> eol_))

qual :: BParser [Pred]
qual = given_ *> sepBy1 pred' comma_ 
    where pred' = IsIn <$> uName <*> csl ptype

-- Optional Annotation Parsing
opSufCons :: BParser (Maybe (Qual Type))
opSufCons = optional sufCons

-- Mandatory Annotation Parsing
sufCons :: BParser (Qual Type)
sufCons = colon_ *> qualType

-- Inline Qualified type
qualType :: BParser (Qual Type)
qualType = (:=>) <$> inlineQual <*> ptype

-- Parses any kind of type
ptype :: BParser Type
ptype = choice [pcons, pvar, pfun]

genType :: BParser Id -> BParser Type -> (Id -> Kind -> Type) -> BParser Type
genType n p c = do
    name <- n
    params <- opList (angles1 p)
    let k = foldl (\s _ -> KFun Star s) Star params
    return $ foldl TAp (c name k) params

-- Parses a type that may be applied to other types
pvar :: BParser Type
pvar = genType lName ptype mkVar

-- Parses a Type constructor that may be applied to other type
pcons :: BParser Type
pcons = genType uName ptype mkCons <|> sugar ptype

-- Parses a function type
pfun :: BParser Type
pfun = mkFun <$> angles ptype <*> (arrow_ *> ptype) 

-- Variable with all variable parameters
vVar :: BParser Type
vVar = genType lName vVar mkVar

-- Constructor with all variable parameters
vCons :: BParser Type
vCons = genType uName vVar mkCons <|> sugar vVar

sugar :: BParser Type -> BParser Type
sugar p = try unit <|> lst <|> tuple
    where unit = symbol "()" >> return tUnit
          lst = tList <$> brackets p
          tuple = tTup p
