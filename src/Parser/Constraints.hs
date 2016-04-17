{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Parser.Constraints where

import Parser.Core
import qualified Data.Foldable as F
import Models.Expressions
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad (liftM, void)
import Types.Utils
import Text.Megaparsec

-- TODO support Tuple sugar

inlineQual = opList $ qual <* dot_

topQual = opList $ qual <* (void dot_ <|> void eol_)

qual = given_ *> sepBy1 pred comma_ 
    where pred = IsIn <$> uName <*> csl ptype

-- parses an optional suffix ptype
opSufCons :: BParser (Maybe (Qual Type))
opSufCons = optional sufCons

sufCons = colon_ *> qualType

-- parses a ptype or fails
qualType = (:=>) <$> inlineQual <*> ptype

ptype = choice [pcons, pvar, pfun]

genType n p c = do
    name <- n
    params <- opList (angles1 p)
    let k = foldl (\s _ -> KFun Star s) Star params
    return $ foldl TAp (c name k) params

pvar = genType lName ptype mkVar

pcons = genType uName ptype mkCons

pfun = mkFun <$> angles ptype <*> (arrow_ *> ptype) 

-- Variable with all variable parameters
vVar = genType lName vVar mkVar

-- Constructor with all variable parameters
vCons = genType uName vVar mkCons

sugar = try unit <|> try list 
    where unit = symbol "()" >> return tUnit
          list = tList <$> brackets ptype 
