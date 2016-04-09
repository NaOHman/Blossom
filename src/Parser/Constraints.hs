{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Parser.Constraints where

import Parser.Core
import qualified Data.Foldable as F
import Models.Expressions
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad (liftM)
import Types.Utils
import Text.Megaparsec

-- TODO support Tuple sugar


inlineQual = opList $ qual <* dot'

topQual = opList $ inlineQual <|> (qual <* eol')

qual = given *> sepBy1 pred comma' 
    where pred = IsIn <$> uName <*> parenCsl ptype

-- parses an optional suffix ptype
opSufCons :: BParser (Maybe (Qual Type))
opSufCons = optional sufCons

sufCons = colon' *> qualType

-- parses a ptype or fails
qualType = (:=>) <$> inlineQual <*> ptype

ptype = tryList [pcons, pvar, pfun]

genType n p c = do
    name <- n
    params <- opList (angles1 p)
    let k = foldl (\s _ -> KFun Star s) Star params
    return $ foldl TAp (c name k) params

pvar = genType lName ptype mkVar

pcons = genType uName ptype mkCons

pfun = mkFun <$> angles ptype <*> (arrow' *> ptype) 

-- Variable with all variable parameters
vVar = genType lName vVar mkVar

-- Constructor with all variable parameters
vCons = genType uName vVar mkCons

sugar = try unit <|> try list 
    where unit = rword "()" >> return tUnit
          list = TAp tList <$> brackets ptype 
