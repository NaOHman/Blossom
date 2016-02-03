{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Constraints where

import ParserUtils
import Models
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char


------------------------------ Constraint Parsing -----------------------------

-- parses an optional suffix constraint
opSufCons = try recSufCons <|> return None

-- parse a required suffix constraint
recSufCons = colon' *> constraint

-- parses a constraint or fails
constraint = lexeme $ try valCons <|> funCons

-- parses a functional constraint ie <T1,T2>::T3
funCons = FunCons <$> angles constraint <*> opSufCons

-- parses a non functional constraint ie T1<t2,T3> or t
valCons = valCons' aName

valCons' p = ValCons <$> p <*>  consParams

consParams = fromMaybe [] <$> optional (angles constraint)

dataCons = valCons' uName

classCons = consOrList lName

instCons = consOrList uName

consOrList p = do 
    c <- optional p
    case c of
        Just n -> ValCons <$> return n <*> consParams
        Nothing -> ValCons <$> return "" <*> angles constraint


