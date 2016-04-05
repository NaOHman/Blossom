{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Parser.Constraints where

import Parser.Core
import qualified Data.Foldable as F
import Models.Expressions
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Types.Utils
import Text.Megaparsec

------------------------------ Constraint Parsing -----------------------------

-- TODO support Tuple and List sugar

-- parses an optional suffix ttype
opSufCons :: BParser (Maybe Type)
opSufCons = optional sufCons

sufCons = colon' *> ttype

-- parses a ttype or fails
ttype :: BParser Type
ttype = tryList [scheme, mono, tvar, funCons] 

mono :: BParser Type
mono = tcons <$> (uName <|> unit) <*> return Star
    where unit = rword "()" >> return ""

tvar :: BParser Type
tvar = TVar <$> (Tyvar <$> lName <*> return Star)
 
-- TODO fix this 
cons :: BParser Id -> BParser Type
cons parser = do 
    name <- parser
    params <- angles1 ttype
    let k = foldl (\s _ -> KFun Star s) Star params
    return $ foldl TAp (tcons name k) params

dataCons :: BParser Type
dataCons = do
    n <- uName
    ts <- F.concat <$> optional (angles1 ttype)
    let k = kAry (length ts)
    return $ foldl TAp (TCons $ Tycon n k) ts

parseProd = fmap tProduct

dataScheme :: BParser Scheme
dataScheme = liftM toScheme (instType >>= check)
    where check t@(TCons _) = return t
          check _ = fail "Something is terribly wrong"

instType = try uScheme <|> try mono

uScheme = cons uName
scheme = cons aName

funCons = TFun <$> angles ttype <*> (arrow' *> ttype)
