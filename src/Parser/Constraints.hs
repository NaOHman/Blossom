{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Parser.Constraints where

import Parser.Core
import Models.Expressions
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Types.Utils
import Text.Megaparsec

------------------------------ Constraint Parsing -----------------------------

-- TODO support Tuple, Dict, and List sugar

-- parses an optional suffix ttype
opSufCons :: BParser (Maybe Type)
opSufCons = optional sufCons

sufCons = withPos sufCons'
sufCons' = colon' *> ttype'

-- parses a ttype or fails
ttype :: BParser Type
ttype = withPos ttype'
ttype' = tryList [scheme', mono', tvar', funCons'] 

mono :: BParser Type
mono = withPos mono'
mono' = tcons <$> (uName <|> unit) <*> return Star
    where unit = rword "()" >> return ""

tvar :: BParser Type
tvar = withPos tvar'
tvar' = TVar <$> (Tyvar <$> lName <*> return Star)
 
-- TODO fix this 
cons :: BParser Id -> BParser Type
cons p = withPos $ cons' p
cons' parser = do 
    p <- getPosition
    name <- parser
    params <- angles1 ttype
    let k = foldl (\s _ -> KFun Star s) Star params
    return $ foldl tAp (tcons name k) params
    where tAp t (Lex _ t') = TAp t t'

dataCons :: BParser Type
dataCons = withPos $ do
    n <- uName
    ts <- toList <$> optional (angles1 ttype')
    let k = kAry (length ts)
    return $ foldl TAp (TCons $ Tycon n k) ts
    where toList (Just as) = as
          toList Nothing   = []


parseProd = withPos . parseProd'
parseProd' = fmap tProduct

dataScheme :: BParser Scheme
dataScheme = liftM toScheme (instType >>= check . unwrap)
    where check t@(TCons _) = return t
          check _ = fail "Something is terribly wrong"

instType = try uScheme <|> try mono

uScheme = cons uName
scheme = cons aName

scheme' = cons' aName

funCons = withPos funCons'
funCons' = TFun <$> angles ttype' <*> rType
    where rType = unwrap <$> (arrow' *> ttype)
