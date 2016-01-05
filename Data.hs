{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Statements where

import ParserUtils
import Models
import Literals
import Control.Monad (void, foldM, ap)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

--------------------------- Class Parsing -----------------------------

classDec = genDec header "when" fStub
    where header = Class <$> varCons <*> is' miUName
          fStub = Stub <$> (fun' *> miLName) <*> sArgs <*> opCons
          sArgs = genArgs parens [sNamed, sAnon]
          sNamed = SNamed <$> miLName <*> opCons
          sAnon  = SAnon <$> constraint

implementaton = genDec header "because" function
    where header = Implementation <$> conCons <*> is' miUName

--------------------------- Data Declaration Parsing --------------------------

-- Parses a data declaration
dataDec = genDec header "where" constructor
    where header = Data <$> dta' miUName <*> ps
          ps = miLexeme (fromMaybe [] <$> optional consParams)
  
-- Parses a constructor
constructor = Constructor <$> miUName <*> cnstrParams <*> opCons <*> pure Nothing

cnstrParams = genArgs parens [named, anon, keyw, anonKey]
    where named = Named <$> miLName <*> opCons
          anon = Anon <$> constraint
          keyw = do
            n <- miLName 
            (AnonKey l cns) <- anonKey
            return $ Keyword n l cns
          anonKey = AnonKey <$> (miEquals *> miLiteral) <*> opCons
