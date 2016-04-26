{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Parser.Patterns (pat, pVar) where

import Parser.Core
import LangDef.Sugar
import Models.Expressions
import Parser.Literals

pat  = try pAs <|> choice [pVar, pNil, pLit, pCons]

pCons = (PCons <$> uName <*> opCsl pat) <|> pList pat <|> pTup pat

pAs = try (PAs <$> (lName <* char '#') <*> pat)
pVar = PVar <$> lName
pNil = PNil <$ symbol "_"
pLit = PLit <$> literal
