{- |
Module      : Parser.Patterns
Description : A Module that provides parsers for Patterns
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module contains parsers for Blossom Patterns.
-}

module Parser.Patterns (pat, pVar) where

import Parser.Core
import Parser.Sugar
import Language.Expressions
import Parser.Literals

pat :: BParser Pat
pat  = try pAs <|> choice [pVar, pNil, pLit, pCons, parens pat]

pCons :: BParser Pat
pCons = (PCons <$> uName <*> opCsl pat) <|> pList pat <|> pTup pat <|> pString

pAs :: BParser Pat
pAs = try (PAs <$> (lName <* char '#') <*> pCons)

pVar :: BParser Pat
pVar = PVar <$> lName

pNil :: BParser Pat
pNil = PNil <$ symbol "_"

pLit :: BParser Pat
pLit = PLit <$> literal
