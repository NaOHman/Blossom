module Parser.Patterns (pat, pVar) where

import Parser.Core
import Parser.Sugar
import Language.Expressions
import Parser.Literals

pat :: BParser Pat
pat  = try pAs <|> choice [pVar, pNil, pLit, pCons]

pCons :: BParser Pat
pCons = (PCons <$> uName <*> opCsl pat) <|> pList pat <|> pTup pat

pAs :: BParser Pat
pAs = try (PAs <$> (lName <* char '#') <*> pat)

pVar :: BParser Pat
pVar = PVar <$> lName

pNil :: BParser Pat
pNil = PNil <$ symbol "_"

pLit :: BParser Pat
pLit = PLit <$> literal
