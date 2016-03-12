{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Parser.Exprs where

import Parser.Core
import Models.Expressions
import Parser.Literals
import Parser.Constraints
import Control.Monad (liftM)
import Text.Megaparsec
import Text.Megaparsec.Expr

eLit = withPos (ELit <$> literal expr)

eVar = withPos (EVar <$> lName)

eAbs = withPos $ EAbs <$> argArrow <*> exblock

eAp = withPos (namedFCall <|> subRef)
    where subRef = do ex@(Lex p _) <- eVar
                      field <- char '.' *> eVar
                      return $ EAp field [parg ex]
          namedFCall = EAp <$> eVar <*> parenCsl args
          args = tryList [withPos kArg, withPos pArg]
          pArg = PArg <$> expr
          kArg = KArg <$> (lName <* equals') <*> expr

eLet = withPos (ELet <$> (lName <* equals') <*> nil)
    where nil = withPos (return EUnit) 
          varName = toBg <$> getPosition <*> lName <*> expr 
          toBg p i e = ([], [(i, [(Lex p (PVar i), e)])])
    --error if found by preprocessor

eCase = withPos (letCase <|> blockCase)
    where letCase = do 
            pt <- pat
            ex <- expr
            return $ ECase ex [(pt, bindgroup pt ex)]
          bindgroup = undefined
          blockCase = ECase <$> header <*> iBlock aCase
          header = case_ *> expr <* of_
          aCase = (,) <$> (pat <* arrow') <*> exblock 
   

pat = withPos pat'
pat' = tryList [cons',  as', lit', var', nil']
    where 
          cons' = PCons <$> uName <*> csl pat'
          as'   = PAs <$> (lName <* char '#') <*> pat'
          lit'  = PLit <$> cTimeLit
          var'  = PVar <$> lName
          nil'  = symbol "_" >> return PNil

cTimeLit = literal (withPos (ELit <$> cTimeLit))

expr = makeExprParser term operators <?> "expression"
--todo exif
term = tryList [eCase, eAp, eLet, eLit, eVar, parens expr]

operators = [[uOp "+", uOp "-"],
         [bOp "*", bOp "/", bOp "//", bOp "%"],
         [bOp "+", bOp "-"],
         [bOp "==", bOp "<", bOp "<=", bOp ">", bOp ">="],
         [uOp "not"],
         [bOp "and", bOp "or", bOp "xor"]]

exblock ::  BParser Expr
exblock = liftM chain (iBlock expr) 
    where chain [e] = e
          chain (e:es) = foldl conChain e es
          conChain (Lex p (ELet bg _)) e2 = 
                Lex p $ ELet bg e2
          conChain e1@(Lex p _) e2 = seq' p e1 e2
          seq' p e1 e2 = Lex p $ EAp (Lex p $ EVar "!seq") 
                            [parg e1, parg e2]

uOp s = Prefix (try (do
    p <- getPosition <* rword s
    return (\e -> opAp p (s++"UN") [parg e])))

bOp s = InfixL (try (do
    p <- getPosition <* rword s
    return $ \e1 e2 -> opAp p s [parg e1, parg e2]))

opAp p s as = Lex p $ EAp (Lex p $ EVar s) as

parg (Lex p e) = Lex p $ PArg (Lex p e)

argArrow = csl argDec <* arrow'
    where argDec = withPos $ try kwdec <|> try pdec
          pdec = PDec <$> lName <*> opSufCons
          kwdec = KDec <$> lName <*> (equals' *> cTimeLit) <*> opSufCons
