{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Exprs where

import ParserUtils
import Models
import Literals
import Constraints
import Control.Monad (void, foldM, ap, liftM)
import Control.Monad.State
import Data.List
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

parseBlossom = parseFromFile (evalStateT program 0)

program :: MyParser Program 
program = myFold (Program [] [] [] []) 
    where statements p = map ($p) [sfix, slet, sdat, scls, sinst]
          top p f prg = liftM (f prg) (indentGuard 1 *> p)
          sfix = top exfix (\p x -> p{globals = x:globals p})
          slet = top exlet (\p x -> p{globals = x:globals p})
          sdat = top dataDec (\p x -> p{datatypes = x:datatypes p})
          scls = top classDec (\p x -> p{classes = x:classes p})
          sinst = top inst (\p x -> p{instances = x:instances p})
          myFold p = do 
              next <- tryList $ statements p
              done <- optional eof
              case done of 
                Just _ -> return next
                Nothing -> myFold next
          
------------------------- Basic Expr Parsing -------------------------------
-- TODO add opCons to the end of argDec
exfcall = efcall lName (parenCsl argDecs)
    where argDecs = tryList [pos, ks, kwS, posS]
          pos = posArg expr
          ks = kwArg (lName <* equals') expr
          posS = pSplat (symbol "*" *> expr)
          kwS = kwSplat (symbol "**" *> expr)

dataDec :: MyParser Data
dataDec = Data <$> gp <*> dataName <*> constrs
    where constrs = block ((,) <$> uName <*> dataArgs)
          dataName = data_ *> dataCons <* where_

classDec :: MyParser Class
classDec = Class <$> gp <*> classCons <*> cheader <*> stubs
    where cheader = is' *> uName <* when'
          stubs = block $ (,,) <$> lName <*> csl consArg <*> recSufCons

inst :: MyParser Instance
inst = Instance <$> gp <*> instCons <*> iheader <*> block exfix 
    where iheader = is' *> uName <* because'

exlet = elet decons (equals' *> expr)
-- TODO let cases ex.
--  a = case even a of Just a = a
--  == a = cast(x,Just a)

-- TODO Fix onecase def
excase :: MyParser Expr
excase = ecase header (inlineOrBlock aCase)
    where header = case_ *> expr <* of_
          aCase = (,) <$> (decons <* arrow') <*> exblock 

exblock ::  MyParser Expr
exblock = liftM chain (inlineOrBlock expr) 

--Todo implement binding syntax
decons = tryList [name',  dec', nil', match', parens decons]
    where dec' = dcons uName $ csl decons
          match' = dmatch cTimeLit
          name' = dname lName
          nil' = dnil (symbol "_")

exvar = evar $ sepBy1 lName (string ".")

exlambda = elambda argArrow exblock

exfix :: MyParser Expr
exfix = efix funName argArrow exblock

chain :: [Expr] -> Expr
chain [e] = e
chain (e:es) = foldl conChain e es
    where conChain e1@(Expr p _) e2 = Expr p (EChain e1 e2)

expr = makeExprParser term operators <?> "expression"
--todo exif
term = tryList [excase, exfcall, exlet, exliteral, exvar, parens expr]

operators = [[uOp "+", uOp "-"],
         [bOp "*", bOp "/", bOp "//", bOp "%"],
         [bOp "+", bOp "-"],
         [bOp "==", bOp "<", bOp "<=", bOp ">", bOp ">="],
         [uOp "not"],
         [bOp "and", bOp "or", bOp "xor"]]

uOp s = Prefix (try (rword s) >> return (\e@(Expr p _) -> 
    Expr p (EFCall (s++"UN") [Arg p $ PosArg e])))

bOp s = InfixL (try (rword s) >> return (\e1@(Expr p _) e2 ->
    Expr p (EFCall s [Arg p $ PosArg e1, Arg p $ PosArg e2])))

exliteral :: MyParser Expr
exliteral = elit (literal expr)

cTimeLit :: MyParser Literal
cTimeLit = literal (elit cTimeLit)

-- Parses function arguments
argArrow =  csl (argWithCons opSufCons) <* arrow'

dataArgs = csl (argWithCons recSufCons <|> try consArg)
                
consArg  = argdec (PosDec <$> return "") constraint

argWithCons c = tryList $ map ($ c) [kwsdec, psdec, kwdec, pdec]
    where pdec = argdec (PosDec <$> lName)
          kwdec = argdec (KWDec <$> lName <*> (equals' *> cTimeLit))
          kwsdec = argdec (KWSDec <$> (lStr <* symbol "**"))
          psdec = argdec (PSDec <$> (lStr <* symbol "*"))
