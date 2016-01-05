{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Constraints where

import ParserUtils
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char

data Constraint where
    None :: Constraint
    Partial :: Type -> Constraint
    Concrete :: Type -> Constraint

data Type = Type String [Constraint]

makeCons n [] = Concrete $ Type n []
makeCons n cs = if concrete cs
                   then Concrete $ Type n cs
                   else Partial $ Type n cs

makeFunCons args rt = if concrete args 
                        then Concrete $ Type (f rt) args
                        else Partial $ Type (f rt) args
    where f rt = "Function to a " ++ show rt

concrete = all p 
    where p (Concrete _) = True
          p _            = False

fType args rt = Type ("function to a " ++ show rt) args


------------------------------ Constraint Parsing -----------------------------

-- parses a constraint if it exists, otherwise returns none
opCons = try (miChar ':' *> miLexeme constraint) <|> return None

-- parses a constraint of fails
constraint = try vanillaCons <|> funCons

-- parses a functional constraint ie <T1,T2>::T3
funCons = makeFunCons <$> consParams <*> opCons

-- parses a non functional constraint ie T1<T2,T3>
vanillaCons = vanillaCons' miName

varCons = vanillaCons' miLName

conCons = vanillaCons' miUName

vanillaCons' p = makeCons <$> p <*> (concat <$> optional consParams)

-- Parses constraint parameters, ie the stuff between angle brackets
consParams = miLexeme $ aBrackets commaSepCs 
    where commaSepCs = constraint `sepBy` comma'

instance Show Constraint where
    show None = "Anything"
    show (Concrete t) = prettyType t
    show (Partial t) = prettyType t

prettyType (Type name cs) = "Type: " ++ name ++ "<" ++ showStruct cs ++ ">" 
