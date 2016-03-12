module Models.Core where

import Text.Megaparsec.Pos
import Text.Megaparsec

type Id = String

data Lex a = Lex SourcePos a

instance Show a => Show (Lex a) where
    show (Lex p a) = show a

nulLex = Lex nullPos
nullPos = newPos "Built in" 1 1

unwrap :: Lex a -> a
unwrap (Lex p a) = a

{-lift :: (a -> b) -> Lex a -> b-}

withLex :: (SourcePos -> a -> b) -> Lex a -> b
withLex f (Lex p a) = f p a

instance Functor Lex where
    fmap f (Lex p a) = Lex p (f a)

instance (Eq a) => Eq (Lex a) where
    (==) (Lex _ a) (Lex _ b) = a == b

withPos p = Lex <$> getPosition <*> p
