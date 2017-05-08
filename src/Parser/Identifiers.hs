{- |
Module      : Parser.Identifiers
Description : The parsers for Blossom keywords and identifiers
Copyright   : (c) Jeffrey Lyman
Liscense    : TBD

Maintainer  : JeffreyTLyman@gmail.com
Stability   : experimental
Portability : portable

This module provides parsers for all keywords. All parsers have an underscore suffix to 
prevent clashes with haskell keywords. Ex. the parser for if is called if_. These keywords
parsers are necessary to avoid restricted word errors.
-}

module Parser.Identifiers 
   ( is_, because_, inherits_, given_, if_, elif_, then_, else_, when_, where_
   , data_, case_, type_, of_, arrow_, equals_, comma_, dot_, colon_, fun_, minus_
   , true_, false_, curry_, send_, request_
   , lName, aName, uName, rword 
   ) where

import Control.Monad (void)
import Parser.Lexeme
import Text.Megaparsec

-- | A list of reserved keywords.
myReserves :: [String]
myReserves = ["if", "then", "else", "elif", "is", "curry"
             , "send", "request", "handler", "fun", "when" 
             , "where", "because", "Given", "and", "or", "not"
             , "True", "False", "case", "of", "inherits"]

-- | Keyword 'is'.
is_ :: BParser ()
is_ = rword "is"

-- | Keyword 'because'.
because_ :: BParser ()
because_ = rword "because"

-- | Keyword 'inherits'.
inherits_ :: BParser ()
inherits_ = rword "inherits"

-- | Keyword 'given'.
given_ :: BParser ()
given_ = rword "Given"

-- | Keyword 'if'.
if_ :: BParser ()
if_ = rword "if"

-- | Keyword 'elif'.
elif_ :: BParser ()
elif_ = rword "elif"

-- | Keyword 'then'.
then_ :: BParser ()
then_ = rword "then"

-- | Keyword 'else'.
else_ :: BParser ()
else_ = rword "else"

-- | Keyword 'when'.
when_ :: BParser ()
when_ = rword "when"

-- | Keyword 'where'.
where_ :: BParser ()
where_ = rword "where"

-- | Keyword 'data'.
data_ :: BParser ()
data_ = rword "data"

-- | Keyword 'case'.
case_ :: BParser ()
case_ = rword "case"

-- | Keyword 'type'.
type_ :: BParser ()
type_ = rword ".type"

-- | Keyword 'of'.
of_ :: BParser ()
of_ = rword "of"

-- | Builtin 'True'.
true_ :: BParser ()
true_ = rword "True"

-- | Builtin 'False'.
false_ :: BParser ()
false_ = rword "False"

-- | Operator '->'.
arrow_ :: BParser ()
arrow_ = void $ symbol "->"

-- | Operator '='
equals_ :: BParser ()
equals_ = void $ symbol "="

-- | Operator ','
comma_ :: BParser ()
comma_ = void $ symbol ","

-- | Operator '.'
dot_ :: BParser ()
dot_ = void $ symbol "."

-- | Operator '-'
minus_ :: BParser ()
minus_ = void $ symbol "-"

-- | Operator ':'
colon_ :: BParser ()
colon_ = void $ symbol ":"

-- | Keyword 'fun'
fun_ :: BParser ()
fun_ = rword "fun" 

-- | Keyword 'curry'.
curry_ :: BParser ()
curry_ = rword "curry" 

-- | Keyword 'send'.
send_ :: BParser ()
send_ = rword "send" 

-- | Keyword 'request'.
request_ :: BParser ()
request_ = rword "request" 

-- | Parses a userspace lowercase identifier.
lName :: BParser String
lName = identifier lowerChar

-- | Parses a userspace uppercase identifier.
uName :: BParser String
uName = identifier upperChar

-- | Parses a userspace identifier.
aName :: BParser String
aName = identifier letterChar

-- | Parses a reserved identifier.
rword :: String -> BParser ()
rword w = lexeme $ try $ string w *> notFollowedBy nameChars

-- | Parses a non-reserved string whos first character is parsed by firstChar.
identifier ::BParser Char ->  BParser String
identifier firstChar = lexeme (p >>= rwcheck)
    where p = (:) <$> firstChar <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ x ++ " is a reserved word"
                      else return x

-- | Matches a single legal character for a userspace identifier.
nameChars :: BParser Char
nameChars = alphaNumChar <|> char '_'
