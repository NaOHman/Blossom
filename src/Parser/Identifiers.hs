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
   , data_, case_, type_, of_, arrow_, equals_, comma_, dot_, colon_, fun_
   , true_, false_, curry_, send_, request_
   , lName, aName, uName, rword
   ) where

import Control.Monad (void)
import Parser.Lexeme
import Text.Megaparsec
import Text.Megaparsec.String

myReserves = ["if", "then", "else", "elif", "is", "curry"
             , "send", "request", "handler", "fun", "when" 
             , "where", "because", "Given", "and", "or", "not"
             , "True", "False", "case", "of", "inherits"]

is_ = rword "is"
because_ = rword "because"
inherits_ = rword "inherits"
given_ = rword "Given"
if_ = rword "if"
elif_ = rword "elif"
then_ = rword "then"
else_ = rword "else"
when_ = rword "when"
where_ = rword "where"
data_ = rword "data"
case_ = rword "case"
type_ = rword ".type"
of_ = rword "of"
true_ = rword "True"
false_ = rword "False"
arrow_ = symbol "->"
equals_ = symbol "="
comma_ = symbol ","
dot_ = void $ symbol "."
colon_ = symbol ":"
fun_ = rword "fun" 
curry_ = rword "curry" 
send_ = rword "send" 
request_ = rword "request" 

lName = identifier lowerChar
uName = identifier upperChar
aName = identifier letterChar

rword w = lexeme $ try $ string w *> notFollowedBy nameChars

identifier = lexeme . rstring

rstring fcp = lexeme (p >>= rwcheck)
    where p = (:) <$> fcp <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ x ++ " is a reserved word"
                      else return x

nameChars = alphaNumChar <|> char '_'
