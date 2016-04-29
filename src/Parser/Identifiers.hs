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

myReserves :: [String]
myReserves = ["if", "then", "else", "elif", "is", "curry"
             , "send", "request", "handler", "fun", "when" 
             , "where", "because", "Given", "and", "or", "not"
             , "True", "False", "case", "of", "inherits"]

is_ :: BParser ()
is_ = rword "is"

because_ :: BParser ()
because_ = rword "because"

inherits_ :: BParser ()
inherits_ = rword "inherits"

given_ :: BParser ()
given_ = rword "Given"

if_ :: BParser ()
if_ = rword "if"

elif_ :: BParser ()
elif_ = rword "elif"

then_ :: BParser ()
then_ = rword "then"

else_ :: BParser ()
else_ = rword "else"

when_ :: BParser ()
when_ = rword "when"

where_ :: BParser ()
where_ = rword "where"

data_ :: BParser ()
data_ = rword "data"

case_ :: BParser ()
case_ = rword "case"

type_ :: BParser ()
type_ = rword ".type"

of_ :: BParser ()
of_ = rword "of"

true_ :: BParser ()
true_ = rword "True"

false_ :: BParser ()
false_ = rword "False"

arrow_ :: BParser ()
arrow_ = void $ symbol "->"

equals_ :: BParser ()
equals_ = void $ symbol "="

comma_ :: BParser ()
comma_ = void $ symbol ","

dot_ :: BParser ()
dot_ = void $ symbol "."

colon_ :: BParser ()
colon_ = void $ symbol ":"

fun_ :: BParser ()
fun_ = rword "fun" 

curry_ :: BParser ()
curry_ = rword "curry" 

send_ :: BParser ()
send_ = rword "send" 

request_ :: BParser ()
request_ = rword "request" 

lName :: BParser String
lName = identifier lowerChar

uName :: BParser String
uName = identifier upperChar

aName :: BParser String
aName = identifier letterChar

rword :: String -> BParser ()
rword w = lexeme $ try $ string w *> notFollowedBy nameChars

identifier ::BParser Char ->  BParser String
identifier = lexeme . rstring

rstring :: BParser Char -> BParser String
rstring fcp = lexeme (p >>= rwcheck)
    where p = (:) <$> fcp <*> many nameChars
          rwcheck x = if x `elem` myReserves
                      then fail $ x ++ " is a reserved word"
                      else return x

nameChars :: BParser Char
nameChars = alphaNumChar <|> char '_'
