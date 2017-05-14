module Parser.IR.Pretty
    ( Pretty(..)
    , pretty
    , csl
    , parenCsl
    , block
    , newBlock
    , indent
    ) where

import Data.List (intercalate)

class Pretty a where
    pp :: Int -> a -> String

pretty :: Pretty a => a -> String
pretty = pp 0

indent :: Int -> String
indent i = '\n' : replicate i ' '

newBlock :: Pretty a => Int -> [a] -> String
newBlock i es = indent i ++ block i es

block :: Pretty a => Int -> [a] -> String
block i = intercalate (indent i) . map (pp i) 

csl :: Pretty a => Int -> [a] -> String
csl i = intercalate ", " . map (pp i) 

parenCsl :: Pretty a => Int -> [a] -> String
parenCsl i ps =  "(" ++ csl i ps ++ ")"
