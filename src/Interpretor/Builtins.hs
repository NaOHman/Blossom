module Interpretor.Builtins (defScope) where

import Interpretor.Values

defScope :: Scope
defScope = Scope $ map (\(n,i,b) -> (i, BuiltIn i n b))
  [(1, "+UN", buplus)
  ,(1, "-UN", bneg)
  ,(2, "+",   bplus)
  ,(2, "-",   bminus)
  ,(2, "/",   bdiv)
  ,(2, "*",   bmult)
  ,(2, "//",  bquot)
  ,(2, "%",   bmod)
  ,(2, "<",   ble)
  ,(2, ">",   bge)
  ,(2, ">=",  bgeq)
  ,(2, "<=",  bleq)
  ,(2, "==",  beq)
  ,(2, "!seq",  bseq)
  ,(1, "print",  bprint)
  ]

builtinError :: Monad m => m a
builtinError = fail "Built in function failed, Something has gone horribly wrong"

bseq :: [Value] -> IO Value
bseq [_, b] = return b
bseq _ = builtinError

bprint :: [Value] -> IO Value
bprint [VInt a] = print a >> return VNull
bprint [VFloat a] = print a >> return VNull
bprint [VChar a] = print a >> return VNull
bprint [VBool a] = print a >> return VNull
bprint [v@(VCons "[cons]" [VChar _, _])] = do
        let cs = extract v
        putStrLn cs
        return VNull
        where extract (VCons "[cons]" [VChar c', v']) = c' : extract v'
              extract (VCons "[nil]" []) = []
              extract _ = error "Something has gone horribly wrong"
bprint [VCons n vs] = print n >> mapM (bprint . (:[])) vs >> return VNull
bprint _ = builtinError

buplus :: [Value] -> IO Value
buplus [a] = return a
buplus _ = builtinError

bneg :: [Value] -> IO Value
bneg [VFloat a] = return . VFloat $ negate a
bneg [VInt a] = return . VInt $ negate a
bneg _ = builtinError

bplus :: [Value] -> IO Value
bplus [VInt a, VInt b] = return . VInt $ a + b
bplus [VFloat a, VFloat b] = return . VFloat $ a + b
bplus _ = builtinError

bminus :: [Value] -> IO Value
bminus [VInt a, VInt b] = return . VInt $ a - b
bminus [VFloat a, VFloat b] = return . VFloat $ a - b
bminus _ = builtinError

bmult :: [Value] -> IO Value
bmult [VInt a, VInt b] = return . VInt  $ a * b
bmult [VFloat a, VFloat b] = return . VFloat $ a * b
bmult _ = builtinError

bdiv :: [Value] -> IO Value
bdiv [VFloat a, VFloat b] = return . VFloat $ a / b
bdiv _ = builtinError

bquot :: [Value] -> IO Value
bquot [VInt a, VInt b] = return . VInt  $ a `quot` b
bquot _ = builtinError

bmod :: [Value] -> IO Value
bmod [VInt a, VInt b] = return . VInt  $ a `mod` b
bmod _ = builtinError

bge :: [Value] -> IO Value
bge [VInt a, VInt b] = return . VBool  $ a > b
bge [VFloat a, VFloat b] = return . VBool $ a > b
bge [VChar a, VChar b] = return . VBool $ a > b
bge _ = builtinError

ble :: [Value] -> IO Value
ble [VInt a, VInt b] = return . VBool $ a < b
ble [VFloat a, VFloat b] = return . VBool $ a < b
ble [VChar a, VChar b] = return . VBool $ a < b
ble _ = builtinError

bgeq :: [Value] -> IO Value
bgeq [VInt a, VInt b] = return  . VBool $ a >= b
bgeq [VFloat a, VFloat b] = return . VBool $ a >= b
bgeq [VChar a, VChar b] = return . VBool $ a >= b
bgeq _ = builtinError

bleq :: [Value] -> IO Value
bleq [VInt a, VInt b] = return . VBool  $ a <= b
bleq [VFloat a, VFloat b] = return . VBool $ a <= b
bleq [VChar a, VChar b] = return . VBool $ a <=  b
bleq _ = builtinError

beq :: [Value] -> IO Value
beq [VInt a, VInt b] = return . VBool  $ a == b
beq [VFloat a, VFloat b] = return . VBool $ a == b
beq [VChar a, VChar b] = return . VBool $ a ==  b
beq _ = builtinError
