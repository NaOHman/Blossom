{-# LANGUAGE GADTs, TupleSections #-}

module Interpretor.Interpretor where

import Models.Expressions
import Models.Program
import qualified Data.Map as M
import Control.Monad
import GHC.Exts
import Control.Monad.State
import Data.List (intercalate)

type MEval a = StateT EvalSt IO a

type EvalSt = (Scope, Bool)

printd :: String -> MEval ()
printd =  whenDebug . lift . print

getScope :: MEval Scope
getScope = liftM fst get

whenDebug :: MEval () -> MEval ()
whenDebug m = get >>= \(_,dbg) -> when dbg m

runEval e = evalStateT (eval e)

eval :: Expr -> MEval Value
eval (Expr _ (ELit l)) = do
    printd "Evaluating Literal"
    return $ lit2Val l

eval (Expr pos (EVar i)) = getScope >>= \s -> do
    printd $ "Evaluating Variable " ++ intercalate "." i
    case i of 
        [] -> fail "Something has gone horribly wrong"
        (v:vs) -> case lookupInScope v s of
            Nothing -> fail $ "Unknown Identifier " ++ v ++ " at " ++ show pos
            Just val -> subVal val vs
        
eval (Expr pos (ELet p e)) = do
    v <- eval e
    case match p v of
        Just bindings -> mapM (uncurry bindVar) bindings
        otherwise     -> fail  $ "Couldn't match pattern " ++ show p ++ " with value " ++ show v ++ " at " ++ show pos
    return VNull

eval (Expr _ (ECase e cs)) = do
    v <- eval e
    pickCase v cs
    where pickCase val [] = fail "Couldn't match pattern" --TODO Return a fail value
          pickCase v ((p,e):cs) = case match p v of
            Just bs -> evalWithBindings bs e
            otherwise -> pickCase v cs

eval (Expr _ (EChain e1 e2)) = do 
    printd "Evaluating a Chain"
    eval e1 
    eval e2

eval (Expr _ (EFCall n args)) = do
    printd $ "Evaluating Function " ++ n
    (pVals,kwVals)  <- evalArgs args
    s <- getScope
    case getFunction n s of 
        Just (args,ex) -> do
            printd $ "Found Function " ++ n
            st <- bindArgs args pVals kwVals
            lift $ runEval ex st
        Nothing -> case getBuiltin n of
            Just (args, fun) -> do
                st <- bindArgs args pVals kwVals
                lift $ evalStateT fun st
            Nothing -> fail $ "Err: Could not find function " ++ n

subVal v [] = return v
subVal (VCons c svs) (v:vs) = do
    idx <- indexOfVal c v
    subVal (svs !! idx) vs
    where indexOfVal cons name = do
            (Scope _ _ cs) <- getScope
            case M.lookup cons cs of
                Just valmap -> case M.lookup name valmap of
                    Just i -> return i 
                    Nothing -> fail $ name  ++ " is an invalid field for constructor " ++ cons
                Nothing -> fail "Something has gone horribly wrong"


getFunction :: String -> Scope -> Maybe ([ArgDec], Expr)
getFunction n (Scope gbl lcl _) = case M.lookup n lcl of
    Just (VLambda a ex) -> Just (a,ex)
    otherwise -> case M.lookup n gbl of
        Just (VLambda a ex) -> Just (a,ex)
        otherwise -> Nothing

getBuiltin :: String -> Maybe ([ArgDec], MEval Value)
getBuiltin s = M.lookup s builtins

evalArgs :: [Arg] -> MEval ([Value],[(Id,Value)])
evalArgs = foldM f ([],[])
    where
      f (a,b) (Arg _ (PosArg e)) = eval e >>= \v -> 
                return (v:a,b)
      f (a,b) (Arg _ (KWArg s e)) = eval e >>= \v -> 
                return (a,(s,v):b)

evalWithBindings bs e = get >>= \s -> do
    mapM_ (uncurry bindVar) bs
    v <- eval e
    put s 
    return v
--Make sure type Checker validates patterns

match :: Pat -> Value -> Maybe [(Id, Value)]
match DNil _ = Just []
match (DName n) v = Just [(n,v)]
match (DMatch l) v 
  | lit2Val l == v = Just []
  | otherwise  = Nothing
match (DCons i ps) (VCons n ls)
  | i == n = foldMatch ps ls 
  | otherwise = Nothing
match _ _ = Nothing

foldMatch :: [Pat] -> [Value] -> Maybe [(Id, Value)]
foldMatch [] [] = Just []
foldMatch (p:ps) (v:vs) = 
    (++) <$> match p v <*> foldMatch ps vs
foldMatch _ _ = Nothing


bindVar :: String -> Value -> MEval ()
bindVar i v = getScope >>= \s -> case lookupInScope i s of 
    Just _  ->  fail "Variable are immutable"
    Nothing -> do
        putScope $ s {local = M.insert i v (local s)}
        whenDebug (getScope >>= lift . print)

putScope :: Scope -> MEval ()
putScope s = modify (\(_,d) -> (s,d))


bindArgs :: [ArgDec] -> [Value] -> [(String, Value)] -> MEval EvalSt
bindArgs as ps ks = get >>= \st -> lift (execStateT (ba as ps ks) (clear st))
    where clear (sc,db) = (sc {local = M.empty}, db)

--Will fail when passed malformed arguments, validate with preprocessor
ba :: [ArgDec] -> [Value] -> [(String, Value)] -> MEval ()
ba as ps ks = do
    let (p,k,aa,ka) = sortArgs as
    (extraP, freeK) <- matchPArgs p k ps 
    extraK <- matchKArgs freeK ks
    aggregateP aa extraP
    aggregateK ka extraK
    where sortArgs = foldl f ([],[],Nothing, Nothing)
          f (a,b,c,d) (ArgDec _ (PosDec n _)) = (n:a,b,c,d)
          f (a,b,c,d) (ArgDec _ (KWDec n v _)) = (a,(n,lit2Val v):b,c,d)
          f (a,b,Nothing,d) (ArgDec _ (PSDec n _)) = (a,b,Just n,d)
          f (a,b,c,Nothing) (ArgDec _ (KWSDec n _)) = (a,b,c,Just n)

matchKArgs ks vs = aggK [] (sortKW ks) (sortKW vs)
    where 
        sortKW = sortWith fst
        aggK d k'@((k,l):ks) a'@((a,v):as)
            | k == a = bindVar a v >> aggK d ks as
            | k < a  = bindVar k l >> aggK d ks a'
            | k > a  = aggK ((a,v):d) k' as
        aggK d ((k,l):ks) _ = bindVar k l >> aggK d ks []
        aggK d [] a = return (a ++ d)
    
matchPArgs (p:ps) k (v:vs) = 
    bindVar p v >> matchPArgs ps k vs
matchPArgs _ ((k,_):ks) (v:vs) =
    bindVar k v >> matchPArgs [] ks vs
matchPArgs (_:_) _ [] = fail "Too few Positional Args"
matchPArgs [] ks vs = return (vs,ks)

aggregateP (Just a) x = bindVar a (a2ValCons  x)
aggregateP Nothing [] = return ()
aggregateP Nothing _  = fail "Too many positional args"

a2ValCons []     = VCons "[nil]" []
a2ValCons (v:vs) = VCons "[cons]" [v, a2ValCons vs]

aggregateK (Just a) x = bindVar a (toLDict x)
    where toLDict = a2ValCons . map f
          f (s,v) = VCons "(,,)"  [str2val s, v]
aggregateK Nothing [] = return ()
aggregateK Nothing _  = fail "Too many keyword args"

str2val []     = VCons "[]" []
str2val (c:cs) = VCons "[cons]" [VChar c, str2val cs]

lookupInScope i s = case M.lookup i (local s) of
    Nothing -> M.lookup i (global s)
    v -> v

-- Assumes Literal is only composed of other Literals
-- This assumption should be verified by the preprocessor
lit2Val :: Literal -> Value
lit2Val (Literal _ (LInt i)) = VInt i
lit2Val (Literal _ (LFloat f)) = VFloat f
lit2Val (Literal _ (LChar c)) = VChar c
lit2Val (Literal _ (LCons c ls)) = VCons c (f ls)
    where f [] = []
          f (Expr _ (ELit l):ls) = lit2Val l : f ls
lit2Val (Literal _ LNull) = VNull


builtins = M.fromList
   [ nar1 "+UN" id
   , nar1 "-UN" negate
   , nar2 "+"   (+)
   , nar2 "-"   (-)
   , nar2 "/"   (/)
   , niar2 "//" (\a b -> floor (a/b))
   , nar2 "%"   (\a b -> a - (a / fromIntegral (round b)))
   , nar2 "*"   (*)
   , nbar2 "<"  (<)
   , nbar2 "<=" (<=)
   , nbar2 ">"  (>)
   , nbar2 ">=" (>=)
   , nbar2 "==" (>=)
   , bar1 "not" not
   , bar2 "and" (&&)
   , bar2 "or"  (||)
   , bar2 "xor" (\a b -> (a || b) && not (a && b))
   , print'
   ]

args1 = [ArgDec nulPos (PosDec "a" None)] 
args2 = [ArgDec nulPos (PosDec "b" None), 
         ArgDec nulPos (PosDec "a" None)] 

nulPos = undefined

uminus = nar1 "Unary -" negate 
uplus = nar1 "Unary +" id 

print' :: (String, ([ArgDec], MEval Value))
print' = ("print",(args1, getScope >>= \s -> do 
    case lookupInScope "a" s of 
        Just v -> lift $ print v
        otherwise -> fail "Unsupported print value"
    return VNull))

nar1 :: String -> (Double -> Double) -> (String, ([ArgDec], MEval Value))
nar1 n f = (n,(args1, getScope >>= \s -> 
    case lookupInScope "a" s of
        Just (VInt a) -> return $ VInt $ round $ f $ fromIntegral a
        Just (VFloat a) -> return $ VFloat $ f a
        otherwise -> fail $ "Must provide numerical argument for " ++ n))

nar2 :: String -> (Double -> Double -> Double) -> (String, ([ArgDec], MEval Value))
nar2 n f = (n, (args2, getScope >>= \s -> do
    a <- numericLit n $ lookupInScope "a" s 
    b <- numericLit n $ lookupInScope "b" s 
    return $ nap2 f a b))

nbar2 :: String -> (Double -> Double -> Bool) -> (String, ([ArgDec], MEval Value))
nbar2 n f = (n, (args2, getScope >>= \s -> do
    a <- numericLit n $ lookupInScope "a" s 
    b <- numericLit n $ lookupInScope "b" s 
    return $ b2c $ nbap2 f a b))

b2c True = VCons "True" []
b2c False = VCons "False" []

niar2 :: String -> (Double -> Double -> Integer) -> (String, ([ArgDec], MEval Value))
niar2 n f = (n, (args2, getScope >>= \s -> do
    a <- numericLit n $ lookupInScope "a" s 
    b <- numericLit n $ lookupInScope "b" s 
    return $ VInt $ niap2 f a b))

bar2 :: String -> (Bool -> Bool -> Bool) -> (String, ([ArgDec], MEval Value))
bar2 n f = (n, (args2, getScope >>= \s -> do
    a <- boolLit n $ lookupInScope "a" s 
    b <- boolLit n $ lookupInScope "b" s 
    return $ b2c $ f a b))

bar1 :: String -> (Bool -> Bool) -> (String, ([ArgDec], MEval Value))
bar1 n f = (n, (args1, getScope >>= \s -> do
    a <- boolLit n $ lookupInScope "a" s 
    return $ b2c $ f a))

nap2 :: (Double -> Double -> Double) -> Value -> Value -> Value
nap2 f (VInt a)   (VInt b) = VInt $ round $ f (fromIntegral a) (fromIntegral b)
nap2 f (VInt a)   (VFloat b) = VFloat $ f (fromIntegral a) b
nap2 f (VFloat a) (VInt b) = VFloat $ f a (fromIntegral b)
nap2 f (VFloat a) (VFloat b) = VFloat $ f a b

nbap2 :: (Double -> Double -> Bool) -> Value -> Value -> Bool
nbap2 f (VInt a)   (VInt b) = f (fromIntegral a) (fromIntegral b)
nbap2 f (VInt a)   (VFloat b) = f (fromIntegral a) b
nbap2 f (VFloat a) (VInt b) = f a (fromIntegral b)
nbap2 f (VFloat a) (VFloat b) = f a b

niap2 :: (Double -> Double -> Integer) -> Value -> Value -> Integer
niap2 f (VInt a)   (VInt b) = f (fromIntegral a) (fromIntegral b)
niap2 f (VInt a)   (VFloat b) = f (fromIntegral a) b
niap2 f (VFloat a) (VInt b) = f a (fromIntegral b)
niap2 f (VFloat a) (VFloat b) = f a b

numericLit s (Just a@(VInt _)) = return a
numericLit s (Just a@(VFloat _)) = return a
numericLit s _ = fail $ "Must pass numeric argument to " ++ s

boolLit s (Just (VCons "True" [])) = return True
boolLit s (Just (VCons "False" [])) = return False
boolLit s _ = fail $ "Must Pass numeric arguments to " ++ s

instance Show Scope where
    show (Scope gbl lcl _) = "Global bindings: {" ++ showk gbl ++ "}\n\tLocal bindings: {" ++ showk lcl ++"}"
        where showk = concatMap show . M.keys

instance Eq Value where
    (==) (VInt x1) (VInt x2) = x1 == x2
    (==) (VFloat x1) (VFloat x2) = x1 == x2
    (==) (VChar x1) (VChar x2) = x1 == x2
    (==) (VCons x1 []) (VCons x2 []) = x1 == x2
    (==) (VCons x1 p1) (VCons x2 p2) = x1 == x2 && eq' p1 p2
        where eq' [] [] = True
              eq' (x:xs) (y:ys) = (x == y) && eq' xs ys
              eq' _ _ = False
    (==) VNull VNull = True
    (==) _ _ = False


instance Show Value where
    show (VFloat d) = show d
    show (VInt d) = show d
    show (VChar d) = show d
    show (VLambda as e) = "(" ++ intercalate "," (map show as) ++ ") -> " ++ show e
    show (VCons "[cons]" [VChar c, cs]) = c : ar2Str cs
        where ar2Str (VCons "[nil]" _) = []
              ar2Str (VCons "[cons]" [VChar c, cs]) = c : ar2Str cs
    show (VCons "[cons]" [c,cs]) = 
        "[" ++ show c ++ showAr cs ++ "]"
        where showAr (VCons "[nil]" _) = ""
              showAr (VCons "[cons]" [VChar c, cs]) = ',': show c ++ showAr cs
    show (VCons "[nil]" _) = "[]"
    show (VCons c vs) = c ++ "(" ++ intercalate "," (map show vs) ++ ")"
