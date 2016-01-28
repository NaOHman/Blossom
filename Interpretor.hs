{-# LANGUAGE GADTs, TupleSections #-}

import Exprs
import Constraints
import Models
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans
import GHC.Exts
import Text.Megaparsec
import System.Environment
import Control.Monad.State

type MEval a = StateT Scope IO a

data Value = VInt Int
           | VFloat Double
           | VChar Char
           | VLambda [ArgDec] Expr'
           | VCons String [Value]

data Scope = Scope
    { global :: M.Map String Value
    , local :: M.Map String Value
    } deriving Show
    

debug = False

printd :: String -> MEval ()
printd = when debug . lift . print

runEval e = evalStateT (eval e)

eval :: Expr -> MEval Value
eval (Expr _ (ELit l)) = do
    printd "Evaluating Literal"
    return $ lit2Val l

eval (Expr pos (EVar i)) = get >>= \s -> do
    printd $ "Evaluating Variable " ++ i
    maybe 
        (fail $ "Unknown Identifier " ++ i ++ " at " ++ show pos)
        return (lookupInScope i s)
        
eval (Expr pos (ELet p e)) = do
    v <- eval e
    case match p v of
        Just bindings -> mapM (uncurry bindVar) bindings
        otherwise     -> fail  $ "Couldn't match pattern " ++ show p ++ " with value " ++ show v ++ " at " ++ show pos
    return LNull

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
    (pVals,kwVals)  <- mapM evalArgs args
    s <- get
    case lookupInScope n s of 
        Just (VLambda args ex) -> do
            printd $ "Found Function " ++ n
            scope <- bindArgs args pVals kwVals
            lift $ runEval ex scope
        Nothing -> case M.lookup n builtins of
            Just (VLambda args fun) -> do
                scope <- bindArgs args pVals kwVals
                lift $ evalStateT fun scope
            Nothing -> fail $ "Err: Could not find function " ++ n
    where evalArgs = foldM f ([],[])
          f (Arg _ (PosArg e)) (a,b) = eval e >>= \v -> (v:a,b)
          f (Arg _ (KWArg s e)) (a,b) = eval e >>= \v -> (a,(s,v):b)

evalWithBindings bs e = get >>= \s -> do
    mapM_ (uncurry bindVar) bs
    v <- eval e
    put s 
    return v
--Make sure type Checker validates patterns


match DNil _ = Just []
match (DName n) v = Just [(n,v)]
match (DMatch l) v 
    | match' l v = Just []
    | otherwise  = Nothing
match (DCons i ps) (LCons n ls)
    | i == n = foldMatch ps ls 
    | otherwise = Nothing
match _ _ = Nothing

foldMatch [] [] = Just []
foldMatch (p:ps) (ELit l:ls) = 
    (++) <$> match p l <*> foldMatch ps ls
foldMatch _ _ = Nothing

match' (LInt x1) (LInt x2) = x1 == x2
match' (LFloat x1) (LFloat x2) = x1 == x2
match' (LChar x1) (LChar x2) = x1 == x2
match' (LString x1) (LString x2) = x1 == x2
match' (LCons x1 []) (LCons x2 []) = x1 == x2
match' LNull LNull = True
match' _ _ = False


bindVar :: String -> Literal -> MEval ()
bindVar i v = get >>= \s -> case lookupInScope i s of 
    Just _  ->  fail "Variable are immutable"
    Nothing -> do
        put $ s {local = M.insert i v (local s)}
        when debug (get >>= lift . print)


bindArgs :: [ArgDec] -> [Literal] -> [(String, Literal)] -> MEval Scope
bindArgs a ps ks = get >>= \s -> lift (execStateT (ba a ps ks) (clear s))
    where clear s = s {local = M.empty}

ba :: [ArgDec] -> [Literal] -> [(String, Literal)] -> MEval ()
ba as ps ks = do
    let (p,k,aa,ka) = sortArgs as
    (extraP, freeK) <- matchPArgs p k ps 
    extraK <- matchKArgs freeK ks
    aggregateP aa extraP
    aggregateK ka extraK
    where sortArgs = foldl ([],[],"","")
          f (a,b,c,d) (ArgDec _ (PosDec n _)) = (n:a,b,c,d)
          f (a,b,c,d) (ArgDec _ (KWDec n v _)) = (a,(n,v):b,c,d)
          f (a,b,c,d) (ArgDec _ (PSDec n _)) = (a,b,n,d)
          f (a,b,c,d) (ArgDec _ (KWSDec n _)) = (a,b,c,n)

matchKArgs ks vs = aggK [] (mySort ks) (mySort vs)
    where 
        mySort = sortWith fst
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

aggregateP (Just (a,_)) x = bindVar a (LArray (map ELit x))
aggregateP Nothing [] = return ()
aggregateP Nothing _  = fail "Too many positional args"

aggregateK (Just (a,_)) x = bindVar a (toLDict x)
    where toLDict = LDict . map f
          f (a,b) = (ELit (LString a), ELit b)
aggregateK Nothing [] = return ()
aggregateK Nothing _  = fail "Too many keyword args"

lookupInScope i s = case M.lookup i (local s) of
    Nothing -> M.lookup i (global s)
    v -> v

lit2Val :: Literal -> Val
lit2Val (Literal _ (LInt i)) = VInt i
lit2Val (Literal _ (LFloat f)) = VInt f
lit2Val (Literal _ (LChar c)) = VInt c
lit2Val (Literal _ (LCons c ls)) = VCons c (map lit2Val ls)
lit2Val (Literal _ LNull) = VNull


builtins = foldl f M.empty
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
   where f m (n,a,e) = M.insert n (a,e) m

args1 = [ArgDec nulPos (PosDec "a" None)] 
args2 = [ArgDec nulPos (PosDec "b" None), 
         ArgDec nulPos (PosDec "a" None)] 

uminus = nar1 "Unary -" negate 
uplus = nar1 "Unary +" id 

print' :: (String, [ArgDec], MEval Val)
print' = ("print",args1, get >>= \s -> do 
    case lookupInScope "a" s of 
        Just (VInt a) -> lift $ print a
        Just (VFloat a) -> lift $ print a
        Just (VCons c vs) -> lift $ print $ showCons c vs
        Just (VChar a) -> lift $ print a
        Just VNull -> lift $ print "Null"
        otherwise -> fail "Unsupported print value"
    return $ Literal nulPos LNull)
    where showCons "[cons]" vs@(VChar _:_) = concatMap (\VChar c -> c) vs
          showCons c vs = c ++ " " concatMap showCons vs

nar1 :: String -> (Double -> Double) -> (String, [ArgDec], MEval Literal)
nar1 n f = (n,args1, get >>= \s -> 
    case lookupInScope "a" s of
        Just (Literal _ (LInt a)) -> return $ Literal nulPos $ LInt $ round $ f $ fromIntegral a
        Just (Literal _ (LFloat a)) -> return $ Literal nulPos $ LFloat $ f a
        otherwise -> fail $ "Must provide numerical argument for " ++ n)

nar2 :: String -> (Double -> Double -> Double) -> (String, [ArgDec], MEval Literal)
nar2 n f = (n, args2, get >>= \s -> do
    a <- numericLit n $ lookupInScope "a" s 
    b <- numericLit n $ lookupInScope "b" s 
    return $ nap2 f a b)

nbar2 :: String -> (Double -> Double -> Bool) -> (String, [ArgDec], MEval Literal)
nbar2 n f = (n, args2, get >>= \s -> do
    a <- numericLit n $ lookupInScope "a" s 
    b <- numericLit n $ lookupInScope "b" s 
    return $ b2c $ nbap2 f a b)

b2c True = LCons "True" []
b2c False = LCons "False" []

niar2 :: String -> (Double -> Double -> Integer) -> (String, [ArgDec], MEval Literal)
niar2 n f = (n, args2, get >>= \s -> do
    a <- numericLit n $ lookupInScope "a" s 
    b <- numericLit n $ lookupInScope "b" s 
    return $ LInt $ niap2 f a b)

bar2 :: String -> (Bool -> Bool -> Bool) -> (String, [ArgDec], MEval Literal)

bar2 n f = (n, args2, get >>= \s -> do
    a <- boolLit n $ lookupInScope "a" s 
    b <- boolLit n $ lookupInScope "b" s 
    return $ b2c $ f a b)

bar1 :: String -> (Bool -> Bool) -> (String, [ArgDec], MEval Literal)
bar1 n f = (n, args1, get >>= \s -> do
    a <- boolLit n $ lookupInScope "a" s 
    return $ b2c $ f a)

nap2 :: (Double -> Double -> Double) -> Literal -> Literal -> Literal
nap2 f (LInt a)   (LInt b) = LInt $ round $ f (fromIntegral a) (fromIntegral b)
nap2 f (LInt a)   (LFloat b) = LFloat $ f (fromIntegral a) b
nap2 f (LFloat a) (LInt b) = LFloat $ f a (fromIntegral b)
nap2 f (LFloat a) (LFloat b) = LFloat $ f a b

nbap2 :: (Double -> Double -> Bool) -> Literal -> Literal -> Bool
nbap2 f (LInt a)   (LInt b) = f (fromIntegral a) (fromIntegral b)
nbap2 f (LInt a)   (LFloat b) = f (fromIntegral a) b
nbap2 f (LFloat a) (LInt b) = f a (fromIntegral b)
nbap2 f (LFloat a) (LFloat b) = f a b

niap2 :: (Double -> Double -> Integer) -> Literal -> Literal -> Integer
niap2 f (LInt a)   (LInt b) = f (fromIntegral a) (fromIntegral b)
niap2 f (LInt a)   (LFloat b) = f (fromIntegral a) b
niap2 f (LFloat a) (LInt b) = f a (fromIntegral b)
niap2 f (LFloat a) (LFloat b) = f a b

numericLit s (Just a@(LInt _)) = return a
numericLit s (Just a@(LFloat _)) = return a
numericLit s _ = fail $ "Must pass numeric argument to " ++ s

boolLit s (Just (LCons "True" [])) = return True
boolLit s (Just (LCons "False" [])) = return False
boolLit s _ = fail $ "Must Pass numeric arguments to " ++ s

instance Show Scope where
    show (Scope fn lcl var) = "Scope Functions: {" ++ showf ++ "}\n\tlocalVars: {" ++ show lcl ++"}\n\tGlobalVars: {" ++ show var ++ "}"
        where showf = show $ M.keys fn