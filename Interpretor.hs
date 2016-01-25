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

data Scope = Scope {
    func  :: M.Map String (Args, Expr),
    local :: M.Map String Literal,
    vars  :: M.Map String Literal}

debug = False
printd :: String -> MEval ()
printd = when debug . lift . print

main = do 
    (file:mainArgs) <- getArgs
    prog <- parseFromFile program file
    case prog of
        Left err -> print err
        Right prg -> runProg (functions prg) (globals prg) mainArgs

runProg f g a = do
    let s = Scope (getFuns f M.empty) M.empty
                  (getGbls g M.empty)
    when debug $ print s
    case lookupFun "main" s of
        Just (args, e) -> do
            -- TODO pass in Commandline args
            let s' = if takesArgs args
                then bindMain args s
                else s
            void $ runEval e s
        Nothing -> putStr "Err: main not found"
    where getFuns (EFix n a e:es) m =
                getFuns es (M.insert n (a,e) m)
          getFuns [] m = m 
          getGbls (ELet (DName n) (ELit l):es) m = 
                getGbls es (M.insert n l m)
          getGbls [] m = m 
          takesArgs _ = False
          bindMain _ s = s

runEval e = evalStateT (eval e)

eval :: Expr -> MEval Literal
eval (ELit l) = do
    printd "Evaluating Literal"
    return l

eval (EVar i) = get >>= \s -> do
    printd $ "Evaluating Variable " ++ i
    maybe 
        (fail $ "Unknown Identifier " ++ i)
        return (lookupVar i s)
        
eval (ELet p e) = do
    v <- eval e
    case match p v of
        Just bindings -> mapM (uncurry bindVar) bindings
        otherwise     -> fail "Couldn't match pattern"
    return LNull

eval (ECase e cs) = do
    v <- eval e
    pickCase v cs
    where pickCase val [] = fail "Couldn't match pattern"
          pickCase v ((p,e):cs) = case match p v of
            Just bs -> evalWithBindings bs e
            otherwise -> pickCase v cs

eval (EChain e1 e2) = do 
    printd "Evaluating a Chain"
    eval e1 
    eval e2

eval (EFCall (Call n pa kwa)) = do
    printd $ "Evaluating Function " ++ n
    pVals  <- mapM eval pa
    kwVals <- mapM (\(k,e) -> liftM (k,) (eval e)) kwa
    s <- get
    case M.lookup n (func s) of 
        Just (args, ex) -> do
            printd $ "Found Function " ++ n
            scope <- bindArgs args pVals kwVals
            lift $ runEval ex scope
        Nothing -> case M.lookup n builtins of
            Just (args, fun) -> do
                scope <- bindArgs args pVals kwVals
                lift $ evalStateT fun scope
            Nothing -> fail $ "Err: Could not find function " ++ n

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
bindVar i v = get >>= \s -> case lookupVar i s of 
    Just _  ->  fail "Variable are immutable"
    Nothing -> do
        put $ s {local = M.insert i v (local s)}
        when debug (get >>= lift . print)


bindArgs :: Args -> [Literal] -> [(String, Literal)] -> MEval Scope
bindArgs a ps ks = get >>= \s -> lift (execStateT (ba a ps ks) (clear s))
    where clear s = s {local = M.empty}

ba :: Args -> [Literal] -> [(String, Literal)] -> MEval ()
ba (Args pargs kargs aa ka) ps ks = do
    let p = map fst pargs 
    let k = map (\(a,b,_) -> (a,b)) kargs
    (extraP, freeK) <- matchPArgs p k ps 
    extraK <- matchKArgs freeK ks
    aggregateP aa extraP
    aggregateK ka extraK

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

lookupVar i s = case M.lookup i (local s) of
    Nothing -> M.lookup i (vars s)
    v -> v

lookupFun i s = M.lookup i (func s)

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

args1 = Args [("a",None)] [] Nothing Nothing
args2 = Args [("b",None),("a",None)] [] Nothing Nothing

uminus = nar1 "Unary -" negate 
uplus = nar1 "Unary +" id 

print' :: (String, Args, MEval Literal)
print' = ("print",args1, get >>= \s -> do 
    case lookupVar "a" s of 
        Just (LInt a) -> lift $ print $ show a
        Just (LFloat a) -> lift $ print $ show a
        Just (LString a) -> lift $ print $ show a
        Just (LChar a) -> lift $ print $ show a
        Just LNull -> lift $ print "Null"
        otherwise -> fail "Unsupported print value"
    return LNull)

nar1 :: String -> (Double -> Double) -> (String,Args, MEval Literal)
nar1 n f = (n,args1, get >>= \s -> 
    case lookupVar "a" s of
        (Just (LInt a)) -> return $ LInt $ round $ f $ fromIntegral a
        (Just (LFloat a)) -> return $ LFloat $ f a
        otherwise -> fail $ "Must provide numerical argument for " ++ n)

nar2 :: String -> (Double -> Double -> Double) -> (String, Args, MEval Literal)
nar2 n f = (n, args2, get >>= \s -> do
    a <- numericLit n $ lookupVar "a" s 
    b <- numericLit n $ lookupVar "b" s 
    return $ nap2 f a b)

nbar2 :: String -> (Double -> Double -> Bool) -> (String, Args, MEval Literal)
nbar2 n f = (n, args2, get >>= \s -> do
    a <- numericLit n $ lookupVar "a" s 
    b <- numericLit n $ lookupVar "b" s 
    return $ b2c $ nbap2 f a b)

b2c True = LCons "True" []
b2c False = LCons "False" []

niar2 :: String -> (Double -> Double -> Integer) -> (String, Args, MEval Literal)
niar2 n f = (n, args2, get >>= \s -> do
    a <- numericLit n $ lookupVar "a" s 
    b <- numericLit n $ lookupVar "b" s 
    return $ LInt $ niap2 f a b)

bar2 :: String -> (Bool -> Bool -> Bool) -> (String, Args, MEval Literal)

bar2 n f = (n, args2, get >>= \s -> do
    a <- boolLit n $ lookupVar "a" s 
    b <- boolLit n $ lookupVar "b" s 
    return $ b2c $ f a b)

bar1 :: String -> (Bool -> Bool) -> (String, Args, MEval Literal)
bar1 n f = (n, args1, get >>= \s -> do
    a <- boolLit n $ lookupVar "a" s 
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
