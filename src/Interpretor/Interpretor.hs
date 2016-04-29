{-# LANGUAGE GADTs, TupleSections #-}

module Interpretor.Interpretor where

import Models.Expressions
import Control.Monad
import qualified Types.Utils as T
import Control.Monad.State
import Data.List (intercalate, unionBy)
import Data.Char (isUpper)

type MEval a = StateT Bool IO a

data Value = VInt Integer
           | VFloat Double
           | VBool Bool
           | VChar Char
           | VNull
           | VOver Expr
           | VCons String [Value]
           | VLambda [Pat] Expr
           | BuiltIn Id Int ([Value] -> IO Value)

instance Show Value where
   show (VInt i) = show i 
   show (VFloat i) = show i 
   show (VBool i) = show i 
   show (VChar i) = show i 
   show (VCons i vs) = i ++ "(" ++ intercalate ","  (map show vs) ++ ")"
   show (VOver ex) = "Over" ++ show ex
   show (BuiltIn n i _) = show n ++ show i
   show (VLambda ps e) = show ps ++ "(" ++ intercalate ","  (map show ps) ++ ")" ++ show e
   show VNull = "Null"

data Scope = Scope [(Id, Value)]
    deriving Show

add :: Scope -> Scope -> Scope
add (Scope b1) (Scope b2) = Scope (unionBy f b1  b2)
    where f (i,_) (i2,_) = i == i2

interpretBlossom ::  [(Id, Expr)] -> [String] -> Bool -> IO ()
interpretBlossom bs args = evalStateT (runProg bs args)

runProg :: [Impl] -> [String] -> MEval ()
runProg bs _ =  do 
    vs <- mapM (\(i,ex) -> eval' i defScope ex) bs
    let sc = Scope $ zip (map fst bs) vs
        (Just (VLambda _ mn)) = lookupScope "main" sc
    void $ eval (sc `add` defScope) mn
    where eval' _ _ e@Over{} = return $ VOver e
          eval' ('$':_) _ (Abs (ps,e)) = return $ VLambda ps e
          eval' _ s ex = eval s ex

lookupScope :: Id -> Scope -> Maybe Value
lookupScope ('#':i) _ = Just $ BuiltIn "Access" 1 (\[VCons _ vs] -> return (vs !! read i))
lookupScope i@(c:_) (Scope sc) 
    | isUpper c || c == '[' || c == '(' = Just $ VCons i []
    | otherwise = lookup i sc
lookupScope _ _ = fail "Tried to look up empty string"


letBinds :: Scope -> [Binding] -> MEval Scope
letBinds s bs = Scope <$> mapM bind' bs 
    where bind' (Expl (i,_,e)) = eval s e >>= \v -> return (i,v)
          bind' (Impl (i,e)) = eval s e >>= \v -> return (i,v)

eval :: Scope -> Expr -> MEval Value
eval _ (Lit l) = do
    dprint "Eval lit"
    return $ lit2Val l

eval s (Var i) = do
    dprint $ "look up " ++ i
    case lookupScope i s of 
        Nothing -> fail $ "Variable " ++ i ++ " is ubound"
        Just v -> return v
        
eval _ (Abs (p,e)) =  do
    dprint $ "Eval lambda " ++ show p ++ " -> " ++ show e
    return (VLambda p e)

eval s (Let bg e) = do
    dprint $ "Eval let " ++ show bg ++ " in " ++ show e
    bs <- letBinds s bg
    eval (bs `add` s) e

eval s (Case e cs) = do
    dprint $ "Eval case " ++ show e ++ " of " ++ show cs
    v <- eval s e
    dprint  "!!!!!!!!!!!!!!!!"
    dprint $ "Case evaluated to " ++ show v
    dprint  "!!!!!!!!!!!!!!!!"
    pickCase v cs
    where pickCase v (([p],e'):cs') = do
              dprint (show p)
              dprint (show v)
              case match p v of
                Just bs -> eval (bs `add` s) e'
                _ -> pickCase v cs'
          pickCase _ _ = fail "Couldn't match pattern" --TODO Return a fail value

eval s (Annot e _) = eval s e

eval sco o@(Over i tv' ps) = do
    ex <- findMatch tv' ps
    eval sco ex
    where findMatch t ((sc,_):es) = 
             let (_ :=> t') = freshInst sc
             in case T.match t' t of
                Nothing -> findMatch t es
                Just _ -> return $ Var $ "$" ++ i ++ "#" ++ show sc 
          findMatch _ _ = fail $ "Over failed to disambiguate" ++ show o
          freshInst (Forall ks qt) = 
            let ts = zipWith (\k n -> TVar $ Tyvar (show (n :: Int)) k) ks [0..]
            in inst ts qt


eval s (Ap e1 e2) = do
    dprint $ "begin ap eval " ++ show e1 ++ " `ap` " ++ show e2
    evalAp s e1 [e2] 

evalAp :: Scope -> Expr -> [Expr] -> MEval Value
evalAp s (Ap e1 e2) es = evalAp s e1 (e2:es)

evalAp s f es  = do
    dprint $ "eval " ++ show f ++ " `ap` " ++ show es
    args <- mapM (eval s) es
    fn <- eval s f
    case fn of
        (VLambda [] e) -> eval s e
        (VLambda ps e) -> do 
            unless (length ps == length args)
                    $ fail "function not fully applied"
            let (Just sc) = matches ps args
                sc' = sc `add` s 
            dprint "_____________________"
            dprint $ show sc'
            dprint "_____________________"
            eval sc' e
        (BuiltIn n i fnc) -> if length args == i 
            then liftIO (fnc args)
            else fail $ "Builtin: " ++ n ++ " applied to wrong number of arguments " ++ show es
        (VCons n _) -> return $ VCons n args
        a -> fail $ "Fail something went horribly wrong " ++ show a

dprint :: String -> MEval()
dprint msg = do debug <- get
                when debug $ liftIO (putStrLn msg)

catScope :: [Scope] -> Scope
catScope = Scope . concatMap (\(Scope s) -> s) 

bind :: Id -> Value -> Scope -> Scope
bind i v (Scope s) = Scope $ (i,v):s

match :: Pat -> Value -> Maybe Scope
match PNil _ = Just $ Scope []
match (PLit l) v 
  | lit2Val l `veq` v = Just $ Scope []
  | otherwise = Nothing
match (PVar n) v = Just $ Scope [(n,v)]
match (PCons n1 ps) (VCons n2 vs) 
  | n1 == n2 = matches ps vs
  | otherwise  = Nothing
match (PAs i p) v = bind i v <$> match p v
match _ _ = Nothing

matches :: [Pat] -> [Value] -> Maybe Scope
matches ps vs = catScope <$> zipWithM match ps vs

lit2Val :: Literal -> Value
lit2Val (LInt i) = VInt i
lit2Val (LFloat f) = VFloat f
lit2Val (LChar c) = VChar c
lit2Val (LBool b) = VBool b
lit2Val LNull = VNull

veq :: Value -> Value -> Bool
veq (VInt i) (VInt i') = i == i'
veq (VFloat f) (VFloat f') = f == f'
veq (VChar c) (VChar c') = c == c'
veq (VBool b) (VBool b') = b == b'
veq VNull VNull = True
veq _ _ = False

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
