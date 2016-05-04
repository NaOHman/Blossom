{-# LANGUAGE TupleSections #-}

module Interpretor.Evaluator where

import Models.Expressions
import Interpretor.Builtins
import Interpretor.Values
import Control.Monad
import qualified Types.Utils as T
import Control.Monad.State

type MEval a = StateT Bool IO a

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

letBinds :: Scope -> [Binding] -> MEval Scope
letBinds s bs = Scope <$> mapM bind' bs 
    where bind' (Expl (i,_,e)) = (i,) <$> eval s e
          bind' (Impl (i,e)) = (i,) <$> eval s e
