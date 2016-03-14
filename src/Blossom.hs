{-# LANGUAGE GADTs, TupleSections #-}

import Parser.Parser
import Types.Inference
import Types.Utils
import Models.Program
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans
import GHC.Exts
import Text.Megaparsec
import System.Environment
import Control.Monad.State

main = do 
    (dbg, file, mainArgs) <- parseArgs <$> getArgs
    parsed <- parseFromFile (evalStateT parseBlossom 0) file
    case parsed of
        Left err -> print err
        Right p@(Program as bs m d ce) -> do
           putStrLn "Hello"
           {-print bs-}
           mapM_ print (as ++ defaultAssumps)
           let as' = tiProgram ce (as ++ defaultAssumps) bs
           print as
           putStrLn "Success"
           {-case preprocess s of-}
               {-Left err -> print err-}
               {-Right (ws,scp) -> do-}
                    {-mapM_ print ws-}
                    {-runProg dbg scp-}

parseArgs ("-d":f:as) = (True, f, as)
parseArgs (f:as) = (False, f, as)

{-runProg dbg s = do-}
    {-when dbg $ print s-}
    {-case getFunction "main" s of-}
        {-Just (args, e) -> do-}
            {--- TODO pass in Commandline args-}
            {-let s' = if takesArgs args-}
                {-then bindMain args s-}
                {-else s-}
            {-void $ runEval e (s,dbg)-}
        {-Nothing -> putStr "Err: main not found"-}
          {---TODO Preprocessor guaruntee strict literal-}
     {-where bindMain _ s = s-}
           {-takesArgs _ = False-}
    {-[>where getGbls = fromList . map f <]-}
          {-[>f (Expr _ (EFix n a e)) = (n, VLambda a e)<]-}
          {-[>f (Expr _ (ELet (DName n) (Expr _ (ELit l)))) = (n, lit2Val l)<]-}

defaultAssumps :: [Assump]
defaultAssumps = map toAsmp [
   ("+UN",   "Num",        unary)
  ,("-UN",   "Num",        unary)
  ,("+",     "Num",        binary)
  ,("-",     "Num",        binary)
  ,("/",     "Fractional", binary)
  ,("*",     "Num",        binary)
  ,("//",    "Integral",   binary)
  ,("%",     "Integral",   binary)
  ,("<",     "Ord",        binary)
  ,(">",     "Ord",        binary)
  ,(">=",    "Ord",        binary)
  ,("<=",    "Ord",        binary)
  ,("==",    "Eq",         binary)
  ,("and",   "",           bbool)
  ,("or",    "",           bbool)
  ,("xor",   "",           bbool)
  ,("not",   "",           ubool)
  ]
  {-,("print", "",           mkFun [tString] unit)-}
  where binary = mkFun' [v',v'] v'
        unary  = mkFun' [v']   v'
        bbool = tBool `func` tBool `func` tBool
        ubool = tBool 
        bool = Tycon "Bool" Star
        unit = Tycon "()" Star
        v' = TVar v
        v  = Tyvar "a" Star
        toAsmp (i,"",t) = i :>: quantify [v] ([]:=> t)
        toAsmp (i,q,t)  = i :>: quantify [v] ([IsIn q [v']] :=> t)
