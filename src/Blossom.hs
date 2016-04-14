{-# LANGUAGE GADTs, TupleSections #-}

import Parser.Parser
import PreProcessor.PreProcessor
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
    parsed <- parseBlossomFile file
    case parsed of
        Left err -> print err
        Right tops -> do
           mapM_ print tops
           let (ce', as, (es,is), bs) = validate tops
               ce = ce' `M.union` classes
           putStrLn "Assumptions:"
           mapM_ print as
           {-putStrLn "Builtin bindings:"-}
           {-mapM_ print bs-}
           putStrLn "Explicit Binds:"
           mapM_ print es
           putStrLn "Implicit Binds:"
           mapM_ print is
           {-putStrLn "ClassEnv:"-}
           {-print ce-}
           putStrLn "Passed PreProcessor"
           let assumps = zeroProd:printAs:sqAssump : as ++ prodAssumps ++ defaultAssumps
           {-mapM_ print assumps-}
           let bg = fixBG (es,is)
               as' = tiProgram ce assumps [bg]
           mapM_ print as'

           {-print bs-}
           {-mapM_ print (as ++ defaultAssumps)-}
           {-let as' = tiProgram ce (as ++ defaultAssumps) bs-}
           {-print as-}
           {-putStrLn "Success"-}
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

prodAssumps = map makePA [1..10]
makePA n = name :>: Forall stars ([] :=> t) 
    where tc = TCons $ Tycon name (kAry n)
          rt = foldl TAp tc gens
          t = foldr func rt gens
          gens = map TGen [0..n-1]
          name = show n ++ "PROD"
          stars = replicate n Star

sqAssump = "!seq" :>: Forall [Star, Star] ([] :=> ([TGen 0,TGen 1] `mkFun` TGen 1))
zeroProd = "0PROD" :>: Forall [] ([] :=> (TCons $ Tycon "0PROD" Star))
printAs = "print" :>: Forall [Star] ([IsIn "Showable" [TGen 0]] :=> (TGen 0 `func` tNull))

classes = M.fromList $ map mkCls 
    [("Eq", [], [tInt, tChar, tFloat, tBool])
    ,("Showable", [], [tInt, tChar, tFloat, tBool])
    ,("Ord", ["Eq"], [tInt, tChar, tFloat])
    ,("Num", ["Eq","Show"], [tInt, tFloat])
    ,("Real", ["Num","Ord"], [tInt, tFloat])
    ,("Fractional", ["Num"], [tFloat])
    ,("Floating", ["Fractional"], [tFloat])]
mkCls (id,ss,is) = (id,(ss, insts, stubs))
    where insts = map (\t -> [] :=> IsIn id [t]) is
          stubs = []

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
  where binary = mkFun [v',v'] v'
        unary  = mkFun [v']   v'
        bbool = tBool `func` tBool `func` tBool
        ubool = tBool 
        bool = Tycon "Bool" Star
        unit = Tycon "()" Star
        v' = TVar v
        v  = Tyvar "a" Star
        toAsmp (i,"",t) = i :>: quantify [v] ([]:=> t)
        toAsmp (i,q,t)  = i :>: quantify [v] ([IsIn q [v']] :=> t)
