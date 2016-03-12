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
           print bs
           let as' = tiProgram ce as bs
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
