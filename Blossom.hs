{-# LANGUAGE GADTs, TupleSections #-}

import Exprs
import Constraints
import Interpretor
import Models
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans
import GHC.Exts
import Text.Megaparsec
import System.Environment
import Control.Monad.State

main = do 
    (dbg, file, mainArgs) <- parseArgs <$> getArgs
    prog <- parseBlossom file
    case prog of
        Left err -> print err
        Right prg -> runProg dbg prg

parseArgs ("-d":f:as) = (True, f, as)
parseArgs (f:as) = (False, f, as)

runProg dbg prog = do
    let s = Scope (getGbls (globals prog)) M.empty
    when dbg $ print s
    case getFunction "main" s of
        Just (args, e) -> do
            -- TODO pass in Commandline args
            let s' = if takesArgs args
                then bindMain args s
                else s
            void $ runEval e (s,dbg)
        Nothing -> putStr "Err: main not found"
          --TODO Preprocessor guaruntee strict literal
    where getGbls = fromList . map f 
          f (Expr _ (EFix n a e)) = (n, VLambda a e)
          f (Expr _ (ELet (DName n) (Expr _ (ELit l)))) = (n, lit2Val l)
          takesArgs _ = False
          bindMain _ s = s
