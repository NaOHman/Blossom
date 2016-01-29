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
    (file:mainArgs) <- getArgs
    prog <- parseBlosom file
    case prog of
        Left err -> print err
        Right prg -> runProg prg

runProg prog = do
    let gbl  = getFuns (functions prog) M.empty
        s = Scope (getGbls (globals prog) gbl) M.empty
    when debug $ print s
    case getFunction "main" s of
        Just (args, e) -> do
            -- TODO pass in Commandline args
            let s' = if takesArgs args
                then bindMain args s
                else s
            void $ runEval e s
        Nothing -> putStr "Err: main not found"
          --TODO Preprocessor guaruntee strict literal
    where getFuns (Expr _ (EFix n a e):es) m =
                getFuns es (M.insert n (VLambda a e) m)
          getFuns [] m = m 
          getGbls (Expr _ (ELet (DName n) (Expr _ (ELit l))):es) m = 
                getGbls es (M.insert n (lit2Val l) m)
          getGbls [] m = m 
          takesArgs _ = False
          bindMain _ s = s
