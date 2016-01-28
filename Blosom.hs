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
