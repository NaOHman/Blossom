{-# LANGUAGE GADTs, TupleSections #-}

import Parser.Parser
import PreProcessor.PreProcessor
import Types.Inference
import LangDef.Blossom
import Interpretor.Interpretor
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
    runBlossom file dbg

runBlossom file dbg = do 
    parsed <- parseBlossomFile file
    case parsed of
        Left err -> print err
        Right tops -> do
           {-mapM_ print tops-}
           let (ce', as, bg, bs) = validate tops
               ce = ce' `M.union` classes
           {-mapM_ (\bg -> print bg >> print "-------------") bg-}
           {-putStrLn "Assumptions:"-}
           {-mapM_ print as-}
           {-putStrLn "::::::::::::::"-}
           {-mapM_ print (flatten bg)-}
           let bgs = map fixBG bg
               (as',s) = tiProgram ce (as ++ blossomAssumps) bgs
               {-binds = map scrubBinds bs ++ map scrubEx es ++ is-}
           mapM_ print as'
           let myBinds = map scrubBinds $ bs ++ apply s (flatten bg)
           {-mapM_ print myBinds-}
           interpretBlossom myBinds dbg
           {-mapM_ print as'-}
           {-mapM_ print (apply s (flatten bg))-}

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

scrubBinds (Expl (i,_,e)) = (i,e)
scrubBinds (Impl (i,e)) = (i,e)
scrubEx (i,_,e) = (i,e)

flatten :: [BindGroup] -> [Binding]
flatten = concatMap flatten' 
    where flatten' (es, is) =  map Expl es ++ map Impl (concat is)

parseArgs ("-d":f:as) = (True, f, as)
parseArgs (f:as) = (False, f, as)
