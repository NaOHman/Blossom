import Parser.Parser
import PreProcessor.PreProcessor
import PreProcessor.Bindings
import Language.Inference
import Interpretor.Evaluator
import Language.Program
import Language.Expressions
{-import qualified Data.Map as M-}
import System.Environment
import System.Exit
import Data.List (isPrefixOf)
import Control.Monad

data Dbg = Dbg
    { dbgParser :: Bool
    , dbgPreProc :: Bool
    , dbgTypeCheck :: Bool
    , dbgInterpret :: Bool
    }

def :: Dbg
def = Dbg False False False False

main :: IO ()
main = do 
    args <- getArgs
    case parseArgs args of
        (Just (dbg, file, progArgs)) -> runBlossom file progArgs dbg
        _ -> usage

runBlossom :: String -> [String] -> Dbg -> IO ()
runBlossom file args dbg = do 
    parsed <- parseBlossomFile file
    case parsed of
        Left err -> do
            print err
            exitFailure  
        Right prg -> do
            when (dbgParser dbg) (debugParser prg)
            let (ce', as', bg, bs) = validate prg
                ce = ce' -- `M.union` classes -- TODO Push into PreProcessor logic
                as = as' -- ++ blossomAssumps
            when (dbgPreProc dbg) (debugPreProc ce as bg bs)
            {-let bgs = map fixBG bg-}
            let (assumps,s) = tiProgram ce as bg
            when (dbgTypeCheck dbg) (debugTypeCheck assumps)
            let myBinds = bs ++ apply s (flatten bg)
            interpretBlossom myBinds args (dbgInterpret dbg)

debugParser :: Program -> IO()
debugParser (Program bs is as rs bhs) = do 
    putStrLn "_______PREPROC OUTPUT_____"
    putStrLn "BEHAVIORS:"
    mapM_ print bhs
    putStrLn "IMPLEMENTATIONS:"
    mapM_ print is
    putStrLn "DATA TYPES:"
    mapM_ print rs
    mapM_ print as
    putStrLn "BINDINGS:"
    mapM_ print bs

debugPreProc :: ClassEnv -> [Assump] -> [BindGroup] -> [Bind] -> IO()
debugPreProc ce as bg bs = do
    putStrLn "_______PREPROC OUTPUT_____"
    putStrLn "CLASSENV:"
    print ce
    putStrLn "ASSUMPS:"
    mapM_ print as
    putStrLn "BINDGROUP:"
    mapM_ print bg
    putStrLn "BINDINGS:"
    mapM_ print bs

debugTypeCheck :: [Assump] -> IO()
debugTypeCheck as = do
    putStrLn "_______TYPECHECKER OUTPUT_____"
    mapM_ print as

parseArgs :: Monad m => [String] -> m (Dbg, String, [String])
parseArgs [f] = return (def, f, [])
parseArgs [f,s] = case parseDbg f of 
    (Just dbg) -> return (dbg, s,[])
    _ -> return (def, f, [s])
parseArgs (f:s:as) = case parseDbg f of 
    (Just dbg) -> return (dbg, s, as)
    _ -> return (def, f, s:as)
parseArgs _ = fail "Could not parse a file"

parseDbg :: String -> Maybe Dbg
parseDbg s | "+DBG" `isPrefixOf` s = Just 
                    (Dbg ('p' `elem` s) ('o' `elem` s) ('t' `elem` s) ('i' `elem` s))
           | otherwise = Nothing

usage :: IO ()
usage = do
    putStrLn "./blossom [+DBG[flags]] filename [prog args]"
    putStrLn "Debug flags:"
    putStrLn "    p: Parser output"
    putStrLn "    o: PreProcessor output"
    putStrLn "    t: Type checker output"
    putStrLn "    i: Interpretor output"
    exitSuccess
