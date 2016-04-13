{-# Language FlexibleContexts, TupleSections #-}

module PreProcessor.ValidateFunctions where

import Models.Program
import Models.Validators
import Control.Monad.State
import Control.Monad.Trans.Either
import qualified Data.Map as M
import PreProcessor.Errors
import Text.Megaparsec.Pos

validateFunctions :: [Expr] -> Either String FnMap
validateFunctions fns = let (e,s) = run fns
                        in fmap (const s) e
    where run f = runState (runEitherT (val f)) M.empty
          val = mapM_ checkFunction 

checkFunction :: Expr -> Validator FnMap ()
checkFunction (Expr p (EFix n as e)) = do
    fns <- get
    if n `M.member` fns 
        then duplicateFunc p n 
        else do
            as <- checkArgs as
            put (M.insert n (as,e) fns)
checkFunction (Expr p _) = parserError p

checkArgs :: Monad m => [ArgDec] -> m FArgs
checkArgs as = checkArgs' as emptyArgs
    where checkArgs' [] = return 
          checkArgs (ArgDec pos a:as) fa = 
              checkArg fa a pos >>= checkArgs' as
          emptyArgs = FArgs [] [] Nothing Nothing

checkArg (FArgs _ [x] _ _) (PosDec i c) pos = 
    posAfterKw pos
checkArg (FArgs _ _ (Just _) _ ) (PosDec i c) pos = 
    posAfterS pos
checkArg (FArgs _ _ _ (Just _)) (PosDec i c) pos = 
    posAfterS pos
checkArg f@(FArgs p _ _ _) (PosDec i c) pos = 
    unique pos i f (FArgs ((i,c):p) [] Nothing Nothing)

checkArg f@(FArgs _ k _ (Just _)) (KWDec i l c) pos = 
    kwAfterSplat pos
checkArg f@(FArgs _ k _ _) (KWDec i l c) pos = 
    unique pos i f (f {faKW = (i, l, c):k})

checkArg f@(FArgs _ _ (Just _) _) (PSDec i c) pos =
    multiplePS pos
checkArg f (PSDec i c) pos = 
    unique pos i f (f {faPS = Just (i,c)})

checkArg f@(FArgs _ _ _ (Just _)) (KWSDec i c) pos =
    multipleKS pos
checkArg f (KWSDec i c) pos = 
    unique pos i f (f {faPS = Just (i,c)})

unique pos i (FArgs p k ps ks) r = if unique'
    then return r
    else duplicateArg pos i
    where unique' = or [inPos, inKw, inSplat ps, inSplat ks]
          inPos = i `elem` map fst p
          inKw  = i `elem` map (\(a,_,_) -> a) k
          inSplat (Just (n,_)) = i == n
          inSplat _ = False
