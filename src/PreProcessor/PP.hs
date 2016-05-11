{-# LANGUAGE TupleSections #-}

module PreProcessor.PP 
    ( PP
    , whenDef
    , addImp
    , addCE
    , addClass
    , modifyAS
    , modifyTV
    , modifyCE
    , getTV
    , getAS
    , getCE
    , putTV
    , putAS
    , putCE
    , freshInst
    , newTVar
    , requireSupers
    , fullImpl
    ) where

import Language.Types
import Language.Program
import Language.Expressions
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.List (sort)

type PP a = State (Int,ClassEnv,[Assump]) a

whenDef :: Id -> (Class -> PP a) -> PP a
whenDef i f = getCE >>= \ce -> case M.lookup i ce of
    Just c -> f c
    Nothing -> fail $ "Behavior " ++ i ++ " not in ClassEnv"

requireSupers :: [Id] -> PP ()
requireSupers ss = do
    supsDef <- mapM bhvrDef ss
    unless (and supsDef) (fail "Super behavior undefined")

addImp :: Implementation -> PP ()
addImp i@(Im (qs:=>IsIn sup _) bs) = do
    requireSupers (supNames qs)
    whenDef sup (\(Class ss is sbs) -> do
        unless (fullImpl sbs bs) $ 
            fail "Must implement all the stubs"
        modifyCE (M.insert sup (Class ss (i:is) sbs)))
 
fullImpl :: [Id] -> [Expl] -> Bool
fullImpl ss bs = 
    let sns = map explName bs
    in sort ss == sort sns
    
addClass :: Id -> Class -> PP ()
addClass i c@(Class ss _ _) = do
    requireSupers ss
    def <- bhvrDef i
    unless (not def) 
        (fail $ "Class " ++ i ++ " Is already defined")
    modifyCE (M.insert i c)

supNames :: [Pred] -> [Id]
supNames = map (\(IsIn i _) -> i)

bhvrDef :: Id -> PP Bool 
bhvrDef i = liftM (i `M.member`) getCE

freshInst :: Scheme -> PP (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

newTVar :: Kind -> PP Type
newTVar k = do i <- getTV 
               let v = "&var" ++ show i
               putTV (i + 1)
               return $ TVar $ Tyvar v k

putTV :: Int -> PP ()
putTV i = modifyTV (const i)

putCE :: ClassEnv -> PP ()
putCE c = modifyCE (const c)

addCE :: ClassEnv -> PP ()
addCE c = getCE >>= mergeCE c >>= putCE
    where mergeCE = undefined

putAS :: [Assump] -> PP ()
putAS a = modifyAS (const a)

getTV :: PP Int
getTV = get >>= \(i,_,_) -> return i

getCE :: PP ClassEnv
getCE = get >>= \(_,c,_) -> return c

getAS :: PP [Assump]
getAS = get >>= \(_,_,a) -> return a

modifyTV :: (Int -> Int) -> PP ()
modifyTV f = let f' (i,c,a) = (f i, c, a)
             in modify f'
modifyCE :: (ClassEnv -> ClassEnv) -> PP ()
modifyCE f = let f' (i,c,a) = (i, f c, a)
             in modify f'
modifyAS :: ([Assump] -> [Assump]) -> PP ()
modifyAS f = let f' (i,c,a) = (i, c, f a)
             in modify f'
