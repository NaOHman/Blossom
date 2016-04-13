{-# Language FlexibleContexts #-}

module PreProcessor.CallValidators where

import Models.Validators
import Models.Expressions
import Models.Models
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import PreProcessor.Errors
import Text.Megaparsec.Pos

v :: Expr -> Validator (FnMap,DataMap) ()
v (Expr pos (ELit _)) = return ()
v (Expr pos (EVar _)) = return ()
v (Expr pos (ELambda _ e)) = v e
v (Expr pos (EFCall n as)) = do
    (fns,_) <- get
    case M.lookup n fns of
        Just (fargs,_) -> matchArgs pos n as fargs
        Nothing -> unknownFunction pos n
v (Expr pos (EFix _ _ e)) = v e
v (Expr pos (ELet p e)) = vp pos p >> v e
v (Expr pos (ECase e cs)) = v e >> vcase cs
    where vcase [] = return ()
          vcase ((p,e):cs) =  vp pos p >> v e >> vcase cs
v (Expr pos (EChain e1 e2)) = v e1 >> v e2

vp _ (DName _) = return ()
vp pos (DCons n ps) = do
    (_,dta) <- get
    case M.lookup n (cnstrs dta) of
        Just cinf -> matchCArgs ps cinf
        Nothing -> if n `S.member` ambig dta 
            then ambiguousConstructor pos n
            else unknownConstructor pos n
vp _ (DMatch _) = return ()
vp _ DNil = return ()

matchArgs pos n as = m n pos (map unpack as)
    where unpack (Arg _ a) = a
    
m n pos [] (FArgs [] _ _ _) = return ()
m n pos [] (FArgs [x] _ _ _) = tooFewArgs pos n

m n pos (PosArg _:as) (FArgs [] [] Nothing _) =
    tooManyArgs pos n
m n pos (PosArg _:as) f@(FArgs (x:xs) _ _ _) =
    m n pos as (f{faPos = xs})
m n pos (PosArg _:as) f@(FArgs _ (x:xs) _ _) =
    m n pos as (f{faKW = xs})

m n pos (KWArg i _:as) f@(FArgs _ [] _ Nothing) = 
    badKWArg i n pos
m n pos (KWArg i _:as) f@(FArgs _ kw _ kws) = 
    case findKW i kw of 
        Just nkw -> m n pos as (f {faKW = nkw})
        Nothing -> case kws of
            Just _ -> m n pos as f
            Nothing -> badKWArg i n pos
    where findKW i [] = Nothing 
          findKW i ((n,l,c):ks) = if i == n 
                then Just ks 
                else ((n,l,c):) <$> findKW i ks 
m n pos (PSplat _:as) f@(FArgs [] [] Nothing _) =
    splatImpossible pos n
m n pos (PSplat _:as) f =
    m n pos as f
m n pos (KWSplat _:as) f@(FArgs _ [] _ Nothing) =
    splatImpossible pos n
m n pos (KWSplat _:as) f =
    m n pos as f

--TODO IMPLEMENT CONSTRUCTOR ARG CHECKS
matchCArgs _ _ = return ()
