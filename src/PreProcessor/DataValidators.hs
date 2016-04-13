{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TupleSections #-}
module PreProcessor.DataValidators where
    {-(validateDataTypes-}
{-) where-}

-- Map from type names to valid Constructors for the type
import Models.Validators
import Models.Program
import PreProcessor.Errors
import Control.Monad (foldM)
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Megaparsec.Pos

type ValData = Validator PrgData [String]
type FnMap = M.Map Id (FArgs,Expr)

validateDataTypes d = let (e,s) = run d
                      in fmap (,s) e
    where run d = runState (runEitherT (val d)) defTypes 
          val = fmap concat . mapM bind . datatypes

defTypes = PrgData (M.fromList [
    ("Bool", ["True","False"]),
    ("Int", []), ("Char", []), ("String",[])]) 
    (M.fromList [("True", CInf "Bool" M.empty), 
    ("False", CInf "Bool" M.empty)]) S.empty

bind :: Data -> ValData
bind (Data pos (ValCons n ps) cs) = do
    prg <- get
    if M.member n (dta prg) -- Checking for redefined names
        then errDataDeclared n pos
        else do 
            modData (M.insert n (map fst cs)) -- add data
            bindCstrs pos n cs -- validate and add constructors to the info
-- The parser shouldn't return anything but a ValCons, but just in case
bind (Data pos _ _) = errDataName pos

bindCstrs :: SourcePos -> Id -> [(Id, [ArgDec])] -> ValData
bindCstrs pos t cs = forM cs $ \(n,as) -> do
    prg <- get
    if S.member n (ambig prg) 
        then bindAmbig pos t n as
        else case M.lookup n (cnstrs prg) of
            Just cinf -> do 
                disambig pos n cinf 
                bindAmbig pos t n as
            Nothing -> 
                bindC pos t n as >> return ""

bindC pos t n as = do
    args <- validateCArgs pos as
    modCnstr (M.insert n (CInf t args))

bindAmbig :: SourcePos -> Id -> Id -> [ArgDec] -> Validator PrgData String
bindAmbig pos t n as = do
    bindC pos t (t ++ "." ++ n) as
    warnMultConstrs pos n

disambig pos n c@(CInf t _) = do
    modCnstr (M.delete n)
    modCnstr (M.insert (t ++ "." ++ n) c)
    modAmbig (S.insert n)
    warnMultConstrs pos n

validateCArgs pos cs = foldM f M.empty (zip [1..] cs)
    where f m (n,ArgDec _ (PosDec "" _)) = return $
              M.insert ('$':show n) n m
          f m (n,ArgDec _ (PosDec s _)) = return $
              M.insert ('$':show n) n (M.insert s n m)
          f _ _ = parserError pos
        
modAmbig f = modify $ \p -> p {ambig = f (ambig p)} 
modData f = modify $ \p -> p {dta = f (dta p)}
modCnstr f = modify $ \p -> p {cnstrs = f (cnstrs p)}
