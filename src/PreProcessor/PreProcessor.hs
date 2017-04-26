module PreProcessor.PreProcessor 
    ( validate
    ) where

import Language.Types
import PreProcessor.Bindings
import PreProcessor.PP
import Language.Utils hiding (find)
import Language.Program
import Language.Expressions
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.List (partition)
{-import Data.Maybe (isJust)-}
import GHC.Exts (groupWith)

-- Take a program and turn the component pieces into a 
-- class environment, set of assumptions, a list of bindgroups
-- and a list of bindings. It will force an error if it encounters
-- any violations of the language spec.
validate :: Program -> (ClassEnv, [Assump], [BindGroup], [Bind])
validate prg = let (ce, as, (bg, b)) = runPP (preprocess prg) M.empty []
               in (ce, as, bg, b)
                          
preprocess :: Program -> PP ([BindGroup], [Bind])
preprocess (Program bnds imps adt rdt bvs) = do
    -- Add user declared behaviors to the store
    mapM_ (uncurry addClass) bvs
    -- Add user declared implementations to the store
    mapM_ addImp imps
    -- Make implicit behaviors out of inheritance relationships
    promoteSupers rdt
    -- Create implicit behavior implementations out of record types.
    -- The unbound behaviors are stored for processing later
    ubs <- concat <$> mapM recImps rdt
    -- Parse bindings from Algebraic Datatype constructors
    let adtBs = concatMap acnstrs adt
    -- overload user bindings and record fields
    bs <- overload $ bnds ++ map expl2Annot ubs
    fail ""
    {-unless (uniq $ stubNames ce)-}
    {-unless (uniq names) (fail "Duplicate name found")-}
    {-fullAp (map dTCons adt ++ map dTCons rdt) binds-}
    {-bs' <- replaceOverVars stubs binds-}
    {-return (ce, assumps, toBindGroup bs', impBinds)-}

expl2Annot :: Expl -> Bind
expl2Annot = undefined

-- Finds the super classes implied by a list of predicates
supNames :: [Pred] -> [Id]
supNames = map (\(IsIn i _) -> i)

-- Takes a list of Record data types and promotes them 
-- to super classes
promoteSupers :: [Rec] -> PP ()
promoteSupers rs = do 
    let sids = concatMap (map instName . sups) rs
        ss = filter ((`elem` sids) . dName) rs
    mapM_ promote ss

-- Returns the name of the class a given instance belongs
-- to
instName :: Inst -> Id
instName (_:=> IsIn i _) = i

-- Takes a single record data type and turns it into a 
-- class with stubs corresponding to the fields
promote :: Rec -> PP ()
promote r@(Rec (q:=>t) ss fs) = do 
    let mySups = map instName ss
    requireSupers mySups
    let cname = mangleRecName $ dName r
        snames = map explName $ rfields r
        im = Im (q :=> IsIn cname [t]) (map explToBind fs)
    addClass cname (Class mySups [im] snames)
    addImp im
 
recImps :: Rec -> PP [Expl]
recImps (Rec _ is fs) = do 
    let mySups = map instName is
    requireSupers mySups
    foldM bindPartial fs is

bindPartial :: [Expl] -> Inst -> PP [Expl]
bindPartial bs i@(qs:=>(IsIn sup _)) = do
    requireSupers (supNames qs)
    whenDef sup $ \(Class ss is sns) -> do
        let (b1, b2) = partition (\e -> explName e `elem` sns) bs
            b1' = map explToBind b1
        unless (fullImpl sns b1') $
            fail "Records must include all fields of their super types"
        let imp = Im i b1'
        modifyCE (M.insert sup (Class ss (imp:is) sns))  
        return b2

implName :: Id -> Scheme -> Id
implName i sc = "$" ++ i ++  "#" ++ show sc

pInst :: Id -> Qual Type -> Scheme -> Scheme
pInst cls (q:=>t) (Forall ks (ps:=>st)) = 
    let ([IsIn _ [TGen n]],qs) = partition qualFilter ps
        ts = zipWith (f n) [0..] ks
    in quantify (tv ts) ((q++qs) :=> inst ts st)
    where qualFilter (IsIn i _) = i == cls
          f n m k
            | n == m = t
            | otherwise = TVar $ Tyvar (show m) k

uniq :: Eq a => [a] -> Bool
uniq xs = uniq' xs []
    where uniq' [] _ = True
          uniq' (y:ys) ls = y `notElem` ls && uniq' ys (y:ls)


overload :: [Bind] -> PP [Bind]
overload bs = do
    let groups = groupWith bindName bs
        (under,_) = split ((1 ==) . length) groups
    {-ce <- M.unions <$> mapM overBehavior over-}
    let bnds = concat under
    return bnds

behaviorNames :: ClassEnv -> [Id]
behaviorNames = M.keys

fullAp :: [Tycon] -> [Bind] -> PP ()
fullAp _ _ = return ()

genMain :: [Bind] -> PP Expr
genMain bs = do
    let (m, bs') = split (("main" ==) . bindName) bs
    unless (length m == 1) (fail "Could not find main")
    return $ Let bs' (bindExpr $ head m)
    
split :: (a -> Bool) -> [a] -> ([a], [a])
split p = foldl f ([],[])
    where f (as,bs) x = if p x then (x:as,bs) else (as,x:bs)

-- Mangles the name of record data type before it becomes a class
mangleRecName :: Id -> Id
mangleRecName = ('_':)
{-emptySch :: Scheme-}
{-emptySch = Forall [Star] ([] :=> TGen 0)-}

{-replaceOverVars :: [Expl] -> [Bind] -> PP [Bind]-}
{-replaceOverVars sts = mapM (repOV sts) -}
    {-where repOV ss (Bind i e) = Bind i <$> rVars ss e-}

{-rVars :: [Expl] -> Expr -> PP Expr-}
{-rVars ss (Ap e1 e2) = Ap <$> rVars ss e1 <*> rVars ss e2-}
{-rVars ss (Annot (e:-:s)) = do e' <- rVars ss e-}
                              {-return (Annot (e' :-: s))-}
{-rVars ss (Let bs e) = Let <$> replaceOverVars ss bs <*> rVars ss e-}
{-rVars ss (Abs p e) = Abs p <$> rVars ss e-}
{-[>rVars ss (Var v) = case find (\(Stub (i:>:_) _) -> i == v) ss of<]-}
    {-[>Nothing -> return (Var v)<]-}
    {-[>Just (Stub (i:>:_) ps) -> do t <- newTVar Star<]-}
                                 {-[>return (Over i t ps)<]-}
{-rVars ss (Case e1 bs) = Case <$> rVars ss e1 <*> mapM bVars bs-}
    {-where bVars (p,e) = do e' <- rVars ss e-}
                           {-return (p, e')-}
{-rVars _ e = return e-}
