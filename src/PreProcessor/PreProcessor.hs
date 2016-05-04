{-# LANGUAGE TupleSections #-}

module PreProcessor.PreProcessor where

import Language.Types
import PreProcessor.Bindings
import Language.Utils hiding (find)
import Language.Program
import Language.Expressions
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.List (find, sort, partition)
import Data.Maybe (isJust)
import GHC.Exts (groupWith)

type PP a = State Int a

validate :: Program -> (ClassEnv, [Assump], [BindGroup], [Bind])
validate prg = evalState (preprocess prg) 0
                          
preprocess :: Program -> PP (ClassEnv, [Assump], [BindGroup], [Bind])
preprocess (Program bnds imps adt rdt bvs) = do
    (fbs', recCE) <- processRecs rdt
    (fbs, fieldCE) <- overload fbs'

    let (expl,impl) = splitBinds bnds
    (expl', overCE) <- overload expl
    bhCE <- makeCE bvs imps

    let constructors = concatMap dCstrs adt ++ concatMap dCstrs rdt 
        ce = M.unions [defClasses, bhCE, overCE, recCE, fieldCE] 
        stubs = ceStubs ce
        fassumps = bindingAssumps (fbs ++ overBinds bhCE)
        stubAssumps = map (\(i,s,_) -> i :>: s) stubs
        stubBins = map (\(i,s,ps) -> Bind i $ Annot s (Over i (TVar $ Tyvar "" Star) ps)) stubs
        assumps = makeCstrAssumps constructors ++ fassumps ++ stubAssumps

        consBinds = constrBinds constructors
        impBinds = fbs ++ consBinds ++ stubBins
        binds = impl ++ expl'  ++ overBinds ce

        {-aNames = concatMap dNames adt-}
        {-rNames = concatMap dNames rdt-}
        {-bNames = behaviorNames ce-}
        {-nNames = map bindName (binds ++ impBinds)-}
        {-names = aNames ++ rNames ++ bNames ++ nNames-}
    {-unless (uniq names) (fail "Duplicate name found")-}
    {-fullAp (map dTCons adt ++ map dTCons rdt) binds-}
 
    bs' <- replaceOverVars stubs binds
    return (ce, assumps, toBindGroup bs', impBinds)

replaceOverVars :: [Stub] -> [Bind] -> PP [Bind]
replaceOverVars sts = mapM (repOV sts) 
    where repOV ss (Bind i e) = Bind i <$> rVars ss e

rVars :: [Stub] -> Expr -> PP Expr
rVars ss (Ap e1 e2) = Ap <$> rVars ss e1 <*> rVars ss e2
rVars ss (Annot s e) = Annot s <$> rVars ss e
rVars ss (Let bs e) = Let <$> replaceOverVars ss bs <*> rVars ss e
rVars ss (Abs p e) = Abs p <$> rVars ss e
rVars ss (Var v) = case find (\(i,_,_) -> i == v) ss of
    Nothing -> return (Var v)
    Just (i,_,ps) -> do t <- newTVar Star
                        return (Over i t ps)
rVars ss (Case e1 bs) = Case <$> rVars ss e1 <*> mapM bVars bs
    where bVars (p,e) = do e' <- rVars ss e
                           return (p, e')
rVars _ e = return e

bindingAssumps :: [Bind] -> [Assump]
bindingAssumps = map (\(Bind i (Annot s _)) -> i :>: s) 

constrBinds :: [(Id, Scheme)] -> [Bind]
constrBinds = map toBind 
    where toBind (n, s) = Bind n (Annot s (Var n))

makeCstrAssumps :: [(Id, Scheme)] -> [Assump]
makeCstrAssumps = map (uncurry (:>:))

overBinds :: ClassEnv -> [Bind]
overBinds = M.foldMapWithKey f
    where f _ (_,_,os) = concatMap unpack os
          unpack (i,_,ps) = map (\(sc,e) -> Bind (implName i sc) (Annot sc e)) ps

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

ceStubs :: ClassEnv -> [Stub]
ceStubs = M.foldl f []
    where f as (_,_,bs) = bs ++ as

uniq :: Eq a => [a] -> Bool
uniq xs = uniq' xs []
    where uniq' [] _ = True
          uniq' (y:ys) ls = y `notElem` ls && uniq' ys (y:ls)

makeCE :: [Behavior] -> [Implementation] -> PP ClassEnv
makeCE bs is = do
    ce <- M.fromList <$> foldCBS bs
    unless (uniq $ stubNames ce) 
           (fail "Behavior methods must be unique")
    foldM addImp ce is

addImp :: ClassEnv -> Implementation -> PP ClassEnv
addImp ce (Im q t i ss) = do
   (sc,is,stbs) <- case M.lookup i ce of
             Just cls -> return cls
             Nothing -> fail $ "Behavior " ++ i ++ "Not Found"
   let ist = q :=> IsIn i [spine t] 
       bns = map bindName ss
       sns = map (\(i',_,_) -> i') stbs
   unless (sort bns == sort sns) 
          (fail $ "Must implement stubs defined by " ++ i)
   let stbs' = foldl (addStub i (q :=> (TCons $  getCons t))) stbs ss
   return (M.insert i (sc,ist:is, stbs') ce)

addStub :: Id -> Qual Type -> [Stub] -> Bind -> [Stub] 
addStub cls qt stbs (Bind sn ex) = map add' stbs
    where add' (n, sc, bs)
              | n == sn = (n, sc, (pInst cls qt sc, ex) : bs)
              | otherwise = (n, sc, bs)

stubNames :: ClassEnv -> [Id]
stubNames = map (\(i,_,_) -> i) . concatMap (\(_,_,o) -> o) . M.elems 
    
foldCBS :: [Behavior] -> PP [(Id, Class)]
foldCBS = foldM f []
    where f bs b = (:bs) <$> toCB b (map fst bs)

toCB :: Behavior -> [Id] -> PP (Id, Class)
toCB (Bhvr q t n stbs) bs = do
    ss <- mapM sps q 
    return (n, (ss, [], map mkStub stbs))
    where mkStub (i, st) = let bt = spine t
                               sc = quantAll ([IsIn n [bt]] :=> st)
                           in (i, sc, [])
          sps (IsIn i _) = if i `elem` bs 
                then return i
                else fail "Super Behavior not yet defined"

spine :: Type -> Type
spine (TAp t _) = spine t
spine t = t

processRecs :: [Rec] -> PP ([Bind], ClassEnv)
processRecs rs = do
    --break into inherited/noninherited datatypes
    (inhPairs, regs) <-  filterInherits rs
    let rfBind = concatMap fieldBinds regs
         
        --group inheritance pairs into classes
        classes = groupWith (tname . dTCons . fst) inhPairs

    -- turn those classes into bindings/behaviors
    (bs, cbs) <- unzip <$> mapM promote classes
    return (rfBind ++ concat bs, M.fromList cbs)

promote :: [(Rec,Rec)] -> PP ([Bind],(Id,Class))
promote rs@((sp,_):_) = do
    -- All the classes that inherit from a type
    let imps = sp : map snd rs
        -- parse a class binding
        (n,(s,i,stubs)) = makeSuperClass sp imps
    -- condense bindings
    (binds,stbs) <- foldM (bindSup n) ([],stubs) imps
    return (binds, (n,(s,i,stbs)))
promote _ = fail "Nothing to Promote"

makeSuperClass :: Rec -> [Rec] -> (Id,Class)
makeSuperClass (Rec _ t ss fs) impl =
    let name = '*' : consName t
        stubs = map (superStub name) fs
        supers = map (('*':) . consName) ss
        ts = map qualed impl
        ists = map (\(p:=>t') -> p :=> IsIn name [t']) ts
    in (name, (supers, ists, stubs))
    where superStub n (i,ft) = 
               let q =  [IsIn n [t]]
                   t' = [t] `mkFun` ft
               in (i, quantQual q t', [])


-- Return the list of overloaded field bindings and the
-- list of type-specific bindings
bindSup :: Id -> ([Bind], [Stub]) -> Rec -> PP ([Bind], [Stub])
bindSup cls (bs,stubs) r@(Rec q t _ _) = do 
    let fbs = fieldBinds r
        (over,under) = split inStubs fbs
        --TODO VALIDATE THIS ASSUMPTION
    unless (length stubs == length over) 
           (fail "Must include all fields of super type")
    let stbs = foldl (addStub cls (q :=> t)) stubs over
    return (under ++ bs, stbs)
    where inStubs b = isJust (find (\(i,_,_) -> i == bindName b) stubs)

fieldBinds :: Rec -> [Bind]
fieldBinds (Rec q t _ fs) = zipWith f fs [0..]
    where f (n, ft) i = let sch = quantQual q ([ft] `mkFun` t)
                        in Bind n (Annot sch $ Var ("#" ++ show (i :: Int)))


filterInherits :: [Rec] -> PP ([(Rec,Rec)],[Rec])
filterInherits rs = do (a,b,_) <- foldM f ([],[],[]) rs
                       let sps = map fst a
                           regs = [x | x <- b, x `notElem` sps]
                       return (a,regs)
    where f (is,ns,rs') r = if null (sups r)
              then return (is, r:ns, r:rs')
              else do impls <- mapM (findSup r rs') (sups r)
                      return (impls ++ is,ns, r:rs')
          findSup r rs' s = case supDefined s rs' of
              (Just sup) -> return (sup,r)
              Nothing  -> fail "Super class not defined"
          supDefined s = find (\x -> getCons s == dTCons x)


overload :: [Bind] -> PP ([Bind], ClassEnv)
overload bs = do
    let groups = groupWith bindName bs
        (under,over) = split ((1 ==) . length) groups
    ce <- M.unions <$> mapM overBehavior over
    let bnds = concat under
    return (bnds, ce)

overBehavior :: [Bind] -> PP ClassEnv
overBehavior bs@((Bind i _):_) = do
    pairs <- mapM bindPairs bs
    let cname = "_over_" ++ i
        v = TVar $ Tyvar "^var" Star
        sch = quantQual [IsIn cname [v]] v
        stubs =  [(i, sch, pairs)]
    ists <- mapM (makeInst cname . fst) pairs
    return $ M.singleton cname ([], ists, stubs)
    where 
          makeInst cls sc = do
                (qs :=> t) <- freshInst sc  
                return $ qs :=> IsIn cls [t]
          bindPairs (Bind _ (Annot sc ex)) = if null (tv sc)
                then return (sc, ex)
                else fail $ "Incomplete type signature for "++i
          bindPairs _ = fail "something is horribly wrong"
overBehavior _ = fail "overBehavior passed empty argument"

freshInst :: Scheme -> PP (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

consName :: Type -> Id
consName (TAp t _) = consName t
consName (TCons (Tycon n _)) = n
consName _ = error "Cons Name passed not a constructor"

behaviorNames :: ClassEnv -> [Id]
behaviorNames = M.keys

fullAp :: [Tycon] -> [Bind] -> PP ()
fullAp _ _ = return ()

genMain :: [Bind] -> PP Expr
genMain bs = do
    let (m, bs') = split (("main" ==) . bindName) bs
    unless (length m == 1) (fail "Could not find main")
    return $ Let bs' (bindExpr $ head m)
    
newTVar :: Kind -> PP Type
newTVar k = do i <- get
               let v = "&var" ++ show i
               put (i + 1)
               return $ TVar $ Tyvar v k

tname :: Tycon -> Id
tname (Tycon n _) = n

qualed :: Rec -> Qual Type
qualed (Rec q t _ _) = q :=> t

split :: (a -> Bool) -> [a] -> ([a], [a])
split p = foldl f ([],[])
    where f (as,bs) x = if p x then (x:as,bs) else (as,x:bs)

groupExpl :: [Bind] -> [[Bind]]
groupExpl = groupWith bindName

emptySch :: Scheme
emptySch = Forall [Star] ([] :=> TGen 0)
