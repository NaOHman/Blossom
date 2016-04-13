{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PreProcessor.PreProcessor where

-- Map from type names to valid Constructors for the type

import Models.Program
import Types.Utils hiding (find)
import Control.Monad (foldM)
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (find, sort)
import Data.Maybe (isJust)
import GHC.Exts (groupWith)
import Text.Megaparsec.Pos

type PP a = State Int a

validate ts = evalState (preprocess ts) 0

preprocess :: [Top] -> PP (ClassEnv, [Assump], Expr)
preprocess ts = do
    let (bnds, rdt, adt, bvs, imps) = splitTops ts
    (fbs, recCE) <- processRecs rdt
    let (expl,impl) = splitBinds (bnds ++ fbs)
    (expl', overCE) <- overload expl
    bhCE <- makeCE bvs imps

    let aAssumps = map (uncurry (:>:)) (concatMap dCstrs adt)
        rAssumps = map (uncurry (:>:)) (concatMap dCstrs rdt)
        assumps = rAssumps ++ aAssumps

        impl' = map Impl impl
        ce = M.unions [bhCE, overCE, recCE] 
        binds = impl' ++ expl' ++ ceBinds ce

        aNames = concatMap dNames adt
        rNames = concatMap dNames rdt
        bNames = behaviorNames ce
        nNames = map bindingName binds
        names = aNames ++ rNames ++ bNames ++ nNames
    unless (uniq names) (fail "Duplicate name found")
    fullAp (map dTCons adt ++ map dTCons rdt) binds
 
    mn <- genMain binds
    return (ce, assumps, mn)

ceBinds :: ClassEnv -> [Binding]
ceBinds = M.foldMapWithKey f 
    where f cls (_,_,bs) = map (g cls) bs
          g cls (OBind i fn ex) = let tv = Tyvar "&var^" Star
                                      t = TVar tv
                                      pred = [IsIn cls [t]]
                                      sch = quantify [tv] (pred :=> fn t)
                                  in Expl (i, sch, ex)

uniq :: Eq a => [a] -> Bool
uniq xs = uniq' xs []
    where uniq' [] _ = True
          uniq' (x:xs) ls = x `notElem` ls && uniq' xs (x:ls)

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
   let inst = q :=> IsIn i [t] 
       bns = map fst ss
       sns = map obName stbs
   unless (sort bns == sort sns) 
          (fail $ "Must implement stubs defined by " ++ i)
   let stbs' = foldl (addStub q t) stbs ss
   return (M.insert i (sc,inst:is, stbs') ce)

addStub :: [Pred] -> Type -> [OBind] -> (Id, Expr) -> [OBind] 
addStub q t stbs (sn, ex) = map add' stbs
    where add' (OBind n f (Over bs))
              | n == sn = OBind n f (Over ((q :=> f t,ex) : bs))
              | otherwise = OBind n f (Over bs)

stubNames = map obName . concatMap (\(_,_,o) -> o) . M.elems 
    
foldCBS = foldM f []
    where f bs b = (:bs) <$> toCB b (map fst bs)

toCB :: Behavior -> [Id] -> PP (Id, Class)
toCB (Bhvr q t n stbs) bs = do
    ss <- mapM sups q 
    return (n, (ss, [], map mkStub stbs))
    where mkStub (i, st) = let btv = bhVar t
                               fn x = apply (btv +-> x) st
                           in OBind i fn (Over [])
          sups (IsIn i _) = if i `elem` bs 
                then return i
                else fail "Super Behavior not yet defined"

bhVar (TAp t _) = bhVar t
bhVar (TVar tv) = tv

processRecs :: [Rec] -> PP ([Binding], ClassEnv)
processRecs rs = do
    --break into inherited/noninherited datatypes
    (inhPairs, regs) <-  filterInherits rs
    -- generate bindings for regular classes
    let rfBind = map Expl $ concatMap fieldBinds regs
         
        --group inheritance pairs into classes
        classes = groupWith (tname . dTCons . fst) inhPairs

    -- turn those classes into bindings/behaviors
    (bs, cbs) <- unzip <$> mapM promote classes
    return (rfBind ++ concat bs, M.fromList cbs)

promote :: [(Rec,Rec)] -> PP ([Binding],(Id,Class))
promote rs@((sup,_):_) = do
    -- All the classes that inherit from a type
    let imps = sup : map snd rs
        -- parse a class binding
        (n,(s,i,stubs)) = makeSuperClass sup imps
    -- condense bindings
    (binds,stbs) <- foldM bindSup ([], stubs) imps
    return (binds, (n,(s,i,stbs)))

makeSuperClass :: Rec -> [Rec] -> (Id,Class)
makeSuperClass s@(Rec q t ss fs) impl =
    let stubs = map superStub fs
        name = '*' : consName t
        supers = map (('*':) . consName) ss
        ts = map qualed impl
        insts = map (\(p:=>t) -> p :=> IsIn name [t]) ts
    in (name, (supers, insts, stubs))
    where superStub (id,ft) = OBind id (\t -> [t] `mkFun` ft) (Over [])


-- Return the list of overloaded field bindings and the
-- list of type-specific bindings
bindSup :: ([Binding],[OBind]) -> Rec -> PP ([Binding], [OBind])
bindSup (bs,stubs) r@(Rec q t _ ss) = do 
    let fbs = fieldBinds r
        (over,under) = split inStubs fbs
        --TODO VALIDATE THIS ASSUMPTION
        obs = map (\(i,_,e) -> (i,e)) over
    unless (length stubs == length over) 
           (fail "Must include all fields of super type")
    let stbs = foldl (addStub q t) stubs obs
    {-return stbs-}
    return (map Expl under ++ bs, stbs)
    where inStubs (b,s,_) = isJust (find (\a -> obName a == b) stubs)

fieldBinds (Rec q t _ fs) = zipWith f fs [0..]
    where f (n, ft) i = let sch = quantQual q ([ft] `mkFun` t)
                        in (n, sch, Var ("#ACC" ++ show i))


filterInherits :: [Rec] -> PP ([(Rec,Rec)],[Rec])
filterInherits rs = do (a,b,_) <- foldM f ([],[],[]) rs
                       return (a,b)
    where f (is,ns,rs) r = if null (sups r)
              then return (is, r:ns, r:rs)
              else do impls <- mapM (findSup r rs) (sups r)
                      return (impls ++ is,ns, r:rs)
          findSup r rs s = case supDefined s rs of
              (Just sup) -> return (sup,r)
              Nothing  -> fail "Super class not defined"
          supDefined s = find (\x -> getCons s == dTCons x)


overload :: [(Id, Scheme, Expr)] -> PP ([Binding], ClassEnv)
overload bs = do
    let groups = groupExpl bs
        (under,over) = split ((1 ==) . length) groups
    return under
    ce <- M.unions <$> mapM overBehavior over
    let bnds = map Expl (concat under)
    return (bnds, ce)

overBehavior :: [Expl] -> PP ClassEnv
overBehavior bs@((i,_, _):_) = do
    pairs <- mapM bindPairs bs
    let cname = "_over_" ++ i
        v = TVar $ Tyvar "^var" Star
        sch = quantQual [IsIn cname [v]] v
        insts = map (makeInst cname . fst) pairs
        stubs =  [OBind i id (Over pairs)]
    return $ M.singleton cname ([], insts, stubs)
    where 
          makeInst cls (qs :=> t) = qs :=> IsIn cls [t]
          bindPairs (_, sc, ex) = if null (tv sc)
                then (do qt <- freshInst sc
                         return (qt,ex))
                else fail $ "Incomplete type signature for "++i

splitBinds = foldl f ([],[]) 
    where  f (es, is) (Expl e) = (e:es,is) 
           f (es, is) (Impl i) = (es,i:is)

freshInst :: Scheme -> PP (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

consName (TAp t _) = consName t
consName (TCons (Tycon n _)) = n

bindingName (Expl (i,_,_)) = i
bindingName (Impl (i,_)) = i

behaviorNames = M.keys

fullAp :: [Tycon] -> [Binding] -> PP ()
fullAp _ _ = return ()

genMain :: [Binding] -> PP Expr
genMain bs = do
    let (m, bs') = split (("main" ==) . bindingName) bs
    unless (length m == 1) (fail "Could not find main")
    return $ Let (splitBinds bs') (exprOf $ head m)
    where exprOf (Impl (_,e)) = e
          exprOf (Expl (_,_,e)) = e
    
newTVar k = do i <- get
               let v = "&var" ++ show i
               put (i + 1)
               return $ TVar $ Tyvar v k

splitTops :: [Top] -> ([Binding], [Rec], [Adt], [Behavior], [Implementation])
splitTops = foldl split ([],[],[],[],[])
    where split (bs, rs, as, cs, is) (Bind b) = (b:bs, rs, as, cs, is)
          split (bs, rs, as, cs, is) (RDT r) = (bs, r:rs, as, cs, is)
          split (bs, rs, as, cs, is) (ADT a) = (bs, rs, a:as, cs, is)
          split (bs, rs, as, cs, is) (Bvr b) = (bs, rs, as, b:cs, is)
          split (bs, rs, as, cs, is) (Imp i) = (bs, rs, as, cs, i:is)

tname (Tycon n _) = n
qualed (Rec q t _ _) = q :=> t

split p = foldl f ([],[])
    where f (as,bs) x = if p x then (x:as,bs) else (as,x:bs)

obName (OBind n _ _) = n

groupExpl = groupWith (\(n,_,_) -> n)
emptySch = Forall [Star] ([] :=> TGen 0)
