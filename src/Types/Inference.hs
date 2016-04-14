module Types.Inference where

import Models.Types
import Types.Utils
import Models.Program
import Models.Expressions
import Text.Megaparsec
import Data.List (nub, union, intersect, partition, (\\))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Control.Arrow (first)
import Control.Monad (msum, liftM, ap, liftM2, foldM, zipWithM, when)
import GHC.Exts
import qualified Data.Map as M

tiExpr :: Infer Expr Type
tiExpr ce as (Var i) =  lookupSc as i
tiExpr ce as (Lit l) = tiLit l
tiExpr ce as (Abs a) = do 
    t <- newTVar Star
    qs <- tiAlt ce as t a
    return (qs, t)
tiExpr ce as (Ap e1 e2) = do 
    (ps, t1) <- tiExpr ce as e1
    (qs, t2) <- tiExpr ce as e2 
    t <- newTVar Star
    let err = "TIAp " ++ show (Ap e1 e2)
    unify err t1 (t2 `func` t)
    return (ps ++ qs, t) 
tiExpr ce as (Let bg ex) = do (ps, as') <- tiBindGroup ce as (fixBG bg)
                              (qs, t) <- tiExpr ce (as' ++ as) ex
                              return (ps++qs, t)
tiExpr ce as (Case e bs) = do 
    (ps, te) <- tiExpr ce as e
    bt <- newTVar Star
    rt <- newTVar Star
    qs <- tiAlts ce as bs bt
    let err = "CASE" ++ show (Case e bs)
    unify err bt (te `func` rt)
    return (ps ++ qs, rt) 

tiExprs ce as ts = do res <- mapM (tiExpr ce as) ts
                      let ps' = concat [ps | (ps,_) <- res]
                          ts' = [ts | (_,ts) <- res]
                      return (ps',ts')

lookupSc as i = do
    sc <- find i as
    (ps :=> t) <- freshInst sc
    return (ps,t)

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LChar _)   = return ([], tChar)
tiLit (LInt _)    = return ([], tInt)
tiLit (LFloat _)  = return ([], tFloat)
tiLit LNull       = return ([], tNull)


tiPat :: [Assump] -> Pat -> TI ([Pred], [Assump], Type)
tiPat as t = uniqVars t >>= tiPat' as
    where uniqVars p = if uniq' (varNames p)
                          then return p
                          else fail "Variable names in patterns must be unique"
          uniq' (x:y:zs) = x /= y && uniq' (y:zs) && uniq' (x:zs)
          uniq' _ = True
          varNames (PAs i p) = i : varNames p
          varNames (PVar i) = [i]
          varNames (PCons i ps) = concatMap varNames ps
          varNames _ = []

tiPat' as (PAs i p) = do 
    (ps, as', t) <- tiPat' as p
    return (ps, (i:>:toScheme t) : as', t)
tiPat' _ (PVar i) = do 
    v <- newTVar Star
    return ([], [i:>:toScheme v], v)
tiPat' _ PNil = do 
    v <- newTVar Star
    return([],[],v)
tiPat' _ (PLit l) = do 
    (ps,t) <- tiLit l
    return (ps, [], t)
tiPat' ass (PCons i pats) = do 
    sc <- find i ass
    (ps, as, ts) <- tiPats ass pats
    rt <- newTVar Star
    (qs :=> t) <- freshInst sc
    let err = "TI Pat " ++ show (PCons i pats)
    {-fail $ show t ++ ", " ++ show ts ++ ", " ++ show rt-}
    unify err t (foldr func rt ts)
    {-unify err t (ts `mkFun` rt )-}
    return (ps ++ qs, as, rt)

tiPats :: [Assump] -> [Pat] -> TI ([Pred], [Assump], [Type])
tiPats ass ps = do ppat <- mapM (tiPat' ass) ps
                   let ps = concat [ps' | (ps',_,_) <- ppat]
                       as = concat [as' | (_,as',_) <- ppat]
                       ts = [t | (_,_,t) <- ppat]
                   return (ps, as, ts)

{-tiAlt :: Infer TAlt Type-}
tiAlt ce as t (p, e) = do (ps, as', tp) <- tiPats as p
                          (qs, te) <- tiExpr ce (as' ++ as) e
                          let err = "TIAlt :" ++ show p ++" :: " ++ show tp ++ ", " ++ show e ++ " :: " ++ show te
                          unify err t (tp `mkFun` te)
                          return (ps ++ qs)
                           {-if null tp then -}
                               {-return (ps ++ qs, te)-}
                             {-else-}
                               {-return (ps ++ qs, tp `mkFun` te)-}

tiAlts ce as alts t = concat <$> mapM (tiAlt ce as t) alts
                         {-let err = "TIAlts " ++ show alts-}
                         {-mapM_ (unify err t . snd) psts-}
                         {-return (concatMap fst psts)-}

split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps = do 
    ps' <- reduce ce ps
    let (ds, rs) = partition (all (`elem` fs) . tv) ps'
    rs' <- defaultedPreds ce (fs++gs) rs
    return (ds, rs \\ rs')

defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds  = withDefaults (\vps ts -> concatMap snd vps)

ambiguities :: [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities vs ps = [(v, filter(elem v . tv) ps) | v <- tv ps \\ vs ]

tiExpl :: ClassEnv -> [Assump] -> TExpl -> TI [Pred]
tiExpl ce as (i, sc, alt) = do 
    (qs :=> t) <- freshInst sc
    ps         <- tiAlt ce as t alt
    s          <- getSubst
    let qs'     = apply s qs
        t'      = apply s t
        fs      = tv (apply s as)
        gs      = tv t' \\ fs
        sc'     = quantify gs (qs':=>t')
        ps'     = filter (not . entail ce qs') (apply s ps)
    (ds,rs)    <- split ce fs gs ps'
    if sc /= sc' then
        fail "signature too general"
      else if not (null rs) then
        fail "context too weak"
      else
        return ds

restricted :: [TImpl] -> Bool
restricted = any simple 
    where simple (i,([],e)) = True
          simple _ = False


tiImpls :: Infer [TImpl] [Assump]
tiImpls ce as bs = do 
    ts <- mapM (\_ -> newTVar Star) bs
    let is    = map fst bs --names of bindings
        scs   = map toScheme ts --empty schemes
        as'   = zipWith (:>:) is scs ++ as -- assume each binding
        altss = map snd bs -- get the expression for the bindings
    pss <- zipWithM (tiAlt ce as') ts altss
    s   <- getSubst
    let ps'     = apply s (concat pss)
        ts'     = apply s ts
        fs      = tv (apply s as)
        vss     = map tv ts'
        gs      = foldr1 union vss \\ fs
    (ds,rs) <- split ce fs (foldr1 intersect vss) ps'
    if restricted bs then
        let gs'  = gs \\ tv rs
            scs' = map (quantify gs' . ([]:=>)) ts'
        in return (ds++rs, zipWith (:>:) is scs')
    else
        let scs' = map (quantify gs . (rs:=>)) ts'
            in return (ds, zipWith (:>:) is scs')

tiBindGroup :: Infer BG [Assump]
tiBindGroup ce as (es,iss) = do 
    let as' = [ v:>:sc | (v,sc,alts) <- es ]
    (ps, as'') <- tiSeq tiImpls ce (as'++as) iss
    qss        <- mapM (tiExpl ce (as''++as'++as)) es
    return (ps++concat qss, as''++as')

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as []       = return ([],[])
tiSeq ti ce as (bs:bss) = do 
    (ps,as')  <- ti ce as bs
    (qs,as'') <- tiSeq ti ce (as'++as) bss
    return (ps++qs, as''++as')

tryPat as p = runTI $ do
    (_,as,t) <- tiPat as p
    s <- getSubst
    return (apply s as, apply s t)

tryPats as ps = runTI $ do
    (_,as,t) <- tiPats as ps
    s <- getSubst
    return (apply s as, apply s t)
   

tiProgram :: ClassEnv -> [Assump] -> [BG] -> [Assump]
tiProgram ce as bgs = runTI $ do
    (ps, as') <- tiSeq tiBindGroup ce as bgs
    s         <- getSubst
    {-return (apply s as')-}
    rs        <- reduce ce (apply s ps)
    s'        <- defaultSubst ce [] rs
    return (apply (s'@@s) as')

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

type TAlt = ([Pat], Expr)
type TImpl = (Id, TAlt)
type TExpl = (Id, Scheme, TAlt)
type BG = ([TExpl], [[TImpl]])

fixBG :: BindGroup -> BG
fixBG (es, is) = (map fixEs es, [map fixIs is])
    where fixEs ((i,s, Abs (p, e))) = (i,s,(p, e))
          fixEs ((i,s, e)) = (i,s,([], e))
          fixIs ((i, Abs (p,e))) = (i,(p, e))
          fixIs ((i, e)) = (i,([], e))

instance Monad TI where
    return x = TI (\s n -> (s, n, x))
    TI f >>= g = TI (\s n -> case f s n of 
            (s', m, x) -> let TI gx = g x
                          in gx s' m)
instance Applicative TI where
    pure  = return
    (<*>) = ap

instance Functor TI where
    fmap = liftM


defaultSubst :: Monad m => ClassEnv -> [Tyvar] -> [Pred ] -> m Subst
defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)

withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a) -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
    | any null tss = fail "cannot resolve ambiguity"
    | otherwise = return (f vps (map head tss))
    where vps = ambiguities vs ps
          tss = map (candidates ce) vps

type Ambiguity = (Tyvar,[Pred])

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v , qs) = []

getSubst :: TI Subst
getSubst = TI (\s n -> (s,n,s))

unify :: Show a => a -> Type -> Type -> TI ()
unify a t1 t2 = do s <- getSubst
                   u <- mgu a (apply s t1) (apply s t2)
                   extSubst u
    where extSubst s' = TI (\s n -> (s'@@s, n, ()))

newTVar :: Kind -> TI Type
newTVar k = TI (\s n -> let v = Tyvar (enumId n) k
                        in (s, n+1, TVar v))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)


runTI :: TI a -> a
runTI (TI f) = x where (s,n,x) = f nullSubst 0
