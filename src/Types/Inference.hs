module Types.Inference where

import Models.Types
import Types.Utils
import Models.Program
import Models.Expressions
import Text.Megaparsec
import Data.List (nub, union, intersect, partition, (\\))
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (msum, liftM, ap, liftM2, foldM, zipWithM)
import GHC.Exts
import qualified Data.Map as M

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

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

tiProgram :: ClassEnv -> [Assump] -> [BindGroup] -> [Assump]
tiProgram ce as bgs = runTI $  do
    (ps, as') <- tiSeq tiBindGroup ce as bgs
    s <- getSubst
    rs <- reduceCtx ce (apply s ps)
    if null (ambiguities (tv (as'++ as)) rs)
        then return $ apply s as'
        else fail "ambiguous type"
        --TODO might not be the right way to handle ambiguities

runTI :: TI a -> a
runTI (TI f) = x where (_, _, x) = f nullSubst 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s,n,s))

unify :: Type' -> Type' -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 {-s <- extSubst u-}
                 {-ss <- checkSubst-}
                 {-return s-}
                 extSubst u
    where extSubst s' = TI (\s n -> (s'@@s, n, ()))

newTVar :: Kind -> TI Type'
newTVar k = TI (\s n -> let v = Tyvar (enumId n) k
                        in (s, n+1, TVar v))

freshInst :: Scheme -> TI (Qual Type')
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)

class Instantiate t where
    inst :: [Type'] -> t -> t

instance Instantiate a => Instantiate (Lex a) where
    inst ts (Lex p a) = Lex p (inst ts a)
instance Instantiate Type' where
    inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
    inst ts (TGen n)  = ts !! n
    inst ts t         = t
instance Instantiate a => Instantiate [a] where
    inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
    inst ts (ps:=>t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
    inst ts (IsIn c t) = IsIn c (inst ts t)

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

tiExpr :: Infer Expr Type'
tiExpr ce as (Lex p e) = tiExpr' ce as e

tiExpr' ce as (Var i) =  lookupSc as i
tiExpr' ce as (Lit l) = tiLit' l
tiExpr' ce as (Abs p e) = tiAlt' ce as (p,e)
tiExpr' ce as (Ap e1 e2) = do 
    (ps, t1) <- tiExpr' ce as e1
    (qs, t2) <- tiExpr' ce as e2 
    t <- newTVar Star
    unify t1 (t2 `func` t)
    return (ps ++ qs, t) 

{-tiExpr' ce as (Let bg ex') = do (ps, as') <- tiBindGroup ce as bg-}
                                 {-(qs, t) <- tiExpr ce (as' ++ as) ex'-}
                                 {-return (ps++qs, t)-}

tiExpr' ce as (Case e bs) = fail "CASE"
tiExpr' ce as (Let{}) = fail "LET"

tiExprs' ce as ts = do res <- mapM (tiExpr' ce as) ts
                       let ps' = concat [ps | (ps,_) <- res]
                           ts' = [ts | (_,ts) <- res]
                       return (ps',ts')

lookupSc as i = do
    sc <- find i as
    (ps :=> t) <- freshInst sc
    return (ps,t)

tiLit :: Literal -> TI ([Pred], Type')
tiLit = tiLit' . unwrap

tiLit' (LChar _)   = return ([], tChar)
tiLit' (LInt _)    = return ([], tInt)
tiLit' (LFloat _)  = return ([], tFloat)
tiLit' (LType _)   = return ([], tType)
tiLit' LNull       = return ([], tNull)


tiPat :: [Assump] -> Pat -> TI ([Pred], [Assump], Type')
tiPat as (Lex pos t) = uniqVars t >>= tiPat' as
    where uniqVars p = if uniq' (varNames p)
                          then return p
                          else fail $ show pos ++ "Variable names in patterns must be unique"
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
tiPat' as (PLit l) = do 
    (ps,t) <- tiLit' l
    return (ps, [], t)
tiPat' ass (PCons i pats) = do 
    --TODO Arity check
    sc <- find i ass
    (ps, as, ts) <- tiPats ass pats
    rt <- newTVar Star
    (qs :=> scType) <- freshInst sc
    unify scType (foldr func rt ts)
    return (ps ++ qs, as, rt)

tiPats :: [Assump] -> [Pat'] -> TI ([Pred], [Assump], [Type'])
tiPats ass ps = do ppat <- mapM (tiPat' ass)  ps
                   let ps = concat [ps' | (ps',_,_) <- ppat]
                       as = concat [as' | (_,as',_) <- ppat]
                       ts = [t | (_,_,t) <- ppat]
                   return (ps, as, ts)

tiAlt :: Infer Alt Type'
tiAlt ce as (pat, e) = do (ps, as', tp) <- tiPat as pat
                          (qs, te) <- tiExpr ce (as' ++ as) e
                          return (ps ++ qs, tp `func` te)

tiAlt' ce as (pat, e) = do (ps, as', tp) <- tiPat' as pat
                           (qs, te) <- tiExpr' ce (as' ++ as) e
                           return (ps ++ qs, tp `func` te)

tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                         mapM_ (unify t .snd) psts
                         return (concatMap fst psts)

split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred],[Pred])
split ce fs gs ps = do 
    ps' <- reduceCtx ce ps
    let (ds,rs) = partition (all (`elem` fs) . tv) ps'
    if null (ambiguities (fs ++ gs) rs)
        then return (ds, rs)
        else fail "ambiguous type"

ambiguities :: [Tyvar] -> [Pred] -> [(Tyvar, [Pred])]
ambiguities vs ps = [(v, filter(elem v .tv) ps) | v <- tv ps \\ vs ]

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (i, sc, e) = do
    (qs:=>t) <- freshInst sc
    s  <- getSubst
    let --qs' = apply s qs
        t' = apply s t
        fs = tv (apply s as)
        gs = tv t'\\fs
        sc' = quantify gs ([] :=> t') 
        {-ps' = filter (not . entail ce qs') (apply s ps)-}
    (ds, rs) <- split ce fs gs []
    if sc /= sc' then
        fail "signature is to general"
    else if not (null rs) then
        fail "context too weak"
    else 
        return ds

{-restricted :: [Impl] -> Bool-}
{-restricted = any simple-}
    {-where simple (i,alt) = any (null . fst) alts-}

tiImpls  :: Infer [Impl] [Assump]
tiImpls ce as bs = do 
    ts <- mapM (\_ -> newTVar Star) bs
    let is    = map fst bs -- the names of the bindings
        scs   = map toScheme ts --create a scheme representing a generic typvar
        as'   = zipWith (:>:) is scs ++ as --assume that each of these ids implies the scheme
        es = map snd bs -- get the expressions for each binding
    {-pss <- zipWithM (tiAlts ce as') altss ts -- get pattern predicates-}
    s   <- getSubst
    let -- ps'     = apply s (concat pss)
        ts'     = apply s ts
        fs      = tv (apply s as)
        vss     = map tv ts'
        gs      = foldr1 union vss \\ fs
    (ds,rs) <- split ce fs (foldr1 intersect vss) []
    let scs' = map (quantify gs . (rs:=>)) ts'
    return (ds, zipWith (:>:) is scs')
    {-if restricted bs then-}
        {-let gs'  = gs \\ tv rs-}
            {-scs' = map (quantify gs' . ([]:=>)) ts'-}
        {-in return (ds++rs, zipWith (:>:) is scs')-}
    {-else-}
        {-let scs' = map (quantify gs . (rs:=>)) ts'-}
        {-in return (ds, zipWith (:>:) is scs')-}

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es,iss) = do 
    let as' = [ v:>:sc | (v,sc,alts) <- es ]
    (ps, as'') <- tiImpls ce (as'++as) iss
    qss        <- mapM (tiExpl ce (as''++as'++as)) es
    return (ps++concat qss, as''++as')

tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as []       = return ([],[])
tiSeq ti ce as (bs:bss) = do 
    (ps,as')  <- ti ce as bs
    (qs,as'') <- tiSeq ti ce (as'++as) bss
    return (ps++qs, as''++as')
                   
{-tiArgs :: Infer [Arg] Type'-}
{-tiArgs ce as args  = do (ps,ts) <- unzip <$> mapM argType args-}
                        {-return (concat ps, tProduct ts)-}
    {-where argType (Lex _ (PArg e)) = tiExpr ce as e-}
