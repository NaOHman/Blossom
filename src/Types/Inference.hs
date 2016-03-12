module Types.Inference where

import Models.Types
import Types.Utils
import Models.Program
import Models.Expressions
import Text.Megaparsec
import Data.List (nub, union, intersect, partition, (\\))
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (msum, liftM, ap, liftM2, foldM, zipWithM)
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
    if null (ambiguities (tv (as'++as)) rs)
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

tiLit :: Literal -> TI ([Pred], Type')
tiLit = tiLit' . unwrap

tiLit' :: Literal' -> TI ([Pred], Type')
tiLit' (LChar _) = return ([], tChar)
tiLit' (LInt _) = do v <- newTVar Star 
                     return([IsIn "Num" [v]], v)
tiLit' (LFloat _) = do v <- newTVar Star
                       return([IsIn "Num" [v]], v) 
tiLit' (LType _)   = return ([], tType)
tiLit' LNull       = return ([], tNull)

{-tiLit (LCons i _) = getType i-}

tiPat :: Pat -> TI ([Pred], [Assump], Type')
tiPat (Lex pos t) = uniqVars t >>= tiPat'
    where uniqVars p = if uniq' (varNames p)
                          then return p
                          else fail $ show pos ++ "Variable names in patterns must be unique"
          uniq' (x:y:zs) = x /= y && uniq' (y:zs) && uniq' (x:zs)
          uniq' _ = True
          varNames (PAs i p) = i : varNames p
          varNames (PVar i) = [i]
          varNames (PCons i ps) = concatMap varNames ps
          varNames _ = []

tiPat' (PAs i p) = do (ps, as, t) <- tiPat' p
                      return (ps, (i:>:toScheme t) : as, t)
tiPat' (PVar i) = do v <- newTVar Star
                     return ([], [i:>:toScheme v], v)
tiPat' PNil = do v <- newTVar Star
                 return([],[],v)
tiPat' (PLit l) = do (ps,t) <- tiLit l
                     return (ps, [], t)
{-tiPat' (PCons i ps) = do (ps, as, ts) <- tiPats ps-}
                         {-t' <- newTVar Star-}
                         {-sc <- getScheme i-}
                         {-(qs :=> t) <- freshInst sc-}
                         {-unify t (foldr func t' ts)-}
                         {-return (ps ++ qs, as, t')-}

tiPats :: [Pat'] -> TI ([Pred], [Assump], [Type'])
tiPats ps = do ppat <- mapM tiPat' ps
               let ps = concat [ps' | (ps',_,_) <- ppat]
                   as = concat [as' | (_,as',_) <- ppat]
                   ts = [t | (_,_,t) <- ppat]
               return (ps, as, ts)

tiExpr :: Infer Expr Type'
tiExpr ce as (Lex p e) = tiExpr' ce as e

tiExpr' ce as (EVar i) = do
    sc <- find i as
    (ps :=> t) <- freshInst sc
    return (ps,t)
tiExpr' ce as (ELit l) = tiLit l
tiExpr' ce as (EAp e f) = do (ps, te) <- tiExpr ce as e
                             (qs, tf) <- tiArgs ce as f 
                             t <- newTVar Star
                             unify (tf `func` t) te
                             return (ps ++ qs, t) 

{-tiExpr' ce as (ELet bg ex') = do (ps, as') <- tiBindGroup ce as bg-}
                                 {-(qs, t) <- tiExpr ce (as' ++ as) ex'-}
                                 {-return (ps++qs, t)-}
tiExpr' ce as (EAbs argDecs ex) = undefined -- convert to let with fresh typeVar

tiExpr' ce as (ECase e bs) = undefined

tiAlt :: Infer Alt Type'
tiAlt ce as (pat, e) = do (ps, as', tp) <- tiPat pat
                          (qs, te) <- tiExpr ce (as' ++ as) e
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
                   
tiArgs :: Infer [Arg] Type'
tiArgs ce as args  = do (ps,ts) <- unzip <$> mapM argType args
                        return (concat ps, tProduct ts)
                        
    where argType (Lex _ (PArg e)) = tiExpr ce as e


defaultAssumps = mapM toAsmp
  [("+UN",   "Num",        unary)
  ,("-UN",   "Num",        unary)
  ,("+",     "Num",        binary)
  ,("-",     "Num",        binary)
  ,("/",     "Fractional", binary)
  ,("*",     "Num",        binary)
  ,("//",    "Integral",   binary)
  ,("%",     "Integral",   binary)
  ,("<",     "Ord",        binary)
  ,(">",     "Ord",        binary)
  ,(">=",    "Ord",        binary)
  ,("<=",    "Ord",        binary)
  ,("==",    "Eq",         binary)
  ,("and",   "",           bbool)
  ,("or",    "",           bbool)
  ,("xor",   "",           bbool)
  ,("not",   "",           ubool)
  {-,("print", "",           mkFun [tString] unit)-}
  ]
  where binary v = mkFun' [TVar v,TVar v] (TVar v)
        unary  v = mkFun' [TVar v] (TVar v)
        bbool _ = tBool `func` tBool `func` tBool
        ubool _ = tBool 
        bool = Tycon "Bool" Star
        unit = Tycon "()" Star
        toAsmp (i,"",f) = do v <- newTVar 
                             return $ i :>:  quantify [v] ([]:=> f v)
        toAsmp (i,q,f) = do v <- newTVar
                            return $ i :>:  quantify [v] ([IsIn q [TVar v]]:=> f v)


