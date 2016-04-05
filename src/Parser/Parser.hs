{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TupleSections #-}

module Parser.Parser where

import Parser.Core
import Parser.Exprs
import Types.Utils
import Models.Program
import Text.Megaparsec
import Models.Expressions
import Parser.Constraints
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (void, foldM, ap, liftM)
import Control.Monad.State (evalStateT)

--TODO quantify and qualify Schemes for everything.

{-parseBlossom :: BParser ([Expr], [Instance], [ClassDec], [Data])-}
{-parseBlossom = re ([],[],[],[])-}
    {-where go prg = parseTop prg >>= re-}
          {-re prg = do done <- optional eof-}
                      {-case done of-}
                          {-Just _ -> return prg-}
                          {-otherwise -> go prg-}

{-parseTop :: BParser Top-}
{-parseTop (es,is,cs,ds) = withPos $ tryList [over, dataDec, tlet, insDec, classDec]-}

gVar = do 
    name <- uName
    equals' 
    e <- expr
    return (name, e, undefined)
  
fDec = do
    f <- funName 
    e <- eAbs
    return (f, e, undefined)

adt = do
    newType <- data_ *> undefined <* where_
    cstrs <- block cStub
    return $ ADT newType (toBinding newType cstrs)
    where cStub = (,) <$> uName <*> optional (parenCsl ttype)
          toBinding rt ats = undefined

rec = do
    preds <- undefined
    newType <- data_ *> dataCons 
    superType <- optional $ inherits *> dataCons 
    fields <- block field 
    let recSch = quantQual preds newType
        supSch = undefined
        bindings = map (toBinding recSch) (zip fields [0..])
    return $ Rec recSch supSch bindings
    where field = (,) <$> lName <*> sufCons
          toBinding rt ((id,t), n) = undefined

behavior = do 
    sch <- undefined
    name <- is' *> uName <* when'
    stubs <- block $ undefined
    return $ Bhvr sch name stubs

implem = do
    sch <- undefined
    bhvr <- is' *> uName <* because'
    bindings <- block fDec
    return $ Imp sch bhvr bindings

{-[>addClass :: ClassEnv -> ClassDec' -> ClassEnv<]-}
{-[>addClass ce (ClassDec' _ n _) = M.insert n ([],[]) ce<]-}

{-[>addDName :: Id -> Program -> Program<]-}
{-[>addDName i (Program a b m d c) = Program a b m (i:d) c<]-}

{-[>addInst :: ClassEnv -> Id -> Inst -> ClassEnv<]-}
{-[>addInst ce c i = M.adjust f c ce<]-}
    {-[>where f (s,is) = (s, i: is)<]-}

{-[>findClass :: ClassDec' -> ClassEnv -> Maybe Class<]-}
{-[>findClass c = M.lookup (cname c)<]-}

{-[>funcScheme :: Maybe Type -> [ArgDec] -> Scheme<]-}
{-[>funcScheme Nothing = funcScheme' (TVar $ Tyvar "!rt" Star)<]-}
{-[>funcScheme (Just t) = funcScheme' (unwrap t)<]-}

{-[>funcScheme' :: Type' -> [ArgDec] -> Scheme<]-}
{-[>funcScheme' rt as = let vs  = map toTV as<]-}
                        {-[>ts  = map TVar vs<]-}
                   {-[>in quantify (vs ++ tv rt) ([] :=> mkFun' ts rt)<]-}
    {-[>where toTV a = Tyvar (nameOf a) Star<]-}


{-[>--TODO handle explicit types<]-}
{-[>insert :: Monad m => Program -> Top' -> m Program<]-}
{-[>insert prg (Over n (Lex pos (EAbs as ex))) = <]-}
   {-[>let sch = funcScheme Nothing as<]-}
   {-[>in addExplicit n sch ex prg<]-}
{-[>insert prg (Dta (Lex p d))<]-}
    {-[>| dname d `elem` dataTs prg = fail "Duplicate datatype"<]-}
    {-[>| otherwise = addDName (dname d) <$> <]-}
                  {-[>addAssumps prg (dataAssumps d)<]-}
        {-[>--add function bindings<]-}
    {-[>where cs = map fst (constructors d) ++ map fst (fields d)<]-}
{-[>-- Add bindings<]-}
{-[>insert prg (CDec c@(Lex _ (ClassDec' v n ss))) = do<]-}
    {-[>modifyCE (withLex newClass c) prg<]-}
    {-[>addAssumps prg (return $ map stubAssump ss)<]-}
    {-[>where stubAssump (id, t) = id :>: quantify (tv v) (qt t)<]-}
          {-[>qt (Lex _ t) = [IsIn n (map TVar $ tv v)] :=> t<]-}
{-[>insert prg (Inst i) = modifyCE (withLex newInst i) prg<]-}
{-[>-- add function to bindings<]-}
{-[>insert prg (Let i e) = addImplicit i e prg<]-}


{-[>isBound n = any p <]-}
    {-[>where p (expl,impl) = any (\(i,_)   -> n == i) impl ||<]-}
                          {-[>any (\(i,_,_) -> n == i) expl<]-}
 
{-[>addImplicit :: Monad m => Id -> Expr -> Program -> m Program<]-}
{-[>addImplicit i e (Program as bs m ds ce)<]-}
    {-[>| isBound i bs = fail "Duplicate Binding"<]-}
    {-[>| otherwise = return $ <]-}
            {-[>Program as (([],[(i,e)]):bs) m ds ce<]-}
            
{-[>addExplicit :: Monad m => Id -> Scheme -> Expr -> Program -> m Program<]-}
{-[>addExplicit i s a (Program as bs m ds ce)<]-}
    {-[>| isBound i bs = fail "Duplicate Binding"<]-}
    {-[>| otherwise = return $ <]-}
            {-[>Program as (([(i,s,a)], []):bs) m ds ce<]-}

{-[>addAssumps :: Monad m => Program -> m [Assump] -> m Program<]-}
{-[>addAssumps (Program a b m d c) ma = ma >>= (\as -> return $ Program (as++a) b m d c)<]-}
{-[>modifyCE :: Monad m => (ClassEnv -> m ClassEnv) -> Program -> m Program<]-}
{-[>modifyCE f p@(Program as bs m d ce) = Program as bs m d <$> f ce <]-}

{-[>newClass p c ce = case findClass c ce of<]-}
    {-[>Just _ -> fail $ show p ++ "Class already defined"<]-}
    {-[>Nothing -> return $ addClass ce c<]-}

{-[>newInst pos (Instance' t i _) ce <]-}
    {-[>| not (M.member i ce) = fail $ show pos ++ "Class not defined"<]-}
    {-[>| any (overlap p) qs  = fail $ show pos ++ "overlapping instance"<]-}
    {-[>| otherwise           = return $ M.adjust f i ce <]-}
    {-[>where its = insts ce i<]-}
          {-[>ps = []<]-}
          {-[>p = IsIn i [unwrap t]<]-}
          {-[>qs = [q | (_:=>q) <- its]<]-}
          {-[>f (s,is) = (s, (ps :=> p) : its)<]-}
          {-[>overlap p q = defined (mguPred p q)<]-}

{-[>classAssumps :: Monad m => ClassDec' -> m [Assump]<]-}
{-[>classAssumps (ClassDec' t c ss) = do<]-}
    {-[>tvs <- checkTV $ tv ts<]-}
    {-[>return $ map (f tvs) ss <]-}
    {-[>where ts = t : map snd ss<]-}
          {-[>f vs (n, Lex _ t') = n :>: quantify vs (qt c t')<]-}
          {-[>qt c t' = [IsIn c [unwrap t]] :=> t'<]-}

{-[>checkTV :: Monad m => [Tyvar] -> m [Tyvar]<]-}
{-[>checkTV = foldM tryInsert []<]-}

{-[>quantAssump i t = i :>: quantify (tv t) ([]:=>t)<]-}
{-[>-- TODO Occurs check<]-}
{-[>dataAssumps :: Monad m => Data' -> m [Assump]<]-}
{-[>dataAssumps (Data' t cs []) = return $ map consAssump cs<]-}
    {-[>where consAssump (i,Just t') = quantAssump i (t' `func` unwrap t)<]-}
          {-[>consAssump (i,_) = quantAssump i (unwrap t)<]-}
{-[>dataAssumps (Data' t [] rs) = return $ map fieldAssump rs<]-}
    {-[>where fieldAssump (i,t') = quantAssump i (unwrap t `func` unwrap t')<]-}
{-[>dataAssumps _ = fail "Something went horribly wrong"<]-}

{-[>dataAssumps' bs f = return $ map f' bs <]-}
    {-[>where f' (i,t) = let ft = f (unwrap t)<]-}
                         {-[>vs = tv ft<]-}
                     {-[>in (i:>: quantify vs ([]:=>ft))<]-}
    {-[>[>sch = f <]<]-}
    {-[>[>vs <- checkTV $ concatMap (tv . snd) fns<]<]-}
    {-[>[>let t' = tcons n (kAry (length ps))<]<]-}
        {-[>[>sc = quantify vs ([]:=>t')<]<]-}
        {-[>[>as = map (\(id,ts) -> assume vs id (f ts t')) fns<]<]-}
    {-[>[>return as<]<]-}
    
{-[>tryInsert :: Monad m => [Tyvar] -> Tyvar -> m [Tyvar]<]-}
{-[>tryInsert m t@(Tyvar n k) = case L.find shareName m of<]-}
    {-[>Just (Tyvar _ k') -> if k == k' then return m else fail $ "Kinds for variable " ++ n ++ " are not the same"<]-}
    {-[>otherwise -> return (t:m)<]-}
    {-[>where shareName (Tyvar n' _) = n == n'<]-}
