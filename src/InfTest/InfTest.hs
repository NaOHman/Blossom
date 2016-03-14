import Types.Inference
import Types.Utils
import Models.Expressions
import qualified Data.Map as M
import Data.List (intercalate)

main = do 
          {-print $ runTI $ tiPat' defAs asPat-}
          {-print $ runTI $ tiPat' defAs pCons-}
          {-print $ runTI $ tiPat' defAs pLit-}
          {-print $ runTI $ tiPat' defAs pVar-}
          {-print $ runTI $ tiPat' defAs pNil-}
          {-print defAs-}
          prettyPrint $ defTI tiExpr' expr0
          prettyPrint $ defTI tiExpr' expr1

exId = Abs idPat (Var "x")
idPat = prod [PVar "x"]
expr0 = Ap 
expr1 = Ap lambda aargs
    where aargs = Lit (prod [LChar 'a', LChar 'b', LChar 'c'])
          lambda = Abs p ex
          p = prod [PVar "x", PVar "y", PVar "z"]
          ex = Var "and" `app` [
                 Var "==" `app` [
                    Var "x",
                    Var "y"],
                 Var "==" `app` [
                    Var "y",
                    Var "z"]]

expr2 = Ap lambda aargs
    where aargs = Lit (prod [LInt 3, LInt 5])
          lambda = Abs p ex
          p = prod [PVar "x", PVar "y"]
          ex = Var "==" `app` [
                  Var "y", 
                  Var "+" `app` [
                     Var "x", 
                     Lit (LInt 1)]]
           

f `app` as = Ap f (prod as)

asPat = PAs "a" (PCons "Cons" [PVar "b", PCons "Nil" []])
pCons = PCons "Cons" [PVar "a", PVar "b"]
pLit = PLit (LChar 'c')
pVar = PVar "a"
pNil = PNil

defTI inf e = runTI $ do
        (ps,t) <- inf defCE defAs e
        s <- getSubst
        return (apply s t)

lst' = Tycon "List" (KFun Star Star) 
listOf = TAp (TCons lst')

defCE = M.fromList 
    [("Eq", ([], [[] :=> IsIn "Eq" [tFloat]
                 ,[] :=> IsIn "Eq" [tInt]
                 ,[] :=> IsIn "Eq" [tChar]
                 ,[] :=> IsIn "Eq" [tBool]
                 ,[IsIn "Eq" [v]] :=> IsIn "Eq" [listOf v]]))
    ,("Ord", ([], [[] :=> IsIn "Ord" [tInt]
                  ,[] :=> IsIn "Ord" [tFloat]
                  ,[] :=> IsIn "Ord" [tChar]
                  ,[IsIn "Ord" [v]] :=> IsIn "Ord" [listOf v]]))
    ,("Num", (["Ord"], [[] :=> IsIn "Num" [tInt]
                       ,[] :=> IsIn "Num" [tFloat]]))
    ,("Integral", (["Num"], [[] :=> IsIn "Integral" [tInt]]))
    ,("Fractional", (["Num"], [[] :=> IsIn "Fractional" [tFloat]]))
    ]


v  = TVar v'
v' = Tyvar "a" Star

defAs = lstAssumps ++ defaultAssumps ++ prodAssumps

plus = Var "+"
eqs = Var "=="
minus = Var "-"
times = Var "*"

lstAssumps = 
    ["Cons" :>: quantify [v'] ([] :=> ([v, listOf v] `pfunc` v))
    ,"Nil"  :>: quantify [v'] ([] :=> listOf v)]

as `pfunc` a = prod as `func` a

prodAssumps = map f [0..3]
    where f i = cname i :>: prodScheme i

cname i = show i ++ "PROD"

prodScheme i = Forall ks ([] :=> fn)
    where ks = replicate i Star
          fn = foldr func (prod vs) vs
          vs = [TGen x | x <- [0..i-1]]

defaultAssumps = map toAsmp [
   ("+UN",   "Num",        unary)
  ,("-UN",   "Num",        unary)
  ,("+",     "Num",        binary)
  ,("-",     "Num",        binary)
  ,("/",     "Fractional", binary)
  ,("*",     "Num",        binary)
  ,("//",    "Integral",   binary)
  ,("%",     "Integral",   binary)
  ,("<",     "Ord",        b2Bool)
  ,(">",     "Ord",        b2Bool)
  ,(">=",    "Ord",        b2Bool)
  ,("<=",    "Ord",        b2Bool)
  ,("==",    "Eq",         b2Bool)
  ,("and",   "",           bbool)
  ,("or",    "",           bbool)
  ,("xor",   "",           bbool)
  ,("not",   "",           ubool)
  ,("print", "",           [tString] `pfunc` tBool)
  ]
  where binary = [v, v] `pfunc` v
        b2Bool = [v ,v] `pfunc` tBool
        unary  = [v] `pfunc` v
        bbool = [tBool, tBool] `pfunc` tBool
        ubool = [tBool] `pfunc` tBool
        toAsmp (i,"",t) = i :>: quantify [v'] ([]:=> t)
        toAsmp (i,q,t)  = i :>: quantify [v'] ([IsIn q [v]] :=> t)

prettyPrint = print . pretty
pretty t@(TAp{}) = case bottom t [] of
                           ("->", [t1,t2]) -> pretty t1 ++ "->" ++ pretty t2 
                           (n,  ts) -> n ++ "<" ++ intercalate ","  (map pretty ts) ++ ">"
    where bottom (TAp t1 t2) ts = bottom t1 (t2:ts)
          bottom (TCons (Tycon i _)) ts = (i, ts)
pretty (TCons (Tycon i _)) = i
pretty (TVar (Tyvar i _)) = i
pretty (TGen i) = "Gen" ++ show i
