import Types.Inference
import Types.Utils
import Models.Expressions
import qualified Data.Map as M
import Data.List (intercalate)

main = do 
          print $ defTI tiExpr' expr0
          print $ defTI tiExpr' expr1
          print $ defTI tiExpr' expr2

expr0 = Ap (Var "+") (prod $ map Lit [LInt 1, LInt 3])
exId = Abs idPat (Var "x")
idPat = prod [PVar "x"]
pat1 = prod [PVar "x", PVar "y", PVar "z"]
expr1 = Ap (Abs pat1 ex) aargs

aargs = prod $ map Lit [LChar 'a', LChar 'b', LChar '4']

ex = Var "and" `app` [
     Var "==" `app` [
        Var "x",
        Var "y"],
     Var "==" `app` [
        Var "y",
        Var "z"]]

expr2 = Ap lambda aargs
    where aargs = prod $ map Lit [LInt 3, LInt 4]
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
        rs <- reduceCtx defCE (apply s ps)
        {-if null (ambiguities (tv ))-}
        {-let ps' = apply s ps-}
        return (apply s t, rs, s)

defSubs inf e = runTI $ do
        (ps,t) <- inf defCE defAs e
        getSubst
        
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
