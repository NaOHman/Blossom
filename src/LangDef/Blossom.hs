module LangDef.Blossom  where

import Models.Types
import Types.Utils
import Models.Expressions
import qualified Data.Map as M

nTup n =  "(" ++ replicate (n - 1) ',' ++ ")"
patNil =  PCons "[nil]" []
patCons = PCons "[cons]"

----------- Types --------------------
tChar = tcons "Char" Star
tInt = tcons "Int" Star
tFloat = tcons "Float" Star
tBool = tcons "Bool" Star
tNull = tcons "Null" Star
tUnit = tcons "()" Star

tList = TAp (tcons "List" (KFun Star Star))
tString = tList tChar

tplName n = "(" ++ replicate (n-1) ',' ++ ")"
tcons n k = TCons $ Tycon n k

tTuple ts = 
    let len = length ts
        name = tplName len 
        ks = kAry len
        tc = tcons name ks
    in foldl TAp tc ts

kAry 0 = Star
kAry n = KFun Star (kAry (n-1))

--------- Behaviors -----------------
classes = M.fromList $ map mkCls 
    [("Eq", [], [tInt, tChar, tFloat, tBool])
    ,("Showable", [], [tInt, tChar, tFloat, tBool, tString])
    ,("Ord", ["Eq"], [tInt, tChar, tFloat])
    ,("Num", ["Eq","Show"], [tInt, tFloat])
    ,("Real", ["Num","Ord"], [tInt, tFloat])
    ,("Fractional", ["Num"], [tFloat])
    ,("Floating", ["Fractional"], [tFloat])]
mkCls (id,ss,is) = (id,(ss, insts, stubs))
    where insts = map (\t -> [] :=> IsIn id [t]) is
          stubs = []

-------- Assumptions -----------------
--
blossomAssumps = [
   "+UN"    :>: unary "Num"
  ,"-UN"    :>: unary "Num"
  ,"+"      :>: binary "Num"
  ,"-"      :>: binary "Num"
  ,"/"      :>: binary "Fractional"
  ,"*"      :>: binary "Num"
  ,"//"     :>: binary "Integral"
  ,"%"      :>: binary "Integral"
  ,"<"      :>: relational "Ord"
  ,">"      :>: relational "Ord"
  ,">="     :>: relational "Ord"
  ,"<="     :>: relational "Ord"
  ,"=="     :>: relational "Eq"
  ,"and"    :>: bbool
  ,"or"     :>: bbool
  ,"xor"    :>: bbool
  ,"not"    :>: ubool
  ,"!seq"   :>: Forall [Star,Star] ([] :=> mkFun [g0, g1] g1)
  ,"print"  :>: sc1 ([IsIn "Showable" [g0]] :=> func g0 tNull)
  ,"[nil]"  :>:sc1 ([] :=> tList g0)
  ,"[cons]" :>: sc1 ([] :=> mkFun [g0, tList g0] (tList g0))
  ] ++ tupleAssumps
  where binary q = sc1 ([IsIn q [g0]] :=> mkFun [g0,g0] g0)
        relational q = sc1 $ [] :=> mkFun [g0,g0] tBool
        unary q = sc1 $ [IsIn q [g0]] :=> func g0 g0
        bbool = Forall [] ([]:=> mkFun [tBool,tBool] tBool)
        ubool = Forall [] ([] :=> func tBool tBool)
        g0 = TGen 0
        g1 = TGen 1
        sc1 = Forall [Star]

tupleAssumps = map tplAsmp [2..20]
    where tplAsmp n = 
            let ks = replicate n Star
                ts = map TGen [0..n]
                name = tplName n
            in (name :>: Forall ks ([] :=> tTuple ts))
