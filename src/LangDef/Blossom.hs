module LangDef.Blossom  where

import Models.Types
import Models.Program
import qualified Data.Map as M

patNil :: Pat
patNil =  PCons "[nil]" []

patCons :: [Pat] -> Pat
patCons = PCons "[cons]"

----------- Types --------------------
tChar :: Type
tChar = tcons "Char" Star

tArrow :: Type
tArrow = TCons $ Tycon "->" (KFun Star (KFun Star Star))

tInt :: Type
tInt = tcons "Int" Star

tFloat :: Type
tFloat = tcons "Float" Star

tBool :: Type
tBool = tcons "Bool" Star

tNull :: Type
tNull = tcons "Null" Star

tUnit :: Type
tUnit = tcons "()" Star

tList :: Type -> Type
tList = TAp (tcons "List" (KFun Star Star))

tString :: Type
tString = tList tChar

tplName :: Int -> String 
tplName n = "(" ++ replicate (n-1) ',' ++ ")"

tcons :: Id -> Kind -> Type
tcons n k = TCons $ Tycon n k

tTuple :: [Type] -> Type
tTuple ts = 
    let len = length ts
        name = tplName len 
        ks = kAry len
        tc = tcons name ks
    in foldl TAp tc ts

kAry :: Int -> Kind
kAry 0 = Star
kAry n = KFun Star (kAry (n-1))

--------- Behaviors -----------------
classes :: ClassEnv
classes = M.fromList $ map mkCls 
    [("Eq", [], [tInt, tChar, tFloat, tBool])
    ,("Showable", [], [tInt, tChar, tFloat, tBool, tString])
    ,("Ord", ["Eq"], [tInt, tChar, tFloat])
    ,("Num", ["Eq","Show"], [tInt, tFloat])
    ,("Real", ["Num","Ord"], [tInt, tFloat])
    ,("Fractional", ["Num"], [tFloat])
    ,("Floating", ["Fractional"], [tFloat])]

mkCls :: (Id, [Id], [Type]) -> (Id, ([Id], [Qual Pred], [Stub]))
mkCls (i,ss,is) = (i, (ss, ints, stubs))
    where ints = map (\t -> [] :=> IsIn i [t]) is
          stubs = []

-------- Assumptions -----------------

blossomAssumps :: [Assump]
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
        relational q = sc1 $ [IsIn q [g0]] :=> mkFun [g0,g0] tBool
        unary q = sc1 $ [IsIn q [g0]] :=> func g0 g0
        bbool = Forall [] ([]:=> mkFun [tBool,tBool] tBool)
        ubool = Forall [] ([] :=> func tBool tBool)
        g0 = TGen 0
        g1 = TGen 1
        sc1 = Forall [Star]

tupleAssumps :: [Assump]
tupleAssumps = map tplAsmp [2..20]
    where tplAsmp n = 
            let ks = replicate n Star
                ts = map TGen [0..n]
                name = tplName n
            in (name :>: Forall ks ([] :=> tTuple ts))

func :: Type -> Type -> Type
a `func` b = TAp (TAp tArrow a) b

mkFun :: [Type] -> Type -> Type
mkFun ts t = foldr func t ts
