module PreProcessor.Bindings 
    ( toBindGroup
    , splitBinds
    , splitImpl
    , bindExpr
    , bindName
    , flatten
    , scrubBinds
    ) 
    where

import Models.Program
import Data.List ((\\), nub)
import Data.Char (isLower)
import Data.Graph hiding (scc)
import Data.Graph.SCC

toBindGroup :: [Binding] -> [BindGroup]
toBindGroup = reverse . map splitImpl . depGroups []

splitImpl :: [Binding] -> BindGroup
splitImpl bs =
            let (es, is) = splitBinds bs
                eIds = map (\(i,_,_) -> i) es
                iGroups = depGroups eIds (map Impl is)
                iBinds = map (map (\(Impl tpl) -> tpl)) iGroups
            in (es, iBinds)
        

depGroups :: [Id] -> [Binding] -> [[Binding]]
depGroups ids bs = 
    let es = map (findDeps ids) bs
        (dGraph, mapping, _) = graphFromEdges es
        (comps, _) = scc dGraph
        verts = map snd comps
        nodeVal (b,_,_) = b
    in map (map (nodeVal . mapping)) verts
        
findDeps :: [Id] -> Binding -> (Binding, Id, [Id])
findDeps ids b@(Impl(i,e)) = (b, i, userVars e \\ (i:ids)) 
findDeps ids b@(Expl(i,_,e)) = (b, i, userVars e \\ (i:ids)) 

userVars :: Expr -> [Id]
userVars (Var i@(c:_)) 
    | isLower c = [i | i /= "print"]
    | otherwise = []
userVars (Abs (ps,e)) = userVars e \\ nub (concatMap pvar ps)
userVars (Ap e1 e2) = nub $ userVars e1 ++ userVars e2
userVars (Let bs e) = let ids = map bindName bs
                          es  = map bindExpr bs
                      in nub (userVars e ++ concatMap userVars es) \\ ids
userVars (Case e bs) = nub $ userVars e ++ concatMap (userVars . Abs) bs
userVars (Annot e _) = userVars e
userVars _ = []
-- overloaded functions don't really count as user vars for our purposes

pvar :: Pat -> [Id]
pvar (PVar v) = [v]
pvar (PAs v p) = v : pvar p
pvar (PCons _ ps) = nub $ concatMap pvar ps
pvar _ = []
                           
splitBinds :: [Binding] -> ([Expl], [Impl])
splitBinds = foldl f ([],[]) 
    where  f (es, is) (Expl e) = (e:es,is) 
           f (es, is) (Impl i) = (es,i:is)

bindName :: Binding -> Id
bindName (Expl (i,_,_)) = i
bindName (Impl (i,_)) = i
    
bindExpr :: Binding -> Expr
bindExpr (Impl (_,e)) = e
bindExpr (Expl (_,_,e)) = e

scrubBinds :: Binding -> (Id, Expr)
scrubBinds (Expl (i,_,e)) = (i,e)
scrubBinds (Impl (i,e)) = (i,e)

flatten :: [BindGroup] -> [Binding]
flatten = concatMap flatten' 
    where flatten' (es, is) =  map Expl es ++ map Impl (concat is)
