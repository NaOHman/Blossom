module PreProcessor.Bindings 
    ( toBindGroup
    , splitBinds
    , splitImpl
    , bindExpr
    , bindName
    ) 
    where

import Models.Program
import Data.List (find, sort, delete, (\\), nub, partition)
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
    let edges = map (findDeps ids) bs
        (dGraph, mapping, _) = graphFromEdges edges
        (components, _) = scc dGraph
        vertices = map snd components
        nodeVal (b,_,_) = b
    in map (map (nodeVal . mapping)) vertices
        
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

pvar (PVar v) = [v]
pvar (PAs v p) = v : pvar p
pvar (PCons _ ps) = nub $ concatMap pvar ps
pvar _ = []
                           
splitBinds = foldl f ([],[]) 
    where  f (es, is) (Expl e) = (e:es,is) 
           f (es, is) (Impl i) = (es,i:is)

bindName (Expl (i,_,_)) = i
bindName (Impl (i,_)) = i
    
bindExpr :: Binding -> Expr
bindExpr (Impl (_,e)) = e
bindExpr (Expl (_,_,e)) = e
