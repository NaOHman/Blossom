module PreProcessor.Bindings 
    ( toBindGroup
    , splitBinds
    , splitImpl
    , bindExpr
    , bindName
    , flatten
    ) 
    where

import Language.Expressions
import Data.List ((\\), nub)
import Data.Char (isLower)
import Data.Graph hiding (scc)
import Data.Graph.SCC

toBindGroup :: [Bind] -> [BindGroup]
toBindGroup = reverse . map splitImpl . depGroups []

splitImpl :: [Bind] -> BindGroup
splitImpl bs =
            let (es, is) = splitBinds bs
                eIds = map bindName es
                iBinds = depGroups eIds is
                {-iBinds = map (map (\(Impl tpl) -> tpl)) iGroups-}
            in (es, iBinds)
        

depGroups :: [Id] -> [Bind] -> [[Bind]]
depGroups ids bs = 
    let es = map (findDeps ids) bs
        (dGraph, mapping, _) = graphFromEdges es
        (comps, _) = scc dGraph
        verts = map snd comps
        nodeVal (b,_,_) = b
    in map (map (nodeVal . mapping)) verts
        
findDeps :: [Id] -> Bind -> (Bind, Id, [Id])
findDeps ids b = let i = bindName b
                 in (b, i, userVars (bindExpr b) \\ (i:ids))

userVars :: Expr -> [Id]
userVars (Var i@(c:_)) 
    | isLower c = [i | i /= "print"]
    | otherwise = []
userVars (Abs ps e) = userVars e \\ nub (concatMap pvar ps)
userVars (Ap e1 e2) = nub $ userVars e1 ++ userVars e2
userVars (Let bs e) = let ids = map bindName bs
                          es  = map bindExpr bs
                      in nub (userVars e ++ concatMap userVars es) \\ ids
userVars (Case e bs) = nub $ userVars e ++ concatMap (\(p,e') ->  userVars e' \\ nub (pvar p)) bs
userVars (Annot _ e) = userVars e
userVars _ = []
-- overloaded functions don't really count as user vars for our purposes

pvar :: Pat -> [Id]
pvar (PVar v) = [v]
pvar (PAs v p) = v : pvar p
pvar (PCons _ ps) = nub $ concatMap pvar ps
pvar _ = []
                           
splitBinds :: [Bind] -> ([Bind], [Bind])
splitBinds = foldl f ([],[]) 
    where  f (es, is) e@(Bind _ (Annot _ _)) = (e:es,is) 
           f (es, is) i = (es,i:is)

bindName :: Bind -> Id
bindName (Bind i _) = i

bindExpr :: Bind -> Expr
bindExpr (Bind _ e) = e

flatten :: [BindGroup] -> [Bind]
flatten = concatMap flatten' 
    where flatten' (es, is) =  es ++ (concat is)
