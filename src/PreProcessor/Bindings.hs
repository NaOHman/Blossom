module PreProcessor.Bindings 
    ( toBindGroup
    , splitBinds
    , flatten
    ) 
    where

import Language.Expressions
import Language.Bindings
import Data.List ((\\), delete, nub)
import Data.Char (isLower)
import Data.Graph hiding (scc)
import Data.Graph.SCC

toBindGroup :: [Binding] -> BindGroup
toBindGroup bs = let (es, is) = splitBinds bs
                     eIds = map name es
                     iBinds = depGroups eIds is
                 in (es, iBinds)

-- This function splits a list of bindings into dependency groups, which
-- are groups of functions that are mutually recursive. 
--
-- We map out the dependencies of function as a directed graph where 
-- each node is a function, and each edge leaving a node points to
-- a function that the first function depends on.
--
-- The stongly connected componets of that graph are our dependency
-- groups
depGroups :: [Id] -> [Impl] -> [[Impl]]
depGroups ids bs = 
    let es = map (findDeps ids) bs
        (dGraph, mapping, _) = graphFromEdges es
        (comps, _) = scc dGraph
        verts = map snd comps
        nodeVal (b,_,_) = b
    in map (map (nodeVal . mapping)) verts
        
findDeps :: [Id] -> Impl -> (Impl, Id, [Id])
findDeps ids b = let i = name b
                 in (b, i, userVars (expr b) \\ (i:ids))

userVars :: Expr -> [Id]
userVars (Var i@(c:_)) 
    | isLower c = [i | i /= "print"] -- fix builtins
    | otherwise = []
userVars (Abs ps e) = userVars e \\ nub (concatMap pvar ps)
userVars (Ap e1 e2) = nub $ userVars e1 ++ userVars e2
userVars (Let i e1 e2) = delete i $ nub (userVars e1 ++ userVars e2)
userVars (LetRec i e1 e2) = delete i $ nub (userVars e1 ++ userVars e2)
userVars (Case e bs) = nub $ userVars e ++ concatMap (\(p,e') ->  userVars e' \\ nub (pvar p)) bs
userVars (Annot (e:-:_)) = userVars e
userVars _ = []
-- overloaded functions don't really count as user vars for our purposes

pvar :: Pat -> [Id]
pvar (PVar v) = [v]
pvar (PAs v p) = v : pvar p
pvar (PCons _ ps) = nub $ concatMap pvar ps
pvar _ = []
                           
splitBinds :: [Binding] -> ([Expl], [Impl])
splitBinds = foldl f ([],[]) 
    where  f (es, is) (ExpB e) = (e:es, is) 
           f (es, is) (ImpB i) = (es,i:is)

flatten :: [BindGroup] -> [Binding]
flatten = concatMap (\(es, is) -> map ExpB es ++ map ImpB (concat is))
