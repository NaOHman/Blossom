{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Models.Program 
    ( module X
    {-, Scope (..)-}
    {-, FArgs (..)-}
    {-, FDec (..)-}
    {-, Value(..)-}
    , ClassEnv(..)
    , Top'(..)
    , Top(..)
    , DataMap(..)
    , Program(..)
    , defPrg
    ) where

import Models.Expressions as X
import Models.Core as X
import Models.Data as X
import Text.Megaparsec.Pos
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Top = Lex Top'
data Top' = Over Id Expr
          | Dta  Data
          | CDec ClassDec
          | Let Id Expr
          | Inst Instance
    deriving Show

data Program = Program
    { assumps  :: [Assump]
    , bindings :: [BindGroup]
    , mainF    :: Expr'
    , dataTs   :: [Id]
    , cEnv     :: ClassEnv
    }
    deriving Show

defPrg = Program [] [] EUnit [] M.empty

type ClassEnv = M.Map Id Class

defClasses = M.fromList 
 [("Eq",        ([],[]))
 ,("Ord",       (["Eq"],[]))
 ,("Num",       (["Eq", "Showable"],[]))
 ,("Enum",      (["Eq", "Showable"],[]))
 ,("Real",      (["Num", "Ord"],[]))
 ,("Fractional",(["Num", "Ord"],[]))
 ,("Integral",  (["Real", "Enum"],[]))
 ,("RealFrac",  (["Real","Fractional"],[]))
 ,("Floating",  (["Fractional"],[]))
 ,("RealFloat", (["RealFrac", "Floating"],[""]))
 ,("Readable",  ([],[]))
 ]

{-data Scope = Scope-}
    {-{ global :: M.Map String Value-}
    {-, local  :: M.Map String Value-}
    {-, constr :: M.Map String (M.Map String Int)-}
    {-} -}
 
{-data FArgs = FArgs-}
    {-{ pArg :: [(Id, Type)]-}
    {-, kArg :: [(Id, Literal, Type)]-}
    {-}-}

data DataMap = DataMap 
    { dta :: M.Map Id Type
    , cnstrs :: M.Map Id [Type]
    , ambig :: S.Set Id
    }
 
defDMap = DataMap M.empty M.empty S.empty
{-data FDec = FDec-}
    {-{ fpos :: SourcePos-}
    {-, fName :: Maybe String-}
    {-, args :: [ArgDec]-}
    {-, returnType :: Type-}
    {-} deriving Show-}
