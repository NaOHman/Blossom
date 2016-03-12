{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Models.Expressions where

import Text.Megaparsec.Pos
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Id = String
type FnMap = M.Map Id (FArgs,Expr)

data Statement = SEx Expr | SDa Data | SCl Class | SIn Instance

data Value = VInt Integer
           | VFloat Double
           | VChar Char
           | VLambda [ArgDec] Expr
           | VCons String [Value]
           | VNull

data Scope = Scope
    { global :: M.Map String Value
    , local  :: M.Map String Value
    , constr :: M.Map String (M.Map String Int)
    } 
 
data FArgs = FArgs
    { faPos :: [(Id, Constraint)]
    , faKW :: [(Id, Literal, Constraint)]
    , faPS :: Maybe (Id, Constraint)
    , faKS :: Maybe (Id, Constraint) }

data Program = Program 
    { globals   :: [Expr]
    , datatypes :: [Data]
    , classes   :: [Class] 
    , instances :: [Instance] 
    } deriving Show
    -- todo type aliases and error types

data DataMap = DataMap 
    { dta :: M.Map Id [Id]
    , cnstrs :: M.Map Id CInf
    , ambig :: S.Set Id
    }
 
data CInf = CInf 
    { cType :: Id
    , fields :: M.Map Id Integer
    }

data Constraint where
    None :: Constraint
    FunCons :: [Constraint] -> Constraint -> Constraint
    ValCons :: Id -> [Constraint] -> Constraint
    deriving Show

data Data = Data
    { dpos :: SourcePos
    , dName :: Constraint
    , constructors :: [(Id, [ArgDec])]
    } deriving Show

data Class = Class
    { cpos :: SourcePos
    , cVar :: Constraint
    , cname :: Id 
    , cstubs :: [(Id, [ArgDec], Constraint)]
    } deriving Show

data Instance = Instance 
    { ipos :: SourcePos
    , icons :: Constraint
    , instncOf :: Id
    , ifuns :: [Expr]
    } deriving Show

data Pat where
    DName  :: Id -> Pat
    DCons  :: Id -> [Pat] -> Pat
    DMatch :: Literal -> Pat
    DNil   :: Pat
    deriving Show

data Then = Then Expr Expr
data Elif = Elif Expr Expr
data Else = Else Expr 

data Expr = Expr SourcePos Expr'
    deriving Show

data Expr' where
    ELit    :: Literal -> Expr'
    EVar    :: [Id] -> Expr'
    ELambda :: [ArgDec] -> Expr -> Expr'
    EFCall  :: Id -> [Arg] -> Expr'
    EFix    :: Id -> [ArgDec] -> Expr -> Expr'
    ELet    :: Pat -> Expr -> Expr' -- EChain ~= in
    ECase   :: Expr -> [(Pat, Expr)] -> Expr'
    EChain  :: Expr -> Expr -> Expr'
    deriving Show

data Arg = Arg SourcePos Arg'
    deriving Show

data Arg' where
    PosArg  :: Expr -> Arg'
    KWArg   :: Id -> Expr -> Arg'
    PSplat  :: Expr -> Arg'
    KWSplat :: Expr -> Arg'
    deriving Show

data FDec = FDec
    { fpos :: SourcePos
    , fName :: Maybe String
    , args :: [ArgDec]
    , returnType :: Constraint
    } deriving Show

-- TODO FIX ARGS
data ArgDec = ArgDec SourcePos ArgDec'
    deriving Show

data ArgDec' where
    PosDec :: Id -> Constraint -> ArgDec'
    KWDec  :: Id -> Literal -> Constraint -> ArgDec'
    PSDec  :: Id -> Constraint -> ArgDec'
    KWSDec :: Id -> Constraint -> ArgDec'
    deriving Show

data Literal = Literal SourcePos Literal'
    deriving Show

data Literal' where
    LChar   :: Char -> Literal'
    LString :: String -> Literal'
    LInt    :: Integer -> Literal'
    LFloat  :: Double -> Literal'
    {-LDict   :: [(Expr,Expr)]  -> Literal'-}
    {-LSet    :: [Expr] -> Literal'-}
    LCons   :: Id -> [Expr] -> Literal'
    LType   :: Id -> Literal'
    LNull   :: Literal'
    deriving Show
