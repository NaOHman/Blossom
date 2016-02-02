{-# LANGUAGE GADTs, NoMonomorphismRestriction, FlexibleContexts #-}

module Models where

import Text.Megaparsec
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

type Id = String

data Statement = SEx Expr | SDa Data | SCl Class | SIn Instance

data Program = Program 
    { globals   :: [Expr]
    , datatypes :: [Data]
    , classes   :: [Class] 
    , instances :: [Instance] 
    } deriving Show
    -- todo type aliases and error types

data Constraint where
    None :: Constraint
    FunCons :: [Constraint] -> Constraint -> Constraint
    ValCons :: Id -> [Constraint] -> Constraint
    deriving Show

data Data = Data
    { dpos :: SourcePos
    , dName :: DecCons
    , constructors :: [(Id, [ArgDec])]
    } deriving Show

data DecCons = DecCons 
    { decName :: Id
    , dParams :: [Constraint]
    } deriving Show

data Class = Class
    { cpos :: SourcePos
    , cVar :: DecCons
    , cname :: Id 
    , cstubs :: [(Id, [ArgDec], Constraint)]
    } deriving Show

data Instance = Instance 
    { ipos :: SourcePos
    , icons :: DecCons
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
    EVar    :: Id -> Expr'
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
    LNull   :: Literal'
    deriving Show

classp v n c = Class <$> getPosition <*> n <*> v <*> c

dname  i   = DName  <$> i
dcons  i p = DCons  <$> i <*> p
dmatch i   = DMatch <$> i
dnil   p   = p >> return DNil

expr' = ((Expr <$> getPosition) <*>)

elit    a     = expr' (ELit    <$> a)
evar    a     = expr' (EVar    <$> a)
efcall  a b   = expr' (EFCall  <$> a <*> b)
elet    a b   = expr' (ELet    <$> a <*> b)
ecase   a b   = expr' (ECase   <$> a <*> b)
echain  a b   = expr' (EChain  <$> a <*> b)
elambda a b   = expr' (ELambda <$> a <*> b)
efix    a b c = expr' (EFix    <$> a <*> b <*> c)

arg = ((Arg <$> getPosition) <*>)

posArg  p   = arg (PosArg <$> p)
kwArg   n p = arg (KWArg <$> n <*> p)
pSplat  p   = arg (PSplat <$> p)
kwSplat p   = arg (KWSplat <$> p)

fdec n a c = FDec <$> getPosition <*> n <*> a <*> c

argdec p c = ArgDec <$> getPosition <*> (p <*> c)

literal' = ((Literal <$> getPosition) <*>)

lchar a = literal' (LChar <$> a)
lint a    = literal' (LInt <$> a)
lfloat a  = literal' (LFloat <$> a)
lstring p  = do
    pos <- getPosition
    s <- p
    return $ Literal pos (a2Cons $ str2Exs pos s)  
    where str2Exs p = map (Expr p . ELit . Literal p .LChar)
larray a  = literal' (a2Cons <$> a)
ltuple es  = getPosition >>= \p -> return $ 
    Literal p (LCons ("(" ++ replicate (length es) ',' ++ ")") es)

ldict a   = undefined
lset a    = undefined
lcons a b = literal' (LCons <$> a <*> b)
lnull a   = a >> literal' (return LNull)

a2Cons :: [Expr] -> Literal'
a2Cons [] = LCons "[nil]" []
a2Cons (e@(Expr p _):es) =  LCons "[cons]" [e, next p es]
    where next p = Expr p . ELit . Literal p . a2Cons

nulPos = newPos "" 1 1 
