module Test.Parser.ParserTest where

import Test.QuickCheck
import Language.Expressions
import Language.Types

instance Arbitrary Expr where
    arbitrary = oneof $
        [ Lit <$> arbitrary
        , Var <$> arbitrary
        , Ap <$> arbitrary <*> arbitrary
        , Let <$> arbitrary <*> arbitrary <*> arbitrary
        , LetRec <$> arbitrary <*> arbitrary <*> arbitrary
        , Case <$> arbitrary <*> arbitrary
        , Annot <$> arbitrary]
        {-, Over <$> arbitrary <*> arbitrary <*> arbitrary]-}

instance Arbitrary Bind where
    arbitrary = Bind <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Annotated a) where
    arbitrary = (:-:) <$> arbitrary <*> arbitrary

instance Arbitrary Scheme where
    arbitrary = Forall <$> arbitrary <*> arbitrary

instance Arbitrary Type where
    arbitrary = oneof $
        [ TVar <$> arbitrary
        , TCons <$> arbitrary
        -- TGen? TAp?
        ]
instance Arbitrary Kind where
    arbitrary = oneof $
        [ return Star
        , KFun Star <$> arbitrary ]

instance Arbitrary Tyvar where
    arbitrary = Tyvar <$> arbitrary <*> arbitrary

instance Arbitrary Tycon where
    arbitrary = Tycon <$> arbitrary <*> arbitrary

instance Arbitrary Pat where
    arbitrary = oneof $
        [ PCons <$> arbitrary <*> arbitrary
        , PAs   <$> arbitrary <*> arbitrary
        , PLit  <$> arbitrary
        , PVar  <$> arbitrary
        , return PNil ]

instance Arbitrary a => Arbitrary (Qual a) where
    arbitrary = (:=>) <$> arbitrary <*> arbitrary

instance Arbitrary Pred where
    arbitrary = IsIn <$> arbitrary <*> arbitrary

instance Arbitrary Literal where
    arbitrary = oneof $ 
        [ LChar <$> arbitrary
        , LInt <$> arbitrary
        , LFloat <$> arbitrary
        , LBool <$> arbitrary
        , return LNull ] 
