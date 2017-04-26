module Test.Parser.TypeUnitTest where

import Test.HUnit
import Parser.Types
import Language.Expressions
import Test.Parser.ParserUtil
import Parser.Identifiers

testType :: Test
testType = TestLabel "Literal Tests" $ 
    TestList [testConstraint, testTypeAnnotation, testPType]
 
tvarA :: Type
tvarA = TVar "a" Star

tvarB :: Type
tvarB = TVar "b" Star

thingOfA :: Type
thingOfA = TAp (TCons "Thing" (KFun Star Star)) tvarA

listOfA :: Type
listOfA = TAp (TCons "List" (KFun Star Star)) tvarA

func :: Type -> Type -> Type
func t1 t2 = TAp (TAp (TCons "->" (KFun Star (KFun Star Star))) t1) t2

funcFromAToB :: Type
funcFromAToB = func tvarA tvarB

funcABThingA :: Type
funcABThingA = func tvarA (func tvarB thingOfA)

testPType :: Test
testPType = TestLabel "Test ptype" $ TestList
    [ testParse "simple variable type"
        tvarA ptype "a"
    , testParse "simple Type Constructor"
        (TCons "SomeType" Star) ptype "SomeType"
    , testParse "Applied Type Constructor"
        thingOfA ptype "Thing<a>"
    , testParse "List sugar"
        listOfA ptype "[a]"
    , testParse "function type"
        funcFromAToB ptype "<a> -> b"
    , testParse "complex function type"
        funcABThingA ptype "<a, b> -> Thing<a>"
    ]

testTypeAnnotation :: Test
testTypeAnnotation = TestLabel "Type Annotations" $ TestList
    [ testParse "no annotation"
        Nothing (dot_ *> typeAnnotation) "."
    , testParse "Simple annotation"
        (Just $ [] :=> TCons "Int" Star) 
        (dot_ *> typeAnnotation) ". : Int"
    , testParse "TAp annotation"
        (Just $ [] :=> thingOfA)
        (dot_ *> typeAnnotation) ". : Thing<a>"
    , testParse "Constrained annotation"
        (Just $ [IsIn "Eq" [tvarA]] :=> tvarA) 
        (dot_ *> typeAnnotation) ". : Given Eq(a). a"
    , testParse "Constrained constructor annotation"
        (Just $ [IsIn "Eq" [tvarA]] :=> thingOfA) 
        (dot_ *> typeAnnotation) ". : Given Eq(a). Thing<a>"
    , testParse "Multi-constrained annotation"
        (Just $ [IsIn "Eq" [tvarA], IsIn "Show" [tvarA]] :=> tvarA)
        (dot_ *> typeAnnotation) ". : Given Eq(a), Show(a). a"
    , testParseFail "Constraint must have a type"
        (dot_ *> typeAnnotation) ". : Given Class(a)."
    , testParseFail "Constraint must be separated by a period"
        (dot_ *> typeAnnotation) ". : Given Class(a) a"
    , testParseFail "Constraint must be a qualified type"
        (dot_ *> typeAnnotation) ". : ^NotA1realType"
    , testParseFail "Constraint must not be empty"
        (dot_ *> typeAnnotation) ". :"
    ]

testConstraint :: Test
testConstraint = TestLabel "Constraint" $ TestList
    [ testParse "Simple constraint"
        [IsIn "Eq" [tvarA]] constraint "Given Eq(a)."
    , testParse "Multivar constraint"
        [IsIn "SomeClass" [tvarA, tvarB]] 
        constraint "Given SomeClass(a, b)."
    , testParse "Multiple constraints"
        [ IsIn "Eq" [tvarA], IsIn "Show" [tvarA]] 
        constraint "Given Eq( a ), Show (a)."
    , testParseFail "Class names must be capital"
        constraint "Given someClass(a)"
    ]
