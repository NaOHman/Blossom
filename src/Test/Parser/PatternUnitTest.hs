module Test.Parser.PatternUnitTest where

import Test.HUnit
import Test.Parser.ParserUtil
import Parser.IR.Patterns
import Parser.IR.Literals
import Parser.Patterns

testPattern :: Test
testPattern = TestLabel "Pattern tests" $ 
    TestList [testPNil, testPVar, testPAs, testPCons, testPLit,
              testString, testTuple, testArray]

testPNil :: Test
testPNil = TestLabel "Nil test" $
    testParse "Should parse nil pat" PNil pat "_"

testPVar :: Test
testPVar = TestLabel "Variable test" $
    testParse "Should Parse variable pattern"
        (PVar "var") pat "var"

testPAs :: Test
testPAs = TestLabel "As test" $ TestList 
    [ testParse "Should parse as pattern"
        (PAs "var" (PCons "Const" [PVar "a"])) pat "var#Const(a)"
    , testParse "Should parse as pattern"
        (PAs "var" (PCons "Const" [PNil, PLit (LChar 'v')])) 
        pat "var#Const(_, 'v')"
    , testParseFail "As patterns may only be followed by a constructor"
         pat "var#var2"
    , testParseFail "As patterns may only be followed by a constructor"
         pat "var#_"
    , testParseFail "As patterns may only be followed by a constructor"
         pat "var#12"
    , testParseFail "As patterns may only be followed by a constructor"
         pat "var#var#Cons(_)"
    , testParseFail "As patterns variables must have lowercase names"
         pat "CapitalVarName#Cons(_)"
    ]

testPCons :: Test
testPCons = TestLabel "Constructor test" $ TestList
    [ testParse "Should parse constructor with 1 arg"
        (PCons "Cons" [PVar "a"]) pat "Cons (a)"
    , testParse "Should parse Constructors with 2 args"
        (PCons "Cons" [PLit (LInt 1), PNil]) pat "Cons( 1   , _ )"
    , testParse "Should parse Constructors with no args"
        (PCons "Cons" []) pat "Cons"
    ]

testPLit :: Test
testPLit = TestLabel "Literal Test" $ TestList
    [ testParse "Should parse character pattern"
        (PLit (LChar 'c')) pat "'c'"
    , testParse "Should parse integer pattern"
        (PLit (LInt 1)) pat "1"
    , testParse "Should parse negative integer pattern"
        (PLit (LInt (-7))) pat "-7"
    , testParse "Should parse float pattern"
        (PLit (LFloat 7.0)) pat "7f"
    , testParse "Should parse negative float pattern"
        (PLit (LFloat (-7.5))) pat "-7.5"
    , testParse "Should parse Boolean pattern"
        (PLit (LBool True)) pat "True"
    , testParse "Should parse Nul pattern"
        (PLit LNull) pat "?"
    ]

testString :: Test
testString = TestLabel "String test" $ TestList
    [ testParse "Should parse empty string pattern"
        (PLit (LString "")) pat "\"\""
    , testParse "Should parse singleton string pattern"
        (PLit (LString "h")) pat "\"h\""
    , testParse "Should parse singleton string pattern"
        (PLit (LString "\'")) pat "\"'\""
    , testParse "Should parse a string pattern"
        (PLit (LString "\n\t")) pat "\"\\n\\t\""
    ]

testTuple :: Test
testTuple = TestLabel "Tuple test" $ TestList
    [ testParse "Should parse a 2 tuple"
        (PTuple [PNil, PCons "Cons" []]) pat "(_, Cons)"
    , testParse "Should parse a 3 tuple"
        (PTuple [PLit (LChar 'h'), PVar "var", PNil])
         pat "('h',   var, _)"
    , testParseFail "Empty parens are an error"
        pat "( )"
    , testParse "parens without a comma are not tuples"
        (PVar "var") pat "(var)"
    ]

testArray :: Test
testArray = TestLabel "Array test" $ TestList
    [ testParse "Should parse an empty list"
        PEmptyList pat "[]"
    , testParse "Should parse a singleton list"
        (PSingleton PNil) pat "[_]"
    , testParse "Should parse multiple element list"
        (PArray [PNil, PVar "var"]) pat "[_, var]"
    , testParse "Should parse the tail of a list"
        (PListComp [PNil] (PVar "var")) pat "[_ | var]"
    , testParse "Should parse the second tail of a list"
        (PListComp [PNil, PVar "var"] (PVar "tail"))
        pat "[_, var | tail]"
    , testParseFail "Commas are forbidden after | in array patterns"
        pat "[_ | var , tail]"
    ]
