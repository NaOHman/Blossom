module Test.Parser.LiteralUnitTest where

import Test.HUnit
import Parser.Literals
import Parser.IR.Literals
import Test.Parser.ParserUtil

testLiteral :: Test
testLiteral = TestLabel "Literal Tests" $ 
    TestList [testChar, testFloat, testInt, testBool]

testChar :: Test
testChar = TestLabel "Char Test" $ TestList [
    testParse "Should parse characters" 
        (LChar 'l') literal "'l'",
    testParse "Should parse escaped newline" 
        (LChar '\n') literal "'\\n'",
    testParse "Should parse escaped tab" 
        (LChar '\t') literal "'\\t'",
    testParse "Should parse escaped \\" 
        (LChar '\\') literal "'\\\\'",
    testParseFail "Should fail when single quotes surround too many chars" 
        literal "'too many chars'",
    testParseFail "Should fail on unescaped tab"
        literal "'\t'",
    testParseFail "Should fail on unescaped slash"
        literal "'\\'",
    testParseFail "Should fail on unescaped newline"
        literal "'\n'"]

testFloat :: Test
testFloat = TestLabel "Float Test" $ TestList [
    testParse "Should parse float with decimal" 
        (LFloat 1.2) literal "1.2",
    testParse "Should parse negative float with decimal" 
        (LFloat (-1.2)) literal "-1.2",
    testParse "Should parse float begining with decimal" 
        (LFloat 0.12) literal ".12",
    testParse "Should parse negative float begining with decimal" 
        (LFloat (-0.12)) literal "-.12",
    testParse "Should parse float with postfix f" 
        (LFloat 1.0) literal "1f",
    testParse "Should parse negative float with postfix f" 
        (LFloat (-1)) literal "-1f"]

testInt :: Test
testInt = TestLabel "Int test" $ TestList [
    testParse "Should parse positive integer"
        (LInt 3) literal "3",
    testParse "Should parse negative integer"
        (LInt (-42)) literal "-42"]

testBool :: Test
testBool = TestLabel "Boolean test" $ TestList [
    testParse "Should parse true"
        (LBool True) literal "True",
    testParse "Should parse false"
        (LBool False) literal "False"]

testNull :: Test
testNull = TestLabel "Null test" $ testParse "Should parse null" LNull literal "?"
