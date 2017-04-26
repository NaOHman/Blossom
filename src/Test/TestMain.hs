module Main where

import Test.HUnit
import Test.Parser.LiteralUnitTest
import Test.Parser.PatternUnitTest
import Test.Parser.TypeUnitTest
import Test.Framework
import Test.Framework.Providers.HUnit

tests = hUnitTestToTests $ TestList 
    [testLiteral, testPattern, testType]

main :: IO ()
main = defaultMain tests
