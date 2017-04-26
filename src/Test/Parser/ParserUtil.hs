module Test.Parser.ParserUtil where

import Test.HUnit
import Parser.Core
import Control.Monad.State (evalStateT)

testParse :: (Show a, Eq a) => String -> a -> BParser a -> String -> Test
testParse msg expected parser source = TestCase $
    case runParser (evalParser parser) "Test" source of
        Right actual -> assertEqual msg expected actual
        Left err -> assertFailure $ show err

testParseFail :: (Show a) => String -> BParser a -> String -> Test
testParseFail msg parser source = TestCase $
    case runParser (evalParser parser) "Test" source of
        Right val -> assertFailure $ "Expected Failure " ++ msg ++ " got: " ++ show val
        Left _ -> assertBool "" True

testParseFailMessage :: (Show a) => String -> String -> BParser a -> String -> Test
testParseFailMessage msg failString parser source = TestCase $
    case runParser (evalParser parser) "Test" source of
        Right val -> assertFailure $ "Expected Failure " ++ msg ++ " got: " ++ show val
        Left err -> assertEqual msg failString $ show err

evalParser :: BParser a -> Parsec String a
evalParser parser = evalStateT (parser <* eof) (0,1)
