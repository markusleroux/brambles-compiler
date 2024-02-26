module ASTPrinterTests where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import AST

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

import Prettyprinter
import Prettyprinter.Render.String


printingTests = testGroup "Printing"
    [ exprTests
    , fnTests
    ]

testPrinter :: Pretty a => Parser a -> String -> Assertion
testPrinter parser code = case (parse parser "" code) of
        Left err -> error $ show err
        Right exp -> makePrettyString exp @?= code
    where
        makePrettyString = renderString . layoutPretty defaultLayoutOptions . pretty

exprTests :: TestTree
exprTests = testGroup "Basic expression printing"
    [ testCase "Variable" $ testExprPrinter "int x"
    , testCase "Binary operation" $ testExprPrinter "3 + 2"
    , testCase "Call" $ testExprPrinter "foo(3, 2)"
    , testCase "Assign" $ testExprPrinter "x = 2"
    ]
    where
        testExprPrinter = testPrinter exprP

fnTests :: TestTree
fnTests = testGroup "Basic function printing"
    [ testCase "Simple function" $ testFnPrinter "fn name() -> int {}"
    , testCase "One argument function" $ testFnPrinter "fn name(int x) -> int {x = 4;}"  -- TODO: spaces
    ]
    where
        testFnPrinter = testPrinter functionP
