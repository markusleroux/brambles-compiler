module Test.Brambles.Frontend.AST.Unit where

import Protolude hiding (exp)
import Protolude.Error (error)

import Brambles.Frontend.Lexer (Parser)
import Brambles.Frontend.Parser
import Brambles.Frontend.Pretty ()

import Prettyprinter
import Prettyprinter.Render.Text

import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec


printingTests :: TestTree
printingTests =
    testGroup
        "Printing"
        [ exprTests
        , fnTests
        ]

testPrinter :: Pretty a => Parser a -> Text -> Assertion
testPrinter parser code = case parse parser "" code of
    Left err -> error $ show err
    Right exp -> makePrettyString exp @?= code
  where
    makePrettyString = renderStrict . layoutPretty defaultLayoutOptions . pretty

exprTests :: TestTree
exprTests =
    testGroup
        "Basic expression printing"
        [ testCase "Variable" $ testExprPrinter "x"
        , testCase "Binary operation" $ testExprPrinter "3 + 2"
        , testCase "Call" $ testExprPrinter "foo(3, 2)"
        , testCase "Assign" $ testExprPrinter "x = 2"
        , testCase "Assign negative float to int" $ testExprPrinter "x = -2.0"
        ]
  where
    testExprPrinter = testPrinter exprP

fnTests :: TestTree
fnTests =
    testGroup
        "Basic function printing"
        [ testCase "Simple function" $ testFnPrinter "fn name() -> int {};"
        , testCase "One argument function" $ testFnPrinter "fn name(x: int) -> int {\nlet x: float = 4;\n};" -- TODO: one line with spaces
        ]
  where
    testFnPrinter = testPrinter statementP
