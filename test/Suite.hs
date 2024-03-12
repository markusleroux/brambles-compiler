module Main where

import AST.PrettyUnit
import Parser.Prop
import Parser.Unit
import Symbolizer.Unit
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ parsingTests
        , parsingQuickTests
        , printingTests
        , symbolizerTests
        ]
