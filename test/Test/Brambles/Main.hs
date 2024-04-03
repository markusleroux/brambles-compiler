module Main where

import Test.Brambles.Frontend.AST.PrettyUnit
import Test.Brambles.Frontend.Parser.Prop
import Test.Brambles.Frontend.Parser.Unit
import Test.Brambles.Frontend.Symbolizer.Unit

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
