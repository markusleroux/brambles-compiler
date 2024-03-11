module Main where

import ASTPrettyUnit
import ParserQuick
import ParserUnit
import SymbolizerUnit
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
