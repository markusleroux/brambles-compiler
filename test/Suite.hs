module Main where

import ASTPrettyUnit
import ParserUnit
import ParserQuick

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ parsingTests
    , parsingQuickTests
    , printingTests
    ]
