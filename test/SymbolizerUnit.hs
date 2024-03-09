{-# LANGUAGE FlexibleContexts #-}

module SymbolizerUnit where

import Symbolize
import AST

import Test.Tasty
import Test.Tasty.HUnit

import Data.Foldable

nestedBlock = Block 
    [ Decl "x" TInt (IntLit 3)
    , Expr (Var "x")
    , Expr (EBlock (Block 
        [ Decl "x" TInt (IntLit 3)
        , Expr (Var "x")
        ]))
    , Expr (Var "x")
    ]


symbolizerTests = testGroup "Symbolizer"
    [ blockTests 
    , funcTests
    ]

blockTests = testGroup "Block" 
    [ testCase "Decl"             $ testBlock (Block [Decl "x" TInt (IntLit 3)]) [0]
    , testCase "Two Decl"         $ testBlock (Block [Decl "x" TInt (IntLit 3), Decl "y" TInt (IntLit 3)]) [0, 1]
    , testCase "Two Decl and Var" $ testBlock (Block [Decl "x" TInt (IntLit 3), Decl "y" TInt (IntLit 3), Expr (Var "x")]) [0, 1, 0]
    , testCase "Nested Blocks"    $ testBlock nestedBlock [0, 0, 1, 1, 0]
    ]
    where
        testBlock = testRename renameBlock

funcTests = testGroup "Function" 
    [ testCase "Recursion" $ testFunc Func
        { fName = "func"
        , fParams = []
        , fType = TCallable [] TInt
        , fBody = Block []
        } [0]  -- recursion!
    , testCase "param binding" $ testFunc Func
        { fName = "func"
        , fParams = ["a", "b"]
        , fType = TCallable [TInt, TInt] TInt
        , fBody = Block [Decl "a" TInt (IntLit 3), Expr (Var "a"), (Expr (Var "b"))]
        } [0, 1, 2, 3, 3, 2]
    , testCase "nested block" $ testFunc Func
        { fName = "func"
        , fParams = ["a", "b"]
        , fType = TCallable [TInt, TInt] TInt
        , fBody = nestedBlock
        } [0, 1, 2, 3, 3, 4, 4, 3]
    ]
    where
        testFunc = testRename renameFunction

testRename :: Foldable t => (t Name -> IncrementalSymbolize (t Int)) -> t Name -> [Int] -> Assertion
testRename renamer v expected = case runIncrementalSymbolize $ renamer v of
    Left err      -> error $ show err
    Right renamed -> toList renamed @?= expected


