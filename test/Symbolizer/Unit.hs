{-# LANGUAGE RankNTypes #-}

module Symbolizer.Unit where

import AST
import Data.Foldable
import Data.Generics.Multiplate
import Symbolize
import Test.Tasty
import Test.Tasty.HUnit

nestedBlock =
    Block
        [ SDecl (V "x") TInt (EIntLit 3)
        , SExpr (EVar (V "x"))
        , SExpr
            ( EBlock
                ( Block
                    [ SDecl (V "x") TInt (EIntLit 3)
                    , SExpr (EVar (V "x"))
                    ]
                )
            )
        , SExpr (EVar (V "x"))
        ]

symbolizerTests =
    testGroup
        "Symbolizer"
        [ blockTests
        , funcTests
        ]

blockTests =
    testGroup
        "Block"
        [ testCase "Decl" $ testBlock (Block [SDecl (V "x") TInt (EIntLit 3)]) [0]
        , testCase "Two Decl" $ testBlock (Block [SDecl (V "x") TInt (EIntLit 3), SDecl (V "y") TInt (EIntLit 3)]) [0, 1]
        , testCase "Two Decl and Var" $ testBlock (Block [SDecl (V "x") TInt (EIntLit 3), SDecl (V "y") TInt (EIntLit 3), SExpr (EVar (V "x"))]) [0, 1, 0]
        , testCase "Nested Blocks" $ testBlock nestedBlock [0, 0, 1, 1, 0]
        ]
  where
    testBlock = testRename block

funcTests =
    testGroup
        "Function"
        [ testCase "Recursion" $
            testFunc
                SFunc
                    { fName = V "func"
                    , fParams = []
                    , fType = TCallable [] TInt
                    , fBody = Block []
                    }
                [0] -- recursion!
        , testCase "param binding" $
            testFunc
                SFunc
                    { fName = V "func"
                    , fParams = [V "a", V "b"]
                    , fType = TCallable [TInt, TInt] TInt
                    , fBody = Block [SDecl (V "a") TInt (EIntLit 3), SExpr (EVar (V "a")), SExpr (EVar (V "b"))]
                    }
                [0, 1, 2, 3, 3, 2]
        , testCase "nested block" $
            testFunc
                SFunc
                    { fName = V "func"
                    , fParams = [V "a", V "b"]
                    , fType = TCallable [TInt, TInt] TInt
                    , fBody = nestedBlock
                    }
                [0, 1, 2, 3, 3, 4, 4, 3]
        ]
  where
    testFunc = testRename stmt

testRename :: Foldable t => Projector (Plate Name) (t Name) -> t Name -> [Int] -> Assertion
testRename p v expected = case runIncrementalSymbolize $ traverseMFor p renamePlate v of
    Left err -> error $ show err
    Right renamed -> toList renamed @?= show <$> expected
