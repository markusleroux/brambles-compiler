{-# LANGUAGE RankNTypes #-}
module Symbolizer.Unit where

import Data.Generics.Multiplate
import AST
import Data.Foldable
import Symbolize
import Test.Tasty
import Test.Tasty.HUnit

nestedBlock =
    Block
        [ Decl (V "x") TInt (IntLit 3)
        , Expr (Var (V "x"))
        , Expr
            ( EBlock
                ( Block
                    [ Decl (V "x") TInt (IntLit 3)
                    , Expr (Var (V "x"))
                    ]
                )
            )
        , Expr (Var (V "x"))
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
        [ testCase "Decl" $ testBlock (Block [Decl (V "x") TInt (IntLit 3)]) [0]
        , testCase "Two Decl" $ testBlock (Block [Decl (V "x") TInt (IntLit 3), Decl (V "y") TInt (IntLit 3)]) [0, 1]
        , testCase "Two Decl and Var" $ testBlock (Block [Decl (V "x") TInt (IntLit 3), Decl (V "y") TInt (IntLit 3), Expr (Var (V "x"))]) [0, 1, 0]
        , testCase "Nested Blocks" $ testBlock nestedBlock [0, 0, 1, 1, 0]
        ]
  where
    testBlock = testRename block

funcTests =
    testGroup
        "Function"
        [ testCase "Recursion" $
            testFunc
                Func
                    { fName = V "func"
                    , fParams = []
                    , fType = TCallable [] TInt
                    , fBody = Block []
                    }
                [0] -- recursion!
        , testCase "param binding" $
            testFunc
                Func
                    { fName = V "func"
                    , fParams = [V "a", V "b"]
                    , fType = TCallable [TInt, TInt] TInt
                    , fBody = Block [Decl (V "a") TInt (IntLit 3), Expr (Var (V "a")), Expr (Var (V "b"))]
                    }
                [0, 1, 2, 3, 3, 2]
        , testCase "nested block" $
            testFunc
                Func
                    { fName = V "func"
                    , fParams = [V "a", V "b"]
                    , fType = TCallable [TInt, TInt] TInt
                    , fBody = nestedBlock
                    }
                [0, 1, 2, 3, 3, 4, 4, 3]
        ]
  where
    testFunc = testRename func


testRename :: Foldable t => Projector (Plate Name) (t Name) -> t Name -> [Int] -> Assertion
testRename p v expected = case runIncrementalSymbolize $ traverseMFor p renamePlate v of
    Left err -> error $ show err
    Right renamed -> toList renamed @?= show <$> expected

