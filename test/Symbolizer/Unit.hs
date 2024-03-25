{-# LANGUAGE RankNTypes #-}

module Symbolizer.Unit where

import AST
import Data.Functor.Constant
import Data.Foldable
import Data.Generics.Multiplate
import Symbolize
import Test.Tasty
import Test.Tasty.HUnit
import Parser (SourceLoc(..))

nestedBlock =
    Block SourceLoc 
        [ SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)
        , SExpr SourceLoc (EVar SourceLoc (V "x"))
        , SExpr SourceLoc 
            ( EBlock SourceLoc 
                ( Block SourceLoc 
                    [ SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)
                    , SExpr SourceLoc (EVar SourceLoc (V "x"))
                    ]
                )
            )
        , SExpr SourceLoc (EVar SourceLoc (V "x"))
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
        [ testCase "Decl" $ testBlock (Block SourceLoc [SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)]) [0]
        , testCase "Two Decl" $ testBlock (Block SourceLoc [SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3), SDecl (SourceLoc, TInt) (V "y") (EIntLit SourceLoc 3)]) [0, 1]
        , testCase "Two Decl and Var" $ testBlock (Block SourceLoc [SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3), SDecl (SourceLoc, TInt) (V "y") (EIntLit SourceLoc 3), SExpr SourceLoc (EVar SourceLoc (V "x"))]) [0, 1, 0]
        , testCase "Nested Blocks" $ testBlock nestedBlock [0, 0, 1, 1, 0]
        ]
  where
    testBlock = testRenameBlock

funcTests =
    testGroup
        "Function"
        [ testCase "Recursion" $
            testFunc
                SFunc
                    { fExt = (SourceLoc, TCallable [] TInt)
                    , fName = V "func"
                    , fParams = []
                    , fBody = Block SourceLoc []
                    }
                [0] -- recursion!
        , testCase "param binding" $
            testFunc
                SFunc
                    { fExt = (SourceLoc, TCallable [TInt, TInt] TInt)
                    , fName = V "func"
                    , fParams = [V "a", V "b"]
                    , fBody = Block SourceLoc [SDecl (SourceLoc, TInt) (V "a") (EIntLit SourceLoc 3), SExpr SourceLoc (EVar SourceLoc (V "a")), SExpr SourceLoc (EVar SourceLoc (V "b"))]
                    }
                [0, 1, 2, 3, 3, 2]
        , testCase "nested block" $
            testFunc
                SFunc
                    { fExt = (SourceLoc, TCallable [TInt, TInt] TInt)
                    , fName = V "func"
                    , fParams = [V "a", V "b"]
                    , fBody = nestedBlock
                    }
                [0, 1, 2, 3, 3, 4, 4, 3]
        ]
  where
    testFunc = testRenameStmt

getVariablesPlate :: Plate Int 'Parsed (Constant [Int])
getVariablesPlate = preorderFold (purePlate { pVar = \v -> Constant [n | V n <- [v]] })

testRenameStmt :: Stmt Name 'Parsed -> [Int] -> Assertion
testRenameStmt v expected = case runIncrementalSymbolize $ renameStmt v of
    Left err -> error $ show err
    Right renamed -> foldFor pStmt getVariablesPlate renamed @?= expected

testRenameBlock :: Block Name 'Parsed -> [Int] -> Assertion
testRenameBlock v expected = case runIncrementalSymbolize $ renameBlock v of
    Left err -> error $ show err
    Right renamed -> foldFor pBlock getVariablesPlate renamed @?= expected

