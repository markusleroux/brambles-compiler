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

nestedBlock :: [Stmt Name 'Parsed]
nestedBlock = 
    [ SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)
    , SExpr SourceLoc (EVar SourceLoc $ V "x")
    , SExpr SourceLoc (EBlock (Block SourceLoc 
        [ SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)
        , SExpr SourceLoc (EVar SourceLoc $ V "x")
        ]
        Nothing
    ))
    , SExpr SourceLoc (EVar SourceLoc $ V "x")
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
        [ testCase "Decl" $ 
            testBlock (EBlock (Block SourceLoc [SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)] Nothing)) [0]
        , testCase "Two Decl" $ 
            testBlock (EBlock (Block SourceLoc 
              [ SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)
              , SDecl (SourceLoc, TInt) (V "y") (EIntLit SourceLoc 3)
              ] Nothing)) [0, 1]
        , testCase "Two Decl and Var" $ 
            testBlock (EBlock (Block SourceLoc 
              [ SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)
              , SDecl (SourceLoc, TInt) (V "y") (EIntLit SourceLoc 3)
              , SExpr SourceLoc (EVar SourceLoc (V "x"))
              ] Nothing)) [0, 1, 0]
        , testCase "Nested Blocks" $ testBlock (EBlock (Block SourceLoc nestedBlock Nothing)) [0, 0, 1, 1, 0]
        ]
  where
    testBlock = testRenameExpr

funcTests =
    testGroup
        "Function"
        [ testCase "Recursion" $
            testFunc
                EFunc
                    { funcX = (SourceLoc, TCallable [] TInt)
                    , funcName = V "func"
                    , funcParams = []
                    , funcBody = Block SourceLoc [] Nothing
                    }
                [0] -- recursion!
        , testCase "param binding" $
            testFunc
                EFunc
                    { funcX = (SourceLoc, TCallable [TInt, TInt] TInt)
                    , funcName = V "func"
                    , funcParams = [V "a", V "b"]
                    , funcBody = Block SourceLoc [ SDecl (SourceLoc, TInt) (V "a") (EIntLit SourceLoc 3)
                                 , SExpr SourceLoc (EVar SourceLoc (V "a"))
                                 , SExpr SourceLoc (EVar SourceLoc (V "b"))
                                 ] Nothing
                    }
                [0, 1, 2, 3, 3, 2]
        , testCase "nested block" $
            testFunc
                EFunc
                    { funcX = (SourceLoc, TCallable [TInt, TInt] TInt)
                    , funcName = V "func"
                    , funcParams = [V "a", V "b"]
                    , funcBody = Block SourceLoc nestedBlock Nothing
                    }
                [0, 1, 2, 3, 3, 4, 4, 3]
        ]
  where
    testFunc = testRenameExpr

getVariablesPlate :: Plate Int 'Parsed (Constant [Int])
getVariablesPlate = preorderFold (purePlate { pVar = \v -> Constant [n | V n <- [v]] })

testRenameStmt :: Stmt Name 'Parsed -> [Int] -> Assertion
testRenameStmt v expected = case runIncrementalSymbolize $ renameStmt v of
    Left err -> error $ show err
    Right renamed -> foldFor pStmt getVariablesPlate renamed @?= expected

testRenameExpr :: Expr Name 'Parsed -> [Int] -> Assertion
testRenameExpr v expected = case runIncrementalSymbolize $ renameExpr v of
    Left err -> error $ show err
    Right renamed -> foldFor pExpr getVariablesPlate renamed @?= expected

