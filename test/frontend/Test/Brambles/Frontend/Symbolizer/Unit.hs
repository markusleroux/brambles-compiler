module Test.Brambles.Frontend.Symbolizer.Unit where

import Protolude
import Protolude.Error (error)

import Brambles.Frontend.AST
import Brambles.Frontend.Symbolize

import Test.Brambles.Util ()

import Data.Functor.Constant
import Data.Generics.Multiplate
import Test.Tasty
import Test.Tasty.HUnit


nestedBlock :: [Stmt Name 'Plain]
nestedBlock = 
    [ SDecl () (V "x") (EIntLit () 3)
    , SExpr () (EVar () $ V "x")
    , SExpr () (EBlock (Block () 
        [ SDecl () (V "x") (EIntLit () 3)
        , SExpr () (EVar () $ V "x")
        ]
        Nothing
    ))
    , SExpr () (EVar () $ V "x")
    ]

symbolizerTests :: TestTree
symbolizerTests =
    testGroup
        "Symbolizer"
        [ blockTests
        , funcTests
        ]

blockTests :: TestTree
blockTests =
    testGroup
        "Block"
        [ testCase "Decl" $ 
            testBlock (EBlock (Block () [SDecl () (V "x") (EIntLit () 3)] Nothing)) [0]
        , testCase "Two Decl" $ 
            testBlock (EBlock (Block () 
              [ SDecl () (V "x") (EIntLit () 3)
              , SDecl () (V "y") (EIntLit () 3)
              ] Nothing)) [0, 1]
        , testCase "Two Decl and Var" $ 
            testBlock (EBlock (Block () 
              [ SDecl () (V "x") (EIntLit () 3)
              , SDecl () (V "y") (EIntLit () 3)
              , SExpr () (EVar () (V "x"))
              ] Nothing)) [0, 1, 0]
        , testCase "Nested Blocks" $ testBlock (EBlock (Block () nestedBlock Nothing)) [0, 0, 1, 1, 0]
        ]
  where
    testBlock = testRenameExpr

funcTests :: TestTree
funcTests =
    testGroup
        "Function"
        [ testCase "Recursion" $
            testFunc
                Func
                    { funcX = ()
                    , funcName = V "func"
                    , funcParams = []
                    , funcBody = Block () [] Nothing
                    }
                [0] -- recursion!
        , testCase "param binding" $
            testFunc
                Func
                    { funcX = ()
                    , funcName = V "func"
                    , funcParams = [V "a", V "b"]
                    , funcBody = Block () [ SDecl () (V "a") (EIntLit () 3)
                                 , SExpr () (EVar () (V "a"))
                                 , SExpr () (EVar () (V "b"))
                                 ] Nothing
                    }
                [0, 1, 2, 3, 3, 2]
        , testCase "nested block" $
            testFunc
                Func
                    { funcX = ()
                    , funcName = V "func"
                    , funcParams = [V "a", V "b"]
                    , funcBody = Block () nestedBlock Nothing
                    }
                [0, 1, 2, 3, 3, 4, 4, 3]
        ]
  where
    testFunc = testRenameFunc

getVariablesPlate :: Plate Int p (Constant [Int])
getVariablesPlate = preorderFold (purePlate { pVar = \v -> Constant [n | V n <- [v]] })

testRenameFunc :: Func Name 'Plain -> [Int] -> Assertion
testRenameFunc v expected = case runIncrementalSymbolize $ renameFunc v of
    Left err -> error $ show err
    Right renamed -> foldFor pFunc getVariablesPlate renamed @?= expected

testRenameExpr :: Expr Name 'Plain -> [Int] -> Assertion
testRenameExpr v expected = case runIncrementalSymbolize $ renameExpr v of
    Left err -> error $ show err
    Right renamed -> foldFor pExpr getVariablesPlate renamed @?= expected

