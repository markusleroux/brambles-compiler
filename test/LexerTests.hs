module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import AST

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

testParser:: Parser a -> String -> a
testParser parser code = case (parse parser "" code) of
    Right e -> e
    Left err -> error $ show err


exprTests :: TestTree
exprTests = testGroup "Basic expression parsing"
    [ testCase "Integer literal" $ testExprParser "3" @?= IntLit 3
    , testCase "Float literal" $ testExprParser "3.0" @?= FloatLit 3.0

    , testCase "Call" $ testExprParser "function(3, 4)" @?= Call "function" [IntLit 3, IntLit 4]
    , testCase "Variable" $ testExprParser "abc" @?= Var "abc"

    -- Binary operators
    , testCase "Addition" $ testExprParser "3 + 2" @?= BinOp Add (IntLit 3) (IntLit 2)
    , testCase "Subtraction" $ testExprParser "3 - 2" @?= BinOp Sub (IntLit 3) (IntLit 2)
    , testCase "Multiplication" $ testExprParser "3 * 2" @?= BinOp Mult (IntLit 3) (IntLit 2)
    , testCase "Division" $ testExprParser "3 / 2" @?= BinOp Div (IntLit 3) (IntLit 2)
    , testCase "Division (no spaces)" $ testExprParser "3/2" @?= BinOp Div (IntLit 3) (IntLit 2)
    , testCase "Assignment" $ testExprParser "x = 2" @?= Assignment (Var "x") (IntLit 2)
    ]
    where
        testExprParser = testParser exprP

typeTests :: TestTree
typeTests = testGroup "Basic types"
    [ testCase "Int" $ testTypeParser "int" @?= TyInt
    , testCase "Float" $ testTypeParser "float" @?= TyFloat
    ]
    where
        testTypeParser = testParser typeP

bindTests :: TestTree
bindTests = testGroup "Variable type (binding)"
    [ testCase "Int" $ testBindParser "int x" @?= Bind { bindType = TyInt, bindName = Var "x" }
    , testCase "Float" $ testBindParser "float y" @?= Bind { bindType = TyFloat, bindName = Var "y" }
    ]
    where
        testBindParser = testParser bindP

fnTests :: TestTree
fnTests = testGroup "Basic fn tests"
    [ testCase "Empty" $ 
        testFnParser "fn function() -> int {}" @?= Function
            { functionName = "function"
            , functionReturnType = TyInt
            , functionArguments = []
            , functionLocals = []
            , functionBody = []
            }

    , testCase "One argument" $ 
        testFnParser "fn function(int a) -> int {}" @?= Function
            { functionName = "function"
            , functionReturnType = TyInt
            , functionArguments = [ Bind { bindType = TyInt, bindName = Var "a" } ]
            , functionLocals = []
            , functionBody = []
            }

    , testCase "Multiple argument" $ 
        testFnParser "fn function(int a, float b) -> float {}" @?= Function
            { functionName = "function"
            , functionReturnType = TyFloat
            , functionArguments = [ Bind { bindType = TyInt,   bindName = Var "a" }
                                  , Bind { bindType = TyFloat, bindName = Var "b" }
                                  ]
            , functionLocals = []
            , functionBody = []
            }

    , testCase "Small body" $ 
        testFnParser "fn function(int a, float b) -> float { int x = 3; }" @?= Function
            { functionName = "function"
            , functionReturnType = TyFloat
            , functionArguments = [ Bind { bindType = TyInt,   bindName = Var "a" }
                                  , Bind { bindType = TyFloat, bindName = Var "b" }
                                  ]
            , functionLocals = [ Bind { bindType = TyInt,   bindName = Var "x" } ]
            , functionBody = [ Assignment (Var "x") (IntLit 3) ]
            }

    , testCase "Medium body" $ 
        testFnParser "fn function(int a, float b) -> float { int x; float y = 3 * 10.0; a = 4; }" @?= Function
            { functionName = "function"
            , functionReturnType = TyFloat
            , functionArguments = [ Bind { bindType = TyInt,   bindName = Var "a" }
                                  , Bind { bindType = TyFloat, bindName = Var "b" }
                                  ]
            , functionLocals = [ Bind { bindType = TyInt,   bindName = Var "x" }
                               , Bind { bindType = TyFloat, bindName = Var "y" }
                               ]
            , functionBody = [ Var "x"
                             , Assignment (Var "y") (BinOp Mult (IntLit 3) (FloatLit 10.0))
                             , Assignment (Var "a") (IntLit 4)
                             ]
            }
    ]
    where
        testFnParser = testParser functionP

main :: IO ()
main = defaultMain $ testGroup "Parsing"
    [ exprTests
    , typeTests
    , bindTests
    , fnTests
    ]


