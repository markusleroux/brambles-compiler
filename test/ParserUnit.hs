module ParserUnit where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import AST

import Text.Parsec (parse)
import Text.Parsec.String (Parser)


parsingTests = testGroup "Parsing"
    [ typeTests
    , variableTests
    , exprTests
    , blockTests
    , fnTests
    , programTests
    ]

testParser:: Parser a -> String -> a
testParser parser code = case (parse parser "" code) of
    Right e -> e
    Left err -> error $ show err


typeTests :: TestTree
typeTests = testGroup "Basic type parsing"
    [ testCase "Int type" $ testTypeParser "int" @?= TInt
    , testCase "Float type" $ testTypeParser "float" @?= TFloat
    ]
    where
        testTypeParser = testParser typeP


variableTests :: TestTree
variableTests = testGroup "Basic variable parsing"
    [ testCase "Int variable" $ testVarParser "int x" @?= TypedVar TInt "x"
    , testCase "Float variable" $ testVarParser "float x" @?= TypedVar TFloat "x"
    , testCase "Untyped variable" $ testVarParser "x" @?= UntypedVar "x"
    ]
    where
        testVarParser = testParser variableP


exprTests :: TestTree
exprTests = testGroup "Basic expression parsing"
    [ testCase "Integer literal" $ testExprParser "3" @?= IntLit 3
    , testCase "Float literal" $ testExprParser "3.0" @?= FloatLit 3.0

    , testCase "Call" $ testExprParser "function(3, 4)" @?= Call "function" [IntLit 3, IntLit 4]
    , testCase "Variable" $ testExprParser "abc" @?= (Variable $ UntypedVar "abc")

    -- Binary operators
    , testCase "Addition" $ testExprParser "3 + 2" @?= BinOp Add (IntLit 3) (IntLit 2)
    , testCase "Addition (braces)" $ testExprParser "3 + (2 - 5)" @?= BinOp Add (IntLit 3) (BinOp Sub (IntLit 2) (IntLit 5))
    , testCase "Subtraction" $ testExprParser "3 - 2" @?= BinOp Sub (IntLit 3) (IntLit 2)
    , testCase "Multiplication" $ testExprParser "3 * 2" @?= BinOp Mult (IntLit 3) (IntLit 2)
    , testCase "Division" $ testExprParser "3 / 2" @?= BinOp Div (IntLit 3) (IntLit 2)
    , testCase "Division (no spaces)" $ testExprParser "3/2" @?= BinOp Div (IntLit 3) (IntLit 2)

    , testCase "Assignment" $ testExprParser "x = 2" @?= Assignment (Variable $ UntypedVar "x") (IntLit 2)
    , testCase "TypedAssignment Int" $ testExprParser "int x = 2" @?= Assignment (Variable $ TypedVar TInt "x") (IntLit 2)
    , testCase "TypedAssignment Float" $ testExprParser "float x = 2" @?= Assignment (Variable $ TypedVar TFloat "x") (IntLit 2)
    ]
    where
        testExprParser = testParser exprP

blockTests :: TestTree
blockTests = testGroup "Basic block parsing"
    [ testCase "Basic block" $ testBlockParser "{ }" @?= Block []
    , testCase "Assignment in block" $ testBlockParser "{ x = 3; }" @?= Block [ Assignment (Variable $ UntypedVar "x") (IntLit 3) ]
    ]
    where
        testBlockParser = testParser blockP


fnTests :: TestTree
fnTests = testGroup "Basic fn tests"
    [ testCase "Empty" $ 
        testFnParser "fn function() -> int {}" @?= Function
            { functionName = "function"
            , functionReturnType = TInt
            , functionArguments = []
            , functionBody = Block []
            }

    , testCase "One argument" $ 
        testFnParser "fn function(int a) -> int {}" @?= Function
            { functionName = "function"
            , functionReturnType = TInt
            , functionArguments = [ TypedVar TInt "a" ]
            , functionBody = Block []
            }

    , testCase "Multiple argument" $ 
        testFnParser "fn function(int a, float b) -> float {}" @?= Function
            { functionName = "function"
            , functionReturnType = TFloat
            , functionArguments = [ TypedVar TInt "a" , TypedVar TFloat "b" ]
            , functionBody = Block []
            }

    , testCase "Small body" $ 
        testFnParser "fn function(int a, float b) -> float { int x = 3; }" @?= Function
            { functionName = "function"
            , functionReturnType = TFloat
            , functionArguments = [ TypedVar TInt "a" , TypedVar TFloat "b"                                   ]
            , functionBody = Block [ Assignment (Variable $ TypedVar TInt "x") (IntLit 3) ]
            }

    , testCase "Medium body" $ 
        testFnParser "fn function(int a, float b) -> float { int x; float y = 3 * 10.0; a = 4; }" @?= Function
            { functionName = "function"
            , functionReturnType = TFloat
            , functionArguments = [ TypedVar TInt "a" , TypedVar TFloat "b" ]
            , functionBody = Block [ Variable $ TypedVar TInt "x"
                                   , Assignment (Variable $ TypedVar TFloat "y") (BinOp Mult (IntLit 3) (FloatLit 10.0))
                                   , Assignment (Variable $ UntypedVar "a") (IntLit 4)
                                   ]
            }
    ]
    where
        testFnParser = testParser functionP


programTests :: TestTree
programTests = testGroup "Basic program tests"
    [ testCase "Empty" $ testProgramParser "" @?= Program { globals = [] , functions = [] }

    , testCase "Assignment and function" $
        testProgramParser "int x = 3; fn test(float y) -> int { int z = 5; };" @?= Program
            { globals = [ Assignment (Variable $ TypedVar TInt "x") (IntLit 3) ]
            , functions = 
                [ Function
                    { functionName = "test"
                    , functionArguments = [ TypedVar TFloat "y" ]
                    , functionReturnType = TInt
                    , functionBody = Block [ Assignment (Variable $ TypedVar TInt "z") (IntLit 5) ]
                    }
                ]
            }
    ]
    where
        testProgramParser = testParser programP
