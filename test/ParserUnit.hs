module ParserUnit where

import Test.Tasty
import Test.Tasty.HUnit

import Parser
import AST

import Text.Parsec (parse)
import Text.Parsec.String (Parser)


parsingTests = testGroup "Parsing"
    [ typeTests
    , exprTests
    , blockTests
    , fnTests
    , programTests
    ]

testParser:: Parser a -> String -> a
testParser parser code = case parse parser "" code of
    Right e -> e
    Left err -> error $ show err


typeTests :: TestTree
typeTests = testGroup "Basic type parsing"
    [ testCase "Int type" $ testTypeParser "int" @?= TInt
    , testCase "Float type" $ testTypeParser "float" @?= TFloat
    -- TODO: callable
    ]
    where
        testTypeParser = testParser typeP


exprTests :: TestTree
exprTests = testGroup "Basic expression parsing"
    [ testCase "Integer literal" $ testExprParser "3"   @?= IntLit 3
    , testCase "Float literal"   $ testExprParser "3.0" @?= FloatLit 3.0

    , testCase "Call" 
        $ testExprParser "function(3, 4)" @?= Call "function" [IntLit 3, IntLit 4]
    , testCase "Call" 
        $ testExprParser "aw(ch(-1.0, 0.0) - 2)" @?= Call "aw" [BinOp Sub (Call "ch" [UnOp Neg $ FloatLit 1.0, FloatLit 0.0]) (IntLit 2)]

    , testCase "Variable" $ testExprParser "abc" @?= Var "abc"

    -- Binary operators
    , testCase "Addition"             $ testExprParser "3 + 2"       @?= BinOp Add  (IntLit 3) (IntLit 2)
    , testCase "Addition (braces)"    $ testExprParser "3 + (2 - 5)" @?= BinOp Add  (IntLit 3) (BinOp Sub (IntLit 2) (IntLit 5))
    , testCase "Subtraction"          $ testExprParser "3 - 2"       @?= BinOp Sub  (IntLit 3) (IntLit 2)
    , testCase "Multiplication"       $ testExprParser "3 * 2"       @?= BinOp Mult (IntLit 3) (IntLit 2)
    , testCase "Division"             $ testExprParser "3 / 2"       @?= BinOp Div  (IntLit 3) (IntLit 2)
    , testCase "Division (no spaces)" $ testExprParser "3/2"         @?= BinOp Div  (IntLit 3) (IntLit 2)

    , testCase "Assignment"                       $ testExprParser "x = 2"        @?= Assign "x" (IntLit 2)
    , testCase "Precedence" 
        $ testExprParser "x = y + 5.0" @?= Assign "x" (BinOp Add (Var "y") (FloatLit 5.0))
    ]
    where
        testExprParser = testParser exprP

blockTests :: TestTree
blockTests = testGroup "Basic block parsing"
    [ testCase "Basic block"         $ testBlockParser "{ }"        @?= Block []
    , testCase "Assignment in block" $ testBlockParser "{ x = 3; }" @?= Block [ Expr $ Assign "x" (IntLit 3) ]
    ]
    where
        testBlockParser = testParser blockP


fnTests :: TestTree
fnTests = testGroup "Basic fn tests"
    [ testCase "Empty" $ 
        testFnParser "fn function() -> int {}" @?= Func
            { fName = "function"
            , fType = TCallable [] TInt
            , fParams = []
            , fBody = Block []
            }

    , testCase "One argument" $ 
        testFnParser "fn function(a: int) -> int {}" @?= Func
            { fName = "function"
            , fType = TCallable [TInt] TInt
            , fParams = [ "a" ]
            , fBody = Block []
            }

    , testCase "Multiple argument" $ 
        testFnParser "fn function(a: int, b: float) -> float {}" @?= Func
            { fName = "function"
            , fType = TCallable [TInt, TFloat] TFloat
            , fParams = [ "a" , "b" ]
            , fBody = Block []
            }

    , testCase "Small body" $ 
        testFnParser "fn function(a: int, b: float) -> float { x = 3; }" @?= Func
            { fName = "function"
            , fType = TCallable [TInt, TFloat] TFloat
            , fParams = [ "a", "b" ]
            , fBody = Block [ Expr (Assign "x" (IntLit 3)) ]
            }

    , testCase "Medium body" $ 
        testFnParser "fn function(a: int, b: float) -> float { x; let y: float = 3 * 10.0; a = 4; }" @?= Func
            { fName = "function"
            , fType = TCallable [TInt, TFloat] TFloat
            , fParams = [ "a", "b" ]
            , fBody = Block [ Expr (Var "x")
                            , Decl "y" TFloat (BinOp Mult (IntLit 3) (FloatLit 10.0))
                            , Expr $ Assign "a" (IntLit 4)
                            ]
            }
    ]
    where
        testFnParser = testParser functionP


programTests :: TestTree
programTests = testGroup "Basic program tests"
    [ testCase "Empty" $ testProgramParser "" @?= Program { globals = [] , funcs = [] }

    , testCase "Assignment and function" $
        testProgramParser "let x: int = 3; fn test(y: float) -> int { z = 5; };" @?= Program
            { globals = [ Decl "x" TInt (IntLit 3) ]
            , funcs = 
                [ Func
                    { fName = "test"
                    , fParams = [ "y" ]
                    , fType = TCallable [ TFloat ] TInt
                    , fBody = Block [ Expr $ Assign "z" (IntLit 5) ]
                    }
                ]
            }
    ]
    where
        testProgramParser = testParser programP

