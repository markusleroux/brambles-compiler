module Parser.Unit where

import AST
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

parsingTests =
    testGroup
        "Parsing"
        [ typeTests
        , exprTests
        , blockTests
        , fnTests
        , programTests
        ]

testParser :: Parser a -> String -> a
testParser parser code = case parse parser "" code of
    Right e -> e
    Left err -> error $ show err

typeTests :: TestTree
typeTests =
    testGroup
        "Basic type parsing"
        [ testCase "Int type" $ testTypeParser "int" @?= TInt
        , testCase "Float type" $ testTypeParser "float" @?= TFloat
        -- TODO: callable
        ]
  where
    testTypeParser = testParser typeP

exprTests :: TestTree
exprTests =
    testGroup
        "Basic expression parsing"
        [ testCase "Integer literal" $ testSExprParser "3" @?= EIntLit 3
        , testCase "Float literal" $ testSExprParser "3.0" @?= EFloatLit 3.0
        , testCase "Call" $
            testSExprParser "function(3, 4)" @?= ECall (V "function") [EIntLit 3, EIntLit 4]
        , testCase "Call" $
            testSExprParser "aw(ch(-1.0, 0.0) - 2)" @?= ECall (V "aw") [EBinOp Sub (ECall (V "ch") [EUnOp Neg $ EFloatLit 1.0, EFloatLit 0.0]) (EIntLit 2)]
        , testCase "Variable" $ testSExprParser "abc" @?= EVar (V "abc")
        , -- Binary operators
          testCase "Addition" $ testSExprParser "3 + 2" @?= EBinOp Add (EIntLit 3) (EIntLit 2)
        , testCase "Addition (braces)" $ testSExprParser "3 + (2 - 5)" @?= EBinOp Add (EIntLit 3) (EBinOp Sub (EIntLit 2) (EIntLit 5))
        , testCase "Subtraction" $ testSExprParser "3 - 2" @?= EBinOp Sub (EIntLit 3) (EIntLit 2)
        , testCase "Multiplication" $ testSExprParser "3 * 2" @?= EBinOp Mult (EIntLit 3) (EIntLit 2)
        , testCase "Division" $ testSExprParser "3 / 2" @?= EBinOp Div (EIntLit 3) (EIntLit 2)
        , testCase "Division (no spaces)" $ testSExprParser "3/2" @?= EBinOp Div (EIntLit 3) (EIntLit 2)
        , testCase "EAssignment" $ testSExprParser "x = 2" @?= EAssign (V "x") (EIntLit 2)
        , testCase "Precedence" $
            testSExprParser "x = y + 5.0" @?= EAssign (V "x") (EBinOp Add (EVar (V "y")) (EFloatLit 5.0))
        ]
  where
    testSExprParser = testParser exprP

blockTests :: TestTree
blockTests =
    testGroup
        "Basic block parsing"
        [ testCase "Basic block" $ testBlockParser "{ }" @?= Block []
        , testCase "EAssignment in block" $ testBlockParser "{ x = 3; }" @?= Block [SExpr $ EAssign (V "x") (EIntLit 3)]
        ]
  where
    testBlockParser = testParser blockP

fnTests :: TestTree
fnTests =
    testGroup
        "Basic fn tests"
        [ testCase "Empty" $
            testFnParser "fn function() -> int {};"
                @?= SFunc
                    { fName = V "function"
                    , fType = TCallable [] TInt
                    , fParams = []
                    , fBody = Block []
                    }
        , testCase "One argument" $
            testFnParser "fn function(a: int) -> int {};"
                @?= SFunc
                    { fName = V "function"
                    , fType = TCallable [TInt] TInt
                    , fParams = [V "a"]
                    , fBody = Block []
                    }
        , testCase "Multiple argument" $
            testFnParser "fn function(a: int, b: float) -> float {};"
                @?= SFunc
                    { fName = V "function"
                    , fType = TCallable [TInt, TFloat] TFloat
                    , fParams = [V "a", V "b"]
                    , fBody = Block []
                    }
        , testCase "Small body" $
            testFnParser "fn function(a: int, b: float) -> float { x = 3; };"
                @?= SFunc
                    { fName = V "function"
                    , fType = TCallable [TInt, TFloat] TFloat
                    , fParams = [V "a", V "b"]
                    , fBody = Block [SExpr (EAssign (V "x") (EIntLit 3))]
                    }
        , testCase "Medium body" $
            testFnParser "fn function(a: int, b: float) -> float { x; let y: float = 3 * 10.0; a = 4; };"
                @?= SFunc
                    { fName = V "function"
                    , fType = TCallable [TInt, TFloat] TFloat
                    , fParams = [V "a", V "b"]
                    , fBody =
                        Block
                            [ SExpr (EVar (V "x"))
                            , SDecl (V "y") TFloat (EBinOp Mult (EIntLit 3) (EFloatLit 10.0))
                            , SExpr $ EAssign (V "a") (EIntLit 4)
                            ]
                    }
        ]
  where
    testFnParser = testParser statementP

programTests :: TestTree
programTests =
    testGroup
        "Basic program tests"
        [ testCase "Empty" $ testProgramParser "" @?= Globals []
        , testCase "EAssignment and function" $
            testProgramParser "let x: int = 3; fn test(y: float) -> int { z = 5; };"
                @?= Globals 
                  [ SDecl (V "x") TInt (EIntLit 3)
                  , SFunc
                      { fName = V "test"
                      , fParams = [V "y"]
                      , fType = TCallable [TFloat] TInt
                      , fBody = Block [SExpr $ EAssign (V "z") (EIntLit 5)]
                      }
                  ]
        ]
  where
    testProgramParser = testParser programP
