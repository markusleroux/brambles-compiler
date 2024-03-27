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
        , testCase "Bool type" $ testTypeParser "bool" @?= TBool
        , testCase "Callable type (no-arg)" $ testTypeParser "() -> bool" @?= TCallable [] TBool
        , testCase "Callable type (one-arg)" $ testTypeParser "(int) -> bool" @?= TCallable [TInt] TBool
        , testCase "Callable type (multi-arg)" $ testTypeParser "(int,int) -> bool" @?= TCallable [TInt, TInt] TBool
        , testCase "Callable type (higher-order-in)" $
            testTypeParser "((int) -> bool) -> bool"
                @?= TCallable [TCallable [TInt] TBool] TBool
        , testCase "Callable type (multi-higher-order-in)" $
            testTypeParser "((int) -> bool, (int) -> int) -> bool"
                @?= TCallable [TCallable [TInt] TBool, TCallable [TInt] TInt] TBool
        , testCase "Callable type (higher-order-out)" $
            testTypeParser "(int) -> (int) -> bool"
                @?= TCallable [TInt] (TCallable [TInt] TBool)
        ]
  where
    testTypeParser = testParser typeP

exprTests :: TestTree
exprTests =
    testGroup
        "Basic expression parsing"
        [ testCase "Integer literal" $ testSExprParser "3" @?= EIntLit SourceLoc 3
        , testCase "Float literal" $ testSExprParser "3.0" @?= EFloatLit SourceLoc 3.0
        , testCase "Call" $
            testSExprParser "function(3, 4)"
                @?= ECall SourceLoc (EVar SourceLoc $ V "function") [EIntLit SourceLoc 3, EIntLit SourceLoc 4]
        , testCase "Call" $
            testSExprParser "aw(ch(-1.0, 0.0) - 2)"
                @?= ECall SourceLoc (EVar SourceLoc $ V "aw") [ EBinOp SourceLoc Sub 
                      (ECall SourceLoc (EVar SourceLoc $ V "ch") [EUnOp SourceLoc Neg $ EFloatLit SourceLoc 1.0, EFloatLit SourceLoc 0.0]) 
                      (EIntLit SourceLoc 2)
                      ]
        , testCase "Variable" $ testSExprParser "abc" @?= EVar SourceLoc (V "abc")
        , -- Binary operators
          testCase "Addition" $ testSExprParser "3 + 2" @?= EBinOp SourceLoc Add (EIntLit SourceLoc 3) (EIntLit SourceLoc 2)
        , testCase "Addition (braces)" $
            testSExprParser "3 + (2 - 5)"
                @?= EBinOp SourceLoc Add (EIntLit SourceLoc 3) (EBinOp SourceLoc Sub (EIntLit SourceLoc 2) (EIntLit SourceLoc 5))
        , testCase "Subtraction" $ testSExprParser "3 - 2" @?= EBinOp SourceLoc Sub (EIntLit SourceLoc 3) (EIntLit SourceLoc 2)
        , testCase "Multiplication" $ testSExprParser "3 * 2" @?= EBinOp SourceLoc Mult (EIntLit SourceLoc 3) (EIntLit SourceLoc 2)
        , testCase "Division" $ testSExprParser "3 / 2" @?= EBinOp SourceLoc Div (EIntLit SourceLoc 3) (EIntLit SourceLoc 2)
        , testCase "Division (no spaces)" $ testSExprParser "3/2" @?= EBinOp SourceLoc Div (EIntLit SourceLoc 3) (EIntLit SourceLoc 2)
        , testCase "EAssignment" $ testSExprParser "x = 2" @?= EAssign SourceLoc (V "x") (EIntLit SourceLoc 2)
        , testCase "Precedence" $
            testSExprParser "x = y + 5.0" @?= EAssign SourceLoc (V "x") (EBinOp SourceLoc Add (EVar SourceLoc (V "y")) (EFloatLit SourceLoc 5.0))
        , testCase "Basic block" $ testSExprParser "{ }" @?= EBlock (Block SourceLoc [] Nothing)
        , testCase "EAssignment in block" $ 
            testSExprParser "{ x = 3; }" 
              @?= EBlock (Block SourceLoc [SExpr SourceLoc $ EAssign SourceLoc (V "x") (EIntLit SourceLoc 3)] Nothing)
        ]
  where
    testSExprParser = testParser exprP

fnTests :: TestTree
fnTests =
    testGroup
        "Basic fn tests"
        [ testCase "Empty" $
            testFnParser "fn function() -> int {};"
                @?= EFunc
                    { funcX = (SourceLoc, TCallable [] TInt)
                    , funcName = V "function"
                    , funcParams = []
                    , funcBody = Block SourceLoc [] Nothing
                    }
        , testCase "One argument" $
            testFnParser "fn function(a: int) -> int {};"
                @?= EFunc
                    { funcX = (SourceLoc, TCallable [TInt] TInt)
                    , funcName = V "function"
                    , funcParams = [V "a"]
                    , funcBody = Block SourceLoc [] Nothing
                    }
        , testCase "Multiple argument" $
            testFnParser "fn function(a: int, b: float) -> float {};"
                @?= EFunc
                    { funcX = (SourceLoc, TCallable [TInt, TFloat] TFloat)
                    , funcName = V "function"
                    , funcParams = [V "a", V "b"]
                    , funcBody = Block SourceLoc [] Nothing
                    }
        , testCase "Small body" $
            testFnParser "fn function(a: int, b: float) -> float { x = 3; };"
                @?= EFunc
                    { funcX = (SourceLoc, TCallable [TInt, TFloat] TFloat)
                    , funcName = V "function"
                    , funcParams = [V "a", V "b"]
                    , funcBody = Block SourceLoc [SExpr SourceLoc (EAssign SourceLoc (V "x") (EIntLit SourceLoc 3))] Nothing
                    }
        , testCase "Medium body" $
            testFnParser "fn function(a: int, b: float) -> float { x; let y: float = 3 * 10.0; a = 4; };"
                @?= EFunc
                    { funcX = (SourceLoc, TCallable [TInt, TFloat] TFloat)
                    , funcName = V "function"
                    , funcParams = [V "a", V "b"]
                    , funcBody = Block SourceLoc
                          [ SExpr SourceLoc (EVar SourceLoc (V "x"))
                          , SDecl (SourceLoc, TFloat) (V "y") (EBinOp SourceLoc Mult (EIntLit SourceLoc 3) (EFloatLit SourceLoc 10.0))
                          , SExpr SourceLoc $ EAssign SourceLoc (V "a") (EIntLit SourceLoc 4)
                          ] Nothing
                    }
        ]
  where
    testFnParser = testParser exprP

programTests :: TestTree
programTests =
    testGroup
        "Basic program tests"
        [ testCase "Empty" $ testProgramParser "" @?= Globals SourceLoc []
        , testCase "EAssignment and function" $
            testProgramParser "let x: int = 3; fn test(y: float) -> int { z = 5; };"
                @?= Globals SourceLoc
                    [ SDecl (SourceLoc, TInt) (V "x") (EIntLit SourceLoc 3)
                    , SExpr SourceLoc $ EFunc
                        { funcX = (SourceLoc, TCallable [TFloat] TInt)
                        , funcName = V "test"
                        , funcParams = [V "y"]
                        , funcBody = Block SourceLoc [SExpr SourceLoc $ EAssign SourceLoc (V "z") (EIntLit SourceLoc 5)] Nothing
                        }
                    ]
        ]
  where
    testProgramParser = testParser programP
