module Parser.Gen where

import Prelude hiding (floatRange)

import qualified AST
import Data.Either (isRight)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lexer (identifier)
import Text.Parsec (parse)

paramRange = Range.linear 0 5
idRange = Range.linear 1 10
intRange = Range.exponential 0 1024
floatRange = Range.exponentialFloat 0.0 1024.0
blockLen = Range.linear 0 10
progStmtRange = Range.linear 0 10
progFuncRange = Range.linear 0 10

genUnOp :: Gen AST.UnOp
genUnOp = Gen.element [AST.Neg, AST.Pos]

genBinOp :: Gen AST.BinOp
genBinOp = Gen.element [AST.Add, AST.Sub, AST.Mult, AST.Div]

genType :: Gen AST.Type
genType = Gen.element [AST.TInt, AST.TFloat]


-- TODO: these should be generic over n

genVar :: Gen (AST.Var AST.Name)
genVar = AST.V <$> genIdentifier
  where
    genIdentifier = Gen.filter isValidIdentifier $ Gen.string idRange validChar
    validChar = Gen.choice [Gen.alpha, Gen.constant '_']
    isValidIdentifier = isRight . parse identifier ""

genExpr :: Gen (AST.Expr AST.Name)
genExpr =
    Gen.recursive
        Gen.choice
        [ AST.IntLit <$> Gen.integral_ intRange
        , AST.FloatLit <$> Gen.double floatRange
        , AST.Var <$> genVar
        ]
        [ AST.UnOp <$> genUnOp <*> genExpr
        , AST.BinOp <$> genBinOp <*> genExpr <*> genExpr
        , AST.Call <$> genVar <*> Gen.list paramRange genExpr
        , AST.Assign <$> genVar <*> genExpr
        , AST.EBlock <$> genBlock
        ]

genStmt :: Gen (AST.Stmt AST.Name)
genStmt = Gen.recursive Gen.choice []
    [ AST.Expr <$> genExpr
    , AST.Decl <$> genVar <*> genType <*> genExpr
    ]

genBlock :: Gen (AST.Block AST.Name)
genBlock = AST.Block <$> Gen.list blockLen genStmt

genFunction :: Gen (AST.Func AST.Name)
genFunction = do
    (params, paramTs) <- unzip <$> Gen.list paramRange ((,) <$> genVar <*> genType)
    let genCallable = AST.TCallable paramTs <$> genType
    AST.Func <$> genVar <*> pure params <*> genCallable <*> genBlock

genProgram :: Gen (AST.Program AST.Name)
genProgram = AST.Program <$> genStatements <*> genFunctions
  where
    genStatements = Gen.list progStmtRange genStmt
    genFunctions = Gen.list progFuncRange genFunction
