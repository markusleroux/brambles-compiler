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
genBinOp = Gen.element [AST.Add, AST.Sub, AST.Mult, AST.Div, AST.Eq]

genType :: Gen AST.Type
genType =
    Gen.recursive
        Gen.choice
        (Gen.constant <$> [AST.TInt, AST.TFloat, AST.TBool])
        [AST.TCallable <$> Gen.list paramRange genType <*> genType]

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
        [ AST.EIntLit <$> Gen.integral_ intRange
        , AST.EFloatLit <$> Gen.double floatRange
        , AST.EBoolLit <$> Gen.bool
        , AST.EVar <$> genVar
        ]
        [ AST.EUnOp <$> genUnOp <*> genExpr
        , AST.EBinOp <$> genBinOp <*> genExpr <*> genExpr
        , AST.ECall <$> genVar <*> Gen.list paramRange genExpr
        , AST.EAssign <$> genVar <*> genExpr
        , AST.EBlock <$> genBlock
        -- TODO: if
        ]

genStmt :: Gen (AST.Stmt AST.Name)
genStmt =
    Gen.frequency
        [ (10, AST.SExpr <$> genExpr)
        , (10, AST.SDecl <$> genVar <*> genType <*> genExpr)
        , (10, AST.SReturn <$> genExpr)
        , (3, genFunction)
        -- TODO: while
        ]
  where
    genFunction :: Gen (AST.Stmt AST.Name)
    genFunction = do
        (params, paramTs) <- unzip <$> Gen.list paramRange ((,) <$> genVar <*> genType)
        let genCallable = AST.TCallable paramTs <$> genType
        AST.SFunc <$> genVar <*> pure params <*> genCallable <*> genBlock

genBlock :: Gen (AST.Block AST.Name)
genBlock = AST.Block <$> Gen.list blockLen genStmt

genProgram :: Gen (AST.Prog AST.Name)
genProgram = AST.Globals <$> genStatements
  where
    genStatements = Gen.list progStmtRange genStmt
