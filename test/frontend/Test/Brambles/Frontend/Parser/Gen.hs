module Test.Brambles.Frontend.Parser.Gen where

import Protolude hiding (floatRange)

import qualified Brambles.Frontend.AST as AST
import Brambles.Frontend.Lexer (identifier)
import Brambles.Frontend.Parser (SourceLoc(..))

import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Megaparsec (parse)

paramRange :: Range.Range Int
paramRange = Range.linear 0 5

idRange :: Range.Range Int
idRange = Range.linear 1 10

intRange :: Range.Range Integer
intRange = Range.exponential 0 1024

floatRange :: Range.Range Double
floatRange = Range.exponentialFloat 0.0 1024.0

blockLen :: Range.Range Int
blockLen = Range.linear 0 10

progStmtRange :: Range.Range Int
progStmtRange = Range.linear 0 10

progFuncRange :: Range.Range Int
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
    genIdentifier = Gen.filter isValidIdentifier $ Gen.text idRange validChar
    validChar = Gen.choice [Gen.alpha, Gen.constant '_']
    isValidIdentifier = isRight . parse identifier ""

genBlock :: Gen (AST.Block AST.Name 'AST.Parsed)
genBlock = AST.Block SourceLoc <$> Gen.list blockLen genStmt <*> Gen.maybe genExpr

genExpr :: Gen (AST.Expr AST.Name 'AST.Parsed)
genExpr =
    Gen.recursive
        Gen.choice
        [ AST.EIntLit   SourceLoc <$> Gen.integral_ intRange
        , AST.EFloatLit SourceLoc <$> Gen.double floatRange
        , AST.EBoolLit  SourceLoc <$> Gen.bool
        , AST.EVar      SourceLoc <$> genVar
        ]
        [ AST.EUnOp     SourceLoc <$> genUnOp <*> genExpr
        , AST.EBinOp    SourceLoc <$> genBinOp <*> genExpr <*> genExpr
        , AST.ECall     SourceLoc <$> (AST.EVar SourceLoc <$> genVar) <*> Gen.list paramRange genExpr  -- for now, only var name
        , AST.EAssign   SourceLoc <$> genVar <*> genExpr
        , AST.EBlock              <$> genBlock
        , AST.EFunc               <$> genFunction
        , AST.EIf       SourceLoc <$> genExpr <*> genBlock <*> Gen.maybe genBlock
        ]

genFunction :: Gen (AST.Func AST.Name 'AST.Parsed)
genFunction = do
    (params, paramTs) <- unzip <$> Gen.list paramRange ((,) <$> genVar <*> genType)
    let genCallable = AST.TCallable paramTs <$> genType
    let genLocAndCallable = (,) SourceLoc <$> genCallable
    AST.Func 
        <$> genLocAndCallable 
        <*> genVar 
        <*> pure params 
        <*> genBlock

genStmt :: Gen (AST.Stmt AST.Name 'AST.Parsed)
genStmt =
    Gen.choice
        [ AST.SExpr SourceLoc 
            <$> genExpr
        , AST.SDecl 
            <$> genLocAndType 
            <*> genVar 
            <*> genExpr
        -- TODO: while
        , AST.SReturn SourceLoc 
            <$> genExpr
        ]
  where
    genLocAndType = (,) SourceLoc <$> genType

genProgram :: Gen (AST.Module AST.Name 'AST.Parsed)
genProgram = 
  let 
    genStatements = Gen.list progStmtRange $
        Gen.filter (not . AST.isFunc) genStmt
    genFunctions  = Gen.list progStmtRange genFunction
  in 
    AST.Module SourceLoc 
        <$> genStatements 
        <*> genFunctions

