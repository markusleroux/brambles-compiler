module Parser.Gen where

import Prelude hiding (floatRange)

import qualified AST
import Data.Either (isRight)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lexer (identifier)
import Text.Parsec (parse)
import Parser (SourceLoc(..))

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
        , AST.EBlock    SourceLoc <$> Gen.list blockLen genStmt <*> Gen.maybe genExpr
        , genFunction
        -- TODO: if
        ]
    where
      genFunction :: Gen (AST.Expr AST.Name 'AST.Parsed)
      genFunction = do
          (params, paramTs) <- unzip <$> Gen.list paramRange ((,) <$> genVar <*> genType)
          let genCallable = AST.TCallable paramTs <$> genType
          let genLocAndCallable = (,) SourceLoc <$> genCallable
          AST.EFunc <$> genLocAndCallable <*> genVar <*> pure params <*> Gen.list blockLen genStmt

genStmt :: Gen (AST.Stmt AST.Name 'AST.Parsed)
genStmt =
    Gen.choice
        [ AST.SExpr SourceLoc <$> genExpr
        , AST.SDecl <$> genLocAndType <*> genVar <*> genExpr
        -- TODO: while
        , AST.SReturn SourceLoc <$> genExpr
        ]
  where
    genLocAndType = (,) SourceLoc <$> genType

genProgram :: Gen (AST.Prog AST.Name 'AST.Parsed)
genProgram = 
  let genStatements = Gen.list progStmtRange genStmt
  in AST.Globals SourceLoc <$> genStatements
    
