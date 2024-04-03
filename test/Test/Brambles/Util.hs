module Test.Brambles.Util where

import Brambles.Frontend.AST

import Data.Void

type instance XEIntLit 'Plain   = ()
type instance XEFloatLit 'Plain = ()
type instance XEBoolLit 'Plain  = ()
type instance XEVar 'Plain      = ()
type instance XEUnOp 'Plain     = ()
type instance XEBinOp 'Plain    = ()
type instance XECall 'Plain     = ()
type instance XEAssign 'Plain   = ()
type instance XEIf 'Plain       = ()
type instance XEFunc 'Plain     = ()

type instance XSExpr 'Plain     = ()
type instance XSDecl 'Plain     = ()
type instance XSWhile 'Plain    = ()
type instance XSReturn 'Plain   = ()

type instance XBlock 'Plain    = ()
type instance XProg 'Plain     = ()


undecBlock :: Block n p -> Block n 'Plain
undecBlock Block{..} = Block () (map undecStmt blockBody) (undecExpr <$> blockResult)

undecExpr :: Expr n p -> Expr n 'Plain
undecExpr EIntLit{..}   = EIntLit   () intLitVal
undecExpr EFloatLit{..} = EFloatLit () floatLitVal
undecExpr EBoolLit{..}  = EBoolLit  () boolLitVal
undecExpr EVar{..}      = EVar      () varVar
undecExpr EUnOp{..}     = EUnOp     () unOp $ undecExpr unRHS
undecExpr EBinOp{..}    = EBinOp    () binOp (undecExpr binLHS) (undecExpr binRHS)
undecExpr ECall{..}     = ECall     () (undecExpr callName) (undecExpr <$> callArgs)
undecExpr EAssign{..}   = EAssign   () assignVar $ undecExpr assignExpr
undecExpr EBlock{..}    = EBlock $ undecBlock unBlock
undecExpr EIf{..}       = EIf       () (undecExpr ifPred) (undecBlock ifThen) (undecBlock <$> ifElseMb)
undecExpr EFunc{..}     = EFunc     () funcName funcParams $ undecBlock funcBody

undecStmt :: Stmt n p -> Stmt n 'Plain
undecStmt SExpr{..}   = SExpr   () $ undecExpr exprExpr
undecStmt SDecl{..}   = SDecl   () declName $ undecExpr declExpr
undecStmt SWhile{..}  = SWhile  () (undecExpr whilePred) (map undecStmt whileBody)
undecStmt SReturn{..} = SReturn () $ undecExpr returnExpr

undecProg :: Prog n p -> Prog n 'Plain
undecProg (Globals _ g) = Globals () $ map undecStmt g

