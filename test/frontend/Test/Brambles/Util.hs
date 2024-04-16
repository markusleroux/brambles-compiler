module Test.Brambles.Util where

import Protolude
import Brambles.Frontend.AST

type instance XEIntLit   'Plain = ()
type instance XEFloatLit 'Plain = ()
type instance XEBoolLit  'Plain = ()
type instance XEVar      'Plain = ()
type instance XEUnOp     'Plain = ()
type instance XEBinOp    'Plain = ()
type instance XECall     'Plain = ()
type instance XEAssign   'Plain = ()
type instance XEIf       'Plain = ()
type instance XEFunc     'Plain = ()

type instance XSExpr     'Plain = ()
type instance XSDecl     'Plain = ()
type instance XSWhile    'Plain = ()
type instance XSReturn   'Plain = ()

type instance XBlock     'Plain = ()
type instance XModule    'Plain = ()


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
undecExpr EIf{..}       = EIf       () (undecExpr ifPred) (undecBlock ifThen) (undecBlock <$> ifElseMb)
undecExpr EBlock{..}    = EBlock $ undecBlock unBlock
undecExpr EFunc{..}     = EFunc  $ undecFunc unFunc

undecStmt :: Stmt n p -> Stmt n 'Plain
undecStmt SExpr{..}   = SExpr   () $ undecExpr exprExpr
undecStmt SDecl{..}   = SDecl   () declName $ undecExpr declExpr
undecStmt SWhile{..}  = SWhile  () (undecExpr whilePred) (map undecStmt whileBody)
undecStmt SReturn{..} = SReturn () $ undecExpr returnExpr

undecFunc :: Func n p -> Func n 'Plain
undecFunc Func{..}    = Func     () funcName funcParams $ undecBlock funcBody

undecModule :: Module n p -> Module n 'Plain
undecModule Module{..} = Module () (map undecStmt moduleGlobals) (map undecFunc moduleFuncs)

