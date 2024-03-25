{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AST where

import Data.Generics.Multiplate (Multiplate (..))

data UnOp
    = Neg
    | Pos
    deriving (Eq, Ord, Show)

data BinOp
    = Add
    | Sub
    | Mult
    | Div
    | Eq
    deriving (Eq, Ord, Show)

data Type
    = TInt
    | TFloat
    | TBool
    | TUnit
    | TCallable {paramT :: [Type], returnT :: Type}
    deriving (Eq, Ord, Show)

type Name = String

newtype Var n = V n
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Expr n
    = EIntLit Integer
    | EFloatLit Double
    | EBoolLit Bool
    | EVar (Var n)
    | EUnOp {unOp :: UnOp, unRHS :: Expr n}
    | EBinOp {binOp :: BinOp, binLHS :: Expr n, binRHS :: Expr n}
    | ECall {callFunc :: Var n, callArgs :: [Expr n]}  -- TODO: callFunc should be expr to support higher-order functions
    | EAssign {assignVar :: Var n, assignVal :: Expr n}
    | EBlock (Block n) -- TODO: returns in block have different meaning depending on function body/scoping block, distinguish body and block?
    | EIf {ifCond :: Expr n, ifBody :: Block n, ifElseMb :: Maybe (Block n)}  -- TODO: make these expr?
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Stmt n
    = SExpr (Expr n)
    | SDecl {declName :: Var n, declT :: Type, declV :: Expr n}
    | SWhile {whileCond :: Expr n, whileBody :: Block n} -- TODO: make while an expr?
    | SReturn (Expr n)
    | SFunc {fName :: Var n, fParams :: [Var n], fType :: Type, fBody :: Block n} -- TODO: make func decl an expr?
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Block n = Block [Stmt n]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Prog n = Globals [Stmt n]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- TODO: somewhere to store line numbers

data Plate n f = Plate
    { pProg :: Prog n -> f (Prog n)
    , pBlock :: Block n -> f (Block n)
    , pStmt :: Stmt n -> f (Stmt n)
    , pExpr :: Expr n -> f (Expr n)
    , pVar :: Var n -> f (Var n)
    , pType :: Type -> f Type
    , pBinOp :: BinOp -> f BinOp
    , pUnOp :: UnOp -> f UnOp
    }

instance Multiplate (Plate n) where
    multiplate :: forall f. Applicative f => Plate n f -> Plate n f
    multiplate Plate{..} = Plate buildProg buildBlock buildStmt buildExpr buildVar buildType buildBinOp buildUnOp
      where
        buildProg :: Prog n -> f (Prog n)
        buildProg (Globals ss) = Globals <$> (pStmt `traverse` ss)

        buildBlock :: Block n -> f (Block n)
        buildBlock (Block stmts) = Block <$> (pStmt `traverse` stmts)

        buildStmt :: Stmt n -> f (Stmt n)
        buildStmt (SExpr e) = SExpr <$> pExpr e
        buildStmt (SDecl{..}) = SDecl <$> pVar declName <*> pType declT <*> pExpr declV
        buildStmt (SWhile{..}) = SWhile <$> pExpr whileCond <*> pBlock whileBody
        buildStmt (SReturn e) = SReturn <$> pExpr e
        buildStmt (SFunc{..}) = SFunc <$> pVar fName <*> (pVar `traverse` fParams) <*> pType fType <*> pBlock fBody

        buildExpr :: Expr n -> f (Expr n)
        buildExpr (EUnOp{..}) = EUnOp unOp <$> pExpr unRHS
        buildExpr (EBinOp{..}) = EBinOp binOp <$> pExpr binLHS <*> pExpr binRHS
        buildExpr (EVar v) = EVar <$> pVar v
        buildExpr (ECall{..}) = ECall <$> pVar callFunc <*> (pExpr `traverse` callArgs)
        buildExpr (EAssign{..}) = EAssign <$> pVar assignVar <*> pExpr assignVal
        buildExpr (EBlock b) = EBlock <$> pBlock b
        buildExpr (EIf{..}) = EIf <$> pExpr ifCond <*> pBlock ifBody <*> (pBlock `traverse` ifElseMb)
        buildExpr v = pure v

        buildType :: Type -> f Type
        buildType (TCallable{..}) = TCallable <$> (pType `traverse` paramT) <*> pType returnT
        buildType t = pure t

        buildBinOp :: BinOp -> f BinOp
        buildBinOp = pure

        buildUnOp :: UnOp -> f UnOp
        buildUnOp = pure

        buildVar :: Var n -> f (Var n)
        buildVar = pure

    mkPlate build =
        Plate
            (build pProg)
            (build pBlock)
            (build pStmt)
            (build pExpr)
            (build pVar)
            (build pType)
            (build pBinOp)
            (build pUnOp)
