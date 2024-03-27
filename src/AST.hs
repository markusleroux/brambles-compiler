{-# LANGUAGE UndecidableInstances, ConstraintKinds, StandaloneDeriving #-}
module AST where

import Data.Generics.Multiplate (Multiplate (..))
import GHC.Exts (Constraint)
import qualified Data.Kind as Kind (Type)

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


{- Trees that Grow -}
data Pass = Parsed | Typed;

type family XEIntLit (p :: Pass)
type family XEFloatLit (p :: Pass)
type family XEBoolLit (p :: Pass)
type family XEVar (p :: Pass)
type family XEUnOp (p :: Pass)
type family XEBinOp (p :: Pass)
type family XECall (p :: Pass)
type family XEAssign (p :: Pass)
type family XEBlock (p :: Pass)
type family XEIf (p :: Pass)

type family XSExpr (p :: Pass)
type family XSDecl (p :: Pass)
type family XSWhile (p :: Pass)
type family XSReturn (p :: Pass)
type family XSFunc (p :: Pass)

type family XBlock (p :: Pass)

type family XProg (p :: Pass)

data Expr n (p :: Pass)
    = EIntLit   (XEIntLit p)   Integer
    | EFloatLit (XEFloatLit p) Double
    | EBoolLit  (XEBoolLit p)  Bool
    | EVar      (XEVar p)     (Var n)
    | EUnOp {unExt :: XEUnOp p, unOp :: UnOp, unRHS :: Expr n p}
    | EBinOp {binExt :: XEBinOp p, binOp :: BinOp, binLHS :: Expr n p, binRHS :: Expr n p}
    -- TODO: callFunc should be expr to support higher-order functions
    | ECall {callExt :: XECall p, callFunc :: Var n, callArgs :: [Expr n p]}
    | EAssign {assignExt :: XEAssign p, assignVar :: Var n, assignVal :: Expr n p}
    -- TODO: returns in block have different meaning depending on function body/scoping block, distinguish body and block?
    | EBlock (XEBlock p) (Block n p) 
    | EIf {ifExt :: XEIf p, ifCond :: Expr n p, ifBody :: Block n p, ifElseMb :: Maybe (Block n p)}  -- TODO: make these expr?

data Stmt n (p :: Pass)
    = SExpr (XSExpr p) (Expr n p)
    | SDecl {declExt :: XSDecl p, declName :: Var n, declV :: Expr n p}
    | SWhile {whileExt :: XSWhile p, whileCond :: Expr n p, whileBody :: Block n p} -- TODO: make while an expr?
    | SReturn (XSReturn p) (Expr n p)
     -- TODO: make func decl an expr?
    | SFunc {fExt :: XSFunc p, fName :: Var n, fParams :: [Var n], fBody :: Block n p}

data Block n (p :: Pass) = Block (XBlock p) [Stmt n p]

data Prog n (p :: Pass) = Globals (XProg p) [Stmt n p]

{- Useful constraint types using constraint kinds -}
type ForAllExprX (c :: Kind.Type -> Constraint) p = 
  ( c (XEIntLit p)
  , c (XEFloatLit p)
  , c (XEBoolLit p)
  , c (XEVar p)
  , c (XEUnOp p)
  , c (XEBinOp p)
  , c (XECall p)
  , c (XEAssign p)
  , c (XEBlock p)
  , c (XEIf p)
  )

type ForAllStmtX (c :: Kind.Type -> Constraint) p = 
  ( c (XSExpr p)
  , c (XSDecl p)
  , c (XSWhile p)
  , c (XSReturn p)
  , c (XSFunc p)
  )

type ForAllX (c :: Kind.Type -> Constraint) p = 
  ( ForAllExprX c p
  , ForAllStmtX c p
  , c (XBlock p)
  , c (XProg p)
  )

{- Automatically derive instances from underlying -}
deriving instance (Show n, ForAllX Show p) => Show (Expr n p)
deriving instance (Show n, ForAllX Show p) => Show (Stmt n p)
deriving instance (Show n, ForAllX Show p) => Show (Block n p)
deriving instance (Show n, ForAllX Show p) => Show (Prog n p)

deriving instance (Eq n, ForAllX Eq p) => Eq (Expr n p)
deriving instance (Eq n, ForAllX Eq p) => Eq (Stmt n p)
deriving instance (Eq n, ForAllX Eq p) => Eq (Block n p)
deriving instance (Eq n, ForAllX Eq p) => Eq (Prog n p)


{- Multiplate -}
data Plate n (p :: Pass) f = Plate
    { pProg :: Prog n p -> f (Prog n p)
    , pBlock :: Block n p -> f (Block n p)
    , pStmt :: Stmt n p -> f (Stmt n p)
    , pExpr :: Expr n p -> f (Expr n p)
    , pVar :: Var n -> f (Var n)
    , pBinOp :: BinOp -> f BinOp
    , pUnOp :: UnOp -> f UnOp
    }

instance Multiplate (Plate n p) where
    multiplate :: forall f. Applicative f => Plate n p f -> Plate n p f
    multiplate Plate{..} = Plate buildProg buildBlock buildStmt buildExpr buildVar buildBinOp buildUnOp
      where
        buildProg :: Prog n p -> f (Prog n p)
        buildProg (Globals x ss) = Globals x <$> (pStmt `traverse` ss)

        buildBlock :: Block n p -> f (Block n p)
        buildBlock (Block x stmts) = Block x <$> (pStmt `traverse` stmts)

        buildStmt :: Stmt n p -> f (Stmt n p)
        buildStmt (SExpr x e) = SExpr x <$> pExpr e
        buildStmt (SDecl{..}) = SDecl declExt <$> pVar declName <*> pExpr declV
        buildStmt (SWhile{..}) = SWhile whileExt <$> pExpr whileCond <*> pBlock whileBody
        buildStmt (SReturn x e) = SReturn x <$> pExpr e
        buildStmt (SFunc{..}) = SFunc fExt <$> pVar fName <*> (pVar `traverse` fParams) <*> pBlock fBody

        buildExpr :: Expr n p -> f (Expr n p)
        buildExpr (EUnOp{..}) = EUnOp unExt unOp <$> pExpr unRHS
        buildExpr (EBinOp{..}) = EBinOp binExt binOp <$> pExpr binLHS <*> pExpr binRHS
        buildExpr (EVar x v) = EVar x <$> pVar v
        buildExpr (ECall{..}) = ECall callExt <$> pVar callFunc <*> (pExpr `traverse` callArgs)
        buildExpr (EAssign{..}) = EAssign assignExt <$> pVar assignVar <*> pExpr assignVal
        buildExpr (EBlock x b) = EBlock x <$> pBlock b
        buildExpr (EIf{..}) = EIf ifExt <$> pExpr ifCond <*> pBlock ifBody <*> (pBlock `traverse` ifElseMb)
        buildExpr v = pure v

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
            (build pBinOp)
            (build pUnOp)
