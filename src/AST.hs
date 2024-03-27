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
    | TCallable { paramT :: [Type], returnT :: Type }
    | TOptional Type
    deriving (Eq, Ord, Show)

isNumeric :: Type -> Bool
isNumeric TInt = True
isNumeric TFloat = True
isNumeric _ = False


{- Trees that Grow -}
data Pass = Parsed | Typed;

type family XEIntLit   (p :: Pass)
type family XEFloatLit (p :: Pass)
type family XEBoolLit  (p :: Pass)
type family XEVar      (p :: Pass)
type family XEUnOp     (p :: Pass)
type family XEBinOp    (p :: Pass)
type family XECall     (p :: Pass)
type family XEAssign   (p :: Pass)
type family XEIf       (p :: Pass)
type family XEFunc     (p :: Pass)

type family XSExpr     (p :: Pass)
type family XSDecl     (p :: Pass)
type family XSWhile    (p :: Pass)
type family XSReturn   (p :: Pass)

type family XBlock     (p :: Pass)
type family XProg      (p :: Pass)

type Name = String

newtype Var n = V n
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Block n (p :: Pass) 
  = Block
    { blockX      :: XBlock p
    , blockBody   :: [Stmt n p]
    , blockResult :: Maybe (Expr n p)
    }

data Expr n (p :: Pass)
    = EIntLit 
      { intLitX     :: XEIntLit p
      , intLitVal   :: Integer
      }
    | EFloatLit 
      { floatLitX   :: XEFloatLit p
      , floatLitVal :: Double
      }
    | EBoolLit  
      { boolLitX    :: XEBoolLit p
      , boolLitVal  :: Bool
      }
    | EVar      
      { varX        :: XEVar p
      , varVar      :: Var n
      }
    | EUnOp 
      { unX         :: XEUnOp p
      , unOp        :: UnOp
      , unRHS       :: Expr n p
      }
    | EBinOp 
      { binX        :: XEBinOp p
      , binOp       :: BinOp
      , binLHS      :: Expr n p
      , binRHS      :: Expr n p
      }
    | ECall 
      { callX       :: XECall p
      , callName    :: Expr n p
      , callArgs    :: [Expr n p]
      }
    | EAssign 
      { assignX     :: XEAssign p
      , assignVar   :: Var n
      , assignExpr  :: Expr n p
      }
    | EBlock { unBlock :: Block n p }
    | EIf 
      { ifX         :: XEIf p
      , ifPred      :: Expr n p
      , ifThen      :: Block n p
      , ifElseMb    :: Maybe (Block n p)
      }
    | EFunc 
      { funcX       :: XEFunc p
      , funcName    :: Var n
      , funcParams  :: [Var n]
      , funcBody    :: Block n p
      }

data Stmt n (p :: Pass)
    = SExpr 
      { exprX      :: XSExpr p
      , exprExpr   :: Expr n p
      }
    | SDecl 
      { declX      :: XSDecl p
      , declName   :: Var n
      , declExpr   :: Expr n p
      }
    | SWhile 
      { whileX     :: XSWhile p
      , whilePred  :: Expr n p
      , whileBody  :: [Stmt n p]
      }
    | SReturn 
      { returnX    :: XSReturn p
      , returnExpr :: Expr n p
      }

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
  , c (XEIf p)
  , c (XEFunc p)
  )

type ForAllStmtX (c :: Kind.Type -> Constraint) p = 
  ( c (XSExpr p)
  , c (XSDecl p)
  , c (XSWhile p)
  , c (XSReturn p)
  )

type ForAllX (c :: Kind.Type -> Constraint) p = 
  ( ForAllExprX c p
  , ForAllStmtX c p
  , c (XBlock p)
  , c (XProg p)
  )


{- Automatically derive instances from underlying -}
deriving instance (Show n, ForAllX Show p) => Show (Block n p)
deriving instance (Show n, ForAllX Show p) => Show (Expr n p)
deriving instance (Show n, ForAllX Show p) => Show (Stmt n p)
deriving instance (Show n, ForAllX Show p) => Show (Prog n p)

deriving instance (Eq n, ForAllX Eq p) => Eq (Block n p)
deriving instance (Eq n, ForAllX Eq p) => Eq (Expr n p)
deriving instance (Eq n, ForAllX Eq p) => Eq (Stmt n p)
deriving instance (Eq n, ForAllX Eq p) => Eq (Prog n p)


{- Multiplate -}
data Plate n (p :: Pass) f = Plate
    { pProg  :: Prog n p  -> f (Prog n p)
    , pStmt  :: Stmt n p  -> f (Stmt n p)
    , pExpr  :: Expr n p  -> f (Expr n p)
    , pBlock :: Block n p -> f (Block n p)
    , pVar   :: Var n     -> f (Var n)
    , pBinOp :: BinOp     -> f BinOp
    , pUnOp  :: UnOp      -> f UnOp
    }

instance Multiplate (Plate n p) where
    multiplate :: forall f. Applicative f => Plate n p f -> Plate n p f
    multiplate Plate{..} = Plate buildProg buildStmt buildExpr buildBlock buildVar buildBinOp buildUnOp
      where
        buildProg :: Prog n p -> f (Prog n p)
        buildProg (Globals x ss) = Globals x <$> (pStmt `traverse` ss)

        buildStmt :: Stmt n p -> f (Stmt n p)
        buildStmt SExpr{..}   = SExpr   exprX   <$> pExpr exprExpr
        buildStmt SDecl{..}   = SDecl   declX   <$> pVar declName   <*> pExpr declExpr
        buildStmt SWhile{..}  = SWhile  whileX  <$> pExpr whilePred <*> (pStmt `traverse` whileBody)
        buildStmt SReturn{..} = SReturn returnX <$> pExpr returnExpr

        buildExpr :: Expr n p -> f (Expr n p)
        buildExpr EUnOp{..}   = EUnOp   unX  unOp  <$> pExpr unRHS
        buildExpr EBinOp{..}  = EBinOp  binX binOp <$> pExpr binLHS <*> pExpr binRHS
        buildExpr EVar{..}    = EVar    varX    <$> pVar varVar
        buildExpr ECall{..}   = ECall   callX   <$> pExpr callName <*> (pExpr `traverse` callArgs)
        buildExpr EAssign{..} = EAssign assignX <$> pVar assignVar <*> pExpr assignExpr
        buildExpr EBlock{..}  = EBlock  <$> buildBlock unBlock
        buildExpr EIf{..}     = EIf     ifX     <$> pExpr ifPred <*> pBlock ifThen <*> (pBlock `traverse` ifElseMb)
        buildExpr EFunc{..}   = EFunc   funcX   <$> pVar funcName <*> (pVar `traverse` funcParams) <*> pBlock funcBody
        buildExpr v = pure v

        buildBlock :: Block n p -> f (Block n p)
        buildBlock Block{..}  = Block  blockX  <$> (pStmt `traverse` blockBody) <*> (pExpr `traverse` blockResult)

        buildBinOp :: BinOp -> f BinOp
        buildBinOp = pure

        buildUnOp :: UnOp -> f UnOp
        buildUnOp = pure

        buildVar :: Var n -> f (Var n)
        buildVar = pure

    mkPlate build = Plate (build pProg) (build pStmt) (build pExpr) (build pBlock) (build pVar) (build pBinOp) (build pUnOp)
