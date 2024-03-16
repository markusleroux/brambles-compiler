{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module AST where

import Data.Generics.Multiplate

data UnOp
    = Neg
    | Pos
    deriving (Eq, Ord, Show)

data BinOp
    = Add
    | Sub
    | Mult
    | Div
    deriving (Eq, Ord, Show)

data Type
    = TInt
    | TFloat
    | TCallable {paramT :: [Type], returnT :: Type}
    deriving (Eq, Ord, Show)

type Name = String

newtype Var n = V n
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Expr n
    = IntLit Integer
    | FloatLit Double
    | Var (Var n)
    | UnOp UnOp (Expr n)
    | BinOp BinOp (Expr n) (Expr n)
    | Call (Var n) [Expr n]
    | Assign (Var n) (Expr n)
    | EBlock (Block n)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Stmt n
    = Expr (Expr n)
    | Decl {sName :: Var n, sType :: Type, sVal :: Expr n}
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Block n = Block {unBlock :: [Stmt n]}
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Func n = Func
    { fName :: Var n
    , fParams :: [Var n]
    , fType :: Type
    , fBody :: Block n
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Program n = Program
    { globals :: [Stmt n]
    , funcs :: [Func n]
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


data Plate n m f = Plate
    { prog :: Program n -> f (Program n)
    , func :: Func n -> f (Func n)
    , block :: Block n -> f (Block n)
    , stmt :: Stmt n -> f (Stmt n)
    , expr :: Expr n -> f (Expr n)
    , type_ :: Type -> f Type
    , binOp :: BinOp -> f BinOp
    , unOp :: UnOp -> f UnOp
    , var :: Var n -> f (Var n)
    }

instance Multiplate (Plate n) where
  multiplate :: forall f. Applicative f => Plate n f -> Plate n f
  multiplate child = Plate buildProg buildFunc buildBlock buildStmt buildExpr buildType buildBinOp buildUnOp buildVar
    where
      buildProg :: Program n -> f (Program n)
      buildProg (Program g f) = Program <$> (stmt child `traverse` g) <*> (func child `traverse` f)

      buildFunc :: Func n -> f (Func n)
      buildFunc (Func{..}) = Func <$> var child fName <*> (var child `traverse` fParams) <*> type_ child fType <*> block child fBody

      buildBlock :: Block n -> f (Block n)
      buildBlock (Block stmts) = Block <$> (stmt child `traverse` stmts)

      buildStmt :: Stmt n -> f (Stmt n)
      buildStmt (Expr e) = Expr <$> expr child e
      buildStmt (Decl{..}) = Decl <$> var child sName <*> type_ child sType <*> expr child sVal

      buildExpr :: Expr n -> f (Expr n)
      buildExpr (UnOp op e) = UnOp op <$> expr child e
      buildExpr (BinOp op e0 e1) = BinOp op <$> expr child e0 <*> expr child e1
      buildExpr (Call n es) = Call <$> var child n <*> (expr child `traverse` es)
      buildExpr (Assign n e) = Assign <$> var child n <*> expr child e
      buildExpr (EBlock b) = EBlock <$> block child b
      buildExpr (Var v) = Var <$> var child v
      buildExpr v = pure v

      buildType :: Type -> f Type
      buildType (TCallable{..}) = TCallable <$> (type_ child `traverse` paramT) <*> type_ child returnT
      buildType t = pure t

      buildBinOp :: BinOp -> f BinOp
      buildBinOp = pure

      buildUnOp :: UnOp -> f UnOp
      buildUnOp = pure

      buildVar :: Var n -> f (Var n)
      buildVar = pure

  mkPlate build = Plate (build prog) 
                        (build func) 
                        (build block) 
                        (build stmt) 
                        (build expr) 
                        (build type_) 
                        (build binOp) 
                        (build unOp) 
                        (build var)


