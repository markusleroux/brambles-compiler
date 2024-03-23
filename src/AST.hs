{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module AST where

import Data.Generics.Multiplate (Multiplate(..))


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
    = EIntLit Integer
    | EFloatLit Double
    | EVar (Var n)
    | EUnOp { uOp :: UnOp, unRHS :: Expr n }
    | EBinOp { bOp :: BinOp, binLHS :: Expr n, binRHS :: Expr n }
    | ECall { callFunc :: Var n, callArgs :: [Expr n] }
    | EAssign { assignVar :: Var n, assignVal :: Expr n }
    | EBlock (Block n)
    | EIf { ifCond :: Expr n, ifBody :: Block n, ifElseMb :: Maybe (Block n) }  -- TODO
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Stmt n
    = SExpr (Expr n)
    | SDecl { declName :: Var n, declT :: Type, declV :: Expr n }
    | SWhile { whileCond :: Expr n, whileBody :: Block n }  -- TODO
    | SReturn (Expr n)  -- TODO
    | SFunc { fName :: Var n , fParams :: [Var n] , fType :: Type , fBody :: Block n }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Block n = Block [Stmt n]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Prog n = Globals [Stmt n]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)



data Plate n f = Plate
    { prog  :: Prog n  -> f (Prog n)
    , block :: Block n -> f (Block n)
    , stmt  :: Stmt n  -> f (Stmt n)
    , expr  :: Expr n  -> f (Expr n)
    , var   :: Var n   -> f (Var n)
    , typ   :: Type    -> f Type
    , binOp :: BinOp   -> f BinOp
    , unOp  :: UnOp    -> f UnOp
    }

instance Multiplate (Plate n) where
  multiplate :: forall f. Applicative f => Plate n f -> Plate n f
  multiplate child = Plate buildProg buildBlock buildStmt buildExpr buildVar buildType buildBinOp buildUnOp
    where
      buildProg :: Prog n -> f (Prog n)
      buildProg (Globals ss) = Globals <$> (stmt child `traverse` ss)

      buildBlock :: Block n -> f (Block n)
      buildBlock (Block stmts) = Block <$> (stmt child `traverse` stmts)

      buildStmt :: Stmt n -> f (Stmt n)
      buildStmt (SExpr e)    = SExpr <$> expr child e
      buildStmt (SDecl{..})  = SDecl <$> var child declName <*> typ child declT <*> expr child declV
      buildStmt (SWhile{..}) = SWhile <$> expr child whileCond <*> block child whileBody
      buildStmt (SReturn e)  = SReturn <$> expr child e
      buildStmt (SFunc{..})  = SFunc <$> var child fName <*> (var child `traverse` fParams) <*> typ child fType <*> block child fBody

      buildExpr :: Expr n -> f (Expr n)
      buildExpr (EUnOp{..})   = EUnOp uOp <$> expr child unRHS
      buildExpr (EBinOp{..})  = EBinOp bOp <$> expr child binLHS <*> expr child binRHS
      buildExpr (EVar v)      = EVar <$> var child v
      buildExpr (ECall{..})   = ECall <$> var child callFunc <*> (expr child `traverse` callArgs)
      buildExpr (EAssign{..}) = EAssign <$> var child assignVar <*> expr child assignVal
      buildExpr (EBlock b)    = EBlock <$> block child b
      buildExpr (EIf{..})     = EIf <$> expr child ifCond <*> block child ifBody <*> (block child `traverse` ifElseMb)
      buildExpr v             = pure v

      buildType :: Type -> f Type
      buildType (TCallable{..}) = TCallable <$> (typ child `traverse` paramT) <*> typ child returnT
      buildType t = pure t

      buildBinOp :: BinOp -> f BinOp
      buildBinOp = pure

      buildUnOp :: UnOp -> f UnOp
      buildUnOp = pure

      buildVar :: Var n -> f (Var n)
      buildVar = pure

  mkPlate build = Plate (build prog) 
                        (build block) 
                        (build stmt) 
                        (build expr) 
                        (build var)
                        (build typ) 
                        (build binOp) 
                        (build unOp) 


