{-# LANGUAGE UndecidableInstances #-}
module Brambles.Frontend.Typecheck where

import Brambles.Frontend.AST
import Brambles.Frontend.Parser (SourceLoc)

import Control.Monad (unless, zipWithM_, (>=>))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.State (
    MonadState,
    StateT,
    evalStateT,
    modify,
    gets,
 )

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data TypeError
    = TypeError
    deriving (Eq, Show)


type instance XEIntLit 'Typed   = (SourceLoc, Type)
type instance XEFloatLit 'Typed = (SourceLoc, Type)
type instance XEBoolLit 'Typed  = (SourceLoc, Type)
type instance XEVar 'Typed      = (SourceLoc, Type)
type instance XEUnOp 'Typed     = (SourceLoc, Type)
type instance XEBinOp 'Typed    = (SourceLoc, Type)
type instance XECall 'Typed     = (SourceLoc, Type)
type instance XEAssign 'Typed   = (SourceLoc, Type)
type instance XEIf 'Typed       = (SourceLoc, Type)
type instance XEFunc 'Typed     = (SourceLoc, Type)

type instance XSExpr 'Typed     = (SourceLoc, Type)
type instance XSDecl 'Typed     = (SourceLoc, Type)
type instance XSWhile 'Typed    = (SourceLoc, Type)
type instance XSReturn 'Typed   = (SourceLoc, Type)

type instance XBlock 'Typed    = (SourceLoc, Type)
type instance XProg 'Typed     = SourceLoc

-- TODO: best way to make the below more generic?
class Typed t where
  getType :: t -> Type

instance Typed (Block n 'Typed) where
  getType (Block ann _ _) = snd ann

instance Typed (Expr n 'Typed) where
  getType (EIntLit ann _)    = snd ann
  getType (EFloatLit ann _)  = snd ann
  getType (EBoolLit ann _)   = snd ann
  getType (EVar ann _)       = snd ann
  getType (EUnOp ann _ _)    = snd ann
  getType (EBinOp ann _ _ _) = snd ann
  getType (ECall ann _ _)    = snd ann
  getType (EAssign ann _ _)  = snd ann
  getType (EBlock b)         = getType b
  getType (EIf ann _ _ _)    = snd ann
  getType (EFunc ann _ _ _)  = snd ann

instance Typed (Stmt n 'Typed) where
  getType (SExpr ann _)     = snd ann
  getType (SDecl ann _ _)   = snd ann
  getType (SWhile ann _ _)  = snd ann
  getType (SReturn ann _)   = snd ann


class MonadError TypeError m => ThrowsTypeException m where
  throwTypeError :: m a
  throwTypeError = throwError TypeError

class (ThrowsTypeException m, Monad m) => MonadTy m n | m -> n where
  setType :: n -> Type -> m ()
  lookupTypeMb :: n -> m (Maybe Type)

  lookupType :: n -> m Type
  lookupType = lookupTypeMb >=> maybe throwTypeError pure


inferBlock :: MonadTy m n => Block n 'Parsed -> m (Block n 'Typed)
inferBlock (Block l ss eMb) = do
  -- statements
  ss' <- mapM inferStmt ss

  -- expression maybe
  case eMb of
    Just e -> do
      e' <- inferExpr e
      pure $ Block (l, getType e') ss' (Just e')
    Nothing -> pure $ Block (l, TUnit) ss' Nothing

inferExpr :: MonadTy m n => Expr n 'Parsed -> m (Expr n 'Typed)
inferExpr (EIntLit l v)   = pure $ EIntLit   (l, TInt)   v
inferExpr (EFloatLit l v) = pure $ EFloatLit (l, TFloat) v
inferExpr (EBoolLit l v)  = pure $ EBoolLit  (l, TBool)  v
inferExpr (EUnOp l op e) = do
  e' <- inferExpr e
  let t = getType e'
  unless (isNumeric t) throwTypeError
  pure $ EUnOp (l, t) op e'
inferExpr (EBinOp l op e1 e2) = do
  e1' <- inferExpr e1
  e2' <- inferExpr e2
  unless (getType e1' == getType e2') throwTypeError
  let t = getType e1'
  case op of
    Eq -> pure $ EBinOp (l, TBool) op e1' e2'
    _ -> do
      unless (isNumeric t) throwTypeError
      pure $ EBinOp (l, t) op e1' e2'
inferExpr EBlock{..} = EBlock <$> inferBlock unBlock
inferExpr EIf{..} = do
  -- predicate
  ifPred' <- inferExpr ifPred
  unless (getType ifPred' == TBool) throwTypeError

  -- then
  ifThen' <- inferBlock ifThen

  -- else maybe
  case ifElseMb of
    Just ifElse -> do
      ifElse' <- inferBlock ifElse
      unless (getType ifElse' == getType ifThen') throwTypeError
      pure $ EIf (ifX, getType ifThen') ifPred' ifThen' (Just ifElse')
    Nothing -> pure $ EIf (ifX, TOptional $ getType ifThen') ifPred' ifThen' Nothing

inferExpr (EFunc (l, ann) n ps b) = do
  zipWithM_ setType (unVar <$> ps) (paramT ann)
  b' <- inferBlock b

  let earlyReturnT = [getType e | SReturn _ e <- blockBody b']
      returnExprT = maybe TUnit getType $ blockResult b'

  case earlyReturnT ++ [returnExprT] of
    [] -> unless (returnT ann == TUnit) throwTypeError
    xs -> unless (all (== returnT ann) xs) throwTypeError

  setType (unVar n) ann
  pure $ EFunc (l, ann) n ps b'

inferExpr EAssign{..} = do
  t <- lookupType $ unVar assignVar  -- TODO: inferVar?
  assignExpr' <- inferExpr assignExpr
  unless (t == getType assignExpr') throwTypeError
  pure $ EAssign (assignX, t) assignVar assignExpr'
inferExpr ECall{..} = do
  callName' <- inferExpr callName
  case getType callName' of
    TCallable{..} -> do
      callArgs' <- mapM inferExpr callArgs
      unless (paramT == (getType <$> callArgs')) throwTypeError
      pure $ ECall (callX, getType callName') callName' callArgs'
    _ -> throwTypeError
inferExpr EVar{..} = do
  t <- lookupType $ unVar varVar
  pure $ EVar (varX, t) varVar


inferStmt :: MonadTy m n => Stmt n 'Parsed -> m (Stmt n 'Typed)
inferStmt (SExpr l e) = SExpr (l, TUnit) <$> inferExpr e
inferStmt (SDecl (l, ann) v@(V n) e) = do
  e' <- inferExpr e
  unless (ann == getType e') throwTypeError
  setType n ann
  pure $ SDecl (l, TUnit) v e'
inferStmt SWhile{..} = do
  -- predicate
  whilePred' <- inferExpr whilePred
  unless (getType whilePred' == TBool) throwTypeError

  -- body
  SWhile (whileX, TUnit) whilePred' <$> mapM inferStmt whileBody
inferStmt (SReturn l e) = SReturn (l, TUnit) <$> inferExpr e

inferProg :: MonadTy m n => Prog n 'Parsed -> m (Prog n 'Typed)
inferProg (Globals l ss) = Globals l <$> mapM inferStmt ss


-- TODO: this isn't really generic in a, a should be Node n 'Parsed
newtype Ord n => TypecheckingM m n a = TypecheckingM 
  { runTypecheckingM :: 
      ExceptT TypeError 
        (StateT (Map n Type) m) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError TypeError, MonadState (Map n Type))
  deriving anyclass (ThrowsTypeException)

instance (Ord n, Monad m) => MonadTy (TypecheckingM m n) n where
  setType n = modify . Map.insert n
  lookupTypeMb = gets . Map.lookup
  

runTypecheckingT :: (Ord n, Monad m) => TypecheckingM m n a -> m (Either TypeError a)
runTypecheckingT = (`evalStateT` Map.empty) . runExceptT . runTypecheckingM

type Typechecking n a = TypecheckingM Identity n a

runTypechecking :: Ord n => Typechecking n a -> Either TypeError a
runTypechecking = runIdentity . runTypecheckingT

-- Approach Two: Generate constraint problem and elaboration, solve constraints and fill in elaboration a la Haskell (https://www.youtube.com/watch?v=-TJGhGa04F8)

