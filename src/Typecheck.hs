{-# LANGUAGE UndecidableInstances #-}
module Typecheck where

import AST
import Parser (SourceLoc)

import Control.Monad (unless)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Except (
    MonadError,
    ExceptT,
    runExceptT,
    throwError
 )

import Control.Exception (Exception)

data TypeError
    = TypeError
    deriving (Eq, Show)

instance Exception TypeError


type instance XEIntLit 'Typed   = (SourceLoc, Type)
type instance XEFloatLit 'Typed = (SourceLoc, Type)
type instance XEBoolLit 'Typed  = (SourceLoc, Type)
type instance XEVar 'Typed      = (SourceLoc, Type)
type instance XEUnOp 'Typed     = (SourceLoc, Type)
type instance XEBinOp 'Typed    = (SourceLoc, Type)
type instance XECall 'Typed     = (SourceLoc, Type)
type instance XEAssign 'Typed   = (SourceLoc, Type)
type instance XEBlock 'Typed    = (SourceLoc, Type)
type instance XEIf 'Typed       = (SourceLoc, Type)
type instance XEFunc 'Typed     = (SourceLoc, Type)

type instance XSExpr 'Typed     = (SourceLoc, Type)
type instance XSDecl 'Typed     = (SourceLoc, Type)
type instance XSWhile 'Typed    = (SourceLoc, Type)
type instance XSReturn 'Typed   = (SourceLoc, Type)

type instance XProg 'Typed = SourceLoc

-- TODO: best way to make the below more generic?
class Typed t where
  getType :: t -> Type

instance Typed (Expr n 'Typed) where
  getType (EIntLit ann _) = snd ann
  getType (EFloatLit ann _) = snd ann
  getType (EBoolLit ann _) = snd ann
  getType (EVar ann _) = snd ann
  getType (EUnOp ann _ _) = snd ann
  getType (EBinOp ann _ _ _) = snd ann
  getType (ECall ann _ _) = snd ann
  getType (EAssign ann _ _) = snd ann
  getType (EBlock ann _ _) = snd ann
  getType (EIf ann _ _ _) = snd ann
  getType (EFunc ann _ _ _) = snd ann

instance Typed (Stmt n 'Typed) where
  getType (SExpr ann _) = snd ann
  getType (SDecl ann _ _) = snd ann
  getType (SWhile ann _ _) = snd ann
  getType (SReturn ann _) = snd ann


inferExpr :: MonadError TypeError m => Expr n 'Parsed -> m (Expr n 'Typed)
inferExpr (EIntLit l v) = pure $ EIntLit (l, TInt) v
inferExpr (EFloatLit l v) = pure $ EFloatLit (l, TFloat) v
inferExpr (EBoolLit l v) = pure $ EBoolLit (l, TBool) v
inferExpr (EUnOp l op e) = do
  e' <- inferExpr e
  let t = getType e'
  unless (isNumeric t) $ throwError TypeError
  pure $ EUnOp (l, t) op e'
inferExpr (EBinOp l op e1 e2) = do
  e1' <- inferExpr e1
  e2' <- inferExpr e2
  unless (getType e1' == getType e2') $ throwError TypeError
  let t = getType e1'
  case op of
    Eq -> pure $ EBinOp (l, TBool) op e1' e2'
    _ -> do
      unless (isNumeric t) $ throwError TypeError
      pure $ EBinOp (l, t) op e1' e2'
inferExpr (EBlock l ss eMb) = do
  ss' <- mapM inferStmt ss
  case eMb of
    Just e -> do
      e' <- inferExpr e
      pure $ EBlock (l, getType e') ss' (Just e')
    Nothing -> pure $ EBlock (l, TUnit) ss' Nothing
inferExpr EIf{..} = do
  ifPred' <- inferExpr ifPred
  unless (getType ifPred' == TBool) $ throwError TypeError
  ifThen' <- inferExpr ifThen
  case ifElseMb of
    Just ifElse -> do
      ifElse' <- inferExpr ifElse
      unless (getType ifElse' == getType ifThen') $ throwError TypeError
      pure $ EIf (ifX, getType ifThen') ifPred' ifThen' (Just ifElse')
    Nothing -> pure $ EIf (ifX, TOptional $ getType ifThen') ifPred' ifThen' Nothing
inferExpr (EFunc (l, ann) n ps b) = do
  b' <- mapM inferStmt b  -- TODO: should be able to lookup param types
  case [getType e | SReturn _ e <- b'] of
    [] -> unless (returnT ann == TUnit) $ throwError TypeError
    xs -> unless (all (== returnT ann) xs) $ throwError TypeError
  pure $ EFunc (l, ann) n ps b'

-- TODO: require lookups
inferExpr EAssign{..} = undefined
inferExpr ECall{..} = undefined
inferExpr EVar{..} = undefined

inferStmt :: MonadError TypeError m => Stmt n 'Parsed -> m (Stmt n 'Typed)
inferStmt (SExpr l e) = SExpr (l, TUnit) <$> inferExpr e
inferStmt (SDecl (l, ann) n e) = do
  e' <- inferExpr e
  unless (ann == getType e') $ throwError TypeError
  pure $ SDecl (l, TUnit) n e'
inferStmt SWhile{..} = do
  whilePred' <- inferExpr whilePred
  unless (getType whilePred' == TBool) $ throwError TypeError
  SWhile (whileX, TUnit) whilePred' <$> mapM inferStmt whileBody
inferStmt (SReturn l e) = SReturn (l, TUnit) <$> inferExpr e

inferProg :: MonadError TypeError m => Prog n 'Parsed -> m (Prog n 'Typed)
inferProg (Globals l ss) = Globals l <$> mapM inferStmt ss

newtype TypecheckingM m a = TypecheckingM { runTypecheckingM :: ExceptT TypeError m a }
  deriving newtype (Functor, Applicative, Monad, MonadError TypeError)

runTypecheckingT :: Monad m => TypecheckingM m a -> m (Either TypeError a)
runTypecheckingT = runExceptT . runTypecheckingM

type Typechecking a = TypecheckingM Identity a

runTypechecking :: Typechecking a -> Either TypeError a
runTypechecking = runIdentity . runTypecheckingT

-- Approach Two: Generate constraint problem and elaboration, solve constraints and fill in elaboration a la Haskell (https://www.youtube.com/watch?v=-TJGhGa04F8)

