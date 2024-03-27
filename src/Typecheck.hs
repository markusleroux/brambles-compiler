{-# LANGUAGE UndecidableInstances #-}
module Typecheck where

import AST
import Parser (SourceLoc)

import Control.Monad (unless)
import Control.Monad.Except (
    MonadError,
    throwError
 )

data TypeError
    = TypeError
    deriving (Eq)

instance Show TypeError where
    show TypeError = undefined


type instance XEIntLit 'Typed = (SourceLoc, Type)
type instance XEFloatLit 'Typed = (SourceLoc, Type)
type instance XEBoolLit 'Typed = (SourceLoc, Type)
type instance XEVar 'Typed = (SourceLoc, Type)
type instance XEUnOp 'Typed = (SourceLoc, Type)
type instance XEBinOp 'Typed = (SourceLoc, Type)
type instance XECall 'Typed = (SourceLoc, Type)
type instance XEAssign 'Typed = (SourceLoc, Type)
type instance XEBlock 'Typed = (SourceLoc, Type)
type instance XEIf 'Typed = (SourceLoc, Type)

type instance XSExpr 'Typed = (SourceLoc, Type)
type instance XSDecl 'Typed = (SourceLoc, Type)
type instance XSWhile 'Typed = (SourceLoc, Type)
type instance XSReturn 'Typed = (SourceLoc, Type)
type instance XSFunc 'Typed = (SourceLoc, Type)

type instance XBlock 'Typed = (SourceLoc, Type)

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
  getType (EBlock ann _) = snd ann
  getType (EIf ann _ _ _) = snd ann

instance Typed (Stmt n 'Typed) where
  getType (SExpr ann _) = snd ann
  getType (SDecl ann _ _) = snd ann
  getType (SWhile ann _ _) = snd ann
  getType (SReturn ann _) = snd ann
  getType (SFunc ann _ _ _) = snd ann

instance Typed (Block n 'Typed) where
  getType (Block ann _) = snd ann



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
inferExpr (EBlock l b) = inferBlock b >>= \b' -> pure $ EBlock (l, getType b') b'
inferExpr (EIf l c t eMb) = do
  c' <- inferExpr c
  unless (getType c' == TBool) $ throwError TypeError
  t' <- inferBlock t
  case eMb of
    Just e -> do
      e' <- inferBlock e
      unless (getType t' == getType e') $ throwError TypeError
      pure $ EIf (l, getType t') c' t' (Just e')
    Nothing -> pure $ EIf (l, TOptional $ getType t') c' t' Nothing
-- TODO: require lookups
inferExpr (EAssign _ _ _) = undefined
inferExpr (ECall _ _ _) = undefined
inferExpr (EVar _ _) = undefined

inferStmt :: MonadError TypeError m => Stmt n 'Parsed -> m (Stmt n 'Typed)
inferStmt (SExpr l e) = inferExpr e >>= \e' -> pure $ SExpr (l, getType e') e'
inferStmt (SDecl (l, ann) n e) = do
  e' <- inferExpr e
  unless (ann == getType e') $ throwError TypeError
  pure $ SDecl (l, ann) n e'
inferStmt (SWhile l c b) = do
  c' <- inferExpr c
  unless (getType c' == TBool) $ throwError TypeError
  b' <- inferBlock b
  pure $ SWhile (l, getType b') c' b'
inferStmt (SReturn l e) = do
  e' <- inferExpr e
  pure $ SReturn (l, getType e') e'
inferStmt (SFunc (l, ann) n ps b) = do
  b' <- inferBlock b
  unless (ann == getType b') $ throwError TypeError
  pure $ SFunc (l, ann) n ps b'

inferBlock :: MonadError TypeError m => Block n 'Parsed -> m (Block n 'Typed)
inferBlock (Block l ss) = do
  ss' <- mapM inferStmt ss
  case [rt | SReturn (_, rt) _ <- ss'] of
    [] -> pure $ Block (l, TUnit) []
    rt:rts -> do
      unless (all (==rt) rts) $ throwError TypeError
      pure $ Block (l, rt) ss'

inferProg :: MonadError TypeError m => Prog n 'Parsed -> m (Prog n 'Typed)
inferProg (Globals l ss) = Globals l <$> mapM inferStmt ss


-- Approach Two: Generate constraint problem and elaboration, solve constraints and fill in elaboration a la Haskell (https://www.youtube.com/watch?v=-TJGhGa04F8)

