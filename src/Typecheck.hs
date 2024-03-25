{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Typecheck where

import AST

import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Control.Monad.Except (
    MonadError,
    throwError,
 )

data TypeError
    = TypeError
    deriving (Eq)

instance Show TypeError where
    show TypeError = undefined


-- Approach One: solve while iterating
-- TODO: store type annotations as we go (add ann to each node in AST?)
--    we probably want to remove the type annotations from the AST, should we use a completely new tree?
class BidirTyped a where
  infer :: MonadError TypeError m => a -> m Type

  check :: MonadError TypeError m => Type -> a -> m Type
  check t e = do
    tInfered <- infer e
    if t == tInfered
      then pure t
      else throwError TypeError

instance BidirTyped (Expr n) where
  infer (EIntLit _) = pure TInt
  infer (EFloatLit _) = pure TFloat
  infer (EBoolLit _) = pure TBool
  -- TODO: this isn't really propagating information in both directions the way we would like:
  --    if we know from another context what e should be, we should push this information down into e
  infer (EUnOp _ e) = infer e >>= \case
    TInt -> pure TInt
    TFloat -> pure TFloat
    _ -> throwError TypeError
  infer EBinOp{..} = do
    tBinLHS <- infer binLHS
    tBinRHS <- check tBinLHS binRHS  -- TODO: order dependent
    case bOp of
      Eq -> pure TBool
      _ -> pure tBinRHS
  infer EIf{..} = infer ifCond >>= \case
    TBool -> do
      tIfBody <- infer ifBody
      case ifElseMb of
        Just ifElse -> check tIfBody ifElse  -- TODO: order dependent
        Nothing -> pure tIfBody
    _ -> throwError TypeError
  infer (EBlock es) = infer es

  -- TODO: require lookups
  infer (EVar v) = undefined
  infer ECall{..} = undefined
  infer EAssign{..} = undefined

instance BidirTyped (Stmt n) where
  infer :: MonadError TypeError m => Stmt n -> m Type
  infer (SExpr e) = infer e
  infer SDecl{..} = check declT declV
  infer SWhile{..} = infer whileCond >>= \case
    TBool -> infer whileBody
    _ -> throwError TypeError
  infer (SReturn e) = infer e
  infer (SFunc _ _ TCallable{..} fBody) = check returnT fBody  -- TODO: what about returns lower down in AST? e.g. in scoping block
  infer (SFunc _ _ _ _) = throwError TypeError

instance BidirTyped (Block n) where
  infer :: forall m. MonadError TypeError m => Block n -> m Type
  -- TODO: order dependent
  infer (Block stmts) = fromMaybe TUnit <$> (foldM collectReturns Nothing stmts :: m (Maybe Type))
    where
      collectReturns Nothing (SReturn e) = Just <$> infer e
      collectReturns (Just t) (SReturn e) = Just <$> check t e
      collectReturns tMb stmt = infer stmt >> pure tMb

instance BidirTyped (Prog n) where
  infer (Globals stmts) = mapM_ infer stmts >> pure TUnit


-- Approach Two: Generate constraint problem and elaboration, solve constraints and fill in elaboration a la Haskell (https://www.youtube.com/watch?v=-TJGhGa04F8)

