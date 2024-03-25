{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Symbolize where

import AST
import Control.Monad.Except (
    ExceptT,
    MonadError,
    runExceptT,
    throwError,
 )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (
    MonadState,
    StateT,
    evalStateT,
    get,
    gets,
    put,
    state,
 )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (exp)

-- all scoping is accomplished through a naming pass, which associates a (globally) unique identifier with each name

{- Error Handling -}
data SymbolizeException
    = Undefined Name
    | Redefined Name
    deriving (Eq)

instance Show SymbolizeException where
    show (Undefined n) = "Undefined name: " ++ show n
    show (Redefined n) = "Redefined name: " ++ show n

class MonadError SymbolizeException m => ThrowsSymbolizeException m where
    throwUndefined :: Name -> m a
    throwUndefined = throwError . Undefined

    throwRedefined :: Name -> m a
    throwRedefined = throwError . Redefined


{- Renaming API -}
class Monad m => MonadScoping m where
    withScope :: m a -> m a

class (ThrowsSymbolizeException m, MonadScoping m) => MonadSymbolize m sym where
    getSymMb :: Name -> m (Maybe sym)
    createSym :: Name -> m sym

    getSym :: Name -> m sym
    getSym name = getSymMb name >>= maybe (throwUndefined name) return


{- Renaming Functions -}
renameProg :: MonadSymbolize m sym => Prog Name 'Parsed -> m (Prog sym 'Parsed)
renameProg (Globals x gs) = Globals x <$> mapM renameStmt gs

renameBlock :: MonadSymbolize m sym => Block Name 'Parsed -> m (Block sym 'Parsed)
renameBlock (Block x gs) = Block x <$> mapM renameStmt gs

renameStmt :: MonadSymbolize m sym => Stmt Name 'Parsed -> m (Stmt sym 'Parsed)
renameStmt (SExpr x e) = SExpr x <$> renameExpr e
renameStmt SDecl{..} = SDecl declExt <$> mapM createSym declName <*> renameExpr declV -- TODO: careful of recursive definitions
renameStmt SWhile{..} = SWhile whileExt <$> renameExpr whileCond <*> renameBlock whileBody
renameStmt (SReturn x e) = SReturn x <$> renameExpr e
renameStmt SFunc{..} = do
  u <- mapM createSym fName -- function symbol will be available inside function (recursion)
  (args, body) <- withScope $ do
      args <- mapM (mapM createSym) fParams
      body <- renameBlock fBody
      return (args, body)

  return $ SFunc fExt u args body

renameExpr :: MonadSymbolize m sym => Expr Name 'Parsed -> m (Expr sym 'Parsed)
renameExpr (EIntLit x v) = pure $ EIntLit x v
renameExpr (EFloatLit x v) = pure $ EFloatLit x v
renameExpr (EBoolLit x v) = pure $ EBoolLit x v
renameExpr (EVar x v) = EVar x <$> mapM getSym v
renameExpr EUnOp{..} = EUnOp unExt unOp <$> renameExpr unRHS
renameExpr EBinOp{..} = EBinOp binExt binOp <$> renameExpr binLHS <*> renameExpr binRHS
renameExpr ECall{..} = ECall callExt <$> mapM getSym callFunc <*> mapM renameExpr callArgs
renameExpr EAssign{..} = EAssign assignExt <$> mapM getSym assignVar <*> renameExpr assignVal
renameExpr (EBlock x b) = EBlock x <$> withScope (renameBlock b)
renameExpr EIf{..} = EIf ifExt <$> renameExpr ifCond <*> renameBlock ifBody <*> mapM renameBlock ifElseMb


{- 
 - Renaming implementation using (Data.Map, [Int]) in StateT 
 -   names are integers 0...
 -}
newtype IncrementalSymbolizeM m a = IncrementalSymbolizeM
    { runIncrementalSymbolizeM :: 
        ExceptT SymbolizeException 
          (StateT (Map String Int, [Int]) m) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadState (Map String Int, [Int]), MonadIO, MonadError SymbolizeException)
    deriving anyclass (ThrowsSymbolizeException)

instance Monad m => MonadScoping (IncrementalSymbolizeM m) where
    withScope computation = do
        outerScope <- get -- save the outer scope
        result <- computation -- run the computation
        put outerScope -- restore the outer scope
        pure result

instance Monad m => MonadSymbolize (IncrementalSymbolizeM m) Int where
    getSymMb name = gets $ Map.lookup name . fst
    createSym name = state $ \(m, x : xs) -> (x, (Map.insert name x m, xs)) -- aliasing allowed

runIncrementalSymbolizeT :: Monad m => IncrementalSymbolizeM m a -> m (Either SymbolizeException a)
runIncrementalSymbolizeT = (`evalStateT` (Map.empty, [0 ..])) . runExceptT . runIncrementalSymbolizeM

type IncrementalSymbolize = IncrementalSymbolizeM Identity

runIncrementalSymbolize :: IncrementalSymbolize a -> Either SymbolizeException a
runIncrementalSymbolize = runIdentity . runIncrementalSymbolizeT
