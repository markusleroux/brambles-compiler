{-# LANGUAGE ScopedTypeVariables, DeriveAnyClass, DerivingStrategies, DerivingVia #-}
module Symbolize where

import Data.Generics.Multiplate
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

class Monad m => MonadScoping m where
    -- block scoping
    withScope :: m a -> m a

-- all scoping is accomplished through a naming pass, which associates a unique identifier with each name
class (ThrowsSymbolizeException m, MonadScoping m) => MonadSymbolize m sym where
    getSymMb :: Name -> m (Maybe sym)
    createSym :: Name -> m sym

    getSym :: Name -> m sym
    getSym name = getSymMb name >>= maybe (throwUndefined name) return


scopingPlate :: forall m. MonadScoping m => Plate Name m -> Plate Name m
scopingPlate p = undefined

-- TODO: How to change type of symbol?
--       MonadSymbolize m Name, then map to int?
--       OR Constant (Block sym)
renamePlate :: forall m. MonadSymbolize m Name => Plate Name m
renamePlate = (mkPlate (\p -> p renameRecurse)) { func = renameFunc, stmt = renameStmt , expr = renameExpr, var = mapM getSym }
  where
    renameRecurse = multiplate renamePlate

    renameFunc Func{..} = do
      u <- mapM createSym fName -- function symbol will be available inside function (recursion)

      (args, body) <- withScope $ do
          args <- mapM (mapM createSym) fParams
          body <- block renameRecurse fBody
          return (args, body)

      return $ Func u args fType body

    renameStmt (Decl n t e) = Decl <$> mapM createSym n <*> type_ renameRecurse t <*> expr renameRecurse e
    renameStmt s = stmt renameRecurse s

    renameExpr (EBlock b) = withScope (EBlock <$> block renameRecurse b)
    renameExpr v = expr renameRecurse v

{-
 - Incremental symbolizer using (Data.Map, [Int]) in StateT
 -}
newtype IncrementalSymbolizeM m a = IncrementalSymbolizeM
    { runIncrementalSymbolizeM :: ExceptT SymbolizeException (StateT (Map String Int, [Int]) m) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (Map String Int, [Int]), MonadIO, MonadError SymbolizeException)
  deriving anyclass ThrowsSymbolizeException

instance Monad m => MonadScoping (IncrementalSymbolizeM m) where
    withScope computation = do
        outerScope <- get -- save the outer scope
        result <- computation -- run the computation
        put outerScope -- restore the outer scope
        pure result

instance Monad m => MonadSymbolize (IncrementalSymbolizeM m) Int where
    getSymMb name = gets $ Map.lookup name . fst
    createSym name = state $ \(m, x:xs) -> (x, (Map.insert name x m, xs)) -- aliasing allowed

instance Monad m => MonadSymbolize (IncrementalSymbolizeM m) Name where
    getSymMb name = gets $ fmap show . Map.lookup name . fst
    createSym name = state $ \(m, x:xs) -> (show x, (Map.insert name x m, xs)) -- aliasing allowed

runIncrementalSymbolizeT :: Monad m => IncrementalSymbolizeM m a -> m (Either SymbolizeException a)
runIncrementalSymbolizeT = (`evalStateT` (Map.empty, [0..])) . runExceptT . runIncrementalSymbolizeM

type IncrementalSymbolize = IncrementalSymbolizeM Identity

runIncrementalSymbolize :: IncrementalSymbolize a -> Either SymbolizeException a
runIncrementalSymbolize = runIdentity . runIncrementalSymbolizeT

