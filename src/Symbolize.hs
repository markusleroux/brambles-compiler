module Symbolize where

import Prelude hiding (exp)

import AST

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

{-
Approach 1: mutual recursion
Approach 2: mapM
    - but how to introduce scoping?
Approach 3: keep a flag like Program (Name, IsScoped)
    - rename :: Program (Name, IsScoped) -> Program Unique
    - How to set-up IsScoped flag?
    - Will this mess with other things? e.g. parsing now needs to know about scoping?
    - Should it go in the type Name itself? e.g. ScopedName
-}



data SymbolizeException
    = Undefined Name
    | Redefined Name
    deriving (Eq)

instance Show SymbolizeException where
    show (Undefined n) = "Undefined name: " ++ show n
    show (Redefined n) = "Redefined name: " ++ show n

class Monad m => ThrowsSymbolizeException m where 
    throwUndefined :: Name -> m a
    throwRedefined :: Name -> m a


class Monad m => ScopedMonad m where
    -- block scoping
    withScope :: m a -> m a

-- all scoping is accomplished through a naming pass, which associates a unique identifier with each name
class (ThrowsSymbolizeException m, ScopedMonad m) => MonadSymbolize m sym where
    -- get the name associated with a given identifier
    getSymMb :: Name -> m (Maybe sym)
    -- create new symbol
    createSym :: Name -> m sym

    -- get the name associated with a given identifier (or throw)
    getSym :: Name -> m sym
    getSym name = getSymMb name >>= maybe (throwUndefined name) return




renameProgram :: MonadSymbolize m sym => Program Name -> m (Program sym)
renameProgram p = Program <$> mapM renameStatement (globals p) <*> mapM renameFunction  (funcs p)

renameFunction :: MonadSymbolize m sym => Func Name -> m (Func sym)
renameFunction f = do
        u <- createSym $ fName f -- function symbol will be available inside function (recursion)

        -- enter new scope, no references to variables defined in new scope from outside
        (args, body) <- withScope $ do
            args <- mapM createSym $ fParams f
            body <- renameBlock    $ fBody f
            return (args, body)

        return $ Func u args (fType f) body

-- scoping is the responsibility of the caller, since function args need to be defined in the block's scope
renameBlock :: MonadSymbolize m sym => Block Name -> m (Block sym)
renameBlock = fmap Block . mapM renameStatement . unBlock

renameStatement :: MonadSymbolize m sym => Stmt Name -> m (Stmt sym)
renameStatement (Expr exp)        = Expr <$> renameExpr exp
renameStatement (Decl name t exp) = Decl <$> createSym name <*> pure t <*> renameExpr exp

renameExpr :: MonadSymbolize m sym => Expr Name -> m (Expr sym)
renameExpr (IntLit v)           = return $ IntLit v
renameExpr (FloatLit v)         = return $ FloatLit v
renameExpr (Var v)              = Var        <$> getSym v
renameExpr (UnOp op exp)        = UnOp op    <$> renameExpr exp
renameExpr (BinOp op expl expr) = BinOp op   <$> renameExpr expl <*> renameExpr expr
renameExpr (Call name exps)     = Call       <$> getSym name     <*> mapM renameExpr exps
renameExpr (Assign var exp)     = Assign     <$> createSym var   <*> renameExpr exp
renameExpr (EBlock block)       = EBlock     <$> withScope (renameBlock block)



-- Incremental symbolizer using (Data.Map, Int) in StateT
newtype IncrementalSymbolizeM m a 
        = IncrementalSymbolizeM { runIncrementalSymbolizeM :: ExceptT SymbolizeException (StateT (Map String Int, Int) m) a }
    deriving (Functor, Applicative, Monad, MonadState (Map String Int, Int), MonadIO, MonadError SymbolizeException)

instance Monad m => ThrowsSymbolizeException (IncrementalSymbolizeM m) where
    throwUndefined = throwError . Undefined
    throwRedefined = throwError . Redefined

instance Monad m => ScopedMonad (IncrementalSymbolizeM m) where
    withScope computation = do
        outerScope <- get      -- save the outer scope
        result <- computation  -- run the computation
        put outerScope         -- restore the outer scope
        pure result

instance Monad m => MonadSymbolize (IncrementalSymbolizeM m) Int where
    getSymMb name = gets $ Map.lookup name . fst
    createSym name = state $ \(m, c) -> (c + 1, (Map.insert name (c + 1) m, c+1)) -- aliasing allowed


runIncrementalSymbolizeT :: Monad m => IncrementalSymbolizeM m a -> m (Either SymbolizeException a)
runIncrementalSymbolizeT = (`evalStateT` (Map.empty, -1)) . runExceptT . runIncrementalSymbolizeM

type IncrementalSymbolize = IncrementalSymbolizeM Identity

runIncrementalSymbolize :: IncrementalSymbolize a -> Either SymbolizeException a
runIncrementalSymbolize = runIdentity . runIncrementalSymbolizeT

