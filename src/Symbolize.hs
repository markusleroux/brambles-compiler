{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Symbolize where

import AST

import Data.Unique

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

    -- get the name associated with a given identifier (or throw)
    getSym :: Name -> m sym
    getSym name = getSymMb name >>= \case
        Just u -> return u
        Nothing -> throwUndefined name

    -- create new symbol
    createSym :: Name -> m sym




renameProgram :: MonadSymbolize m sym => Program Name -> m (Program sym)
renameProgram p = Program 
              <$> mapM renameStatement (globals p) 
              <*> mapM renameFunction  (funcs p)

renameFunction :: MonadSymbolize m sym => Func Name -> m (Func sym)
renameFunction f = do
        u <- createSym $ fName f

        -- enter new scope, no references to variables defined in new scope from outside
        (args, body) <- withScope $ do
            args <- mapM createSym $ fParams f  -- will throw if function args mask (TODO: work on masking rules)
            body <- renameBlock    $ fBody f
            return (args, body)

        return $ Func u args (fType f) body

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
renameExpr (BinOp op expl expr) = BinOp op   <$> renameExpr expl <*> renameExpr expr  -- prohibit name definitions in ops?
renameExpr (Call name exps)     = Call       <$> getSym name     <*> mapM renameExpr exps
renameExpr (Assign var exp)     = Assign     <$> createSym var   <*> renameExpr exp



-- Unique-based symbolizer using Data.Map in StateT
newtype UniqueSymbolizeM m a 
        = UniqueSymbolizeM { runUniqueSymbolizeM :: ExceptT SymbolizeException (StateT (Map String Unique) m) a }
    deriving (Functor, Applicative, Monad, MonadState (Map String Unique), MonadIO, MonadError SymbolizeException)

instance Monad m => ThrowsSymbolizeException (UniqueSymbolizeM m) where
    throwUndefined = throwError . Undefined
    throwRedefined = throwError . Redefined

instance Monad m => ScopedMonad (UniqueSymbolizeM m) where
    withScope computation = do
        outerScope <- get      -- save the outer scope
        result <- computation  -- run the computation
        put outerScope         -- restore the outer scope
        pure result

instance MonadIO m => MonadSymbolize (UniqueSymbolizeM m) Unique where
    getSymMb = gets . Map.lookup
        
    createSym name = do
        u <- gets (Map.lookup name) >>= \case
            Just u -> throwRedefined name
            Nothing -> liftIO newUnique
        modify $ Map.insert name u
        pure u

runUniqueSymbolizeT :: MonadIO m => UniqueSymbolizeM m a -> m (Either SymbolizeException a)
runUniqueSymbolizeT = 
    let defaultState = Map.empty
    in (`evalStateT` defaultState) . runExceptT . runUniqueSymbolizeM


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
       
    createSym name = do
        u <- gets (Map.lookup name . fst) >>= \case
            Just u -> throwRedefined name
            Nothing -> gets snd
        modify $ \(m, c) -> (Map.insert name c m, c+1)
        pure u


runIncrementalSymbolizeT :: Monad m => IncrementalSymbolizeM m a -> m (Either SymbolizeException a)
runIncrementalSymbolizeT = 
    let defaultState = (Map.empty, 0)
    in (`evalStateT` defaultState) . runExceptT . runIncrementalSymbolizeM

type IncrementalSymbolize = IncrementalSymbolizeM Identity
runIncrementalSymbolize = runIdentity . runIncrementalSymbolizeT

