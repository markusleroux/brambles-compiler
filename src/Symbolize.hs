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

    default withScope :: MonadState s m => m a -> m a
    withScope computation = do
        outerScope <- get -- save the outer scope
        result <- computation -- run the computation
        put outerScope -- restore the outer scope
        pure result

class (ThrowsSymbolizeException m, MonadScoping m) => MonadSymbolize m sym | m -> sym where
    getSymMb :: Name -> m (Maybe sym)
    createSym :: Name -> m sym

    getSym :: Name -> m sym
    getSym name = getSymMb name >>= maybe (throwUndefined name) pure


{- Renaming Functions -}
renameProg :: MonadSymbolize m sym => Prog Name p -> m (Prog sym p)
renameProg (Globals x gs) = Globals x <$> mapM renameStmt gs

renameStmt :: MonadSymbolize m sym => Stmt Name p -> m (Stmt sym p)
renameStmt SExpr{..}   = SExpr exprX     <$> renameExpr exprExpr
renameStmt SDecl{..}   = SDecl declX     <$> mapM createSym declName <*> renameExpr declExpr -- TODO: careful of recursive definitions
renameStmt SWhile{..}  = SWhile whileX   <$> renameExpr whilePred <*> withScope (mapM renameStmt whileBody)
renameStmt SReturn{..} = SReturn returnX <$> renameExpr returnExpr

renameExpr :: MonadSymbolize m sym => Expr Name p -> m (Expr sym p)
renameExpr EIntLit{..}   = pure $ EIntLit   intLitX   intLitVal
renameExpr EFloatLit{..} = pure $ EFloatLit floatLitX floatLitVal
renameExpr EBoolLit{..}  = pure $ EBoolLit  boolLitX  boolLitVal
renameExpr EVar{..}      = EVar varX         <$> mapM getSym varVar
renameExpr EUnOp{..}     = EUnOp unX unOp    <$> renameExpr unRHS
renameExpr EBinOp{..}    = EBinOp binX binOp <$> renameExpr binLHS <*> renameExpr binRHS
renameExpr ECall{..}     = ECall callX       <$> renameExpr callName <*> mapM renameExpr callArgs
renameExpr EAssign{..}   = EAssign assignX   <$> mapM getSym assignVar <*> renameExpr assignExpr
renameExpr EIf{..}       = EIf ifX           <$> renameExpr ifPred <*> renameBlock ifThen <*> mapM renameBlock ifElseMb
renameExpr EBlock{..}    = EBlock            <$> renameBlock unBlock
renameExpr EFunc{..} = do
  u <- mapM createSym funcName -- function symbol will be available inside function (recursion)
  (args, body) <- withScope $ do
      args <- mapM (mapM createSym) funcParams
      body <- renameBlock funcBody
      return (args, body)
  pure $ EFunc funcX u args body

renameBlock :: MonadSymbolize m sym => Block Name p -> m (Block sym p)
renameBlock Block{..} = withScope $ Block blockX <$> mapM renameStmt blockBody <*> mapM renameExpr blockResult

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
    deriving anyclass (ThrowsSymbolizeException, MonadScoping)

instance Monad m => MonadSymbolize (IncrementalSymbolizeM m) Int where
    getSymMb name = gets $ Map.lookup name . fst
    createSym name = state $ \(m, x : xs) -> (x, (Map.insert name x m, xs)) -- aliasing allowed

runIncrementalSymbolizeT :: Monad m => IncrementalSymbolizeM m a -> m (Either SymbolizeException a)
runIncrementalSymbolizeT = (`evalStateT` (Map.empty, [0 ..])) . runExceptT . runIncrementalSymbolizeM

type IncrementalSymbolize = IncrementalSymbolizeM Identity

runIncrementalSymbolize :: IncrementalSymbolize a -> Either SymbolizeException a
runIncrementalSymbolize = runIdentity . runIncrementalSymbolizeT

