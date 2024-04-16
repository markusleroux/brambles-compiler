module Brambles.Frontend.Symbolize where

import Protolude

import Brambles.Frontend.AST

import qualified Data.Map as Map

-- all scoping is accomplished through a naming pass, which associates a (globally) unique identifier with each name

{- Error Handling -}
data SymbolizeException
    = Undefined Name
    | Redefined Name
    deriving (Eq, Show)

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
        outerScope <- get     -- save the outer scope
        result <- computation -- run the computation
        put outerScope        -- restore the outer scope
        pure result

class (ThrowsSymbolizeException m, MonadScoping m) => MonadSymbolize m sym | m -> sym where
    getSymMb :: Name -> m (Maybe sym)
    createSym :: Name -> m sym

    getSym :: Name -> m sym
    getSym name = getSymMb name >>= maybe (throwUndefined name) pure


{- Renaming Functions -}
renameModule :: MonadSymbolize m sym => Module Name p -> m (Module sym p)
renameModule Module{..} = do
  -- load top-level function names first
  funcNames <- mapM (mapM createSym . funcName) moduleFuncs
  Module moduleX 
    <$> mapM renameStmt moduleGlobals 
    <*> zipWithM renameTopLevelFunc funcNames moduleFuncs

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
renameExpr EFunc{..}     = EFunc             <$> renameFunc unFunc

renameFunc :: MonadSymbolize m sym => Func Name p -> m (Func sym p)
renameFunc f@Func{..} = do
  n <- mapM createSym funcName
  renameTopLevelFunc n f

renameTopLevelFunc 
  :: MonadSymbolize m sym 
  => Var sym
  -> Func Name p 
  -> m (Func sym p)
renameTopLevelFunc u Func{..} = do
  (args, body) <- withScope $ do
      args <- mapM (mapM createSym) funcParams
      body <- renameBlock funcBody
      return (args, body)
  pure $ Func funcX u args body

renameBlock :: MonadSymbolize m sym => Block Name p -> m (Block sym p)
renameBlock Block{..} = withScope $ Block blockX 
  <$> mapM renameStmt blockBody 
  <*> mapM renameExpr blockResult

{- 
 - Renaming implementation using (Data.Map, [Int]) in StateT 
 -   names are integers 0...
 -}
newtype IncrementalSymbolizeM m a = IncrementalSymbolizeM
    { runIncrementalSymbolizeM :: 
        ExceptT SymbolizeException 
          (StateT (Map Name Int, [Int]) m) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadState (Map Name Int, [Int]), MonadIO, MonadError SymbolizeException)
    deriving anyclass (ThrowsSymbolizeException, MonadScoping)

instance Monad m => MonadSymbolize (IncrementalSymbolizeM m) Int where
    getSymMb name = gets $ Map.lookup name . fst
    createSym name = state $ \(m, x : xs) -> (x, (Map.insert name x m, xs)) -- aliasing allowed

runIncrementalSymbolizeT :: Monad m => IncrementalSymbolizeM m a -> m (Either SymbolizeException a)
runIncrementalSymbolizeT = (`evalStateT` (Map.empty, [0 ..])) . runExceptT . runIncrementalSymbolizeM

type IncrementalSymbolize = IncrementalSymbolizeM Identity

runIncrementalSymbolize :: IncrementalSymbolize a -> Either SymbolizeException a
runIncrementalSymbolize = runIdentity . runIncrementalSymbolizeT

