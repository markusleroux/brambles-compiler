{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Codegen 
  ( exprToLLVM
  , toLLVM
  , LLVM.AST.Module
  , CodegenError
  , optimize
  , jit
  , run
  )
  where

import qualified AST
import Typecheck (getType)
import Symbolize (MonadScoping, withScope)

import Foreign.Ptr

import qualified LLVM.AST
import qualified LLVM.AST.Type as LLVM.Type
import qualified LLVM.AST.Typed as LLVM.Type
import qualified LLVM.AST.Constant as LLVM.AST
import qualified LLVM.AST.IntegerPredicate as LLVM.AST.IP
import qualified LLVM.AST.FloatingPointPredicate as LLVM.AST.FP

import qualified LLVM.IRBuilder.Constant as LLVMC
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM

import LLVM.Passes
import LLVM.Analysis
import LLVM.Module
import LLVM.Context
import LLVM.ExecutionEngine as EE

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State.Strict
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

import qualified Data.ByteString as B
import Data.ByteString.Short (ShortByteString)

data CodegenError = CodegenError
  deriving (Show)

class MonadError CodegenError m => ThrowsCodegenError m where
  throwCodegenError :: m a
  throwCodegenError = throwError CodegenError

instance ThrowsCodegenError m => ThrowsCodegenError (LLVM.IRBuilderT m)


instance MonadScoping m => MonadScoping (LLVM.IRBuilderT m) where
    withScope (LLVM.IRBuilderT irState) = LLVM.IRBuilderT . StateT $ withScope . runStateT irState

class (ThrowsCodegenError m, MonadScoping m, Ord n) => MonadSymbolTable m n | m -> n where
  getSymbolMb :: n -> m (Maybe LLVM.AST.Operand)
  addSymbol :: n -> LLVM.AST.Operand -> m ()

  getSymbol :: n -> m LLVM.AST.Operand
  getSymbol = getSymbolMb >=> maybe throwCodegenError pure

instance MonadSymbolTable m n => MonadSymbolTable (LLVM.IRBuilderT m) n where
  getSymbolMb = lift . getSymbolMb
  addSymbol n = lift . addSymbol n


typeToLLVM 
  :: AST.Type -> LLVM.AST.Type
typeToLLVM AST.TInt          = LLVM.Type.i64
typeToLLVM AST.TFloat        = LLVM.Type.double
typeToLLVM AST.TBool         = LLVM.Type.i1
typeToLLVM AST.TUnit         = undefined
typeToLLVM AST.TCallable{..} = LLVM.Type.FunctionType (typeToLLVM returnT) (map typeToLLVM paramT) {-isVarArg-}False
typeToLLVM (AST.TOptional _) = undefined



binOpToLLVM 
  :: (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m)
  => AST.Type 
  -> AST.BinOp 
  -> LLVM.AST.Operand 
  -> LLVM.AST.Operand 
  -> m LLVM.AST.Operand
binOpToLLVM t AST.Add = case t of
  AST.TInt   -> LLVM.add
  AST.TFloat -> LLVM.fadd
  _          -> error "Codegen error"
binOpToLLVM t AST.Sub = case t of
  AST.TInt   -> LLVM.sub
  AST.TFloat -> LLVM.fsub
  _          -> error "Codegen error"
binOpToLLVM t AST.Mult = case t of
  AST.TInt   -> LLVM.mul
  AST.TFloat -> LLVM.fmul
  _          -> error "Codegen error"
binOpToLLVM t AST.Div = case t of
  AST.TInt   -> LLVM.udiv  -- TODO: should be signed?
  AST.TFloat -> LLVM.fdiv
  _          -> error "Codegen error"
binOpToLLVM t AST.Eq = case t of
  AST.TInt   -> LLVM.icmp LLVM.AST.IP.EQ
  AST.TFloat -> LLVM.fcmp LLVM.AST.FP.UEQ  -- possibly infinity
  AST.TBool  -> LLVM.icmp LLVM.AST.IP.EQ
  _          -> error "Codegen error"



unOpToLLVM 
  :: (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m)
  => AST.Type 
  -> AST.UnOp 
  -> LLVM.AST.Operand 
  -> m LLVM.AST.Operand
unOpToLLVM _ AST.Pos = error "Codegen error"
unOpToLLVM t AST.Neg = case t of
  AST.TInt   -> LLVM.sub $ LLVMC.int64 0
  AST.TFloat -> LLVM.fsub $ LLVMC.double 0
  _          -> error "Codegen error"

-- terminator already exists in block, LLVM disallows branch
mkTerminatorMb :: LLVM.MonadIRBuilder m => m () -> m ()
mkTerminatorMb instr = LLVM.hasTerminator >>= flip unless instr

namedBlock :: LLVM.MonadIRBuilder m => ShortByteString -> m LLVM.AST.Name
namedBlock = LLVM.named LLVM.block

entryBlockName :: ShortByteString
entryBlockName = "entry"

allocate ::
  ( MonadSymbolTable m n
  , LLVM.MonadModuleBuilder m
  , LLVM.MonadIRBuilder m
  ) => AST.Var n -> LLVM.AST.Operand -> m LLVM.AST.Operand
allocate (AST.V argName) arg = do
  t <- LLVM.Type.typeOf arg >>= \case
    Left _ -> throwCodegenError
    Right t -> pure t
  var <- LLVM.alloca t {-count-}Nothing {-align-}0
  LLVM.store var {-align-}0 arg
  addSymbol argName var
  pure var



blockToLLVM 
  :: (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m, MonadSymbolTable m n)
  => AST.Block n 'AST.Typed -> m LLVM.AST.Operand
blockToLLVM AST.Block{..} = do
  mapM_ stmtToLLVM blockBody
  mapM exprToLLVM blockResult >>= \case
    Just r  -> pure r
    Nothing -> pure $ LLVM.AST.ConstantOperand $ LLVM.AST.Undef LLVM.Type.void
    

exprToLLVM 
  :: forall n m. (MonadSymbolTable m n, LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) 
  => AST.Expr n 'AST.Typed -> m LLVM.AST.Operand
exprToLLVM (AST.EIntLit _ v)   = pure $ LLVMC.int64 v
exprToLLVM (AST.EFloatLit _ v) = pure $ LLVMC.double v
exprToLLVM (AST.EBoolLit _ v)  = pure $ LLVMC.bit (if v then 1 else 0)
exprToLLVM var@(AST.EVar _ (AST.V v))  = getSymbol v >>= flip (LLVM.load $ typeToLLVM $ getType var){-align-}0
exprToLLVM (AST.EUnOp (_, t) op v) = case op of
  AST.Pos -> exprToLLVM v
  AST.Neg -> unOpToLLVM t op =<< exprToLLVM v
exprToLLVM (AST.EBinOp (_, t) op e1 e2) = join $ binOpToLLVM t op <$> exprToLLVM e1 <*> exprToLLVM e2
exprToLLVM c@AST.ECall{..}   = do
  args' <- mapM exprToLLVM callArgs
  f <- case callName of  -- TODO
    AST.EVar _ (AST.V v) -> getSymbol v
    _ -> error "Codegen error"
  LLVM.call (typeToLLVM $ getType c) f [(arg, []) | arg <- args']
exprToLLVM AST.EAssign{..} = allocate assignVar =<< exprToLLVM assignExpr
exprToLLVM AST.EBlock{..}  = blockToLLVM unBlock
exprToLLVM AST.EIf{..} = mdo
    ifPred' <- exprToLLVM ifPred
    let ifElse = fromJust ifElseMb  -- TODO

    LLVM.condBr ifPred' ifThen' ifElse'

    ifThen' <- namedBlock "if.then"
    ifThenVal <- blockToLLVM ifThen
    mkTerminatorMb $ LLVM.br endBlock

    ifElse' <- namedBlock "if.else"
    ifElseVal <- blockToLLVM ifElse
    mkTerminatorMb $ LLVM.br endBlock

    endBlock <- namedBlock "if.exit"
    LLVM.phi [(ifThenVal, ifThen'), (ifElseVal, ifElse')]
exprToLLVM f@AST.EFunc{..} = do
  fName <- LLVM.fresh  -- TODO: proper names
  f' <- LLVM.function fName llvmParams llvmRet $ \args -> withScope $ do
    _ <- namedBlock entryBlockName
    zipWithM_ allocate funcParams args
    LLVM.ret =<< blockToLLVM funcBody

  addSymbol (AST.unVar funcName) f'
  pure f'
    where
      (llvmParams, llvmRet) = case getType f of  -- TODO: not neat
        AST.TCallable{..} -> (map (\t -> (typeToLLVM t, LLVM.NoParameterName)) paramT, typeToLLVM returnT)
        _ -> undefined

stmtToLLVM :: 
  ( MonadSymbolTable m n
  , LLVM.MonadIRBuilder m
  , LLVM.MonadModuleBuilder m
  , MonadFix m
  ) => AST.Stmt n 'AST.Typed -> m ()
stmtToLLVM (AST.SExpr _ e)    = void $ exprToLLVM e
stmtToLLVM (AST.SDecl _ _ _)  = undefined
stmtToLLVM (AST.SWhile _ _ _) = undefined
stmtToLLVM (AST.SReturn (_, t) e) = case t of
  AST.TUnit -> LLVM.retVoid  -- TODO: unit vs void?
  _ -> LLVM.ret =<< exprToLLVM e

progToLLVM :: 
  ( MonadSymbolTable m n
  , LLVM.MonadIRBuilder m
  , LLVM.MonadModuleBuilder m
  , MonadFix m
  ) => AST.Prog n 'AST.Typed -> m ()
progToLLVM (AST.Globals _ stmts) = mapM_ stmtToLLVM stmts



newtype CodegenM m n a = CodegenT 
  { runCodegenM :: 
      StateT (Map n LLVM.AST.Operand)
        (LLVM.ModuleBuilderT 
          (ExceptT CodegenError m)) a 
  }
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadError CodegenError
    , MonadState (Map n LLVM.AST.Operand)
    , MonadFix
    , LLVM.MonadModuleBuilder
    )
  deriving anyclass (ThrowsCodegenError, MonadScoping)

instance (Monad m, Ord n) => MonadSymbolTable (CodegenM m n) n where
  getSymbolMb = gets . Map.lookup
  addSymbol n = modify . Map.insert n

-- TODO: lowering rather than code-gen?
-- TODO: bracket errors in codegen
runCodegenT :: Monad m => CodegenM m n a -> m (Either CodegenError LLVM.AST.Module)
runCodegenT = runExceptT . LLVM.buildModuleT "test" . (`evalStateT` Map.empty) . runCodegenM

type Codegen n = CodegenM Identity n

runCodegen :: Codegen n a -> Either CodegenError LLVM.AST.Module
runCodegen = runIdentity . runCodegenT

toLLVM :: forall n. Ord n => AST.Expr n 'AST.Typed -> Either CodegenError LLVM.AST.Module
toLLVM expr = runCodegen $ do
    LLVM.function "main" [] LLVM.Type.i64 $ \_ -> do
        LLVM.ret =<< exprToLLVM expr

optimize :: LLVM.AST.Module -> IO LLVM.AST.Module
optimize astMod = do
  withContext $ \ctx -> do
    jit ctx $ \executionEngine ->
      withModuleFromAST ctx astMod $ \m -> do
        putStrLn "\nLLVM IR (pre-optimization)"
        putStrLn "--------------------------"
        moduleLLVMAssembly m >>= B.putStr

        verify m
        runPasses passSpec m
        optmod <- moduleAST m

        putStrLn "\nLLVM IR (post-optimization)"
        putStrLn "---------------------------"
        moduleLLVMAssembly m >>= B.putStr

        EE.withModuleInEngine executionEngine m $ \ee ->
          EE.getFunction ee "main" >>= \case
            Just fn -> do
              res <- run fn
              putStrLn $ "Evaluated to: " ++ show res
            Nothing -> pure ()
          
        pure optmod
  where
    passSpec = PassSetSpec [CuratedPassSet 3] Nothing

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Nothing
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> IO Int

run :: FunPtr a -> IO Int
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

