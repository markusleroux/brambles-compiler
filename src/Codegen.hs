{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen 
  ( exprToLLVM
  , toLLVM
  , compile 
  , ppllvm
  , LLVM.AST.Module
  , LLVM.Pretty.ppllvm
  , CodegenError
  )
  where

import qualified AST
import Typecheck (getType)
import Symbolize (MonadScoping, withScope)

import qualified LLVM.AST
import qualified LLVM.AST.Type as LLVM.Type
import qualified LLVM.AST.Typed as LLVM.Type
import qualified LLVM.AST.IntegerPredicate as LLVM.AST.IP
import qualified LLVM.AST.FloatingPointPredicate as LLVM.AST.FP

import qualified LLVM.IRBuilder.Constant as LLVMC
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM
import LLVM.Pretty (ppllvm)

import Control.Monad (join, void, unless, zipWithM_)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Control.Monad.Fix
import Control.Exception (bracket)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Data.FileEmbed (embedFileRelative)
import Data.String.Conversions (cs)
import System.Directory (removePathForcibly, withCurrentDirectory)
import System.IO (hClose)
import System.Posix.Temp (mkdtemp, mkstemps)
import System.Process (callProcess)


typeToLLVM 
  :: AST.Type -> LLVM.AST.Type
typeToLLVM AST.TInt          = LLVM.Type.i64
typeToLLVM AST.TFloat        = LLVM.Type.double
typeToLLVM AST.TBool         = LLVM.Type.i1
typeToLLVM AST.TUnit         = undefined
typeToLLVM AST.TCallable{..} = LLVM.Type.FunctionType (typeToLLVM returnT) (map typeToLLVM paramT) {-isVarArg-}False
typeToLLVM (AST.TOptional _) = undefined



binOpToLLVM 
  :: LLVM.MonadIRBuilder m 
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
  :: LLVM.MonadIRBuilder m 
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

data CodegenError = CodegenError
  deriving (Show)

class MonadError CodegenError m => ThrowsCodegenError m where
  throwCodegenError :: m a
  throwCodegenError = throwError CodegenError

class (ThrowsCodegenError m, MonadScoping m, Ord n) => MonadSymbolTable m n where
  getSymbolMb :: n -> m (Maybe LLVM.AST.Operand)
  addSymbol :: n -> LLVM.AST.Operand -> m ()

  getSymbol :: n -> m LLVM.AST.Operand
  getSymbol = getSymbolMb >=> maybe throwCodegenError pure


blockToLLVM 
  :: (LLVM.MonadIRBuilder m, MonadFix m)
  => AST.Block n 'AST.Typed 
  -> m LLVM.AST.Operand
blockToLLVM = undefined

exprToLLVM 
  :: forall n m. (MonadSymbolTable m n, LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) 
  => AST.Expr n 'AST.Typed 
  -> m LLVM.AST.Operand
exprToLLVM (AST.EIntLit _ v)   = pure $ LLVMC.int64 v
exprToLLVM (AST.EFloatLit _ v) = pure $ LLVMC.double v
exprToLLVM (AST.EBoolLit _ v)  = pure $ LLVMC.bit (if v then 1 else 0)
exprToLLVM (AST.EVar _ (AST.V v))  = getSymbol v >>= flip LLVM.load {-align-}0
exprToLLVM (AST.EUnOp (_, t) op v) = case op of
  AST.Pos -> exprToLLVM v
  AST.Neg -> unOpToLLVM t op =<< exprToLLVM v
exprToLLVM (AST.EBinOp (_, t) op e1 e2) = join $ binOpToLLVM t op <$> exprToLLVM e1 <*> exprToLLVM e2
exprToLLVM AST.ECall{..}   = do
  args' <- mapM exprToLLVM callArgs
  f <- case callName of  -- TODO
    AST.EVar _ (AST.V v) -> getSymbol v
    _ -> error "Codegen error"
  LLVM.call f [(arg, []) | arg <- args']
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
  f' <- LLVM.function fName llvmParams llvmRet $ \args -> lift . withScope $ do
    _ <- namedBlock entryBlockName
    zipWithM_ allocate funcParams args
    blockToLLVM funcBody >>= LLVM.ret

  addSymbol (AST.unVar funcName) f'
  pure f'
    where
      (llvmParams, llvmRet) = case getType f of  -- TODO: not neat
        AST.TCallable{..} -> (map (\t -> (typeToLLVM t, LLVM.NoParameterName)) paramT, typeToLLVM returnT)
        _ -> undefined

allocate ::
  ( MonadSymbolTable m n
  , LLVM.MonadIRBuilder m
  ) => AST.Var n -> LLVM.AST.Operand -> m LLVM.AST.Operand
allocate (AST.V argName) arg = do
  var <- LLVM.alloca (LLVM.Type.typeOf arg) {-count-}Nothing {-align-}0
  LLVM.store var {-align-}0 arg
  addSymbol argName var
  pure var

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
  deriving anyclass (ThrowsCodegenError)


instance Monad m => MonadScoping (CodegenM m n) where
  withScope computation = do
    outerScope <- get -- save the outer scope
    result <- computation -- run the computation
    put outerScope -- restore the outer scope
    pure result

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

instance ThrowsCodegenError m => ThrowsCodegenError (LLVM.IRBuilderT m)

instance MonadScoping m => MonadScoping (LLVM.IRBuilderT m) where
  withScope = withScope

instance MonadSymbolTable m n => MonadSymbolTable (LLVM.IRBuilderT m) n where
  getSymbolMb = lift . getSymbolMb
  addSymbol = addSymbol


toLLVM :: forall n. Ord n => AST.Expr n 'AST.Typed -> Either CodegenError LLVM.AST.Module
toLLVM expr = runCodegen $ do
    printInt <- LLVM.extern "printint" [LLVM.Type.i32] LLVM.Type.i32

    LLVM.function "main" [] LLVM.Type.i32 $ \_ -> do
        e <- (exprToLLVM expr :: LLVM.IRBuilderT (Codegen n) LLVM.AST.Operand)
        _ <- LLVM.call printInt [(e, [])]

        LLVM.ret $ LLVMC.int32 0

-- https://github.com/danieljharvey/llvm-calc/blob/trunk/llvm-calc/src/Calc/Compile/RunLLVM.hs
cRuntime :: Text
cRuntime = T.decodeUtf8 $(embedFileRelative "static/runtime.c")

compile :: LLVM.AST.Module -> FilePath -> IO ()
compile llvmModule outfile =
    bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
        withCurrentDirectory buildDir $ do
            (llvm, llvmHandle) <- mkstemps "output" ".ll"
            (runtime, runtimeHandle) <- mkstemps "runtime" ".c"

            let moduleText = cs (ppllvm llvmModule)

            T.hPutStrLn llvmHandle moduleText -- write the llvmmodule a file
            T.hPutStrLn runtimeHandle cRuntime

            hClose llvmHandle
            hClose runtimeHandle

            -- link the runtime with the assembly
            callProcess "clang" ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outfile]

