{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Brambles.Backend.ASTToLLVM where

import Protolude hiding (StateT, runStateT, evalStateT)
import Protolude.Error

import qualified Brambles.Frontend.AST as AST
import Brambles.Frontend.Typecheck (getType)
import Brambles.Frontend.Symbolize (MonadScoping, withScope)

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

import Control.Monad.State.Strict

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

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

alloca :: LLVM.MonadIRBuilder m => LLVM.Type.Type -> m LLVM.AST.Operand
alloca t = LLVM.alloca t {-count-}Nothing {-align-}0

store :: LLVM.MonadIRBuilder m => LLVM.AST.Operand -> LLVM.AST.Operand -> m ()
store at = LLVM.store at {-align-}0


allocate ::
  ( MonadSymbolTable m n
  , LLVM.MonadModuleBuilder m
  , LLVM.MonadIRBuilder m
  ) => AST.Var n -> LLVM.AST.Operand -> m LLVM.AST.Operand
allocate (AST.V argName) arg = do
  t <- LLVM.Type.typeOf arg >>= \case
    Left _ -> throwCodegenError
    Right t -> pure t
  var <- alloca t
  store var arg
  addSymbol argName var
  pure var

voidOp :: LLVM.AST.Operand
voidOp = LLVM.AST.ConstantOperand $ LLVM.AST.Undef LLVM.Type.void
    
getSymbolFromVar :: MonadSymbolTable m n => AST.Var n -> m LLVM.AST.Operand
getSymbolFromVar (AST.V v) = getSymbol v


blockToLLVM 
  :: (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m, MonadSymbolTable m n)
  => AST.Block n 'AST.Typed -> m LLVM.AST.Operand
blockToLLVM AST.Block{..} = mapM_ stmtToLLVM blockBody >> mapM exprToLLVM blockResult >>= maybe (pure voidOp) pure


exprToLLVM 
  :: forall n m. (MonadSymbolTable m n, LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m, MonadFix m) 
  => AST.Expr n 'AST.Typed -> m LLVM.AST.Operand
exprToLLVM (AST.EIntLit _ v)   = pure $ LLVMC.int64 v
exprToLLVM (AST.EFloatLit _ v) = pure $ LLVMC.double v
exprToLLVM (AST.EBoolLit _ v)  = pure $ LLVMC.bit (if v then 1 else 0)
exprToLLVM var@AST.EVar{..}    = getSymbolFromVar varVar >>= flip (LLVM.load $ typeToLLVM $ getType var){-align-}0
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
exprToLLVM AST.EAssign{..} = do
  var <- getSymbolFromVar assignVar
  store var =<< exprToLLVM assignExpr
  pure var
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
        AST.TCallable{..} -> ([(typeToLLVM t, LLVM.NoParameterName) | t <- paramT], typeToLLVM returnT)
        _ -> undefined

stmtToLLVM :: 
  ( MonadSymbolTable m n
  , LLVM.MonadIRBuilder m
  , LLVM.MonadModuleBuilder m
  , MonadFix m
  ) => AST.Stmt n 'AST.Typed -> m ()
stmtToLLVM (AST.SExpr _ e) = void $ exprToLLVM e
stmtToLLVM AST.SDecl{..}   = void $ exprToLLVM declExpr >>= allocate declName
stmtToLLVM AST.SWhile{..} = mdo
  LLVM.br whileStart
  whileStart <- namedBlock "while.start"

  whilePred' <- exprToLLVM whilePred
  LLVM.condBr whilePred' whileBodyBlock whileEnd
  
  whileBodyBlock <- namedBlock "while.body"
  mapM_ stmtToLLVM whileBody
  
  LLVM.br whileStart
  whileEnd <- namedBlock "while.end"
  pure ()
stmtToLLVM (AST.SReturn (_, t) e) = case t of
  AST.TUnit -> LLVM.retVoid  -- TODO: unit vs void?
  _         -> LLVM.ret =<< exprToLLVM e

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

runCodegenT :: Monad m => CodegenM m n a -> m (Either CodegenError LLVM.AST.Module)
runCodegenT = runExceptT . LLVM.buildModuleT "test" . (`evalStateT` Map.empty) . runCodegenM

type Codegen n = CodegenM Identity n

runCodegen :: Codegen n a -> Either CodegenError LLVM.AST.Module
runCodegen = runIdentity . runCodegenT

toLLVM :: forall n. Ord n => AST.Expr n 'AST.Typed -> Either CodegenError LLVM.AST.Module
toLLVM AST.EFunc{..} = undefined
toLLVM expr = runCodegen $ do
    LLVM.function "main" [] LLVM.Type.i64 $ \_ -> do
        LLVM.ret =<< exprToLLVM expr

