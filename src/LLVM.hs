{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module LLVM 
  ( exprToLLVM
  , toLLVM
  , compile 
  , ppllvm
  , LLVM.Module
  , LLVM.Pretty.ppllvm
  )
  where

import qualified AST
import Typecheck ()

import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.FloatingPointPredicate as LLVM
import qualified LLVM.IRBuilder.Constant as LLVMC
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM
import LLVM.Pretty (ppllvm)

import Control.Monad (join, void, unless)
import Control.Exception (bracket)

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.String.Conversions (cs)
import System.Directory (removePathForcibly, withCurrentDirectory)
import System.IO (hClose)
import System.Posix.Temp (mkdtemp, mkstemps)
import System.Process (callProcess)

typeToLLVM :: AST.Type -> LLVM.Type
typeToLLVM AST.TInt = LLVM.i64
typeToLLVM AST.TFloat = LLVM.double
typeToLLVM AST.TBool = LLVM.i1
typeToLLVM AST.TUnit = undefined
typeToLLVM AST.TCallable{..} = LLVM.FunctionType (typeToLLVM returnT) (map typeToLLVM paramT) {-isVarArg-}False
typeToLLVM (AST.TOptional _) = undefined

binOpToLLVM :: LLVM.MonadIRBuilder m => AST.Type -> AST.BinOp -> LLVM.Operand -> LLVM.Operand -> m LLVM.Operand
binOpToLLVM t AST.Add = case t of
  AST.TInt -> LLVM.add
  AST.TFloat -> LLVM.fadd
  _ -> error "Codegen error"
binOpToLLVM t AST.Sub = case t of
  AST.TInt -> LLVM.sub
  AST.TFloat -> LLVM.fsub
  _ -> error "Codegen error"
binOpToLLVM t AST.Mult = case t of
  AST.TInt -> LLVM.mul
  AST.TFloat -> LLVM.fmul
  _ -> error "Codegen error"
binOpToLLVM t AST.Div = case t of
  AST.TInt -> LLVM.udiv  -- TODO: should be signed?
  AST.TFloat -> LLVM.fdiv
  _ -> error "Codegen error"
binOpToLLVM t AST.Eq = case t of
  AST.TInt -> LLVM.icmp LLVM.EQ
  AST.TFloat -> LLVM.fcmp LLVM.UEQ  -- possibly infinity
  AST.TBool -> LLVM.icmp LLVM.EQ
  _ -> error "Codegen error"
binOpToLLVM _ _ = undefined

unOpToLLVM :: LLVM.MonadIRBuilder m => AST.Type -> AST.UnOp -> LLVM.Operand -> m LLVM.Operand
unOpToLLVM t AST.Pos = error "Codegen error"
unOpToLLVM t AST.Neg = case t of
  AST.TInt -> LLVM.sub $ LLVMC.int64 0
  AST.TFloat -> LLVM.fsub $ LLVMC.double 0
  _ -> error "Codegen error"

exprToLLVM :: LLVM.MonadIRBuilder m => AST.Expr n 'AST.Typed -> m LLVM.Operand
exprToLLVM (AST.EIntLit _ v) = pure $ LLVMC.int64 v
exprToLLVM (AST.EFloatLit _ v) = pure $ LLVMC.double v
exprToLLVM (AST.EBoolLit _ v) = pure $ LLVMC.bit (if v then 1 else 0)
exprToLLVM (AST.EVar _ v) = undefined
exprToLLVM (AST.EUnOp (_, t) op v) = case op of
  AST.Pos -> exprToLLVM v
  AST.Neg -> unOpToLLVM t op =<< exprToLLVM v
exprToLLVM (AST.EBinOp (_, t) op e1 e2) = join $ binOpToLLVM t op <$> exprToLLVM e1 <*> exprToLLVM e2
exprToLLVM (AST.ECall _ _ _) = undefined
exprToLLVM (AST.EAssign _ _ _) = undefined
exprToLLVM (AST.EBlock _ _ b) = undefined
{-
exprToLLVM AST.EIf{..} = mdo
    pred <- exprToLLVM ifCond

    LLVM.condBr pred bThen (maybe mergeBlock (const bElse) ifElseMb)

    bThen <- LLVM.block `LLVM.named` "then"
    blockToLLVM ifBody
    mkTerminator $ LLVM.br mergeBlock

    bElse <- LLVM.block `LLVM.named` "else"
    blockToLLVM <$> ifElseMb
    mkTerminator $ LLVM.br mergeBlock

    mergeBlock <- LLVM.block `LLVM.named` "merge"
    pure ()
  where
    -- terminator already exists in block, LLVM disallows branch
    mkTerminator = unless =<< LLVM.hasTerminator
-}
exprToLLVM (AST.EFunc _ _ _ _) = undefined

stmtToLLVM :: LLVM.MonadIRBuilder m => AST.Stmt n 'AST.Typed -> m ()
stmtToLLVM (AST.SExpr _ e) = void $ exprToLLVM e
stmtToLLVM (AST.SDecl _ _ _) = undefined
stmtToLLVM (AST.SWhile _ _ _) = undefined
stmtToLLVM (AST.SReturn (_, t) e) = case t of
  AST.TUnit -> LLVM.retVoid  -- TODO: unit vs void?
  _ -> LLVM.ret =<< exprToLLVM e

progToLLVM :: LLVM.MonadIRBuilder m => AST.Prog n 'AST.Typed -> m ()
progToLLVM (AST.Globals _ stmts) = mapM_ stmtToLLVM stmts


toLLVM :: AST.Expr n 'AST.Typed -> LLVM.Module
toLLVM expr = LLVM.buildModule "test" $ do
    printInt <- LLVM.extern "printint" [LLVM.i32] LLVM.i32

    LLVM.function "main" [] LLVM.i32 $ \_ -> do
        e <- exprToLLVM expr
        _ <- LLVM.call printInt [(e, [])]

        LLVM.ret $ LLVMC.int32 0

-- https://github.com/danieljharvey/llvm-calc/blob/trunk/llvm-calc/src/Calc/Compile/RunLLVM.hs
cRuntime :: Text
cRuntime = T.decodeUtf8 $(makeRelativeToProject "static/runtime.c" >>= embedFile)

compile :: LLVM.Module -> FilePath -> IO ()
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
