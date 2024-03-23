{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module LLVM where

import qualified AST

import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.Type as LLVM hiding (double)
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM
import LLVM.Pretty (ppllvm)

import Control.Exception (bracket)

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Data.String.Conversions
import Data.FileEmbed
import System.IO
import System.Directory (removePathForcibly, withCurrentDirectory)
import System.Posix.Temp
import System.Process

-- import Data.Bool (bool)
import Control.Monad (join)

exprToLLVM :: (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) => AST.Expr n -> m LLVM.Operand
exprToLLVM (AST.EIntLit v) = pure $ LLVM.int64 v
--exprToLLVM (AST.EFloatLit v) = pure $ LLVM.double v
-- exprToLLVM (AST.EBoolLit v) = pure $ LLVM.bit (bool 1 0 v)
exprToLLVM (AST.EBinOp op e1 e2) = join $ binOpToLLVM AST.TInt op <$> exprToLLVM e1 <*> exprToLLVM e2  -- TODO: typing
    where
      binOpToLLVM AST.TInt AST.Add = LLVM.add
      binOpToLLVM AST.TInt AST.Sub = LLVM.sub
      binOpToLLVM AST.TInt AST.Mult = LLVM.mul
      binOpToLLVM AST.TInt AST.Div = LLVM.udiv
      binOpToLLVM AST.TFloat AST.Add = LLVM.fadd
      binOpToLLVM AST.TFloat AST.Sub = LLVM.fsub
      binOpToLLVM AST.TFloat AST.Mult = LLVM.fmul
      binOpToLLVM AST.TFloat AST.Div = LLVM.fdiv
      binOpToLLVM _ _ = undefined
exprToLLVM _ = undefined

toLLVM :: AST.Expr n -> LLVM.Module
toLLVM expr = LLVM.buildModule "test" $ do
  printInt <- LLVM.extern "printint" [LLVM.i32] LLVM.i32

  LLVM.function "main" [] LLVM.i32 $ \_ -> do
    e <- exprToLLVM expr
    _ <- LLVM.call printInt [(e, [])]

    LLVM.ret $ LLVM.int32 0


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

      T.hPutStrLn llvmHandle moduleText  -- write the llvmmodule a file
      T.hPutStrLn runtimeHandle cRuntime

      hClose llvmHandle
      hClose runtimeHandle

      -- link the runtime with the assembly
      callProcess "clang" ["-Wno-override-module", "-lm", llvm, runtime, "-o", "../" <> outfile]

