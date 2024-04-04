{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brambles.Backend.Codegen 
  ( exprToLLVM
  , toLLVM
  , LLVM.AST.Module
  , CodegenError
  , optimize
  , jit
  , run
  )
  where

import Protolude

import Brambles.Backend.ASTToLLVM (exprToLLVM, toLLVM, CodegenError)

import Foreign.Ptr (castFunPtr)

import qualified LLVM.AST
import LLVM.Passes
import LLVM.Analysis (verify)
import LLVM.Module (moduleAST, moduleLLVMAssembly, withModuleFromAST)
import LLVM.Context (withContext, Context)
import LLVM.ExecutionEngine as EE (withMCJIT, ExecutionEngine(getFunction, withModuleInEngine), MCJIT)

import qualified Data.ByteString as B


jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing

foreign import ccall "dynamic" 
  haskFun :: FunPtr (IO Int) -> IO Int

run :: FunPtr a -> IO Int
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))


optimize :: LLVM.AST.Module -> IO LLVM.AST.Module
optimize astMod = do
  withContext $ \ctx -> do
    jit ctx $ \executionEngine ->
      withModuleFromAST ctx astMod $ \m -> do
        putText "\nLLVM IR (pre-optimization)"
        putText "--------------------------"
        moduleLLVMAssembly m >>= B.putStr

        verify m
        runPasses passSpec m
        optmod <- moduleAST m

        putText "\nLLVM IR (post-optimization)"
        putText "---------------------------"
        moduleLLVMAssembly m >>= B.putStr

        EE.withModuleInEngine executionEngine m $ \ee ->
          EE.getFunction ee "main" >>= \case
            Just fn -> do -- TODO: might leak
              res <- run fn
              putStrLn $ "Evaluated to: " ++ show res
            Nothing -> pure ()
          
        pure optmod
  where
    passSpec = PassSetSpec [CuratedPassSet 3] Nothing
