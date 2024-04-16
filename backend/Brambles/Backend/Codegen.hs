{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brambles.Backend.Codegen 
  ( LLVM.AST.Module
  , optimize
  , run
  , writeObjectFile
  , generateLLVMAssembly
  , Brambles.Backend.Codegen.writeLLVMAssemblyToFile
  , defaultPassSpec
  )
  where

import Protolude

import Foreign.Ptr (castFunPtr)

import qualified LLVM.AST
import LLVM.Passes
import LLVM.Analysis (verify)
import LLVM.Module as LLVM 
  ( File(..)
  , moduleAST
  , moduleLLVMAssembly
  , withModuleFromAST
  , writeObjectToFile
  , Module
  , writeLLVMAssemblyToFile
  )
import LLVM.Target (withHostTargetMachineDefault)
import LLVM.Context (withContext, Context)
import LLVM.ExecutionEngine as EE (withMCJIT, ExecutionEngine(getFunction, withModuleInEngine), MCJIT)

import qualified Data.ByteString as B

-- TODO: catch the exceptions thrown in IO


defaultPassSpec :: PassSetSpec
defaultPassSpec = PassSetSpec [] Nothing

optimizedPassSpec :: PassSetSpec
optimizedPassSpec = PassSetSpec [CuratedPassSet 3] Nothing


withModule :: LLVM.Passes.PassSetSpec -> LLVM.AST.Module -> (LLVM.Module -> IO a) -> IO a
withModule passSpec astMod f =
  withContext $ \ctx ->
    withModuleFromAST ctx astMod $ \m -> do
      verify m
      runPasses passSpec m
      f m

writeObjectFile :: FilePath -> LLVM.AST.Module -> IO ()
writeObjectFile file astMod =
  withModule optimizedPassSpec astMod $ \m ->
    withHostTargetMachineDefault $ \tm -> do
      writeObjectToFile tm file' m
  where
    file' = LLVM.File file
  
writeLLVMAssemblyToFile :: FilePath -> LLVM.AST.Module -> IO ()
writeLLVMAssemblyToFile file astMod =
  withModule optimizedPassSpec astMod $ \m -> do
    _ <- moduleAST m
    LLVM.writeLLVMAssemblyToFile file' m
  where
    file' = LLVM.File file

generateLLVMAssembly :: LLVM.Passes.PassSetSpec -> LLVM.AST.Module -> IO ByteString
generateLLVMAssembly passSpec astMod =
  withModule passSpec astMod $ \m -> do
    _ <- moduleAST m
    moduleLLVMAssembly m

optimize :: LLVM.AST.Module -> IO LLVM.AST.Module
optimize astMod =
  withContext $ \ctx ->
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


