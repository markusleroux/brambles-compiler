module Brambles.Backend where

import Protolude

import qualified Brambles.Frontend.AST as AST
import qualified Brambles.Backend.Codegen as CG
import qualified Brambles.Backend.ASTToLLVM as AtL

import Control.Monad.Except (modifyError, liftEither)


data BackendError
  = CodegenError AtL.ASTToLLVMError
  deriving (Show)


generateLLVMAssemblyToFile :: MonadIO m => FilePath -> AST.Module Int 'AST.Typed -> ExceptT BackendError m ()
generateLLVMAssemblyToFile file = doLowering >=> writeAssembly
  where
    doLowering = modifyError CodegenError . liftEither . AtL.runModuleToLLVM
    writeAssembly = liftIO . CG.writeLLVMAssemblyToFile file

generateLLVMAssemblyToByteString :: MonadIO m => AST.Module Int 'AST.Typed -> ExceptT BackendError m ByteString
generateLLVMAssemblyToByteString = doLowering >=> generateLLVMAssembly'
  where
    doLowering = modifyError CodegenError . liftEither . AtL.runModuleToLLVM
    generateLLVMAssembly' = liftIO . CG.generateLLVMAssembly CG.defaultPassSpec
