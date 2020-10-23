module Main where

import Parser

import qualified Data.ByteString.Char8 as BS

import Control.Monad.Trans

import qualified LLVM.AST as L (Module(..))
import qualified LLVM.IRBuilder.Module as L

import Codegen

import LLVM.Target
import LLVM.Context
import LLVM.Internal.Module
import LLVM.Internal.FFI.LLVMCTypes

process :: String -> IO (Maybe L.Module)
process line = do
  let res = parseModule line
  case res of
    Left err -> print err >> return Nothing
    Right tree -> return $ Just $ codegen tree

main :: IO ()
main = do
  code <- getContents
  mmod <- process code
  case mmod of
    Just mod ->
      withContext $ \ctx ->
        withModuleFromAST ctx mod $ \m -> do
          asm <- moduleLLVMAssembly m
          BS.putStrLn asm
          withHostTargetMachineDefault $ \t ->
            emitToFile codeGenFileTypeObject t (File "a.o") m
