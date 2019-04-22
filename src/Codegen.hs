{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Codegen where

import Control.Lens
import Control.Lens.Prism
import Control.Monad
import Data.ByteString.Char8 as BS (putStrLn)
import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import LLVM.AST.Type as AST
import LLVM.Context
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Module
import Syntax

makePrisms ''Decl

makePrisms ''Expr

generateModule :: [Decl] -> IO ()
generateModule xs = do
  let mod = buildModule "exampleModule" $ traverseOf_ each codegenTop xs
  withContext $ \ctx -> do
    llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
    BS.putStrLn llvm

codegenTop :: MonadModuleBuilder m => Decl -> m Operand
codegenTop (Var a (Constant b)) = global (mkName a) AST.i32 (C.Int 32 b)
codegenTop (Extern a b) =
  extern (mkName a) (toListOf (replicated $ length b) AST.i32) AST.i32

codegen :: MonadIRBuilder m => Expr -> m Operand
codegen (Constant a) = return $ ConstantOperand $ C.Int 32 a
codegen (Syntax.Call a b) =
  call (ConstantOperand (C.GlobalReference AST.i32 (mkName a))) []
codegen (Binary Syntax.Add a b) = do
  x <- codegen a
  y <- codegen b
  add x y
