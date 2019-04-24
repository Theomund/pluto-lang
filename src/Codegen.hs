{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Codegen where

import Control.Lens
import Control.Lens.Prism
import Control.Monad
import Data.ByteString.Char8 as BS (putStrLn)
import Data.Either
import qualified Data.Map.Strict as Map
import LLVM.AST hiding (Add, Call, Div, Mul, Sub, element, function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type as AST
import LLVM.Context
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Module
import Syntax

makePrisms ''Decl

makePrisms ''Expr

makePrisms ''Stmt

makePrisms ''Item

generateModule :: [Decl] -> IO ()
generateModule xs =
  mdo let mod = buildModule "exampleModule" $ traverseOf_ each codegenDecl xs
      withContext $ \ctx ->
        mdo llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
            BS.putStrLn llvm

codegenDecl :: MonadModuleBuilder m => Decl -> m Operand
codegenDecl (Var a (Constant b)) = global (mkName a) AST.i32 (C.Int 32 b)
codegenDecl (Extern a b) =
  extern (mkName a) (toListOf (replicated $ length b) AST.i32) AST.i32
codegenDecl (Func a b (CompoundStmt c)) =
  function (mkName a) [] AST.i32 $ \[] ->
    mdo forOf each c $ \case
          StmtItem v -> codegenStmt v
          DeclItem v -> error "Local variables are not implemented yet."
        ret (ConstantOperand (C.Int 32 0))

codegenStmt :: MonadIRBuilder m => Stmt -> m Operand
codegenStmt (ExprStmt a) = codegenExpr a

codegenExpr :: MonadIRBuilder m => Expr -> m Operand
codegenExpr (Constant a) = return $ ConstantOperand $ C.Int 32 a
codegenExpr (Call a b) =
  call (ConstantOperand (C.GlobalReference AST.i32 (mkName a))) []
codegenExpr (Binary op a b) =
  mdo let binOps =
            Map.fromList
              [ (Add, add)
              , (Sub, sub)
              , (Mul, mul)
              , (Div, sdiv)
              , (Mod, srem)
              , (Equal, icmp IP.EQ)
              , (NotEqual, icmp IP.NE)
              , (LessThan, icmp IP.SLT)
              , (GreaterThan, icmp IP.SGT)
              , (LessEqual, icmp IP.SLE)
              , (GreaterEqual, icmp IP.SGE)
              ]
      case Map.lookup op binOps of
        Just f ->
          mdo x <- codegenExpr a
              y <- codegenExpr b
              f x y
        Nothing -> error "No such operator was found."
