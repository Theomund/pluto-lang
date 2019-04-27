{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Codegen where

import Control.Lens hiding (assign)
import Control.Lens.Prism
import Control.Monad
import Data.ByteString.Char8 as BS (putStrLn)
import Data.ByteString.Short
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
codegenDecl (Extern a b) = extern (mkName a) [] AST.i32
codegenDecl (Func a b c) =
  function (mkName a) [] AST.i32 $ \[] -> mdo codegenStmt c

codegenLocal :: MonadIRBuilder m => Decl -> m ()
codegenLocal (Var a b) = do
  x <- codegenExpr b
  alloca AST.i32 (Just x) 0 `named` "a"
  return ()

codegenStmt :: MonadIRBuilder m => Stmt -> m ()
codegenStmt (ExprStmt a) = do
  codegenExpr a
  return ()
codegenStmt (CompoundStmt a) = do
  block `named` "entry"
  forOf each a $ \case
    StmtItem i -> codegenStmt i
    DeclItem i -> codegenLocal i
  return ()
codegenStmt (If a b) = do
  cond <- codegenExpr a
  condBr cond "if.then" "if.exit"
  block `named` "if.then"
  codegenStmt b
  br "if.exit"
  block `named` "if.exit"
  return ()
codegenStmt (IfElse a b c) = do
  cond <- codegenExpr a
  condBr cond "if.then" "if.else"
  block `named` "if.then"
  codegenStmt b
  br "if.exit"
  block `named` "if.else"
  codegenStmt c
  br "if.exit"
  block `named` "if.exit"
  return ()
codegenStmt (While a b) = do
  cond <- codegenExpr a
  condBr cond "while.body" "while.exit"
  block `named` "while.body"
  codegenStmt b
  condBr cond "while.body" "while.exit"
  block `named` "while.exit"
  return ()
codegenStmt (DoWhile a b) = do
  block `named` "while.body"
  codegenStmt a
  cond <- codegenExpr b
  condBr cond "while.body" "while.exit"
  block `named` "while.exit"
  return ()
codegenStmt (Return a) = do
  x <- codegenExpr a
  ret x

codegenExpr :: MonadIRBuilder m => Expr -> m Operand
codegenExpr (Constant a) = return $ ConstantOperand $ C.Int 32 a
codegenExpr (Identifier a) =
  mdo let sig = AST.ptr $ AST.IntegerType 32
      load (LocalReference sig (mkName a)) 0
codegenExpr (Call name args) =
  mdo let sig = AST.ptr $ AST.FunctionType AST.i32 [] False
      call (ConstantOperand (C.GlobalReference sig (mkName name))) []
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
codegenExpr (Unary op a) =
  mdo let unOps = Map.fromList [(Inc, inc), (Dec, dec)]
      case Map.lookup op unOps of
        Just f ->
          mdo x <- codegenExpr a
              f x
        Nothing -> error "No such operator was found."

inc :: MonadIRBuilder m => Operand -> m Operand
inc a = add a (ConstantOperand (C.Int 32 1))

dec :: MonadIRBuilder m => Operand -> m Operand
dec a = sub a (ConstantOperand (C.Int 32 1))
