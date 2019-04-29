{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Codegen where

import Control.Lens hiding (assign)
import Control.Lens.Prism
import Control.Monad
import Data.ByteString.Char8 as BS (putStrLn)
import qualified Data.Map.Strict as Map
import LLVM.AST (Operand(ConstantOperand, LocalReference), mkName)
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type as AST
import LLVM.Context
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Module
import LLVM.Target
import Prelude hiding (and, or)
import Syntax

makePrisms ''Decl

makePrisms ''Expr

makePrisms ''Stmt

makePrisms ''Item

generateModule :: [Decl] -> IO ()
generateModule xs =
  mdo let mod = buildModule "exampleModule" $ traverseOf_ each codegenDecl xs
      withContext $ \ctx ->
        withHostTargetMachine $ \tm ->
          withModuleFromAST
            ctx
            mod
            (writeObjectToFile tm (File "example/main.o"))

codegenDecl :: MonadModuleBuilder m => Decl -> m Operand
codegenDecl (Var name (Constant n)) = global (mkName name) AST.i32 (C.Int 32 n)
codegenDecl (Extern name args) =
  extern (mkName name) (toListOf (replicated $ length args) AST.i32) AST.i32
codegenDecl (Func name args body) =
  function (mkName name) [] AST.i32 $ \[] -> codegenStmt body

codegenLocal :: MonadIRBuilder m => Decl -> m ()
codegenLocal (Var name val) = do
  x <- codegenExpr val
  alloca AST.i32 (Just x) 0 `named` "a"
  return ()

codegenStmt :: MonadIRBuilder m => Stmt -> m ()
codegenStmt (ExprStmt exp) = do
  codegenExpr exp
  return ()
codegenStmt (CompoundStmt items) = do
  block `named` "entry"
  forOf each items $ \case
    StmtItem i -> codegenStmt i
    DeclItem i -> codegenLocal i
  return ()
codegenStmt (If exp body) = do
  cond <- codegenExpr exp
  condBr cond "if.then" "if.exit"
  block `named` "if.then"
  codegenStmt body
  br "if.exit"
  block `named` "if.exit"
  return ()
codegenStmt (IfElse exp body elseBody) = do
  cond <- codegenExpr exp
  condBr cond "if.then" "if.else"
  block `named` "if.then"
  codegenStmt body
  br "if.exit"
  block `named` "if.else"
  codegenStmt elseBody
  br "if.exit"
  block `named` "if.exit"
  return ()
codegenStmt (While exp body) = do
  cond <- codegenExpr exp
  condBr cond "while.body" "while.exit"
  block `named` "while.body"
  codegenStmt body
  condBr cond "while.body" "while.exit"
  block `named` "while.exit"
  return ()
codegenStmt (DoWhile body exp) = do
  block `named` "while.body"
  codegenStmt body
  cond <- codegenExpr exp
  condBr cond "while.body" "while.exit"
  block `named` "while.exit"
  return ()
codegenStmt (Return val) = do
  x <- codegenExpr val
  ret x

codegenExpr :: MonadIRBuilder m => Expr -> m Operand
codegenExpr (Constant n) = return $ ConstantOperand $ C.Int 32 n
codegenExpr (Identifier id) =
  mdo let sig = AST.ptr $ AST.IntegerType 32
      load (LocalReference sig (mkName id)) 0
codegenExpr (Call name args) =
  mdo let sig = AST.ptr $ AST.FunctionType AST.i32 (toListOf (replicated $ length args) AST.i32) False
      call (ConstantOperand $ C.GlobalReference sig (mkName name)) []
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
              , (And, and)
              , (Or, or)
              ]
      case Map.lookup op binOps of
        Just f ->
          mdo x <- codegenExpr a
              y <- codegenExpr b
              f x y
        Nothing -> error "No such operator was found."
codegenExpr (Unary op a) =
  mdo let unOps =
            Map.fromList [(Plus, plus), (Minus, minus), (Inc, inc), (Dec, dec)]
      case Map.lookup op unOps of
        Just f ->
          mdo x <- codegenExpr a
              f x
        Nothing -> error "No such operator was found."

inc :: MonadIRBuilder m => Operand -> m Operand
inc x = add x (ConstantOperand $ C.Int 32 1)

dec :: MonadIRBuilder m => Operand -> m Operand
dec x = sub x (ConstantOperand $ C.Int 32 1)

minus :: MonadIRBuilder m => Operand -> m Operand
minus x = mul x (ConstantOperand $ C.Int 32 (-1))

plus :: MonadIRBuilder m => Operand -> m Operand
plus x = mul x (ConstantOperand $ C.Int 32 (-1))
