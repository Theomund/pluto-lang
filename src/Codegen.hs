{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Codegen where

import           Control.Lens               hiding (assign)
import           Control.Lens.Prism
import           Control.Monad

import           Data.ByteString.Char8      as BS (pack, putStrLn)
import           Data.ByteString.Short      as SH (toShort)
import qualified Data.Map.Strict            as Map
import           Data.String
import           LLVM.AST                   (Operand (ConstantOperand, LocalReference),
                                             mkName)
import qualified LLVM.AST.Attribute         as A
import qualified LLVM.AST.Constant          as C
import qualified LLVM.AST.IntegerPredicate  as IP
import           LLVM.AST.Type              as AST
import           LLVM.Context
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Module
import           LLVM.Target
import           Prelude                    hiding (and, not, or)
import           Syntax

makePrisms ''Decl

makePrisms ''Expr

makePrisms ''Stmt

makePrisms ''Item

generateModule :: [Decl] -> IO ()
generateModule xs =
  mdo let mod = buildModule "exampleModule" $ traverseOf_ each codegenDecl xs
      withContext $ \ctx ->
        withHostTargetMachine $ \tm ->
          mdo llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
              BS.putStrLn llvm
              withModuleFromAST
                ctx
                mod
                (writeObjectToFile tm (File "example/main.o"))

codegenDecl :: MonadModuleBuilder m => Decl -> m Operand
codegenDecl (Var name (Constant n)) = global (mkName name) AST.i32 (C.Int 32 n)
codegenDecl (Extern name args) =
  extern (mkName name) (toListOf (replicated $ length args) AST.i32) AST.i32
codegenDecl (Func name args body) =
  mdo let fargs =
            over mapped (\x -> (AST.i32, fromString $ x ^. _Identifier)) args
      function (mkName name) fargs AST.i32 $ \a ->
        mdo block `named` "entry"
            codegenStmt body

codegenLocal :: MonadIRBuilder m => Decl -> m ()
codegenLocal (Var name val) =
  mdo x <- codegenExpr val
      alloca AST.i32 (Just $ ConstantOperand $ C.Int 32 4) 0 `named`
        SH.toShort (BS.pack name)
      let sig = AST.ptr $ AST.IntegerType 32
      store (LocalReference sig (mkName name)) 0 x

codegenStmt :: MonadIRBuilder m => Stmt -> m ()
codegenStmt (ExprStmt exp) =
  mdo case exp of
        (Binary Assign (Identifier id) val) ->
          mdo let sig = AST.ptr $ AST.IntegerType 32
              x <- codegenExpr val
              store (LocalReference sig (mkName id)) 0 x
        _ ->
          mdo codegenExpr exp
              return ()
codegenStmt (CompoundStmt items) =
  mdo forOf each items $ \case
        StmtItem i -> codegenStmt i
        DeclItem i -> codegenLocal i
      return ()
codegenStmt (If exp body) =
  mdo cond <- codegenExpr exp
      condBr cond "if.then" "if.exit"
      block `named` "if.then"
      codegenStmt body
      br "if.exit"
      block `named` "if.exit"
      return ()
codegenStmt (IfElse exp body elseBody) =
  mdo cond <- codegenExpr exp
      condBr cond "if.then" "if.else"
      block `named` "if.then"
      codegenStmt body
      br "if.exit"
      block `named` "if.else"
      codegenStmt elseBody
      br "if.exit"
      block `named` "if.exit"
      return ()
codegenStmt (While exp body) =
  mdo initCond <- codegenExpr exp
      condBr initCond "while.body" "while.exit"
      block `named` "while.body"
      codegenStmt body
      cond <- codegenExpr exp
      condBr cond "while.body" "while.exit"
      block `named` "while.exit"
      return ()
codegenStmt (DoWhile body exp) =
  mdo block `named` "while.body"
      codegenStmt body
      cond <- codegenExpr exp
      condBr cond "while.body" "while.exit"
      block `named` "while.exit"
      return ()
codegenStmt (Return val) =
  mdo x <- codegenExpr val
      ret x

codegenExpr :: MonadIRBuilder m => Expr -> m Operand
codegenExpr (Constant n) = return $ ConstantOperand $ C.Int 32 n
codegenExpr (Identifier id) =
  mdo let sig = AST.ptr $ AST.IntegerType 32
      load (LocalReference sig (mkName id)) 0
codegenExpr (Call name args) =
  mdo let sig =
            AST.ptr $
            AST.FunctionType
              AST.i32
              (toListOf (replicated $ length args) AST.i32)
              False
      ops <- mapMOf each codegenExpr args
      call
        (ConstantOperand $ C.GlobalReference sig (mkName name))
        (over mapped (, []) ops)
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
            Map.fromList
              [(Plus, plus), (Minus, minus), (Inc, inc), (Dec, dec), (Not, not)]
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
minus x =
  mdo cond <- icmp IP.SGE x (ConstantOperand (C.Int 32 0))
      condBr cond "if.then" "if.else"
      block `named` "if.then"
      trueVal <- mul x (ConstantOperand $ C.Int 32 (-1))
      br "if.exit"
      block `named` "if.else"
      falseVal <- return x
      br "if.exit"
      block `named` "if.exit"
      phi [(trueVal, "if.then"), (falseVal, "if.else")]

plus :: MonadIRBuilder m => Operand -> m Operand
plus x =
  mdo cond <- icmp IP.SLE x (ConstantOperand (C.Int 32 0))
      condBr cond "if.then" "if.else"
      block `named` "if.then"
      trueVal <- mul x (ConstantOperand $ C.Int 32 (-1))
      br "if.exit"
      block `named` "if.else"
      falseVal <- return x
      br "if.exit"
      block `named` "if.exit"
      phi [(trueVal, "if.then"), (falseVal, "if.else")]

not :: MonadIRBuilder m => Operand -> m Operand
not x =
  mdo cond <- icmp IP.EQ x (ConstantOperand (C.Int 32 0))
      condBr cond "if.then" "if.else"
      block `named` "if.then"
      trueVal <- return (ConstantOperand (C.Int 32 1))
      br "if.exit"
      block `named` "if.else"
      falseVal <- return (ConstantOperand (C.Int 32 0))
      br "if.exit"
      block `named` "if.exit"
      phi [(trueVal, "if.then"), (falseVal, "if.else")]
