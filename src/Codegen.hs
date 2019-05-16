{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Codegen where

import Control.Lens hiding (assign)
import Control.Monad
import Data.ByteString.Char8 as BS (pack, putStrLn)
import Data.ByteString.Short as SH (toShort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String
import LLVM.AST (Operand(ConstantOperand, LocalReference), mkName)
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type as AST hiding (void)
import LLVM.AST.Typed
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

-- | The 'generateModule' function generates a LLVM module given an abstract syntax tree.
generateModule :: [Decl] -> String -> IO ()
generateModule xs name =
  mdo let mod = buildModule "exampleModule" $ traverseOf_ each codegenDecl xs
      withContext $ \ctx ->
        withHostTargetMachine $ \tm ->
          mdo llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
              BS.putStrLn llvm
              withModuleFromAST ctx mod (writeObjectToFile tm (File name))

-- | The 'codegenDecl' function generates LLVM intermediate representation code from a declaration.
codegenDecl :: MonadModuleBuilder m => Decl -> m Operand
codegenDecl (Var name (Constant n)) = global (mkName name) AST.i32 (C.Int 32 n)
codegenDecl (Extern name args) =
  extern (mkName name) (toListOf (replicated $ length args) AST.i32) AST.i32
codegenDecl (Func name args body) =
  mdo let names = over mapped (^. _Identifier) args
      let fargs =
            over mapped (\x -> (AST.i32, fromString $ x ^. _Identifier)) args
      function (mkName name) fargs AST.i32 $ \ops ->
        mdo block `named` "entry"
            let symtab = zip names ops
            forM symtab $ \t ->
              mdo var <-
                    alloca AST.i32 (Just $ ConstantOperand $ C.Int 32 4) 0 `named`
                    SH.toShort (BS.pack $ t ^. _1)
                  let sig = AST.ptr $ AST.IntegerType 32
                  store var 0 (t ^. _2)
            codegenStmt body (Set.fromList names)

-- | The 'codegenLocal' function generates LLVM intermediate representation code from local variable declarations.
codegenLocal :: MonadIRBuilder m => Decl -> Set.Set String -> m ()
codegenLocal (Var name val) params =
  mdo x <- codegenExpr val params
      alloca AST.i32 (Just $ ConstantOperand $ C.Int 32 4) 0 `named`
        SH.toShort (BS.pack name)
      let sig = AST.ptr $ AST.IntegerType 32
      store (LocalReference sig (mkName name)) 0 x

-- | The 'codegenStmt' function generates LLVM intermediate representation code from statements.
codegenStmt :: MonadIRBuilder m => Stmt -> Set.Set String -> m ()
codegenStmt (ExprStmt exp) params =
  mdo case exp of
        (Binary Assign (Identifier id) val) ->
          mdo let sig = AST.ptr $ AST.IntegerType 32
              x <- codegenExpr val params
              if Set.member id params
                then store (LocalReference sig (mkName $ id ++ "1")) 0 x
                else store (LocalReference sig (mkName id)) 0 x
        (Binary Assign _ _) ->
          error "The assignment operator only works on identifiers"
        _ ->
          mdo codegenExpr exp params
              return ()
codegenStmt (CompoundStmt items) params =
  mdo forOf each items $ \case
        StmtItem i -> codegenStmt i params
        DeclItem i -> codegenLocal i params
      return ()
codegenStmt (If exp body) params =
  mdo cond <- codegenExpr exp params
      ifThen <- freshName $ SH.toShort (BS.pack "if.then")
      ifExit <- freshName $ SH.toShort (BS.pack "if.exit")
      condBr cond ifThen ifExit
      emitBlockStart ifThen
      codegenStmt body params
      hasTerm <- hasTerminator
      if hasTerm
        then return ()
        else br ifExit
      emitBlockStart ifExit
codegenStmt (IfElse exp body elseBody) params =
  mdo cond <- codegenExpr exp params
      ifThen <- freshName $ SH.toShort (BS.pack "if.then")
      ifElse <- freshName $ SH.toShort (BS.pack "if.else")
      ifExit <- freshName $ SH.toShort (BS.pack "if.exit")
      condBr cond ifThen ifElse
      emitBlockStart ifThen
      codegenStmt body params
      hasTerm <- hasTerminator
      if hasTerm
        then return ()
        else br ifExit
      emitBlockStart ifElse
      codegenStmt elseBody params
      hasTermElse <- hasTerminator
      if hasTerm && hasTermElse
        then return ()
        else if not hasTerm && hasTermElse
               then return ()
               else br ifExit
      emitBlockStart ifExit
codegenStmt (While exp body) params =
  mdo initCond <- codegenExpr exp params
      whileBody <- freshName $ SH.toShort (BS.pack "while.body")
      whileExit <- freshName $ SH.toShort (BS.pack "while.exit")
      condBr initCond whileBody whileExit
      emitBlockStart whileBody
      codegenStmt body params
      hasTerm <- hasTerminator
      if hasTerm
        then return ()
        else mdo cond <- codegenExpr exp params
                 condBr cond whileBody whileExit
      emitBlockStart whileExit
      return ()
codegenStmt (DoWhile body exp) params =
  mdo whileBody <- freshName $ SH.toShort (BS.pack "while.body")
      whileExit <- freshName $ SH.toShort (BS.pack "while.exit")
      br whileBody
      emitBlockStart whileBody
      codegenStmt body params
      hasTerm <- hasTerminator
      if hasTerm
        then return ()
        else mdo cond <- codegenExpr exp params
                 condBr cond whileBody whileExit
      emitBlockStart whileExit
      return ()
codegenStmt (Return val) params =
  mdo blockName <- freshName $ SH.toShort (BS.pack "return")
      br blockName
      emitBlockStart blockName
      x <- codegenExpr val params
      ret x

-- | The 'codegenStmt' function generates LLVM intermediate representation code from expressions.
codegenExpr :: MonadIRBuilder m => Expr -> Set.Set String -> m Operand
codegenExpr (Constant n) _ = return $ ConstantOperand $ C.Int 32 n
codegenExpr (Identifier id) params =
  mdo let sig = AST.ptr $ AST.IntegerType 32
      if Set.member id params
        then load (LocalReference sig (mkName $ id ++ "1")) 0
        else load (LocalReference sig (mkName id)) 0
codegenExpr (Call name args) params =
  mdo let sig =
            AST.ptr $
            AST.FunctionType
              AST.i32
              (toListOf (replicated $ length args) AST.i32)
              False
      ops <- mapMOf each (`codegenExpr` params) args
      call
        (ConstantOperand $ C.GlobalReference sig (mkName name))
        (over mapped (, []) ops)
codegenExpr (Binary op a b) params =
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
          mdo x <- codegenExpr a params
              y <- codegenExpr b params
              f x y
        Nothing -> error "No such operator was found."
codegenExpr (Unary op a) params =
  mdo let unOps =
            Map.fromList
              [ (Plus, plus)
              , (Minus, minus)
              , (Inc, inc)
              , (Dec, dec)
              , (Not, lnot)
              ]
      case Map.lookup op unOps of
        Just f -> f a params
        Nothing -> error "No such operator was found."

-- | The 'inc' function generates LLVM intermediate representation code from increment expressions.
inc :: MonadIRBuilder m => Expr -> Set.Set String -> m Operand
inc (Identifier id) params =
  mdo x <- codegenExpr (Identifier id) params
      result <- add x (ConstantOperand $ C.Int 32 1)
      let sig = AST.ptr $ AST.IntegerType 32
      if Set.member id params
        then store (LocalReference sig (mkName $ id ++ "1")) 0 result
        else store (LocalReference sig (mkName id)) 0 result
      return result
inc _ _ = error "The increment operator can only work on identifiers."

-- | The 'dec' function generates LLVM intermediate representation code from decrement expressions.
dec :: MonadIRBuilder m => Expr -> Set.Set String -> m Operand
dec (Identifier id) params =
  mdo x <- codegenExpr (Identifier id) params
      result <- sub x (ConstantOperand $ C.Int 32 1)
      let sig = AST.ptr $ AST.IntegerType 32
      if Set.member id params
        then store (LocalReference sig (mkName $ id ++ "1")) 0 result
        else store (LocalReference sig (mkName id)) 0 result
      return result
dec _ _ = error "The decrement operator can only work on identifiers."

-- | The 'minus' function generates LLVM intermediate representation code from unary minus expressions.
minus :: MonadIRBuilder m => Expr -> Set.Set String -> m Operand
minus a params =
  mdo x <- codegenExpr a params
      cond <- icmp IP.SGE x (ConstantOperand (C.Int 32 0))
      condBr cond "if.then" "if.else"
      block `named` "if.then"
      trueVal <- mul x (ConstantOperand $ C.Int 32 (-1))
      br "if.exit"
      block `named` "if.else"
      falseVal <- return x
      br "if.exit"
      block `named` "if.exit"
      phi [(trueVal, "if.then"), (falseVal, "if.else")]

-- | The 'plus' function generates LLVM intermediate representation code from unary plus expressions.
plus :: MonadIRBuilder m => Expr -> Set.Set String -> m Operand
plus a params =
  mdo x <- codegenExpr a params
      cond <- icmp IP.SLE x (ConstantOperand (C.Int 32 0))
      condBr cond "if.then" "if.else"
      block `named` "if.then"
      trueVal <- mul x (ConstantOperand $ C.Int 32 (-1))
      br "if.exit"
      block `named` "if.else"
      falseVal <- return x
      br "if.exit"
      block `named` "if.exit"
      phi [(trueVal, "if.then"), (falseVal, "if.else")]

-- | The 'lnot' function generates LLVM intermediate representation code from unary logical not expressions.
lnot :: MonadIRBuilder m => Expr -> Set.Set String -> m Operand
lnot a params =
  mdo x <- codegenExpr a params
      cond <- icmp IP.EQ x (ConstantOperand (C.Int 32 0))
      condBr cond "if.then" "if.else"
      block `named` "if.then"
      trueVal <- return (ConstantOperand (C.Int 32 1))
      br "if.exit"
      block `named` "if.else"
      falseVal <- return (ConstantOperand (C.Int 32 0))
      br "if.exit"
      block `named` "if.exit"
      phi [(trueVal, "if.then"), (falseVal, "if.else")]
