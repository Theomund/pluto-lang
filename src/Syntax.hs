{-# LANGUAGE DeriveDataTypeable #-}

module Syntax where

import Control.Lens
import Data.Data
import Data.Data.Lens (uniplate)

type Name = String

data Stmt
  = ExprStmt Expr
  | CompoundStmt [Item]
  | If Expr
       Stmt
  | IfElse Expr
           Stmt
           Stmt
  | While Expr
          Stmt
  | DoWhile Stmt
            Expr
  | Return Expr
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Expr
  = Constant Integer
  | Identifier String
  | Call Name
         [Expr]
  | Binary Op
           Expr
           Expr
  | Unary Op
          Expr
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Decl
  = Extern Name
           [Expr]
  | Func Name
         [Expr]
         Stmt
  | Var Name
        Expr
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Item
  = StmtItem Stmt
  | DeclItem Decl
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Op
  = Inc
  | Dec
  | Plus
  | Minus
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Assign
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  | And
  | Or
  | Not
  deriving (Eq, Ord, Show, Read, Data, Typeable)
