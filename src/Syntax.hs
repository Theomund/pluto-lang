module Syntax where

data Stmt =
  Assign String
         Expr
  deriving (Show)

data Expr
  = Constant Integer
  | Var String
  | Neg Expr
  | Binary Op
           Expr
           Expr
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)
