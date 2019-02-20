module Syntax where

data Expr
  = Constant Integer
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
