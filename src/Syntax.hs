module Syntax where

data Statement
  = ExpressionStatement [Expression]
  | CompoundStatement [Statement]
  deriving (Show)

data Expression
  = Assignment String
               Expression
  | Constant Integer
  | Variable String
  | Binary Op
           Expression
           Expression
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)
