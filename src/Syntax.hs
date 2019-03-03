module Syntax where

type Name = String

data Statement
  = ExpressionStatement [Expression]
  | CompoundStatement [Statement]
  | If [Expression]
       Statement
  | IfElse [Expression]
           Statement
           Statement
  | While [Expression]
          Statement
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

data Declaration =
  Function Name
           Statement
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)
