module Syntax where

type Name = String

data Statement
  = ExpressionStatement Expression
  | CompoundStatement [Statement]
  | If Expression
       Statement
  | IfElse Expression
           Statement
           Statement
  | While Expression
          Statement
  | Return Expression
  deriving (Show)

data Expression
  = Assignment AOp
               Expression
               Expression
  | Comparison COp
               Expression
               Expression
  | Constant Integer
  | Variable String
  | Binary AOp
           Expression
           Expression
  | Unary UOp
          Expression
  deriving (Show)

data Declaration =
  Function Name
           Statement
  deriving (Show)

data AOp
  = Add
  | Sub
  | Mul
  | Div
  | Basic
  deriving (Show)

data COp
  = Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  deriving (Show)

data UOp
  = Plus
  | Minus
  deriving (Show)
