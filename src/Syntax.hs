module Syntax where

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
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data Decl
  = Func Name
         [Decl]
         Stmt
  | Var Name
        Expr
  deriving (Show, Eq)

data Item
  = StmtItem Stmt
  | DeclItem Decl
  deriving (Show, Eq)

data Op
  = Plus
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
  deriving (Show, Eq)
