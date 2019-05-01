{-# LANGUAGE DeriveDataTypeable #-}

module Syntax where

import           Control.Lens
import           Data.Data

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

instance Plated Stmt where
  plate _ (ExprStmt x) = ExprStmt <$> pure x
  plate f (CompoundStmt xs) = CompoundStmt <$> traverse pure xs
  plate f (If x y) = If <$> pure x <*> f y 
  plate f (IfElse x y z) = IfElse <$> pure x <*> f y <*> f z
  plate f (While x y) = While <$> pure x <*> f y
  plate f (DoWhile x y) = DoWhile <$> f x <*> pure y 
  plate _ (Return x) = Return <$> pure x

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

instance Plated Decl where
  plate _ (Extern x y) = Extern <$> pure x <*> traverse pure y
  plate _ (Func x y z) = Func <$> pure x <*> traverse pure y <*> pure z
  plate _ (Var x y) = Var <$> pure x <*> pure y

data Item
  = StmtItem Stmt
  | DeclItem Decl
  deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Plated Item where
  plate _ (StmtItem x) = StmtItem <$> pure x 
  plate _ (DeclItem x) = DeclItem <$> pure x 

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
