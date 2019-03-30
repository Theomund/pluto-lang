module Parser where

import Lexer
import Syntax

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Debug

constantExpr :: Parser Expr
constantExpr = Constant <$> integer

identifierExpr :: Parser Expr
identifierExpr = Identifier <$> identifier

callExpr :: Parser Expr
callExpr = do
  name <- identifier
  args <- parens $ sepBy expr (symbol ",")
  return $ Call name args

expr :: Parser Expr
expr = makeExprParser exprTerms exprOperators

exprTerms :: Parser Expr
exprTerms = choice [try callExpr, identifierExpr, constantExpr, parens expr]

exprOperators :: [[Operator Parser Expr]]
exprOperators =
  [ [ Prefix (Unary Plus <$ symbol "+")
    , Prefix (Unary Minus <$ symbol "-")
    , Prefix (Unary Not <$ symbol "!")
    ]
  , [ InfixL (Binary Mul <$ symbol "*")
    , InfixL (Binary Div <$ symbol "/")
    , InfixL (Binary Mod <$ symbol "%")
    ]
  , [InfixL (Binary Add <$ symbol "+"), InfixL (Binary Sub <$ symbol "-")]
  , [ InfixL (Binary LessEqual <$ symbol "<=")
    , InfixL (Binary GreaterEqual <$ symbol ">=")
    , InfixL (Binary LessThan <$ symbol "<")
    , InfixL (Binary GreaterThan <$ symbol ">")
    ]
  , [ InfixL (Binary Equal <$ symbol "==")
    , InfixL (Binary NotEqual <$ symbol "!=")
    ]
  , [InfixL (Binary And <$ symbol "&&")]
  , [InfixL (Binary Or <$ symbol "||")]
  , [InfixR (Binary Assign <$ symbol "=")]
  ]

item :: Parser Item
item = choice [StmtItem <$> stmt, DeclItem <$> decl]

compoundStmt :: Parser Stmt
compoundStmt = do
  items <- brackets $ many item
  return $ CompoundStmt items

exprStmt :: Parser Stmt
exprStmt = do
  expr <- expr
  symbol ";"
  return $ ExprStmt expr

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond <- parens expr
  If cond <$> stmt

ifElseStmt :: Parser Stmt
ifElseStmt = do
  rword "if"
  cond <- parens expr
  body <- stmt
  rword "else"
  IfElse cond body <$> stmt

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- parens expr
  While cond <$> stmt

doWhileStmt :: Parser Stmt
doWhileStmt = do
  rword "do"
  body <- stmt
  rword "while"
  cond <- parens expr
  symbol ";"
  return $ DoWhile body cond

returnStmt :: Parser Stmt
returnStmt = do
  rword "return"
  value <- expr
  symbol ";"
  return $ Return value

stmt :: Parser Stmt
stmt =
  choice
    [ try ifElseStmt
    , ifStmt
    , whileStmt
    , doWhileStmt
    , returnStmt
    , exprStmt
    , compoundStmt
    ]

funcDecl :: Parser Decl
funcDecl = do
  rword "int"
  name <- identifier
  params <- parens $ sepBy decl (symbol ",")
  body <- stmt
  return $ Func name params body

varDecl :: Parser Decl
varDecl = do
  rword "int"
  name <- identifier
  symbol "="
  value <- expr
  symbol ";"
  return $ Var name value

decl :: Parser Decl
decl = choice [try funcDecl, varDecl]

parser :: Parser [Decl]
parser = between sc eof (some decl)
