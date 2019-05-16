module Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Lexer
import Syntax
import Text.Megaparsec

-- | The 'constantExpr' function defines a parser for constant expressions.
constantExpr :: Parser Expr
constantExpr = Constant <$> integer

-- | The 'identifierExpr' function defines a parser for identifier expressions.
identifierExpr :: Parser Expr
identifierExpr = Identifier <$> identifier

-- | The 'callExpr' function defines a parser for function call expressions.
callExpr :: Parser Expr
callExpr = do
  name <- identifier
  args <- parens $ sepBy expr (symbol ",")
  return $ Call name args

-- | The 'expr' function defines a parser for unary and binary expressions.
expr :: Parser Expr
expr = makeExprParser exprTerms exprOperators

-- | The 'exprTerms' function defines the operands for the 'expr' parser.
exprTerms :: Parser Expr
exprTerms = choice [try callExpr, identifierExpr, constantExpr, parens expr]

-- | The 'exprOperators' function defines the operators for the 'expr' parser.
exprOperators :: [[Operator Parser Expr]]
exprOperators =
  [ [Postfix (Unary Inc <$ symbol "++"), Postfix (Unary Dec <$ symbol "--")]
  , [ Prefix (Unary Plus <$ symbol "+")
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

-- | The 'item' function defines a choice parser for compound statement items.
item :: Parser Item
item = choice [StmtItem <$> stmt, DeclItem <$> varDecl]

-- | The 'compoundStmt' function defines a parser for compound statements.
compoundStmt :: Parser Stmt
compoundStmt = do
  items <- brackets $ many item
  return $ CompoundStmt items

-- | The 'exprStmt' function defines a parser for expression statements.
exprStmt :: Parser Stmt
exprStmt = do
  expr <- expr
  symbol ";"
  return $ ExprStmt expr

-- | The 'ifStmt' function defines a parser for 'if' statements.
ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond <- parens expr
  If cond <$> stmt

-- | The 'ifElseStmt' function defines a parser for 'if-else' statements.
ifElseStmt :: Parser Stmt
ifElseStmt = do
  rword "if"
  cond <- parens expr
  body <- stmt
  rword "else"
  IfElse cond body <$> stmt

-- | The 'whileStmt' function defines a parser for 'while' statements.
whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- parens expr
  While cond <$> stmt

-- | The 'doWhileStmt' function defines a parser for 'do-while' statements.
doWhileStmt :: Parser Stmt
doWhileStmt = do
  rword "do"
  body <- stmt
  rword "while"
  cond <- parens expr
  symbol ";"
  return $ DoWhile body cond

-- | The 'returnStmt' function defines a parser for return statements.
returnStmt :: Parser Stmt
returnStmt = do
  rword "return"
  value <- expr
  symbol ";"
  return $ Return value

-- | The 'stmt' function defines a choice parser for statements.
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

-- | The 'funcDecl' function defines a parser for function declarations.
funcDecl :: Parser Decl
funcDecl = do
  rword "int"
  name <- identifier
  params <- parens $ sepBy (rword "int" >> identifierExpr) (symbol ",")
  Func name params <$> compoundStmt

-- | The 'externDecl' function defines a parser for external function declarations.
externDecl :: Parser Decl
externDecl = do
  rword "extern"
  rword "int"
  name <- identifier
  params <- parens $ sepBy (rword "int" >> identifierExpr) (symbol ",")
  symbol ";"
  return $ Extern name params

-- | The 'varDecl' function defines a parser for variable declarations.
varDecl :: Parser Decl
varDecl = do
  rword "int"
  name <- identifier
  symbol "="
  value <- expr
  symbol ";"
  return $ Var name value

-- | The 'decl' function defines a choice parser for declarations.
decl :: Parser Decl
decl = choice [try funcDecl, try externDecl, varDecl]

-- | The 'parser' function defines a top-level parser for declarations.
parser :: Parser [Decl]
parser = between sc eof (some decl)
