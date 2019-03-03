module Parser where

import Lexer
import Syntax

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec

arithmeticExpression :: Parser Expression
arithmeticExpression = makeExprParser arithmeticTerms arithmeticOperators

arithmeticTerms :: Parser Expression
arithmeticTerms =
  parens arithmeticExpression <|> Constant <$> integer <|>
  Variable <$> identifier

arithmeticOperators :: [[Operator Parser Expression]]
arithmeticOperators =
  [ [InfixL (Binary Mul <$ symbol "*"), InfixL (Binary Div <$ symbol "/")]
  , [InfixL (Binary Add <$ symbol "+"), InfixL (Binary Sub <$ symbol "-")]
  ]

assignmentExpression :: Parser Expression
assignmentExpression = do
  ident <- identifier
  void equals
  Assignment ident <$> arithmeticExpression

expression :: Parser [Expression]
expression = sepBy1 assignmentExpression comma

compoundStatement :: Parser Statement
compoundStatement = do
  stmt <- brackets $ many statement
  return $ CompoundStatement stmt

expressionStatement :: Parser Statement
expressionStatement = do
  expr <- expression
  void semi
  return $ ExpressionStatement expr

ifStatement :: Parser Statement
ifStatement = do
  rword "if"
  cond <- parens expression
  If cond <$> statement

ifElseStatement :: Parser Statement
ifElseStatement = do
  rword "if"
  cond <- parens expression
  stmt <- statement
  rword "else"
  IfElse cond stmt <$> statement

whileStatement :: Parser Statement
whileStatement = do
  rword "while"
  cond <- parens expression
  While cond <$> statement

statement :: Parser Statement
statement =
  expressionStatement <|> compoundStatement <|> try ifElseStatement <|>
  ifStatement <|>
  whileStatement

declaration :: Parser Declaration
declaration = do
  rword "int"
  name <- identifier
  void (symbol "(")
  void (symbol ")")
  Function name <$> statement

parser :: Parser [Declaration]
parser = between sc eof (some declaration)
