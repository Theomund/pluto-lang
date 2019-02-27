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
  id <- identifier
  void (symbol "=")
  Assignment id <$> arithmeticExpression

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

statement :: Parser Statement
statement = compoundStatement <|> expressionStatement

parser :: Parser Statement
parser = between sc eof statement
