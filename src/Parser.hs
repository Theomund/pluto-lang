module Parser where

import Lexer
import Syntax

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Debug

arithmeticExpression :: Parser Expression
arithmeticExpression = makeExprParser arithmeticTerms arithmeticOperators

arithmeticTerms :: Parser Expression
arithmeticTerms =
  parens arithmeticExpression <|> variableExpression <|> constantExpression

arithmeticOperators :: [[Operator Parser Expression]]
arithmeticOperators =
  [ [Prefix (Unary Plus <$ symbol "+"), Prefix (Unary Minus <$ symbol "-")]
  , [InfixL (Binary Mul <$ symbol "*"), InfixL (Binary Div <$ symbol "/")]
  , [InfixL (Binary Add <$ symbol "+"), InfixL (Binary Sub <$ symbol "-")]
  ]

assignmentExpression :: Parser Expression
assignmentExpression = makeExprParser assignmentTerms assignmentOperators

assignmentTerms :: Parser Expression
assignmentTerms =
  parens assignmentExpression <|> variableExpression <|> constantExpression

assignmentOperators :: [[Operator Parser Expression]]
assignmentOperators =
  [ [InfixL (Assignment Basic <$ symbol "=")]
  , [ InfixL (Assignment Mul <$ symbol "*=")
    , InfixL (Assignment Div <$ symbol "/=")
    ]
  , [ InfixL (Assignment Add <$ symbol "+=")
    , InfixL (Assignment Sub <$ symbol "-=")
    ]
  ]

comparisonExpression :: Parser Expression
comparisonExpression = makeExprParser comparisonTerms comparisonOperators

comparisonTerms :: Parser Expression
comparisonTerms =
  parens comparisonExpression <|> variableExpression <|> constantExpression

comparisonOperators :: [[Operator Parser Expression]]
comparisonOperators =
  [ [ InfixL (Comparison Equal <$ symbol "==")
    , InfixL (Comparison NotEqual <$ symbol "!=")
    ]
  , [ InfixL (Comparison LessThan <$ symbol "<")
    , InfixL (Comparison GreaterThan <$ symbol ">")
    ]
  , [ InfixL (Comparison LessEqual <$ symbol "<=")
    , InfixL (Comparison GreaterEqual <$ symbol ">=")
    ]
  ]

constantExpression :: Parser Expression
constantExpression = Constant <$> integer

variableExpression :: Parser Expression
variableExpression = Variable <$> identifier

expression :: Parser Expression
expression =
  arithmeticExpression <|> assignmentExpression <|> comparisonExpression <|>
  constantExpression <|>
  variableExpression

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

returnStatement :: Parser Statement
returnStatement = do
  rword "return"
  expr <- expression
  void semi
  return $ Return expr

statement :: Parser Statement
statement =
  returnStatement <|> expressionStatement <|> try compoundStatement <|>
  ifElseStatement <|>
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
parser = between sc eof (dbg "DEBUG" (some declaration))
