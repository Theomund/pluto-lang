module Parser where

import Syntax

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

expr :: Parser Expr
expr = makeExprParser terms operators

terms :: Parser Expr
terms = parens expr <|> Constant <$> integer

operators :: [[Operator Parser Expr]]
operators =
  [ [InfixL (Binary Mul <$ symbol "*"), InfixL (Binary Div <$ symbol "/")]
  , [InfixL (Binary Add <$ symbol "+"), InfixL (Binary Sub <$ symbol "-")]
  ]

parser :: Parser Expr
parser = between sc eof expr
