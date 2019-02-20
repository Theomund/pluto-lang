module Parser where

import Syntax

import Control.Monad (void)
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

brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = lexeme L.decimal

expr :: Parser Expr
expr = makeExprParser terms operators

terms :: Parser Expr
terms = parens expr <|> Constant <$> integer <|> Var <$> identifier

operators :: [[Operator Parser Expr]]
operators =
  [ [Prefix (Neg <$ symbol "-")]
  , [InfixL (Binary Mul <$ symbol "*"), InfixL (Binary Div <$ symbol "/")]
  , [InfixL (Binary Add <$ symbol "+"), InfixL (Binary Sub <$ symbol "-")]
  ]

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = ["if"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

assign :: Parser Stmt
assign = do
  var <- identifier
  void (symbol "=")
  Assign var <$> expr

while :: Parser Stmt
while = do
  rword "while"
  cond <- (parens expr)
  While cond <$> brackets stmt

stmt :: Parser Stmt
stmt = assign <|> while

parser :: Parser Stmt
parser = between sc eof while
