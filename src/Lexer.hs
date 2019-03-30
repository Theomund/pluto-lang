module Lexer where

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

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = ["do", "if", "else", "while", "return", "int"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "Keyword " ++ show x ++ " cannot be an identifier"
        else return x
