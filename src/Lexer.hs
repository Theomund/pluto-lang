module Lexer where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- | The 'sc' function consumes whitespace and comments.
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- | The 'lexeme' function defines a lexeme for consuming whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | The 'symbol' function defines a parser for parsing symbols.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | The 'parens' function defines a parser for parsing parantheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | The 'brackets' function defines a parser for parsing brackets.
brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

-- | The 'integer' function defines a parser for parsing integer values (-1, 0, 1, ...).
integer :: Parser Integer
integer = lexeme L.decimal

-- | The 'rword' function defines a parser for reserved words in the language.
rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- | The 'rws' function defines a list for reserved words.
rws :: [String]
rws = ["do", "if", "else", "while", "return", "fun", "var", "extern"]

-- | The 'identifier' function defines a parser for parsing identifiers.
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "Keyword " ++ show x ++ " cannot be an identifier"
        else return x
