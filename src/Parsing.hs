module Parsing (Parser, parseWith) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseWith :: Parser a -> String -> a
parseWith p s = case parse p "" s of
  Left bundle -> error (errorBundlePretty bundle)
  Right x -> x

symbol :: String -> Parser String
symbol = L.symbol space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space1

integer :: Parser Integer
integer = lexeme L.decimal
