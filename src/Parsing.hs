module Parsing where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseWith :: Parser a -> String -> a
parseWith p s = case parse (p <* eof) "" s of
  Left bundle -> error (errorBundlePretty bundle)
  Right x -> x

symbol :: String -> Parser String
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

integer :: Parser Int
integer = lexeme L.decimal
