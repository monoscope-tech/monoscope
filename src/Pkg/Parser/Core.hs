module Pkg.Parser.Core (Parser, lexeme, symbol, sc) where

import Relude
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L


type Parser = Parsec Void Text


sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)


-- lexeme is a wrapper for lexemes that picks up all trailing white space using the supplied space consumer.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


-- symbol is a parser that matches given text using string internally and then similarly picks up all trailing white space.
symbol :: Text -> Parser Text
symbol = L.symbol sc
