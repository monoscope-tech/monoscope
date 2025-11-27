module Pkg.Parser.Core (Parser, symbol, sc, ToQueryText (..)) where

import Relude
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L


type Parser = Parsec Void Text


class ToQueryText a where
  toQText :: a -> Text


sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)


symbol :: Text -> Parser Text
symbol = L.symbol sc
