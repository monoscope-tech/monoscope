-- Parser implemented with help and code from: https://markkarpov.com/tutorial/megaparsec.html
module Pkg.Parser (parseQueryStringToWhereClause) where

import Control.Monad.Combinators.Expr
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayParen, displayPrec)
import Relude hiding (GT, LT, many, some)
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data Values = Num Text | Str Text | Boolean Bool | Null
  deriving stock (Eq, Ord, Show)

data Subject = Subject Text [Text]
  deriving stock (Eq, Ord, Show)

data Expr
  = Eq Subject Values
  | NotEq Subject Values
  | GT Subject Values
  | LT Subject Values
  | GTEq Subject Values
  | LTEq Subject Values
  | Paren Expr
  | And Expr Expr
  | Or Expr Expr
  deriving stock (Eq, Ord, Show)

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

pSubject :: Parser Subject
pSubject = do
  sub <- toText <$> lexeme (some (alphaNumChar <|> oneOf @[] ['.', '-', '_']))
  case T.splitOn "." sub of
    (x : xs) -> pure $ Subject x xs
    _ -> error "unreachable step, empty subject in query unit expr parsing."

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pValues :: Parser Values
pValues =
  choice @[]
    [ Null <$ string "null",
      Boolean <$> ((True <$ string "true") <|> False <$ string "false" <|> False <$ string "FALSE" <|> True <$ string "TRUE"),
      Num . toText <$> some (digitChar <|> char '.'),
      Str . toText <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
    ]

pTerm :: Parser Expr
pTerm =
  try (Eq <$> pSubject <* void (symbol "=") <*> pValues)
    <|> try (NotEq <$> pSubject <* void (symbol "!=") <*> pValues)
    <|> try (GT <$> pSubject <* void (symbol ">") <*> pValues)
    <|> try (LT <$> pSubject <* void (symbol "<") <*> pValues)
    <|> try (GTEq <$> pSubject <* void (symbol ">=") <*> pValues)
    <|> try (LTEq <$> pSubject <* void (symbol "<=") <*> pValues)
    <|> Paren <$> parens pExpr

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary " AND " And,
      binary "AND" And,
      binary " OR " Or,
      binary "OR" Or,
      binary " and " And,
      binary "and" And,
      binary " or " Or,
      binary "or" Or
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

-- >>> parseTest parseQuery "a.b=\"x\" AND (x=1 OR b!=2) "
parseQuery :: Parser Expr
parseQuery = pExpr <* eof

-------------------------------------------------------
--
-- SQL Where clause segment interpreter
--
-------------------------------------------------------
instance Display Subject where
  displayPrec prec (Subject x []) = displayPrec prec x
  displayPrec prec (Subject x [y]) = displayPrec prec $ x <> "->>" <> "'" <> y <> "'"
  displayPrec prec (Subject x z) = do
    let z' = reverse $ map (\a -> "'" <> a <> "'") z
    let (y, ys) = (Unsafe.head z', reverse $ Unsafe.tail z')
    let val = x <> "->" <> T.intercalate "->" ys <> "->>" <> y
    displayPrec prec val

instance Display Values where
  displayPrec prec (Num a) = displayBuilder a
  displayPrec prec (Str a) = displayBuilder $ "'" <> a <> "'"
  displayPrec prec (Boolean True) = "'true'"
  displayPrec prec (Boolean False) = "'false'"
  displayPrec prec Null = "null"

instance Display Expr where
  displayPrec prec (Eq sub val) = displayParen (prec > 0) $ displayPrec prec sub <> displayPrec @Text prec "=" <> displayBuilder val
  displayPrec prec (NotEq sub val) = displayParen (prec > 0) $ displayPrec prec sub <> displayPrec @Text prec "!=" <> displayBuilder val
  displayPrec prec (GT sub val) = displayParen (prec > 0) $ displayPrec prec sub <> displayPrec @Text prec ">" <> displayBuilder val
  displayPrec prec (LT sub val) = displayParen (prec > 0) $ displayPrec prec sub <> displayPrec @Text prec "<" <> displayBuilder val
  displayPrec prec (GTEq sub val) = displayParen (prec > 0) $ displayPrec prec sub <> displayPrec @Text prec ">=" <> displayBuilder val
  displayPrec prec (LTEq sub val) = displayParen (prec > 0) $ displayPrec prec sub <> displayPrec @Text prec "<=" <> displayBuilder val
  displayPrec prec (Paren u1) = displayParen True $ displayPrec prec u1
  displayPrec prec (And u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " AND " <> displayBuilder u2
  displayPrec prec (Or u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " OR " <> displayBuilder u2

----------------------------------------------------------------------------------
-- Convert Query as string to a string capable of being
-- used int the where clause of an sql statement
----------------------------------------------------------------------------------
parseQueryStringToWhereClause :: Text -> Either Text Text
parseQueryStringToWhereClause q = if q == "" then Right "" else bimap (toText . errorBundlePretty) display (parse parseQuery "" q)
