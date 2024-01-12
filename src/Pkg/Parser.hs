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


-- Example queries
-- request_body.v1.v2 = "abc" AND (request_body.v3.v4 = 123 OR request_body.v5[].v6=ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 |> {.v7, .v8} |>

type Parser = Parsec Void Text


-- Values is an enum of the list of supported value types.
-- Num is a text  that represents a float as float covers ints in a lot of cases. But its basically the json num type.
data Values = Num Text | Str Text | Boolean Bool | Null | List [Values]
  deriving stock (Eq, Ord, Show)


-- A subject consists of the primary key, and then the list of fields keys which are delimited by a .
-- To support jsonpath, we will have more powerfule field keys, so instead of a text array, we could have an enum field key type?
data Subject = Subject Text [FieldKey]
  deriving stock (Eq, Ord, Show)


data FieldKey = FieldKey Text | ArrayIndex Text Int | ArrayWildcard Text
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
  | Not Expr
  | FunctionCall String [Expr] -- For functions like ANY
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


-- >>> parse pSubject "" "key"
-- Right (Subject "key" [FieldKey ""])
-- >>> parse pSubject "" "key.abc[1]"
-- Right (Subject "key" [FieldKey "",ArrayIndex "abc" 1])
-- >>> parse pSubject "" "key.abc[*]"
-- Right (Subject "key" [FieldKey "",ArrayWildcard "abc"])
-- >>> parse pSubject "" "key.abc[*].xyz"
-- Right (Subject "key" [FieldKey "",ArrayWildcard "abc",FieldKey "xyz"])
-- >>> parse pSubject "" "abc[*].xyz"
-- Right (Subject "abc" [ArrayWildcard "",FieldKey "xyz"])
-- >>> parse pSubject "" "abc[1].xyz.cde[*]"
-- Right (Subject "abc" [ArrayIndex "" 1,FieldKey "xyz",ArrayWildcard "cde"])
pSubject :: Parser Subject
pSubject = do
  primaryKey <- toText <$> some (alphaNumChar <|> oneOf ("-_" :: [Char]))
  firstField <- optional $ pSquareBracketKey ""
  fields <- many $ char '.' *> pFieldKey
  return $ Subject primaryKey $ maybeToList firstField ++ fields


-- >>> parse pFieldKey "" "key.abc[1]"
-- Right (FieldKey "key")
-- >>> parse pFieldKey "" "abc[1]"
-- Right (ArrayIndex "abc" 1)
-- >>> parse pFieldKey "" "abc[*]"
-- Right (ArrayWildcard "abc")
pFieldKey :: Parser FieldKey
pFieldKey = do
  key <- toText <$> some (alphaNumChar <|> oneOf ("-_" :: [Char]))
  pSquareBracketKey key


-- >>> parse (pSquareBracketKey "") "" "[1]"
-- Right (ArrayIndex "" 1)
-- >>> parse (pSquareBracketKey "key") "" "[*]"
-- Right (ArrayWildcard "key")
pSquareBracketKey :: Text -> Parser FieldKey
pSquareBracketKey key = do
  arrayAccess <- optional $ sqParens (try (Right <$> num) <|> Left <$> star)
  case arrayAccess of
    Just (Left ()) -> pure $ ArrayWildcard key
    Just (Right idx) -> pure $ ArrayIndex key idx
    Nothing -> pure $ FieldKey key
  where
    num :: Parser Int
    num = Unsafe.read <$> some digitChar
    star = char '*' >> pass


sqParens :: Parser a -> Parser a
sqParens = between (symbol "[") (symbol "]")


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


pValues :: Parser Values
pValues =
  choice @[]
    [ Null <$ string "null"
    , Boolean <$> (True <$ string "true" <|> False <$ string "false" <|> False <$ string "FALSE" <|> True <$ string "TRUE")
    , Num . toText <$> some (digitChar <|> char '.')
    , Str . toText <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
    ]


pTerm :: Parser Expr
pTerm =
  try (Eq <$> pSubject <* void (symbol "=") <*> pValues)
    <|> try (NotEq <$> pSubject <* void (symbol "!=") <*> pValues)
    <|> try (GT <$> pSubject <* void (symbol ">") <*> pValues)
    <|> try (LT <$> pSubject <* void (symbol "<") <*> pValues)
    <|> try (GTEq <$> pSubject <* void (symbol ">=") <*> pValues)
    <|> try (LTEq <$> pSubject <* void (symbol "<=") <*> pValues)
    <|> Paren
    <$> parens pExpr


pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ binary " AND " And
    , binary "AND" And
    , binary " OR " Or
    , binary "OR" Or
    , binary " and " And
    , binary "and" And
    , binary " or " Or
    , binary "or" Or
    ]
  ]


binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)


-- >>> parseQueryStringToWhereClause "a.b=\"x\" AND (x=1 OR b!=2) "
-- Right "a->>'b'='x' AND (x=1 OR b!=2)"
-- >>> parseQueryStringToWhereClause "a.b[*].c=\"x\""
-- Right "a->'b[*]'->>'c'='x'"
-- >>> parseQueryStringToWhereClause "a.b.c=\"xyz\""
-- Right "a->'b'->>'c'='xyz'"

-- parse parseQuery "" "request_body.v1.v2 = \"abc\""
-- Eq(Subject "request_body" [FieldKey "v1", FieldKey "v2"]) (Str "abc").
parseQuery :: Parser Expr
parseQuery = pExpr <* eof


-- TODO:
--
--  - Query arrays
--  - - query real numbers
--  - - wildcard query
--  - Group by
--  - Aggregate by / rollup
--  -

-------------------------------------------------------
--
-- SQL Where clause segment interpreter
--
-------------------------------------------------------
-- instance Display Subject where
--   displayPrec prec (Subject x []) = displayPrec prec x
--   displayPrec prec (Subject x [y]) = displayPrec prec $ x <> "->>" <> "'" <> y <> "'"
--   displayPrec prec (Subject x z) = do
--     let z' = reverse $ map (\a -> "'" <> a <> "'") z
--     let (y, ys) = (Unsafe.head z', reverse $ Unsafe.tail z')
--     let val = x <> "->" <> T.intercalate "->" ys <> "->>" <> y
--     displayPrec prec val

instance Display Subject where
  displayPrec prec (Subject x []) = displayPrec prec x
  displayPrec prec (Subject x ys) = do
    let (fields, jsonPaths) = foldr processField ([], False) ys
    let val =
          if jsonPaths
            then buildJsonArrayElements x fields
            else x <> "->" <> T.intercalate "->" fields
    displayPrec prec val


-- Process each field to detect and handle JSON array path
processField :: Text -> ([Text], Bool) -> ([Text], Bool)
processField field (acc, jsonPathDetected) =
  if T.isSuffixOf "[]" field
    then (T.dropEnd 2 field : acc, True)
    else (field : acc, jsonPathDetected)


-- Build the query with json_array_elements
buildJsonArrayElements :: Text -> [Text] -> Text
buildJsonArrayElements x fields = foldr applyJsonArrayElements "" fields
  where
    applyJsonArrayElements field acc =
      if T.null acc
        then x <> "->" <> "'" <> field <> "'"
        else "json_array_elements(" <> acc <> ")->" <> "'" <> field <> "'"


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
-- used in the where clause of an sql statement
----------------------------------------------------------------------------------
-- >>> parseQueryStringToWhereClause "request_body.message!=\"blabla\" AND method=\"GET\""
-- Right "request_body->>'message'!='blabla' AND method='GET'"
-- >>> parseQueryStringToWhereClause "request_body.message!=\"blabla\" AND method=\"GET\""
-- Right "request_body->>'message'!='blabla' AND method='GET'"
-- >>> parseQueryStringToWhereClause "request_body.message.tags[*].name!=\"blabla\" AND method=\"GET\""
-- Left "1:26:\n  |\n1 | request_body.message.tags[*].name!=\"blabla\" AND method=\"GET\"\n  |                          ^^\nunexpected \"[*\"\nexpecting \"!=\", \"<=\", \">=\", '<', '=', '>', or alphanumeric character\n"
parseQueryStringToWhereClause :: Text -> Either Text Text
parseQueryStringToWhereClause q = if q == "" then Right "" else bimap (toText . errorBundlePretty) display (parse parseQuery "" q)
