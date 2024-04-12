module Pkg.Parser.Expr where

import Control.Monad.Combinators.Expr
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayParen, displayPrec)
import Data.Text.Lazy.Builder (Builder)
import Pkg.Parser.Types
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (GT, LT, Sum, many, some)


-- Example queries
-- request_body.v1.v2 = "abc" AND (request_body.v3.v4 = 123 OR request_body.v5[].v6=ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |

-- >>> request_body="jfdshkjfds" | stat field1, field2 by field3

-- >>> parse pSubject "" "key"
-- Right (Subject "key" "key" [])
-- >>> parse pSubject "" "*"
-- Right (Subject "*" "*" [])
-- >>> parse pSubject "" "key.abc[1]"
-- Right (Subject "key.abc[1]" "key" [ArrayIndex "abc" 1])
-- >>> parse pSubject "" "key.abc[*]"
-- Right (Subject "key.abc[*]" "key" [ArrayWildcard "abc"])
-- >>> parse pSubject "" "key.abc[*].xyz"
-- Right (Subject "key.abc[*].xyz" "key" [ArrayWildcard "abc",FieldKey "xyz"])
-- >>> parse pSubject "" "abc[*].xyz"
-- Right (Subject "abc[*].xyz" "abc" [ArrayWildcard "",FieldKey "xyz"])
-- >>> parse pSubject "" "abc[1].xyz.cde[*]"
-- Right (Subject "abc[1].xyz.cde[*]" "abc" [ArrayIndex "" 1,FieldKey "xyz",ArrayWildcard "cde"])
-- >>> parse pSubject "" "request_body.message.tags[*].name"
-- Right (Subject "request_body.message.tags[*].name" "request_body" [FieldKey "message",ArrayWildcard "tags",FieldKey "name"])
-- >>> parse pSubject "" "request_body.roles[*]"
-- Right (Subject "request_body.roles[*]" "request_body" [ArrayWildcard "roles"])
pSubject :: Parser Subject
pSubject = do
  startPos <- getOffset
  restOfInputToProcess <- getInput
  (primaryKey, firstField) <- pPrimaryKey
  fields <- many $ char '.' *> pFieldKey
  endPos <- getOffset
  let entireLength = endPos - startPos
  let entire = T.take entireLength restOfInputToProcess
  return $ Subject entire primaryKey $ maybeToList firstField ++ fields


-- >>> parse pFieldKey "" "key.abc[1]"
-- Right (FieldKey "key")
--
-- >>> parse pFieldKey "" "abc[1]"
-- Right (ArrayIndex "abc" 1)
--
-- >>> parse pFieldKey "" "abc[*]"
-- Right (ArrayWildcard "abc")
pFieldKey :: Parser FieldKey
pFieldKey = do
  key <- toText <$> some (alphaNumChar <|> oneOf ("-_" :: String))
  try (pSquareBracketKey key) <|> pure (FieldKey key)


-- >>> parse ( pPrimaryKey ) "" "abc[1]"
-- Right ("abc",Just (ArrayIndex "" 1))
--
-- >>> parse ( pPrimaryKey ) "" "abc[*]"
-- Right ("abc",Just (ArrayWildcard ""))
--
-- >>> parse ( pPrimaryKey ) "" "abc"
-- Right ("abc",Nothing)
pPrimaryKey :: Parser (Text, Maybe FieldKey)
pPrimaryKey = do
  key <- toText <$> some (alphaNumChar <|> oneOf ("-_*" :: String))
  fKey <- optional $ pSquareBracketKey ""
  pure (key, fKey)


-- | pSquareBracketKey parses an array element, usually an index with an integer within the bracket
-- or an asterisk indicating a wildcard
--
-- >>> parse (pSquareBracketKey "") "" "[1]"
-- Right (ArrayIndex "" 1)
--
-- >>> parse (pSquareBracketKey "key") "" "[*]"
-- Right (ArrayWildcard "key")
pSquareBracketKey :: Text -> Parser FieldKey
pSquareBracketKey key = sqParens (arrayWildcard <|> arrayIndex)
  where
    arrayWildcard = ArrayWildcard key <$ char '*'
    arrayIndex = ArrayIndex key <$> L.decimal


sqParens :: Parser a -> Parser a
sqParens = between (symbol "[") (symbol "]")


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


-- | parse values into our internal AST representation. Int, Str, Num, Bool, List, etc
--
-- Examples:
--
-- >>> parseTest pValues "[1,2,3]"
-- Right (List [Num "1",Num "2",Num "3"])
--
-- >>> parseTest pValues "[true,false]"
-- Right (List [Boolean True,Boolean False])
--
-- >>> parseTest pValues "[\"as\",1,2]"
-- Right (List [Str "as",Num "1",Num "2"])
--
-- >>> parseTest pValues "[\"as\",\"b\"]"
-- Right (List [Str "as",Str "b"])
pValues :: Parser Values
pValues =
  choice @[]
    [ Null <$ string "null"
    , Boolean <$> (True <$ string "true" <|> False <$ string "false" <|> False <$ string "FALSE" <|> True <$ string "TRUE")
    , Num . toText <$> some (digitChar <|> char '.')
    , Str . toText <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
    , List <$> sqParens (pValues `sepBy` char ',')
    ]


-- | pTerm is the main entry point that desides what tree lines to decend
--
-- Exampes:
--
-- >>> parseTest pTerm "abc != \"GET\""
pTerm :: Parser Expr
pTerm =
  (Paren <$> parens pExpr)
    <|> try (Eq <$> pSubject <* space <* void (symbol "==") <* space <*> pValues)
    <|> try (NotEq <$> pSubject <* space <* void (symbol "!=") <* space <*> pValues)
    <|> try (GT <$> pSubject <* space <* void (symbol ">") <* space <*> pValues)
    <|> try (LT <$> pSubject <* space <* void (symbol "<") <* space <*> pValues)
    <|> try (GTEq <$> pSubject <* space <* void (symbol ">=") <* space <*> pValues)
    <|> try (LTEq <$> pSubject <* space <* void (symbol "<=") <* space <*> pValues)
    <|> try regexParser


-- >>> parse regexParser "" "abc=~/abc.*/"
-- Right (Regex (Subject "abc" []) "abc.*")
regexParser :: Parser Expr
regexParser = do
  subj <- pSubject
  space
  void $ symbol "=~"
  space
  regexStr <- char '/' *> manyTill L.charLiteral (char '/')
  pure $ Regex subj (toText regexStr)


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


-- TODO:
--
--  - Query arrays
--  - - query real numbers [x]
--  - - wildcard query [x]
--  - - array values [x]
--  - - regex [x]
--  - Group by [~]
--  - Aggregate by / rollup [~]
--  -

-- Core problem to solve is metrics. Metrics can be any of these values,
-- but must be aggregated over a timeline
-- Suporting a splunk like query style.
-- https://chat.openai.com/share/cc9553fd-1e02-482b-a01f-427ca755d977
--
-- stats
-- timechart
-- fields
-- where condition for alerts.
--

-------------------------------------------------------
--
-- SQL Where clause segment interpreter
--
-------------------------------------------------------

-- Helper function to detect if Subject contains an ArrayWildcard
subjectHasWildcard :: Subject -> Bool
subjectHasWildcard (Subject _ _ keys) = any isArrayWildcard keys
  where
    isArrayWildcard (ArrayWildcard _) = True
    isArrayWildcard _ = False


-- >>> display (Subject "" "request_body" [FieldKey "message"])
-- "request_body->>'message' as message"
--
-- >>> display (Subject "" "request_body" [FieldKey "message", FieldKey "value"])
-- "request_body->'message'->>'value' as value"
--
-- >>> display (Subject "" "errors" [ArrayIndex "" 0, FieldKey "message"])
-- "errors->0->>'message' as message"
--
-- >>> display (Subject "" "errors" [ArrayIndex "" 0, FieldKey "message"])
-- "errors->0->>'message' as message"
instance Display Subject where
  displayPrec prec (Subject entire x []) = displayPrec prec x
  displayPrec prec (Subject entire x (y : ys)) =
    displayPrec prec $ buildQuerySequence x (y : ys) <> " as " <> normalizeKeyPath entire
    where
      normalizeKeyPath txt = T.toLower $ T.replace "]" "❳" $ T.replace "[" "❲" $ T.replace "." "•" txt

      buildQuerySequence :: Text -> [FieldKey] -> Text
      buildQuerySequence acc [] = acc
      buildQuerySequence acc [lastKey] = buildQuery acc lastKey True
      buildQuerySequence acc (key : rest) = buildQuerySequence (buildQuery acc key False) rest

      buildQuery :: Text -> FieldKey -> Bool -> Text
      buildQuery acc (FieldKey key) isLast = acc <> separator isLast <> key <> "'"
      buildQuery acc (ArrayWildcard key) isLast = acc <> separator isLast <> key <> "'"
      buildQuery acc (ArrayIndex "" idx) isLast = acc <> separatorInt isLast <> show idx
      buildQuery acc (ArrayIndex key idx) isLast = acc <> separator False <> key <> "'" <> separatorInt isLast <> show idx

      separator isLast = if isLast then "->>'" else "->'"
      separatorInt isLast = if isLast then "->>" else "->"


-- ArrayWildcard handling is marked as unreachable, assuming it's not used in your context.

-- >>> display (List [Str "a"])
-- "ARRAY['a']"
--
-- >>> display (List [Num "2"])
-- "ARRAY[2]"
instance Display Values where
  displayPrec prec (Num a) = displayBuilder a
  displayPrec prec (Str a) = displayBuilder $ "'" <> a <> "'"
  displayPrec prec (Boolean True) = "'true'"
  displayPrec prec (Boolean False) = "'false'"
  displayPrec prec Null = "null"
  displayPrec prec (List vs) =
    let arrayElements = mconcat . intersperse "," . map (displayBuilder . display) $ vs
     in "ARRAY[" <> arrayElements <> "]"


-- | Render the expr ast to a value. Start with Eq only, for supporting jsonpath
--
-- >>> display (Eq (Subject "" "request_body" [FieldKey "message"]) (Str "val"))
-- "request_body->>'message'=='val'"
--
-- >>> display (Eq (Subject "" "errors" [ArrayIndex "" 0, FieldKey "message"]) (Str "val"))
-- "errors->0->>'message'=='val'"
--
-- >>> display (Eq (Subject "" "abc" [ArrayWildcard "",FieldKey "xyz"]) (Str "val"))
-- "jsonb_path_exists(abc, $$$[*].\"xyz\" ?? (@ == \"val\")$$)"
--
-- >>> display (Eq (Subject "" "errors" [ArrayWildcard "", ArrayIndex "message" 0, FieldKey "details"]) (Str "details"))
-- "jsonb_path_exists(errors, $$$[*][0].\"details\" ?? (@ == \"details\")$$)"
--
-- >>> display (Subject "" "abc" [ArrayWildcard "",FieldKey "xyz"])
-- buildQuery for ArrayWildcard should be unreachable
--
-- >>> display (Subject "" "request_body" [FieldKey "message", ArrayWildcard "tags", FieldKey "name"])
-- buildQuery for ArrayWildcard should be unreachable
--
-- >>> display (Subject "" "abc" [ArrayWildcard "",FieldKey "xyz"])
-- buildQuery for ArrayWildcard should be unreachable
--
-- >>> display (Subject "" "request_body" [FieldKey "message", ArrayWildcard "tags", FieldKey "name"])
-- buildQuery for ArrayWildcard should be unreachable
--
-- >>> display (Regex (Subject "" "request_body" [FieldKey "msg"]) "^abc.*")
-- "jsonb_path_exists(request_body, '$.\"msg\" ?? (@ like_regex \"^abc.*\")')"
instance Display Expr where
  displayPrec prec expr@(Eq sub val) = displayExprHelper "=" prec sub val
  displayPrec prec (NotEq sub val) = displayExprHelper "!=" prec sub val
  displayPrec prec (GT sub val) = displayExprHelper ">" prec sub val
  displayPrec prec (LT sub val) = displayExprHelper "<" prec sub val
  displayPrec prec (GTEq sub val) = displayExprHelper ">=" prec sub val
  displayPrec prec (LTEq sub val) = displayExprHelper "<=" prec sub val
  displayPrec prec (Paren u1) = displayParen True $ displayPrec prec u1
  displayPrec prec (And u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " AND " <> displayBuilder u2
  displayPrec prec (Or u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " OR " <> displayBuilder u2
  displayPrec prec (Regex sub val) = displayPrec prec $ jsonPathQuery "like_regex" sub (Str val)


-- Helper function to handle the common display logic
displayExprHelper :: Text -> Int -> Subject -> Values -> Builder
displayExprHelper op prec sub val =
  displayParen (prec > 0) $
    if subjectHasWildcard sub
      then displayPrec prec (jsonPathQuery op sub val)
      else displayPrec prec sub <> displayPrec @Text prec op <> displayBuilder val


-- | Generate PostgreSQL JSONPath queries from AST with specified operator
--
-- Examples:
--
-- >>> jsonPathQuery "==" (Subject "" "data" [FieldKey "name"]) (Str "John Doe")
-- "jsonb_path_exists(data, $$$.\"name\" ? (@ == \"John Doe\")$$)"
--
-- >>> jsonPathQuery "!=" (Subject "" "users" [ArrayIndex "" 1, FieldKey "age"]) (Num "30")
-- "jsonb_path_exists(users, $$$[1].\"age\" ? (@ != 30)$$)"
--
-- >>> jsonPathQuery "!=" (Subject "" "settings" [ArrayWildcard "", FieldKey "enabled"]) (Boolean True)
-- "jsonb_path_exists(settings, $$$[*].\"enabled\" ? (@ != True)$$)"
--
-- >>> jsonPathQuery "<" (Subject "" "user" [FieldKey "profile", FieldKey "address", FieldKey "zipcode"]) Null
-- "jsonb_path_exists(user, $$$.\"profile\".\"address\".\"zipcode\" ? (@ < null)$$)"
--
-- >>> jsonPathQuery ">" (Subject "" "orders" [ArrayIndex "" 0, ArrayWildcard "val", FieldKey "status"]) (Str "pending")
-- "jsonb_path_exists(orders, $$$[0].val[*].\"status\" ? (@ > \"pending\")$$)"
jsonPathQuery :: Text -> Subject -> Values -> Text
jsonPathQuery op' (Subject entire base keys) val =
  "jsonb_path_exists(" <> base <> ", $$" <> "$" <> buildPath keys <> buildCondition op val <> "$$)"
  where
    op = if op' == "=" then "==" else "="

    buildPath :: [FieldKey] -> Text
    buildPath [] = ""
    buildPath (FieldKey key : rest) = ".\"" <> key <> "\"" <> buildPath rest
    buildPath (ArrayIndex "" idx : rest) = "[" <> show idx <> "]" <> buildPath rest
    buildPath (ArrayIndex key idx : rest) = "." <> key <> "[" <> show idx <> "]" <> buildPath rest
    buildPath (ArrayWildcard "" : rest) = "[*]" <> buildPath rest
    buildPath (ArrayWildcard key : rest) = "." <> key <> "[*]" <> buildPath rest

    buildCondition :: Text -> Values -> Text
    buildCondition oper (Num n) = " ? (@ " <> oper <> " " <> n <> ")"
    buildCondition oper (Str s) = " ? (@ " <> oper <> " \"" <> s <> "\")"
    buildCondition oper (Boolean b) = " ? (@ " <> oper <> " " <> show b <> ")"
    buildCondition oper Null = " ? (@ " <> oper <> " null)"
    buildCondition oper (List xs) = " ? (@ " <> oper <> " {" <> (mconcat . intersperse "," . map display) xs <> "} )"


instance Display AggFunction where
  displayPrec prec (Count sub alias) = displayBuilder $ "count(" <> display sub <> ")"
  displayPrec prec (Sum sub alias) = displayBuilder $ "sum(" <> display sub <> ")"
  displayPrec prec (Avg sub alias) = displayBuilder $ "avg(" <> display sub <> ")"
  displayPrec prec (Min sub alias) = displayBuilder $ "min(" <> display sub <> ")"
  displayPrec prec (Max sub alias) = displayBuilder $ "max(" <> display sub <> ")"
  displayPrec prec (Median sub alias) = displayBuilder $ "median(" <> display sub <> ")"
  displayPrec prec (Stdev sub alias) = displayBuilder $ "stdev(" <> display sub <> ")"
  displayPrec prec (Range sub alias) = displayBuilder $ "range(" <> display sub <> ")"
  displayPrec prec (Plain sub alias) = displayBuilder $ "" <> display sub <> ""
