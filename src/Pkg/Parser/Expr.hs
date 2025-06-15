module Pkg.Parser.Expr (pSubject, pExpr, Subject (..), Values (..), Expr (..), kqlTimespanToTimeBucket) where

import Control.Monad.Combinators.Expr (
  Operator (InfixL),
  makeExprParser,
 )
import Data.Aeson qualified as AE
import Data.Char (isDigit)
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific)
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder)
import Data.Text.Display (Display, display, displayBuilder, displayParen, displayPrec)
import Data.Vector qualified as V
import Pkg.Parser.Core
import Relude hiding (GT, LT, Sum, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, space, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L


-- Values is an enum of the list of supported value types.
-- Num is a text  that represents a float as float covers ints in a lot of cases. But its basically the json num type.
-- Duration stores original unit and nanoseconds value for precise time comparisons
-- TimeFunction stores KQL time functions like now()
-- AgoExpression represents KQL ago() function with the value and unit for SQL interval conversion
data Values
  = Num Text
  | Str Text
  | Boolean Bool
  | Null
  | List [Values]
  | Duration Text Integer
  | TimeFunction Text
  | AgoExpression Text -- The original KQL timespan expression for direct conversion to PostgreSQL interval
  | NowExpression -- Represents now() function
  deriving stock (Eq, Generic, Ord, Show)


instance AE.FromJSON Values where
  parseJSON (AE.Number n) = return $ Num (toText (formatScientific Fixed Nothing n))
  parseJSON (AE.String s) = return $ Str s
  parseJSON (AE.Bool b) = return $ Boolean b
  parseJSON AE.Null = return Null
  parseJSON (AE.Array arr) = List <$> traverse AE.parseJSON (V.toList arr)
  parseJSON (AE.Object _) = fail "Cannot parse Object into Values"


instance AE.ToJSON Values where
  toJSON (Num t) = case readMaybe (toString t) :: Maybe Scientific of
    Just n -> AE.Number n
    Nothing -> error $ "Invalid number: " <> show t
  toJSON (Str s) = AE.String s
  toJSON (Boolean b) = AE.Bool b
  toJSON Null = AE.Null
  toJSON (List xs) = AE.Array (V.fromList (map AE.toJSON xs))
  toJSON (Duration _ ns) = AE.Number (fromInteger ns)
  toJSON (TimeFunction tf) = AE.String tf
  toJSON (AgoExpression expr) = AE.String ("ago(" <> expr <> ")")
  toJSON NowExpression = AE.String "now()"


instance ToQueryText Values where
  toQText v = decodeUtf8 $ AE.encode v


-- A subject consists of the primary key, and then the list of fields keys which are delimited by a .
-- To support jsonpath, we will have more powerfule field keys, so instead of a text array, we could have an enum field key type?
data Subject = Subject Text Text [FieldKey]
  deriving stock (Eq, Generic, Ord, Show)


-- Custom ToJSON which lets us stick to the jsonpath representation of subjects, when rendering subject to json
instance AE.ToJSON Subject where
  toJSON (Subject a _ _) = AE.String a


-- Custom FromJSON which lets decodes the jsonpath representation into a subject by parsing it
instance AE.FromJSON Subject where
  parseJSON = AE.withText "Subject" \text ->
    case parse pSubject "" text of
      Left err -> fail $ "Parse error: " ++ errorBundlePretty err
      Right subject -> pure subject


instance ToQueryText Subject where
  toQText (Subject a _ _) = a


data FieldKey = FieldKey Text | ArrayIndex Text Int | ArrayWildcard Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


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


data Expr
  = Eq Subject Values
  | NotEq Subject Values
  | GT Subject Values
  | LT Subject Values
  | GTEq Subject Values
  | LTEq Subject Values
  | Regex Subject Text
  | In Subject Values
  | NotIn Subject Values
  | Has Subject Values
  | NotHas Subject Values
  | HasAny Subject Values
  | HasAll Subject Values
  | Contains Subject Values
  | NotContains Subject Values
  | StartsWith Subject Values
  | NotStartsWith Subject Values
  | EndsWith Subject Values
  | NotEndsWith Subject Values
  | Matches Subject Text
  | Paren Expr
  | And Expr Expr
  | Or Expr Expr
  --  | Not Expr
  --  | JSONPathExpr Expr
  --  | FunctionCall String [Expr] -- For functions like ANY
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Example queries
-- request_body.v1.v2 = "abc" AND (request_body.v3.v4 = 123 OR request_body.v5[].v6=ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |

-- >>> request_body="jfdshkjfds" | stat field1, field2 by field3

-- >>> parse pFieldKey "" "key.abc[1]"
-- Right (FieldKey "key")
--
-- >>> parse pFieldKey "" "abc[1]"
-- Right (ArrayIndex "abc" 1)
--
-- >>> parse pFieldKey "" "abc[*]"
-- Right (ArrayWildcard "abc")
--
-- >>> parse pFieldKey "" "ab___b___c.a"
-- Right (FieldKey "ab___b___c")
--
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
pPrimaryKey :: Parser (T.Text, Maybe FieldKey)
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
pSquareBracketKey :: T.Text -> Parser FieldKey
pSquareBracketKey key = sqParens (arrayWildcard <|> arrayIndex)
  where
    arrayWildcard = ArrayWildcard key <$ char '*'
    arrayIndex = ArrayIndex key <$> L.decimal


sqParens :: Parser a -> Parser a
sqParens = between (symbol "[") (symbol "]")


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


-- | Parse duration values and convert to nanoseconds
--
-- Examples:
--
-- >>> parse pDuration "" "100ms"
-- Right (Duration "ms" 100000000)
--
-- >>> parse pDuration "" "4.3ms"
-- Right (Duration "ms" 4300000)
--
-- >>> parse pDuration "" "5s"
-- Right (Duration "s" 5000000000)
--
-- >>> parse pDuration "" "2.5m"
-- Right (Duration "m" 150000000000)
--
-- >>> parse pDuration "" "1h"
-- Right (Duration "h" 3600000000000)
pDuration :: Parser Values
pDuration = do
  value <- try L.float <|> (fromIntegral <$> L.decimal)
  unit <- (string "ns" <|> string "us" <|> string "µs" <|> string "ms" <|> string "s" <|> string "m" <|> string "h")
  let multiplier = case unit of
        "ns" -> 1
        "us" -> 1000
        "µs" -> 1000
        "ms" -> 1000000
        "s" -> 1000000000
        "m" -> 60000000000
        "h" -> 3600000000000
        _ -> error "Invalid duration unit"
  return $ Duration unit (round (value * multiplier))


-- | Parse the now() function
--
-- >>> parse pNowFunction "" "now()"
-- Right NowExpression
pNowFunction :: Parser Values
pNowFunction = NowExpression <$ string "now()"


-- | Parse the ago() function with various time units (d, h, m, s, ms, us, ns)
--
-- >>> parse pAgoFunction "" "ago(7d)"
-- Right (AgoExpression "7d")
--
-- >>> parse pAgoFunction "" "ago(12h)"
-- Right (AgoExpression "12h")
--
-- >>> parse pAgoFunction "" "ago(30m)"
-- Right (AgoExpression "30m")
--
-- >>> parse pAgoFunction "" "ago(45s)"
-- Right (AgoExpression "45s")
--
-- >>> parse pAgoFunction "" "ago(500ms)"
-- Right (AgoExpression "500ms")
--
-- >>> parse pAgoFunction "" "ago(1.5h)"
-- Right (AgoExpression "1.5h")
--
-- >>> parse pAgoFunction "" "ago(1d2h30m)"
-- Right (AgoExpression "1d2h30m")
pAgoFunction :: Parser Values
pAgoFunction = do
  _ <- string "ago("
  timespan <- some (alphaNumChar <|> char '.')
  _ <- string ")"
  return $ AgoExpression (toText timespan)


-- | parse values into our internal AST representation. Int, Str, Num, Bool, List, etc
--
-- Examples:
--
-- >>> parse pValues "" "[1,2,3]"
-- Right (List [Num "1",Num "2",Num "3"])
--
-- >>> parse pValues "" "[true,false]"
-- Right (List [Boolean True,Boolean False])
--
-- >>> parse pValues "" "[\"as\",1,2]"
-- Right (List [Str "as",Num "1",Num "2"])
--
-- >>> parse pValues "" "[\"as\",\"b\"]"
-- Right (List [Str "as",Str "b"])
--
-- >>> parse pValues "" "[]"
-- Right (List [])
--
-- Test parenthesized lists:
-- >>> parse pValues "" "(\"success\", \"error\")"
-- Right (List [Str "success",Str "error"])
--
-- >>> parse pValues "" "()"
-- Right (List [])
--
-- >>> parse pValues "" "(\"GET\", \"POST\", \"PUT\")"
-- Right (List [Str "GET",Str "POST",Str "PUT"])
--
-- >>> parse pValues "" "now()"
-- Right NowExpression
--
-- >>> parse pValues "" "ago(7d)"
-- Right (AgoExpression "7d")
pValues :: Parser Values
pValues =
  choice @[]
    [ Null <$ string "null"
    , Boolean <$> (True <$ string "true" <|> False <$ string "false" <|> False <$ string "FALSE" <|> True <$ string "TRUE")
    , Str . toText <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
    , List [] <$ string "[]"
    , List <$> sqParens (pValues `sepBy` (space *> char ',' <* space))
    , List [] <$ string "()"
    , List <$> parens (pValues `sepBy` (space *> char ',' <* space))
    , try pNowFunction
    , try pAgoFunction
    , try pDuration
    , try (Num . toText . show <$> L.float)
    , Num . toText . show <$> L.decimal
    , Str . toText <$> manyTill L.charLiteral space1
    ]


-- | pTerm is the main entry point that desides what tree lines to decend
--
-- Examples:
--
-- >>> parseTest pTerm "abc != \"GET\""
--
-- Test new 'in' and '!in' operators:
-- >>> parse pTerm "" "status in (\"success\", \"error\")"
-- Right (In (Subject "status" "status" []) (List [Str "success",Str "error"]))
--
-- >>> parse pTerm "" "method !in (\"GET\", \"POST\")"
-- Right (NotIn (Subject "method" "method" []) (List [Str "GET",Str "POST"]))
--
-- Test text search operators:
-- >>> parse pTerm "" "message has \"error\""
-- Right (Has (Subject "message" "message" []) (Str "error"))
--
-- >>> parse pTerm "" "message !has \"success\""
-- Right (NotHas (Subject "message" "message" []) (Str "success"))
--
-- >>> parse pTerm "" "tags has_any [\"urgent\", \"critical\"]"
-- Right (HasAny (Subject "tags" "tags" []) (List [Str "urgent",Str "critical"]))
--
-- >>> parse pTerm "" "description has_all [\"user\", \"login\"]"
-- Right (HasAll (Subject "description" "description" []) (List [Str "user",Str "login"]))
--
-- >>> parse pTerm "" "url contains \"api\""
-- Right (Contains (Subject "url" "url" []) (Str "api"))
--
-- >>> parse pTerm "" "path !contains \"admin\""
-- Right (NotContains (Subject "path" "path" []) (Str "admin"))
--
-- >>> parse pTerm "" "endpoint startswith \"/api/\""
-- Right (StartsWith (Subject "endpoint" "endpoint" []) (Str "/api/"))
--
-- >>> parse pTerm "" "path !startswith \"/internal\""
-- Right (NotStartsWith (Subject "path" "path" []) (Str "/internal"))
--
-- >>> parse pTerm "" "filename endswith \".log\""
-- Right (EndsWith (Subject "filename" "filename" []) (Str ".log"))
--
-- >>> parse pTerm "" "url !endswith \".css\""
-- Right (NotEndsWith (Subject "url" "url" []) (Str ".css"))
--
-- >>> parse pTerm "" "email matches /.*@company\\.com/"
-- Right (Matches (Subject "email" "email" []) ".*@company\\.com")
pTerm :: Parser Expr
pTerm =
  (Paren <$> parens pExpr)
    <|> try (Eq <$> pSubject <* space <* void (symbol "==") <* space <*> pValues)
    <|> try (NotEq <$> pSubject <* space <* void (symbol "!=") <* space <*> pValues)
    <|> try (GTEq <$> pSubject <* space <* void (symbol ">=") <* space <*> pValues)
    <|> try (LTEq <$> pSubject <* space <* void (symbol "<=") <* space <*> pValues)
    <|> try (GT <$> pSubject <* space <* void (symbol ">") <* space <*> pValues)
    <|> try (LT <$> pSubject <* space <* void (symbol "<") <* space <*> pValues)
    <|> try (NotIn <$> pSubject <* space <* void (symbol "!in") <* space <*> pValues)
    <|> try (In <$> pSubject <* space <* void (symbol "in") <* space <*> pValues)
    <|> try (NotHas <$> pSubject <* space <* void (symbol "!has") <* space <*> pValues)
    <|> try (HasAll <$> pSubject <* space <* void (symbol "has_all") <* space <*> pValues)
    <|> try (HasAny <$> pSubject <* space <* void (symbol "has_any") <* space <*> pValues)
    <|> try (Has <$> pSubject <* space <* void (symbol "has") <* space <*> pValues)
    <|> try (NotContains <$> pSubject <* space <* void (symbol "!contains") <* space <*> pValues)
    <|> try (Contains <$> pSubject <* space <* void (symbol "contains") <* space <*> pValues)
    <|> try (NotStartsWith <$> pSubject <* space <* void (symbol "!startswith") <* space <*> pValues)
    <|> try (StartsWith <$> pSubject <* space <* void (symbol "startswith") <* space <*> pValues)
    <|> try (NotEndsWith <$> pSubject <* space <* void (symbol "!endswith") <* space <*> pValues)
    <|> try (EndsWith <$> pSubject <* space <* void (symbol "endswith") <* space <*> pValues)
    <|> try (Matches <$> pSubject <* space <* void (symbol "matches") <* space <*> (toText <$> (char '/' *> manyTill L.charLiteral (char '/'))))
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


binary :: T.Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
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


-- List of OpenTelemetry attribute paths that have been flattened in the database schema
flattenedOtelAttributes :: [T.Text]
flattenedOtelAttributes =
  [ "attributes.http.request.method"
  , "attributes.http.request.method_original"
  , "attributes.http.response.status_code"
  , "attributes.http.request.resend_count"
  , "attributes.http.request.body.size"
  , "attributes.url.fragment"
  , "attributes.url.full"
  , "attributes.url.path"
  , "attributes.url.query"
  , "attributes.url.scheme"
  , "attributes.user_agent.original"
  , "attributes.db.system.name"
  , "attributes.db.collection.name"
  , "attributes.db.namespace"
  , "attributes.db.operation.name"
  , "attributes.db.operation.batch.size"
  , "attributes.db.query.summary"
  , "attributes.db.query.text"
  , "context.trace_id"
  , "context.span_id"
  , "context.trace_state"
  , "context.trace_flags"
  , "context.is_remote"
  , "resource.service.name"
  , "resource.service.version"
  , "resource.service.instance.id"
  , "resource.service.namespace"
  , "resource.telemetry.sdk.language"
  , "resource.telemetry.sdk.name"
  , "resource.telemetry.sdk.version"
  ]


-- Transform dot notation to triple-underscore notation for flattened attributes
transformFlattenedAttribute :: T.Text -> T.Text
transformFlattenedAttribute entire
  | entire `elem` flattenedOtelAttributes = T.replace "." "___" entire
  | otherwise = entire


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
  displayPrec prec (Subject entire x keys) =
    if entire `elem` flattenedOtelAttributes
      then displayPrec prec (transformFlattenedAttribute entire)
      else case keys of
        [] -> displayPrec prec x
        (y : ys) -> displayPrec prec $ buildQuerySequence x (y : ys)
    where
      buildQuerySequence :: T.Text -> [FieldKey] -> T.Text
      buildQuerySequence acc [] = acc
      buildQuerySequence acc [lastKey] = buildQuery acc lastKey True
      buildQuerySequence acc (key : rest) = buildQuerySequence (buildQuery acc key False) rest

      buildQuery :: T.Text -> FieldKey -> Bool -> T.Text
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

-- | Convert a KQL timespan to PostgreSQL interval syntax
--
-- Example conversion:
-- 7d -> INTERVAL '7 days'
-- 12h -> INTERVAL '12 hours'
-- 30m -> INTERVAL '30 minutes'
-- 45s -> INTERVAL '45 seconds'
-- 500ms -> INTERVAL '500 milliseconds'
kqlTimespanToInterval :: Text -> Text
kqlTimespanToInterval timespan =
  let
    parseTimeUnit :: Text -> (Text, Text)
    parseTimeUnit input =
      let (digits, rest) = T.span (\c -> isDigit c || c == '.') input
       in case rest of
            rest' | T.isPrefixOf "d" rest' -> (digits <> " days", T.drop 1 rest')
            rest' | T.isPrefixOf "h" rest' -> (digits <> " hours", T.drop 1 rest')
            rest' | T.isPrefixOf "m" rest' -> (digits <> " minutes", T.drop 1 rest')
            rest' | T.isPrefixOf "s" rest' -> (digits <> " seconds", T.drop 1 rest')
            rest' | T.isPrefixOf "ms" rest' -> (digits <> " milliseconds", T.drop 2 rest')
            rest' | T.isPrefixOf "us" rest' -> (digits <> " microseconds", T.drop 2 rest')
            rest' | T.isPrefixOf "ns" rest' -> (digits <> " nanoseconds", T.drop 2 rest')
            _ -> ("", input) -- No recognized unit or parsing error
    parseComplexTimespan :: Text -> Text
    parseComplexTimespan ts =
      case parseTimeUnit ts of
        ("", _) -> "" -- Parsing error or no recognized unit
        (unit, "") -> unit -- End of input
        (unit, rest) -> unit <> " " <> parseComplexTimespan rest

    intervalExpr =
      case T.strip (parseComplexTimespan timespan) of
        "" -> "0" -- Default if parsing fails
        expr -> expr
   in
    "INTERVAL '" <> intervalExpr <> "'"


-- | Convert KQL timespan to PostgreSQL time bucket string
-- This is used for bin() function in summarize queries
kqlTimespanToTimeBucket :: Text -> Text
kqlTimespanToTimeBucket timespan =
  case T.strip timespan of
    ts | T.isSuffixOf "w" ts -> T.dropEnd 1 ts <> " weeks"
    ts | T.isSuffixOf "d" ts -> T.dropEnd 1 ts <> " days"
    ts | T.isSuffixOf "h" ts -> T.dropEnd 1 ts <> " hours"
    ts | T.isSuffixOf "m" ts -> T.dropEnd 1 ts <> " minutes"
    ts | T.isSuffixOf "s" ts -> T.dropEnd 1 ts <> " seconds"
    ts | T.isSuffixOf "ms" ts -> T.dropEnd 2 ts <> " milliseconds"
    ts | T.isSuffixOf "µs" ts -> T.dropEnd 2 ts <> " microseconds"
    ts | T.isSuffixOf "us" ts -> T.dropEnd 2 ts <> " microseconds"
    ts | T.isSuffixOf "ns" ts -> T.dropEnd 2 ts <> " nanoseconds"
    _ -> "5 minutes" -- Default to 5 minutes if parsing fails


instance Display Values where
  displayPrec prec (Num a) = displayPrec prec a
  displayPrec prec (Str a) = displayPrec prec $ "'" <> a <> "'"
  displayPrec prec (Boolean True) = "true"
  displayPrec prec (Boolean False) = "false"
  displayPrec prec Null = "null"
  displayPrec prec (Duration _ ns) = displayPrec prec (show ns)
  displayPrec prec NowExpression = displayBuilder "NOW()"
  displayPrec prec (AgoExpression timespan) = displayBuilder $ "NOW() - " <> kqlTimespanToInterval timespan
  displayPrec prec (TimeFunction tf) = displayBuilder tf
  displayPrec prec (List vs) =
    let arrayElements = mconcat . intersperse "," . map (displayPrec prec) $ vs
     in "ARRAY[" <> arrayElements <> "]"


-- | Render the expr ast to a value. Start with Eq only, for supporting jsonpath
--
-- >>> display (Eq (Subject "" "request_body" [FieldKey "message"]) (Str "val"))
-- "request_body->>'message'='val'"
--
-- >>> display (Eq (Subject "" "errors" [ArrayIndex "" 0, FieldKey "message"]) (Str "val"))
-- "errors->0->>'message'='val'"
--
-- >>> display (Eq (Subject "" "abc" [ArrayWildcard "",FieldKey "xyz"]) (Str "val"))
-- "jsonb_path_exists(abc, $$$[*].\"xyz\" ? (@ == \"val\")$$::jsonpath)"
--
-- >>> display (Eq (Subject "" "errors" [ArrayWildcard "", ArrayIndex "message" 0, FieldKey "details"]) (Str "detailsVal"))
-- "jsonb_path_exists(errors, $$$[*].message[0].\"details\" ? (@ == \"detailsVal\")$$::jsonpath)"
--
-- -- abc[*].xyz which should generate something else than what is generated.
-- -- TODO: investigate and then FIXME
-- >>> display (Subject "" "abc" [ArrayWildcard "",FieldKey "xyz"])
-- "abc->''->>'xyz'"
--
-- -- buildQuery for ArrayWildcard should be unreachable
-- -- TODO: investigate and then FIXME
-- >>> display (Subject "" "request_body" [FieldKey "message", ArrayWildcard "tags", FieldKey "name"])
-- "request_body->'message'->'tags'->>'name'"
--
-- -- buildQuery for ArrayWildcard should be unreachable
-- -- TODO: investigate and then FIXME
-- >>> display (Subject "" "abc" [ArrayWildcard "",FieldKey "xyz"])
-- "abc->''->>'xyz'"
--
-- -- buildQuery for ArrayWildcard should be unreachable
-- -- TODO: investigate and then FIXME
-- >>> display (Subject "" "request_body" [FieldKey "message", ArrayWildcard "tags", FieldKey "name"])
-- "request_body->'message'->'tags'->>'name'"
--
-- >>> display (Regex (Subject "" "request_body" [FieldKey "msg"]) "^abc.*")
-- "jsonb_path_exists(request_body, $$$.\"msg\" ? (@ = \"^abc.*\")$$::jsonpath)"
--
-- Test new operators Display instances for SQL generation:
--
-- >>> display (In (Subject "" "status" []) (List [Str "success", Str "error"]))
-- "status IN ('success','error')"
--
-- >>> display (NotIn (Subject "" "method" []) (List [Str "GET", Str "POST"]))
-- "method NOT IN ('GET','POST')"
--
-- >>> display (Has (Subject "" "message" []) (Str "error"))
-- "message ~* 'error'"
--
-- >>> display (NotHas (Subject "" "message" []) (Str "success"))
-- "message !~* 'success'"
--
-- >>> display (Contains (Subject "" "url" []) (Str "api"))
-- "url ILIKE '%' || 'api' || '%'"
--
-- >>> display (NotContains (Subject "" "path" []) (Str "admin"))
-- "path NOT ILIKE '%' || 'admin' || '%'"
--
-- >>> display (StartsWith (Subject "" "endpoint" []) (Str "/api/"))
-- "endpoint ILIKE '/api/' || '%'"
--
-- >>> display (NotStartsWith (Subject "" "path" []) (Str "/internal"))
-- "path NOT ILIKE '/internal' || '%'"
--
-- >>> display (EndsWith (Subject "" "filename" []) (Str ".log"))
-- "filename ILIKE '%' || '.log'"
--
-- >>> display (NotEndsWith (Subject "" "url" []) (Str ".css"))
-- "url NOT ILIKE '%' || '.css'"
--
-- >>> display (Matches (Subject "" "email" []) ".*@company\\.com")
-- "jsonb_path_exists(to_jsonb(email), $$$ ? (@ like_regex \".*@company\\.com\" flag \"i\" )$$::jsonpath)"
instance Display Expr where
  displayPrec prec expr@(Eq sub val) = displayExprHelper "=" prec sub val
  displayPrec prec (NotEq sub val) = displayExprHelper "!=" prec sub val
  displayPrec prec (GT sub val) = displayExprHelper ">" prec sub val
  displayPrec prec (LT sub val) = displayExprHelper "<" prec sub val
  displayPrec prec (GTEq sub val) = displayExprHelper ">=" prec sub val
  displayPrec prec (LTEq sub val) = displayExprHelper "<=" prec sub val
  displayPrec prec (In sub val) = displayExprHelper "IN" prec sub val
  displayPrec prec (NotIn sub val) = displayExprHelper "NOT IN" prec sub val
  displayPrec prec (Has sub val) = displayExprHelper "HAS" prec sub val
  displayPrec prec (NotHas sub val) = displayExprHelper "NOT HAS" prec sub val
  displayPrec prec (HasAny sub val) = displayExprHelper "HAS_ANY" prec sub val
  displayPrec prec (HasAll sub val) = displayExprHelper "HAS_ALL" prec sub val
  displayPrec prec (Contains sub val) = displayExprHelper "CONTAINS" prec sub val
  displayPrec prec (NotContains sub val) = displayExprHelper "NOT CONTAINS" prec sub val
  displayPrec prec (StartsWith sub val) = displayExprHelper "STARTSWITH" prec sub val
  displayPrec prec (NotStartsWith sub val) = displayExprHelper "NOT STARTSWITH" prec sub val
  displayPrec prec (EndsWith sub val) = displayExprHelper "ENDSWITH" prec sub val
  displayPrec prec (NotEndsWith sub val) = displayExprHelper "NOT ENDSWITH" prec sub val
  displayPrec prec (Matches sub val) = displayPrec prec $ jsonPathQuery "like_regex" sub (Str val)
  displayPrec prec (Paren u1) = displayParen True $ displayPrec prec u1
  displayPrec prec (And u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " AND " <> displayPrec prec u2
  displayPrec prec (Or u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " OR " <> displayPrec prec u2
  displayPrec prec (Regex sub val) = displayPrec prec $ jsonPathQuery "like_regex" sub (Str val)


-- To be used when generating the text query given an ast
instance ToQueryText Expr where
  toQText (Eq sub val) = toQText sub <> " == " <> toQText val
  toQText (NotEq sub val) = toQText sub <> " != " <> toQText val
  toQText (GT sub val) = toQText sub <> " > " <> toQText val
  toQText (LT sub val) = toQText sub <> " < " <> toQText val
  toQText (GTEq sub val) = toQText sub <> " >= " <> toQText val
  toQText (LTEq sub val) = toQText sub <> " <= " <> toQText val
  toQText (In sub val) = toQText sub <> " in " <> toQText val
  toQText (NotIn sub val) = toQText sub <> " !in " <> toQText val
  toQText (Has sub val) = toQText sub <> " has " <> toQText val
  toQText (NotHas sub val) = toQText sub <> " !has " <> toQText val
  toQText (HasAny sub val) = toQText sub <> " has_any " <> toQText val
  toQText (HasAll sub val) = toQText sub <> " has_all " <> toQText val
  toQText (Contains sub val) = toQText sub <> " contains " <> toQText val
  toQText (NotContains sub val) = toQText sub <> " !contains " <> toQText val
  toQText (StartsWith sub val) = toQText sub <> " startswith " <> toQText val
  toQText (NotStartsWith sub val) = toQText sub <> " !startswith " <> toQText val
  toQText (EndsWith sub val) = toQText sub <> " endswith " <> toQText val
  toQText (NotEndsWith sub val) = toQText sub <> " !endswith " <> toQText val
  toQText (Matches sub val) = toQText sub <> " matches /" <> val <> "/"
  toQText (Paren expr) = "(" <> toQText expr <> ")"
  toQText (And left right) = toQText left <> " AND " <> toQText right
  toQText (Or left right) = toQText left <> " OR " <> toQText right
  toQText (Regex sub val) = toQText sub <> " =~ " <> toQText (Str val)


-- Helper function to handle the common display logic
displayExprHelper :: T.Text -> Int -> Subject -> Values -> Builder
displayExprHelper op prec sub val =
  displayParen (prec > 0)
    $ if subjectHasWildcard sub
      then displayPrec prec (jsonPathQuery op sub val)
      else case (op, val) of
        ("=", Null) -> displayPrec prec sub <> " IS NULL"
        ("!=", Null) -> displayPrec prec sub <> " IS NOT NULL"
        ("IN", List vs) -> displayPrec prec sub <> " IN (" <> (mconcat . intersperse "," . map (displayPrec prec)) vs <> ")"
        ("NOT IN", List vs) -> displayPrec prec sub <> " NOT IN (" <> (mconcat . intersperse "," . map (displayPrec prec)) vs <> ")"
        ("HAS", _) -> displayPrec prec sub <> " ~* " <> displayPrec prec val
        ("NOT HAS", _) -> displayPrec prec sub <> " !~* " <> displayPrec prec val
        ("HAS_ANY", List vs) -> "(" <> (mconcat . intersperse " OR " . map (\v -> displayPrec prec sub <> " ~* " <> displayPrec prec v)) vs <> ")"
        ("HAS_ALL", List vs) -> "(" <> (mconcat . intersperse " AND " . map (\v -> displayPrec prec sub <> " ~* " <> displayPrec prec v)) vs <> ")"
        ("CONTAINS", _) -> displayPrec prec sub <> " ILIKE '%' || " <> displayPrec prec val <> " || '%'"
        ("NOT CONTAINS", _) -> displayPrec prec sub <> " NOT ILIKE '%' || " <> displayPrec prec val <> " || '%'"
        ("STARTSWITH", _) -> displayPrec prec sub <> " ILIKE " <> displayPrec prec val <> " || '%'"
        ("NOT STARTSWITH", _) -> displayPrec prec sub <> " NOT ILIKE " <> displayPrec prec val <> " || '%'"
        ("ENDSWITH", _) -> displayPrec prec sub <> " ILIKE '%' || " <> displayPrec prec val
        ("NOT ENDSWITH", _) -> displayPrec prec sub <> " NOT ILIKE '%' || " <> displayPrec prec val
        _ -> displayPrec prec sub <> " " <> displayPrec @T.Text prec op <> " " <> displayPrec prec val


-- | Generate PostgreSQL JSONPath queries from AST with specified operator
--
-- Examples:
--
-- >>> jsonPathQuery "==" (Subject "" "data" [FieldKey "name"]) (Str "John Doe")
-- "jsonb_path_exists(data, $$$.\"name\" ? (@ == \"John Doe\")$$::jsonpath)"
--
-- >>> jsonPathQuery "!=" (Subject "" "users" [ArrayIndex "" 1, FieldKey "age"]) (Num "30")
-- "jsonb_path_exists(users, $$$[1].\"age\" ? (@ != 30)$$::jsonpath)"
--
-- >>> jsonPathQuery "!=" (Subject "" "settings" [ArrayWildcard "", FieldKey "enabled"]) (Boolean True)
-- "jsonb_path_exists(settings, $$$[*].\"enabled\" ? (@ != true)$$::jsonpath)"
--
-- >>> jsonPathQuery "<" (Subject "" "user" [FieldKey "profile", FieldKey "address", FieldKey "zipcode"]) Null
-- "jsonb_path_exists(user, $$$.\"profile\".\"address\".\"zipcode\" ? (@ < null)$$::jsonpath)"
--
-- >>> jsonPathQuery ">" (Subject "" "orders" [ArrayIndex "" 0, ArrayWildcard "val", FieldKey "status"]) (Str "pending")
-- "jsonb_path_exists(orders, $$$[0].val[*].\"status\" ? (@ > \"pending\")$$::jsonpath)"
--
-- >>> jsonPathQuery "like_regex" (Subject "" "request_body" [FieldKey "msg"]) (Str "^abc.*")
-- "jsonb_path_exists(request_body, $$$.\"msg\" ? (@ like_regex \"^abc.*\" flag \"i\" )$$::jsonpath)"
--
-- Test new operators with JSONPath (wildcard subjects):
--
-- >>> jsonPathQuery "IN" (Subject "" "users" [ArrayWildcard "", FieldKey "role"]) (List [Str "admin", Str "user"])
-- "jsonb_path_exists(to_jsonb(users), $$$[*].\"role\" ? (@ IN {\"admin\",\"user\"})$$::jsonpath)"
--
-- >>> jsonPathQuery "HAS" (Subject "" "logs" [ArrayWildcard "", FieldKey "message"]) (Str "error")
-- "jsonb_path_exists(to_jsonb(logs), $$$[*].\"message\" ? (@ HAS \"error\")$$::jsonpath)"
--
-- >>> jsonPathQuery "CONTAINS" (Subject "" "events" [ArrayWildcard "", FieldKey "description"]) (Str "login")
-- "jsonb_path_exists(to_jsonb(events), $$$[*].\"description\" ? (@ CONTAINS \"login\")$$::jsonpath)"
--
-- >>> jsonPathQuery "STARTSWITH" (Subject "" "requests" [ArrayWildcard "", FieldKey "path"]) (Str "/api")
-- "jsonb_path_exists(to_jsonb(requests), $$$[*].\"path\" ? (@ STARTSWITH \"/api\")$$::jsonpath)"
jsonPathQuery :: T.Text -> Subject -> Values -> T.Text
jsonPathQuery op' (Subject entire base keys) val =
  "jsonb_path_exists(to_jsonb(" <> base <> "), $$" <> "$" <> buildPath keys <> buildCondition op val postfix <> "$$::jsonpath)"
  where
    op = if op' == "=" then "==" else op'
    postfix = if op' == "like_regex" then " flag \"i\" " else ""

    buildPath :: [FieldKey] -> T.Text
    buildPath [] = ""
    buildPath (FieldKey key : rest) = ".\"" <> key <> "\"" <> buildPath rest
    buildPath (ArrayIndex "" idx : rest) = "[" <> show idx <> "]" <> buildPath rest
    buildPath (ArrayIndex key idx : rest) = "." <> key <> "[" <> show idx <> "]" <> buildPath rest
    buildPath (ArrayWildcard "" : rest) = "[*]" <> buildPath rest
    buildPath (ArrayWildcard key : rest) = "." <> key <> "[*]" <> buildPath rest

    buildCondition :: T.Text -> Values -> T.Text -> T.Text
    buildCondition oper (Num n) pstfx = " ? (@ " <> oper <> " " <> n <> postfix <> ")"
    buildCondition oper (Str s) pstfx = " ? (@ " <> oper <> " \"" <> s <> "\"" <> postfix <> ")"
    buildCondition oper (Boolean b) pstfx = " ? (@ " <> oper <> " " <> T.toLower (show b) <> pstfx <> ")"
    buildCondition oper (Duration _ ns) pstfx = " ? (@ " <> oper <> " " <> show ns <> postfix <> ")"
    buildCondition oper NowExpression pstfx = " ? (@ " <> oper <> " now()" <> pstfx <> ")"
    buildCondition oper (AgoExpression timespan) pstfx = " ? (@ " <> oper <> " ago(" <> timespan <> ")" <> pstfx <> ")"
    buildCondition oper (TimeFunction tf) pstfx = " ? (@ " <> oper <> " " <> tf <> pstfx <> ")"
    buildCondition oper Null pstfx =
      case op' of
        "=" -> " ? (@ is null" <> pstfx <> ")"
        "!=" -> " ? (@ is not null" <> pstfx <> ")"
        _ -> " ? (@ " <> oper <> " null" <> pstfx <> ")"
    buildCondition oper (List xs) pstfx = " ? (@ " <> oper <> " [" <> (mconcat . intersperse "," . map display) xs <> "]" <> pstfx <> ")"
