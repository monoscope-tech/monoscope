module Pkg.Parser.Expr (pSubject, pExpr, Subject (..), Values (..), Expr (..), kqlTimespanToTimeBucket, FieldKey (..), pSquareBracketKey, pTerm, jsonPathQuery, display, pDuration, pNowFunction, pAgoFunction, pValues, Parser, symbol, sc, ToQueryText (..), flattenedOtelAttributes, flattenedOtelAttributesBuiltin, setFlattenedOtelColumns, topLevelOtelColumns, transformFlattenedAttribute, outputFieldAliases) where

import Control.Monad.Combinators.Expr (
  Operator (InfixL),
  makeExprParser,
 )
import Data.Aeson qualified as AE
import Data.Char (isDigit)
import Data.Map.Strict qualified as M
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder)
import Data.Text.Display (Display, display, displayBuilder, displayParen, displayPrec)
import Data.Vector qualified as V
import Relude hiding (GT, LT, Sum, many, some)
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, space, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L


type Parser = Parsec Void Text


class ToQueryText a where
  toQText :: a -> Text


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")


symbol :: Text -> Parser Text
symbol = L.symbol sc


-- $setup
-- >>> import Text.Megaparsec (parse, parseTest)
-- >>> import Data.Text.Display (display)
-- >>> import Pkg.Parser.Expr (Values(..), Subject(..), FieldKey(..), jsonPathQuery)
-- >>> import Prelude (Bool(..))
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes


-- Values is an enum of the list of supported value types.
-- Num is a text  that represents a float as float covers ints in a lot of cases. But its basically the json num type.
-- Duration stores original unit and nanoseconds value for precise time comparisons
-- TimeFunction stores KQL time functions like now()
-- AgoExpression represents KQL ago() function with the value and unit for SQL interval conversion
-- Field represents a field reference (column name) - displayed unquoted in SQL
-- ScalarFunc represents KQL scalar functions: coalesce, iff, isnull, etc.
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
  | Field Subject -- Field reference - displayed as column name, not quoted string
  | ScalarFunc Text [Values] -- Scalar function: name, arguments (coalesce, iff, isnull, toint, etc.)
  deriving stock (Eq, Generic, Ord, Show)


instance AE.FromJSON Values where
  parseJSON (AE.Number n) = return $ Num (toText (formatScientific Fixed Nothing n))
  parseJSON (AE.String s) = return $ Str s
  parseJSON (AE.Bool b) = return $ Boolean b
  parseJSON AE.Null = return Null
  parseJSON (AE.Array arr) = List <$> traverse AE.parseJSON (V.toList arr)
  parseJSON (AE.Object obj) = ScalarFunc <$> obj AE..: "func" <*> obj AE..: "args"


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
  toJSON (Field sub) = AE.toJSON sub
  toJSON (ScalarFunc name args) = AE.object ["func" AE..= name, "args" AE..= args]


instance ToQueryText Values where
  toQText (ScalarFunc name args) = name <> "(" <> T.intercalate ", " (map toQText args) <> ")"
  toQText (Field sub) = toQText sub
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
  | ValEq Values Values -- Value-to-value comparison (e.g., "" == "")
  | ValNotEq Values Values -- Value-to-value not-equal (e.g., "" != "x")
  | ValGT Values Values -- Value-to-value greater than (e.g., coalesce(x,0) > 100)
  | ValLT Values Values -- Value-to-value less than
  | ValGTEq Values Values -- Value-to-value greater than or equal
  | ValLTEq Values Values -- Value-to-value less than or equal
  | BoolFunc Values -- Boolean scalar function as standalone expression (isnull, isnotnull, isempty, isnotempty)
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
  unit <- string "ns" <|> string "us" <|> string "µs" <|> string "ms" <|> string "s" <|> string "m" <|> string "h"
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


-- | KQL scalar function names (longer names first to avoid prefix matching)
scalarFuncNames :: [Text]
scalarFuncNames = ["isnotnull", "isnotempty", "isnull", "isempty", "coalesce", "strcat", "iff", "iif", "todouble", "tofloat", "tolong", "toint", "tostring", "tobool"]


-- | Boolean scalar functions that can be used as standalone expressions in filters
boolScalarFuncNames :: [Text]
boolScalarFuncNames = ["isnotnull", "isnotempty", "isnull", "isempty"]


-- | Parse boolean scalar function as standalone expression (isnull(x), isnotnull(x), etc.)
pBoolScalarFunc :: Parser Values
pBoolScalarFunc = do
  name <- asum $ map (\n -> n <$ string n) boolScalarFuncNames
  args <- parens (pScalarArg `sepBy` (space *> char ',' <* space))
  pure $ ScalarFunc name args


-- | Parse scalar function argument: nested function, field reference, or literal
pScalarArg :: Parser Values
pScalarArg = try pScalarFunc <|> try (Field <$> pSubject) <|> pValuesNoFunc


-- | Parse KQL scalar functions: coalesce(a,b), iff(cond,t,f), isnull(x), toint(x), etc.
pScalarFunc :: Parser Values
pScalarFunc = do
  name <- asum $ map (\n -> n <$ string n) scalarFuncNames
  args <- parens (pScalarArg `sepBy` (space *> char ',' <* space))
  pure $ ScalarFunc (if name == "iif" then "iff" else name) args


-- | pValuesNoFunc: pValues without scalar function parsing (avoids left recursion)
pValuesNoFunc :: Parser Values
pValuesNoFunc = pValuesWith pValuesNoFunc []


-- | Shared body of pValues / pValuesNoFunc. @self@ ties the recursion for
-- nested list elements (so the no-func variant stays no-func inside lists);
-- @extra@ prepends the leading scalar-func alternative for pValues.
pValuesWith :: Parser Values -> [Parser Values] -> Parser Values
pValuesWith self extra =
  choice @[]
    $ extra
    <> [ Null <$ string "null"
       , Boolean <$> (True <$ string "true" <|> False <$ string "false" <|> False <$ string "FALSE" <|> True <$ string "TRUE")
       , Str . toText <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
       , Str . toText <$> (char '\'' *> manyTill L.charLiteral (char '\''))
       , List [] <$ string "[]"
       , List <$> sqParens (self `sepBy` (space *> char ',' <* space))
       , List [] <$ string "()"
       , List <$> parens (self `sepBy` (space *> char ',' <* space))
       , try pNowFunction
       , try pAgoFunction
       , try pDuration
       , try (Num . toText . show <$> L.signed pass L.float)
       , Num . toText . show <$> L.signed pass L.decimal
       ]


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
--
-- Test single-quoted strings:
-- >>> parse pValues "" "'hello'"
-- Right (Str "hello")
--
-- >>> parse pValues "" "['SELECT','INSERT']"
-- Right (List [Str "SELECT",Str "INSERT"])
pValues :: Parser Values
pValues = pValuesWith pValues [try pScalarFunc] -- try pScalarFunc must come first to handle coalesce(), iff(), etc.


-- | pTerm is the main entry point that desides what tree lines to decend
--
-- Examples:
--
-- >>> parseTest pTerm "abc != \"GET\""
-- NotEq (Subject "abc" "abc" []) (Str "GET")
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
-- Test has_any and has_all with single-quoted strings:
-- >>> parse pTerm "" "attributes.db.operation.name has_any ['SELECT','INSERT']"
-- Right (HasAny (Subject "attributes.db.operation.name" "attributes" [FieldKey "db",FieldKey "operation",FieldKey "name"]) (List [Str "SELECT",Str "INSERT"]))
--
-- >>> parse pTerm "" "tags has_all ['urgent','critical']"
-- Right (HasAll (Subject "tags" "tags" []) (List [Str "urgent",Str "critical"]))
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
-- >>> parse pTerm "" "email matches /.*@company\\\\.com/"
-- Right (Matches (Subject "email" "email" []) ".*@company\\.com")
--
-- Standalone subject is treated as isnotempty check (KQL spec):
-- >>> parse pTerm "" "x.y.z"
-- Right (BoolFunc (ScalarFunc "isnotempty" [Field (Subject "x.y.z" "x" [FieldKey "y",FieldKey "z"])]))
pTerm :: Parser Expr
pTerm =
  (Paren <$> parens pExpr)
    <|> asum [binTerm pValues ctor sym | (ctor, sym, _, _) <- valBinOps]
    <|> asum [binTerm pSubject ctor sym | (ctor, sym, _, _) <- subjectBinOps]
    <|> try (Matches <$> pSubject <* space <* void (symbol "matches") <* space <*> (toText <$> (char '/' *> manyTill L.charLiteral (char '/'))))
    <|> try regexParser
    <|> try (BoolFunc <$> pBoolScalarFunc) -- Standalone boolean functions: isnull(x), isnotnull(x), etc.
    <|> (BoolFunc . ScalarFunc "isnotempty" . pure . Field <$> pSubject) -- Standalone subject = isnotempty check per KQL spec


-- | One try-based binary alternative: @lhs OP pValues@.
binTerm :: Parser a -> (a -> Values -> Expr) -> Text -> Parser Expr
binTerm pLhs ctor sym = try (ctor <$> pLhs <* space <* void (symbol sym) <* space <*> pValues)


-- | The shared binary-operator tables, each row carrying:
-- (constructor, KQL parse symbol, ToQueryText infix token, Display op-string).
-- Ordering is significant for 'pTerm' (try-based, longest token first, e.g.
-- @>=@ before @>@, @!in@ before @in@). The same rows drive 'ToQueryText Expr'
-- and 'Display Expr' so the constructor->token map lives in one place.
valBinOps :: [(Values -> Values -> Expr, Text, Text, Text)]
valBinOps =
  [ (ValEq, "==", " == ", "=")
  , (ValNotEq, "!=", " != ", "!=")
  , (ValGTEq, ">=", " >= ", ">=")
  , (ValLTEq, "<=", " <= ", "<=")
  , (ValGT, ">", " > ", ">")
  , (ValLT, "<", " < ", "<")
  ]


subjectBinOps :: [(Subject -> Values -> Expr, Text, Text, Text)]
subjectBinOps =
  [ (Eq, "==", " == ", "=")
  , (NotEq, "!=", " != ", "!=")
  , (GTEq, ">=", " >= ", ">=")
  , (LTEq, "<=", " <= ", "<=")
  , (GT, ">", " > ", ">")
  , (LT, "<", " < ", "<")
  , (NotIn, "!in", " !in ", "NOT IN")
  , (In, "in", " in ", "IN")
  , (NotHas, "!has", " !has ", "NOT HAS")
  , (HasAll, "has_all", " has_all ", "HAS_ALL")
  , (HasAny, "has_any", " has_any ", "HAS_ANY")
  , (Has, "has", " has ", "HAS")
  , (NotContains, "!contains", " !contains ", "NOT CONTAINS")
  , (Contains, "contains", " contains ", "CONTAINS")
  , (NotStartsWith, "!startswith", " !startswith ", "NOT STARTSWITH")
  , (StartsWith, "startswith", " startswith ", "STARTSWITH")
  , (NotEndsWith, "!endswith", " !endswith ", "NOT ENDSWITH")
  , (EndsWith, "endswith", " endswith ", "ENDSWITH")
  ]


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


-- | Hand-coded fallback for the flattened OTel attribute set. Used at
-- bootstrap and by any code that runs before 'setFlattenedOtelColumns'
-- (unit tests, scripts). The runtime set is read via
-- 'flattenedOtelAttributes' which prefers whatever was populated from the
-- live introspection of @otel_logs_and_spans@.
flattenedOtelAttributesBuiltin :: Set T.Text
flattenedOtelAttributesBuiltin =
  fromList
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
    , "attributes.session.id"
    , "attributes.user.id"
    , "attributes.user.email"
    , "attributes.user.name"
    , "attributes.user.full_name"
    , "attributes.exception.type"
    , "attributes.exception.message"
    , "attributes.exception.stacktrace"
    , "attributes.exception.escaped"
    , "severity.severity_text"
    , "severity.severity_number"
    ]


-- | Cache for the introspected column set. Initialised to the hand-coded
-- fallback so anything that runs before the bootstrap (e.g. unit tests, the
-- KQL parser invoked from a script) still sees a sensible value.
--
-- Set by 'setFlattenedOtelColumns' at server startup. After that it is
-- effectively immutable for the process lifetime — the parser's hot path
-- ('transformFlattenedAttribute' below) reads from it via
-- 'flattenedOtelAttributes', which inlines an 'unsafePerformIO' read.
-- The 'unsafePerformIO' is safe because writes happen exactly once at boot
-- before any query handler runs.
{-# NOINLINE flattenedOtelColumnsRef #-}
flattenedOtelColumnsRef :: IORef (Set T.Text)
flattenedOtelColumnsRef = unsafePerformIO (newIORef flattenedOtelAttributesBuiltin)


-- | Replace the cached flattened-attribute set. Called from 'Start.hs' after
-- the introspection query against @information_schema.columns@ resolves.
-- The new set is unioned with the hand-coded fallback so unknown
-- attributes (a missing live introspection, a schema-learning gap) still
-- behave correctly.
setFlattenedOtelColumns :: Set T.Text -> IO ()
setFlattenedOtelColumns live = writeIORef flattenedOtelColumnsRef (live <> flattenedOtelAttributesBuiltin)


-- | The runtime flattened-attribute set. Reads from the bootstrap-once
-- cache. Stays a pure value so existing call sites (which are themselves
-- pure: 'transformFlattenedAttribute', 'Display Subject' codegen) don't need
-- to change.
flattenedOtelAttributes :: Set T.Text
flattenedOtelAttributes = unsafePerformIO (readIORef flattenedOtelColumnsRef)
{-# NOINLINE flattenedOtelAttributes #-}


-- | Bare top-level columns on @otel_logs_and_spans@. KQL accepts these
-- without translation (no @___@); listed here so the facet doctest
-- ('prop_facetsAreFast') can gate them as fast-filter columns too.
topLevelOtelColumns :: Set T.Text
topLevelOtelColumns = fromList ["level", "name", "kind", "status_code", "status_message"]


-- | Map user-facing output field names (SELECT aliases) to their real DB column names.
-- These aliases appear in query results and the schema endpoint, so users naturally
-- try to filter by them — without this map the WHERE clause would reference a
-- non-existent column and return a 400.
--
-- This map is applied unconditionally by 'Display Subject', covering both WHERE
-- predicates (the intended use case) and SELECT/EXTEND expressions. In an EXTEND
-- context the rewrite is still correct: @| extend x = span_name@ emits
-- @name as x@, referencing the right column. The only edge case is
-- @| summarize … by span_name@ which would GROUP BY @name@ and surface the
-- result column as @name@ rather than @span_name@; that is an acceptable
-- trade-off given how rarely aggregations target these alias fields.
--
-- >>> outputFieldAliases M.! "span_name"
-- "name"
-- >>> outputFieldAliases M.! "service"
-- "resource___service___name"
outputFieldAliases :: M.Map T.Text T.Text
outputFieldAliases =
  M.fromList
    [ ("span_name", "name")
    , ("service", "resource___service___name")
    , ("trace_id", "context___trace_id")
    ]


-- Transform dot notation to triple-underscore notation for flattened attributes
transformFlattenedAttribute :: T.Text -> T.Text
-- Exception fields can live on the flattened attributes___exception___* column
-- or as a span event (event_name="exception"); OTel SDKs (e.g. hs-opentelemetry)
-- take the latter, so we COALESCE both sources.
transformFlattenedAttribute entire
  | entire `S.member` flattenedOtelAttributes
  , Just field <- T.stripPrefix "attributes.exception." entire =
      "COALESCE(attributes___exception___"
        <> field
        <> ", (jsonb_path_query_first(events, '$[*] ? (@.event_name == \"exception\").event_attributes.exception."
        <> field
        <> "') #>> '{}'))"
  | entire `S.member` flattenedOtelAttributes = T.replace "." "___" entire
  | entire == "url_path" = "attributes___url___path"
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
    case M.lookup entire outputFieldAliases of
      Just col -> displayPrec prec col
      Nothing
        | entire `S.member` flattenedOtelAttributes -> displayPrec prec (transformFlattenedAttribute entire)
        | otherwise -> case keys of
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

-- | Maximum allowed timespan in days (1 year) to prevent DoS
maxTimespanDays :: Double
maxTimespanDays = 365


-- | Convert a KQL timespan to PostgreSQL interval syntax with bounds checking.
-- Timespans exceeding 1 year are capped to prevent DoS attacks.
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
    parseTimeUnit :: Text -> (Text, Text, Double)
    parseTimeUnit input =
      let (digits, rest) = T.span (\c -> isDigit c || c == '.') input
          num = fromMaybe 0 $ readMaybe @Double (toString digits)
       in case rest of
            rest' | T.isPrefixOf "d" rest' -> (digits <> " days", T.drop 1 rest', num)
            rest' | T.isPrefixOf "h" rest' -> (digits <> " hours", T.drop 1 rest', num / 24)
            rest' | T.isPrefixOf "m" rest' -> (digits <> " minutes", T.drop 1 rest', num / 1440)
            rest' | T.isPrefixOf "s" rest' -> (digits <> " seconds", T.drop 1 rest', num / 86400)
            rest' | T.isPrefixOf "ms" rest' -> (digits <> " milliseconds", T.drop 2 rest', num / 86400000)
            rest' | T.isPrefixOf "us" rest' -> (digits <> " microseconds", T.drop 2 rest', num / 86400000000)
            rest' | T.isPrefixOf "ns" rest' -> (digits <> " nanoseconds", T.drop 2 rest', num / 86400000000000)
            _ -> ("", input, 0)
    parseComplexTimespan :: Text -> (Text, Double)
    parseComplexTimespan ts =
      case parseTimeUnit ts of
        ("", _, _) -> ("", 0)
        (unit, "", days) -> (unit, days)
        (unit, rest, days) -> let (restUnits, restDays) = parseComplexTimespan rest in (unit <> " " <> restUnits, days + restDays)

    (intervalExpr, totalDays) =
      case parseComplexTimespan timespan of
        ("", _) -> ("0", 0)
        (expr, days) -> (T.strip expr, days)
   in
    if totalDays > maxTimespanDays
      then "INTERVAL '365 days'"
      else "INTERVAL '" <> intervalExpr <> "'"


-- | Convert KQL timespan to PostgreSQL time bucket string
-- This is used for bin() function in summarize queries
-- Handles both KQL short format (30s) and PostgreSQL format (30 seconds)
-- SECURITY: Never passes through user input - always returns validated/reconstructed strings
kqlTimespanToTimeBucket :: Text -> Text
kqlTimespanToTimeBucket timespan = fromMaybe "5 minutes" $ parsePostgresInterval ts <|> parseKqlFormat ts
  where
    ts = T.strip timespan
    validUnits = S.fromList ["second", "seconds", "minute", "minutes", "hour", "hours", "day", "days", "week", "weeks", "millisecond", "milliseconds", "microsecond", "microseconds", "nanosecond", "nanoseconds"]
    -- Parse and reconstruct PostgreSQL interval format (returns validated string, not original input)
    parsePostgresInterval t = case words t of
      [num, unit] | Just n <- readMaybe @Int (toString num), unit `S.member` validUnits -> Just $ show n <> " " <> unit
      _ -> Nothing
    -- Parse KQL short format (e.g., "30s", "5m", "1h")
    parseKqlFormat t
      | T.isSuffixOf "ms" t, Just n <- readMaybe @Int (toString $ T.dropEnd 2 t) = Just $ show n <> " milliseconds"
      | T.isSuffixOf "µs" t, Just n <- readMaybe @Int (toString $ T.dropEnd 2 t) = Just $ show n <> " microseconds"
      | T.isSuffixOf "us" t, Just n <- readMaybe @Int (toString $ T.dropEnd 2 t) = Just $ show n <> " microseconds"
      | T.isSuffixOf "ns" t, Just n <- readMaybe @Int (toString $ T.dropEnd 2 t) = Just $ show n <> " nanoseconds"
      | T.isSuffixOf "w" t, Just n <- readMaybe @Int (toString $ T.dropEnd 1 t) = Just $ show n <> " weeks"
      | T.isSuffixOf "d" t, Just n <- readMaybe @Int (toString $ T.dropEnd 1 t) = Just $ show n <> " days"
      | T.isSuffixOf "h" t, Just n <- readMaybe @Int (toString $ T.dropEnd 1 t) = Just $ show n <> " hours"
      | T.isSuffixOf "m" t, Just n <- readMaybe @Int (toString $ T.dropEnd 1 t) = Just $ show n <> " minutes"
      | T.isSuffixOf "s" t, Just n <- readMaybe @Int (toString $ T.dropEnd 1 t) = Just $ show n <> " seconds"
      | otherwise = Nothing


instance Display Values where
  displayPrec prec (Num a) = displayPrec prec a
  displayPrec prec (Str a) = displayPrec prec $ "'" <> T.replace "'" "''" a <> "'"
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
  displayPrec prec (Field sub) = displayPrec prec sub
  displayPrec _ (ScalarFunc name args) = displayBuilder $ scalarFuncToSQL name args


-- | Type cast function name to SQL type mapping
typeCastMap :: Map Text Text
typeCastMap = M.fromList [("toint", "integer"), ("tolong", "bigint"), ("tostring", "text"), ("tofloat", "float"), ("todouble", "double precision"), ("tobool", "boolean")]


-- | Unary scalar functions with their SQL templates (use {} for argument placeholder)
unaryFuncSQL :: Map Text (Text -> Text)
unaryFuncSQL =
  M.fromList
    [ ("isnull", (<> " IS NULL"))
    , ("isnotnull", (<> " IS NOT NULL"))
    , ("isempty", \v -> "(" <> v <> " IS NULL OR " <> v <> " = '')")
    , ("isnotempty", \v -> "(" <> v <> " IS NOT NULL AND " <> v <> " != '')")
    ]


-- | Map scalar function to SQL (consolidates all function->SQL logic)
scalarFuncToSQL :: Text -> [Values] -> Text
scalarFuncToSQL "coalesce" args = "COALESCE(" <> T.intercalate ", " (map display args) <> ")"
scalarFuncToSQL "strcat" args = "CONCAT(" <> T.intercalate ", " (map display args) <> ")"
scalarFuncToSQL "iff" [c, t, f] = "CASE WHEN " <> display c <> " THEN " <> display t <> " ELSE " <> display f <> " END"
scalarFuncToSQL "iff" args = error $ "iff requires 3 arguments, got " <> show (length args)
scalarFuncToSQL name [v]
  | Just sqlType <- M.lookup name typeCastMap = "(" <> display v <> ")::" <> sqlType
  | Just toSQL <- M.lookup name unaryFuncSQL = toSQL (display v)
scalarFuncToSQL name args
  | name `M.member` unaryFuncSQL = error $ name <> " requires 1 argument, got " <> show (length args)
  | otherwise = T.toUpper name <> "(" <> T.intercalate ", " (map display args) <> ")"


-- | Render the expr ast to a value. Start with Eq only, for supporting jsonpath
--
-- >>> display (Eq (Subject "" "request_body" [FieldKey "message"]) (Str "val"))
-- "request_body->>'message' = 'val'"
--
-- >>> display (Eq (Subject "" "errors" [ArrayIndex "" 0, FieldKey "message"]) (Str "val"))
-- "errors->0->>'message' = 'val'"
--
-- >>> display (Eq (Subject "" "abc" [ArrayWildcard "",FieldKey "xyz"]) (Str "val"))
-- "jsonb_path_exists(to_jsonb(abc), $$$[*].\"xyz\" ? (@ == \"val\")$$::jsonpath)"
--
-- >>> display (Eq (Subject "" "errors" [ArrayWildcard "", ArrayIndex "message" 0, FieldKey "details"]) (Str "detailsVal"))
-- "jsonb_path_exists(to_jsonb(errors), $$$[*].message[0].\"details\" ? (@ == \"detailsVal\")$$::jsonpath)"
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
-- "jsonb_path_exists(to_jsonb(request_body), $$$.\"msg\" ? (@ like_regex \"^abc.*\" flag \"i\" )$$::jsonpath)"
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
-- "message::text ~* 'error'"
--
-- >>> display (NotHas (Subject "" "message" []) (Str "success"))
-- "message::text !~* 'success'"
--
-- >>> display (Contains (Subject "" "url" []) (Str "api"))
-- "url::text ILIKE '%' || 'api' || '%'"
--
-- >>> display (NotContains (Subject "" "path" []) (Str "admin"))
-- "path::text NOT ILIKE '%' || 'admin' || '%'"
--
-- >>> display (StartsWith (Subject "" "endpoint" []) (Str "/api/"))
-- "endpoint::text ILIKE '/api/' || '%'"
--
-- >>> display (NotStartsWith (Subject "" "path" []) (Str "/internal"))
-- "path::text NOT ILIKE '/internal' || '%'"
--
-- >>> display (EndsWith (Subject "" "filename" []) (Str ".log"))
-- "filename::text ILIKE '%' || '.log'"
--
-- >>> display (NotEndsWith (Subject "" "url" []) (Str ".css"))
-- "url::text NOT ILIKE '%' || '.css'"
--
-- >>> display (Matches (Subject "" "email" []) ".*@company\\.com")
-- "jsonb_path_exists(to_jsonb(email), $$$ ? (@ like_regex \".*@company\\.com\" flag \"i\" )$$::jsonpath)"

-- | Decompose a binary Expr into its operands plus the (display op-string,
-- toQText infix token) drawn from the shared operator tables, so Display and
-- ToQueryText reuse a single constructor->token map. The bespoke cases
-- (Matches/Paren/And/Or/Regex/BoolFunc) are handled directly in the instances.
subBinaryParts :: Expr -> Maybe (Subject, Values, Text, Text)
subBinaryParts e = do
  (s, v, sym) <- case e of
    Eq s v -> at s v "=="; NotEq s v -> at s v "!="; GTEq s v -> at s v ">="; LTEq s v -> at s v "<="
    GT s v -> at s v ">"; LT s v -> at s v "<"; NotIn s v -> at s v "!in"; In s v -> at s v "in"
    NotHas s v -> at s v "!has"; HasAll s v -> at s v "has_all"; HasAny s v -> at s v "has_any"; Has s v -> at s v "has"
    NotContains s v -> at s v "!contains"; Contains s v -> at s v "contains"; NotStartsWith s v -> at s v "!startswith"
    StartsWith s v -> at s v "startswith"; NotEndsWith s v -> at s v "!endswith"; EndsWith s v -> at s v "endswith"
    _ -> Nothing
  (_, _, qtok, dop) <- find (\(_, sym', _, _) -> sym' == sym) subjectBinOps
  pure (s, v, dop, qtok)
  where
    at s v sym = Just (s, v, sym :: Text)


valBinaryParts :: Expr -> Maybe (Values, Values, Text, Text)
valBinaryParts e = do
  (a, b, sym) <- case e of
    ValEq a b -> at a b "=="; ValNotEq a b -> at a b "!="; ValGTEq a b -> at a b ">="
    ValLTEq a b -> at a b "<="; ValGT a b -> at a b ">"; ValLT a b -> at a b "<"
    _ -> Nothing
  (_, _, qtok, dop) <- find (\(_, sym', _, _) -> sym' == sym) valBinOps
  pure (a, b, dop, qtok)
  where
    at a b sym = Just (a, b, sym :: Text)


instance Display Expr where
  displayPrec prec e
    | Just (sub, val, op, _) <- subBinaryParts e = displayExprHelper op prec sub val
    | Just (v1, v2, op, _) <- valBinaryParts e = displayParen (prec > 0) $ displayPrec prec v1 <> " " <> displayBuilder op <> " " <> displayPrec prec v2
  displayPrec prec (Matches sub val) = displayPrec prec $ jsonPathQuery "like_regex" sub (Str val)
  displayPrec prec (Paren u1) = displayParen True $ displayPrec prec u1
  displayPrec prec (And u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " AND " <> displayPrec prec u2
  displayPrec prec (Or u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " OR " <> displayPrec prec u2
  displayPrec prec (Regex sub val) = displayPrec prec $ jsonPathQuery "like_regex" sub (Str val)
  displayPrec prec (BoolFunc v) = displayPrec prec v -- Boolean scalar function renders directly to SQL
  displayPrec _ _ = error "Display Expr: unreachable"


-- To be used when generating the text query given an ast
instance ToQueryText Expr where
  toQText e
    | Just (sub, val, _, tok) <- subBinaryParts e = toQText sub <> tok <> toQText val
    | Just (v1, v2, _, tok) <- valBinaryParts e = toQText v1 <> tok <> toQText v2
  toQText (Matches sub val) = toQText sub <> " matches /" <> val <> "/"
  toQText (Paren expr) = "(" <> toQText expr <> ")"
  toQText (And left right) = toQText left <> " AND " <> toQText right
  toQText (Or left right) = toQText left <> " OR " <> toQText right
  toQText (Regex sub val) = toQText sub <> " =~ " <> toQText (Str val)
  toQText (BoolFunc v) = toQText v
  toQText _ = error "ToQueryText Expr: unreachable"


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
        ("HAS", _) -> subAsText <> " ~* " <> displayPrec prec val
        ("NOT HAS", _) -> subAsText <> " !~* " <> displayPrec prec val
        ("HAS_ANY", List vs) -> "(" <> (mconcat . intersperse " OR " . map (\v -> subAsText <> " ~* " <> displayPrec prec v)) vs <> ")"
        ("HAS_ALL", List vs) -> "(" <> (mconcat . intersperse " AND " . map (\v -> subAsText <> " ~* " <> displayPrec prec v)) vs <> ")"
        ("CONTAINS", _) -> subAsText <> " ILIKE '%' || " <> displayPrec prec val <> " || '%'"
        ("NOT CONTAINS", _) -> subAsText <> " NOT ILIKE '%' || " <> displayPrec prec val <> " || '%'"
        ("STARTSWITH", _) -> subAsText <> " ILIKE " <> displayPrec prec val <> " || '%'"
        ("NOT STARTSWITH", _) -> subAsText <> " NOT ILIKE " <> displayPrec prec val <> " || '%'"
        ("ENDSWITH", _) -> subAsText <> " ILIKE '%' || " <> displayPrec prec val
        ("NOT ENDSWITH", _) -> subAsText <> " NOT ILIKE '%' || " <> displayPrec prec val
        _ -> displayPrec prec sub <> " " <> displayPrec @T.Text prec op <> " " <> displayPrec prec val
  where
    -- Cast to text for subjects without field keys (may be JSONB columns)
    subAsText = case sub of
      Subject _ _ [] -> displayPrec prec sub <> "::text"
      _ -> displayPrec prec sub


-- | Generate PostgreSQL JSONPath queries from AST with specified operator
--
-- Examples:
--
-- >>> jsonPathQuery "==" (Subject "" "data" [FieldKey "name"]) (Str "John Doe")
-- "jsonb_path_exists(to_jsonb(data), $$$.\"name\" ? (@ == \"John Doe\")$$::jsonpath)"
--
-- >>> jsonPathQuery "!=" (Subject "" "users" [ArrayIndex "" 1, FieldKey "age"]) (Num "30")
-- "jsonb_path_exists(to_jsonb(users), $$$[1].\"age\" ? (@ != 30)$$::jsonpath)"
--
-- >>> jsonPathQuery "!=" (Subject "" "settings" [ArrayWildcard "", FieldKey "enabled"]) (Boolean True)
-- "jsonb_path_exists(to_jsonb(settings), $$$[*].\"enabled\" ? (@ != true)$$::jsonpath)"
--
-- >>> jsonPathQuery "<" (Subject "" "user" [FieldKey "profile", FieldKey "address", FieldKey "zipcode"]) Null
-- "jsonb_path_exists(to_jsonb(user), $$$.\"profile\".\"address\".\"zipcode\" ? (@ < null)$$::jsonpath)"
--
-- >>> jsonPathQuery ">" (Subject "" "orders" [ArrayIndex "" 0, ArrayWildcard "val", FieldKey "status"]) (Str "pending")
-- "jsonb_path_exists(to_jsonb(orders), $$$[0].val[*].\"status\" ? (@ > \"pending\")$$::jsonpath)"
--
-- >>> jsonPathQuery "like_regex" (Subject "" "request_body" [FieldKey "msg"]) (Str "^abc.*")
-- "jsonb_path_exists(to_jsonb(request_body), $$$.\"msg\" ? (@ like_regex \"^abc.*\" flag \"i\" )$$::jsonpath)"
--
-- Test new operators with JSONPath (wildcard subjects):
--
-- >>> jsonPathQuery "IN" (Subject "" "users" [ArrayWildcard "", FieldKey "role"]) (List [Str "admin", Str "user"])
-- "jsonb_path_exists(to_jsonb(users), $$$[*].\"role\" ? (@ IN ['admin','user'])$$::jsonpath)"
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
    buildCondition oper (Field sub) pstfx = " ? (@ " <> oper <> " " <> display sub <> pstfx <> ")"
    buildCondition oper (ScalarFunc name args) pstfx = " ? (@ " <> oper <> " " <> scalarFuncToSQL name args <> pstfx <> ")"
