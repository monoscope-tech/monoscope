module Pkg.Parser.Expr (pSubject, pExpr, Subject (..), Values (..), Expr (..), kqlTimespanToTimeBucket, unsupportedTimespan, defaultBinWidth, FieldKey (..), pSquareBracketKey, pTerm, Jsonpath, LowerErr (..), lowerPred, renderJsonpath, resolveWildcardTimes, display, pDuration, pNowFunction, pAgoFunction, pValues, Parser, symbol, sc, ToQueryText (..), flattenedOtelAttributes, flattenedOtelAttributesBuiltin, setOtelColumns, setMetricsColumns, topLevelOtelColumns, acceptedFieldRoots, FieldUniverse (..), otelFieldUniverse, metricsFieldUniverse, knownFieldRoot, suggestFieldRoot, transformFlattenedAttribute, outputFieldAliases, sqlStringLit) where

import Control.Monad.Combinators.Expr (
  Operator (InfixL),
  makeExprParser,
 )
import Data.Aeson qualified as AE
import Data.Aeson.Text (encodeToLazyText)
import Data.Char (isDigit)
import Data.List (lookup, partition)
import Data.Map.Strict qualified as M
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder)
import Data.Text.Display (Display, display, displayBuilder, displayParen, displayPrec)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Pkg.Deriving (escapeRegex)
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
-- >>> import Pkg.Parser.Expr (Values(..), Subject(..), FieldKey(..), lowerPred, renderJsonpath, LowerErr(..))
-- >>> import Prelude (Bool(..))
-- >>> import Data.Time (UTCTime(..))
-- >>> import Data.Time.Calendar (fromGregorian)
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes


-- | Supported value types. 'Num' is text holding a JSON-style number (float covers ints).
data Values
  = Num Text
  | Str Text
  | Boolean Bool
  | Null
  | List [Values]
  | Duration Text Integer -- Original unit + nanoseconds, for precise time comparisons
  | TimeFunction Text -- KQL time functions like now()
  | AgoExpression Text -- The original KQL timespan expression for direct conversion to PostgreSQL interval
  | NowExpression -- Represents now() function
  | Field Subject -- Field reference - displayed as column name, not quoted string
  | ScalarFunc Text [Values] -- Scalar function: name, arguments (coalesce, iff, isnull, toint, etc.)
  | TimestampLit Text -- now()/ago() resolved to a concrete ISO-8601 instant (see resolveWildcardTimes)
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
  toJSON (TimestampLit iso) = AE.String iso


instance ToQueryText Values where
  toQText (ScalarFunc name args) = name <> "(" <> T.intercalate ", " (map toQText args) <> ")"
  toQText (Field sub) = toQText sub
  toQText v = decodeUtf8 $ AE.encode v


-- A subject consists of the primary key, and then the list of field keys which are delimited by a .
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
  (unit, multiplier) <- asum [(u, m) <$ string u | (u, m) <- durationUnits]
  return $ Duration unit (round (value * multiplier))


-- | Accepted duration units paired with their nanosecond multipliers — the single source of
-- truth for 'pDuration'. Multi-char units precede their single-char prefixes ("ms" before
-- "s"/"m") so @string@ matches greedily. Driving the parser and the multiplier from the same
-- list means they can't drift apart into an unreachable @error@ fallthrough.
durationUnits :: [(Text, Double)]
durationUnits =
  [("ns", 1), ("us", 1000), ("µs", 1000), ("ms", 1000000), ("s", 1000000000), ("m", 60000000000), ("h", 3600000000000)]


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


-- | Parse a call to one of @names@ (tried in order, so longer names must come first).
-- @iif@ is normalised to @iff@.
pNamedFunc :: [Text] -> Parser Values
pNamedFunc names = do
  name <- asum [n <$ string n | n <- names]
  args <- parens (pScalarArg `sepBy` (space *> char ',' <* space))
  pure $ ScalarFunc (if name == "iif" then "iff" else name) args


-- | Parse boolean scalar function as standalone expression (isnull(x), isnotnull(x), etc.)
pBoolScalarFunc :: Parser Values
pBoolScalarFunc = pNamedFunc boolScalarFuncNames


-- | Parse scalar function argument: nested function, field reference, or literal
pScalarArg :: Parser Values
pScalarArg = try pScalarFunc <|> try (Field <$> pSubject) <|> pValuesNoFunc


-- | Parse KQL scalar functions: coalesce(a,b), iff(cond,t,f), isnull(x), toint(x), etc.
pScalarFunc :: Parser Values
pScalarFunc = pNamedFunc scalarFuncNames


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
    <|> asum [binTerm pValues ctor sym | (ctor, _, sym, _) <- valBinOps]
    <|> asum [binTerm pSubject ctor sym | (ctor, _, sym, _) <- subjectBinOps]
    <|> try (Matches <$> pSubject <* space <* void (symbol "matches") <* space <*> (toText <$> (char '/' *> manyTill L.charLiteral (char '/'))))
    <|> try regexParser
    <|> try (BoolFunc <$> pBoolScalarFunc) -- Standalone boolean functions: isnull(x), isnotnull(x), etc.
    <|> (BoolFunc . ScalarFunc "isnotempty" . pure . Field <$> pSubject) -- Standalone subject = isnotempty check per KQL spec


-- | One try-based binary alternative: @lhs OP pValues@.
binTerm :: Parser a -> (a -> Values -> Expr) -> Text -> Parser Expr
binTerm pLhs ctor sym = try (ctor <$> pLhs <* space <* void (symbol sym) <* space <*> pValues)


-- | The shared binary-operator tables, one row per binary 'Expr' constructor:
-- (constructor, matcher, KQL parse symbol, Display op-string). The ToQueryText
-- infix token is always the parse symbol surrounded by spaces, so it isn't stored.
-- The same rows drive 'pTerm', 'resolveWildcardTimes', 'Display Expr' and
-- 'ToQueryText Expr', so the constructor<->token map lives in one place.
-- Ordering is significant for 'pTerm' (try-based, longest token first, e.g.
-- @>=@ before @>@, @!in@ before @in@).
--
-- NOTE: because the tables are decoupled from the constructor definitions, adding
-- a new binary 'Expr' constructor compiles cleanly but crashes at render until you
-- add its row here — GHC's exhaustiveness check won't catch it.
valBinOps :: [(Values -> Values -> Expr, Expr -> Maybe (Values, Values), Text, Text)]
valBinOps =
  [ (ValEq, \case ValEq a b -> Just (a, b); _ -> Nothing, "==", "=")
  , (ValNotEq, \case ValNotEq a b -> Just (a, b); _ -> Nothing, "!=", "!=")
  , (ValGTEq, \case ValGTEq a b -> Just (a, b); _ -> Nothing, ">=", ">=")
  , (ValLTEq, \case ValLTEq a b -> Just (a, b); _ -> Nothing, "<=", "<=")
  , (ValGT, \case ValGT a b -> Just (a, b); _ -> Nothing, ">", ">")
  , (ValLT, \case ValLT a b -> Just (a, b); _ -> Nothing, "<", "<")
  ]


subjectBinOps :: [(Subject -> Values -> Expr, Expr -> Maybe (Subject, Values), Text, Text)]
subjectBinOps =
  [ (Eq, \case Eq s v -> Just (s, v); _ -> Nothing, "==", "=")
  , (NotEq, \case NotEq s v -> Just (s, v); _ -> Nothing, "!=", "!=")
  , (GTEq, \case GTEq s v -> Just (s, v); _ -> Nothing, ">=", ">=")
  , (LTEq, \case LTEq s v -> Just (s, v); _ -> Nothing, "<=", "<=")
  , (GT, \case GT s v -> Just (s, v); _ -> Nothing, ">", ">")
  , (LT, \case LT s v -> Just (s, v); _ -> Nothing, "<", "<")
  , (NotIn, \case NotIn s v -> Just (s, v); _ -> Nothing, "!in", "NOT IN")
  , (In, \case In s v -> Just (s, v); _ -> Nothing, "in", "IN")
  , (NotHas, \case NotHas s v -> Just (s, v); _ -> Nothing, "!has", "NOT HAS")
  , (HasAll, \case HasAll s v -> Just (s, v); _ -> Nothing, "has_all", "HAS_ALL")
  , (HasAny, \case HasAny s v -> Just (s, v); _ -> Nothing, "has_any", "HAS_ANY")
  , (Has, \case Has s v -> Just (s, v); _ -> Nothing, "has", "HAS")
  , (NotContains, \case NotContains s v -> Just (s, v); _ -> Nothing, "!contains", "NOT CONTAINS")
  , (Contains, \case Contains s v -> Just (s, v); _ -> Nothing, "contains", "CONTAINS")
  , (NotStartsWith, \case NotStartsWith s v -> Just (s, v); _ -> Nothing, "!startswith", "NOT STARTSWITH")
  , (StartsWith, \case StartsWith s v -> Just (s, v); _ -> Nothing, "startswith", "STARTSWITH")
  , (NotEndsWith, \case NotEndsWith s v -> Just (s, v); _ -> Nothing, "!endswith", "NOT ENDSWITH")
  , (EndsWith, \case EndsWith s v -> Just (s, v); _ -> Nothing, "endswith", "ENDSWITH")
  ]


-- >>> parse regexParser "" "abc=~/abc.*/"
-- Right (Regex (Subject "abc" "abc" []) "abc.*")
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


-- | Both the space-padded and the bare spelling of each keyword, padded first so it wins.
operatorTable :: [[Operator Parser Expr]]
operatorTable = [[InfixL (ctor <$ symbol sym) | (kw, ctor) <- [("AND", And), ("OR", Or), ("and", And), ("or", Or)], sym <- [" " <> kw <> " ", kw]]]


-------------------------------------------------------
--
-- SQL Where clause segment interpreter
--
-------------------------------------------------------

-- Helper function to detect if Subject contains an ArrayWildcard
subjectHasWildcard :: Subject -> Bool
subjectHasWildcard (Subject _ _ keys) = any (\case ArrayWildcard _ -> True; _ -> False) keys


-- | Hand-coded fallback for the flattened OTel attribute set. Used at
-- bootstrap and by any code that runs before 'setFlattenedOtelColumns'
-- (unit tests, the CLI, scripts). The runtime set is read via
-- 'flattenedOtelAttributes' which prefers whatever was populated from the
-- live introspection of @otel_logs_and_spans@.
--
-- This mirrors the @___@ columns the migrations create, so keep it in step when a
-- migration adds one: a name missing here is rejected wherever there is no database
-- to introspect (@monoscope validate@ turned away @attributes.error.type@ for exactly
-- that reason), and a name here that no column backs is worse — it passes validation
-- and then fails at the database, which is the failure validation exists to prevent.
flattenedOtelAttributesBuiltin :: Set T.Text
flattenedOtelAttributesBuiltin =
  fromList
    [ "attributes.client.address"
    , "attributes.client.port"
    , "attributes.code.file.path"
    , "attributes.code.function.name"
    , "attributes.code.line.number"
    , "attributes.code.number"
    , "attributes.code.stacktrace"
    , "attributes.db.collection.name"
    , "attributes.db.namespace"
    , "attributes.db.operation.batch.size"
    , "attributes.db.operation.name"
    , "attributes.db.query.summary"
    , "attributes.db.query.text"
    , "attributes.db.response.status_code"
    , "attributes.db.system.name"
    , "attributes.error.type"
    , "attributes.exception.message"
    , "attributes.exception.stacktrace"
    , "attributes.exception.type"
    , "attributes.http.request.body.size"
    , "attributes.http.request.method"
    , "attributes.http.request.method_original"
    , "attributes.http.request.resend_count"
    , "attributes.http.response.status_code"
    , "attributes.log__record.original"
    , "attributes.log__record.uid"
    , "attributes.network.local__address"
    , "attributes.network.local__port"
    , "attributes.network.peer.address"
    , "attributes.network.peer__port"
    , "attributes.network.protocol.name"
    , "attributes.network.protocol.version"
    , "attributes.network.transport"
    , "attributes.network.type"
    , "attributes.server.address"
    , "attributes.server.port"
    , "attributes.session.id"
    , "attributes.session.previous.id"
    , "attributes.url.fragment"
    , "attributes.url.full"
    , "attributes.url.path"
    , "attributes.url.query"
    , "attributes.url.scheme"
    , "attributes.user.email"
    , "attributes.user.full_name"
    , "attributes.user.hash"
    , "attributes.user.id"
    , "attributes.user.name"
    , "attributes.user_agent.original"
    , "context.is_remote"
    , "context.span_id"
    , "context.trace_flags"
    , "context.trace_id"
    , "context.trace_state"
    , "resource.deployment.environment.name"
    , "resource.service.instance.id"
    , "resource.service.name"
    , "resource.service.namespace"
    , "resource.service.version"
    , "resource.telemetry.sdk.language"
    , "resource.telemetry.sdk.name"
    , "resource.telemetry.sdk.version"
    , "resource.user_agent.original"
    , "severity.severity_number"
    , "severity.severity_text"
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


-- | Seed both column caches from the live @information_schema.columns@ read
-- at startup: @___@ columns become the dotted flattened-attribute set, the
-- rest the bare-column set. Both are unioned with their hand-coded fallbacks
-- so a missing/partial introspection still behaves.
setOtelColumns :: [T.Text] -> IO ()
setOtelColumns = seedColumns flattenedOtelColumnsRef flattenedOtelAttributesBuiltin bareOtelColumnsRef bareOtelColumnsBuiltin


-- | 'setOtelColumns' for @otel_metrics@.
setMetricsColumns :: [T.Text] -> IO ()
setMetricsColumns = seedColumns flattenedMetricsColumnsRef flattenedMetricsAttributesBuiltin bareMetricsColumnsRef bareMetricsColumnsBuiltin


seedColumns :: IORef (Set T.Text) -> Set T.Text -> IORef (Set T.Text) -> Set T.Text -> [T.Text] -> IO ()
seedColumns flatRef flatBuiltin bareRef bareBuiltin cols = do
  writeIORef flatRef (fromList [T.replace "___" "." c | c <- flattened] <> flatBuiltin)
  writeIORef bareRef (fromList bare <> bareBuiltin)
  where
    (flattened, bare) = partition (T.isInfixOf "___") cols


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


-- | Hand-coded fallback for the bare (dot-free) column set — the same role
-- 'flattenedOtelAttributesBuiltin' plays for the @___@ columns. Used before
-- 'setOtelColumns' runs, and unioned with the live set after.
bareOtelColumnsBuiltin :: Set T.Text
bareOtelColumnsBuiltin =
  topLevelOtelColumns
    <> fromList
      [ "timestamp"
      , "observed_timestamp"
      , "id"
      , "parent_id"
      , "hashes"
      , "severity"
      , "body"
      , "duration"
      , "start_time"
      , "end_time"
      , "context"
      , "events"
      , "links"
      , "attributes"
      , "resource"
      , "summary"
      , "errors"
      , "message_size_bytes"
      , "processed_at"
      , "date"
      , "project_id"
      ]


{-# NOINLINE bareOtelColumnsRef #-}
bareOtelColumnsRef :: IORef (Set T.Text)
bareOtelColumnsRef = unsafePerformIO (newIORef bareOtelColumnsBuiltin)


bareOtelColumns :: Set T.Text
bareOtelColumns = unsafePerformIO (readIORef bareOtelColumnsRef)
{-# NOINLINE bareOtelColumns #-}


-- | Bare columns on @otel_metrics@ — the fallback until 'setMetricsColumns'
-- reads the live schema. Nothing here overlaps the spans-only signal columns
-- (@level@, @status_code@, @duration@, @name@, @kind@): a metrics query naming
-- one is a real mistake and must be caught before it reaches TimeFusion.
bareMetricsColumnsBuiltin :: Set T.Text
bareMetricsColumnsBuiltin =
  fromList
    [ "timestamp"
    , "start_timestamp"
    , "ingested_at"
    , "id"
    , "series_id"
    , "metric_name"
    , "metric_description"
    , "metric_unit"
    , "metric_type"
    , "aggregation_temporality"
    , "is_monotonic"
    , "flags"
    , "resource"
    , "resource_schema_url"
    , "scope_name"
    , "scope_version"
    , "scope_schema_url"
    , "attributes"
    , "dropped_attributes_count"
    , "exemplars"
    , "value"
    , "value_double"
    , "value_int"
    , "distribution_count"
    , "distribution_sum"
    , "distribution_min"
    , "distribution_max"
    , "hist_bucket_counts"
    , "hist_explicit_bounds"
    , "exp_hist_scale"
    , "exp_hist_zero_count"
    , "exp_hist_zero_threshold"
    , "exp_hist_pos_offset"
    , "exp_hist_pos_buckets"
    , "exp_hist_neg_offset"
    , "exp_hist_neg_buckets"
    , "summary_quantiles"
    , "summary_values"
    , "message_size_bytes"
    , "updated_at"
    , "deleted"
    , "project_id"
    , "date"
    ]


-- | Flattened @___@ columns on @otel_metrics@, dotted. A strict subset of the
-- spans set: metrics carry resource/RPC/DB/messaging dimensions but no span
-- context (no @context.trace_id@), which is why @trace_id@ must not validate.
flattenedMetricsAttributesBuiltin :: Set T.Text
flattenedMetricsAttributesBuiltin =
  fromList
    [ "attributes.db.operation.name"
    , "attributes.db.system.name"
    , "attributes.error.type"
    , "attributes.http.request.method"
    , "attributes.http.response.status_code"
    , "attributes.http.route"
    , "attributes.messaging.destination.name"
    , "attributes.messaging.operation"
    , "attributes.messaging.system"
    , "attributes.rpc.grpc.status_code"
    , "attributes.rpc.method"
    , "attributes.rpc.service"
    , "resource.cloud.availability.zone"
    , "resource.cloud.provider"
    , "resource.cloud.region"
    , "resource.container.name"
    , "resource.deployment.environment.name"
    , "resource.host.name"
    , "resource.k8s.cluster.name"
    , "resource.k8s.container.name"
    , "resource.k8s.namespace.name"
    , "resource.k8s.pod.name"
    , "resource.service.instance.id"
    , "resource.service.name"
    , "resource.service.namespace"
    , "resource.service.version"
    ]


{-# NOINLINE bareMetricsColumnsRef #-}
bareMetricsColumnsRef :: IORef (Set T.Text)
bareMetricsColumnsRef = unsafePerformIO (newIORef bareMetricsColumnsBuiltin)


bareMetricsColumns :: Set T.Text
bareMetricsColumns = unsafePerformIO (readIORef bareMetricsColumnsRef)
{-# NOINLINE bareMetricsColumns #-}


{-# NOINLINE flattenedMetricsColumnsRef #-}
flattenedMetricsColumnsRef :: IORef (Set T.Text)
flattenedMetricsColumnsRef = unsafePerformIO (newIORef flattenedMetricsAttributesBuiltin)


flattenedMetricsAttributes :: Set T.Text
flattenedMetricsAttributes = unsafePerformIO (readIORef flattenedMetricsColumnsRef)
{-# NOINLINE flattenedMetricsAttributes #-}


-- | The column universe a KQL subject is validated against. One per queryable
-- table: @otel_logs_and_spans@ and @otel_metrics@ share the KQL grammar but not
-- their columns, and validating a metrics query against the spans set is what
-- let @level == …@ reach TimeFusion as an unplannable @No field named level@.
data FieldUniverse = FieldUniverse
  { bareCols :: Set T.Text
  -- ^ Top-level columns, written as-is.
  , flatCols :: Set T.Text
  -- ^ Flattened @___@ columns, held in the dotted form users write.
  }


otelFieldUniverse :: FieldUniverse
otelFieldUniverse = FieldUniverse bareOtelColumns flattenedOtelAttributes


metricsFieldUniverse :: FieldUniverse
metricsFieldUniverse = FieldUniverse bareMetricsColumns flattenedMetricsAttributes


-- | Whether a real column name — bare or @___@-flattened — exists in a universe.
resolvesIn :: FieldUniverse -> T.Text -> Bool
resolvesIn u col = col `S.member` u.bareCols || T.replace "___" "." col `S.member` u.flatCols


-- | Names a KQL subject may start with: a real bare column, a SELECT alias,
-- or the @url_path@ shim 'transformFlattenedAttribute' rewrites. Also the set
-- @/api/v1/schema@ advertises, so the query editor validates against exactly
-- what the parser accepts.
--
-- An alias is only accepted where the column it expands to actually exists:
-- 'Display Subject' rewrites @trace_id@ to @context___trace_id@ unconditionally,
-- so on the metrics table the alias would name a column that isn't there.
acceptedFieldRoots :: FieldUniverse -> Set T.Text
acceptedFieldRoots u =
  u.bareCols
    <> M.keysSet (M.filter (resolvesIn u) outputFieldAliases)
    <> (if resolvesIn u "attributes___url___path" then one "url_path" else mempty)


-- | Whether a subject's root names something queryable. @*@/@""@ come from
-- @count(*)@/@count()@ and carry no field.
--
-- The raw @___@ column names are accepted alongside the dotted form users
-- normally write: they are what the table actually calls those columns, they
-- pass straight through to SQL, and saved queries do use them.
--
-- >>> map (knownFieldRoot otelFieldUniverse) ["attributes", "duration", "service", "url_path", "*"]
-- [True,True,True,True,True]
-- >>> map (knownFieldRoot otelFieldUniverse) ["context___trace_id", "resource___service___name"]
-- [True,True]
-- >>> map (knownFieldRoot otelFieldUniverse) ["attribute", "context___nosuch"]
-- [False,False]
--
-- The metrics universe accepts its own columns and rejects the spans-only ones
-- that used to sail through unchecked:
--
-- >>> map (knownFieldRoot metricsFieldUniverse) ["metric_name", "value", "resource___service___name"]
-- [True,True,True]
-- >>> map (knownFieldRoot metricsFieldUniverse) ["level", "status_code", "duration", "trace_id"]
-- [False,False,False,False]
knownFieldRoot :: FieldUniverse -> T.Text -> Bool
knownFieldRoot u root =
  T.null root
    || root
    == "*"
    || root
    `S.member` acceptedFieldRoots u
    || T.replace "___" "." root
    `S.member` u.flatCols


-- | Nearest known field for a typo'd one. A prefix relation catches the
-- truncated/extended cases ("attribute" -> "attributes"); an edit distance of
-- at most two catches the mistyped ones ("attributs", "context___trace_ix"),
-- which the prefix rule alone silently gave up on. Still conservative: beyond
-- that, no suggestion beats a misleading one.
--
-- >>> suggestFieldRoot otelFieldUniverse "attribute"
-- Just "attributes"
-- >>> suggestFieldRoot otelFieldUniverse "attributs"
-- Just "attributes"
-- >>> suggestFieldRoot otelFieldUniverse "context___trace_ix"
-- Just "context___trace_id"
-- >>> suggestFieldRoot otelFieldUniverse "zzz"
-- Nothing
--
-- Suggestions come from the universe being validated, so a metrics typo is
-- answered with a metrics column:
--
-- >>> suggestFieldRoot metricsFieldUniverse "metric_nam"
-- Just "metric_name"
suggestFieldRoot :: FieldUniverse -> T.Text -> Maybe T.Text
suggestFieldRoot u root =
  viaNonEmpty head . map snd . sortWith fst $ [(d, c) | c <- candidates, Just d <- [rank c]]
  where
    -- Suggest in the notation the user is already writing, so a mistyped
    -- `context___trace_ix` is not answered with the dotted form.
    candidates
      | "___" `T.isInfixOf` root = map (T.replace "." "___") (toList u.flatCols)
      | otherwise = toList (acceptedFieldRoots u)
    rank c
      | T.isPrefixOf c root || T.isPrefixOf root c = Just (0 :: Int, T.length c)
      | d <- editDistance root c, d <= 2 = Just (d, T.length c)
      | otherwise = Nothing


-- | Levenshtein distance, capped implicitly by the caller's threshold. Small
-- enough not to warrant a dependency for the one place it is used.
--
-- >>> map (editDistance "kind") ["kind", "kinds", "kimd", "duration"]
-- [0,1,1,7]
editDistance :: T.Text -> T.Text -> Int
editDistance a b = fromMaybe 0 (viaNonEmpty last (foldl' step [0 .. T.length a] (toString b)))
  where
    step row@(prev : rest) c = scanl' next (prev + 1) (zip3 (toString a) row rest)
      where
        next left (ca, diag, up) = min (min (left + 1) (up + 1)) (if ca == c then diag else diag + 1)
    step [] _ = []


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
--
-- @method@ earns its place the same way: it is the name the product itself writes —
-- the shipped @http-stats@/@kitchensink@ dashboards and the apitoolkit-era saved
-- queries all filter on a bare @method@ — and once field validation started rejecting
-- roots that name no column, those widgets stopped rendering.
--
-- >>> outputFieldAliases M.! "method"
-- "attributes___http___request___method"
outputFieldAliases :: M.Map T.Text T.Text
outputFieldAliases =
  M.fromList
    [ ("span_name", "name")
    , ("service", "resource___service___name")
    , ("trace_id", "context___trace_id")
    , ("method", "attributes___http___request___method")
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
-- "request_body->>'message'"
--
-- >>> display (Subject "" "request_body" [FieldKey "message", FieldKey "value"])
-- "request_body->'message'->>'value'"
--
-- >>> display (Subject "" "errors" [ArrayIndex "" 0, FieldKey "message"])
-- "errors->0->>'message'"
instance Display Subject where
  displayPrec prec (Subject entire x keys) =
    case M.lookup entire outputFieldAliases of
      Just col -> displayPrec prec col
      Nothing
        | entire `S.member` flattenedOtelAttributes -> displayPrec prec (transformFlattenedAttribute entire)
        | otherwise -> displayPrec prec (buildQuerySequence x keys)
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

      separator = bool "->'" "->>'"
      separatorInt = bool "->" "->>"


-- | Convert a KQL timespan to PostgreSQL interval syntax. Timespans exceeding
-- 1 year are capped to prevent DoS attacks.
--
-- Example conversion:
-- 7d -> INTERVAL '7 days'
-- 12h -> INTERVAL '12 hours'
-- 30m -> INTERVAL '30 minutes'
-- 45s -> INTERVAL '45 seconds'
-- 500ms -> INTERVAL '500 milliseconds'
kqlTimespanToInterval :: Text -> Text
kqlTimespanToInterval timespan
  | totalSecs > 365 * 86400 = "INTERVAL '365 days'"
  | otherwise = "INTERVAL '" <> intervalExpr <> "'"
  where
    (intervalExpr, totalDuration) = case parseTimespan timespan of ("", _) -> ("0", 0); r -> r
    totalSecs = realToFrac totalDuration :: Double


-- | KQL timespan units: parse suffix, PostgreSQL interval unit name, seconds per unit.
-- Multi-char suffixes precede their single-char prefixes ("ms" before "m"/"s") so a
-- prefix match can't swallow the wrong unit.
timespanUnits :: [(Text, Text, Double)]
timespanUnits =
  [ ("ms", "milliseconds", 0.001)
  , ("us", "microseconds", 0.000001)
  , ("ns", "nanoseconds", 0.000000001)
  , ("d", "days", 86400)
  , ("h", "hours", 3600)
  , ("m", "minutes", 60)
  , ("s", "seconds", 1)
  ]


-- | Parse a KQL timespan (@1h30m@, @7d@, @500ms@) into a rendered PostgreSQL
-- interval expression and its total duration. Single source of truth shared by
-- 'kqlTimespanToInterval' (SQL rendering) and 'timespanToSeconds' (ago() arithmetic).
parseTimespan :: Text -> (Text, NominalDiffTime)
parseTimespan t
  | (digits, rest) <- T.span (\c -> isDigit c || c == '.') t
  , Just (name, secs, rest') <- asum [(name,secs,) <$> T.stripPrefix u rest | (u, name, secs) <- timespanUnits] =
      let (restExpr, restSecs) = parseTimespan rest'
          n = fromMaybe 0 (readMaybe @Double (toString digits))
       in (T.strip (digits <> " " <> name <> " " <> restExpr), realToFrac (n * secs) + restSecs)
  | otherwise = ("", 0)


-- | Convert a KQL timespan to a PostgreSQL @time_bucket@ width, or 'Nothing'
-- when the spelling names no unit we can bucket by.
--
-- Callers must not paper over the 'Nothing'. This returned @"5 minutes"@ for
-- anything it failed to parse, which meant @bin(timestamp, 30sec)@ charted
-- 5-minute buckets and @bin(timestamp, 10min)@ charted 5-minute buckets —
-- silently, with no error anywhere. A chart that answers a different question
-- than the one asked is worse than a chart that refuses; 'unsupportedTimespan'
-- is how the query is rejected instead.
--
-- >>> map kqlTimespanToTimeBucket ["30s", "5m", "1h", "7d", "1w", "500ms"]
-- [Just "30 seconds",Just "5 minutes",Just "1 hours",Just "7 days",Just "1 weeks",Just "500 milliseconds"]
--
-- A unitless number is seconds — the reading the shipped README assumes when it
-- writes @bin(timestamp, 86400)@ for a daily bucket:
--
-- >>> map kqlTimespanToTimeBucket ["60", "300", "86400"]
-- [Just "60 seconds",Just "300 seconds",Just "86400 seconds"]
--
-- Everything else is REFUSED rather than guessed at — calendar units because
-- @time_bucket@ has no fixed width for them, longhand aliases because they did
-- not survive the trip to SQL intact (see 'timespanSuffixes'):
--
-- >>> map kqlTimespanToTimeBucket ["1mo", "1M", "1y", "1 month", "30sec", "10min"]
-- [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
--
-- SECURITY: never passes user input through — always reconstructs the string.
kqlTimespanToTimeBucket :: Text -> Maybe T.Text
kqlTimespanToTimeBucket timespan = parseBareSeconds ts <|> parsePostgresInterval ts <|> parseKqlFormat ts
  where
    ts = T.strip timespan
    -- A unitless number is SECONDS, which is what the shipped README means by
    -- `bin(timestamp, 86400)` — one day. It used to fall through to the
    -- five-minute default, so that documented example silently drew 5-minute
    -- buckets. `bin(timestamp, 300)` is unaffected: 300 seconds IS five
    -- minutes, which is why the old default hid this for so long.
    parseBareSeconds t = (<> " seconds") . show @Text @Int <$> (readMaybe (toString t) >>= \n -> guard (n > 0) $> n)
    validUnits = S.fromList ["second", "seconds", "minute", "minutes", "hour", "hours", "day", "days", "week", "weeks", "millisecond", "milliseconds", "microsecond", "microseconds", "nanosecond", "nanoseconds"]
    -- Parse and reconstruct PostgreSQL interval format (returns validated string, not original input)
    parsePostgresInterval t = case words t of
      [num, unit] | Just n <- readMaybe @Int (toString num), unit `S.member` validUnits -> Just $ show n <> " " <> unit
      _ -> Nothing
    -- Longest suffix first: "30sec" must match "sec", not "s" with a "30se"
    -- numeral that fails to read.
    parseKqlFormat t =
      listToMaybe
        [ show n <> " " <> name
        | (sfx, name) <- sortOn (negate . T.length . fst) timespanSuffixes
        , Just n <- [T.stripSuffix sfx t >>= readMaybe @Int . toString]
        ]


-- | The width a @bin()@ falls back to when a caller renders without validating.
-- Named so the fallback is greppable: it used to be an inline @"5 minutes"@,
-- which is exactly what made the wrong-bucket bug invisible.
defaultBinWidth :: T.Text
defaultBinWidth = "5 minutes"


-- | Every timespan suffix @bin()@ accepts, mapped to its PostgreSQL unit.
--
-- Deliberately ONLY the single-letter KQL forms plus @µs@/@w@. Multi-character
-- aliases (@sec@, @min@, @hr@, @wk@) were tried and REVERTED on 2026-08-31:
-- 'kqlTimespanToTimeBucket' rendered them correctly — the doctests above prove
-- @"30sec" -> Just "30 seconds"@ — but the query still bucketed at one minute
-- end to end, and @10min@ at roughly five. Something between this function and
-- the emitted SQL mangles them and was not identified. Accepting an alias that
-- silently buckets wrong is the exact bug this module is being fixed for, so
-- they stay rejected (with a message naming the spelling to use) until that
-- path is understood. Do not re-add them without an end-to-end row-count check
-- against a fixed window.
timespanSuffixes :: [(T.Text, T.Text)]
timespanSuffixes = ("µs", "microseconds") : ("w", "weeks") : [(u, nm) | (u, nm, _) <- timespanUnits]


-- | The error for a @bin()@ width we cannot bucket by, or 'Nothing' when it is
-- fine. Split from 'kqlTimespanToTimeBucket' so validation can reject the query
-- at parse time — with a position for the editor squiggle — rather than letting
-- a wrong bucket reach a chart.
--
-- >>> unsupportedTimespan "30s"
-- Nothing
-- >>> unsupportedTimespan "1mo"
-- Just "Unsupported bin() width \"1mo\". Use a KQL unit: ms, s, m, h, d, w (e.g. 30s, 10m, 1h). Calendar months and years have no fixed width."
unsupportedTimespan :: T.Text -> Maybe T.Text
unsupportedTimespan t
  | isJust (kqlTimespanToTimeBucket t) = Nothing
  | otherwise =
      Just
        $ "Unsupported bin() width \""
        <> T.strip t
        <> "\". Use a KQL unit: ms, s, m, h, d, w (e.g. 30s, 10m, 1h). Calendar months and years have no fixed width."


-- | The canonical SQL string-literal encoder: single-quote and double any
-- embedded quote. Single source of truth for value escaping across the query
-- generator (scalar comparisons, IN-lists, jsonpath) so the injection guarantee
-- lives in one place. Relies on standard_conforming_strings (PostgreSQL default).
--
-- >>> sqlStringLit "foo'bar"
-- "'foo''bar'"
sqlStringLit :: Text -> Text
sqlStringLit v = "'" <> T.replace "'" "''" v <> "'"


instance Display Values where
  displayPrec prec (Num a) = displayPrec prec a
  displayPrec prec (Str a) = displayPrec prec $ sqlStringLit a
  displayPrec _ (Boolean b) = bool "false" "true" b
  displayPrec _ Null = "null"
  displayPrec prec (Duration _ ns) = displayPrec prec (show ns)
  displayPrec _ NowExpression = displayBuilder @Text "NOW()"
  displayPrec _ (AgoExpression timespan) = displayBuilder $ "NOW() - " <> kqlTimespanToInterval timespan
  displayPrec _ (TimeFunction tf) = displayBuilder tf
  displayPrec prec (List vs) =
    let arrayElements = mconcat . intersperse "," . map (displayPrec prec) $ vs
     in "ARRAY[" <> arrayElements <> "]"
  displayPrec prec (Field sub) = displayPrec prec sub
  displayPrec _ (ScalarFunc name args) = displayBuilder $ scalarFuncToSQL name args
  displayPrec prec (TimestampLit iso) = displayPrec prec (sqlStringLit iso <> "::timestamptz")


-- | Type cast function name to SQL type mapping
typeCastMap :: Map Text Text
typeCastMap = M.fromList [("toint", "integer"), ("tolong", "bigint"), ("tostring", "text"), ("tofloat", "float"), ("todouble", "double precision"), ("tobool", "boolean")]


-- | Unary scalar functions, each mapping its rendered argument to the SQL predicate
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
-- "jsonb_path_exists(to_jsonb(abc), '$[*].\"xyz\" ? (@ == \"val\")'::jsonpath)"
--
-- >>> display (Eq (Subject "" "errors" [ArrayWildcard "", ArrayIndex "message" 0, FieldKey "details"]) (Str "detailsVal"))
-- "jsonb_path_exists(to_jsonb(errors), '$[*].\"message\"[0].\"details\" ? (@ == \"detailsVal\")'::jsonpath)"
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
-- >>> display (Regex (Subject "" "request_body" [FieldKey "msg"]) "^abc.*")
-- "jsonb_path_exists(to_jsonb(request_body), '$.\"msg\" ? (@ like_regex \"^abc.*\" flag \"i\")'::jsonpath)"
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
-- "url::text ~* 'api'"
--
-- >>> display (NotContains (Subject "" "path" []) (Str "admin"))
-- "path::text !~* 'admin'"
--
-- The search term is matched literally — regex metacharacters are escaped:
--
-- >>> display (Contains (Subject "" "v" []) (Str "3.14"))
-- "v::text ~* '3\\.14'"
--
-- >>> display (StartsWith (Subject "" "endpoint" []) (Str "/api/"))
-- "endpoint::text ~* '^/api/'"
--
-- >>> display (NotStartsWith (Subject "" "path" []) (Str "/internal"))
-- "path::text !~* '^/internal'"
--
-- >>> display (EndsWith (Subject "" "filename" []) (Str ".log"))
-- "filename::text ~* '\\.log$'"
--
-- >>> display (NotEndsWith (Subject "" "url" []) (Str ".css"))
-- "url::text !~* '\\.css$'"
--
-- On a wildcard subject, negation wraps the whole existence test (NOT jsonb_path_exists),
-- so @!in@ means "no element matches" — not "some element differs":
--
-- >>> display (NotIn (Subject "" "roles" [ArrayWildcard ""]) (List [Str "admin"]))
-- "NOT (jsonb_path_exists(to_jsonb(roles), '$[*] ? (@ == \"admin\")'::jsonpath))"
--
-- >>> display (NotHas (Subject "" "logs" [ArrayWildcard "", FieldKey "msg"]) (Str "error"))
-- "NOT (jsonb_path_exists(to_jsonb(logs), '$[*].\"msg\" ? (@ like_regex \"error\" flag \"i\")'::jsonpath))"
--
-- >>> display (Matches (Subject "" "email" []) ".*@company\\.com")
-- "jsonb_path_exists(to_jsonb(email), '$ ? (@ like_regex \".*@company\\\\.com\" flag \"i\")'::jsonpath)"

-- | Decompose a binary Expr into its operands plus the (display op-string, KQL
-- parse symbol) drawn from the shared operator tables, so Display and
-- ToQueryText reuse a single constructor->token map. The bespoke cases
-- (Matches/Paren/And/Or/Regex/BoolFunc) are handled directly in the instances.
binaryParts :: [(ctor, Expr -> Maybe (a, b), Text, Text)] -> Expr -> Maybe (a, b, Text, Text)
binaryParts tbl e = listToMaybe [(x, y, dop, sym) | (_, matchP, sym, dop) <- tbl, Just (x, y) <- [matchP e]]


instance Display Expr where
  displayPrec prec e
    | Just (sub, val, op, _) <- binaryParts subjectBinOps e = displayExprHelper op prec sub val
    | Just (v1, v2, op, _) <- binaryParts valBinOps e = displayParen (prec > 0) $ displayPrec prec v1 <> " " <> displayBuilder op <> " " <> displayPrec prec v2
  displayPrec prec (Matches sub val) = displayPrec prec $ renderJsonpathSQL "like_regex" sub (Str val)
  displayPrec prec (Paren u1) = displayParen True $ displayPrec prec u1
  displayPrec prec (And u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " AND " <> displayPrec prec u2
  displayPrec prec (Or u1 u2) = displayParen (prec > 0) $ displayPrec prec u1 <> " OR " <> displayPrec prec u2
  displayPrec prec (Regex sub val) = displayPrec prec $ renderJsonpathSQL "like_regex" sub (Str val)
  displayPrec prec (BoolFunc v) = displayPrec prec v -- Boolean scalar function renders directly to SQL
  displayPrec _ _ = error "Display Expr: unreachable"


-- To be used when generating the text query given an ast
instance ToQueryText Expr where
  toQText e
    | Just (sub, val, _, sym) <- binaryParts subjectBinOps e = toQText sub <> " " <> sym <> " " <> toQText val
    | Just (v1, v2, _, sym) <- binaryParts valBinOps e = toQText v1 <> " " <> sym <> " " <> toQText v2
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
      then displayPrec prec (renderJsonpathSQL op sub val)
      else case (op, val) of
        ("=", Null) -> displayPrec prec sub <> " IS NULL"
        ("!=", Null) -> displayPrec prec sub <> " IS NOT NULL"
        (_, List vs) | op `elem` ["IN", "NOT IN"] -> displayPrec prec sub <> displayBuilder (" " <> op <> " (") <> commaSep vs <> ")"
        -- has_any/has_all fan the (escaped, case-insensitive) literal match over the list
        (_, List vs) | Just j <- lookup op [("HAS_ANY", " OR "), ("HAS_ALL", " AND ")] -> "(" <> (mconcat . intersperse j . map (\v -> subAsText <> " ~* " <> reTerm "" "" v)) vs <> ")"
        -- has/contains/startswith/endswith match a literal (case-insensitively) — the term
        -- is regex-escaped and rendered as `~*`, mirroring the wildcard jsonpath path
        -- (like_regex + escapeRegex). ^/$ anchor startswith/endswith. "NOT X" negates to `!~*`.
        _
          | (neg, opPos) <- maybe (False, op) (True,) (T.stripPrefix "NOT " op)
          , Just (pre, post) <- lookup opPos [("HAS", ("", "")), ("CONTAINS", ("", "")), ("STARTSWITH", ("^", "")), ("ENDSWITH", ("", "$"))] ->
              subAsText <> (if neg then " !~* " else " ~* ") <> reTerm pre post val
        _ -> displayPrec prec sub <> " " <> displayPrec @T.Text prec op <> " " <> displayPrec prec val
  where
    commaSep = mconcat . intersperse "," . map (displayPrec prec)
    -- Cast to text for subjects without field keys (may be JSONB columns)
    subAsText = case sub of
      Subject _ _ [] -> displayPrec prec sub <> "::text"
      _ -> displayPrec prec sub
    -- SQL regex literal for a search term: metacharacters escaped so it matches
    -- literally, with optional ^/$ anchors. Non-string terms render as-is.
    reTerm pre post = \case
      Str s -> displayPrec prec (Str (pre <> escapeRegex s <> post))
      v -> displayPrec prec v


-- | Postgres SQL/JSON-path (jsonpath) target IR. Only constructs that are valid
-- Postgres jsonpath are representable, so 'renderJsonpath' is total and always
-- emits syntactically valid jsonpath. KQL operators are translated into this IR
-- by 'lowerPred'; a comparison with no jsonpath form (now()/ago(), field refs) is
-- a typed 'LowerErr', never a silently-invalid string like @\@ IN [...]@.
data Jsonpath = Jsonpath Text JPath JPred
  deriving stock (Show)


-- | An absolute path from the document root @$@, then a sequence of steps.
newtype JPath = JPath [JStep]
  deriving stock (Show)


data JStep = JKey Text | JIdx Int | JWild
  deriving stock (Show)


-- | A filter predicate: the body of @? ( … )@, always about the current item @\@@.
data JPred
  = JCmp JCmpOp JLit -- @\@ == "x"@
  | JLikeRegex Text -- @\@ like_regex "…" flag "i"@ (always case-insensitive here)
  | JAnd JPred JPred
  | JOr JPred JPred
  deriving stock (Show)


data JCmpOp = JEq | JNeq | JLt | JLte | JGt | JGte
  deriving stock (Show)


data JLit
  = JStr Text
  | JNum Text
  | JBool Bool
  | JNull
  | -- | ISO-8601 instant compared via @.datetime()@
    JDateTime Text
  deriving stock (Show)


-- | Why a KQL comparison has no Postgres jsonpath rendering.
data LowerErr
  = -- | value has no jsonpath equivalent (now(), ago(), field ref, empty list)
    NoJsonpathForm Text
  | -- | operator token not in the KQL operator set
    UnknownOp Text
  deriving stock (Show)


-- | Render the jsonpath IR to a Postgres @jsonb_path_exists@ predicate. Total: by
-- construction the IR can only encode valid jsonpath.
renderJsonpath :: Jsonpath -> Text
renderJsonpath (Jsonpath base path pred_) =
  -- Single-quoted SQL literal with @''@-escaping (matches the rest of the module and
  -- transformFlattenedAttribute), not @$$@ dollar-quoting which a @$$@ in a user value would break.
  "jsonb_path_exists(to_jsonb(" <> base <> "), " <> sqlStringLit (renderPath path <> " ? (" <> renderPred False pred_ <> ")") <> "::jsonpath)"
  where
    renderPath (JPath steps) = "$" <> foldMap step steps
    step (JKey k) = ".\"" <> k <> "\""
    step (JIdx i) = "[" <> show i <> "]"
    step JWild = "[*]"

    -- The 'Bool' requests parenthesization: the mandatory @? ( … )@ wrapper already
    -- groups, so a binary op only self-parenthesizes when nested inside another one.
    -- Datetime comparison coerces both operands via @.datetime()@ (jsonpath has no now()).
    renderPred _ (JCmp o (JDateTime iso)) = "@.datetime() " <> cmp o <> " " <> jsonString iso <> ".datetime()"
    renderPred _ (JCmp o l) = "@ " <> cmp o <> " " <> lit l
    renderPred _ (JLikeRegex rx) = "@ like_regex " <> jsonString rx <> " flag \"i\""
    renderPred p (JAnd a b) = paren p (renderPred True a <> " && " <> renderPred True b)
    renderPred p (JOr a b) = paren p (renderPred True a <> " || " <> renderPred True b)

    paren doParen s = if doParen then "(" <> s <> ")" else s

    cmp JEq = "=="
    cmp JNeq = "!="
    cmp JLt = "<"
    cmp JLte = "<="
    cmp JGt = ">"
    cmp JGte = ">="

    lit (JStr s) = jsonString s
    lit (JNum n) = n
    lit (JBool b) = bool "false" "true" b
    lit JNull = "null"
    lit (JDateTime iso) = jsonString iso <> ".datetime()"


-- | JSON-escaped, double-quoted string literal — valid inside a jsonpath.
jsonString :: Text -> Text
jsonString = toText . encodeToLazyText . AE.String


-- | Lower a KQL @operator/subject/value@ comparison into the jsonpath IR. The
-- operator token is the display token from 'subjectBinOps' (plus @like_regex@ for
-- the regex exprs); every token that reaches here has an explicit arm, and any
-- value with no jsonpath form is a typed 'LowerErr'.
--
-- >>> renderJsonpath <$> lowerPred "=" (Subject "" "data" [FieldKey "name"]) (Str "John Doe")
-- Right "jsonb_path_exists(to_jsonb(data), '$.\"name\" ? (@ == \"John Doe\")'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "!=" (Subject "" "settings" [ArrayWildcard "", FieldKey "enabled"]) (Boolean True)
-- Right "jsonb_path_exists(to_jsonb(settings), '$[*].\"enabled\" ? (@ != true)'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "=" (Subject "" "u" [ArrayWildcard "", FieldKey "x"]) Null
-- Right "jsonb_path_exists(to_jsonb(u), '$[*].\"x\" ? (@ == null)'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "!=" (Subject "" "u" [ArrayWildcard "", FieldKey "x"]) Null
-- Right "jsonb_path_exists(to_jsonb(u), '$[*].\"x\" ? (@ != null)'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "IN" (Subject "" "users" [ArrayWildcard "", FieldKey "role"]) (List [Str "admin", Str "user"])
-- Right "jsonb_path_exists(to_jsonb(users), '$[*].\"role\" ? (@ == \"admin\" || @ == \"user\")'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "HAS" (Subject "" "logs" [ArrayWildcard "", FieldKey "message"]) (Str "error")
-- Right "jsonb_path_exists(to_jsonb(logs), '$[*].\"message\" ? (@ like_regex \"error\" flag \"i\")'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "CONTAINS" (Subject "" "e" [ArrayWildcard "", FieldKey "d"]) (Str "login")
-- Right "jsonb_path_exists(to_jsonb(e), '$[*].\"d\" ? (@ like_regex \"login\" flag \"i\")'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "HAS_ANY" (Subject "" "t" [ArrayWildcard "", FieldKey "k"]) (List [Str "a", Str "b"])
-- Right "jsonb_path_exists(to_jsonb(t), '$[*].\"k\" ? (@ like_regex \"a\" flag \"i\" || @ like_regex \"b\" flag \"i\")'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "HAS_ALL" (Subject "" "t" [ArrayWildcard "", FieldKey "k"]) (List [Str "a", Str "b"])
-- Right "jsonb_path_exists(to_jsonb(t), '$[*].\"k\" ? (@ like_regex \"a\" flag \"i\" && @ like_regex \"b\" flag \"i\")'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "STARTSWITH" (Subject "" "r" [ArrayWildcard "", FieldKey "path"]) (Str "/api")
-- Right "jsonb_path_exists(to_jsonb(r), '$[*].\"path\" ? (@ like_regex \"^/api\" flag \"i\")'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "ENDSWITH" (Subject "" "f" [ArrayWildcard "", FieldKey "name"]) (Str ".log")
-- Right "jsonb_path_exists(to_jsonb(f), '$[*].\"name\" ? (@ like_regex \"\\\\.log$\" flag \"i\")'::jsonpath)"
--
-- >>> renderJsonpath <$> lowerPred "like_regex" (Subject "" "request_body" [FieldKey "msg"]) (Str "^abc.*")
-- Right "jsonb_path_exists(to_jsonb(request_body), '$.\"msg\" ? (@ like_regex \"^abc.*\" flag \"i\")'::jsonpath)"
--
-- A resolved timestamp compares via @.datetime()@ on both operands:
--
-- >>> renderJsonpath <$> lowerPred ">" (Subject "" "spans" [ArrayWildcard "", FieldKey "ts"]) (TimestampLit "2026-07-11T20:00:00Z")
-- Right "jsonb_path_exists(to_jsonb(spans), '$[*].\"ts\" ? (@.datetime() > \"2026-07-11T20:00:00Z\".datetime())'::jsonpath)"
--
-- Bare now()\/ago() have no jsonpath form; 'resolveWildcardTimes' must resolve them first:
--
-- >>> lowerPred ">" (Subject "" "attrs" [ArrayWildcard "", FieldKey "ts"]) NowExpression
-- Left (NoJsonpathForm "now()")
--
-- >>> lowerPred ">" (Subject "" "attrs" [ArrayWildcard "", FieldKey "ts"]) (AgoExpression "1h")
-- Left (NoJsonpathForm "ago(1h)")
lowerPred :: Text -> Subject -> Values -> Either LowerErr Jsonpath
lowerPred op (Subject _ base keys) val = Jsonpath base (JPath (concatMap toSteps keys)) <$> pred_
  where
    toSteps (FieldKey k) = [JKey k]
    toSteps (ArrayIndex "" i) = [JIdx i]
    toSteps (ArrayIndex k i) = [JKey k, JIdx i]
    toSteps (ArrayWildcard "") = [JWild]
    toSteps (ArrayWildcard k) = [JKey k, JWild]

    -- Negation is NOT handled here: on a wildcard subject it must negate the whole
    -- existence test (@NOT jsonb_path_exists@), not the inner filter — see renderJsonpathSQL.
    pred_ = case (lookup op cmpOps, op) of
      (Just c, _) -> JCmp c <$> lit val
      (Nothing, "IN") -> orEq
      (Nothing, "HAS") -> substr
      (Nothing, "CONTAINS") -> substr
      (Nothing, "HAS_ANY") -> likeList JOr
      (Nothing, "HAS_ALL") -> likeList JAnd
      (Nothing, "STARTSWITH") -> JLikeRegex . (\t -> "^" <> escapeRegex t) <$> asTerm val
      (Nothing, "ENDSWITH") -> JLikeRegex . (\t -> escapeRegex t <> "$") <$> asTerm val
      (Nothing, "like_regex") -> JLikeRegex <$> asTerm val -- raw user regex, not escaped
      (Nothing, _) -> Left (UnknownOp op)

    cmpOps :: [(Text, JCmpOp)]
    cmpOps = [("=", JEq), ("==", JEq), ("!=", JNeq), (">=", JGte), ("<=", JLte), (">", JGt), ("<", JLt)]
    substr = JLikeRegex . escapeRegex <$> asTerm val
    orEq = combineList JOr . fmap (JCmp JEq) =<< traverse lit =<< listVals
    likeList c = combineList c . fmap (JLikeRegex . escapeRegex) =<< traverse asTerm =<< listVals
    combineList c xs = maybe (Left (NoJsonpathForm "empty list")) (Right . \(y :| ys) -> foldl' c y ys) (nonEmpty xs)

    listVals = case val of List xs -> Right xs; _ -> Left (NoJsonpathForm "expected list")

    lit (Num n) = Right (JNum n)
    lit (Str s) = Right (JStr s)
    lit (Boolean b) = Right (JBool b)
    lit Null = Right JNull
    lit (Duration _ ns) = Right (JNum (show ns))
    lit NowExpression = Left (NoJsonpathForm "now()")
    lit (AgoExpression t) = Left (NoJsonpathForm ("ago(" <> t <> ")"))
    lit (TimeFunction tf) = Left (NoJsonpathForm tf)
    lit (Field _) = Left (NoJsonpathForm "field reference")
    lit (ScalarFunc n _) = Left (NoJsonpathForm (n <> "()"))
    lit (List _) = Left (NoJsonpathForm "list")
    lit (TimestampLit iso) = Right (JDateTime iso)

    asTerm (Str s) = Right s
    asTerm (Num n) = Right n
    asTerm v = Left (NoJsonpathForm (display v))


-- | SQL for a wildcard-subject comparison. A @NOT X@ operator negates the whole
-- existence test (@NOT jsonb_path_exists@) — pushing @!@ into the @$[*] ? (…)@ filter
-- would instead match rows where *some* element fails, i.e. the wrong quantifier.
-- 'lowerPred' failures (values with no jsonpath form, e.g. now()\/ago()) render as the
-- non-matching predicate @false@, keeping the SQL valid rather than emitting jsonpath
-- Postgres would reject.
renderJsonpathSQL :: Text -> Subject -> Values -> Text
renderJsonpathSQL op sub val = case T.stripPrefix "NOT " op of
  Just pos -> "NOT (" <> render pos <> ")"
  Nothing -> render op
  where
    render o = either (const "false") renderJsonpath (lowerPred o sub val)


-- | Resolve now()/ago() on wildcard-subject comparisons to a concrete ISO-8601 instant,
-- so they lower to a jsonpath @.datetime()@ comparison (jsonpath has no now()). Runs per
-- query build, so the instant is fresh each execution. Non-wildcard comparisons are left
-- untouched — the SQL path renders them as NOW()/INTERVAL.
--
-- Wildcard @ago(1h)@ resolves to (now − 1h) as a jsonpath datetime comparison:
--
-- >>> display (resolveWildcardTimes (UTCTime (fromGregorian 2026 7 11) 75600) (GT (Subject "" "spans" [ArrayWildcard "", FieldKey "ts"]) (AgoExpression "1h")))
-- "jsonb_path_exists(to_jsonb(spans), '$[*].\"ts\" ? (@.datetime() > \"2026-07-11T20:00:00Z\".datetime())'::jsonpath)"
--
-- A non-wildcard comparison is untouched — now() stays NOW() (query-execution time):
--
-- >>> display (resolveWildcardTimes (UTCTime (fromGregorian 2026 7 11) 75600) (GT (Subject "" "ts" []) NowExpression))
-- "ts > NOW()"
resolveWildcardTimes :: UTCTime -> Expr -> Expr
resolveWildcardTimes now = go
  where
    go (And a b) = And (go a) (go b)
    go (Or a b) = Or (go a) (go b)
    go (Paren a) = Paren (go a)
    go e = fromMaybe e $ listToMaybe [ctor sub (resolveVal val) | (ctor, matchP, _, _) <- subjectBinOps, Just (sub, val) <- [matchP e], subjectHasWildcard sub]

    resolveVal NowExpression = TimestampLit (toText (iso8601Show now))
    resolveVal (AgoExpression ts) = TimestampLit (toText (iso8601Show (addUTCTime (negate (timespanToSeconds ts)) now)))
    resolveVal v = v


-- | KQL timespan (@1h30m@, @7d@, @500ms@) to seconds, for ago() arithmetic.
timespanToSeconds :: Text -> NominalDiffTime
timespanToSeconds = snd . parseTimespan
