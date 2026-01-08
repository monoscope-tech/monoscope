module Pkg.Parser.Stats (
  -- Types
  AggFunction (..),
  Section (..),
  SummarizeByClause (..),
  BinFunction (..),
  SortField (..),
  Sources (..),
  SubjectExpr (..),
  -- Section parsers
  pSummarizeSection,
  pSortSection,
  pTakeSection,
  pExtendSection,
  pProjectSection,
  parseQuery,
  pSource,
  -- Aggregation parsers
  aggFunctionParser,
  namedAggregation,
  pBinFunction,
  pSummarizeByClause,
  pSortField,
  -- KQL function parsers
  pCountIf,
  pDCount,
  pCount,
  pCoalesce,
  pStrcat,
  pIff,
  pCase,
  pRound,
  pToFloat,
  pToInt,
  pToString,
  pPercentileSingle,
  pPercentilesMulti,
  pSubjectExpr,
  pScalarExpr,
  -- Utilities
  defaultBinSize,
  extractPercentilesInfo,
  subjectExprToSQL,
) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayPrec)
import Pkg.Parser.Expr (Expr, Parser, Subject (..), ToQueryText (..), Values (..), kqlTimespanToTimeBucket, pExpr, pSubject, pValues)
import Relude hiding (Sum, many, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, space, string)
import Text.Megaparsec.Char.Lexer qualified as L


-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import Pkg.Parser.Expr (FieldKey(..))
-- >>> import Pkg.Parser.Stats
-- >>> :set -XOverloadedStrings


-- Default bin size for auto binning if not otherwise specified
defaultBinSize :: Text
defaultBinSize = "5 minutes"


-- | Comma separator with trailing space - common parser pattern
comma :: Parser ()
comma = void $ string "," *> space


-- | Helper to avoid repeating <*> pure Nothing for alias-less aggregations
withoutAlias :: Parser (Maybe Text -> a) -> Parser a
withoutAlias p = p <*> pure Nothing


-- | Simple arithmetic expression for percentile functions
-- Supports: field, field / number, field * number
data SubjectExpr = SubjectExpr Subject (Maybe (Text, Double))
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Display a SubjectExpr as SQL
subjectExprToSQL :: SubjectExpr -> Text
subjectExprToSQL (SubjectExpr sub Nothing) = display sub
subjectExprToSQL (SubjectExpr sub (Just (op, val))) = "(" <> display sub <> " " <> op <> " " <> show val <> ")"


instance ToQueryText SubjectExpr where
  toQText (SubjectExpr sub Nothing) = toQText sub
  toQText (SubjectExpr sub (Just (op, val))) = toQText sub <> " " <> op <> " " <> show val


-- Modify Aggregation Functions to include optional aliases
data AggFunction
  = Count Subject (Maybe Text) -- Optional field and alias
  | CountIf Expr (Maybe Text) -- Count with condition (KQL countif)
  | DCount Subject (Maybe Int) (Maybe Text) -- Distinct count with optional accuracy (0-4)
  | P50 Subject (Maybe Text)
  | P75 Subject (Maybe Text)
  | P90 Subject (Maybe Text)
  | P95 Subject (Maybe Text)
  | P99 Subject (Maybe Text)
  | P100 Subject (Maybe Text)
  | Percentile SubjectExpr Double (Maybe Text) -- subject expr, single percentile value, optional alias
  | Percentiles SubjectExpr [Double] (Maybe Text) -- subject expr, percentile values, optional alias
  | Sum Subject (Maybe Text)
  | Avg Subject (Maybe Text)
  | Min Subject (Maybe Text)
  | Max Subject (Maybe Text)
  | Median Subject (Maybe Text)
  | Stdev Subject (Maybe Text)
  | Range Subject (Maybe Text)
  | -- | CustomAgg String [Field] (Maybe String)
    -- Scalar functions that can be used in aggregations (Microsoft KQL compatible)
    Coalesce [Values] (Maybe Text) -- coalesce(expr1, expr2, ...) - variadic, returns first non-null
  | Strcat [Values] (Maybe Text) -- strcat(expr1, expr2, ...) - concatenate any scalar expressions
  | Iff Expr Values Values (Maybe Text) -- iff(condition, then_expr, else_expr)
  | Case [(Expr, Values)] Values (Maybe Text) -- case(pred1, val1, pred2, val2, ..., else)
  | -- Type casting and formatting functions (Microsoft KQL compatible)
    Round Values Values (Maybe Text) -- round(value, decimals) - round to N decimal places
  | ToFloat Values (Maybe Text) -- tofloat(value) - convert to float
  | ToInt Values (Maybe Text) -- toint(value) - convert to integer
  | ToString Values (Maybe Text) -- tostring(value) - convert to text
  | Plain Subject (Maybe Text)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Added to support bin() and bin_auto() functions in by clause
data BinFunction
  = Bin Subject Text
  | BinAuto Subject -- Will use fixed interval or legacy rollup calculation
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Support for summarize by with bin() function
newtype SummarizeByClause = SummarizeByClause [Either Subject BinFunction]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Sources = SSpans | SMetrics
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Syntax:
--
-- summarize
-- <aggregation> ...
-- [<by-clause>]
--
-- aggregation
-- Syntax: [<name>=]<aggregate-function>"("<field>) ["," [<name>=]<aggregate-function>"("<field>)]...
-- for example ... | summarize sum_bytes=sum(bytes)

-- | Handles different aggregation functions and their optional aliases.
--
-- >>> parse aggFunctionParser "" "count(field)"
-- Right (Count (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "count()"
-- Right (Count (Subject "*" "*" []) Nothing)
--
-- >>> parse aggFunctionParser "" "sum(field)"
-- Right (Sum (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "avg(field)"
-- Right (Avg (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "min(field)"
-- Right (Min (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "max(field)"
-- Right (Max (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "median(field)"
-- Right (Median (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "stdev(field)"
-- Right (Stdev (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "range(field)"
-- Right (Range (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "p50(field)"
-- Right (P50 (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "customFunc(field)"
-- Right (Plain (Subject "customFunc" "customFunc" []) Nothing)

-- | Parse a numeric value (float or int) for percentile values
pNumericValue :: Parser Double
pNumericValue = try L.float <|> (fromIntegral <$> (L.decimal :: Parser Int))


-- | Parse a subject expression: field, field / number, field * number
-- >>> parse pSubjectExpr "" "duration"
-- Right (SubjectExpr (Subject "duration" "duration" []) Nothing)
-- >>> parse pSubjectExpr "" "duration / 1e6"
-- Right (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0)))
pSubjectExpr :: Parser SubjectExpr
pSubjectExpr = do
  subj <- pSubject
  opM <- optional $ do
    space
    op <- (string "/" $> "/") <|> (string "*" $> "*")
    space
    val <- pNumericValue
    pure (op, val)
  pure $ SubjectExpr subj opM


-- | Parse percentile(expr, percentile) - single percentile (Microsoft KQL syntax)
-- >>> parse pPercentileSingle "" "percentile(duration, 95)"
-- Right (Percentile (SubjectExpr (Subject "duration" "duration" []) Nothing) 95.0 Nothing)
-- >>> parse pPercentileSingle "" "percentile(duration / 1e6, 95)"
-- Right (Percentile (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0))) 95.0 Nothing)
pPercentileSingle :: Parser AggFunction
pPercentileSingle =
  withoutAlias
    $ Percentile
    <$> (string "percentile(" *> pSubjectExpr)
    <*> (comma *> pNumericValue <* string ")")


-- | Parse percentiles(expr, p1, p2, ...) - multiple percentiles (Microsoft KQL syntax)
-- >>> parse pPercentilesMulti "" "percentiles(duration, 50, 75, 90, 95)"
-- Right (Percentiles (SubjectExpr (Subject "duration" "duration" []) Nothing) [50.0,75.0,90.0,95.0] Nothing)
-- >>> parse pPercentilesMulti "" "percentiles(duration / 1e6, 50, 75, 90, 95)"
-- Right (Percentiles (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0))) [50.0,75.0,90.0,95.0] Nothing)
pPercentilesMulti :: Parser AggFunction
pPercentilesMulti =
  withoutAlias
    $ Percentiles
    <$> (string "percentiles(" *> pSubjectExpr <* comma)
    <*> (pNumericValue `sepBy1` comma <* string ")")


-- | Parse countif(predicate) - count with a filter condition
-- Microsoft KQL: countif(predicate)
-- >>> parse pCountIf "" "countif(status_code == \"ERROR\")"
-- Right (CountIf (Eq (Subject "status_code" "status_code" []) (Str "ERROR")) Nothing)
pCountIf :: Parser AggFunction
pCountIf = withoutAlias $ CountIf <$> (string "countif(" *> pExpr <* string ")")


-- | Parse dcount(expr [, accuracy]) - distinct count with optional accuracy
-- Microsoft KQL: dcount(expr [, accuracy]) where accuracy is 0-4
-- >>> parse pDCount "" "dcount(user_id)"
-- Right (DCount (Subject "user_id" "user_id" []) Nothing Nothing)
-- >>> parse pDCount "" "dcount(user_id, 2)"
-- Right (DCount (Subject "user_id" "user_id" []) (Just 2) Nothing)
-- >>> parse pDCount "" "dcount(user_id, 5)"
-- Left ...
pDCount :: Parser AggFunction
pDCount = withoutAlias $ do
  _ <- string "dcount("
  sub <- pSubject
  accM <- optional $ do
    _ <- comma
    acc <- L.decimal :: Parser Int
    if acc >= 0 && acc <= 4
      then pure acc
      else fail "dcount accuracy must be 0-4 per KQL spec"
  _ <- string ")"
  pure $ DCount sub accM


-- | Parse a scalar expression (field reference or literal value)
-- Used by iff, case, coalesce, strcat for flexible value parsing
-- Field references are wrapped in Field constructor to output as column names (not quoted strings)
-- Note: try is needed because pValues has a catch-all that may consume delimiters before failing
pScalarExpr :: Parser Values
pScalarExpr = try pValues <|> (Field <$> pSubject)


-- | Helper for variadic scalar functions like coalesce/strcat
pVariadicAgg :: Text -> ([Values] -> Maybe Text -> AggFunction) -> Parser AggFunction
pVariadicAgg name ctor =
  withoutAlias
    $ ctor
    <$> (string (name <> "(") *> pScalarExpr `sepBy1` comma <* string ")")


-- | Parse coalesce(expr1, expr2, ...) - return first non-null (variadic, 2-64 args)
-- Microsoft KQL: coalesce(arg, arg_2, [arg_3,...])
-- >>> parse pCoalesce "" "coalesce(method, \"unknown\")"
-- Right (Coalesce [Field (Subject "method" "method" []),Str "unknown"] Nothing)
-- >>> parse pCoalesce "" "coalesce(x)"
-- Left ...
pCoalesce :: Parser AggFunction
pCoalesce = do
  _ <- string "coalesce("
  args <- pScalarExpr `sepBy1` comma
  _ <- string ")"
  when (length args < 2) $ fail "coalesce requires at least 2 arguments"
  pure $ Coalesce args Nothing


-- | Parse strcat(expr1, expr2, ...) - string concatenation (1-64 args)
-- Microsoft KQL: strcat(argument1, argument2 [, argument3 ... ])
-- >>> parse pStrcat "" "strcat(method, \" \", url_path)"
-- Right (Strcat [Field (Subject "method" "method" []),Str " ",Field (Subject "url_path" "url_path" [])] Nothing)
pStrcat :: Parser AggFunction
pStrcat = pVariadicAgg "strcat" Strcat


-- | Parse iff(condition, then_expr, else_expr) - conditional expression
-- Microsoft KQL: iff(if, then, else)
-- >>> parse pIff "" "iff(status_code == \"ERROR\", \"error\", \"ok\")"
-- Right (Iff (Eq (Subject "status_code" "status_code" []) (Str "ERROR")) (Str "error") (Str "ok") Nothing)
pIff :: Parser AggFunction
pIff =
  withoutAlias
    $ Iff
    <$> (string "iff(" *> pExpr)
    <*> (comma *> pScalarExpr)
    <*> (comma *> pScalarExpr <* string ")")


-- | Parse case(pred1, val1, [pred2, val2, ...] else) - multi-branch conditional
-- Microsoft KQL: case(predicate_1, then_1, [predicate_2, then_2, ...] else)
-- >>> parse pCase "" "case(status >= 500, \"5xx\", status >= 400, \"4xx\", \"ok\")"
-- Right (Case [(GTEq (Subject "status" "status" []) (Num "500"),Str "5xx"),(GTEq (Subject "status" "status" []) (Num "400"),Str "4xx")] (Str "ok") Nothing)
pCase :: Parser AggFunction
pCase =
  withoutAlias
    $ Case
    <$> (string "case(" *> many (try $ (,) <$> pExpr <*> (comma *> pScalarExpr <* comma)))
    <*> (pScalarExpr <* string ")")


-- | Parse round(value, decimals) - round to N decimal places
-- Microsoft KQL: round(value [, decimals])
-- >>> parse pRound "" "round(3.14159, 2)"
-- Right (Round (Num "3.14159") (Num "2") Nothing)
-- >>> parse pRound "" "round(duration / 1000000, 2)"
-- Right (Round (Field (Subject "duration / 1000000" "duration" [])) (Num "2") Nothing)
pRound :: Parser AggFunction
pRound =
  withoutAlias
    $ Round
    <$> (string "round(" *> pScalarExpr)
    <*> (comma *> pScalarExpr <* string ")")


-- | Helper for type casting parsers with keyword aliases
pTypeCast :: [Text] -> (Values -> Maybe Text -> AggFunction) -> Parser AggFunction
pTypeCast keywords ctor = withoutAlias $ ctor <$> (asum (map (\k -> string (k <> "(")) keywords) *> pScalarExpr <* string ")")


-- | Parse tofloat(value) / todouble(value) - convert to float
-- >>> parse pToFloat "" "tofloat(count_)"
-- Right (ToFloat (Field (Subject "count_" "count_" [])) Nothing)
pToFloat :: Parser AggFunction
pToFloat = pTypeCast ["tofloat", "todouble"] ToFloat


-- | Parse toint(value) / tolong(value) - convert to integer
-- >>> parse pToInt "" "toint(duration)"
-- Right (ToInt (Field (Subject "duration" "duration" [])) Nothing)
pToInt :: Parser AggFunction
pToInt = pTypeCast ["toint", "tolong"] ToInt


-- | Parse tostring(value) - convert to text
-- >>> parse pToString "" "tostring(status_code)"
-- Right (ToString (Field (Subject "status_code" "status_code" [])) Nothing)
pToString :: Parser AggFunction
pToString = pTypeCast ["tostring"] ToString


-- | Helper for simple function(subject) parsers
pSimpleAgg :: Text -> (Subject -> Maybe Text -> AggFunction) -> Parser AggFunction
pSimpleAgg name ctor = withoutAlias $ ctor <$> (string (name <> "(") *> pSubject <* string ")")


-- | Parse count() or count(field) - handles both empty parens and field reference
pCount :: Parser AggFunction
pCount =
  withoutAlias
    $ Count
    <$> (string "count(" *> ((pSubject <* string ")") <|> (Subject "*" "*" [] <$ string ")")))


aggFunctionParser :: Parser AggFunction
aggFunctionParser =
  choice @[]
    [ pSimpleAgg "sum" Sum
    , try pPercentilesMulti
    , try pPercentileSingle
    , pSimpleAgg "p50" P50
    , pSimpleAgg "p75" P75
    , pSimpleAgg "p90" P90
    , pSimpleAgg "p95" P95
    , pSimpleAgg "p99" P99
    , pSimpleAgg "p100" P100
    , try pCountIf -- countif must come before count
    , pDCount -- dcount commits after matching "dcount(" to enforce accuracy validation (0-4)
    , try pCount
    , try pCoalesce
    , try pStrcat
    , try pIff
    , try pCase
    , try pRound -- Type casting and formatting functions
    , try pToFloat
    , try pToInt
    , try pToString
    , pSimpleAgg "avg" Avg
    , pSimpleAgg "min" Min
    , pSimpleAgg "max" Max
    , pSimpleAgg "median" Median
    , pSimpleAgg "stdev" Stdev
    , pSimpleAgg "range" Range
    , withoutAlias $ Plain <$> pSubject
    ]


-- | Convert AggFunction to KQL syntax (NOT SQL - use Display for SQL)
instance ToQueryText AggFunction where
  toQText (Count (Subject "*" _ _) _) = "count()"
  toQText (Count sub _) = "count(" <> toQText sub <> ")"
  toQText (CountIf cond _) = "countif(" <> toQText cond <> ")"
  toQText (DCount sub Nothing _) = "dcount(" <> toQText sub <> ")"
  toQText (DCount sub (Just acc) _) = "dcount(" <> toQText sub <> ", " <> show acc <> ")"
  toQText (P50 sub _) = "p50(" <> toQText sub <> ")"
  toQText (P75 sub _) = "p75(" <> toQText sub <> ")"
  toQText (P90 sub _) = "p90(" <> toQText sub <> ")"
  toQText (P95 sub _) = "p95(" <> toQText sub <> ")"
  toQText (P99 sub _) = "p99(" <> toQText sub <> ")"
  toQText (P100 sub _) = "p100(" <> toQText sub <> ")"
  toQText (Percentile subExpr pct _) = "percentile(" <> toQText subExpr <> ", " <> show pct <> ")"
  toQText (Percentiles subExpr pcts _) = "percentiles(" <> toQText subExpr <> ", " <> T.intercalate ", " (map show pcts) <> ")"
  toQText (Sum sub _) = "sum(" <> toQText sub <> ")"
  toQText (Avg sub _) = "avg(" <> toQText sub <> ")"
  toQText (Min sub _) = "min(" <> toQText sub <> ")"
  toQText (Max sub _) = "max(" <> toQText sub <> ")"
  toQText (Median sub _) = "median(" <> toQText sub <> ")"
  toQText (Stdev sub _) = "stdev(" <> toQText sub <> ")"
  toQText (Range sub _) = "range(" <> toQText sub <> ")"
  toQText (Coalesce exprs _) = "coalesce(" <> T.intercalate ", " (map toQText exprs) <> ")"
  toQText (Strcat exprs _) = "strcat(" <> T.intercalate ", " (map toQText exprs) <> ")"
  toQText (Iff cond thenVal elseVal _) = "iff(" <> toQText cond <> ", " <> toQText thenVal <> ", " <> toQText elseVal <> ")"
  toQText (Case pairs defVal _) = "case(" <> T.intercalate ", " (concatMap (\(c, v) -> [toQText c, toQText v]) pairs) <> ", " <> toQText defVal <> ")"
  toQText (Round val decimals _) = "round(" <> toQText val <> ", " <> toQText decimals <> ")"
  toQText (ToFloat val _) = "tofloat(" <> toQText val <> ")"
  toQText (ToInt val _) = "toint(" <> toQText val <> ")"
  toQText (ToString val _) = "tostring(" <> toQText val <> ")"
  toQText (Plain sub _) = toQText sub


instance ToQueryText [AggFunction] where
  toQText xs = T.intercalate ", " $ map toQText xs


-- | Get KQL default alias for aggregation (e.g., count_, sum_field)
defaultAlias :: AggFunction -> Text
defaultAlias = \case
  Count (Subject name _ _) _ -> if name == "*" then "count_" else "count_" <> name
  CountIf _ _ -> "countif_"
  DCount (Subject name _ _) _ _ -> "dcount_" <> name
  P50 (Subject name _ _) _ -> "p50_" <> name
  P75 (Subject name _ _) _ -> "p75_" <> name
  P90 (Subject name _ _) _ -> "p90_" <> name
  P95 (Subject name _ _) _ -> "p95_" <> name
  P99 (Subject name _ _) _ -> "p99_" <> name
  P100 (Subject name _ _) _ -> "p100_" <> name
  Percentile (SubjectExpr (Subject name _ _) _) pct _ -> "percentile_" <> name <> "_" <> T.replace "." "_" (show pct)
  Percentiles (SubjectExpr (Subject name _ _) _) _ _ -> "percentiles_" <> name
  Sum (Subject name _ _) _ -> "sum_" <> name
  Avg (Subject name _ _) _ -> "avg_" <> name
  Min (Subject name _ _) _ -> "min_" <> name
  Max (Subject name _ _) _ -> "max_" <> name
  Median (Subject name _ _) _ -> "median_" <> name
  Stdev (Subject name _ _) _ -> "stdev_" <> name
  Range (Subject name _ _) _ -> "range_" <> name
  Coalesce _ _ -> "coalesce_"
  Strcat _ _ -> "strcat_"
  Iff _ _ _ _ -> "iff_"
  Case _ _ _ -> "case_"
  Round _ _ _ -> "round_"
  ToFloat _ _ -> "tofloat_"
  ToInt _ _ -> "toint_"
  ToString _ _ -> "tostring_"
  Plain (Subject name _ _) _ -> name


-- | Wrap SQL expression with AS alias (user-supplied or default)
withAlias :: Text -> Maybe Text -> AggFunction -> Text
withAlias sql explicitAlias agg = sql <> " AS " <> fromMaybe (defaultAlias agg) explicitAlias


instance Display AggFunction where
  displayPrec _prec agg@(Count sub alias) = displayBuilder $ withAlias ("count(" <> display sub <> ")::float") alias agg
  displayPrec _prec agg@(CountIf cond alias) = displayBuilder $ withAlias ("COUNT(*) FILTER (WHERE " <> display cond <> ")::float") alias agg
  -- dcount(field [, accuracy]) -> COUNT(DISTINCT field)
  -- Note: KQL accuracy parameter (0-4 for HyperLogLog precision) is intentionally ignored
  -- because PostgreSQL's COUNT(DISTINCT) doesn't support HLL accuracy hints
  displayPrec _prec agg@(DCount sub _ alias) = displayBuilder $ withAlias ("COUNT(DISTINCT " <> display sub <> ")::float") alias agg
  displayPrec _prec agg@(P50 sub alias) = displayBuilder $ withAlias ("approx_percentile(0.50, percentile_agg((" <> display sub <> ")::float))::int") alias agg
  displayPrec _prec agg@(P75 sub alias) = displayBuilder $ withAlias ("approx_percentile(0.75, percentile_agg((" <> display sub <> ")::float))::int") alias agg
  displayPrec _prec agg@(P90 sub alias) = displayBuilder $ withAlias ("approx_percentile(0.90, percentile_agg((" <> display sub <> ")::float))::int") alias agg
  displayPrec _prec agg@(P95 sub alias) = displayBuilder $ withAlias ("approx_percentile(0.95, percentile_agg((" <> display sub <> ")::float))::int") alias agg
  displayPrec _prec agg@(P99 sub alias) = displayBuilder $ withAlias ("approx_percentile(0.99, percentile_agg((" <> display sub <> ")::float))::int") alias agg
  displayPrec _prec agg@(P100 sub alias) = displayBuilder $ withAlias ("approx_percentile(1, percentile_agg((" <> display sub <> ")::float))::int") alias agg
  -- Percentile/Percentiles are handled specially via extractPercentilesInfo and LATERAL unnest SQL
  displayPrec _prec agg@(Percentile subExpr pct alias) =
    displayBuilder $ withAlias ("approx_percentile(" <> show (pct / 100.0) <> ", percentile_agg((" <> subjectExprToSQL subExpr <> ")::float))::float") alias agg
  displayPrec _prec agg@(Percentiles subExpr pcts alias) =
    let firstPct = fromMaybe 50.0 (listToMaybe pcts)
     in displayBuilder $ withAlias ("approx_percentile(" <> show (firstPct / 100.0) <> ", percentile_agg((" <> subjectExprToSQL subExpr <> ")::float))::float") alias agg
  displayPrec _prec agg@(Sum sub alias) = displayBuilder $ withAlias ("sum((" <> display sub <> ")::float)") alias agg
  displayPrec _prec agg@(Avg sub alias) = displayBuilder $ withAlias ("avg((" <> display sub <> ")::float)") alias agg
  displayPrec _prec agg@(Min sub alias) = displayBuilder $ withAlias ("min((" <> display sub <> ")::float)") alias agg
  displayPrec _prec agg@(Max sub alias) = displayBuilder $ withAlias ("max((" <> display sub <> ")::float)") alias agg
  displayPrec _prec agg@(Median sub alias) = displayBuilder $ withAlias ("median((" <> display sub <> ")::float)") alias agg
  displayPrec _prec agg@(Stdev sub alias) = displayBuilder $ withAlias ("stdev((" <> display sub <> ")::float)") alias agg
  displayPrec _prec agg@(Range sub alias) = displayBuilder $ withAlias ("range((" <> display sub <> ")::float)") alias agg
  -- coalesce(expr1, expr2, ...) -> COALESCE(expr1, expr2, ...)
  displayPrec _prec agg@(Coalesce exprs alias) =
    displayBuilder $ withAlias ("COALESCE(" <> T.intercalate ", " (map display exprs) <> ")") alias agg
  -- strcat(expr1, expr2, ...) -> CONCAT(expr1, expr2, ...)
  displayPrec _prec agg@(Strcat exprs alias) =
    displayBuilder $ withAlias ("CONCAT(" <> T.intercalate ", " (map display exprs) <> ")") alias agg
  -- iff(condition, then, else) -> CASE WHEN condition THEN then ELSE else END
  displayPrec _prec agg@(Iff cond thenVal elseVal alias) =
    displayBuilder $ withAlias ("CASE WHEN " <> display cond <> " THEN " <> display thenVal <> " ELSE " <> display elseVal <> " END") alias agg
  -- case(pred1, val1, ..., else) -> CASE WHEN pred1 THEN val1 ... ELSE else END
  displayPrec _prec agg@(Case pairs defaultVal alias) =
    displayBuilder $ withAlias ("CASE " <> foldMap displayCaseWhen pairs <> "ELSE " <> display defaultVal <> " END") alias agg
    where
      displayCaseWhen (cond, val) = "WHEN " <> display cond <> " THEN " <> display val <> " "
  -- round(value, decimals) -> ROUND((value)::numeric, decimals)::float
  displayPrec _prec agg@(Round val decimals alias) =
    displayBuilder $ withAlias ("ROUND((" <> display val <> ")::numeric, " <> display decimals <> ")::float") alias agg
  -- tofloat(value) -> (value)::float
  displayPrec _prec agg@(ToFloat val alias) =
    displayBuilder $ withAlias ("(" <> display val <> ")::float") alias agg
  -- toint(value) -> (value)::integer
  displayPrec _prec agg@(ToInt val alias) =
    displayBuilder $ withAlias ("(" <> display val <> ")::integer") alias agg
  -- tostring(value) -> (value)::text
  displayPrec _prec agg@(ToString val alias) =
    displayBuilder $ withAlias ("(" <> display val <> ")::text") alias agg
  displayPrec _prec agg@(Plain sub alias) = displayBuilder $ withAlias (display sub) alias agg


instance ToQueryText Section where
  toQText (Source source) = toQText source
  toQText (Search expr) = toQText expr
  toQText (WhereClause expr) = "where " <> toQText expr
  toQText (SummarizeCommand funcs byClauseM) =
    "summarize "
      <> T.intercalate "," (map toQText funcs)
      <> maybeToMonoid (toQText <$> byClauseM)
  toQText (ExtendCommand cols) =
    "extend " <> T.intercalate ", " [name <> " = " <> toQText expr | (name, expr) <- cols]
  toQText (ProjectCommand cols) =
    "project " <> T.intercalate ", " [name <> " = " <> toQText expr | (name, expr) <- cols]
  toQText (SortCommand fields) = "sort by " <> T.intercalate ", " (map toQText fields)
  toQText (TakeCommand limit) = "take " <> toText (show (limit :: Int))


----------------------------------------------------------------------
----

-- Example queries
-- request_body.v1.v2 == "abc" AND (request_body.v3.v4 == 123 OR request_body.v5[].v6==ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |

-- Sort field can include optional direction
data SortField = SortField Subject (Maybe Text) -- field and optional direction (asc/desc)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Section
  = Search Expr
  | WhereClause Expr -- New constructor for where clause after pipe
  | SummarizeCommand [AggFunction] (Maybe SummarizeByClause)
  | ExtendCommand [(Text, AggFunction)] -- extend col = expr, col2 = expr2 (add computed columns)
  | ProjectCommand [(Text, AggFunction)] -- project col = expr, col2 = expr2 (select specific columns)
  | SortCommand [SortField] -- sort by multiple fields
  | TakeCommand Int -- limit/take number of results
  | Source Sources
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Parse a bin function call like 'bin(timestamp, 60)' or 'bin_auto(timestamp)'
--
-- >>> parse pBinFunction "" "bin(timestamp, 60)"
-- Right (Bin (Subject "timestamp" "timestamp" []) "60")
--
-- >>> parse pBinFunction "" "bin_auto(timestamp)"
-- Right (BinAuto (Subject "timestamp" "timestamp" []))
pBinFunction :: Parser BinFunction
pBinFunction = try binParser <|> binAutoParser
  where
    binParser = do
      _ <- string "bin("
      subj <- pSubject
      _ <- string ","
      space
      -- Support time duration units or just plain numbers (seconds)
      interval <- toText <$> some (alphaNumChar <|> char '.' <|> char 'm' <|> char 's' <|> char 'h' <|> char 'd' <|> char 'w')
      _ <- string ")"
      return $ Bin subj interval

    binAutoParser = do
      _ <- string "bin_auto("
      subj <- pSubject
      _ <- string ")"
      return $ BinAuto subj


instance ToQueryText BinFunction where
  toQText (Bin subj interval) = "bin(" <> toQText subj <> ", " <> interval <> ")"
  toQText (BinAuto subj) = "bin_auto(" <> toQText subj <> ")"


instance Display BinFunction where
  displayPrec _prec (Bin subj interval) = displayBuilder $ "time_bucket('" <> kqlTimespanToTimeBucket interval <> "', " <> display subj <> ")"
  -- Use default bin size for auto binning (defined in Parser.hs)
  -- Don't include "as bin_timestamp" here as it causes syntax errors in GROUP BY
  displayPrec _prec (BinAuto subj) = displayBuilder $ "time_bucket('" <> defaultBinSize <> "', " <> display subj <> ")"


-- | Parse a summarize by clause which can contain fields and bin functions
--
-- >>> parse pSummarizeByClause "" "by attributes.client, bin(timestamp, 60)"
-- Right (SummarizeByClause [Left (Subject "attributes.client" "attributes" [FieldKey "client"]),Right (Bin (Subject "timestamp" "timestamp" []) "60")])
--
-- >>> parse pSummarizeByClause "" "by Computer, bin_auto(timestamp)"
-- Right (SummarizeByClause [Left (Subject "Computer" "Computer" []),Right (BinAuto (Subject "timestamp" "timestamp" []))])
--
-- >>> parse pSummarizeByClause "" "by bin_auto(timestamp)"
-- Right (SummarizeByClause [Right (BinAuto (Subject "timestamp" "timestamp" []))])
pSummarizeByClause :: Parser SummarizeByClause
pSummarizeByClause = do
  _ <- string "by"
  space
  SummarizeByClause <$> (try (Right <$> pBinFunction) <|> (Left <$> pSubject)) `sepBy` comma


instance ToQueryText SummarizeByClause where
  toQText (SummarizeByClause fields) = "by " <> T.intercalate ", " (map (either toQText toQText) fields)


-- | Parse a sort field that can include an optional direction
--
-- >>> parse pSortField "" "parent_id asc"
-- Right (SortField (Subject "parent_id" "parent_id" []) (Just "asc"))
--
-- >>> parse pSortField "" "timestamp"
-- Right (SortField (Subject "timestamp" "timestamp" []) Nothing)
pSortField :: Parser SortField
pSortField = do
  field <- pSubject
  dirM <- optional $ try (space *> (string "asc" <|> string "desc"))
  return $ SortField field dirM


instance ToQueryText SortField where
  toQText (SortField field Nothing) = toQText field
  toQText (SortField field (Just dir)) = toQText field <> " " <> dir


-- | Parser for 'sort by' command (also supports 'order by' synonym)
--
-- >>> parse pSortSection "" "sort by parent_id asc, timestamp desc"
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just "asc"),SortField (Subject "timestamp" "timestamp" []) (Just "desc")])
--
-- >>> parse pSortSection "" "order by parent_id asc"
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just "asc")])
pSortSection :: Parser Section
pSortSection = do
  _ <- string "sort by" <|> string "order by"
  space
  fields <- sepBy pSortField comma
  return $ SortCommand fields


-- | Parser for 'take' command (also supports 'limit' synonym)
--
-- >>> parse pTakeSection "" "take 1000"
-- Right (TakeCommand 1000)
--
-- >>> parse pTakeSection "" "limit 500"
-- Right (TakeCommand 500)
pTakeSection :: Parser Section
pTakeSection = do
  _ <- string "take" <|> string "limit"
  space
  limitStr <- some digitChar
  let limit = readMaybe (toString limitStr) :: Maybe Int
  return $ TakeCommand (fromMaybe 1000 limit) -- Default to 1000 if parsing fails


-- | Parse a column assignment like 'col_name = expression' for extend/project
pColumnAssignment :: Parser (Text, AggFunction)
pColumnAssignment = do
  name <- toText <$> some (alphaNumChar <|> oneOf ("_" :: String))
  space
  _ <- string "="
  space
  expr <- aggFunctionParser
  pure (name, setAlias name expr)


-- | Helper for column-assignment commands (extend/project)
pColumnCommand :: Text -> ([(Text, AggFunction)] -> Section) -> Parser Section
pColumnCommand keyword ctor = string keyword *> space *> (ctor <$> pColumnAssignment `sepBy1` comma)


-- | Parser for 'extend' command - add computed columns
-- Microsoft KQL: extend ColumnName = Expression [, ...]
--
-- >>> parse pExtendSection "" "extend error_cat = case(status >= 500, \"5xx\", \"ok\")"
-- Right (ExtendCommand [("error_cat",Case [(GTEq (Subject "status" "status" []) (Num "500"),Str "5xx")] (Str "ok") (Just "error_cat"))])
pExtendSection :: Parser Section
pExtendSection = pColumnCommand "extend" ExtendCommand


-- | Parser for 'project' command - select specific columns
-- Microsoft KQL: project ColumnName = Expression [, ...]
--
-- >>> parse pProjectSection "" "project service = resource.service.name, count = count()"
-- Right (ProjectCommand [("service",Plain (Subject "resource.service.name" "resource" [FieldKey "service",FieldKey "name"]) (Just "service")),("count",Count (Subject "*" "*" []) (Just "count"))])
pProjectSection :: Parser Section
pProjectSection = pColumnCommand "project" ProjectCommand


-- | Set alias on an aggregation function
setAlias :: Text -> AggFunction -> AggFunction
setAlias name (Count sub _) = Count sub (Just name)
setAlias name (CountIf cond _) = CountIf cond (Just name)
setAlias name (DCount sub acc _) = DCount sub acc (Just name)
setAlias name (Sum sub _) = Sum sub (Just name)
setAlias name (Avg sub _) = Avg sub (Just name)
setAlias name (Min sub _) = Min sub (Just name)
setAlias name (Max sub _) = Max sub (Just name)
setAlias name (P50 sub _) = P50 sub (Just name)
setAlias name (P75 sub _) = P75 sub (Just name)
setAlias name (P90 sub _) = P90 sub (Just name)
setAlias name (P95 sub _) = P95 sub (Just name)
setAlias name (P99 sub _) = P99 sub (Just name)
setAlias name (P100 sub _) = P100 sub (Just name)
setAlias name (Percentile subExpr pct _) = Percentile subExpr pct (Just name)
setAlias name (Percentiles subExpr pcts _) = Percentiles subExpr pcts (Just name)
setAlias name (Median sub _) = Median sub (Just name)
setAlias name (Stdev sub _) = Stdev sub (Just name)
setAlias name (Range sub _) = Range sub (Just name)
setAlias name (Coalesce exprs _) = Coalesce exprs (Just name)
setAlias name (Strcat exprs _) = Strcat exprs (Just name)
setAlias name (Iff cond thenVal elseVal _) = Iff cond thenVal elseVal (Just name)
setAlias name (Case pairs defVal _) = Case pairs defVal (Just name)
setAlias name (Round val decimals _) = Round val decimals (Just name)
setAlias name (ToFloat val _) = ToFloat val (Just name)
setAlias name (ToInt val _) = ToInt val (Just name)
setAlias name (ToString val _) = ToString val (Just name)
setAlias name (Plain sub _) = Plain sub (Just name)


-- | Parse a named aggregation like 'TotalCount = count()'
--
-- >>> parse namedAggregation "" "TotalCount=count()"
-- Right (Count (Subject "*" "*" []) (Just "TotalCount"))
--
-- >>> parse namedAggregation "" "error_count = countif(status_code == \"ERROR\")"
-- Right (CountIf (Eq (Subject "status_code" "status_code" []) (Str "ERROR")) (Just "error_count"))
namedAggregation :: Parser AggFunction
namedAggregation = do
  name <- toText <$> some (alphaNumChar <|> oneOf ("_" :: String))
  space
  _ <- string "="
  space
  setAlias name <$> aggFunctionParser


-- | Parse the summarize command
--
-- >>> parse pSummarizeSection "" "summarize sum(attributes.client) by attributes.client, bin(timestamp, 60)"
-- Right (SummarizeCommand [Sum (Subject "attributes.client" "attributes" [FieldKey "client"]) Nothing] (Just (SummarizeByClause [Left (Subject "attributes.client" "attributes" [FieldKey "client"]),Right (Bin (Subject "timestamp" "timestamp" []) "60")])))
--
-- >>> parse pSummarizeSection "" "summarize TotalCount=count() by Computer"
-- Right (SummarizeCommand [Count (Subject "*" "*" []) (Just "TotalCount")] (Just (SummarizeByClause [Left (Subject "Computer" "Computer" [])])))
--
-- >>> parse pSummarizeSection "" "summarize count(*) by bin_auto(timestamp)"
-- Right (SummarizeCommand [Count (Subject "*" "*" []) Nothing] (Just (SummarizeByClause [Right (BinAuto (Subject "timestamp" "timestamp" []))])))
pSummarizeSection :: Parser Section
pSummarizeSection = do
  _ <- string "summarize"
  space
  funcs <- sepBy (try namedAggregation <|> aggFunctionParser) (char ',' <* space)
  byClause <- optional $ try (space *> pSummarizeByClause)

  -- If no by clause was provided, automatically add "by status_code"
  let defaultByClause = SummarizeByClause [Left $ Subject "status_code" "status_code" []]
      finalByClause = Just $ fromMaybe defaultByClause byClause

  return $ SummarizeCommand funcs finalByClause


-- | Parser for where clause section
pWhereSection :: Parser Section
pWhereSection = do
  _ <- string "where"
  space
  WhereClause <$> pExpr


pSection :: Parser Section
pSection = do
  _ <- space
  choice @[]
    [ pSummarizeSection
    , pSortSection
    , pTakeSection
    , try pExtendSection
    , try pProjectSection
    , try pWhereSection -- Try to parse 'where' clause first
    , Search <$> pExpr -- Fall back to bare expression for backward compatibility
    , Source <$> pSource
    ]


-- find what source to use when processing a query. By default, the requests source is used
--
-- >>> parse pSource "" "spans"
-- Right SSpans
--
-- >>> parse pSource "" "metrics"
-- Right SMetrics
--
pSource :: Parser Sources
pSource =
  choice @[]
    [ SSpans <$ string "spans"
    , SSpans <$ string "otlp_logs_and_spans"
    , SMetrics <$ string "metrics"
    , SMetrics <$ string "telemetry.metrics"
    ]


instance ToQueryText Sources where
  toQText SSpans = "spans"
  toQText SMetrics = "metrics"


instance Display Sources where
  displayPrec _prec SSpans = "otel_logs_and_spans"
  displayPrec _prec SMetrics = "telemetry.metrics"


--- >>> parse parseQuery "" "method// = bla "
-- Right []
--
-- >>> parse parseQuery "" "| summarize count(*) by bin_auto(timestamp)"
-- Right [SummarizeCommand [Count (Subject "*" "*" []) Nothing] (Just (SummarizeByClause [Right (BinAuto (Subject "timestamp" "timestamp" []))]))]
--
parseQuery :: Parser [Section]
parseQuery = do
  -- Optionally parse a leading pipe character and ignore it
  _ <- optional (space *> char '|' <* space)
  sepBy pSection (space *> char '|' <* space)


instance ToQueryText [Section] where
  toQText secs = T.intercalate " | " $ map toQText secs


-- | Extract percentiles info from a list of sections
-- Returns (subjectExpr as SQL, percentiles) if a Percentile(s) aggregation is found
extractPercentilesInfo :: [Section] -> Maybe (Text, [Double])
extractPercentilesInfo = foldr go Nothing
  where
    go (SummarizeCommand (findPercentiles -> Just info) _) _ = Just info
    go _ acc = acc
    findPercentiles [] = Nothing
    findPercentiles (Percentile subExpr pct _ : _) = Just (subjectExprToSQL subExpr, [pct])
    findPercentiles (Percentiles subExpr pcts _ : _) = Just (subjectExprToSQL subExpr, pcts)
    findPercentiles (_ : rest) = findPercentiles rest
