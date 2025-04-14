module Pkg.Parser.Stats (pTimeChartSection, pStatsSection, AggFunction (..), Section (..), Rollup (..), ByClause (..), Sources (..), parseQuery, pSource) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayPrec)
import Pkg.Parser.Core
import Pkg.Parser.Expr (Expr, Subject (..), pExpr, pSubject)
import Relude hiding (Sum, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, space, space1, string)


-- Modify Aggregation Functions to include optional aliases
data AggFunction
  = Count Subject (Maybe Text) -- Optional field and alias
  | P50 Subject (Maybe Text)
  | P75 Subject (Maybe Text)
  | P90 Subject (Maybe Text)
  | P95 Subject (Maybe Text)
  | P99 Subject (Maybe Text)
  | P100 Subject (Maybe Text)
  | Sum Subject (Maybe Text)
  | Avg Subject (Maybe Text)
  | Min Subject (Maybe Text)
  | Max Subject (Maybe Text)
  | Median Subject (Maybe Text)
  | Stdev Subject (Maybe Text)
  | Range Subject (Maybe Text)
  | -- | CustomAgg String [Field] (Maybe String)
    Plain Subject (Maybe Text)
  deriving stock (Show, Generic, Eq)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Define an optional 'by' clause
newtype ByClause = ByClause [Subject] -- List of fields to group by
  deriving stock (Show, Generic, Eq)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


newtype Rollup = Rollup Text
  deriving stock (Show, Generic, Eq)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Sources = SRequests | SLogs | STraces | SSpans | SMetrics
  deriving stock (Show, Generic, Eq)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Syntax:
--
-- stats
-- <aggregation> ...
-- ( [<by-clause>] [<time-span>] )
--
-- aggregation
-- Syntax: <aggregate-function>"("<field> [AS <field>] ) ["," <aggregate-function> "("<field> [AS <field>] ) ]...
-- for example ... | stats sum(bytes) AS 'sum of bytes'.

-- | Handles different aggregation functions and their optional aliases.
--
-- >>> parse aggFunctionParser "" "count(field)"
-- Right (Count (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "count(*)"
-- Right (Count (Subject "*" "*" []) Nothing)
--
-- >>> parse aggFunctionParser "" "sum(field) as total"
-- Right (Sum (Subject "field" "field" []) (Just "total"))
--
-- >>> parse aggFunctionParser "" "avg(field) as average"
-- Right (Avg (Subject "field" "field" []) (Just "average"))
--
-- >>> parse aggFunctionParser "" "min(field)"
-- Right (Min (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "max(field) as maxVal"
-- Right (Max (Subject "field" "field" []) (Just "maxVal"))
--
-- >>> parse aggFunctionParser "" "median(field) as medianVal"
-- Right (Median (Subject "field" "field" []) (Just "medianVal"))
--
-- >>> parse aggFunctionParser "" "stdev(field)"
-- Right (Stdev (Subject "field" "field" []) Nothing)
--
-- >>> parse aggFunctionParser "" "range(field) as rangeVal"
-- Right (Range (Subject "field" "field" []) (Just "rangeVal"))
--
-- >>> parse aggFunctionParser "" "p50(field)"
-- Right (Range (Subject "field" "field" []) (Just "rangeVal"))
--
-- >>> parse aggFunctionParser "" "customFunc(field) as custom"
-- Right (Plain (Subject "customFunc" "customFunc" []) Nothing)
aggFunctionParser :: Parser AggFunction
aggFunctionParser =
  choice @[]
    [ Sum <$> (string "sum(" *> pSubject <* string ")") <*> optional aliasParser
    , P50 <$> (string "p50(" *> pSubject <* string ")") <*> optional aliasParser
    , P75 <$> (string "p75(" *> pSubject <* string ")") <*> optional aliasParser
    , P90 <$> (string "p90(" *> pSubject <* string ")") <*> optional aliasParser
    , P95 <$> (string "p95(" *> pSubject <* string ")") <*> optional aliasParser
    , P99 <$> (string "p99(" *> pSubject <* string ")") <*> optional aliasParser
    , P100 <$> (string "p100(" *> pSubject <* string ")") <*> optional aliasParser
    , Count <$> (string "count(" *> pSubject <* string ")") <*> optional aliasParser
    , Avg <$> (string "avg(" *> pSubject <* string ")") <*> optional aliasParser
    , Min <$> (string "min(" *> pSubject <* string ")") <*> optional aliasParser
    , Max <$> (string "max(" *> pSubject <* string ")") <*> optional aliasParser
    , Median <$> (string "median(" *> pSubject <* string ")") <*> optional aliasParser
    , Stdev <$> (string "stdev(" *> pSubject <* string ")") <*> optional aliasParser
    , Range <$> (string "range(" *> pSubject <* string ")") <*> optional aliasParser
    , Plain <$> pSubject <*> optional aliasParser
    ]


instance ToQueryText AggFunction where
  toQText v = display v


instance ToQueryText [AggFunction] where
  toQText xs = T.intercalate "," $ map toQText xs


instance Display AggFunction where
  displayPrec prec (Count sub alias) = displayBuilder $ "count(" <> display sub <> ")"
  displayPrec prec (P50 sub alias) = displayBuilder $ "approx_percentile(0.50, percentile_agg(" <> display sub <> "))::int"
  displayPrec prec (P75 sub alias) = displayBuilder $ "approx_percentile(0.75, percentile_agg(" <> display sub <> "))::int"
  displayPrec prec (P90 sub alias) = displayBuilder $ "approx_percentile(0.90, percentile_agg(" <> display sub <> "))::int"
  displayPrec prec (P95 sub alias) = displayBuilder $ "approx_percentile(0.95, percentile_agg(" <> display sub <> "))::int"
  displayPrec prec (P99 sub alias) = displayBuilder $ "approx_percentile(0.99, percentile_agg(" <> display sub <> "))::int"
  displayPrec prec (P100 sub alias) = displayBuilder $ "approx_percentile(1, percentile_agg(" <> display sub <> "))::int"
  displayPrec prec (Sum sub alias) = displayBuilder $ "sum(" <> display sub <> ")"
  displayPrec prec (Avg sub alias) = displayBuilder $ "avg(" <> display sub <> ")"
  displayPrec prec (Min sub alias) = displayBuilder $ "min(" <> display sub <> ")"
  displayPrec prec (Max sub alias) = displayBuilder $ "max(" <> display sub <> ")"
  displayPrec prec (Median sub alias) = displayBuilder $ "median(" <> display sub <> ")"
  displayPrec prec (Stdev sub alias) = displayBuilder $ "stdev(" <> display sub <> ")"
  displayPrec prec (Range sub alias) = displayBuilder $ "range(" <> display sub <> ")"
  displayPrec prec (Plain sub alias) = displayBuilder $ "" <> display sub <> ""


-- | Utility Parser: Parses an alias
--
-- >>> parse aliasParser "" " as min_price"
-- Right "min_price"
aliasParser :: Parser Text
aliasParser = toText <$> (string " as" *> space1 *> some (alphaNumChar <|> oneOf @[] "_-") <* space)


-- | Parses the 'by' clause, which can include multiple fields.
--
-- >>> parse byClauseParser "" "by field1,field2"
-- Right (ByClause [Subject "field1" "field1" [],Subject "field2" "field2" []])
byClauseParser :: Parser ByClause
byClauseParser = ByClause <$> (string "by" *> space *> sepBy pSubject (char ','))


-- >>> toQText $ ByClause [Subject "field1" "field1" [],Subject "field2" "field2" []]
-- "by field1,field2"
instance ToQueryText ByClause where
  toQText (ByClause subs) = "by " <> T.intercalate "," (map toQText subs)


-- StatsCommand [Count Nothing Nothing] (Just (ByClause [FieldName "field1"]))

-- | Combines the above parsers to parse the entire 'stats' command.
--
-- >>> parse pStatsSection "" "stats count by field1"
-- Right (StatsCommand [Plain (Subject "count" "count" []) Nothing] (Just (ByClause [Subject "field1" "field1" []])))
--
-- >>> parse pStatsSection "" "stats sum(field) as total,avg(field2)"
-- Right (StatsCommand [Sum (Subject "field" "field" []) (Just "total"),Avg (Subject "field2" "field2" []) Nothing] Nothing)
--
-- >>> parse pStatsSection "" "stats max(field) by field1,field2"
-- Right (StatsCommand [Max (Subject "field" "field" []) Nothing] (Just (ByClause [Subject "field1" "field1" [],Subject "field2" "field2" []])))
pStatsSection :: Parser Section
pStatsSection = do
  _ <- string "stats"
  space
  funcs <- sepBy aggFunctionParser (char ',')
  byClause <- optional $ try (space *> byClauseParser)
  return $ StatsCommand funcs byClause


instance ToQueryText Section where
  toQText (Source source) = toQText source
  toQText (Search expr) = toQText expr
  toQText (StatsCommand funcs byClauseM) = "stats " <> T.intercalate "," (map toQText funcs) <> maybeToMonoid (toQText <$> byClauseM)
  toQText (TimeChartCommand aggF byClauseM rollupM) = "timechart " <> toQText aggF <> maybeToMonoid (toQText <$> byClauseM) <> maybeToMonoid (toQText <$> rollupM)


-- TimeChartSection  (Count Nothing Nothing) (Just (ByClause [FieldName "field1"])) (Rollup "1w")
--

-- | parses the timechart command which is used to describe timeseries charts based off the request log data.
--
-- >>> parse pTimeChartSection "" "timechart count(*) by field1 [1d]"
-- Right (TimeChartCommand (Count (Subject "*" "*" []) Nothing) (Just (ByClause [Subject "field1" "field1" []])) (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart sum(field1) by field2,field3 [1d]"
-- Right (TimeChartCommand (Sum (Subject "field1" "field1" []) Nothing) (Just (ByClause [Subject "field2" "field2" [],Subject "field3" "field3" []])) (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart sum(field1) [1d]"
-- Right (TimeChartCommand (Sum (Subject "field1" "field1" []) Nothing) Nothing (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart count(*) [1d]"
-- Right (TimeChartCommand (Count (Subject "*" "*" []) Nothing) Nothing (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart [1d]"
-- Right (TimeChartCommand (Count (Subject "*" "*" []) Nothing) Nothing (Just (Rollup "1d")))
--
-- >>> parse pTimeChartSection "" "timechart"
-- Right (TimeChartCommand (Count (Subject "*" "*" []) Nothing) Nothing Nothing)
pTimeChartSection :: Parser Section
pTimeChartSection = do
  _ <- string "timechart"
  space
  agg <- (space *> aggFunctionParser <* space) `sepBy` string ","
  byClauseM <- optional $ try (space *> byClauseParser)
  rollupM <- optional $ try (space *> rollupParser)
  return $ TimeChartCommand (bool agg [Count (Subject "*" "*" []) Nothing] (null agg)) byClauseM rollupM


rollupParser :: Parser Rollup
rollupParser = Rollup . toText <$> (string "[" *> some alphaNumChar <* string "]")


instance ToQueryText Rollup where
  toQText (Rollup val) = "[" <> val <> "]"


----------------------------------------------------------------------
----

-- Example queries
-- request_body.v1.v2 == "abc" AND (request_body.v3.v4 == 123 OR request_body.v5[].v6==ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |

data Section
  = Search Expr
  | -- Define the AST for the 'stats' command
    StatsCommand [AggFunction] (Maybe ByClause)
  | TimeChartCommand [AggFunction] (Maybe ByClause) (Maybe Rollup)
  | Source Sources
  deriving stock (Show, Generic, Eq)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Example queries
-- request_body.v1.v2 = "abc" AND (request_body.v3.v4 = 123 OR request_body.v5[].v6=ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |
--

-- >>> parse pSection "" "traces"
-- Right (Source STraces)
--
-- >>> parse pSection "" "method==\"GET\""
-- Right (Search (Eq (Subject "method" "method" []) (Str "GET")))
--
-- >>> parse pSection "" "method==\"GET\" | traces"
-- Right (Search (Eq (Subject "method" "method" []) (Str "GET")))
--
-- >>> parse pSection "" "traces | method==\"GET\" "
-- Right (Source STraces)
--
-- >>> parse pSection "" " traces | method==\"GET\" "
-- Right (Source STraces)
--
pSection :: Parser Section
pSection = do
  _ <- space
  choice @[]
    [ pStatsSection
    , pTimeChartSection
    , Search <$> pExpr
    , Source <$> pSource
    ]


-- find what source to use when processing a query. By default, the requests source is used
--
-- >>> parse pSource "" "traces"
-- Right STraces
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
    [ SRequests <$ string "requests"
    , SLogs <$ string "logs"
    , STraces <$ string "traces"
    , SSpans <$ string "spans"
    , SMetrics <$ string "metrics"
    ]


instance ToQueryText Sources where
  toQText SRequests = "requests"
  toQText SLogs = "logs"
  toQText STraces = "traces"
  toQText SSpans = "spans"
  toQText SMetrics = "metrics"


instance Display Sources where
  displayPrec prec SRequests = "apis.request_dumps"
  displayPrec prec SLogs = "telemetry.logs"
  displayPrec prec STraces = "telemetry.traces"
  displayPrec prec SSpans = "otel_logs_and_spans"
  displayPrec prec SMetrics = "telemetry.metrics"


--- >>> parse parseQuery "" "method// = bla "
-- Right []
--
parseQuery :: Parser [Section]
parseQuery = sepBy pSection (space *> char '|' <* space)


instance ToQueryText [Section] where
  toQText secs = T.intercalate " | " $ map toQText secs
