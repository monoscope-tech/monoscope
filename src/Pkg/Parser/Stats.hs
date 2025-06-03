module Pkg.Parser.Stats (
  pTimeChartSection, 
  pStatsSection, 
  pSummarizeSection,
  pSortSection,
  pTakeSection,
  AggFunction (..), 
  Section (..), 
  Rollup (..), 
  ByClause (..),
  SummarizeByClause(..),
  BinFunction(..),
  SortField(..),
  Sources (..), 
  parseQuery, 
  pSource
) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayPrec)
import Pkg.Parser.Core
import Pkg.Parser.Expr (Expr, Subject (..), pExpr, pSubject)
import Relude hiding (Sum, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, space, space1, string)


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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Define an optional 'by' clause
newtype ByClause = ByClause [Subject] -- List of fields to group by
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


newtype Rollup = Rollup Text
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Added to support bin() and bin_auto() functions in by clause
data BinFunction 
  = Bin Subject Text
  | BinAuto Subject -- Will use fixed interval or legacy rollup calculation
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Support for summarize by with bin() function
data SummarizeByClause = SummarizeByClause [Either Subject BinFunction]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Sources = SSpans | SMetrics
  deriving stock (Eq, Generic, Show)
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
  toQText (SummarizeCommand funcs byClauseM) = 
    "summarize " <> T.intercalate "," (map toQText funcs) <> 
    maybeToMonoid (toQText <$> byClauseM)
  toQText (SortCommand fields) = "sort by " <> T.intercalate ", " (map toQText fields)
  toQText (TakeCommand limit) = "take " <> toText (show limit)


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

-- Sort field can include optional direction
data SortField = SortField Subject (Maybe Text) -- field and optional direction (asc/desc)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)

data Section
  = Search Expr
  | -- Define the AST for the 'stats' command
    StatsCommand [AggFunction] (Maybe ByClause)
  | TimeChartCommand [AggFunction] (Maybe ByClause) (Maybe Rollup)
  | SummarizeCommand [AggFunction] (Maybe SummarizeByClause)
  | SortCommand [SortField] -- sort by multiple fields
  | TakeCommand Int -- limit/take number of results
  | Source Sources
  deriving stock (Eq, Generic, Show)
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
      interval <- toText <$> some (alphaNumChar <|> char '.')
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
  displayPrec prec (Bin subj interval) = displayBuilder $ "time_bucket('" <> interval <> " seconds', " <> display subj <> ")"
  -- Use fixed 5min interval for now, can be enhanced later to use legacy rollup calculation
  displayPrec prec (BinAuto subj) = displayBuilder $ "time_bucket('5 minutes', " <> display subj <> ")"


-- | Parse a summarize by clause which can contain fields and bin functions
--
-- >>> parse pSummarizeByClause "" "by attributes.client, bin(timestamp, 60)"
-- Right (SummarizeByClause [Left (Subject "attributes.client" "attributes.client" []), Right (Bin (Subject "timestamp" "timestamp" []) "60")])
--
-- >>> parse pSummarizeByClause "" "by Computer, bin_auto(timestamp)"
-- Right (SummarizeByClause [Left (Subject "Computer" "Computer" []), Right (BinAuto (Subject "timestamp" "timestamp" []))])
--
-- >>> parse pSummarizeByClause "" "by bin_auto(timestamp)"
-- Right (SummarizeByClause [Right (BinAuto (Subject "timestamp" "timestamp" []))])
pSummarizeByClause :: Parser SummarizeByClause
pSummarizeByClause = do
  _ <- string "by"
  space
  fields <- sepBy (try (Right <$> pBinFunction) <|> (Left <$> pSubject)) (string "," <* space)
  return $ SummarizeByClause fields


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
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just "asc"), SortField (Subject "timestamp" "timestamp" []) (Just "desc")])
--
-- >>> parse pSortSection "" "order by parent_id asc"
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just "asc")])
pSortSection :: Parser Section
pSortSection = do
  _ <- (string "sort by" <|> string "order by")
  space
  fields <- sepBy pSortField (string "," <* space)
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
  _ <- (string "take" <|> string "limit")
  space
  limitStr <- some digitChar
  let limit = readMaybe (toString limitStr) :: Maybe Int
  return $ TakeCommand (fromMaybe 1000 limit) -- Default to 1000 if parsing fails


-- | Parse a named aggregation like 'TotalCount = count()'
--
-- >>> parse namedAggregation "" "TotalCount = count()"
-- Right (Count (Subject "*" "*" []) (Just "TotalCount"))
namedAggregation :: Parser AggFunction
namedAggregation = do
  name <- toText <$> some (alphaNumChar <|> oneOf "_")
  space
  _ <- string "="
  space
  func <- aggFunctionParser
  case func of
    Count sub _ -> return $ Count sub (Just name)
    Sum sub _ -> return $ Sum sub (Just name)
    Avg sub _ -> return $ Avg sub (Just name)
    Min sub _ -> return $ Min sub (Just name)
    Max sub _ -> return $ Max sub (Just name)
    P50 sub _ -> return $ P50 sub (Just name)
    P75 sub _ -> return $ P75 sub (Just name)
    P90 sub _ -> return $ P90 sub (Just name)
    P95 sub _ -> return $ P95 sub (Just name)
    P99 sub _ -> return $ P99 sub (Just name)
    P100 sub _ -> return $ P100 sub (Just name)
    Median sub _ -> return $ Median sub (Just name)
    Stdev sub _ -> return $ Stdev sub (Just name)
    Range sub _ -> return $ Range sub (Just name)
    Plain sub _ -> return $ Plain sub (Just name)


-- | Parse the summarize command
-- 
-- >>> parse pSummarizeSection "" "summarize sum(attributes.client) by attributes.client, bin(timestamp, 60)"
-- Right (SummarizeCommand [Sum (Subject "attributes.client" "attributes.client" []) Nothing] (Just (SummarizeByClause [Left (Subject "attributes.client" "attributes.client" []), Right (Bin (Subject "timestamp" "timestamp" []) "60")])))
--
-- >>> parse pSummarizeSection "" "summarize TotalCount = count() by Computer"
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
  return $ SummarizeCommand funcs byClause


pSection :: Parser Section
pSection = do
  _ <- space
  choice @[]
    [ pStatsSection
    , pTimeChartSection
    , pSummarizeSection
    , pSortSection
    , pTakeSection
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
    [ SSpans <$ string "spans"
    , SMetrics <$ string "metrics"
    ]


instance ToQueryText Sources where
  toQText SSpans = "spans"
  toQText SMetrics = "metrics"


instance Display Sources where
  displayPrec prec SSpans = "otel_logs_and_spans"
  displayPrec prec SMetrics = "telemetry.metrics"


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
