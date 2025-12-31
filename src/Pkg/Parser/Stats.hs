module Pkg.Parser.Stats (
  pSummarizeSection,
  pSortSection,
  pTakeSection,
  AggFunction (..),
  Section (..),
  SummarizeByClause (..),
  BinFunction (..),
  SortField (..),
  Sources (..),
  parseQuery,
  pSource,
  defaultBinSize,
  aggFunctionParser,
  pBinFunction,
  namedAggregation,
  pSummarizeByClause,
  pSortField,
  extractPercentilesInfo,
) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayPrec)
import Pkg.Parser.Expr (Expr, Parser, Subject (..), ToQueryText (..), kqlTimespanToTimeBucket, pExpr, pSubject)
import Relude hiding (Sum, some)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, space, string)
import Text.Megaparsec.Char.Lexer qualified as L


-- $setup
-- >>> import Text.Megaparsec (parse)
-- >>> import Pkg.Parser.Expr (FieldKey(..))
-- >>> :set -XOverloadedStrings


-- Default bin size for auto binning if not otherwise specified
defaultBinSize :: Text
defaultBinSize = "5 minutes"


-- | Simple arithmetic expression for percentile functions
-- Supports: field, field / number, field * number
data SubjectExpr = SubjectExpr Subject (Maybe (Text, Double))
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)

-- | Display a SubjectExpr as SQL
subjectExprToSQL :: SubjectExpr -> Text
subjectExprToSQL (SubjectExpr sub Nothing) = display sub
subjectExprToSQL (SubjectExpr sub (Just (op, val))) = "(" <> display sub <> " " <> op <> " " <> show val <> ")"

-- Modify Aggregation Functions to include optional aliases
data AggFunction
  = Count Subject (Maybe Text) -- Optional field and alias
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
    Plain Subject (Maybe Text)
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
pPercentileSingle = do
  _ <- string "percentile("
  subj <- pSubjectExpr
  _ <- string ","
  space
  pct <- pNumericValue
  _ <- string ")"
  pure $ Percentile subj pct Nothing

-- | Parse percentiles(expr, p1, p2, ...) - multiple percentiles (Microsoft KQL syntax)
-- >>> parse pPercentilesMulti "" "percentiles(duration, 50, 75, 90, 95)"
-- Right (Percentiles (SubjectExpr (Subject "duration" "duration" []) Nothing) [50.0,75.0,90.0,95.0] Nothing)
-- >>> parse pPercentilesMulti "" "percentiles(duration / 1e6, 50, 75, 90, 95)"
-- Right (Percentiles (SubjectExpr (Subject "duration" "duration" []) (Just ("/",1000000.0))) [50.0,75.0,90.0,95.0] Nothing)
pPercentilesMulti :: Parser AggFunction
pPercentilesMulti = do
  _ <- string "percentiles("
  subj <- pSubjectExpr
  _ <- string ","
  space
  pcts <- pNumericValue `sepBy1` (string "," <* space)
  _ <- string ")"
  pure $ Percentiles subj pcts Nothing

aggFunctionParser :: Parser AggFunction
aggFunctionParser =
  choice @[]
    [ Sum <$> (string "sum(" *> pSubject <* string ")") <*> pure Nothing
    , try pPercentilesMulti
    , try pPercentileSingle
    , P50 <$> (string "p50(" *> pSubject <* string ")") <*> pure Nothing
    , P75 <$> (string "p75(" *> pSubject <* string ")") <*> pure Nothing
    , P90 <$> (string "p90(" *> pSubject <* string ")") <*> pure Nothing
    , P95 <$> (string "p95(" *> pSubject <* string ")") <*> pure Nothing
    , P99 <$> (string "p99(" *> pSubject <* string ")") <*> pure Nothing
    , P100 <$> (string "p100(" *> pSubject <* string ")") <*> pure Nothing
    , Count <$> (string "count(" *> (pSubject <|> (string ")" $> Subject "*" "*" [])) <* optional (string ")")) <*> pure Nothing
    , Avg <$> (string "avg(" *> pSubject <* string ")") <*> pure Nothing
    , Min <$> (string "min(" *> pSubject <* string ")") <*> pure Nothing
    , Max <$> (string "max(" *> pSubject <* string ")") <*> pure Nothing
    , Median <$> (string "median(" *> pSubject <* string ")") <*> pure Nothing
    , Stdev <$> (string "stdev(" *> pSubject <* string ")") <*> pure Nothing
    , Range <$> (string "range(" *> pSubject <* string ")") <*> pure Nothing
    , Plain <$> pSubject <*> pure Nothing
    ]


instance ToQueryText AggFunction where
  toQText = display


instance ToQueryText [AggFunction] where
  toQText xs = T.intercalate "," $ map toQText xs


instance Display AggFunction where
  displayPrec _prec (Count sub _alias) = displayBuilder $ "count(" <> display sub <> ")"
  displayPrec _prec (P50 sub _alias) = displayBuilder $ "approx_percentile(0.50, percentile_agg((" <> display sub <> ")::float))::int"
  displayPrec _prec (P75 sub _alias) = displayBuilder $ "approx_percentile(0.75, percentile_agg((" <> display sub <> ")::float))::int"
  displayPrec _prec (P90 sub _alias) = displayBuilder $ "approx_percentile(0.90, percentile_agg((" <> display sub <> ")::float))::int"
  displayPrec _prec (P95 sub _alias) = displayBuilder $ "approx_percentile(0.95, percentile_agg((" <> display sub <> ")::float))::int"
  displayPrec _prec (P99 sub _alias) = displayBuilder $ "approx_percentile(0.99, percentile_agg((" <> display sub <> ")::float))::int"
  displayPrec _prec (P100 sub _alias) = displayBuilder $ "approx_percentile(1, percentile_agg((" <> display sub <> ")::float))::int"
  -- Encode percentile(s) info as marker for Parser.hs to generate LATERAL unnest SQL
  displayPrec _prec (Percentile subExpr pct _alias) =
    displayBuilder $ "__PERCENTILES__" <> subjectExprToSQL subExpr <> "__PCTS__" <> show pct <> "__END__"
  displayPrec _prec (Percentiles subExpr pcts _alias) =
    displayBuilder $ "__PERCENTILES__" <> subjectExprToSQL subExpr <> "__PCTS__" <> T.intercalate "," (map show pcts) <> "__END__"
  displayPrec _prec (Sum sub _alias) = displayBuilder $ "sum((" <> display sub <> ")::float)"
  displayPrec _prec (Avg sub _alias) = displayBuilder $ "avg((" <> display sub <> ")::float)"
  displayPrec _prec (Min sub _alias) = displayBuilder $ "min((" <> display sub <> ")::float)"
  displayPrec _prec (Max sub _alias) = displayBuilder $ "max((" <> display sub <> ")::float)"
  displayPrec _prec (Median sub _alias) = displayBuilder $ "median((" <> display sub <> ")::float)"
  displayPrec _prec (Stdev sub _alias) = displayBuilder $ "stdev((" <> display sub <> ")::float)"
  displayPrec _prec (Range sub _alias) = displayBuilder $ "range((" <> display sub <> ")::float)"
  displayPrec _prec (Plain sub _alias) = displayBuilder $ "" <> display sub <> ""


instance ToQueryText Section where
  toQText (Source source) = toQText source
  toQText (Search expr) = toQText expr
  toQText (WhereClause expr) = "where " <> toQText expr
  toQText (SummarizeCommand funcs byClauseM) =
    "summarize "
      <> T.intercalate "," (map toQText funcs)
      <> maybeToMonoid (toQText <$> byClauseM)
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
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just "asc"),SortField (Subject "timestamp" "timestamp" []) (Just "desc")])
--
-- >>> parse pSortSection "" "order by parent_id asc"
-- Right (SortCommand [SortField (Subject "parent_id" "parent_id" []) (Just "asc")])
pSortSection :: Parser Section
pSortSection = do
  _ <- string "sort by" <|> string "order by"
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
  _ <- string "take" <|> string "limit"
  space
  limitStr <- some digitChar
  let limit = readMaybe (toString limitStr) :: Maybe Int
  return $ TakeCommand (fromMaybe 1000 limit) -- Default to 1000 if parsing fails


-- | Parse a named aggregation like 'TotalCount = count()'
--
-- >>> parse namedAggregation "" "TotalCount=count()"
-- Right (Count (Subject "*" "*" []) (Just "TotalCount"))
namedAggregation :: Parser AggFunction
namedAggregation = do
  name <- toText <$> some (alphaNumChar <|> oneOf ("_" :: String))
  _ <- string "="
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
    Percentile subExpr pct _ -> return $ Percentile subExpr pct (Just name)
    Percentiles subExpr pcts _ -> return $ Percentiles subExpr pcts (Just name)
    Median sub _ -> return $ Median sub (Just name)
    Stdev sub _ -> return $ Stdev sub (Just name)
    Range sub _ -> return $ Range sub (Just name)
    Plain sub _ -> return $ Plain sub (Just name)


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
    go (SummarizeCommand aggs _) acc = case findPercentiles aggs of
      Just info -> Just info
      Nothing -> acc
    go _ acc = acc
    findPercentiles [] = Nothing
    findPercentiles (Percentile subExpr pct _ : _) = Just (subjectExprToSQL subExpr, [pct])
    findPercentiles (Percentiles subExpr pcts _ : _) = Just (subjectExprToSQL subExpr, pcts)
    findPercentiles (_ : rest) = findPercentiles rest
