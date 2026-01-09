-- Parser implemented with help and code from: https://markkarpov.com/tutorial/megaparsec.html
module Pkg.Parser (queryASTToComponents, parseQueryToComponents, getProcessedColumns, fixedUTCTime, parseQuery, sectionsToComponents, defSqlQueryCfg, defPid, SqlQueryCfg (..), QueryComponents (..), NormalizedQuery (..), normalizeQuery, buildDateRange, buildGroupBy, buildOrderBy, buildLimit, buildWhereCondition, listToColNames, pSource, parseQueryToAST, ToQueryText (..), calculateAutoBinWidth) where

import Control.Error (hush)
import Data.Default (Default (def))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, diffUTCTime, secondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Pkg.Parser.Expr
import Pkg.Parser.Stats
import PyF (fmt)
import Relude
import Safe qualified
import Text.Megaparsec (errorBundlePretty, parse)


data QueryComponents = QueryComponents
  { whereClause :: Maybe Text
  , groupByClause :: [Text]
  , fromTable :: Maybe Text
  , select :: [Text] -- project: replaces default columns
  , extendSelect :: [Text] -- extend: appends to default columns
  , aggregations :: [Text] -- Summarize aggregations (separate from extended columns in select)
  , finalColumns :: [Text]
  , finalSqlQuery :: Text -- includes count(*) OVER() as last column
  , finalAlertQuery :: Maybe Text
  , finalSummarizeQuery :: Maybe Text -- For summarize query commands
  , sortFields :: Maybe [SortField] -- Fields to sort by
  , takeLimit :: Maybe Int -- Limit number of results
  , percentilesInfo :: Maybe (Text, [Double]) -- (field expr SQL, percentile values) extracted from AST
  , extendedColumns :: [(Text, Text)] -- Extended column mappings (alias -> full SQL expression without AS)
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)


-- | Normalized query with pre-computed SQL fragments ready for generation.
-- This is the intermediate representation between QueryComponents and final SQL.
data NormalizedQuery = NormalizedQuery
  { nqTable :: Text
  , nqSelectCols :: [Text]
  , nqAggregations :: [Text]
  , nqWhere :: Text -- "TRUE" or "(conditions)"
  , nqDateRange :: Text -- Date range clause
  , nqProjectId :: Text
  , nqGroupBy :: Text -- "" or "GROUP BY x, y"
  , nqOrderBy :: Text -- "" or "ORDER BY x DESC"
  , nqLimit :: Text -- "" or "LIMIT 500"
  , nqBinInterval :: Maybe Text
  , nqPercentiles :: Maybe (Text, [Double])
  , nqExtendedColumns :: [(Text, Text)]
  }
  deriving stock (Generic, Show)


-- | Build date range SQL clause from config
buildDateRange :: SqlQueryCfg -> Text
buildDateRange cfg =
  let fmtTime = toText . iso8601Show
      timestampCol = "timestamp"
   in case (cfg.dateRange, cfg.cursorM) of
        ((Nothing, Just b), Just cursor) -> timestampCol <> " <= '" <> fmtTime cursor <> "'"
        ((Just a, Just _), Just cursor) -> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime cursor <> "'"
        ((Just a, Nothing), Just cursor) -> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime cursor <> "'"
        ((Nothing, Just b), Nothing) -> timestampCol <> " <= '" <> fmtTime b <> "'"
        ((Just a, Nothing), Nothing) -> timestampCol <> " >= '" <> fmtTime a <> "'"
        ((Just a, Just b), Nothing) -> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime b <> "'"
        _ -> ""


-- | Build GROUP BY clause with resolved extended columns
buildGroupBy :: [(Text, Text)] -> [Text] -> Text
buildGroupBy extCols cols
  | null cols = ""
  | otherwise = let extColsMap = Map.fromList extCols in " GROUP BY " <> T.intercalate "," (map (resolveExtendedColumn extColsMap) cols)


-- | Build ORDER BY clause with fallback logic
buildOrderBy :: QueryComponents -> Text
buildOrderBy qc =
  let timestampCol = "timestamp"
   in case qc.sortFields of
        Just fields -> "ORDER BY " <> T.intercalate ", " (map displaySortField fields)
        Nothing -> case qc.finalSummarizeQuery of
          Just binInterval -> "ORDER BY time_bucket('" <> binInterval <> "', " <> timestampCol <> ") desc"
          Nothing
            | any (\s -> "time_bucket" `T.isInfixOf` s) qc.select ->
                "ORDER BY time_bucket('" <> defaultBinSize <> "', " <> timestampCol <> ") desc"
            | not (null qc.groupByClause) ->
                "ORDER BY " <> fromMaybe timestampCol (listToMaybe qc.groupByClause) <> " desc"
            | otherwise -> "ORDER BY " <> timestampCol <> " desc"


-- | Build LIMIT clause with defaults
buildLimit :: QueryComponents -> Text
buildLimit qc = case qc.takeLimit of
  Just limit -> "limit " <> toText (show limit)
  Nothing -> case qc.finalSummarizeQuery of
    Just _ -> "" -- No limit for summarize queries
    Nothing -> "limit 500"


-- | Build WHERE condition from raw clause
buildWhereCondition :: Maybe Text -> Text
buildWhereCondition Nothing = "TRUE"
buildWhereCondition (Just w) | T.null w = "TRUE"
buildWhereCondition (Just w) = "(" <> w <> ")"


-- | Normalize QueryComponents into NormalizedQuery for SQL generation
normalizeQuery :: SqlQueryCfg -> QueryComponents -> NormalizedQuery
normalizeQuery cfg qc =
  let (_, selVec) = getProcessedColumns cfg.projectedColsByUser cfg.defaultSelect
      baseCols = if null qc.select then selVec else qc.select
      selectedCols = baseCols <> qc.extendSelect
   in NormalizedQuery
        { nqTable = fromMaybe "otel_logs_and_spans" qc.fromTable
        , nqSelectCols = selectedCols
        , nqAggregations = qc.aggregations
        , nqWhere = buildWhereCondition qc.whereClause
        , nqDateRange = buildDateRange cfg
        , nqProjectId = cfg.pid.toText
        , nqGroupBy = buildGroupBy qc.extendedColumns qc.groupByClause
        , nqOrderBy = buildOrderBy qc
        , nqLimit = buildLimit qc
        , nqBinInterval = qc.finalSummarizeQuery
        , nqPercentiles = qc.percentilesInfo
        , nqExtendedColumns = qc.extendedColumns
        }


sectionsToComponents :: SqlQueryCfg -> [Section] -> QueryComponents
sectionsToComponents sqlCfg = foldl' (applySectionToComponent sqlCfg) (def :: QueryComponents)


-- | Resolve a column name to its full expression if it's an extended column (O(log n) lookup)
resolveExtendedColumn :: Map.Map Text Text -> Text -> Text
resolveExtendedColumn extColsMap colName = fromMaybe colName (Map.lookup colName extColsMap)


-- | Combine where clauses using AND
combineWhereClause :: Maybe Text -> Text -> Maybe Text
combineWhereClause Nothing new = Just new
combineWhereClause (Just existing) new = Just $ existing <> " AND " <> new


applySectionToComponent :: SqlQueryCfg -> QueryComponents -> Section -> QueryComponents
applySectionToComponent _ qc (Search expr) = qc{whereClause = combineWhereClause qc.whereClause (display expr)}
applySectionToComponent _ qc (WhereClause expr) = qc{whereClause = combineWhereClause qc.whereClause (display expr)}
applySectionToComponent _ qc (Source source) = qc{fromTable = Just $ display source}
applySectionToComponent sqlCfg qc (SummarizeCommand aggs byClauseM) =
  let pctInfo = extractPercentilesInfo [SummarizeCommand aggs byClauseM]
   in applySummarizeByClauseToQC sqlCfg byClauseM $ qc{aggregations = qc.aggregations <> map display aggs, percentilesInfo = pctInfo}
-- extend adds computed columns (appends to defaults) AND tracks mappings for GROUP BY resolution
applySectionToComponent _ qc (ExtendCommand cols) =
  let colMappings = [(name, stripAlias $ display expr) | (name, expr) <- cols]
      extendCols = map (display . snd) cols
   in qc{extendedColumns = qc.extendedColumns <> colMappings, extendSelect = qc.extendSelect <> extendCols}
  where
    stripAlias expr = let (pre, post) = T.breakOn " AS " expr in if T.null post then expr else pre
-- project replaces select with only the specified columns
applySectionToComponent _ qc (ProjectCommand cols) = qc{select = map (display . snd) cols}
applySectionToComponent _ qc (SortCommand sortFields) = qc{sortFields = Just sortFields}
applySectionToComponent _ qc (TakeCommand limit) = qc{takeLimit = Just limit}


-- | Apply summarize by clause to query components
applySummarizeByClauseToQC :: SqlQueryCfg -> Maybe SummarizeByClause -> QueryComponents -> QueryComponents
applySummarizeByClauseToQC _ Nothing qc = qc
applySummarizeByClauseToQC sqlCfg (Just (SummarizeByClause items)) qc =
  let binFields = [b | ByBinFunc b <- items]
      regularFields = [item | item <- items, not (isBinFunc item)]
      isBinFunc (ByBinFunc _) = True
      isBinFunc _ = False
      hasBinFuncs = not (null binFields)
      binInterval = case listToMaybe binFields of
        Just (Bin _ interval) -> kqlTimespanToTimeBucket interval
        Just (BinAuto _) -> calculateAutoBinWidth sqlCfg.dateRange sqlCfg.currentTime
        Nothing -> defaultBinSize
      groupByClauses = map display regularFields
   in qc
        { finalSummarizeQuery = if hasBinFuncs then Just binInterval else Nothing
        , groupByClause = qc.groupByClause <> groupByClauses
        }


-- | Display a sort field for SQL generation
displaySortField :: SortField -> Text
displaySortField (SortField field Nothing) = display field
displaySortField (SortField field (Just dir)) = display field <> " " <> dir


-- Legacy clause application functions removed

----------------------------------------------------------------------------------

data SqlQueryCfg = SqlQueryCfg
  { pid :: Projects.ProjectId
  , presetRollup :: Maybe Text
  , dateRange :: (Maybe UTCTime, Maybe UTCTime)
  , cursorM :: Maybe UTCTime
  , projectedColsByUser :: [Text] -- cols selected explicitly by user
  , currentTime :: UTCTime
  , defaultSelect :: [Text]
  , source :: Maybe Sources
  , targetSpansM :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)


normalizeKeyPath :: Text -> Text
normalizeKeyPath txt = T.toLower $ T.replace "]" "❳" $ T.replace "[" "❲" $ T.replace "." "•" txt


getProcessedColumns :: [Text] -> [Text] -> (Text, [Text])
getProcessedColumns cols defaultSelect = (T.intercalate "," $ colsNoAsClause selectedCols, selectedCols)
  where
    prs =
      cols & mapMaybe \col -> do
        subJ@(Subject entire _ _) <- hush (parse pSubject "" col)
        pure $ display subJ <> " as " <> normalizeKeyPath entire
    selectedCols = prs <> defaultSelect


sqlFromQueryComponents :: SqlQueryCfg -> QueryComponents -> (Text, QueryComponents)
sqlFromQueryComponents sqlCfg qc =
  let
    -- Use normalized query for pre-computed values
    nq = normalizeQuery sqlCfg qc
    timestampCol = "timestamp"

    -- Process columns for summary JSON conversion
    processedCols = map (\col -> if "summary" == col || "summary" `T.isSuffixOf` col then "to_json(summary)" else col) $ colsNoAsClause nq.nqSelectCols
    selectClause = T.intercalate "," processedCols

    -- Use pre-computed values from NormalizedQuery
    fromTable = nq.nqTable
    groupByClause = nq.nqGroupBy
    sortOrder = nq.nqOrderBy
    limitClause = nq.nqLimit
    whereCondition = nq.nqWhere
    dateRangeStr = nq.nqDateRange

    -- Build complete WHERE clause for data queries
    buildWhere = T.intercalate " and " $ filter (not . T.null) ["project_id='" <> nq.nqProjectId <> "'", dateRangeStr, "(" <> whereCondition <> ")"]

    -- All queries include count(*) OVER() as last column for total count
    countOver = "count(*) OVER() as _total_count" :: Text
    finalSqlQuery = case sqlCfg.targetSpansM of
      Just "service-entry-spans" ->
        [fmt|WITH ranked_spans AS (SELECT *, resource->'service'->>'name' AS service_name,
                ROW_NUMBER() OVER (PARTITION BY trace_id, resource->'service'->>'name' ORDER BY start_time) AS rn
                FROM otel_logs_and_spans where {buildWhere}
                {groupByClause}
                )
               SELECT {selectClause}, {countOver} FROM ranked_spans
                  WHERE rn = 1 {sortOrder} {limitClause} |]
      _ ->
        case qc.finalSummarizeQuery of
          Just binInterval ->
            let timeBucketExpr = "time_bucket('" <> binInterval <> "', " <> timestampCol <> ")"
                -- Use aggregations if available (from summarize), otherwise fall back to select
                cols = if null qc.aggregations then qc.select else qc.aggregations
                selectCols = T.intercalate "," $ filter (not . T.isInfixOf "time_bucket") cols
                -- Only add comma separator if there are select columns
                selectPart = if T.null selectCols then "" else selectCols <> ", "
             in [fmt|SELECT extract(epoch from {timeBucketExpr})::integer, {selectPart}{countOver}
                   FROM {fromTable}
                   WHERE {buildWhere}
                   GROUP BY {timeBucketExpr}
                   ORDER BY {timeBucketExpr} DESC
                   {limitClause} |]
          Nothing ->
            -- For non-bin summarize queries with aggregations, use aggregations instead of default columns
            let useAggregations = not (null qc.aggregations) && not (null qc.groupByClause)
                selectPart = if useAggregations then T.intercalate "," qc.aggregations else selectClause
             in [fmt|SELECT {selectPart}, {countOver} FROM {fromTable}
                 WHERE {buildWhere}
                 {groupByClause} {sortOrder} {limitClause} |]

    -- Generate the summarize query depending on bin functions and data type
    -- Percentiles info is extracted directly from AST in applySectionToComponent
    -- The fieldExpr comes from parser-validated SubjectExpr and is safe for SQL interpolation
    summarizeQuery =
      case qc.finalSummarizeQuery of
        Just binInterval ->
          case qc.percentilesInfo of
            Just (fieldExpr, pcts) ->
              -- Generate LATERAL unnest query for percentiles with custom percentile values
              let timeBucketExpr = "time_bucket('" <> binInterval <> "', " <> timestampCol <> ")"
                  -- Generate ARRAY of percentile expressions (fieldExpr already includes any division)
                  pctExprs =
                    T.intercalate
                      ",\n                          "
                      [ "COALESCE((approx_percentile(" <> show (p / 100.0) <> ", percentile_agg((" <> fieldExpr <> ")::float)))::float, 0)"
                      | p <- pcts
                      ]
                  -- Generate ARRAY of percentile labels
                  pctLabels = T.intercalate "," ["'p" <> show (round p :: Int) <> "'" | p <- pcts]
               in [fmt|SELECT timeB, quantile, value FROM (
                      SELECT extract(epoch from {timeBucketExpr})::integer AS timeB,
                        ARRAY[{pctExprs}] AS values,
                        ARRAY[{pctLabels}] AS quantiles
                      FROM {fromTable}
                      WHERE {buildWhere}
                      GROUP BY timeB
                      HAVING COUNT(*) > 0
                    ) s, LATERAL unnest(s.values, s.quantiles) AS u(value, quantile)
                    WHERE value IS NOT NULL
                    ORDER BY timeB DESC {limitClause}|]
            Nothing ->
              -- Normal summarize query
              let resolve = resolveExtendedColumn (Map.fromList qc.extendedColumns)
                  groupCol =
                    if null qc.groupByClause
                      then "'" <> (if null qc.aggregations then "count" else "value") <> "'"
                      else "COALESCE(" <> maybe "status_code" resolve (listToMaybe qc.groupByClause) <> "::text, 'null')"
                  aggCol = if null qc.aggregations then "count(*)::float" else fromMaybe "count(*)::float" (listToMaybe qc.aggregations)
                  timeBucketExpr = "time_bucket('" <> binInterval <> "', " <> timestampCol <> ")"
                  firstGroupCol = resolve $ fromMaybe "status_code" (listToMaybe qc.groupByClause)
                  groupByPart = if null qc.groupByClause then timeBucketExpr else timeBucketExpr <> ", " <> firstGroupCol
               in [fmt|SELECT extract(epoch from {timeBucketExpr})::integer, {groupCol}, {aggCol}
                      FROM {fromTable} WHERE {buildWhere} GROUP BY {groupByPart}
                      ORDER BY {timeBucketExpr} DESC {limitClause}|]
        Nothing ->
          let hasAggregationsNoGroupBy = not (null qc.aggregations) && null qc.groupByClause
              orderCol = if null qc.groupByClause then timestampCol else fromMaybe timestampCol (listToMaybe qc.groupByClause)
              selectCols = if null qc.aggregations then qc.select else qc.aggregations
              orderClause = if hasAggregationsNoGroupBy then "" else " ORDER BY " <> orderCol <> " DESC"
              limitPart = if hasAggregationsNoGroupBy then "" else limitClause
           in [fmt|SELECT {T.intercalate "," selectCols} FROM {fromTable} WHERE {buildWhere}
              {groupByClause}{orderClause} {limitPart}|]

    -- FIXME: render this based on the aggregations, but without the aliases
    alertSelect = [fmt| count(*)::integer|] :: Text
    resolvedGroupByCols = let extColsMap = Map.fromList nq.nqExtendedColumns in map (resolveExtendedColumn extColsMap) qc.groupByClause
    alertGroupByClause = if null qc.groupByClause then "" else " GROUP BY " <> T.intercalate "," resolvedGroupByCols
    -- For alert queries, we use a simplified whereCondition without the time range/cursor
    alertWhereCondition = nq.nqWhere

    alertQuery =
      case qc.finalSummarizeQuery of
        Just binInterval ->
          -- For queries with bin functions, create a time-bucketed alert query
          let
            -- Create time bucket expression
            timeBucketExpr = "time_bucket('" <> binInterval <> "', " <> timestampCol <> ")"
           in
            [fmt|
                SELECT GREATEST({alertSelect}) FROM {fromTable}
                WHERE project_id='{sqlCfg.pid.toText}' and ({alertWhereCondition})
                GROUP BY {timeBucketExpr}
              |]
        Nothing ->
          -- For regular summarize queries without time buckets
          [fmt|
              SELECT GREATEST({alertSelect}) FROM {fromTable}
              WHERE project_id='{sqlCfg.pid.toText}' and ({alertWhereCondition})
              {alertGroupByClause}
            |]
   in
    ( finalSqlQuery
    , qc
        { finalColumns = listToColNames nq.nqSelectCols
        , finalSqlQuery = finalSqlQuery
        , whereClause = Just whereCondition
        , finalSummarizeQuery = Just summarizeQuery
        , finalAlertQuery = Just alertQuery
        }
    )


----------------------------------------------------------------------------------
-- parseQueryToComponents converts a monoscope query to components which can be executed directly against a database
----------------------------------------------------------------------------------
-- >>> Right (q, cmp) = parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "request_body.message!=\"blabla\" AND method==\"GET\""
-- >>> q
-- "SELECT json_build_array(id::text,to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS.US\"Z\"'),request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),LEFT(\n        CONCAT(\n            'url=', COALESCE(raw_url, 'null'),\n            ' response_body=', COALESCE(response_body, 'null'),\n            ' request_body=', COALESCE(request_body, 'null') \n        ),\n        255\n    )) FROM  \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and ( created_at > NOW() - interval '14 days' \n             AND (request_body->>'message'!='blabla' AND method='GET') )\n           ORDER BY created_at desc limit 200 "
--
-- >>> Right (q2, cmp2) = parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "request_body.message!=\"blabla\" AND method==\"GET\""
-- >>> q2
-- "SELECT json_build_array(id::text,to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS.US\"Z\"'),request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),LEFT(\n        CONCAT(\n            'url=', COALESCE(raw_url, 'null'),\n            ' response_body=', COALESCE(response_body, 'null'),\n            ' request_body=', COALESCE(request_body, 'null') \n        ),\n        255\n    )) FROM  \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and ( created_at > NOW() - interval '14 days' \n             AND (request_body->>'message'!='blabla' AND method='GET') )\n           ORDER BY created_at desc limit 200 "
--
-- >>> Right (q3, cmp3) = parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "errors[*].error_type==[]"
-- >>> cmp3.whereClause
-- Just "jsonb_path_exists(errors, $$$[*].\"error_type\" ? (@ == {} )$$::jsonpath)"
--
-- >>> Right (q4, cmp4) = parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "errors[*].error_type=~/^ab.*c/"
-- >>> cmp4.whereClause
-- Just "jsonb_path_exists(errors, $$$[*].\"error_type\" ? (@ = \"^ab.*c\")$$::jsonpath)"
--
parseQueryToComponents :: SqlQueryCfg -> Text -> Either Text (Text, QueryComponents)
parseQueryToComponents sqlCfg q = bimap (toText . errorBundlePretty) (queryASTToComponents sqlCfg) (parse parseQuery "" (T.strip q))


queryASTToComponents :: SqlQueryCfg -> [Section] -> (Text, QueryComponents)
queryASTToComponents sqlCfg = sqlFromQueryComponents sqlCfg . sectionsToComponents sqlCfg


parseQueryToAST :: Text -> Either Text [Section]
parseQueryToAST q = first (toText . errorBundlePretty) (parse parseQuery "" (T.strip q))


defPid :: Projects.ProjectId
defPid = def :: Projects.ProjectId


-- Only for tests. and harrd coding
fixedUTCTime :: UTCTime
fixedUTCTime = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)


-- | Calculate automatic bin width based on date range duration
-- Rules:
-- - Less than 1 hour: 1 minute bins
-- - 1-6 hours: 5 minute bins
-- - 6-24 hours: 10 minute bins
-- - 1-7 days: 1 hour bins
-- - 7-30 days: 6 hour bins
-- - More than 30 days: 1 day bins
calculateAutoBinWidth :: (Maybe UTCTime, Maybe UTCTime) -> UTCTime -> Text
calculateAutoBinWidth (Just startTime, Just endTime) _ =
  let duration = diffUTCTime endTime startTime
      seconds = realToFrac duration :: Double
      minutes = seconds / 60
      hours = minutes / 60
      days = hours / 24
   in case () of
        _
          | minutes <= 2 -> "1 second"
          | minutes <= 5 -> "5 seconds"
          | minutes <= 15 -> "10 seconds"
          | hours <= 1 -> "30 seconds"
          | hours <= 6 -> "1 minutes"
          | hours <= 14 -> "5 minutes"
          | hours <= 48 -> "10 minutes"
          | days < 7 -> "1 hour"
          | days < 30 -> "6 hours"
          | otherwise -> "1 day"
calculateAutoBinWidth (Nothing, Just endTime) currentTime =
  -- If no start time, assume 14 days ago (same as default date range)
  let startTime = addUTCTime (-(14 * 24 * 60 * 60)) currentTime
   in calculateAutoBinWidth (Just startTime, Just endTime) currentTime
calculateAutoBinWidth (Just startTime, Nothing) currentTime =
  -- If no end time, use current time
  calculateAutoBinWidth (Just startTime, Just currentTime) currentTime
calculateAutoBinWidth (Nothing, Nothing) currentTime =
  -- Default to 14 days range if no date range specified
  let startTime = addUTCTime (-(14 * 24 * 60 * 60)) currentTime
   in calculateAutoBinWidth (Just startTime, Just currentTime) currentTime


defSqlQueryCfg :: Projects.ProjectId -> UTCTime -> Maybe Sources -> Maybe Text -> SqlQueryCfg
defSqlQueryCfg pid currentTime source spanT =
  SqlQueryCfg
    { pid = pid
    , presetRollup = Nothing
    , cursorM = Nothing
    , dateRange = (Nothing, Nothing)
    , source = source
    , targetSpansM = spanT
    , projectedColsByUser = []
    , currentTime
    , defaultSelect = defaultSelectSqlQuery source
    }


timestampLogFmt :: Text -> Text
timestampLogFmt colName =
  [fmt|CASE 
        WHEN RIGHT(TRIM('"' FROM CAST(to_json({colName} at time zone 'UTC') AS VARCHAR)), 1) = 'Z'
        THEN TRIM('"' FROM CAST(to_json({colName} at time zone 'UTC') AS VARCHAR))
        ELSE TRIM('"' FROM CAST(to_json({colName} at time zone 'UTC') AS VARCHAR)) || 'Z'
    END as {colName}|]


defaultSelectSqlQuery :: Maybe Sources -> [Text]
defaultSelectSqlQuery (Just SMetrics) = ["id"]
defaultSelectSqlQuery Nothing = defaultSelectSqlQuery (Just SSpans)
defaultSelectSqlQuery (Just SSpans) =
  [ "id"
  , timestampLogFmt "timestamp"
  , "context___trace_id as trace_id"
  , "name as span_name"
  , "duration"
  , "resource___service___name as service"
  , "parent_id as parent_span_id"
  , "CAST(EXTRACT(EPOCH FROM (start_time)) * 1000000000 AS BIGINT) as start_time_ns"
  , "errors is not null as errors"
  , "summary"
  , "context___span_id as latency_breakdown"
  ]


-- >>> listToColNames ["id", "JSONB_ARRAY_LENGTH(errors) as errors_count"]
-- ["id","errors_count"]
listToColNames :: [Text] -> [Text]
listToColNames = map \x -> T.strip $ last $ "" :| T.splitOn "as" x


-- >>> colsNoAsClause ["id", "JSONB_ARRAY_LENGTH(errors) as errors_count"]
-- ["id","JSONB_ARRAY_LENGTH(errors)"]
colsNoAsClause :: [Text] -> [Text]
colsNoAsClause = mapMaybe (\x -> Safe.headMay $ T.strip <$> T.splitOn "as" x)


instance HasField "toColNames" QueryComponents [Text] where
  getField qc = qc.finalColumns
