-- Parser implemented with help and code from: https://markkarpov.com/tutorial/megaparsec.html
module Pkg.Parser (queryASTToComponents, parseQueryToComponents, getProcessedColumns, fixedUTCTime, parseQuery, sectionsToComponents, defSqlQueryCfg, defPid, SqlQueryCfg (..), QueryComponents (..), listToColNames, pSource, parseQueryToAST, ToQueryText (..), calculateAutoBinWidth) where

import Control.Error (hush)
import Data.Default (Default (def))
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
import Text.Read (readMaybe)


data QueryComponents = QueryComponents
  { whereClause :: Maybe Text
  , groupByClause :: [Text]
  , fromTable :: Maybe Text
  , select :: [Text]
  , finalColumns :: [Text]
  , -- A query which can be run to retrieve the count
    countQuery :: Text
  , -- final generated sql query
    finalSqlQuery :: Text
  , finalAlertQuery :: Maybe Text
  , finalSummarizeQuery :: Maybe Text -- For summarize query commands
  , sortFields :: Maybe [SortField] -- Fields to sort by
  , takeLimit :: Maybe Int -- Limit number of results
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)


sectionsToComponents :: SqlQueryCfg -> [Section] -> QueryComponents
sectionsToComponents sqlCfg = foldl' (applySectionToComponent sqlCfg) (def :: QueryComponents)


applySectionToComponent :: SqlQueryCfg -> QueryComponents -> Section -> QueryComponents
applySectionToComponent _ qc (Search expr) = qc{whereClause = Just $ display expr}
applySectionToComponent _ qc (WhereClause expr) = qc{whereClause = Just $ display expr} -- Handle where clause same as Search
applySectionToComponent _ qc (Source source) = qc{fromTable = Just $ display source}
applySectionToComponent sqlCfg qc (SummarizeCommand aggs byClauseM) = applySummarizeByClauseToQC sqlCfg byClauseM $ qc{select = qc.select <> map display aggs}
applySectionToComponent _ qc (SortCommand sortFields) = qc{sortFields = Just sortFields}
applySectionToComponent _ qc (TakeCommand limit) = qc{takeLimit = Just limit}


-- | Apply summarize by clause to query components
applySummarizeByClauseToQC :: SqlQueryCfg -> Maybe SummarizeByClause -> QueryComponents -> QueryComponents
applySummarizeByClauseToQC _ Nothing qc = qc
applySummarizeByClauseToQC sqlCfg (Just (SummarizeByClause fields)) qc =
  let (regularFields, binFields) = partitionEithers fields
      -- Extract bin functions for special handling
      hasBinFuncs = not (null binFields)
      -- Extract the bin interval from the first bin function, or use auto bin calculation
      binInterval = case listToMaybe binFields of
        Just (Bin _ interval) -> kqlTimespanToTimeBucket interval
        Just (BinAuto _) -> calculateAutoBinWidth sqlCfg.dateRange sqlCfg.currentTime
        Nothing -> defaultBinSize
      -- Only add regular fields to GROUP BY
      groupByClauses = map display regularFields
   in qc
        { -- Store bin interval for SQL generation when bin functions are present
          finalSummarizeQuery = if hasBinFuncs then Just binInterval else Nothing
        , -- Add regular fields to GROUP BY
          groupByClause = qc.groupByClause <> groupByClauses
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
  let fmtTime = toText . iso8601Show
      -- Use the table from query components if available, otherwise default to otel_logs_and_spans
      fromTable = fromMaybe "otel_logs_and_spans" qc.fromTable
      timestampCol = "timestamp"

      -- Note: cursorT is now handled in the whereCondition computation
      -- Handle the Either error case correctly not hushing it.
      (_, selVec) = getProcessedColumns sqlCfg.projectedColsByUser sqlCfg.defaultSelect
      selectedCols = if null qc.select then selVec else qc.select
      -- When building json_build_array, we need to handle TEXT[] columns specially
      -- Process each column to convert arrays to JSON
      processedCols =
        map
          ( \col ->
              if "summary" == col || "summary" `T.isSuffixOf` col
                then "to_json(summary)"
                else col
          )
          $ colsNoAsClause selectedCols
      selectClause = T.intercalate "," processedCols

      -- Extract the raw where clause without the AND prefix
      rawWhere = fromMaybe "" qc.whereClause
      -- Use raw where with parentheses but NO AND prefix
      whereClause =
        if T.null rawWhere
          then ""
          else "(" <> rawWhere <> ")"
      groupByClause = if null qc.groupByClause then "" else " GROUP BY " <> T.intercalate "," qc.groupByClause

      -- Date range for data queries (with cursor for pagination)
      -- When cursor is present, use it as the upper bound for data queries
      dateRangeStr = case (sqlCfg.dateRange, sqlCfg.cursorM) of
        -- Cursor modifies the end time for pagination
        ((Nothing, Just b), Just cursor) -> timestampCol <> " <= '" <> fmtTime cursor <> "'"
        ((Just a, Just b), Just cursor) -> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime cursor <> "'"
        ((Just a, Nothing), Just cursor) -> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime cursor <> "'"
        -- No cursor - use the original date range
        ((Nothing, Just b), Nothing) -> timestampCol <> " <= '" <> fmtTime b <> "'"
        ((Just a, Nothing), Nothing) -> timestampCol <> " >= '" <> fmtTime a <> "'"
        ((Just a, Just b), Nothing) -> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime b <> "'"
        _ -> ""

      -- Date range for count queries (never uses cursor)
      -- Count should always reflect the full user-selected time range
      dateRangeStrForCount = case sqlCfg.dateRange of
        (Nothing, Just b) -> timestampCol <> " <= '" <> fmtTime b <> "'"
        (Just a, Nothing) -> timestampCol <> " >= '" <> fmtTime a <> "'"
        (Just a, Just b) -> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime b <> "'"
        _ -> ""

      -- Time-based calculations removed with timechart/stats support

      -- Handle sort order
      sortOrder = case qc.sortFields of
        Just fields -> "ORDER BY " <> T.intercalate ", " (map displaySortField fields)
        Nothing ->
          -- If there's a bin function on timestamp, order by that instead of raw timestamp
          case qc.finalSummarizeQuery of
            Just binInterval -> "ORDER BY " <> "time_bucket('" <> binInterval <> "', " <> timestampCol <> ") desc"
            Nothing ->
              if any (\s -> "time_bucket" `T.isInfixOf` s) qc.select
                then "ORDER BY " <> "time_bucket('" <> defaultBinSize <> "', " <> timestampCol <> ") desc"
                else "ORDER BY " <> timestampCol <> " desc"

      -- Handle limit - only apply for non-summarize queries by default
      limitClause = case qc.takeLimit of
        Just limit -> "limit " <> toText (show limit)
        Nothing ->
          -- For summarize queries, don't apply a default limit to ensure we get complete data
          case qc.finalSummarizeQuery of
            Just _ -> "" -- No limit for summarize queries unless explicitly specified
            Nothing -> "limit 500"

      -- Create WHERE clause with user query only (cursor and timestamp range handled in dateRangeStr)
      whereCondition =
        let whereEmpty = T.null rawWhere
         in if whereEmpty then "TRUE" else whereClause

      -- Build complete WHERE clause for data queries (with cursor)
      buildWhere = T.intercalate " and " $ filter (not . T.null) ["project_id='" <> sqlCfg.pid.toText <> "'", dateRangeStr, "(" <> whereCondition <> ")"]

      -- Build complete WHERE clause for count queries (without cursor)
      buildWhereForCount = T.intercalate " and " $ filter (not . T.null) ["project_id='" <> sqlCfg.pid.toText <> "'", dateRangeStrForCount, "(" <> whereCondition <> ")"]

      finalSqlQuery = case sqlCfg.targetSpansM of
        Just "service-entry-spans" ->
          [fmt|WITH ranked_spans AS (SELECT *, resource->'service'->>'name' AS service_name,
                ROW_NUMBER() OVER (PARTITION BY trace_id, resource->'service'->>'name' ORDER BY start_time) AS rn
                FROM otel_logs_and_spans where {buildWhere}
                {groupByClause}
                )
               SELECT {selectClause} FROM ranked_spans
                  WHERE rn = 1 {sortOrder} {limitClause} |]
        _ ->
          case qc.finalSummarizeQuery of
            Just binInterval ->
              -- For summarize queries with bin functions, create a special format
              let
                -- Create time bucket expression
                timeBucketExpr = "time_bucket('" <> binInterval <> "', " <> timestampCol <> ")"
               in
                -- Include the time bucket expression as the first column
                let selectCols = T.intercalate "," (filter (\s -> not ("time_bucket" `T.isInfixOf` s)) qc.select)
                 in [fmt|SELECT
                       extract(epoch from {timeBucketExpr})::integer,
                       {selectCols}
                   FROM {fromTable}
                   WHERE {buildWhere}
                   GROUP BY {timeBucketExpr}
                   ORDER BY {timeBucketExpr} DESC
                   {limitClause} |]
            Nothing ->
              [fmt|SELECT {selectClause} FROM {fromTable}
                 WHERE {buildWhere}
                 {groupByClause} {sortOrder} {limitClause} |]
      countQuery =
        case qc.finalSummarizeQuery of
          Just binInterval ->
            -- For summarize with time bucket, count the number of unique time buckets
            let
              -- Create time bucket expression
              timeBucketExpr = "time_bucket('" <> binInterval <> "', " <> timestampCol <> ")"

              -- Create the subquery column list and GROUP BY clause
              -- Including group by columns if present
              groupByCols = T.intercalate ", " qc.groupByClause

              -- Create column selections for subquery
              subqueryCols =
                if null qc.groupByClause
                  then timeBucketExpr
                  else timeBucketExpr <> ", " <> groupByCols

              -- Create GROUP BY clause
              groupByPart =
                if null qc.groupByClause
                  then timeBucketExpr
                  else timeBucketExpr <> ", " <> groupByCols
             in
              [fmt|SELECT count(*) FROM
                  (SELECT {subqueryCols}
                   FROM {fromTable}
                   WHERE {buildWhereForCount}
                   GROUP BY {groupByPart}) as subq|]
          Nothing ->
            -- For regular summarize queries
            [fmt|SELECT count(*) FROM {fromTable}
              WHERE {buildWhereForCount}
              {groupByClause} limit 1|]

      -- Parse percentiles marker: __PERCENTILES__fieldExpr__PCTS__p1,p2,p3__END__
      percentilesInfo = do
        sel <- listToMaybe qc.select
        rest1 <- T.stripPrefix "__PERCENTILES__" sel >>= T.stripSuffix "__END__"
        let (fieldPart, rest2) = T.breakOn "__PCTS__" rest1
        rest3 <- T.stripPrefix "__PCTS__" rest2
        let pcts = mapMaybe (readMaybe . toString) $ T.splitOn "," rest3 :: [Double]
        if null pcts then Nothing else pure (fieldPart, pcts)

      -- Generate the summarize query depending on bin functions and data type
      summarizeQuery =
        case qc.finalSummarizeQuery of
          Just binInterval ->
            case percentilesInfo of
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
                let groupCol =
                      if null qc.groupByClause
                        then "'" <> (if null qc.select then "count" else "value") <> "'"
                        else "COALESCE(" <> fromMaybe "status_code" (listToMaybe qc.groupByClause) <> "::text, 'null')"
                    aggCol = if null qc.select then "count(*)" else fromMaybe "count(*)" (listToMaybe qc.select)
                    timeBucketExpr = "time_bucket('" <> binInterval <> "', " <> timestampCol <> ")"
                    firstGroupCol = fromMaybe "status_code" (listToMaybe qc.groupByClause)
                    groupByPart = if null qc.groupByClause then timeBucketExpr else timeBucketExpr <> ", " <> firstGroupCol
                 in [fmt|SELECT extract(epoch from {timeBucketExpr})::integer, {groupCol}, ({aggCol})::float
                      FROM {fromTable} WHERE {buildWhere} GROUP BY {groupByPart}
                      ORDER BY {timeBucketExpr} DESC {limitClause}|]
          Nothing ->
            [fmt|SELECT {T.intercalate "," qc.select} FROM {fromTable} WHERE {buildWhere}
              {groupByClause} ORDER BY {timestampCol} DESC {limitClause}|]

      -- FIXME: render this based on the aggregations, but without the aliases
      alertSelect = [fmt| count(*)::integer|] :: Text
      alertGroupByClause = if null qc.groupByClause then "" else " GROUP BY " <> T.intercalate "," qc.groupByClause
      -- Returns the max of all the values returned by the query. Change 5mins to
      -- For alert queries, we use a simplified whereCondition without the time range/cursor
      alertWhereCondition =
        if not (T.null rawWhere)
          then whereClause
          else "TRUE"

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
   in ( finalSqlQuery
      , qc
          { finalColumns = listToColNames selectedCols
          , countQuery
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
