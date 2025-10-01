-- Parser implemented with help and code from: https://markkarpov.com/tutorial/megaparsec.html
module Pkg.Parser (parseQueryStringToWhereClause, queryASTToComponents, parseQueryToComponents, getProcessedColumns, fixedUTCTime, parseQuery, sectionsToComponents, defSqlQueryCfg, defPid, SqlQueryCfg (..), QueryComponents (..), listToColNames, pSource, parseQueryToAST, ToQueryText (..), replaceNestJsonWithColumns, calculateAutoBinWidth) where

import Control.Error (hush)
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, diffUTCTime, secondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Pkg.Parser.Core (ToQueryText (..))
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
      -- Only generate date range conditions if they exist, otherwise return empty string
      dateRangeStr = case sqlCfg.dateRange of
        (Nothing, Just b) -> timestampCol <> " BETWEEN '" <> fmtTime b <> "' AND NOW() "
        (Just a, Just b) -> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime b <> "'"
        _ -> "TRUE"

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
            Nothing -> if (T.null rawWhere) then "limit 30" else "limit 150" -- Default limit for non-summarize queries

      -- Create a properly formatted WHERE clause condition
      whereCondition =
        let cursorStr = maybe "" (\c -> timestampCol <> "<'" <> fmtTime c <> "'") sqlCfg.cursorM
            dateRangeEmpty = sqlCfg.dateRange == (Nothing, Nothing) || dateRangeStr == "TRUE"
            whereEmpty = T.null rawWhere
         in if T.null cursorStr && dateRangeEmpty && whereEmpty
              then "TRUE" -- No conditions at all, use TRUE
              else
                let
                  cursorPart = if not (T.null cursorStr) then Just cursorStr else Nothing
                  datePart = if not dateRangeEmpty then Just dateRangeStr else Nothing
                  wherePart = if not whereEmpty then Just whereClause else Nothing

                  nonEmptyParts = filter (not . T.null) $ catMaybes [cursorPart, datePart, wherePart]
                 in
                  T.intercalate " AND " nonEmptyParts
      noConditionClause = if (T.null rawWhere) then " and parent_id is null" else ""

      finalSqlQuery = case sqlCfg.targetSpansM of
        Just "service-entry-spans" ->
          [fmt|WITH ranked_spans AS (SELECT *, resource->'service'->>'name' AS service_name,
                ROW_NUMBER() OVER (PARTITION BY trace_id, resource->'service'->>'name' ORDER BY start_time) AS rn
                FROM otel_logs_and_spans where project_id='{sqlCfg.pid.toText}' and ({whereCondition})
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
                   WHERE project_id='{sqlCfg.pid.toText}' and ({whereCondition})
                   GROUP BY {timeBucketExpr}
                   ORDER BY {timeBucketExpr} DESC
                   {limitClause} |]
            Nothing ->
              [fmt|SELECT {selectClause} FROM {fromTable}
                 WHERE project_id='{sqlCfg.pid.toText}' and ({whereCondition}) {noConditionClause}
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
                   WHERE project_id='{sqlCfg.pid.toText}' and ({whereCondition}) 
                   GROUP BY {groupByPart}) as subq|]
          Nothing ->
            -- For regular summarize queries
            [fmt|SELECT count(*) FROM {fromTable}
              WHERE project_id='{sqlCfg.pid.toText}' and ({whereCondition})
              {groupByClause} limit 1|]

      -- Generate the summarize query depending on bin functions and data type
      summarizeQuery =
        case qc.finalSummarizeQuery of
          Just binInterval ->
            -- For queries with bin functions, create a time-bucketed query returning (timestamp, group, value) tuples
            -- This format is compatible with the pivot' function in Charts.hs
            let
              -- Default to status_code as the group by column if none specified
              -- This simplifies the case where we need to return the tuple (time, group, value) format
              -- but only have a time series with no explicit grouping
              groupCol =
                if null qc.groupByClause
                  then "'" <> (if null qc.select then "count" else "value") <> "'"
                  else "COALESCE(" <> fromMaybe "status_code" (listToMaybe qc.groupByClause) <> "::text, 'null')"

              -- Default to count(*) if no aggregation specified
              aggCol =
                if null qc.select
                  then "count(*)"
                  else fromMaybe "count(*)" (listToMaybe qc.select)

              -- Create time bucket expression
              timeBucketExpr = "time_bucket('" <> binInterval <> "', " <> timestampCol <> ")"

              -- Create GROUP BY clause with conditional logic outside PyF template
              firstGroupCol = fromMaybe "status_code" (listToMaybe qc.groupByClause)
              groupByPart =
                if null qc.groupByClause
                  then timeBucketExpr
                  else timeBucketExpr <> ", " <> firstGroupCol
             in
              [fmt|
                SELECT 
                  extract(epoch from {timeBucketExpr})::integer, 
                  {groupCol},
                  ({aggCol})::float
                FROM {fromTable}
                WHERE project_id='{sqlCfg.pid.toText}' and ({whereCondition})
                GROUP BY {groupByPart}
                ORDER BY {timeBucketExpr} DESC
                {limitClause}
              |]
          Nothing ->
            -- For regular summarize queries without time buckets
            [fmt|
              SELECT {T.intercalate "," qc.select} 
              FROM {fromTable}
              WHERE project_id='{sqlCfg.pid.toText}' and ({whereCondition})
              {groupByClause}
              ORDER BY {timestampCol} DESC
              {limitClause}
            |]

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


-----------------------------------------------------------------------------------

-- Add more cases as needed, e.g., for List

----------------------------------------------------------------------------------
-- Convert Query as string to a string capable of being
-- used in the where clause of an sql statement
----------------------------------------------------------------------------------
-- >>> parseQueryStringToWhereClause "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "request_body->>'message'!='blabla' AND method='GET'"
--
-- >>> parseQueryStringToWhereClause "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "request_body->>'message'!='blabla' AND method='GET'"
--
-- >>> parseQueryStringToWhereClause "request_body.message==\"blabla\" AND method==\"GET\""
-- Right "request_body->>'message'='blabla' AND method='GET'"
--
-- >>> parseQueryStringToWhereClause "request_body.message.tags[*].name!=\"blabla\" AND method==\"GET\""
-- Right "jsonb_path_exists(request_body, $$$.\"message\".tags[*].\"name\" ? (@ != \"blabla\")$$::jsonpath) AND method='GET'"
--
-- >>> parseQueryStringToWhereClause "errors[*].error_type==\"Exception\""
-- Right "jsonb_path_exists(errors, $$$[*].\"error_type\" ? (@ == \"Exception\")$$::jsonpath)"
--
-- -- FIXME: broken. Should return non empty query
-- >>> parseQueryStringToWhereClause "errors==[]"
-- Right ""
--
-- -- FIXME: broken. Should return non empty query
-- >>> parseQueryStringToWhereClause "errors[*].error_type==[]" -- works with empty array but not otherwise. Right "jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == {} )')"
-- Right ""
--
-- -- FIXME: broken. Should return non empty query
-- >>> parseQueryStringToWhereClause "errors.error_type==[]"
-- Right ""
--
-- >>> parseQueryStringToWhereClause "errors[*].error_type=~/^ab.*c/"
-- Right "jsonb_path_exists(errors, $$$[*].\"error_type\" ? (@ like_regex \"^ab.*c\" flag \"i\" )$$::jsonpath)"
--
-- >>> parseQueryStringToWhereClause "response_body.roles[*] == \"user\""
-- Right "jsonb_path_exists(response_body, $$$.roles[*] ? (@ == \"user\")$$::jsonpath)"
--
-- >>> parseQueryStringToWhereClause "field_hashes[*]==\"42dd8b6020091ea9c01\""
-- Right "jsonb_path_exists(field_hashes, $$$[*] ? (@ == \"42dd8b6020091ea9c01\")$$::jsonpath)"
--
-- >>>  parseQueryStringToWhereClause "request_body.is_customer==true"
-- Right "request_body->>'is_customer'=true"
--
-- >>>  parseQueryStringToWhereClause "duration > 500ms"
-- Right "request_body->>'is_customer'=true"
--
parseQueryStringToWhereClause :: Text -> Either Text Text
parseQueryStringToWhereClause q =
  let trimmedQ = T.strip q
   in if trimmedQ == ""
        then Right ""
        else
          bimap
            (toText . errorBundlePretty)
            (\x -> fromMaybe "" (sectionsToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) x).whereClause)
            (parse parseQuery "" trimmedQ)


----------------------------------------------------------------------------------
-- parseQueryToComponents converts an apitoolkit query to components which can be executed directly against a database
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
          | hours <= 1 -> "30 seconds"
          | hours <= 6 -> "1 minutes"
          | hours <= 14 -> "5 minutes"
          | hours <= 48 -> "10 minutes"
          | days < 7 -> "1 hour"
          | days < 30 -> "6 hours"
          | otherwise -> "1 day"
calculateAutoBinWidth (Nothing, Just endTime) currentTime =
  -- If no start time, assume 14 days ago (same as default date range)
  let startTime = addUTCTime (-14 * 24 * 60 * 60) currentTime
   in calculateAutoBinWidth (Just startTime, Just endTime) currentTime
calculateAutoBinWidth (Just startTime, Nothing) currentTime =
  -- If no end time, use current time
  calculateAutoBinWidth (Just startTime, Just currentTime) currentTime
calculateAutoBinWidth (Nothing, Nothing) currentTime =
  -- Default to 14 days range if no date range specified
  let startTime = addUTCTime (-14 * 24 * 60 * 60) currentTime
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


-- | Converts OpenTelemetry nested JSON attribute notation to flattened database column names
-- This function handles the translation from dot notation (e.g., "attributes.http.request.method")
-- to triple-underscore notation (e.g., "attributes___http___request___method") used in the database schema
replaceNestJsonWithColumns :: Text -> Text
replaceNestJsonWithColumns =
  T.replace "url_path" "attributes___url___path"
    . T.replace "attributes.http.request.method" "attributes___http___request___method"
    . T.replace "attributes.http.request.method_original" "attributes___http___request___method_original"
    . T.replace "attributes.http.response.status_code" "attributes___http___response___status_code"
    . T.replace "attributes.http.request.resend_count" "attributes___http___request___resend_count"
    . T.replace "attributes.http.request.body.size" "attributes___http___request___body___size"
    . T.replace "attributes.url.fragment" "attributes___url___fragment"
    . T.replace "attributes.url.full" "attributes___url___full"
    . T.replace "attributes.url.path" "attributes___url___path"
    . T.replace "attributes.url.query" "attributes___url___query"
    . T.replace "attributes.url.scheme" "attributes___url___scheme"
    . T.replace "attributes.user_agent.original" "attributes___user_agent___original"
    . T.replace "attributes.db.system.name" "attributes___db___system___name"
    . T.replace "attributes.db.collection.name" "attributes___db___collection___name"
    . T.replace "attributes.db.namespace" "attributes___db___namespace"
    . T.replace "attributes.db.operation.name" "attributes___db___operation___name"
    . T.replace "attributes.db.operation.batch.size" "attributes___db___operation___batch___size"
    . T.replace "attributes.db.query.summary" "attributes___db___query___summary"
    . T.replace "attributes.db.query.text" "attributes___db___query___text"
    . T.replace "context.trace_id" "context___trace_id"
    . T.replace "context.span_id" "context___span_id"
    . T.replace "context.trace_state" "context___trace_state"
    . T.replace "context.trace_flags" "context___trace_flags"
    . T.replace "context.is_remote" "context___is_remote"
    . T.replace "resource.service.name" "resource___service___name"
    . T.replace "resource.service.version" "resource___service___version"
    . T.replace "resource.service.instance.id" "resource___service___instance___id"
    . T.replace "resource.service.namespace" "resource___service___namespace"
    . T.replace "resource.telemetry.sdk.language" "resource___telemetry___sdk___language"
    . T.replace "resource.telemetry.sdk.name" "resource___telemetry___sdk___name"
    . T.replace "resource.telemetry.sdk.version" "resource___telemetry___sdk___version"
