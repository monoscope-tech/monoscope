-- Parser implemented with help and code from: https://markkarpov.com/tutorial/megaparsec.html
module Pkg.Parser (parseQueryStringToWhereClause, queryASTToComponents, parseQueryToComponents, fixedUTCTime, parseQuery, sectionsToComponents, defSqlQueryCfg, defPid, SqlQueryCfg (..), QueryComponents (..), listToColNames, pSource, parseQueryToAST, ToQueryText (..)) where

import Control.Error (hush)
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), diffUTCTime, nominalDiffTimeToSeconds, secondsToDiffTime)
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
  , rollup :: Maybe Text
  , finalTimechartQuery :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)


sectionsToComponents :: [Section] -> QueryComponents
sectionsToComponents = foldl' applySectionToComponent (def :: QueryComponents)


applySectionToComponent :: QueryComponents -> Section -> QueryComponents
applySectionToComponent qc (Search expr) = qc{whereClause = Just $ display expr}
applySectionToComponent qc (Source source) = qc{fromTable = Just $ display source}
applySectionToComponent qc (StatsCommand aggs Nothing) = qc{select = qc.select <> map display aggs}
applySectionToComponent qc (StatsCommand aggs byClauseM) = applyByClauseToQC byClauseM $ qc{select = qc.select <> map display aggs}
applySectionToComponent qc (TimeChartCommand agg byClauseM rollupM) = applyRollupToQC rollupM $ applyByClauseToQC byClauseM $ qc{select = qc.select <> [display agg]}


applyByClauseToQC :: Maybe ByClause -> QueryComponents -> QueryComponents
applyByClauseToQC Nothing qc = qc
applyByClauseToQC (Just (ByClause fields)) qc = qc{groupByClause = qc.groupByClause <> map display fields}


applyRollupToQC :: Maybe Rollup -> QueryComponents -> QueryComponents
applyRollupToQC Nothing qc = qc
applyRollupToQC (Just (Rollup ru)) qc = qc{rollup = Just ru}


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
  deriving stock (Show, Generic)
  deriving anyclass (Default)


normalizeKeyPath :: Text -> Text
normalizeKeyPath txt = T.toLower $ T.replace "]" "❳" $ T.replace "[" "❲" $ T.replace "." "•" txt


sqlFromQueryComponents :: SqlQueryCfg -> QueryComponents -> (Text, QueryComponents)
sqlFromQueryComponents sqlCfg qc =
  let fmtTime = toText . iso8601Show
      fromTable = fromMaybe "apis.request_dumps" $ qc.fromTable <|> (display <$> sqlCfg.source)
      timestampCol = if fromTable == "apis.request_dumps" then "created_at" else "timestamp"

      cursorT = maybe "" (\c -> " AND " <> timestampCol <> "<'" <> fmtTime c <> "' ") sqlCfg.cursorM
      -- Handle the Either error case correctly not hushing it.
      projectedColsProcessed =
        sqlCfg.projectedColsByUser & mapMaybe \col -> do
          subJ@(Subject entire _ _) <- hush (parse pSubject "" col)
          pure $ display subJ <> " as " <> normalizeKeyPath entire
      selectedCols = if null qc.select then projectedColsProcessed <> sqlCfg.defaultSelect else qc.select
      selectClause = T.intercalate "," $ colsNoAsClause selectedCols
      where' = maybe "" (\whereC -> " AND (" <> whereC <> ")") qc.whereClause
      whereClause = case sqlCfg.source of
        Just SSpans -> case sqlCfg.targetSpansM of
          Just "root-spans" -> where' <> " AND parent_span_id IS NULL"
          Just "service-entry-spans" -> where'
          _ -> where'
        _ -> where'
      groupByClause = if null qc.groupByClause then "" else " GROUP BY " <> T.intercalate "," qc.groupByClause
      dateRangeStr = case sqlCfg.dateRange of
        (Nothing, Just b) -> "AND " <> timestampCol <> " BETWEEN '" <> fmtTime b <> "' AND NOW() "
        (Just a, Just b) -> "AND " <> timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime b <> "'"
        _ -> ""

      (fromT, toT) = bimap (fromMaybe sqlCfg.currentTime) (fromMaybe sqlCfg.currentTime) sqlCfg.dateRange
      timeDiffSecs = abs $ nominalDiffTimeToSeconds $ diffUTCTime fromT toT
      finalSqlQuery = case sqlCfg.targetSpansM of
        Just "service-entry-spans" ->
          [fmt|WITH ranked_spans AS (SELECT *, resource->>'service.name' AS service_name,
                ROW_NUMBER() OVER (PARTITION BY trace_id, resource->>'service.name' ORDER BY start_time) AS rn
                FROM telemetry.spans where project_id='{sqlCfg.pid.toText}'::uuid  and (
                {timestampCol} > NOW() - interval '14 days'
                {cursorT} {dateRangeStr} {whereClause} )
                {groupByClause}
                )
               SELECT json_build_array({selectClause}) FROM ranked_spans
                  WHERE rn = 1 ORDER BY {timestampCol} desc limit 200 |]
        _ ->
          [fmt|SELECT json_build_array({selectClause}) FROM {fromTable}
             WHERE project_id='{sqlCfg.pid.toText}'::uuid  and ( {timestampCol} > NOW() - interval '14 days'
             {cursorT} {dateRangeStr} {whereClause} )
             {groupByClause} ORDER BY {timestampCol} desc limit 200 |]
      countQuery =
        [fmt|SELECT count(*) FROM {fromTable}
          WHERE project_id='{sqlCfg.pid.toText}'::uuid  and ( {timestampCol} > NOW() - interval '14 days'
          {cursorT} {dateRangeStr} {whereClause} )
          {groupByClause} limit 1|]

      defRollup
        | timeDiffSecs == 0 = "1h"
        | timeDiffSecs <= (60 * 30) = "1s"
        | timeDiffSecs <= (60 * 60) = "20s"
        | timeDiffSecs <= (60 * 60 * 6) = "1m"
        | timeDiffSecs <= (60 * 60 * 24 * 3) = "5m"
        | otherwise = "1h"

      timeRollup = fromMaybe defRollup (sqlCfg.presetRollup <|> qc.rollup)

      timebucket = [fmt|extract(epoch from time_bucket('{timeRollup}', {timestampCol}))::integer as timeB, |] :: Text
      -- FIXME: render this based on the aggregations
      chartSelect = [fmt| count(*)::integer as count|] :: Text
      timeGroupByClause = " GROUP BY " <> T.intercalate "," ("timeB" : qc.groupByClause)
      timeChartQuery =
        [fmt|
      SELECT {timebucket} {chartSelect}, 'Throughput' FROM {fromTable}
          WHERE project_id='{sqlCfg.pid.toText}'::uuid  and ( {timestampCol} > NOW() - interval '14 days'
          {cursorT} {dateRangeStr} {whereClause} )
          {timeGroupByClause}
    |]

      -- FIXME: render this based on the aggregations, but without the aliases
      alertSelect = [fmt| count(*)::integer|] :: Text
      alertGroupByClause = if null qc.groupByClause then "" else " GROUP BY " <> T.intercalate "," qc.groupByClause
      -- Returns the max of all the values returned by the query. Change 5mins to
      alertQuery =
        [fmt|
      SELECT GREATEST({alertSelect}) FROM {fromTable}
          WHERE project_id='{sqlCfg.pid.toText}'::uuid  and ( {timestampCol} > NOW() - interval '{timeRollup}'
          {whereClause}) {alertGroupByClause}
    |]
   in ( finalSqlQuery
      , qc
          { finalColumns = listToColNames selectedCols
          , countQuery
          , finalSqlQuery
          , finalTimechartQuery = Just timeChartQuery
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
parseQueryStringToWhereClause :: Text -> Either Text Text
parseQueryStringToWhereClause q =
  if q == ""
    then Right ""
    else
      bimap
        (toText . errorBundlePretty)
        (\x -> fromMaybe "" (sectionsToComponents x).whereClause)
        (parse parseQuery "" q)


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
parseQueryToComponents sqlCfg q = bimap (toText . errorBundlePretty) (queryASTToComponents sqlCfg) (parse parseQuery "" q)


queryASTToComponents :: SqlQueryCfg -> [Section] -> (Text, QueryComponents)
queryASTToComponents sqlCfg = sqlFromQueryComponents sqlCfg . sectionsToComponents


parseQueryToAST :: Text -> Either Text [Section]
parseQueryToAST q = first (toText . errorBundlePretty) (parse parseQuery "" q)


defPid :: Projects.ProjectId
defPid = def :: Projects.ProjectId


-- Only for tests. and harrd coding
fixedUTCTime :: UTCTime
fixedUTCTime = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)


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
timestampLogFmt colName = [fmt|to_char({colName} AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"') as {colName}|]


defaultSelectSqlQuery :: Maybe Sources -> [Text]
defaultSelectSqlQuery (Just SMetrics) = ["id"]
defaultSelectSqlQuery (Just STraces) = ["id"]
defaultSelectSqlQuery Nothing = defaultSelectSqlQuery (Just SRequests)
defaultSelectSqlQuery (Just SLogs) =
  [ "id"
  , timestampLogFmt "timestamp"
  , "severity_text"
  , "body"
  , [fmt|LEFT(
        CONCAT(
            COALESCE(attributes, 'null')
        ),
        255
    ) as rest|]
  ]
defaultSelectSqlQuery (Just SSpans) =
  [ "id"
  , timestampLogFmt "timestamp"
  , "trace_id"
  , "kind"
  , "status"
  , "span_name"
  , "CAST(EXTRACT(EPOCH FROM (end_time - start_time)) * 1_000_000_000 AS BIGINT) as duration"
  , "resource->>'service.name' as service"
  , "span_id as latency_breakdown"
  , [fmt|LEFT(
        CONCAT(
            'attributes=', COALESCE(attributes, 'null'),
            ' events=', COALESCE(events, 'null')
        ),
        255
    ) as rest|]
  ]
defaultSelectSqlQuery (Just SRequests) =
  [ "id::text as id"
  , timestampLogFmt "created_at"
  , "duration_ns as duration"
  , "request_type"
  , "host"
  , "status_code"
  , "method"
  , "url_path"
  , "JSONB_ARRAY_LENGTH(errors) as errors_count"
  , [fmt|LEFT(
        CONCAT(
            'url=', COALESCE(raw_url, 'null'),
            ' response_body=', COALESCE(response_body, 'null'),
            ' request_body=', COALESCE(request_body, 'null')
        ),
        255
    ) as rest|]
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
