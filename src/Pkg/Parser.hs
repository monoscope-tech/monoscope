-- Parser implemented with help and code from: https://markkarpov.com/tutorial/megaparsec.html
module Pkg.Parser (parseQueryStringToWhereClause, parseQueryToComponents, fixedUTCTime, parseQuery, sectionsToComponents, defSqlQueryCfg, defPid, SqlQueryCfg (..), QueryComponents (..), listToColNames) where

import Control.Error (hush)
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), diffUTCTime, nominalDiffTimeToSeconds, secondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Pkg.Parser.Expr (pExpr, pSubject)
import Pkg.Parser.Stats (pStatsSection, pTimeChartSection)
import Pkg.Parser.Types (
  Subject(..),
  ByClause (..),
  Parser,
  Rollup (..),
  Section (..),
 )
import PyF (fmt)
import Debug.Pretty.Simple
import Relude
import Safe qualified
import Text.Megaparsec (choice, errorBundlePretty, parse, sepBy)
import Text.Megaparsec.Char (char, space)


-- Example queries
-- request_body.v1.v2 = "abc" AND (request_body.v3.v4 = 123 OR request_body.v5[].v6=ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |
--

pSection :: Parser Section
pSection =
  choice @[]
    [ pStatsSection
    , pTimeChartSection
    , Search <$> pExpr
    ]


parseQuery :: Parser [Section]
parseQuery = sepBy pSection (space *> char '|' <* space)


data QueryComponents = QueryComponents
  { whereClause :: Maybe Text
  , groupByClause :: [Text]
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)

normalizeKeyPath :: Text -> Text
normalizeKeyPath txt = T.toLower $ T.replace "]" "❳" $ T.replace "[" "❲" $ T.replace "." "•" txt

sqlFromQueryComponents :: SqlQueryCfg -> QueryComponents -> (Text, QueryComponents)
sqlFromQueryComponents sqlCfg qc =
  let
    fmtTime = toText . iso8601Show
    cursorT = maybe "" (\c -> " AND created_at<'" <> fmtTime c <> "' ") sqlCfg.cursorM
    -- Handle the Either error case correctly not hushing it.
    projectedColsProcessed = sqlCfg.projectedColsByUser & mapMaybe \col -> do
      subJ@(Subject entire _ _) <- hush (parse pSubject "" col) 
      pure $ display subJ <> " as " <> normalizeKeyPath entire 
    selectedCols = if null qc.select then projectedColsProcessed <> sqlCfg.defaultSelect else qc.select
    selectClause = T.intercalate "," $ colsNoAsClause selectedCols
    whereClause = maybe "" (\whereC -> " AND (" <> whereC <> ")") qc.whereClause
    groupByClause = if null qc.groupByClause then "" else " GROUP BY " <> T.intercalate "," qc.groupByClause
    dateRangeStr = case sqlCfg.dateRange of
      (Nothing, Just b) -> "AND created_at BETWEEN NOW() AND '" <> fmtTime b <> "'"
      (Just a, Just b) -> "AND created_at BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime b <> "'"
      _ -> ""

    (fromT, toT) = bimap (fromMaybe sqlCfg.currentTime) (fromMaybe sqlCfg.currentTime) sqlCfg.dateRange
    timeDiffSecs = abs $ nominalDiffTimeToSeconds $ diffUTCTime fromT toT

    finalSqlQuery =
      [fmt|SELECT json_build_array({selectClause}) FROM apis.request_dumps 
            WHERE project_id='{sqlCfg.pid.toText}'::uuid  and ( created_at > NOW() - interval '14 days' 
            {cursorT} {dateRangeStr} {whereClause} )
            {groupByClause} ORDER BY created_at desc limit 200 |]

    countQuery =
      [fmt|SELECT count(*) FROM apis.request_dumps 
            WHERE project_id='{sqlCfg.pid.toText}'::uuid  and ( created_at > NOW() - interval '14 days' 
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

    timebucket = [fmt|extract(epoch from time_bucket('{timeRollup}', created_at))::integer as timeB, |] :: Text
    -- FIXME: render this based on the aggregations
    chartSelect = [fmt| count(*)::integer as count|] :: Text
    timeGroupByClause = " GROUP BY " <> T.intercalate "," ("timeB" : qc.groupByClause)
    timeChartQuery =
      [fmt|
        SELECT {timebucket} {chartSelect}, 'Throughput' FROM apis.request_dumps
            WHERE project_id='{sqlCfg.pid.toText}'::uuid  and ( created_at > NOW() - interval '14 days' 
            {cursorT} {dateRangeStr} {whereClause} )
            {timeGroupByClause}
      |]

    -- FIXME: render this based on the aggregations, but without the aliases
    alertSelect = [fmt| count(*)::integer|] :: Text
    alertGroupByClause = if null qc.groupByClause then "" else " GROUP BY " <> T.intercalate "," qc.groupByClause
    -- Returns the max of all the values returned by the query. Change 5mins to
    alertQuery =
      [fmt|
        SELECT GREATEST({alertSelect}) FROM apis.request_dumps
            WHERE project_id='{sqlCfg.pid.toText}'::uuid  and ( created_at > NOW() - interval '{timeRollup}'
            {whereClause}) {alertGroupByClause} 
      |]
   in
    ( finalSqlQuery
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
-- Right "request_body->>'message' as request_body\8226message!='blabla' AND method=='GET'"
--
-- >>> parseQueryStringToWhereClause "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "request_body->>'message' as request_body\8226message!='blabla' AND method=='GET'"
--
-- >>> parseQueryStringToWhereClause "request_body.message.tags[*].name!=\"blabla\" AND method==\"GET\""
-- Right "jsonb_path_exists(request_body, '$.\"message\"[*].\"name\" ?? (@ != \"blabla\")') AND method=='GET'"
--
-- >>> parseQueryStringToWhereClause "errors[*].error_type==\"Exception\""
-- Right "jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == \"Exception\")')"
--
-- >>> parseQueryStringToWhereClause "errors==[]"
-- Right "errors==ARRAY[]"
--
-- >>> parseQueryStringToWhereClause "errors[*].error_type==[]" -- works with empty array but not otherwise. Right "jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == {} )')"
-- Right "jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == {} )')"
--
-- >>> parseQueryStringToWhereClause "errors[*].error_type=~/^ab.*c/"
-- Right "jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ like_regex \"^ab.*c\")')"
--
-- >>> parseQueryStringToWhereClause "response_body.roles[*] == \"user\""
-- Right "jsonb_path_exists(response_body, '$[*] ?? (@ == \"user\")')"
--
-- >>> parseQueryStringToWhereClause "field_hashes[*]==\"42dd8b6020091ea9c01\""
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
-- Convert Query as string to a string capable of being
-- used in the where clause of an sql statement
----------------------------------------------------------------------------------
-- >>> parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND request_body->>'message'!='blabla' AND method=='GET'\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND request_body->>'message'!='blabla' AND method=='GET'\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "request_body.message.tags[*].name!=\"blabla\" AND method==\"GET\""
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND jsonb_path_exists(request_body, '$.\"message\"[*].\"name\" ?? (@ != \"blabla\")') AND method=='GET'\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "errors[*].error_type==\"Exception\""
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == \"Exception\")')\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "errors==[]"
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND errors==ARRAY[]\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "errors[*].error_type==[]" -- works with empty array but not otherwise. Right "jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == {} )')"
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == {} )')\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "errors[*].error_type=~/^ab.*c/"
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ like_regex \"^ab.*c\")')\n           ORDER BY created_at desc limit 200"
--
parseQueryToComponents :: SqlQueryCfg -> Text -> Either Text (Text, QueryComponents)
parseQueryToComponents sqlCfg q =
  bimap
    (toText . errorBundlePretty)
    (sqlFromQueryComponents sqlCfg . sectionsToComponents)
    (parse parseQuery "" q)


defPid :: Projects.ProjectId
defPid = def :: Projects.ProjectId


-- Only for tests. and harrd coding
fixedUTCTime :: UTCTime
fixedUTCTime = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)


defSqlQueryCfg :: Projects.ProjectId -> UTCTime -> SqlQueryCfg
defSqlQueryCfg pid currentTime =
  SqlQueryCfg
    { pid = pid
    , presetRollup = Nothing
    , cursorM = Nothing
    , dateRange = (Nothing, Nothing)
    , projectedColsByUser = []
    , currentTime
    , defaultSelect =
        [ "id::text as id"
        , [fmt|to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"') as created_at|]
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
    }


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
