-- Parser implemented with help and code from: https://markkarpov.com/tutorial/megaparsec.html
module Pkg.Parser (parseQueryStringToWhereClause, parseQueryToComponents, defSqlQueryCfg, SqlQueryCfg(..), listToColNames) where

import Control.Monad.Combinators.Expr
import Data.Default (Default (def))
import Data.Foldable (foldl)
import Data.Text qualified as T
import Data.Text.Display (Display, display, displayBuilder, displayParen, displayPrec)
import Data.Text.Lazy.Builder (Builder)
import Data.Time (CalendarDiffTime, ZonedTime, diffUTCTime, zonedTimeToUTC)
import Data.Time.Format
import Models.Projects.Projects qualified as Projects
import Pkg.Parser.Expr
import Pkg.Parser.Stats
import Pkg.Parser.Types
import PyF
import Relude hiding (GT, LT, many, some)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Witch (from)


-- Example queries
-- request_body.v1.v2 = "abc" AND (request_body.v3.v4 = 123 OR request_body.v5[].v6=ANY[1,2,3] OR request_body[1].v7 OR NOT request_body[-1].v8 )
-- request_body[1].v7 | {.v7, .v8} |
--

pSection :: Parser Section
pSection =
  choice @[]
    [ pStatsSection
    , Search <$> pExpr
    ]


parseQuery :: Parser [Section]
parseQuery = sepBy pSection (char '|')


data QueryComponents = QueryComponents
  { whereClause :: Maybe Text
  , groupByClause :: [Text]
  , select :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)


sectionsToComponents :: [Section] -> QueryComponents
sectionsToComponents = foldl' applySectionToComponent (def :: QueryComponents)


applySectionToComponent :: QueryComponents -> Section -> QueryComponents
applySectionToComponent qc (Search expr) = qc{whereClause = Just $ display expr}
applySectionToComponent qc (StatsCommand agg Nothing) = qc{select = qc.select <> map display agg}
applySectionToComponent qc (StatsCommand agg (Just (ByClause fields))) =
  qc
    { select = qc.select <> map display agg
    , groupByClause = qc.groupByClause <> map display fields
    }


----------------------------------------------------------------------------------

data SqlQueryCfg = SqlQueryCfg
  { pid :: Projects.ProjectId
  , dateRange :: (Maybe ZonedTime, Maybe ZonedTime)
  , cursorM :: Maybe ZonedTime
  , defaultSelect :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)


sqlFromQueryComponents :: SqlQueryCfg -> QueryComponents -> Text
sqlFromQueryComponents sqlCfg qc =
  let
    fmtTime = formatTime defaultTimeLocale "%F %R"
    cursorT = maybe "" (\c -> " AND " <> fmtTime c) sqlCfg.cursorM
    selectClause = T.intercalate "," $ if null qc.select then sqlCfg.defaultSelect else qc.select
    whereClause = maybe "" (" AND " <>) qc.whereClause
    groupByClause = if null qc.groupByClause then "" else " GROUP BY " <> T.intercalate "," qc.groupByClause
    dateRangeStr = case sqlCfg.dateRange of
      (Nothing, Just b) -> "AND created_at BETWEEN NOW() AND '" <> fmtTime b <> "'"
      (Just a, Just b) -> "AND created_at BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime b <> "'"
      _ -> ""
   in
    [fmt|SELECT row_to_json(t) FROM ( SELECT {selectClause} FROM apis.request_dumps 
          WHERE project_id='{sqlCfg.pid.toText}'::uuid  and created_at > NOW() - interval '14 days' {cursorT} {dateRangeStr} {whereClause}
          {groupByClause} ORDER BY created_at desc limit 200 ) t|]


-----------------------------------------------------------------------------------

-- Add more cases as needed, e.g., for List

----------------------------------------------------------------------------------
-- Convert Query as string to a string capable of being
-- used in the where clause of an sql statement
----------------------------------------------------------------------------------
-- >>> parseQueryStringToWhereClause "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "request_body->>'message'!='blabla' AND method=='GET'"
--
-- >>> parseQueryStringToWhereClause "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "request_body->>'message'!='blabla' AND method=='GET'"
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
-- >>> parseQueryToComponents (defSqlQueryCfg defPid) "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND request_body->>'message'!='blabla' AND method=='GET'\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid) "request_body.message!=\"blabla\" AND method==\"GET\""
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND request_body->>'message'!='blabla' AND method=='GET'\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid) "request_body.message.tags[*].name!=\"blabla\" AND method==\"GET\""
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND jsonb_path_exists(request_body, '$.\"message\"[*].\"name\" ?? (@ != \"blabla\")') AND method=='GET'\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid) "errors[*].error_type==\"Exception\""
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == \"Exception\")')\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid) "errors==[]"
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND errors==ARRAY[]\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid) "errors[*].error_type==[]" -- works with empty array but not otherwise. Right "jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == {} )')"
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ == {} )')\n           ORDER BY created_at desc limit 200"
--
-- >>> parseQueryToComponents (defSqlQueryCfg defPid) "errors[*].error_type=~/^ab.*c/"
-- Right "SELECT id,created_at,host,status_code,LEFT(\n        CONCAT(\n            'request_body=', COALESCE(request_body, 'null'), \n            ' response_body=', COALESCE(response_body, 'null')\n        ),\n        255\n    ) AS rest FROM apis.request_dumps \n          WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid  and created_at > NOW() - interval '14 days'    AND jsonb_path_exists(errors, '$[*].\"error_type\" ?? (@ like_regex \"^ab.*c\")')\n           ORDER BY created_at desc limit 200"
parseQueryToComponents :: SqlQueryCfg -> Text -> Either Text Text
parseQueryToComponents sqlCfg q =
  bimap
    (toText . errorBundlePretty)
    (sqlFromQueryComponents sqlCfg . sectionsToComponents)
    (parse parseQuery "" q)


defPid :: Projects.ProjectId
defPid = def :: Projects.ProjectId


defSqlQueryCfg :: Projects.ProjectId -> SqlQueryCfg
defSqlQueryCfg pid =
  SqlQueryCfg
    { pid = pid
    , cursorM = Nothing
    , dateRange = (Nothing, Nothing)
    , defaultSelect =
        [ "id::text as id"
        , "created_at"
        , "host"
        , "status_code"
        , "JSONB_ARRAY_LENGTH(errors) as errors_count"
        , [fmt|LEFT(
        CONCAT(
            'request_body=', COALESCE(request_body, 'null'), 
            ' response_body=', COALESCE(response_body, 'null')
        ),
        255
    ) as rest|]
        ]
    }

-- >>> listToColNames ["id", "JSONB_ARRAY_LENGTH(errors) as errors_count"]
listToColNames :: [Text] -> [Text]
listToColNames = map \x-> T.strip $ last  $ "" :| T.splitOn "as" x
