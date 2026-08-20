-- Parser implemented with help and code from: https://markkarpov.com/tutorial/megaparsec.html
module Pkg.Parser (queryASTToComponents, parseQueryToComponents, getProcessedColumns, fixedUTCTime, parseQuery, sectionsToComponents, defSqlQueryCfg, defPid, PageDirection (..), PageCursor (..), SqlQueryCfg (..), QueryComponents (..), NormalizedQuery (..), normalizeQuery, buildDateRange, buildGroupBy, buildOrderBy, buildLimit, buildWhereCondition, buildEnvFilter, listToColNames, colsNoAsClause, defaultSelectSqlQuery, pSource, parseQueryToAST, ToQueryText (..), calculateAutoBinWidth, replacePlaceholders, variablePresets, variablePresetsKQL, constantToSQLList, constantToKQLList, defaultQueryLimit) where

import Control.Error (hush)
import Data.Char (isAlphaNum)
import Data.Default (Default (def))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, diffUTCTime, secondsToDiffTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Pkg.Deriving (WrappedEnumSC (..))
import Pkg.Parser.Expr
import Pkg.Parser.Stats
import PyF (fmt)
import Relude
import Text.Megaparsec (parse)
import Utils (formatUTC)
import Web.HttpApiData (FromHttpApiData)


data QueryComponents = QueryComponents
  { whereClause :: Maybe Text
  , havingClause :: Maybe Text -- Post-summarize filter; aggregate aliases resolved to their expressions
  , groupByClause :: [Text]
  , fromTable :: Maybe Text
  , select :: [Text] -- project: replaces default columns
  , extendSelect :: [Text] -- extend: appends to default columns
  , aggregations :: [Text] -- Summarize aggregations (separate from extended columns in select)
  , finalColumns :: [Text]
  , finalSqlQuery :: Text
  , hasCountOver :: Bool -- True if finalSqlQuery includes count(*) OVER() as last column
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
  , nqEnvironment :: Text -- "" or the deployment-environment predicate
  , nqGroupBy :: Text -- "" or "GROUP BY x, y"
  , nqHaving :: Text -- "" or "HAVING (...)"
  , nqOrderBy :: Text -- "" or "ORDER BY x DESC"
  , nqLimit :: Text -- "" or "LIMIT 500"
  , nqBinInterval :: Maybe Text
  , nqPercentiles :: Maybe (Text, [Double])
  , nqExtendedColumns :: [(Text, Text)]
  }
  deriving stock (Generic, Show)


-- | One source of truth for the timestamp column across every builder below.
timestampCol :: Text
timestampCol = "timestamp"


timeBucketExpr :: Text -> Text
timeBucketExpr interval = "time_bucket('" <> interval <> "', " <> timestampCol <> ")"


-- | Build date range SQL clause from config
buildDateRange :: SqlQueryCfg -> Text
buildDateRange cfg =
  let fmtTime = toText . iso8601Show
      between a b = timestampCol <> " BETWEEN '" <> fmtTime a <> "' AND '" <> fmtTime b <> "'"
   in case (cfg.dateRange, cfg.cursorM) of
        ((Just a, _), Just (PageCursor PageOlder cursor)) -> between a cursor
        ((Nothing, _), Just (PageCursor PageOlder cursor)) -> timestampCol <> " <= '" <> fmtTime cursor <> "'"
        ((_, Just b), Just (PageCursor PageNewer cursor)) -> between cursor b
        ((_, Nothing), Just (PageCursor PageNewer cursor)) -> timestampCol <> " >= '" <> fmtTime cursor <> "'"
        ((Just a, Just b), Nothing) -> between a b
        ((Just a, Nothing), Nothing) -> timestampCol <> " >= '" <> fmtTime a <> "'"
        ((Nothing, Just b), Nothing) -> timestampCol <> " <= '" <> fmtTime b <> "'"
        ((Nothing, Nothing), Nothing) -> ""


-- | Build GROUP BY clause with resolved extended columns
buildGroupBy :: [(Text, Text)] -> [Text] -> Text
buildGroupBy extCols cols
  | null cols = ""
  | otherwise = let extColsMap = Map.fromList extCols in " GROUP BY " <> T.intercalate "," (map (resolveExtendedColumn extColsMap) cols)


-- | Alias → expression for every aggregation, e.g. @count_ → count(*)::float@.
-- Needed wherever an aggregate is projected as something other than its raw
-- expression: @ORDER BY count_@ would then sort the projected form (text, in
-- the categorical branch) instead of the number.
aggregationAliases :: QueryComponents -> Map.Map Text Text
aggregationAliases qc = Map.fromList [(alias, expr) | agg <- qc.aggregations, (expr, Just alias) <- [splitTrailingAlias agg]]


-- | Build ORDER BY clause with fallback logic
buildOrderBy :: QueryComponents -> Text
buildOrderBy qc = case qc.sortFields of
  Just fields -> "ORDER BY " <> T.intercalate ", " (map (displaySortFieldWith (aggregationAliases qc)) fields)
  Nothing -> case qc.finalSummarizeQuery of
    Just binInterval -> bucketOrder binInterval
    Nothing
      | any (T.isInfixOf "time_bucket") qc.select -> bucketOrder defaultBinSize
      | otherwise -> "ORDER BY " <> fromMaybe timestampCol (listToMaybe qc.groupByClause) <> " desc"
  where
    bucketOrder interval = "ORDER BY " <> timeBucketExpr interval <> " desc"


defaultQueryLimit :: Int
defaultQueryLimit = 500


-- | Build LIMIT clause with defaults
buildLimit :: QueryComponents -> Text
buildLimit qc = case (qc.takeLimit, qc.finalSummarizeQuery) of
  (Just limit, _) -> "limit " <> show limit
  (Nothing, Just _) -> "" -- No limit for summarize queries
  (Nothing, Nothing) -> "limit " <> show defaultQueryLimit


-- | Build WHERE condition from raw clause
-- | @HAVING@ for a post-summarize filter, with aggregation aliases replaced by
-- the expressions they name: SQL has no @HAVING count_ > 1@, only
-- @HAVING count(*)::float > 1@. Empty when the query has no such filter, which
-- is every query that never summarized.
--
-- >>> buildHaving def{havingClause = Just "count_ > 1", aggregations = ["count(*)::float AS count_"]}
-- "HAVING ((count(*)::float) > 1)"
-- >>> buildHaving def
-- ""
buildHaving :: QueryComponents -> Text
buildHaving qc = case qc.havingClause of
  Nothing -> ""
  Just cond -> "HAVING (" <> foldr substitute cond aliases <> ")"
  where
    aliases = [(alias, expr) | agg <- qc.aggregations, (expr, Just alias) <- [splitTrailingAlias agg]]
    substitute (alias, expr) = T.replace alias ("(" <> expr <> ")")


buildWhereCondition :: Maybe Text -> Text
buildWhereCondition = maybe "TRUE" \w -> if T.null w then "TRUE" else "(" <> w <> ")"


-- | The environment predicate, or @""@ when no environment is selected.
--
-- A column predicate rather than an injection into the user's KQL: the query box must keep
-- showing what the user typed, and this has to apply to the summarize and alert shapes too.
-- Rows predating the promoted column carry NULL here, so a selection genuinely excludes
-- them — which is why the picker treats "all environments" as the default rather than
-- picking one for the user.
--
-- >>> buildEnvFilter (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing)
-- ""
--
-- >>> buildEnvFilter (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing){environment = Just "prod"}
-- "resource___deployment___environment___name = 'prod'"
--
-- Values reach this from a cookie and a query param, so they are escaped, not trusted:
--
-- >>> buildEnvFilter (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing){environment = Just "o'brien"}
-- "resource___deployment___environment___name = 'o''brien'"
buildEnvFilter :: SqlQueryCfg -> Text
buildEnvFilter cfg = maybe "" (\e -> "resource___deployment___environment___name = " <> sqlStringLit e) cfg.environment


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
        , nqEnvironment = buildEnvFilter cfg
        , nqGroupBy = buildGroupBy qc.extendedColumns qc.groupByClause
        , nqHaving = buildHaving qc
        , nqOrderBy = case cfg.cursorM of Just (PageCursor PageNewer _) -> "ORDER BY " <> timestampCol <> " asc"; _ -> buildOrderBy qc
        , nqLimit = buildLimit qc
        , nqBinInterval = qc.finalSummarizeQuery
        , nqPercentiles = qc.percentilesInfo
        , nqExtendedColumns = qc.extendedColumns
        }


sectionsToComponents :: SqlQueryCfg -> [Section] -> QueryComponents
sectionsToComponents sqlCfg = foldl' (applySectionToComponent sqlCfg) def


-- | Resolve a column name to its full expression when it's an extended column.
resolveExtendedColumn :: Map.Map Text Text -> Text -> Text
resolveExtendedColumn extColsMap colName = Map.findWithDefault colName colName extColsMap


applySectionToComponent :: SqlQueryCfg -> QueryComponents -> Section -> QueryComponents
applySectionToComponent sqlCfg qc = \case
  Search expr -> narrow expr
  WhereClause expr -> narrow expr
  HavingClause expr ->
    let new = display (resolveWildcardTimes sqlCfg.currentTime expr)
     in qc{havingClause = Just $ maybe new (\old -> old <> " AND " <> new) qc.havingClause}
  Source source -> qc{fromTable = Just $ display source}
  sec@(SummarizeCommand aggs byClauseM) ->
    applySummarizeByClauseToQC sqlCfg byClauseM qc{aggregations = qc.aggregations <> map display aggs, percentilesInfo = extractPercentilesInfo [sec]}
  -- extend adds computed columns (appends to defaults) AND tracks mappings for GROUP BY resolution
  ExtendCommand cols ->
    qc
      { extendedColumns = qc.extendedColumns <> [(name, fst $ splitTrailingAlias $ display expr) | (name, expr) <- cols]
      , extendSelect = qc.extendSelect <> map (display . snd) cols
      }
  -- project replaces select with only the specified columns
  ProjectCommand cols -> qc{select = map (display . snd) cols}
  SortCommand sortFields -> qc{sortFields = Just sortFields}
  TakeCommand limit -> qc{takeLimit = Just limit}
  where
    narrow expr =
      let new = display (resolveWildcardTimes sqlCfg.currentTime expr)
       in qc{whereClause = Just $ maybe new (<> " AND " <> new) qc.whereClause}


-- | Apply summarize by clause to query components
applySummarizeByClauseToQC :: SqlQueryCfg -> Maybe SummarizeByClause -> QueryComponents -> QueryComponents
applySummarizeByClauseToQC _ Nothing qc = qc
applySummarizeByClauseToQC sqlCfg (Just (SummarizeByClause items)) qc =
  qc
    { finalSummarizeQuery =
        listToMaybe [b | ByBinFunc b <- items] <&> \case
          Bin _ interval -> kqlTimespanToTimeBucket interval
          BinAuto _ -> calculateAutoBinWidth sqlCfg.dateRange sqlCfg.currentTime
    , groupByClause = qc.groupByClause <> [display item | item <- items, not (isBinFunc item)]
    }
  where
    isBinFunc = \case
      ByBinFunc _ -> True
      BySubject _ -> False
      ByScalarFunc _ -> False


-- | Display a sort field for SQL generation, resolving aggregation aliases to
-- the expressions they stand for.
displaySortFieldWith :: Map.Map Text Text -> SortField -> Text
displaySortFieldWith aliases (SortField field dirM) =
  resolveExtendedColumn aliases (display field) <> maybe "" ((" " <>) . display) dirM


----------------------------------------------------------------------------------

data PageDirection = PageOlder | PageNewer
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (FromHttpApiData) via WrappedEnumSC 'Nothing "Page" PageDirection


data PageCursor = PageCursor PageDirection UTCTime
  deriving stock (Eq, Generic, Show)


data SqlQueryCfg = SqlQueryCfg
  { pid :: Projects.ProjectId
  , presetRollup :: Maybe Text
  , dateRange :: (Maybe UTCTime, Maybe UTCTime)
  , cursorM :: Maybe PageCursor
  , projectedColsByUser :: [Text] -- cols selected explicitly by user
  , currentTime :: UTCTime
  , defaultSelect :: [Text]
  , source :: Maybe Sources
  , metricJsonAsVariant :: Bool
  , targetSpansM :: Maybe Text
  , -- Time window (minutes) the alert query should look back over.
    -- Monitors pass max(60, 2 * checkIntervalMins) so buckets aren't missed.
    alertLookbackMins :: Int
  , -- The app-wide environment selection (prod/staging/…). Applied as a column predicate on
    -- every generated query rather than spliced into the user's KQL, so the query box keeps
    -- showing what the user typed. 'Nothing' is every environment.
    environment :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)


normalizeKeyPath :: Text -> Text
normalizeKeyPath txt = T.toLower $ T.replace "]" "❳" $ T.replace "[" "❲" $ T.replace "." "•" txt


-- | Build the SELECT column list from the default columns plus any user-requested
-- extra columns. A requested column is skipped when the parser resolves it to a SQL
-- expression already in @defaultSelect@ (e.g. @resource.service.name@ and the default
-- @resource___service___name as service@ are the same column) — otherwise it would be
-- fetched twice and rendered as a duplicate column.
--
-- >>> length $ snd $ getProcessedColumns ["service"] ["resource___service___name as service"]
-- 1
-- >>> length $ snd $ getProcessedColumns ["attributes.http.request.method"] ["id"]
-- 2
-- >>> snd $ getProcessedColumns ["name"] ["name as span_name","duration"]
-- ["name as name","name as span_name","duration"]
getProcessedColumns :: [Text] -> [Text] -> (Text, [Text])
getProcessedColumns cols defaultSelect = (T.intercalate "," $ colsNoAsClause selectedCols, selectedCols)
  where
    defaultAliases = listToColNames defaultSelect
    prs =
      cols & mapMaybe \col -> do
        subJ@(Subject entire _ _) <- hush (parse pSubject "" col)
        let alias = normalizeKeyPath entire
        -- Dedup by output alias, not by SQL expr. A requested column whose alias already
        -- appears in the defaults is a real duplicate (e.g. "service"). But one resolving to
        -- the same DB expr under a *different* default alias (requested "name" vs default
        -- "name as span_name") must still be projected under the requested alias — else the
        -- user's requested column silently vanishes from the result.
        guard (alias `notElem` defaultAliases)
        pure $ display subJ <> " as " <> alias
    selectedCols = prs <> defaultSelect


sqlFromQueryComponents :: SqlQueryCfg -> QueryComponents -> (Text, QueryComponents)
sqlFromQueryComponents sqlCfg qc =
  let
    nq = normalizeQuery sqlCfg qc

    selectClause = T.intercalate "," $ map (\col -> if "summary" `T.isSuffixOf` col then "to_jsonb(summary)" else col) $ colsNoAsClause nq.nqSelectCols

    fromTable = nq.nqTable
    groupByClause = nq.nqGroupBy
    -- Only ever non-empty when a summarize ran, and every summarize shape below
    -- groups (or aggregates the whole set, where a bare HAVING is still valid).
    havingClause = nq.nqHaving
    sortOrder = nq.nqOrderBy
    limitClause = nq.nqLimit
    whereCondition = nq.nqWhere

    -- Build complete WHERE clause for data queries
    buildWhere = T.intercalate " and " $ filter (not . T.null) ["project_id='" <> nq.nqProjectId <> "'", nq.nqDateRange, nq.nqEnvironment, "(" <> whereCondition <> ")"]

    -- count(*) OVER() goes inside the array as the LAST element when
    -- hasCountOver = True; 'selectLogTable' peels it back off via dropLast.
    countOver = "count(*) OVER()" :: Text
    wrap :: Text -> Text
    wrap cols = "jsonb_build_array(" <> cols <> ")"
    -- Standard data queries skip count(*) OVER() and use LIMIT+1 to detect hasMore
    overflowLimitClause = "limit " <> show (fromMaybe defaultQueryLimit qc.takeLimit + 1)
    (finalSqlQuery, countOverIncluded) = case sqlCfg.targetSpansM of
      Just "service-entry-spans" ->
        ( [fmt|WITH ranked_spans AS (SELECT *, resource->'service'->>'name' AS service_name,
                ROW_NUMBER() OVER (PARTITION BY trace_id, resource->'service'->>'name' ORDER BY start_time) AS rn
                FROM otel_logs_and_spans where {buildWhere}
                {groupByClause} {havingClause}
                )
               SELECT {wrap (selectClause <> ", " <> countOver)} FROM ranked_spans
                  WHERE rn = 1 {sortOrder} {limitClause} |]
        , True
        )
      _ ->
        case qc.finalSummarizeQuery of
          Just binInterval ->
            let bucketExpr = timeBucketExpr binInterval
                -- jsonb_build_array(...) args cannot carry @AS alias@, so strip
                -- trailing aliases from aggregations / projected cols here.
                cols = colsNoAsClause $ if null qc.aggregations then qc.select else qc.aggregations
                selectCols = T.intercalate "," $ filter (not . T.isInfixOf "time_bucket") cols
                selectPart = if T.null selectCols then "" else selectCols <> ", "
                args = "extract(epoch from " <> bucketExpr <> ")::integer, " <> selectPart <> countOver
             in ( [fmt|SELECT {wrap args}
                   FROM {fromTable}
                   WHERE {buildWhere}
                   GROUP BY {bucketExpr} {havingClause}
                   ORDER BY {bucketExpr} DESC
                   {limitClause} |]
                , True
                )
          Nothing
            | not (null qc.aggregations) && not (null qc.groupByClause) ->
                ( [fmt|SELECT {wrap (T.intercalate "," (colsNoAsClause qc.aggregations) <> ", " <> countOver)} FROM {fromTable}
                   WHERE {buildWhere}
                   {groupByClause} {havingClause} {sortOrder} {limitClause} |]
                , True
                )
            | otherwise ->
                ( [fmt|SELECT {wrap selectClause} FROM {fromTable}
                   WHERE {buildWhere}
                   {groupByClause} {havingClause} {sortOrder} {overflowLimitClause} |]
                , False
                )

    -- Generate the summarize query depending on bin functions and data type
    -- Percentiles info is extracted directly from AST in applySectionToComponent
    -- The fieldExpr comes from parser-validated SubjectExpr and is safe for SQL interpolation
    summarizeQuery =
      case qc.finalSummarizeQuery of
        Just binInterval ->
          case qc.percentilesInfo of
            Just (fieldExpr, pcts) ->
              let bucketExpr = timeBucketExpr binInterval
                  percentiles =
                    T.intercalate
                      ", "
                      [ "(" <> show (p / 100.0) <> ", 'p" <> show (round p :: Int) <> "')"
                      | p <- pcts
                      ]
               in [fmt|WITH bucket_digests AS (SELECT extract(epoch from {bucketExpr})::integer AS timeB,
                    percentile_agg(CAST({fieldExpr} AS DOUBLE PRECISION)) AS digest
                  FROM {fromTable}
                  WHERE {buildWhere}
                  GROUP BY timeB
                  HAVING COUNT(*) > 0) SELECT b.timeB, q.quantile,
                         COALESCE(approx_percentile(q.percentile, b.digest), 0)::float AS value
                  FROM bucket_digests b
                  CROSS JOIN (VALUES {percentiles}) AS q(percentile, quantile)
                  ORDER BY b.timeB DESC, q.quantile {limitClause}|]
            Nothing ->
              -- Normal summarize query
              let resolve = resolveExtendedColumn (Map.fromList qc.extendedColumns)
                  groupCol =
                    if null qc.groupByClause
                      then "'" <> (if null qc.aggregations then "count" else "value") <> "'"
                      else "COALESCE(" <> maybe "status_code" resolve (listToMaybe qc.groupByClause) <> "::text, 'null')"
                  aggCol = fromMaybe "count(*)::float" (listToMaybe qc.aggregations)
                  bucketExpr = timeBucketExpr binInterval
                  groupByPart = if null qc.groupByClause then bucketExpr else bucketExpr <> ", " <> groupCol
               in [fmt|SELECT extract(epoch from {bucketExpr})::integer, {groupCol}, {aggCol}
                      FROM {fromTable} WHERE {buildWhere} GROUP BY {groupByPart} {havingClause}
                      ORDER BY {bucketExpr} DESC {limitClause}|]
        Nothing ->
          let hasAggregationsNoGroupBy = not (null qc.aggregations) && null qc.groupByClause
              resolve = resolveExtendedColumn (Map.fromList qc.extendedColumns)
              -- `summarize <agg> by <field>` with no time bin has to project the
              -- grouping column alongside the aggregate, exactly as the binned
              -- branch above emits (bucket, group, agg). Selecting the aggregate
              -- alone yields a bare column of numbers with nothing saying which
              -- group each belongs to — unreadable, and no decoder accepts it.
              groupCols = ["COALESCE(" <> resolve c <> "::text, 'null')" | c <- qc.groupByClause]
              -- The DTText decoder this shape is read with takes every column as
              -- text (see Charts.decoderFor), so the aggregate is cast too —
              -- otherwise a float4 count fails to decode and the whole query
              -- reports as failed.
              asText col = let (expr, aliasM) = splitTrailingAlias col in "(" <> expr <> ")::text" <> maybe "" (" AS " <>) aliasM
              selectCols
                | null qc.aggregations = qc.select
                -- No `by` at all is a *scalar* summarize, which Charts.decoderFor
                -- reads as DTFloat — casting that to text breaks the decode. Only
                -- the grouped shape goes through the all-text DTText decoder.
                | null qc.groupByClause = qc.aggregations
                | otherwise = groupCols <> map asText qc.aggregations
              -- buildOrderBy honours an explicit `| sort by`; hand-rolling the
              -- clause here silently dropped it, so `summarize … by svc | sort
              -- by count_ desc` came back alphabetical.
              orderClause = if hasAggregationsNoGroupBy then "" else " " <> buildOrderBy qc
              limitPart = if hasAggregationsNoGroupBy then "" else limitClause
           in [fmt|SELECT {T.intercalate "," selectCols} FROM {fromTable} WHERE {buildWhere}
              {groupByClause} {havingClause}{orderClause} {limitPart}|]

    -- Alert queries reuse whereCondition but drop the date range/cursor for this
    -- recency filter; without it time_bucket groups across all history and max
    -- returns the all-time peak bucket.
    alertTimeFilter = timestampCol <> " >= NOW() - INTERVAL '" <> show sqlCfg.alertLookbackMins <> " minutes'"
    -- The alert query is built by hand rather than through 'buildWhere', so the environment
    -- has to be spliced here too — a monitor scoped to staging that silently alerts on prod
    -- is worse than one that never fires.
    alertEnvFilter = if T.null nq.nqEnvironment then "" else "AND " <> nq.nqEnvironment
    -- With a bin function the alert reads only the most recent bucket's value.
    alertTail = case qc.finalSummarizeQuery of
      Just binInterval -> let e = timeBucketExpr binInterval in "GROUP BY " <> e <> " ORDER BY " <> e <> " DESC LIMIT 1"
      Nothing -> buildGroupBy nq.nqExtendedColumns qc.groupByClause
    -- FIXME: render the selection from the aggregations, but without the aliases
    alertQuery =
      [fmt|
          SELECT GREATEST( count(*)::float8) FROM {fromTable}
          WHERE project_id='{sqlCfg.pid.toText}' AND {alertTimeFilter} {alertEnvFilter} AND ({whereCondition})
          {alertTail}
        |]
   in
    ( finalSqlQuery
    , qc
        { finalColumns = listToColNames nq.nqSelectCols
        , finalSqlQuery = finalSqlQuery
        , hasCountOver = countOverIncluded
        , whereClause = Just whereCondition
        , finalSummarizeQuery = Just summarizeQuery
        , finalAlertQuery = Just alertQuery
        }
    )


-- | Parse a monoscope query to components ready for database execution.
--
-- >>> let cfg = defSqlQueryCfg defPid fixedUTCTime Nothing Nothing
-- >>> let Right (q, c) = parseQueryToComponents cfg "kind == \"server\""
-- >>> T.isPrefixOf "SELECT jsonb_build_array(" (T.stripStart q)
-- True
-- >>> c.hasCountOver
-- False
-- >>> T.isInfixOf "kind = 'server'" q
-- True
--
-- A filter after a summarize reads the aggregated rows, so it lowers to HAVING
-- with the alias resolved — as a WHERE it referenced a column that does not
-- exist yet and the query died at the database:
-- >>> let Right (q5, _) = parseQueryToComponents cfg "kind == \"server\" | summarize count() by kind | where count_ > 1"
-- >>> T.isInfixOf "HAVING ((count(*)::float) > 1)" q5
-- True
-- >>> T.isInfixOf "count_ > 1" q5
-- False
--
-- An `extend` alias is not an aggregate, so it stays in the WHERE:
-- >>> let Right (_, c6) = parseQueryToComponents cfg "kind == \"log\" | extend slow = duration"
-- >>> c6.havingClause
-- Nothing
--
-- >>> let Right (q2, c2) = parseQueryToComponents cfg "kind == \"server\" | summarize count(*) by bin(timestamp, 1h)"
-- >>> c2.hasCountOver
-- True
-- >>> T.isInfixOf "count(*) OVER()" q2
-- True
-- >>> T.isInfixOf "as _total_count" q2
-- False
--
-- Comparing to an empty list literal has no jsonpath form, so it lowers to a
-- non-matching predicate rather than invalid jsonpath:
-- >>> let Right (_, c3) = parseQueryToComponents cfg "errors[*].error_type == []"
-- >>> c3.whereClause
-- Just "(false)"
--
-- >>> let Right (_, c4) = parseQueryToComponents cfg "errors[*].error_type =~ /^ab.*c/"
-- >>> c4.whereClause
-- Just "(jsonb_path_exists(to_jsonb(errors), '$[*].\"error_type\" ? (@ like_regex \"^ab.*c\" flag \"i\")'::jsonpath))"
--
-- Field validation is told the cfg's source for the same reason 'queryASTToComponents'
-- resolves the FROM table from it: a metrics query can arrive with its source in the
-- request rather than in the query text, and validating those fields against
-- @otel_logs_and_spans@ rejects the metrics table's own columns.
parseQueryToComponents :: SqlQueryCfg -> Text -> Either Text (Text, QueryComponents)
parseQueryToComponents sqlCfg = fmap (queryASTToComponents sqlCfg) . first (.message) . parseQueryDiagnosed sqlCfg.source


queryASTToComponents :: SqlQueryCfg -> [Section] -> (Text, QueryComponents)
queryASTToComponents sqlCfg sections =
  let effectiveSource = sqlCfg.source <|> listToMaybe [s | Source s <- sections]
      qc = sectionsToComponents sqlCfg $ rewriteSectionsForSource sqlCfg.metricJsonAsVariant effectiveSource sections
   in -- The FROM table follows the effective source (cfg or an explicit `source`
      -- section); without this a metrics query built via the cfg arg alone would
      -- silently read otel_logs_and_spans.
      sqlFromQueryComponents sqlCfg qc{fromTable = qc.fromTable <|> (display <$> effectiveSource)}


defPid :: Projects.ProjectId
defPid = def


-- | Fixed clock for tests and hardcoded queries.
fixedUTCTime :: UTCTime
fixedUTCTime = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)


-- | Bin width for a date range: the shortest bucket that keeps the series readable.
calculateAutoBinWidth :: (Maybe UTCTime, Maybe UTCTime) -> UTCTime -> Text
calculateAutoBinWidth (startM, endM) currentTime
  | minutes <= 2 = "1 second"
  | minutes <= 5 = "5 seconds"
  | minutes <= 15 = "10 seconds"
  | hours <= 1 = "30 seconds"
  | hours <= 6 = "1 minutes"
  | hours <= 14 = "5 minutes"
  | hours <= 48 = "10 minutes"
  | days < 7 = "1 hour"
  | days < 30 = "6 hours"
  | otherwise = "1 day"
  where
    -- Missing bounds default to the 14-day window the UI defaults to, ending now.
    startTime = fromMaybe (addUTCTime (-(14 * 24 * 60 * 60)) currentTime) startM
    minutes = realToFrac (diffUTCTime (fromMaybe currentTime endM) startTime) / 60 :: Double
    hours = minutes / 60
    days = hours / 24


defSqlQueryCfg :: Projects.ProjectId -> UTCTime -> Maybe Sources -> Maybe Text -> SqlQueryCfg
defSqlQueryCfg pid currentTime source spanT =
  SqlQueryCfg
    { pid
    , presetRollup = Nothing
    , cursorM = Nothing
    , dateRange = (Nothing, Nothing)
    , source = source
    , metricJsonAsVariant = False
    , targetSpansM = spanT
    , projectedColsByUser = []
    , currentTime
    , defaultSelect = defaultSelectSqlQuery source
    , alertLookbackMins = 60
    , environment = Nothing
    }


-- ISO 8601 with explicit Z suffix; works natively on both PG and TF.
timestampLogFmt :: Text -> Text
timestampLogFmt colName = [fmt|to_char({colName} at time zone 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"') as {colName}|]


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
  , "parent_id"
  , "CAST(EXTRACT(EPOCH FROM (start_time)) * 1000000000 AS BIGINT) as start_time_ns"
  , -- Error flag the trace waterfall uses to light the red badge on a row and bubble it
    -- to ancestors. For spans it's the presence of structured error data (status-only
    -- error spans are caught client-side via the summary marker); for logs, which carry
    -- no `errors` payload, we apply LogQueries.is_error (level/severity/status_code)
    -- scoped to kind='log', so ERROR/FATAL logs also propagate the badge to their parent.
    "COALESCE(errors is not null OR (kind = 'log' AND (lower(level) = 'error' OR severity___severity_number >= 17 OR status_code = 'ERROR')), false) as errors"
  , "summary"
  , "context___span_id as latency_breakdown"
  , "kind"
  ]


-- | Split on a *trailing* @ as <ident>@ — case-insensitive (aggregations
-- emit @AS@, projections emit @as@). Internal @AS@ inside @CAST(x AS T)@
-- or string literals stays untouched.
--
-- >>> splitTrailingAlias "JSONB_ARRAY_LENGTH(errors) as errors_count"
-- ("JSONB_ARRAY_LENGTH(errors)",Just "errors_count")
-- >>> splitTrailingAlias "sum(x)::float AS total"
-- ("sum(x)::float",Just "total")
-- >>> splitTrailingAlias "CAST(x AS VARCHAR)"
-- ("CAST(x AS VARCHAR)",Nothing)
--
-- 'normalizeKeyPath' aliases dotted/bracketed paths to @•❲❳@; those must still strip,
-- else the @AS@ leaks into @jsonb_build_array(…)@ and the query fails to parse:
--
-- >>> fst $ splitTrailingAlias "resource___service___name as resource•service•name"
-- "resource___service___name"
splitTrailingAlias :: Text -> (Text, Maybe Text)
splitTrailingAlias (T.strip -> t) =
  let asNeedle = " as "
      (preLower, _) = T.breakOnEnd asNeedle (T.toLower t)
      preLen = T.length preLower
   in if T.null preLower
        then (t, Nothing)
        else
          let pre = T.take preLen t
              alias = T.strip $ T.drop preLen t
           in if T.null alias || not (T.all (\c -> isAlphaNum c || c `elem` ("_•❲❳" :: [Char])) alias)
                then (t, Nothing)
                else (T.strip $ T.dropEnd (T.length asNeedle) pre, Just alias)


-- | Output names of a select list: the alias when present, else the expression.
--
-- >>> listToColNames ["id", "JSONB_ARRAY_LENGTH(errors) as errors_count"]
-- ["id","errors_count"]
listToColNames :: [Text] -> [Text]
listToColNames = map (uncurry fromMaybe . splitTrailingAlias)


-- | Select list with trailing aliases stripped (jsonb_build_array args can't carry @AS@).
--
-- >>> colsNoAsClause ["id", "JSONB_ARRAY_LENGTH(errors) as errors_count"]
-- ["id","JSONB_ARRAY_LENGTH(errors)"]
colsNoAsClause :: [Text] -> [Text]
colsNoAsClause = map (fst . splitTrailingAlias)


instance HasField "toColNames" QueryComponents [Text] where
  getField qc = qc.finalColumns


-- | Replace all occurrences of {{key}} in the input text using the provided mapping.
-- Unknown placeholders are left as-is (useful for debugging).
--
-- >>> replacePlaceholders (Map.fromList [("name", "world")]) "Hello {{name}}!"
-- "Hello world!"
-- >>> replacePlaceholders (Map.fromList [("a", "1"), ("b", "2")]) "{{a}} + {{b}}"
-- "1 + 2"
-- >>> replacePlaceholders Map.empty "{{missing}}"
-- "{{missing}}"
replacePlaceholders :: Map.Map Text Text -> Text -> Text
replacePlaceholders mappng txt = Map.foldlWithKey' (\t k v -> T.replace ("{{" <> k <> "}}") v t) txt mappng


variablePresets :: Text -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> Map.Map Text Text
variablePresets pid mf mt allParams currentTime =
  let fmtUTC = maybe "" formatUTC
      andPrefix = (" AND " <>)
      bound op t field' = field' <> " " <> op <> " '" <> formatUTC t <> "'"
      clause field = case catMaybes [bound ">=" <$> mf, bound "<=" <$> mt] of
        [] -> ""
        bs -> andPrefix $ "(" <> T.intercalate " AND " (map ($ field) bs) <> ")"
   in Map.fromList
        $ [ ("project_id", pid)
          , ("from", andPrefix $ fmtUTC mf)
          , ("to", andPrefix $ fmtUTC mt)
          , ("time_filter", clause "timestamp")
          , ("time_filter_sql_created_at", clause "created_at")
          , ("rollup_interval", calculateAutoBinWidth (mf, mt) currentTime)
          ]
        <> (allParams <&> second maybeToMonoid)


-- | Like variablePresets but uses KQL format for constants.
variablePresetsKQL :: Text -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> Map.Map Text Text
variablePresetsKQL pid mf mt allParams currentTime =
  let basePresets = variablePresets pid mf mt allParams currentTime
      paramsMap = Map.fromList [(k, fromMaybe "" v) | (k, v) <- allParams]
      kqlRemapping = Map.fromList [(k, v) | (k, _) <- allParams, "const-" `T.isPrefixOf` k, not ("-kql" `T.isSuffixOf` k), Just v <- [Map.lookup (k <> "-kql") paramsMap]]
      filteredBase = Map.filterWithKey (\k _ -> not ("-kql" `T.isSuffixOf` k)) basePresets
   in Map.union kqlRemapping filteredBase


-- | First column of each row as a SQL @IN@ list; empty stays valid but matches nothing.
--
-- >>> constantToSQLList [["api/users"], ["api/orders"]]
-- "('api/users', 'api/orders')"
-- >>> constantToSQLList [["foo'bar"]]
-- "('foo''bar')"
-- >>> constantToSQLList []
-- "(SELECT NULL::text WHERE FALSE)"
constantToSQLList :: [[Text]] -> Text
constantToSQLList = \case
  [] -> "(SELECT NULL::text WHERE FALSE)"
  rows -> "(" <> T.intercalate ", " [sqlStringLit v | (v : _) <- rows] <> ")"


-- | Same as 'constantToSQLList' but in KQL literal syntax.
--
-- >>> constantToKQLList [["api/users"], ["api/orders"]]
-- "(\"api/users\", \"api/orders\")"
-- >>> constantToKQLList [["foo\"bar"]]
-- "(\"foo\\\"bar\")"
-- >>> constantToKQLList [["back\\slash"]]
-- "(\"back\\\\slash\")"
-- >>> constantToKQLList []
-- "(\"__EMPTY_CONST__\")"
constantToKQLList :: [[Text]] -> Text
constantToKQLList = \case
  [] -> "(\"__EMPTY_CONST__\")" -- Sentinel value - valid syntax but won't match real data
  rows -> "(" <> T.intercalate ", " [escapeDoubleQuote v | (v : _) <- rows] <> ")"
  where
    escapeDoubleQuote v = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" v) <> "\""
