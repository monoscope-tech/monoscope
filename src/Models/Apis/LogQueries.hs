module Models.Apis.LogQueries (
  SDKTypes (..),
  RequestTypes (..),
  ATError (..),
  PatternRow (..),
  SessionRow (..),
  SessionSummary (..),
  normalizeUrlPath,
  selectLogTable,
  executeArbitraryQuery,
  executeSecuredQuery,
  logExplorerUrlPath,
  getLastSevenDaysTotalRequest,
  fetchLogPatterns,
  fetchSessions,
  fetchSessionSummary,
  ExpandKind (..),
  fetchEventExamples,
  selectChildSpansAndLogs,
  sessionUserDisplay,
  templateToLike,
  bucketWidthSecs,
  bucketRange,
  densifyBuckets,
  aggregatePageSize,
)
where

import Control.Exception.Annotated (checkpoint, try)
import Control.Lens (view, _5)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.Fields ()
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB, WrappedEnumShow (..), rawSql)
import Pkg.Drain qualified as Drain
import Pkg.Parser
import Pkg.Parser.Expr (flattenedOtelAttributes, transformFlattenedAttribute)
import Pkg.Parser.Stats (Section, Sources (SSpans))
import Relude hiding (many, some)
import System.Logging qualified as Log
import Utils (replaceAllFormats)
import Web.HttpApiData (ToHttpApiData (..))


data SDKTypes
  = GoGin
  | GoBuiltIn
  | GoGorillaMux
  | GoFiber
  | GoDefault
  | GoOutgoing
  | PhpLaravel
  | PhpSymfony
  | JsExpress
  | JsNest
  | JsFastify
  | JsAdonis
  | JsNext
  | JavaSpringBoot
  | JsAxiosOutgoing
  | JsOutgoing
  | DotNet
  | PythonFastApi
  | PythonFlask
  | PythonDjango
  | PythonOutgoing
  | PhpSlim
  | GuzzleOutgoing
  | ElixirPhoenix
  | PythonPyramid
  | DotNetOutgoing
  | TestkitOutgoing
  | JavaSpring
  | JavaApacheOutgoing
  | JavaVertx
  | SDKUnknown
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SDKTypes
  deriving (FromField, ToField) via WrappedEnumShow SDKTypes


data RequestTypes
  = Incoming
  | Outgoing
  | Background
  | System
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestTypes
  deriving (FromField, ToField) via WrappedEnumShow RequestTypes


-- normalize URLPatg based off the SDKTypes. Should allow us have custom logic to parse and transform url paths into a form we are happy with, per library
-- >>> normalizeUrlPath GoGin 200 "GET" "https://monoscope.tech/abc/:bla?q=abc"
-- "https://monoscope.tech/abc/:bla"
-- >>> normalizeUrlPath GoGin 200 "GET" "/abc/:bla?q=abc"
-- "/abc/:bla"
--
-- >>> normalizeUrlPath GoGin 404 "GET" "https://monoscope.tech/abc/:bla?q=abc"
-- ""
--
-- >>> normalizeUrlPath JsExpress 200 "OPTIONS" "https://monoscope.tech/abc/:bla?q=abc"
-- ""
-- >>> normalizeUrlPath JsExpress 200 "PATCH" "https://monoscope.tech/abc/:bla?q=abc"
-- "https://monoscope.tech/abc/:bla"
--
normalizeUrlPath :: SDKTypes -> Int -> Text -> Text -> Text
-- NOTE: Temporary workaround due to storing complex paths in the urlPath, which should be unaccepted, and messes with our logic
normalizeUrlPath JsExpress _ "OPTIONS" _ = ""
normalizeUrlPath _ statusCode _ urlPath = removeQueryParams statusCode urlPath


-- removeQueryParams ...
-- >>> removeQueryParams 200 "https://monoscope.tech/abc/:bla?q=abc"
--
-- Function to remove the query parameter section from a URL
removeQueryParams :: Int -> Text -> Text
removeQueryParams 404 _ = ""
removeQueryParams _ urlPath = T.takeWhile (/= '?') urlPath


data ATError = ATError
  { projectId :: Maybe Projects.ProjectId
  , when :: UTCTime
  , errorType :: Text
  , rootErrorType :: Text
  , message :: Text
  , rootErrorMessage :: Text
  , stackTrace :: Text
  , hash :: Maybe Text
  , technology :: Maybe SDKTypes
  , requestMethod :: Maybe Text
  , requestPath :: Maybe Text
  , spanId :: Maybe Text
  , traceId :: Maybe Text
  , serviceName :: Maybe Text
  , stack :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson ATError
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ATError


incrementByOneMillisecond :: String -> String
incrementByOneMillisecond dateStr =
  case maybeTime of
    Nothing -> ""
    Just utcTime ->
      let newTime = posixSecondsToUTCTime $ utcTimeToPOSIXSeconds utcTime + 0.000001
       in iso8601Show newTime
  where
    maybeTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" dateStr :: Maybe UTCTime


logExplorerUrlPath :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Bool -> Text
logExplorerUrlPath pid q cols cursor since fromV toV layout source recent = "/p/" <> pid.toText <> "/log_explorer?" <> T.intercalate "&" params
  where
    recentTo = cursor >>= (\x -> Just (toText . incrementByOneMillisecond . toString $ x))
    params =
      catMaybes
        [ Just "json=true"
        , fmap ("query=" <>) (toQueryParam <$> q)
        , fmap ("cols=" <>) (toQueryParam <$> cols)
        , if recent then Nothing else fmap ("cursor=" <>) (toQueryParam <$> cursor)
        , if recent then Nothing else fmap ("since=" <>) (toQueryParam <$> since)
        , fmap ("from=" <>) (toQueryParam <$> fromV)
        , if recent then fmap ("to=" <>) (toQueryParam <$> recentTo) else fmap ("to=" <>) (toQueryParam <$> toV)
        , fmap ("layout=" <>) (toQueryParam <$> layout)
        , fmap ("source=" <>) (toQueryParam <$> source)
        ]


-- | Execute arbitrary SQL query and return results as vector of vectors.
-- Wraps in row_to_json + json_each to preserve column order via hasql.
executeArbitraryQuery :: DB es => HI.Sql -> Eff es (V.Vector (V.Vector AE.Value))
executeArbitraryQuery querySql = do
  results :: [AE.Value] <-
    Hasql.interp
      $ rawSql "SELECT (SELECT json_agg(x.value ORDER BY x.ordinality)::jsonb FROM json_each(row_to_json(sub.*)) WITH ORDINALITY AS x) FROM ("
      <> querySql
      <> rawSql ") AS sub"
  pure $ V.fromList $ mapMaybe jsonArrayToVector results


-- | Execute a user-provided SQL query with mandatory project_id filtering.
-- SECURITY: Validates query for dangerous patterns and verifies project_id filter is present in query.
-- Note: The query must already contain project_id='<pid>' filtering (via {{project_id}} placeholder substitution done before calling this function)
executeSecuredQuery :: DB es => Projects.ProjectId -> Text -> Int -> Eff es (Either Text (V.Vector (V.Vector AE.Value)))
executeSecuredQuery pid userQuery limit
  | not (validateSqlQuery userQuery) = pure $ Left "Query contains disallowed operations"
  | not (hasProjectIdFilter userQuery pid) = pure $ Left "Query must filter by project_id"
  | otherwise = do
      let selectQuery = "SELECT (SELECT json_agg(x.value ORDER BY x.ordinality)::jsonb FROM json_each(row_to_json(sub.*)) WITH ORDINALITY AS x) FROM (SELECT * FROM (" <> userQuery <> ") AS subq LIMIT " <> show limit <> ") AS sub"
      resultE <- try @Hasql.HasqlException do
        results :: [AE.Value] <- Hasql.interp $ rawSql selectQuery
        pure $ V.fromList $ mapMaybe jsonArrayToVector results
      pure $ first (\e -> "Query execution failed: " <> toText (displayException e)) resultE


jsonArrayToVector :: AE.Value -> Maybe (V.Vector AE.Value)
jsonArrayToVector (AE.Array arr) = Just arr
jsonArrayToVector _ = Nothing


-- | Check that query contains a project_id filter with the correct project ID
-- This validates that the query has been properly prepared with the project_id substituted
hasProjectIdFilter :: Text -> Projects.ProjectId -> Bool
hasProjectIdFilter query pid =
  let pidText = pid.toText
   in ("project_id='" <> pidText <> "'")
        `T.isInfixOf` query
        || ("project_id = '" <> pidText <> "'")
        `T.isInfixOf` query


-- | Dangerous SQL patterns that must not appear in user queries
dangerousSqlPatterns :: [Text]
dangerousSqlPatterns =
  [ "insert "
  , "update "
  , "delete "
  , "drop "
  , "truncate "
  , "alter "
  , "create "
  , "grant "
  , "revoke "
  , "copy "
  , "execute "
  , "explain "
  , "set "
  , "; "
  , "--"
  , "/*"
  , "*/"
  , "information_schema"
  , "pg_catalog"
  , "pg_"
  ]


-- | Validate SQL query for dangerous constructs
validateSqlQuery :: Text -> Bool
validateSqlQuery query =
  let lowerQuery = T.toLower query
      -- Check for comment-based bypass attempts in original query
      hasComments = "--" `T.isInfixOf` query || "/*" `T.isInfixOf` query
   in not hasComments
        && all (\p -> not $ p `T.isInfixOf` lowerQuery) dangerousSqlPatterns
        && "select"
        `T.isInfixOf` lowerQuery


selectLogTable :: (DB es, Log :> es, Time.Time :> es) => Projects.ProjectId -> [Section] -> Text -> Maybe UTCTime -> (Maybe UTCTime, Maybe UTCTime) -> [Text] -> Maybe Sources -> Maybe Text -> Eff es (Either Text (V.Vector (V.Vector AE.Value), [Text], Int))
selectLogTable pid queryAST queryText cursorM dateRange projectedColsByUser source targetSpansM = do
  now <- Time.currentTime
  let (q, queryComponents) = queryASTToComponents ((defSqlQueryCfg pid now source targetSpansM){cursorM, dateRange, projectedColsByUser, source, targetSpansM}) queryAST

  Log.logTrace
    "Query Debug Info"
    ( AE.object
        [ "original_query_input" AE..= queryText
        , "parsed_query_ast" AE..= show queryAST
        , "generated_sql_query" AE..= q
        , "project_id" AE..= show pid
        , "source" AE..= source
        , "target_spans" AE..= fromMaybe "" targetSpansM
        , "time_range_from" AE..= fmap (toText . iso8601Show) (fst dateRange)
        , "time_range_to" AE..= fmap (toText . iso8601Show) (snd dateRange)
        , "current_time_used_for_defaults" AE..= toText (iso8601Show now)
        ]
    )

  result <- try @SomeException $ checkpoint (toAnnotation ("selectLogTable", q)) $ executeArbitraryQuery (rawSql q)
  case result of
    Left e -> pure $ Left $ show e
    Right logItemsV -> do
      let limit = fromMaybe defaultQueryLimit queryComponents.takeLimit
      if queryComponents.hasCountOver
        then do
          let dropLast v = V.take (V.length v - 1) v
              count = fromMaybe 0 $ do
                firstRow <- logItemsV V.!? 0
                AE.Number n <- firstRow V.!? (V.length firstRow - 1)
                pure $ round n
          pure $ Right (V.map dropLast logItemsV, queryComponents.toColNames, count)
        else do
          -- No count(*) OVER(): use overflow row to detect hasMore
          let hasOverflow = V.length logItemsV > limit
              rows = if hasOverflow then V.take limit logItemsV else logItemsV
              -- Signal hasMore by returning count > rows length
              count = if hasOverflow then V.length rows + 1 else V.length rows
          pure $ Right (rows, queryComponents.toColNames, count)


selectChildSpansAndLogs :: (DB es, Time.Time :> es) => Projects.ProjectId -> [Text] -> V.Vector Text -> (Maybe UTCTime, Maybe UTCTime) -> V.Vector Text -> Eff es [V.Vector AE.Value]
selectChildSpansAndLogs pid projectedColsByUser traceIds dateRange excludedSpanIds = do
  now <- Time.currentTime
  let qConfig = defSqlQueryCfg pid now (Just SSpans) Nothing
      (r, _) = getProcessedColumns projectedColsByUser qConfig.defaultSelect
  let pidText = pid.toText
      traceIdsList = V.toList traceIds
      excludedList = V.toList excludedSpanIds
      dateRangeSql = case dateRange of
        (Nothing, Just b) -> [HI.sql| AND timestamp BETWEEN #{b} AND #{now} |]
        (Just a, Just b) -> let a' = addUTCTime (-30) a; b' = addUTCTime 30 b in [HI.sql| AND timestamp BETWEEN #{a'} AND #{b'} |]
        _ -> mempty
  results <- Hasql.interp $ rawSql ("SELECT json_build_array(" <> r <> ")::jsonb FROM otel_logs_and_spans WHERE project_id=") <> [HI.sql|#{pidText}::text|] <> dateRangeSql <> [HI.sql| AND context___trace_id=ANY(#{traceIdsList}) AND parent_id IS NOT NULL AND id::text != ALL(#{excludedList}) ORDER BY timestamp DESC LIMIT 2000|]
  pure $ mapMaybe valueToVector results


valueToVector :: AE.Value -> Maybe (V.Vector AE.Value)
valueToVector val = case val of
  AE.Array arr -> Just arr
  _ -> Nothing


data PatternRow = PatternRow {logPattern :: Text, count :: Int64, level :: Maybe Text, service :: Maybe Text, volume :: [Int], mergedCount :: Int}


-- | Display name for a session row: prefers email, then name, then user ID.
--
-- >>> sessionUserDisplay (Just "alice@example.com") (Just "Alice") (Just "u1")
-- "alice@example.com"
-- >>> sessionUserDisplay Nothing Nothing (Just "u1")
-- "u1"
-- >>> sessionUserDisplay Nothing Nothing Nothing
-- "\8212"
sessionUserDisplay :: Maybe Text -> Maybe Text -> Maybe Text -> Text
sessionUserDisplay email name uid = fromMaybe "\8212" $ email <|> name <|> uid


-- | One aggregated session row for the Sessions visualization.
--
-- @landingUrl@, @userAgent@, and @firstError@ are first-observed-by-timestamp
-- values taken from the raw events in the session. They're what turns a row
-- from "some session by some user" into something a support engineer can
-- recognize at a glance (where did they land, what are they on, what broke).
data SessionRow = SessionRow
  { sessionId :: Text
  , userId :: Maybe Text
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  , eventCount :: Int64
  , errorCount :: Int64
  , firstSeen :: UTCTime
  , lastSeen :: UTCTime
  , durationNs :: Int64
  , traceCount :: Int64
  , services :: V.Vector Text
  , volume :: [Int]
  , landingUrl :: Maybe Text
  , userAgent :: Maybe Text
  , firstError :: Maybe Text
  }
  deriving stock (Generic, Show)


-- | Mirrors the column order of the SELECT in 'fetchSessions'. Decoded
-- directly via hasql to bypass the row_to_json/json_each wrapper used by
-- 'executeArbitraryQuery' — that wrapper trips PostgreSQL's JSON parser
-- whenever a TEXT value (e.g. a user_agent or url_path captured from raw
-- OTLP) contains characters JSON forbids (NULL bytes, lone surrogates).
data RawSessionRow = RawSessionRow
  { sessionId :: Text
  , userId :: Maybe Text
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  , eventCount :: Int64
  , errorCount :: Int64
  , firstSeen :: UTCTime
  , lastSeen :: UTCTime
  , durationNs :: Int64
  , traceCount :: Int64
  , totalSessions :: Int32
  , services :: V.Vector Text
  , bis :: V.Vector Int32
  , cnts :: V.Vector Int32
  , landingUrl :: Maybe Text
  , userAgent :: Maybe Text
  , firstError :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | Build a WHERE body always scoped to the project. `QueryComponents.whereClause`
-- carries only user-supplied filters (see `Pkg.Parser.sqlFromQueryComponents`
-- where `project_id` is attached separately via `buildWhere`), so any caller
-- that embeds `whereClause` into its own SQL must prepend project scoping
-- here — otherwise a non-empty user filter silently bypasses project isolation.
--
-- >>> scopedWhere "p1" Nothing
-- "project_id='p1'"
-- >>> scopedWhere "p1" (Just "")
-- "project_id='p1'"
-- >>> scopedWhere "p1" (Just "level='error'")
-- "project_id='p1' AND (level='error')"
scopedWhere :: Text -> Maybe Text -> Text
scopedWhere pidTxt mUser =
  let base = "project_id='" <> pidTxt <> "'"
   in case mUser of
        Just w | not (T.null w) -> base <> " AND (" <> w <> ")"
        _ -> base


fetchLogPatterns
  :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es)
  => Bool
  -- ^ enableTimefusionReads (caller threads it through — keeps this module a
  -- leaf of `System.Config` so `Telemetry.OtelLogsAndSpans` can be named in
  -- `AuthContext.extractionWorker` without a module cycle).
  -> Projects.ProjectId
  -> [Section]
  -> (Maybe UTCTime, Maybe UTCTime)
  -> Maybe Sources
  -> Maybe Text
  -> Int
  -> Eff es (Int, [PatternRow])
fetchLogPatterns enableTfReads pid queryAST dateRange sourceM targetM skip = do
  now <- Time.currentTime
  let sqlCfg = (defSqlQueryCfg pid now sourceM Nothing){dateRange}
      (_, queryComponents) = queryASTToComponents sqlCfg queryAST
      pidTxt = pid.toText
      dateRangeClause = buildDateRange sqlCfg
      whereCondition = scopedWhere pidTxt queryComponents.whereClause
      fullWhere = whereCondition <> if T.null dateRangeClause then "" else " AND " <> dateRangeClause
      target = fromMaybe "summary" targetM
  Log.logTrace "fetchLogPatterns: start"
    $ AE.object
      ["project_id" AE..= pid, "target" AE..= target, "skip" AE..= skip, "date_range" AE..= show dateRange]
  let (dateFrom, dateTo) = dateRange
  precomputed :: [(Text, Int64, Maybe Text, Maybe Text, Text, Int, Int)] <-
    if target `elem` map fst LogPatterns.knownPatternFields
      then Hasql.interp [HI.sql|SELECT lp.log_pattern, (lp.occurrence_count + COALESCE(m.member_count, 0))::BIGINT as total_count, lp.log_level, lp.service_name, lp.pattern_hash, COALESCE(m.member_cnt, 0)::INT as merged_count, COUNT(*) OVER()::INT as total_patterns FROM apis.log_patterns lp LEFT JOIN LATERAL (SELECT SUM(occurrence_count) as member_count, COUNT(*)::INT as member_cnt FROM apis.log_patterns WHERE canonical_id = lp.id) m ON TRUE WHERE lp.project_id = #{pid} AND lp.source_field = #{target} AND lp.canonical_id IS NULL AND (#{dateFrom} IS NULL OR lp.last_seen_at >= #{dateFrom}) AND (#{dateTo} IS NULL OR lp.last_seen_at <= #{dateTo}) ORDER BY total_count DESC OFFSET #{skip} LIMIT #{aggregatePageSize}|]
      else pure []
  if not (null precomputed)
    then do
      Log.logTrace "fetchLogPatterns: using precomputed" $ AE.object ["count" AE..= length precomputed]
      let totalPatterns = maybe 0 (\(_, _, _, _, _, _, n) -> n) $ listToMaybe precomputed
          hashes = V.fromList $ map (view _5) precomputed
          hourlyFrom = fromMaybe (addUTCTime (-(24 * 3600)) now) dateFrom
          hourlyTo = fromMaybe now dateTo
      -- Fetch member hashes for merged patterns so their hourly stats are included
      memberHashMap :: HM.HashMap Text [Text] <- HM.fromListWith (++) . map (\(canonical, mHash) -> (canonical, [mHash])) <$> Hasql.interp [HI.sql|SELECT c.pattern_hash, m.pattern_hash FROM apis.log_patterns m JOIN apis.log_patterns c ON m.canonical_id = c.id WHERE c.project_id = #{pid} AND c.source_field = #{target} AND c.pattern_hash = ANY(#{hashes})|]
      let allHashes = V.fromList $ concatMap (\h -> h : fromMaybe [] (HM.lookup h memberHashMap)) $ V.toList hashes
      hourlyRows :: [(Text, UTCTime, Int)] <- Hasql.interp [HI.sql|SELECT pattern_hash, hour_bucket, event_count::INT FROM apis.log_pattern_hourly_stats WHERE project_id = #{pid} AND source_field = #{target} AND pattern_hash = ANY(#{allHashes}) AND hour_bucket >= #{hourlyFrom} AND hour_bucket <= #{hourlyTo} ORDER BY pattern_hash, hour_bucket|]
      let volumeMap = HM.fromListWith (++) [(h, [(t, c)]) | (h, t, c) <- hourlyRows]
          lookupVolume h = buildHourlyBuckets now $ concatMap (\mh -> fromMaybe [] $ HM.lookup mh volumeMap) (h : fromMaybe [] (HM.lookup h memberHashMap))
      pure (totalPatterns, [PatternRow{logPattern = pat, count = cnt, level = lvl, service = svc, volume = lookupVolume h, mergedCount = mc} | (pat, cnt, lvl, svc, h, mc, _) <- precomputed])
    else do
      Log.logTrace "fetchLogPatterns: falling back to on-the-fly query" $ AE.object ["full_where" AE..= fullWhere]
      let bucketW = bucketWidthSecs dateRange now
          bucketCol = "floor(extract(epoch from timestamp) / " <> show bucketW <> ")::INT"
      rawResults :: [(Text, Int, Int, Maybe Text, Maybe Text)] <- Hasql.withHasqlTimefusion enableTfReads case resolveFieldExpr target of
        Just (Left colExpr) ->
          Hasql.interp $ rawSql ("SELECT " <> colExpr <> "::text, " <> bucketCol <> " as bi, count(*)::INT as cnt, mode() WITHIN GROUP (ORDER BY level) as lvl, mode() WITHIN GROUP (ORDER BY resource___service___name) as svc FROM otel_logs_and_spans WHERE project_id=") <> [HI.sql|#{pidTxt} |] <> rawSql (" AND " <> fullWhere <> " AND " <> colExpr <> " IS NOT NULL GROUP BY 1, 2 ORDER BY cnt DESC LIMIT 20000")
        Just (Right pathParts) ->
          let pathPartsList = V.toList pathParts
           in Hasql.interp $ [HI.sql|SELECT attributes #>> #{pathPartsList}, |] <> rawSql (bucketCol <> " as bi, count(*)::INT as cnt, mode() WITHIN GROUP (ORDER BY level) as lvl, mode() WITHIN GROUP (ORDER BY resource___service___name) as svc FROM otel_logs_and_spans WHERE project_id=") <> [HI.sql|#{pidTxt} |] <> rawSql (" AND " <> fullWhere) <> [HI.sql| AND attributes #>> #{pathPartsList} IS NOT NULL GROUP BY 1, 2 ORDER BY cnt DESC LIMIT 20000|]
        Nothing -> pure []
      Log.logTrace "fetchLogPatterns: on-the-fly query done" $ AE.object ["raw_results" AE..= length rawResults]
      let agg (c1, l1, s1, b1) (c2, l2, s2, b2) = (c1 + c2, l1 <|> l2, s1 <|> s2, b1 ++ b2)
          grouped =
            HM.fromListWith
              agg
              [(replaceAllFormats val, (cnt, lvl, svc, [(bi, cnt)])) | (val, bi, cnt, lvl, svc) <- rawResults, not (T.null val)]
          drainTree =
            let keys = V.fromList $ HM.keys grouped
             in Drain.buildDrainTree Drain.tokenizeForDrain Relude.id (const Nothing) Drain.emptyDrainTree keys now
          merged =
            HM.fromListWith
              agg
              [ (dr.templateStr, foldl' agg (0, Nothing, Nothing, []) $ mapMaybe (`HM.lookup` grouped) $ V.toList dr.logIds)
              | dr <- V.toList $ Drain.getAllLogGroups drainTree
              ]
          sorted = take 100 $ drop skip $ sortOn (Down . (\(c, _, _, _) -> c) . snd) $ HM.toList merged
          range = bucketRange [bi | (_, (_, _, _, bs)) <- sorted, (bi, _) <- bs]
      Log.logTrace "fetchLogPatterns: normalization done" $ AE.object ["patterns" AE..= HM.size merged]
      pure (HM.size merged, [PatternRow{logPattern = pat, count = fromIntegral cnt, level = lvl, service = svc, volume = densifyBuckets range bs, mergedCount = 0} | (pat, (cnt, lvl, svc, bs)) <- sorted])
  where
    -- SAFETY: All Left branches produce safe column names from hardcoded whitelists
    -- (flattenedOtelAttributes, rootColumns). Right branch uses parameterized #>> operator.
    -- User input never reaches SQL as raw text — only as column lookups or parameterized values.
    resolveFieldExpr f
      | f == "url_path" = Just $ Left "attributes___url___path"
      | f == "exception" = Just $ Left "attributes___exception___message"
      | f == "summary" = Just $ Left "array_to_string(summary, chr(30))"
      | f `S.member` flattenedOtelAttributes = Just $ Left $ transformFlattenedAttribute f
      | f `elem` rootColumns = Just $ Left f
      | "attributes." `T.isPrefixOf` f = let parts = drop 1 $ T.splitOn "." f in if null parts || parts == [""] then Nothing else Just $ Right $ V.fromList parts
      | otherwise = Nothing
    rootColumns = ["body", "level", "kind", "name", "status_code", "status_message"] :: [Text]


-- | Fetch session-aggregated rows for the Sessions visualization tab.
-- Rows are keyed by attributes___session___id; user identity is taken as the
-- MAX non-null value within the session (stable for a given user). Pagination
-- happens before the hourly bucket/service joins so join cost tracks page size.
fetchSessions :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es) => Bool -> Projects.ProjectId -> [Section] -> (Maybe UTCTime, Maybe UTCTime) -> Maybe Text -> Int -> Eff es (Int, [SessionRow])
fetchSessions enableTfReads pid queryAST dateRange sortByM skip = do
  now <- Time.currentTime
  let sqlCfg = (defSqlQueryCfg pid now (Just SSpans) Nothing){dateRange}
      (_, queryComponents) = queryASTToComponents sqlCfg queryAST
      pidTxt = pid.toText
      dateRangeClause = buildDateRange sqlCfg
      whereCondition = scopedWhere pidTxt queryComponents.whereClause
      fullWhere = whereCondition <> if T.null dateRangeClause then "" else " AND " <> dateRangeClause
      bucketW = bucketWidthSecs dateRange now
      sortCol = case sortByM of
        Just "duration" -> "duration_ns" :: Text
        Just "errors" -> "error_count"
        Just "events" -> "event_count"
        Just "first_seen" -> "first_seen"
        _ -> "last_seen"
  Log.logTrace "fetchSessions: start"
    $ AE.object ["project_id" AE..= pid, "skip" AE..= skip, "date_range" AE..= show dateRange, "sort_by" AE..= sortByM]
  let sortColSql = rawSql sortCol
      whereSql = rawSql fullWhere
      q =
        [HI.sql|WITH filtered AS (
          SELECT attributes___session___id AS session_id,
            attributes___user___id AS user_id,
            attributes___user___email AS user_email,
            COALESCE(NULLIF(attributes___user___full_name, ''), NULLIF(attributes___user___name, '')) AS user_name,
            resource___service___name AS service_name,
            context___trace_id AS trace_id,
            attributes___url___path AS url_path,
            attributes___user_agent___original AS user_agent,
            COALESCE(NULLIF(status_message, ''), NULLIF(body::text, '')) AS error_text,
            (lower(level) = 'error' OR severity___severity_number >= 17 OR status_code = 'ERROR') AS is_error,
            timestamp, end_time, level, severity___severity_number, status_code,
            floor(extract(epoch from timestamp) / #{bucketW})::INT AS bi
          FROM otel_logs_and_spans
          WHERE |]
          <> whereSql
          <> [HI.sql|
            AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''
        ), agg AS (
          SELECT session_id,
            MAX(user_id) AS user_id, MAX(user_email) AS user_email, MAX(user_name) AS user_name,
            COUNT(*)::BIGINT AS event_count,
            COUNT(*) FILTER (WHERE is_error)::BIGINT AS error_count,
            MIN(timestamp) AS first_seen,
            MAX(COALESCE(end_time, timestamp)) AS last_seen,
            (EXTRACT(EPOCH FROM (MAX(COALESCE(end_time, timestamp)) - MIN(timestamp))) * 1000000000)::BIGINT AS duration_ns,
            COUNT(DISTINCT trace_id)::BIGINT AS trace_count,
            -- First-observed context, by timestamp. FILTER drops empties so
            -- we don't report "user landed on '' " for sessions where the
            -- opening event has no url_path / user_agent.
            (ARRAY_AGG(url_path ORDER BY timestamp) FILTER (WHERE url_path IS NOT NULL AND url_path <> ''))[1] AS landing_url,
            (ARRAY_AGG(user_agent ORDER BY timestamp) FILTER (WHERE user_agent IS NOT NULL AND user_agent <> ''))[1] AS user_agent,
            (ARRAY_AGG(error_text ORDER BY timestamp) FILTER (WHERE is_error AND error_text IS NOT NULL AND error_text <> ''))[1] AS first_error,
            COUNT(*) OVER()::INT AS total_sessions
          FROM filtered GROUP BY session_id
        ), page AS (
          SELECT * FROM agg ORDER BY |]
          <> sortColSql
          <> [HI.sql| DESC NULLS LAST OFFSET #{skip} LIMIT #{aggregatePageSize}
        ), svcs AS (
          SELECT session_id, ARRAY_REMOVE(ARRAY_AGG(DISTINCT service_name), NULL) AS services
          FROM filtered WHERE session_id IN (SELECT session_id FROM page) GROUP BY session_id
        ), hourly AS (
          SELECT session_id, bi, COUNT(*)::INT AS cnt
          FROM filtered WHERE session_id IN (SELECT session_id FROM page) GROUP BY session_id, bi
        ), hourly_agg AS (
          SELECT session_id, ARRAY_AGG(bi ORDER BY bi) AS bis, ARRAY_AGG(cnt ORDER BY bi) AS cnts
          FROM hourly GROUP BY session_id
        ) SELECT p.session_id, p.user_id, p.user_email, p.user_name,
            p.event_count, p.error_count, p.first_seen,
            p.last_seen, p.duration_ns, p.trace_count, p.total_sessions,
            COALESCE(s.services, '{}'::TEXT[]),
            COALESCE(h.bis, '{}'::INT[]), COALESCE(h.cnts, '{}'::INT[]),
            p.landing_url, p.user_agent, p.first_error
          FROM page p LEFT JOIN svcs s USING (session_id) LEFT JOIN hourly_agg h USING (session_id)
          ORDER BY p.|]
          <> sortColSql
          <> [HI.sql| DESC NULLS LAST|]
  rawRows :: [RawSessionRow] <- Hasql.withHasqlTimefusion enableTfReads $ Hasql.interp q
  Log.logTrace "fetchSessions: query done" $ AE.object ["rows" AE..= length rawRows]
  let total = maybe 0 (fromIntegral . (.totalSessions)) $ listToMaybe rawRows
      allBuckets = concatMap (\r -> map fromIntegral $ V.toList r.bis) rawRows
      range = bucketRange allBuckets
      toRowWithVolume r =
        SessionRow
          { sessionId = r.sessionId
          , userId = r.userId
          , userEmail = r.userEmail
          , userName = r.userName
          , eventCount = r.eventCount
          , errorCount = r.errorCount
          , firstSeen = r.firstSeen
          , lastSeen = r.lastSeen
          , durationNs = r.durationNs
          , traceCount = r.traceCount
          , services = r.services
          , volume = densifyBuckets range $ zip (map fromIntegral $ V.toList r.bis) (map fromIntegral $ V.toList r.cnts)
          , landingUrl = r.landingUrl
          , userAgent = r.userAgent
          , firstError = r.firstError
          }
  pure (total, map toRowWithVolume rawRows)


-- | Session-level aggregates for the Sessions viz header. Shares the filter
-- and CTE shape with 'fetchSessions' so the header reconciles with the table.
-- @clean@ and @errored@ are parallel arrays: bucket @i@ starts at
-- @bucketStartEpoch + i*bucketWidthSec@. Both are empty when there are no
-- sessions in range.
data SessionSummary = SessionSummary
  { totalSessions :: Int64
  , erroredSessions :: Int64
  , uniqueUsers :: Int64
  , uniqueServices :: Int64
  , medianDurationNs :: Int64
  , p95DurationNs :: Int64
  , medianEvents :: Int64
  , totalEvents :: Int64
  , bucketWidthSec :: Int
  , bucketStartEpoch :: Int
  , clean :: [Int]
  , errored :: [Int]
  }
  deriving stock (Generic, Show)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake], DAE.OmitNothingFields] SessionSummary


fetchSessionSummary
  :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es)
  => Bool
  -> Projects.ProjectId
  -> [Section]
  -> (Maybe UTCTime, Maybe UTCTime)
  -> Eff es SessionSummary
fetchSessionSummary enableTfReads pid queryAST dateRange = do
  now <- Time.currentTime
  let sqlCfg = (defSqlQueryCfg pid now (Just SSpans) Nothing){dateRange}
      (_, queryComponents) = queryASTToComponents sqlCfg queryAST
      dateRangeClause = buildDateRange sqlCfg
      whereCondition = scopedWhere pid.toText queryComponents.whereClause
      fullWhere = whereCondition <> if T.null dateRangeClause then "" else " AND " <> dateRangeClause
      bucketW = bucketWidthSecs dateRange now
      q =
        [HI.sql|WITH filtered AS (
          SELECT attributes___session___id AS sid,
            COALESCE(attributes___user___id, attributes___user___email) AS user_key,
            resource___service___name AS svc, timestamp, end_time,
            (lower(level) = 'error' OR severity___severity_number >= 17 OR status_code = 'ERROR') AS is_err
          FROM otel_logs_and_spans WHERE |]
          <> rawSql fullWhere
          <> [HI.sql| AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''
        ), per_session AS (
          SELECT MAX(user_key) AS user_key, COUNT(*)::BIGINT AS events, BOOL_OR(is_err) AS has_err,
            (EXTRACT(EPOCH FROM (MAX(COALESCE(end_time, timestamp)) - MIN(timestamp))) * 1e9)::BIGINT AS dur_ns,
            floor(extract(epoch from MIN(timestamp)) / #{bucketW})::INT AS bi
          FROM filtered GROUP BY sid
        ), bkt AS (
          SELECT bi, COUNT(*) FILTER (WHERE NOT has_err)::INT AS c, COUNT(*) FILTER (WHERE has_err)::INT AS e
          FROM per_session GROUP BY bi
        )
        SELECT COUNT(*)::BIGINT, COUNT(*) FILTER (WHERE has_err)::BIGINT,
          COUNT(DISTINCT user_key) FILTER (WHERE user_key IS NOT NULL)::BIGINT,
          (SELECT COUNT(DISTINCT svc) FILTER (WHERE svc IS NOT NULL)::BIGINT FROM filtered),
          COALESCE(PERCENTILE_CONT(0.5)  WITHIN GROUP (ORDER BY dur_ns), 0)::BIGINT,
          COALESCE(PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY dur_ns), 0)::BIGINT,
          COALESCE(PERCENTILE_CONT(0.5)  WITHIN GROUP (ORDER BY events),  0)::BIGINT,
          COALESCE(SUM(events), 0)::BIGINT,
          COALESCE((SELECT ARRAY_AGG(bi ORDER BY bi) FROM bkt), '{}'::INT[]),
          COALESCE((SELECT ARRAY_AGG(c  ORDER BY bi) FROM bkt), '{}'::INT[]),
          COALESCE((SELECT ARRAY_AGG(e  ORDER BY bi) FROM bkt), '{}'::INT[])
        FROM per_session|]
  rows :: [(Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, V.Vector Int32, V.Vector Int32, V.Vector Int32)] <-
    Hasql.withHasqlTimefusion enableTfReads $ Hasql.interp q
  -- A no-GROUP-BY aggregate always produces exactly one row; zero rows would
  -- indicate a decoder/arity drift, not a legitimate "no sessions" case.
  let zeroSumm = SessionSummary 0 0 0 0 0 0 0 0 bucketW 0 [] []
  case rows of
    [] -> do
      Log.logAttention "fetchSessionSummary: aggregate returned zero rows" $ AE.object ["project_id" AE..= pid]
      pure zeroSumm
    (total, err, users, svcs, medDur, p95Dur, medEvt, totEvt, bisV, cV, eV) : _ ->
      if total == 0
        then pure zeroSumm{erroredSessions = err, uniqueUsers = users, uniqueServices = svcs, totalEvents = totEvt}
        else do
          let bisI = map fromIntegral $ V.toList bisV :: [Int]
              range@(minB, _) = bucketRange bisI
              dens v = densifyBuckets range $ zip bisI (map fromIntegral $ V.toList v)
          pure
            SessionSummary
              { totalSessions = total
              , erroredSessions = err
              , uniqueUsers = users
              , uniqueServices = svcs
              , medianDurationNs = medDur
              , p95DurationNs = p95Dur
              , medianEvents = medEvt
              , totalEvents = totEvt
              , bucketWidthSec = bucketW
              , bucketStartEpoch = minB * bucketW
              , clean = dens cV
              , errored = dens eV
              }


-- | Which aggregate group to expand in the inline detail view.
data ExpandKind = ExpandSession Text | ExpandPattern Text


-- | Fetch example events belonging to a session or matching a pattern, used by
-- the inline expand in the Sessions and Patterns visualizations. Returns rows
-- projected with the same default-select columns as logs mode so the web
-- component can render them with the existing logs-mode row template.
fetchEventExamples
  :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es)
  => Bool
  -> Projects.ProjectId
  -> [Section]
  -> (Maybe UTCTime, Maybe UTCTime)
  -> ExpandKind
  -> Int
  -- ^ skip
  -> Int
  -- ^ limit
  -> Eff es (V.Vector (V.Vector AE.Value), [Text])
fetchEventExamples enableTfReads pid queryAST dateRange expandKind skip limitN = do
  now <- Time.currentTime
  let sqlCfg = (defSqlQueryCfg pid now (Just SSpans) Nothing){dateRange}
      (_, qc) = queryASTToComponents sqlCfg queryAST
      pidTxt = pid.toText
      dateRangeClause = buildDateRange sqlCfg
      whereCondition = scopedWhere pidTxt qc.whereClause
      fullWhereSql = rawSql (whereCondition <> if T.null dateRangeClause then "" else " AND " <> dateRangeClause)
      expandFilter = case expandKind of
        ExpandSession sid -> [HI.sql| AND attributes___session___id = #{sid}|]
        ExpandPattern tpl -> [HI.sql| AND array_to_string(summary, chr(30)) ILIKE #{templateToLike tpl}|]
      cols = defaultSelectSqlQuery (Just SSpans)
      -- Mirror selectLogTable's summary column handling: wrap TEXT[] as JSON for row output.
      processedCols = map (\c -> if c == "summary" || "summary" `T.isSuffixOf` c then "to_json(summary)" else c) $ colsNoAsClause cols
      selectClause = T.intercalate ", " processedCols
      -- Sessions: fetch one root event per trace via DISTINCT ON so [+N] expansion
      -- covers all traces.  Patterns/other: fetch raw events as before.
      q = case expandKind of
        ExpandSession _ ->
          rawSql ("SELECT " <> selectClause <> " FROM (SELECT DISTINCT ON (context___trace_id) * FROM otel_logs_and_spans WHERE ")
            <> fullWhereSql
            <> expandFilter
            <> [HI.sql| ORDER BY context___trace_id, parent_id ASC NULLS FIRST, timestamp ASC) sub ORDER BY timestamp ASC OFFSET #{skip}::INT LIMIT #{limitN}::INT|]
        _ ->
          rawSql ("SELECT " <> selectClause <> " FROM otel_logs_and_spans WHERE ")
            <> fullWhereSql
            <> expandFilter
            <> [HI.sql| ORDER BY timestamp ASC OFFSET #{skip}::INT LIMIT #{limitN}::INT|]
  Log.logTrace "fetchEventExamples: query" $ AE.object ["project_id" AE..= pid]
  rows <- Hasql.withHasqlTimefusion enableTfReads $ checkpoint (toAnnotation ("fetchEventExamples" :: Text, pid)) $ executeArbitraryQuery q
  pure (rows, listToColNames cols)


-- | Page size for patterns and sessions aggregations. The JSON encoder
-- checks @>= aggregatePageSize@ to set the @hasMore@ flag.
aggregatePageSize :: Int
aggregatePageSize = 100


-- | Convert a Drain template to a SQL LIKE pattern: @\<*\>@ becomes @%@,
-- existing @%@ and @_@ are backslash-escaped.
--
-- >>> templateToLike "GET /api/<*>/users_list"
-- "GET /api/%/users\\_list"
-- >>> templateToLike "100% <*>"
-- "100\\% %"
templateToLike :: Text -> Text
templateToLike t =
  let escaped = T.replace "_" "\\_" $ T.replace "%" "\\%" t
   in T.replace "<*>" "%" escaped


-- | Pick a bucket width in seconds targeting ~20 buckets across the given
-- date range, with a 1 second floor. Shared by sparkline producers.
--
-- >>> import Data.Time (UTCTime(..))
-- >>> import Data.Time.Calendar (fromGregorian)
-- >>> let t0 = UTCTime (fromGregorian 2026 1 1) 0
-- >>> let t1 = UTCTime (fromGregorian 2026 1 1) 3600
-- >>> bucketWidthSecs (Just t0, Just t1) t0
-- 180
-- >>> bucketWidthSecs (Nothing, Nothing) t0
-- 180
bucketWidthSecs :: (Maybe UTCTime, Maybe UTCTime) -> UTCTime -> Int
bucketWidthSecs (from, to) n =
  let start = fromMaybe (addUTCTime (-3600) n) from
      rangeSecs = max 60 $ round $ diffUTCTime (fromMaybe n to) start :: Int
   in max 1 $ rangeSecs `div` 20


-- | Global (min,max) bucket index across all rows. (0,0) when no buckets.
--
-- >>> bucketRange []
-- (0,0)
-- >>> bucketRange [3, 1, 5]
-- (1,5)
bucketRange :: [Int] -> (Int, Int)
bucketRange [] = (0, 0)
bucketRange xs = (foldl' min maxBound xs, foldl' max minBound xs)


-- | Densify one row's sparse (bucket, count) pairs into a fixed-length
-- array covering the given global range, summing duplicate buckets.
--
-- >>> densifyBuckets (0, 4) [(0, 2), (2, 3), (4, 1)]
-- [2,0,3,0,1]
-- >>> densifyBuckets (0, 0) []
-- [0]
-- >>> densifyBuckets (1, 3) [(2, 5), (2, 3)]
-- [0,8,0]
densifyBuckets :: (Int, Int) -> [(Int, Int)] -> [Int]
densifyBuckets (minB, maxB) bs =
  let bMap = HM.fromListWith (+) bs
   in [fromMaybe 0 $ HM.lookup i bMap | i <- [minB .. maxB]]


-- | Build a fixed 24-slot hourly bucket array from sparse (UTCTime, Int) pairs.
buildHourlyBuckets :: UTCTime -> [(UTCTime, Int)] -> [Int]
buildHourlyBuckets now pairs = [fromMaybe 0 $ HM.lookup i bucketMap | i <- [0 .. 23]]
  where
    startHour = addUTCTime (-(23 * 3600)) $ truncateToHour now
    bucketMap = HM.fromListWith (+) [(hourIndex t, c) | (t, c) <- pairs, let idx = hourIndex t, idx >= 0 && idx < 24]
    hourIndex t = floor (diffUTCTime (truncateToHour t) startHour / 3600) :: Int
    truncateToHour t = let s = utcTimeToPOSIXSeconds t in posixSecondsToUTCTime $ fromIntegral (floor s `div` 3600 * 3600 :: Int)


getLastSevenDaysTotalRequest :: (DB es, Time.Time :> es) => Projects.ProjectId -> Eff es Int
getLastSevenDaysTotalRequest = getRequestCountForInterval "7 days"


getRequestCountForInterval :: (DB es, Time.Time :> es) => Text -> Projects.ProjectId -> Eff es Int
getRequestCountForInterval interval pid = do
  now <- Time.currentTime
  let pidText = pid.toText
  fromMaybe 0 <$> Hasql.interpOne ([HI.sql| SELECT count(*)::INT FROM otel_logs_and_spans WHERE project_id=#{pidText}::text AND timestamp > #{now}::timestamptz - interval |] <> rawSql ("'" <> interval <> "'"))
