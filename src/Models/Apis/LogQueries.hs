module Models.Apis.LogQueries (
  SDKTypes (..),
  RequestTypes (..),
  ATError (..),
  PatternRow (..),
  normalizeUrlPath,
  selectLogTable,
  executeArbitraryQuery,
  executeSecuredQuery,
  logExplorerUrlPath,
  getLastSevenDaysTotalRequest,
  fetchLogPatterns,
  selectChildSpansAndLogs,
)
where

import Control.Exception.Annotated (checkpoint, try)
import Control.Lens (view, _5)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Hasql.Interpolate qualified as HI
import Effectful.Time qualified as Time
import Models.Apis.Fields ()
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
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
executeArbitraryQuery :: DB es => Text -> Eff es (V.Vector (V.Vector AE.Value))
executeArbitraryQuery queryText = do
  results :: [AE.Value] <- Hasql.interp $ rawSql $
    "SELECT (SELECT json_agg(x.value ORDER BY x.ordinality)::jsonb FROM json_each(row_to_json(sub.*)) WITH ORDINALITY AS x) FROM (" <> queryText <> ") AS sub"
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

  result <- try @SomeException $ checkpoint (toAnnotation ("selectLogTable", q)) $ executeArbitraryQuery q
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
      whereCondition = fromMaybe [text|project_id=${pidTxt}|] queryComponents.whereClause
      fullWhere = whereCondition <> if T.null dateRangeClause then "" else " AND " <> dateRangeClause
      target = fromMaybe "summary" targetM
  Log.logTrace "fetchLogPatterns: start"
    $ AE.object
      ["project_id" AE..= pid, "target" AE..= target, "skip" AE..= skip, "date_range" AE..= show dateRange]
  let (dateFrom, dateTo) = dateRange
  precomputed :: [(Text, Int64, Maybe Text, Maybe Text, Text, Int, Int)] <-
    if target `elem` map fst LogPatterns.knownPatternFields
      then Hasql.interp [HI.sql|SELECT lp.log_pattern, (lp.occurrence_count + COALESCE(m.member_count, 0))::BIGINT as total_count, lp.log_level, lp.service_name, lp.pattern_hash, COALESCE(m.member_cnt, 0)::INT as merged_count, COUNT(*) OVER()::INT as total_patterns FROM apis.log_patterns lp LEFT JOIN LATERAL (SELECT SUM(occurrence_count) as member_count, COUNT(*)::INT as member_cnt FROM apis.log_patterns WHERE canonical_id = lp.id) m ON TRUE WHERE lp.project_id = #{pid} AND lp.source_field = #{target} AND lp.canonical_id IS NULL AND (#{dateFrom} IS NULL OR lp.last_seen_at >= #{dateFrom}) AND (#{dateTo} IS NULL OR lp.last_seen_at <= #{dateTo}) ORDER BY total_count DESC OFFSET #{skip} LIMIT 100|]
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
          allBIs = [bi | (_, (_, _, _, bs)) <- sorted, (bi, _) <- bs]
          (minB, maxB) = case allBIs of [] -> (0, 0); xs -> (foldl' min maxBound xs, foldl' max minBound xs)
          buildVolume bs = let bMap = HM.fromListWith (+) bs in [fromMaybe 0 $ HM.lookup i bMap | i <- [minB .. maxB]]
      Log.logTrace "fetchLogPatterns: normalization done" $ AE.object ["patterns" AE..= HM.size merged]
      pure (HM.size merged, [PatternRow{logPattern = pat, count = fromIntegral cnt, level = lvl, service = svc, volume = buildVolume bs, mergedCount = 0} | (pat, (cnt, lvl, svc, bs)) <- sorted])
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
    -- ~20 buckets for the sparkline, minimum 1 second
    bucketWidthSecs (from, to) n =
      let start = fromMaybe (addUTCTime (-3600) n) from
          rangeSecs = max 60 $ round $ diffUTCTime (fromMaybe n to) start :: Int
       in max 1 $ rangeSecs `div` 20 :: Int


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

