{-# LANGUAGE NoFieldSelectors #-}

module Pages.Charts.Charts (queryMetrics, MetricsData (..), fetchMetricsData, MetricsStats (..), DataType (..), convertTimestampsToMs) where

import Control.Exception.Annotated (checkpoint, try)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.List qualified as L (maximum)
import Data.Map.Strict qualified as M
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Simple (SomePostgreSqlException, query_)
import Database.PostgreSQL.Simple.Types (Only (..), Query (Query), fromOnly)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Log qualified
import Models.Projects.Projects qualified as Projects
import OpenTelemetry.Attributes qualified as OA
import Pages.Charts.Types (DataType (..), MetricsData (..), MetricsStats (..))
import Pkg.Components.TimePicker qualified as Components
import Pkg.DeriveUtils (DB)
import Pkg.Parser (QueryComponents (finalSummarizeQuery, whereClause), SqlQueryCfg (..), defSqlQueryCfg, pSource, parseQueryToAST, queryASTToComponents, replacePlaceholders, variablePresets, variablePresetsKQL)
import Pkg.Parser.Stats (Section (..), Sources (..))
import Pkg.QueryCache qualified as QC
import Relude
import Servant.Server (ServerError (errBody), err400)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Tracing (Tracing, withSpan_)
import Text.Megaparsec (parseMaybe)
import UnliftIO.Exception (catch, throwIO)
import Utils qualified


pivot' :: V.Vector (Int, Text, Double) -> (V.Vector Text, V.Vector (V.Vector (Maybe Double)), Double, Double)
pivot' rows
  | V.null rows = (V.empty, V.empty, 0.0, 0.0)
  | otherwise =
      let extractHeaders = V.uniq . V.map snd3 . V.modify (VA.sortBy (comparing snd3))
          headers = extractHeaders rows
          grouped =
            V.groupBy (\a b -> fst3 a == fst3 b)
              $ V.modify (VA.sortBy (comparing fst3)) rows
          ngrouped = map (transform headers) grouped
          totalSum = V.sum $ V.map thd3 rows

          -- Calculate rate (rows per minute)
          timeVec = V.map fst3 rows
          minTime = V.minimum timeVec
          maxTime = V.maximum timeVec
          timeSpanMinutes = fromIntegral (maxTime - minTime) / 60.0
          numRows = fromIntegral $ V.length rows
          rate = if timeSpanMinutes > 0 then numRows / timeSpanMinutes else 0.0
       in (headers, V.fromList ngrouped, totalSum, rate)


-- Helper to convert from Double timestamp to Int timestamp tuples

transform :: V.Vector Text -> V.Vector (Int, Text, Double) -> V.Vector (Maybe Double)
transform fields tuples =
  V.cons (Just timestamp) (V.map getValue fields)
  where
    getValue field = V.find (\(_, b, _) -> b == field) tuples >>= \(_, _, a) -> Just a
    timestamp = fromIntegral $ maybe 0 fst3 (V.find (const True) tuples)


statsTriple :: V.Vector (Int, Text, Double) -> MetricsStats
statsTriple v
  | V.null v = MetricsStats 0 0 0 0 0 0 0
  | otherwise = MetricsStats mn mx tot cnt (tot / fromIntegral cnt) mode maxGroupSum
  where
    -- Extract the Double values from each tuple
    doubles = V.map thd3 v

    (!mn, !mx, !tot, !cnt, !freq, !timestampMap) =
      V.foldl'
        ( \(a, b, c, d, m, tsMap) (ts, _, x) ->
            ( min a x
            , max b x
            , c + x
            , d + 1
            , M.insertWith (+) x 1 m
            , M.insertWith (+) ts x tsMap
            )
        )
        (V.head doubles, V.head doubles, 0, 0, M.empty, M.empty)
        v

    -- Find the maximum of the grouped sums
    maxGroupSum =
      if M.null timestampMap
        then 0
        else L.maximum $ M.elems timestampMap

    mode =
      fst
        $ M.foldlWithKey'
          ( \acc@(_, cnt') k c ->
              if c > cnt' then (k, c) else acc
          )
          (V.head doubles, 0)
          freq


type M = Maybe


sourceTable :: Maybe Sources -> Text
sourceTable = \case
  Just SMetrics -> "telemetry.metrics"
  _ -> "otel_logs_and_spans"


-- Helper function: converts Just "" to Nothing.
nonNull :: Maybe Text -> Maybe Text
nonNull Nothing = Nothing
nonNull (Just "") = Nothing
nonNull x = x


queryMetrics :: (DB es, Effectful.Error.Static.Error ServerError :> es, Effectful.Reader.Static.Reader AuthContext :> es, Log :> es, Time.Time :> es, Tracing :> es) => M Text -> M DataType -> M Projects.ProjectId -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> [(Text, Maybe Text)] -> Eff es MetricsData
queryMetrics dbSource (maybeToMonoid -> respDataType) pidM (nonNull -> queryM) (nonNull -> querySQLM) (nonNull -> sinceM) (nonNull -> fromM) (nonNull -> toM) (nonNull -> sourceM) allParams = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  -- project_id is required for every query path; a missing one is a malformed request,
  -- not a 500 (this used to be `Unsafe.fromJust pidM`, which crashed the handler).
  pid <- maybe (throwError err400{errBody = "project_id is required"}) pure pidM
  let (fromD, toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  let mappngSQL = variablePresets pid.toText fromD toD allParams now
      mappngKQL = variablePresetsKQL pid.toText fromD toD allParams now
  let parseQuery q = either (\err -> throwError err400{errBody = encodeUtf8 $ "Invalid query: " <> err}) pure (parseQueryToAST $ replacePlaceholders mappngKQL q)

  case (queryM, querySQLM) of
    (_, Just querySQL) -> do
      queryAST <-
        checkpoint (toAnnotation ("queryMetrics", queryM))
          $ parseQuery
          $ maybeToMonoid queryM
      let sqlQueryComponents =
            (defSqlQueryCfg pid now (parseMaybe pSource =<< sourceM) Nothing)
              { dateRange = (fromD, toD)
              }
      let (_, qc) = queryASTToComponents sqlQueryComponents queryAST
      let mappngSQL' = mappngSQL <> M.fromList [("query_ast_filters", maybe "" (" AND " <>) qc.whereClause)]
      let sqlQuery = replacePlaceholders mappngSQL' querySQL
      let tbl = sourceTable (parseMaybe pSource =<< sourceM)
      convertTimestampsToMs
        <$> withChartSpan
          tbl
          ( chartSpanAttrs tbl sqlQuery respDataType pid.toText
              <> [ ("monoscope.kql.query", OA.toAttribute (maybeToMonoid queryM))
                 , ("monoscope.kql.mode", OA.toAttribute ("sql" :: Text))
                 ]
          )
          sqlQuery
          (emptyMetricsFor now fromD toD)
          (runFetchMetrics respDataType sqlQuery now fromD toD authCtx dbSource)
    _ -> do
      queryAST <-
        checkpoint (toAnnotation ("queryMetrics", queryM))
          $ parseQuery
          $ maybeToMonoid queryM
      let source = parseMaybe pSource =<< sourceM
      let sqlQueryCfg = (defSqlQueryCfg pid now source Nothing){dateRange = (fromD, toD)}
      -- Scalar aggregates (summarize with no `by` clause) produce a single
      -- float row; decoding them with the default DTMetric (timestamp-first
      -- pivot) raises a type error. Callers that don't pass data_type — the
      -- CLI's `metrics query ... --assert` flow — get the right decoder here.
      let respDataType' = if respDataType == DTMetric && isScalarSummarize queryAST then DTFloat else respDataType
      convertTimestampsToMs <$> queryMetricsWithCache authCtx dbSource respDataType' pid source queryAST sqlQueryCfg (maybeToMonoid queryM) now fromD toD


-- | A summarize with no @by@ clause at all — yields one scalar row.
isScalarSummarize :: [Section] -> Bool
isScalarSummarize = any isScalar
  where
    isScalar = \case
      SummarizeCommand _ Nothing -> True
      _ -> False


-- | Execute query with caching support for timeseries queries
queryMetricsWithCache
  :: (DB es, Log :> es, Time.Time :> es, Tracing :> es)
  => AuthContext
  -> Maybe Text
  -> DataType
  -> Projects.ProjectId
  -> Maybe Sources
  -> [Section]
  -> SqlQueryCfg
  -> Text
  -> UTCTime
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Eff es MetricsData
queryMetricsWithCache authCtx dbSource respDataType pid source queryAST sqlQueryCfg originalQuery now fromD toD
  | respDataType /= DTMetric = executeQueryWith sqlQueryCfg queryAST
  | not (QC.hasSummarizeWithBin queryAST) = executeQueryWith sqlQueryCfg queryAST
  | otherwise = do
      let reqFrom = fromMaybe (addUTCTime (-86400) now) fromD
      let reqTo = fromMaybe now toD
      let cacheKey = QC.generateCacheKey pid source queryAST sqlQueryCfg
      cacheResult <- QC.lookupCache cacheKey (reqFrom, reqTo)
      case cacheResult of
        QC.CacheHit entry -> do
          let trimmed = QC.trimToRange entry.cachedData reqFrom reqTo
              coversRange = entry.cachedFrom <= reqFrom && entry.cachedTo >= reqTo
          refetchUnlessAdequate coversRange entry.cachedData trimmed
        QC.PartialHit entry -> do
          let deltaFromTime = entry.cachedTo
          let deltaSqlCfg = sqlQueryCfg{dateRange = (Just deltaFromTime, Just reqTo)}
          -- Rewrite bin_auto to fixed interval for delta fetch to match cached data
          let deltaAST = QC.rewriteBinAutoToFixed cacheKey.binInterval queryAST
          deltaData <- executeQueryWith deltaSqlCfg deltaAST
          let merged = QC.mergeTimeseriesData entry.cachedData deltaData
          let windowSecs = fromIntegral $ QC.slidingWindowSeconds cacheKey.binInterval
          let slidingWindowStart = addUTCTime (negate windowSecs) reqTo
          let trimmed = QC.trimOldData slidingWindowStart merged
          -- Only advance the watermark when the delta actually succeeded. A failed
          -- fetch (timeout, TF planning error, dropped conn) is swallowed into an
          -- empty MetricsData by 'withChartSpan'; advancing cached_to past it would
          -- orphan the [cachedTo, reqTo] window forever (no later delta revisits it).
          whenNothing_ deltaData.error
            $ QC.updateCache cacheKey (slidingWindowStart, reqTo) trimmed originalQuery
          let result = QC.trimToRange trimmed reqFrom reqTo
          refetchUnlessAdequate (slidingWindowStart <= reqFrom) trimmed result
        QC.CacheMiss -> do
          result <- executeQueryWith sqlQueryCfg queryAST
          -- Don't cache a failed fetch (same reasoning as the partial-hit guard);
          -- it carries no data, so leave the cache cold and let the next request retry.
          when (isNothing result.error)
            $ QC.updateCache cacheKey (reqFrom, reqTo) result originalQuery
          pure result
        QC.CacheBypassed _ -> executeQueryWith sqlQueryCfg queryAST
  where
    executeQueryWith cfg ast = do
      let (_, qc) = queryASTToComponents cfg ast
      let sqlQuery = maybeToMonoid qc.finalSummarizeQuery
      let tbl = sourceTable source
      withChartSpan
        tbl
        ( chartSpanAttrs tbl sqlQuery respDataType pid.toText
            <> [ ("monoscope.kql.query", OA.toAttribute originalQuery)
               , ("monoscope.kql.mode", OA.toAttribute ("kql" :: Text))
               , ("monoscope.kql.source", OA.toAttribute (maybe "" (toText . show) source :: Text))
               ]
        )
        sqlQuery
        (emptyMetricsFor now fromD toD)
        (runFetchMetrics respDataType sqlQuery now fromD toD authCtx dbSource)
    refetchUnlessAdequate coversRange cached result
      | coversRange || not (V.null result.dataset) || V.null cached.dataset = pure result
      | otherwise = executeQueryWith sqlQueryCfg queryAST


-- | Run fetchMetricsData; rethrow on failure so the surrounding 'withSpan_'
-- marks the span 'Error' via its 'withException' handler (System/Tracing.hs).
-- The outer 'withChartSpan' catches and produces a user-visible error payload.
runFetchMetrics
  :: IOE :> es
  => DataType -> Text -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> AuthContext -> Maybe Text -> Eff es MetricsData
runFetchMetrics respDataType sqlQuery now fromD toD authCtx dbSource = liftIO do
  fetchMetricsData respDataType sqlQuery now fromD toD authCtx dbSource >>= either throwIO pure


-- | Six attributes every chart span carries. Each call-site appends its own
-- mode-specific (sql vs kql) extras.
chartSpanAttrs :: Text -> Text -> DataType -> Text -> [(Text, OA.Attribute)]
chartSpanAttrs tbl sqlQuery respDataType pidText =
  [ ("db.system.name", OA.toAttribute ("postgresql" :: Text))
  , ("db.operation.name", OA.toAttribute ("SELECT" :: Text))
  , ("db.collection.name", OA.toAttribute tbl)
  , ("db.query.text", OA.toAttribute sqlQuery)
  , ("monoscope.kql.data_type", OA.toAttribute (show @Text respDataType))
  , ("monoscope.project.id", OA.toAttribute pidText)
  ]


-- | Build the empty-result 'MetricsData' the catch handler returns on failure.
-- Pre-computed at the call site so 'withChartSpan' doesn't need now/fromD/toD.
emptyMetricsFor :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> MetricsData
emptyMetricsFor now fromD toD =
  def
    { from = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe (addUTCTime (-86400) now) fromD
    , to = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe now toD
    }


-- | Map common backend failure modes to a short user-facing label. Covers both
-- Postgres ("column \"x\" does not exist") and TimeFusion's wrapped form
-- ("External error: Kernel error: Predicate references unknown column: x")
-- since prod chart reads can hit either backend. Raw error stays in the OTEL
-- span + log line.
sanitizeChartError :: SomePostgreSqlException -> Text
sanitizeChartError = Utils.sanitizeBackendError . toText . displayException


-- | Wrap a chart-data fetch with its OTEL span and turn any SQL failure into
-- an error-tagged empty 'MetricsData' so the dashboard keeps rendering. The
-- exception is rethrown through 'withSpan_' first, so the span gets
-- 'Error' status + 'exception.*' attributes from
-- @System.Tracing.withException@ before we catch it here.
withChartSpan
  :: (IOE :> es, Log :> es, Tracing :> es)
  => Text
  -> [(Text, OA.Attribute)]
  -> Text
  -> MetricsData
  -> Eff es MetricsData
  -> Eff es MetricsData
withChartSpan tbl attrs sqlQuery fallback action =
  withSpan_ ("SELECT " <> tbl) attrs action `catch` \(e :: SomePostgreSqlException) -> do
    let userMsg = sanitizeChartError e
    -- TODO(otel-metrics): widget_sql_error{project_id, error_class=userMsg}
    Log.logAttention
      "widget SQL execution failed; rendering error overlay"
      (AE.object ["error" AE..= show @Text e, "sql" AE..= unwords (words sqlQuery), "error_message" AE..= userMsg])
    pure fallback{error = Just userMsg}


fetchMetricsData :: DataType -> Text -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> AuthContext -> Maybe Text -> IO (Either SomePostgreSqlException MetricsData)
fetchMetricsData respDataType sqlQuery now fromD toD authCtx dbSource = do
  let pool = case dbSource of
        Just "postgres" -> authCtx.pool
        Just "timefusion" -> authCtx.timefusionPgPool
        _ -> if authCtx.env.enableTimefusionReads then authCtx.timefusionPgPool else authCtx.pool
  let baseMetricsData =
        def
          { from = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe (addUTCTime (-86400) now) fromD
          , to = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe now toD
          }

  try @SomePostgreSqlException $ checkpoint (toAnnotation (respDataType, sqlQuery)) $ case respDataType of
    DTFloat -> do
      chartData <- withResource pool \conn -> query_ conn (Query $ encodeUtf8 sqlQuery) :: IO [Only (Maybe Double)]
      pure
        baseMetricsData
          { dataFloat = listToMaybe chartData >>= fromOnly
          , rowsCount = 1
          }
    DTMetric -> do
      chartData <- withResource pool $ \conn -> query_ conn (Query $ encodeUtf8 sqlQuery)
      let chartsDataV = V.fromList chartData
      let (hdrs, groupedData, rowsCount, rpm) = pivot' chartsDataV
      pure
        baseMetricsData
          { dataset = groupedData
          , headers = V.cons "timestamp" hdrs
          , rowsCount
          , rowsPerMin = Just rpm
          , stats = Just $ statsTriple chartsDataV
          }
    DTText -> do
      chartData <- withResource pool $ \conn -> query_ conn (Query $ encodeUtf8 sqlQuery)
      pure
        baseMetricsData
          { dataText = V.fromList chartData
          , rowsCount = fromIntegral $ length chartData
          }
    DTJson -> do
      chartData <- withResource pool $ \conn -> query_ conn (Query $ encodeUtf8 sqlQuery)
      pure
        baseMetricsData
          { dataJSON = V.fromList chartData
          , rowsCount = fromIntegral $ length chartData
          }


-- | Convert timestamps in MetricsData from seconds to milliseconds for ECharts
convertTimestampsToMs :: MetricsData -> MetricsData
convertTimestampsToMs md = md{dataset = V.map convertRow md.dataset, from = (* 1000) <$> md.from, to = (* 1000) <$> md.to}
  where
    convertRow row = case V.uncons row of
      Just (Just ts, rest) -> V.cons (Just $ ts * 1000) rest
      _ -> row
