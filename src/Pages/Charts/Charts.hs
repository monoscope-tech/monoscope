{-# LANGUAGE NoFieldSelectors #-}

module Pages.Charts.Charts (queryMetrics, queryMetricsStream, ChartStream, streamQuery_, MetricsData (..), fetchMetricsData, MetricsStats (..), DataType (..), convertTimestampsToMs) where

import Control.Concurrent (threadWaitRead)
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as E
import Control.Exception.Annotated (checkpoint, try)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.Map.Strict qualified as M
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple (Connection, FromRow, SomePostgreSqlException, query_)
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..))
import Database.PostgreSQL.Simple.FromRow (fromRow)
import Database.PostgreSQL.Simple.Internal qualified as PGI
import Database.PostgreSQL.Simple.Ok (ManyErrors (..), Ok (..))
import Database.PostgreSQL.Simple.Types (Only (..), Query (Query), fromOnly)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import GHC.Clock (getMonotonicTimeNSec)
import Log qualified
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Media ((//))
import OpenTelemetry.Attributes qualified as OA
import Pages.Charts.Types (DataType (..), MetricsData (..), MetricsStats (..))
import Pkg.Components.TimePicker qualified as Components
import Pkg.DeriveUtils (DB)
import Pkg.Metrics qualified as Metrics
import Pkg.Parser (BinDensity, QueryComponents (finalSummarizeQuery, whereClause), SqlQueryCfg (..), defSqlQueryCfg, pSource, queryASTToComponents, replacePlaceholders, variablePresets, variablePresetsKQL)
import Pkg.Parser.Stats (QueryError (..), Section (..), Sources (..), parseQueryDiagnosed)
import Pkg.QueryCache qualified as QC
import Relude
import Servant qualified
import Servant.Server (ServerError (errBody), err400)
import Servant.Types.SourceT (SourceT (..), StepT (..))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Tracing (Tracing, withSpan_)
import Text.Megaparsec (parseMaybe)
import UnliftIO.Async (withAsync)
import UnliftIO.Async qualified
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


transform :: V.Vector Text -> V.Vector (Int, Text, Double) -> V.Vector (Maybe Double)
transform fields tuples =
  V.cons (Just timestamp) (V.map (\field -> thd3 <$> V.find ((== field) . snd3) tuples) fields)
  where
    timestamp = fromIntegral $ maybe 0 fst3 (tuples V.!? 0)


statsTriple :: V.Vector (Int, Text, Double) -> MetricsStats
statsTriple v
  | V.null v = def
  | otherwise = MetricsStats mn mx tot cnt (tot / fromIntegral cnt) mode maxGroupSum
  where
    d0 = thd3 $ V.head v

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
        (d0, d0, 0, 0, M.empty, M.empty)
        v

    maxGroupSum = fromMaybe 0 $ viaNonEmpty (\(x :| xs) -> foldl' max x xs) (M.elems timestampMap)

    -- first (smallest) value wins ties, hence the strict `>`
    mode = fst $ M.foldlWithKey' (\acc@(_, cnt') k c -> if c > cnt' then (k, c) else acc) (d0, 0) freq


type M = Maybe


sourceTable :: Maybe Sources -> Text
sourceTable = \case
  Just SMetrics -> "otel_metrics"
  _ -> "otel_logs_and_spans"


-- | Select the storage semantics used by both SQL generation and execution.
-- Keeping this decision in one helper prevents PostgreSQL queries from being
-- generated with TimeFusion-only functions while still targeting the PG pool.
--
-- >>> usesTimefusionBackend False Nothing
-- False
-- >>> usesTimefusionBackend True (Just "postgres")
-- False
-- >>> usesTimefusionBackend False (Just "timefusion")
-- True
usesTimefusionBackend :: Bool -> Maybe Text -> Bool
usesTimefusionBackend enableTimefusionReads = \case
  Just "postgres" -> False
  Just "timefusion" -> True
  _ -> enableTimefusionReads


queryMetrics :: (DB es, Effectful.Error.Static.Error ServerError :> es, Effectful.Reader.Static.Reader AuthContext :> es, Log :> es, Time.Time :> es, Tracing :> es) => M Text -> M DataType -> M Projects.ProjectId -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> M BinDensity -> [(Text, Maybe Text)] -> Eff es MetricsData
queryMetrics dbSource (maybeToMonoid -> respDataType) pidM (Utils.nonEmptyT -> queryM) (Utils.nonEmptyT -> querySQLM) (Utils.nonEmptyT -> sinceM) (Utils.nonEmptyT -> fromM) (Utils.nonEmptyT -> toM) (Utils.nonEmptyT -> sourceM) binDensityM allParams = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  -- project_id is required for every query path; a missing one is a malformed request,
  -- not a 500 (this used to be `Unsafe.fromJust pidM`, which crashed the handler).
  pid <- maybe (throwError err400{errBody = "project_id is required"}) pure pidM
  let (fromD, toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  let density = fromMaybe def binDensityM
  let mappngSQL = variablePresets density pid.toText fromD toD allParams now
      mappngKQL = variablePresetsKQL density pid.toText fromD toD allParams now
  let source = parseMaybe pSource =<< sourceM
  -- A KQL parse/unknown-field failure is user input, not a server fault: carry it
  -- in the payload so the widget renders its error overlay. A 400 body isn't JSON,
  -- so the client would otherwise fall back to a generic "couldn't load this chart".
  -- Validated against `source`, not blind: a widget can carry `source=metrics` in the
  -- request rather than in the query text, and checking those fields against
  -- otel_logs_and_spans rejects the metrics table's own columns (`metric_name`, `value`).
  case first (.message) $ parseQueryDiagnosed source $ replacePlaceholders mappngKQL $ maybeToMonoid queryM of
    Left err -> pure (emptyMetricsFor now fromD toD){error = Just err}
    Right queryAST -> runQueryAST authCtx dbSource respDataType pid source density queryAST (maybeToMonoid queryM) querySQLM mappngSQL now fromD toD


-- | Run a parsed chart query, either through the caller's raw SQL template or
-- the KQL-generated query.
runQueryAST :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AuthContext -> Maybe Text -> DataType -> Projects.ProjectId -> Maybe Sources -> BinDensity -> [Section] -> Text -> Maybe Text -> M.Map Text Text -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> Eff es MetricsData
runQueryAST authCtx dbSource respDataType pid source binDensity queryAST queryM querySQLM mappngSQL now fromD toD = do
  let sqlQueryCfg =
        (defSqlQueryCfg pid now source Nothing)
          { dateRange = (fromD, toD)
          , binDensity
          , metricJsonAsVariant = usesTimefusionBackend authCtx.env.enableTimefusionReads dbSource
          }

  case querySQLM of
    Just querySQL -> do
      let (_, qc) = queryASTToComponents sqlQueryCfg queryAST
      let mappngSQL' = mappngSQL <> M.fromList [("query_ast_filters", maybe "" (" AND " <>) qc.whereClause)]
      let sqlQuery = replacePlaceholders mappngSQL' querySQL
      let tbl = sourceTable source
      convertTimestampsToMs
        <$> withChartSpan
          tbl
          ( chartSpanAttrs tbl sqlQuery respDataType pid.toText
              <> [ ("monoscope.kql.query", OA.toAttribute queryM)
                 , ("monoscope.kql.mode", OA.toAttribute ("sql" :: Text))
                 ]
          )
          sqlQuery
          (emptyMetricsFor now fromD toD)
          (runFetchMetrics respDataType sqlQuery now fromD toD authCtx dbSource)
    Nothing ->
      let actualDataType = decoderFor respDataType queryAST
       in convertTimestampsToMs
            . coerceBinnedScalar respDataType actualDataType
            <$> queryMetricsWithCache authCtx dbSource actualDataType pid source queryAST sqlQueryCfg queryM now fromD toD


-- | Pick the result decoder when the caller didn't. @DTMetric@ pivots on a
-- leading timestamp column, so it only fits a query binned by time; used on
-- anything else it fails with a column-type error rather than returning data.
-- The browser always sends an explicit @data_type@; API and CLI callers
-- (@monoscope chart@, @metrics query --assert@) generally don't, so infer it
-- from the query shape:
--
--   * @summarize@ with no @by@      → one scalar row      → 'DTFloat'
--   * @summarize … by bin(...)@     → a timeseries        → 'DTMetric'
--   * anything else (@by <field>@,
--     or no summarize at all)       → label/value rows    → 'DTText'
--
-- A scalar request for a binned query is the one exception to the explicit
-- type rule. Stat widgets can carry the same binned KQL as a chart; decoding
-- that three-column result as one float fails before the widget can select its
-- latest value. Decode the actual series shape, then 'coerceBinnedScalar'
-- extracts the newest point for the scalar response.
decoderFor :: DataType -> [Section] -> DataType
decoderFor requested queryAST
  | requested == DTFloat && QC.hasSummarizeWithBin queryAST = DTMetric
  | requested /= DTMetric = requested
  | isScalarSummarize queryAST = DTFloat
  | QC.hasSummarizeWithBin queryAST = DTMetric
  | otherwise = DTText


-- | Return the newest value when a scalar widget supplied binned KQL.
-- The metric decoder pivots rows to @[timestamp, series…]@ and sorts them by
-- timestamp ascending, so the last cell in the last row is the newest
-- aggregate value.
--
-- >>> let md = def{dataset = V.fromList [V.fromList [Just 10, Just 7], V.fromList [Just 20, Just 42]]}
-- >>> (coerceBinnedScalar DTFloat DTMetric md).dataFloat
-- Just 42.0
-- >>> (coerceBinnedScalar DTMetric DTMetric md).dataFloat
-- Nothing
coerceBinnedScalar :: DataType -> DataType -> MetricsData -> MetricsData
coerceBinnedScalar requested actual md
  | requested == DTFloat && actual == DTMetric = md{dataFloat = latestMetricValue}
  | otherwise = md
  where
    latestMetricValue = do
      guard $ not (V.null md.dataset)
      let row = V.last md.dataset
      guard $ not (V.null row)
      row V.! (V.length row - 1)


-- | A summarize with no @by@ clause at all — yields one scalar row.
isScalarSummarize :: [Section] -> Bool
isScalarSummarize = any \case
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
          -- Re-read a trailing overlap, not just [cachedTo, reqTo]: a bin read
          -- once and never revisited freezes a bad read into the chart forever.
          let deltaFromTime = addUTCTime (negate $ fromIntegral $ QC.deltaOverlapSeconds cacheKey.binInterval) entry.cachedTo
          let deltaSqlCfg = sqlQueryCfg{dateRange = (Just deltaFromTime, Just reqTo)}
          -- Rewrite bin_auto to fixed interval for delta fetch to match cached data
          let deltaAST = QC.rewriteBinAutoToFixed cacheKey.binInterval queryAST
          deltaData <- executeQueryWith deltaSqlCfg deltaAST
          let merged = QC.mergeTimeseriesData entry.cachedData deltaData
          let slidingWindowStart = QC.cacheWindowStart cacheKey.binInterval (reqFrom, reqTo)
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
    -- sanitizeBackendError covers both Postgres ("column \"x\" does not exist") and
    -- TimeFusion's wrapped form; the raw error stays on the span + log line.
    let userMsg = Utils.sanitizeBackendError . toText $ displayException e
    -- Unlabelled on purpose — project id and the sanitised error text are both unbounded, and
    -- both are already on the span and the log line below. See 'Metrics.widgetSqlErrors'.
    Metrics.count Metrics.widgetSqlErrors 1 []
    Log.logAttention
      "widget SQL execution failed; rendering error overlay"
      (AE.object ["error" AE..= show @Text e, "sql" AE..= unwords (words sqlQuery), "error_message" AE..= userMsg])
    pure fallback{error = Just userMsg}


fetchMetricsData :: DataType -> Text -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> AuthContext -> Maybe Text -> IO (Either SomePostgreSqlException MetricsData)
fetchMetricsData = fetchMetricsDataWithProgress Nothing


fetchMetricsDataWithProgress :: Maybe (MetricsData -> IO ()) -> DataType -> Text -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> AuthContext -> Maybe Text -> IO (Either SomePostgreSqlException MetricsData)
fetchMetricsDataWithProgress progress respDataType sqlQuery now fromD toD authCtx dbSource = do
  let pool = case dbSource of
        Just "postgres" -> authCtx.pool
        Just "timefusion" -> authCtx.timefusionPgPool
        _ -> if usesTimefusionBackend authCtx.env.enableTimefusionReads dbSource then authCtx.timefusionPgPool else authCtx.pool
  let baseMetricsData = emptyMetricsFor now fromD toD
  let runQ :: FromRow r => IO [r]
      runQ = withResource pool \conn -> query_ conn (Query $ encodeUtf8 sqlQuery)

  try @SomePostgreSqlException $ checkpoint (toAnnotation (respDataType, sqlQuery)) $ case respDataType of
    DTFloat -> do
      chartData <- runQ :: IO [Only (Maybe Double)]
      pure
        baseMetricsData
          { dataFloat = listToMaybe chartData >>= fromOnly
          , rowsCount = 1
          }
    DTMetric -> do
      let metrics rows =
            let (hdrs, groupedData, rowsCount, rpm) = pivot' rows
             in baseMetricsData{dataset = groupedData, headers = V.cons "timestamp" hdrs, rowsCount, rowsPerMin = Just rpm, stats = Just $ statsTriple rows}
      case progress of
        Nothing -> metrics . V.fromList <$> runQ
        Just emit -> do
          rowsRef <- newIORef []
          lastEmit <- newIORef 0
          withResource pool \conn -> streamQuery_ conn (Query $ encodeUtf8 sqlQuery) \row -> do
            modifyIORef' rowsRef (row :)
            tick <- getMonotonicTimeNSec
            previous <- readIORef lastEmit
            when (previous == 0 || tick - previous >= 100000000) do
              readIORef rowsRef >>= emit . metrics . V.fromList . reverse
              writeIORef lastEmit tick
          metrics . V.fromList . reverse <$> readIORef rowsRef
    DTText -> do
      -- Decoded through 'AnyText', not 'Text': a raw-SQL widget selects whatever its author
      -- wrote, and demanding `text` for every column failed the *whole* widget on one
      -- uncast value. Explicitly annotated because 'runQ' is polymorphic in the row type.
      rows <- runQ :: IO [V.Vector AnyText]
      let chartData = coerce (V.fromList rows) :: V.Vector (V.Vector Text)
      pure baseMetricsData{dataText = chartData, rowsCount = fromIntegral $ V.length chartData}
    DTJson -> do
      chartData <- V.fromList <$> runQ
      pure baseMetricsData{dataJSON = chartData, rowsCount = fromIntegral $ V.length chartData}


-- | Any column, as the text Postgres already sent for it.
--
-- The @DTText@ path feeds tables, trace tables and stat labels — every consumer only ever
-- prints these. Decoding them as 'Text' meant @FromField Text@ rejected any other column
-- type, so a single uncast column failed the entire widget with
-- @Incompatible {errSQLType = "int4", errHaskellType = "Text"}@ and the user got an error
-- overlay instead of their data. Widget SQL is author-written, so requiring a @::text@ cast
-- on every column was a trap rather than a contract.
--
-- Sound because postgresql-simple requests results in **text format**: the bytes handed to
-- 'fromField' are already the rendering @::text@ would have produced. Decoded leniently —
-- a decode error here would just trade one whole-widget failure for another — and NULL
-- becomes @""@, which previously threw 'UnexpectedNull' and killed the widget too.
newtype AnyText = AnyText Text


instance FromField AnyText where
  fromField _ = pure . AnyText . maybe "" (decodeUtf8With lenientDecode)


-- | Convert timestamps in MetricsData from seconds to milliseconds for ECharts
convertTimestampsToMs :: MetricsData -> MetricsData
convertTimestampsToMs md = md{dataset = V.map convertRow md.dataset, from = (* 1000) <$> md.from, to = (* 1000) <$> md.to}
  where
    convertRow row = case V.uncons row of
      Just (Just ts, rest) -> V.cons (Just $ ts * 1000) rest
      _ -> row


-- | Receive one libpq result at a time, keeping the existing typed row decoders.
-- The caller owns the connection exclusively for the duration of the stream.
-- On abandonment, cancel the server query and let withResource discard the connection.
streamQuery_ :: FromRow row => Connection -> Query -> (row -> IO ()) -> IO ()
streamQuery_ conn (Query sql) emit = run `E.onException` cancel
  where
    cancel = PGI.withConnection conn \handle -> do
      token <- PQ.getCancel handle
      forM_ token \c -> void $ PQ.cancel c
    run = do
      PGI.withConnection conn \handle -> do
        sent <- PQ.sendQuery handle sql
        unless sent $ PGI.throwLibPQError handle "Could not send chart query"
        enabled <- PQ.setSingleRowMode handle
        unless enabled $ PGI.throwLibPQError handle "Could not enable streaming results"
      loop
    next = PGI.withConnection conn await
    await handle = do
      consumed <- PQ.consumeInput handle
      unless consumed $ PGI.throwLibPQError handle "Could not read chart results"
      busy <- PQ.isBusy handle
      if busy
        then do
          fd <- PQ.socket handle >>= maybe (PGI.throwLibPQError handle "Chart connection closed") pure
          threadWaitRead fd
          await handle
        else PQ.getResult handle
    loop =
      next >>= \case
        Nothing -> pure ()
        Just result -> do
          status <- PQ.resultStatus result
          case status of
            PQ.SingleTuple -> do
              columns <- PQ.nfields result
              decoded <- PGI.runConversion (runStateT (runReaderT (PGI.unRP fromRow) (PGI.Row 0 result)) 0) conn
              case decoded of
                Ok (value, consumed) | consumed == columns -> emit value
                Ok _ -> throwIO $ ConversionFailed "" Nothing "" "" "Chart result column count does not match its decoder"
                Errors [] -> throwIO $ ConversionFailed "" Nothing "" "" "Chart row decoder failed without an error"
                Errors [err] -> throwIO err
                Errors errors -> throwIO $ PGI.SomePostgreSqlException $ ManyErrors errors
            PQ.TuplesOk -> pure ()
            _ -> PGI.throwResultError "chart stream" result status
          loop


-- | A separate framed endpoint preserves the existing complete JSON contract.
-- Only IO values escape the handler. The producer exists only while the response
-- is consumed, and the bounded queue propagates browser backpressure to libpq.
queryMetricsStream :: (Effectful.Reader.Static.Reader AuthContext :> es, Error ServerError :> es, Time.Time :> es) => M Text -> M DataType -> M Projects.ProjectId -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> M BinDensity -> [(Text, Maybe Text)] -> Eff es (Servant.Headers '[Servant.Header "Cache-Control" Text, Servant.Header "X-Accel-Buffering" Text] (Servant.SourceIO AE.Value))
queryMetricsStream dbSource dataTypeM pidM queryM querySQLM sinceM fromM toM sourceM densityM allParams = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  pid <- maybe (throwError err400{errBody = "project_id is required"}) pure pidM
  let requested = maybeToMonoid dataTypeM
      (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker (Utils.nonEmptyT sinceM) (Utils.nonEmptyT fromM) (Utils.nonEmptyT toM))
      density = fromMaybe def densityM
      source = parseMaybe pSource =<< Utils.nonEmptyT sourceM
      mapping = variablePresets density pid.toText fromD toD allParams now
      mappingKQL = variablePresetsKQL density pid.toText fromD toD allParams now
      parsed = first (.message) $ parseQueryDiagnosed source $ replacePlaceholders mappingKQL $ maybeToMonoid $ Utils.nonEmptyT queryM
      cfg = (defSqlQueryCfg pid now source Nothing){dateRange = (fromD, toD), binDensity = density, metricJsonAsVariant = usesTimefusionBackend authCtx.env.enableTimefusionReads dbSource}
      run emit = case parsed of
        Left message -> pure $ Left message
        Right ast -> do
          let (_, components) = queryASTToComponents cfg ast
              raw = Utils.nonEmptyT querySQLM
              actual = if isJust raw then requested else decoderFor requested ast
              sql = case raw of
                Just template -> replacePlaceholders (mapping <> M.fromList [("query_ast_filters", maybe "" (" AND " <>) components.whereClause)]) template
                Nothing -> maybeToMonoid components.finalSummarizeQuery
              convert = convertTimestampsToMs . coerceBinnedScalar requested actual
          result <- fetchMetricsDataWithProgress (Just $ emit . convert) actual sql now fromD toD authCtx dbSource
          pure $ first (Utils.sanitizeBackendError . toText . displayException) $ convert <$> result
      frame kind value = AE.object ["type" AE..= (kind :: Text), "data" AE..= value]
      body = SourceT \consume -> do
        queue <- STM.newTBQueueIO 2
        let write = STM.atomically . STM.writeTBQueue queue
            producer = do
              result <- run (write . Just . frame "partial") `E.catch` \(e :: E.IOException) -> pure (Left $ Utils.sanitizeBackendError $ toText $ displayException e)
              write $ Just $ case result of
                Left message -> AE.object ["type" AE..= ("error" :: Text), "error" AE..= message]
                Right value -> frame "complete" value
              write Nothing
            step = Effect do
              item <- STM.atomically $ STM.readTBQueue queue
              pure $ maybe Stop (\value -> Yield value step) item
        withAsync producer \worker -> do
          -- A producer failure must wake a waiting consumer rather than hang.
          UnliftIO.Async.link worker
          consume step
  pure $ Servant.addHeader "no-store, no-transform" $ Servant.addHeader "no" body


-- | NewlineFraming separates each JSON message on the wire.
data ChartStream


instance Servant.Accept ChartStream where
  contentType _ = "application" // "x-ndjson"


instance Servant.MimeRender ChartStream AE.Value where
  mimeRender _ = AE.encode
