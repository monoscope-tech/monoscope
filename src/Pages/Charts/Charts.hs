{-# LANGUAGE NoFieldSelectors #-}

module Pages.Charts.Charts (queryMetrics, MetricsData (..), fetchMetricsData, MetricsStats (..), DataType (..)) where

import Control.Exception.Annotated (checkpoint)
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
import Database.PostgreSQL.Simple (query_)
import Database.PostgreSQL.Simple.Types (Only (..), Query (Query), fromOnly)
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Types (DataType (..), MetricsData (..), MetricsStats (..))
import Pkg.Components.TimePicker qualified as Components
import Pkg.DashboardUtils qualified as DashboardUtils
import Pkg.Parser (
  QueryComponents (finalSummarizeQuery),
  SqlQueryCfg (..),
  defSqlQueryCfg,
  pSource,
  parseQueryToAST,
  queryASTToComponents,
 )
import Pkg.Parser qualified as Parser
import Pkg.Parser.Stats (Section, Sources)
import Pkg.QueryCache qualified as QC
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant.Server (ServerError (errBody), err400)
import System.Config (AuthContext (..), EnvConfig (..))
import System.DB (DB)
import Text.Megaparsec (parseMaybe)


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


-- Helper function: converts Just "" to Nothing.
nonNull :: Maybe Text -> Maybe Text
nonNull Nothing = Nothing
nonNull (Just "") = Nothing
nonNull x = x


queryMetrics :: (DB es, Effectful.Error.Static.Error ServerError :> es, Effectful.Reader.Static.Reader AuthContext :> es, Time.Time :> es) => M DataType -> M Projects.ProjectId -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> [(Text, Maybe Text)] -> Eff es MetricsData
queryMetrics (maybeToMonoid -> respDataType) pidM (nonNull -> queryM) (nonNull -> querySQLM) (nonNull -> sinceM) (nonNull -> fromM) (nonNull -> toM) (nonNull -> sourceM) allParams = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  let (fromD, toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  let mappng = DashboardUtils.variablePresets (maybe "" (.toText) pidM) fromD toD allParams now
  let parseQuery q = either (\err -> throwError err400{errBody = "Invalid signature; " <> show err}) pure (parseQueryToAST $ DashboardUtils.replacePlaceholders mappng q)

  case (queryM, querySQLM) of
    (_, Just querySQL) -> do
      queryAST <-
        checkpoint (toAnnotation ("queryMetrics", queryM))
          $ parseQuery
          $ maybeToMonoid queryM
      let sqlQueryComponents =
            (defSqlQueryCfg (Unsafe.fromJust pidM) now (parseMaybe pSource =<< sourceM) Nothing)
              { dateRange = (fromD, toD)
              }
      let (_, qc) = Parser.queryASTToComponents sqlQueryComponents queryAST
      let mappng' = mappng <> M.fromList [("query_ast_filters", maybe "" (" AND " <>) qc.whereClause)]
      let sqlQuery = DashboardUtils.replacePlaceholders mappng' querySQL
      liftIO $ fetchMetricsData respDataType sqlQuery now fromD toD authCtx
    _ -> do
      queryAST <-
        checkpoint (toAnnotation ("queryMetrics", queryM))
          $ parseQuery
          $ maybeToMonoid queryM
      let pid = Unsafe.fromJust pidM
      let source = parseMaybe pSource =<< sourceM
      let sqlQueryCfg = (defSqlQueryCfg pid now source Nothing){dateRange = (fromD, toD)}
      queryMetricsWithCache authCtx respDataType pid source queryAST sqlQueryCfg (maybeToMonoid queryM) now fromD toD


-- | Execute query with caching support for timeseries queries
queryMetricsWithCache
  :: DB es
  => AuthContext
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
queryMetricsWithCache authCtx respDataType pid source queryAST sqlQueryCfg originalQuery now fromD toD
  | respDataType /= DTMetric = executeQuery sqlQueryCfg
  | not (QC.hasSummarizeWithBin queryAST) = executeQuery sqlQueryCfg
  | otherwise = do
      let reqFrom = fromMaybe (addUTCTime (-86400) now) fromD
      let reqTo = fromMaybe now toD
      let cacheKey = QC.generateCacheKey pid source queryAST sqlQueryCfg
      cacheResult <- QC.lookupCache cacheKey (reqFrom, reqTo)
      case cacheResult of
        QC.CacheHit entry -> pure $ QC.trimToRange entry.cachedData reqFrom reqTo
        QC.PartialHit entry -> do
          let deltaFromTime = entry.cachedTo
          let deltaSqlCfg = sqlQueryCfg{dateRange = (Just deltaFromTime, Just reqTo)}
          deltaData <- executeQuery deltaSqlCfg
          let merged = QC.mergeTimeseriesData entry.cachedData deltaData
          let slidingWindowStart = addUTCTime (-86400) reqTo
          let trimmed = QC.trimOldData slidingWindowStart merged
          QC.updateCache cacheKey (entry.cachedFrom, reqTo) trimmed originalQuery
          pure $ QC.trimToRange trimmed reqFrom reqTo
        QC.CacheMiss -> do
          result <- executeQuery sqlQueryCfg
          QC.updateCache cacheKey (reqFrom, reqTo) result originalQuery
          pure result
        QC.CacheBypassed _ -> executeQuery sqlQueryCfg
  where
    executeQuery cfg = do
      let (_, qc) = queryASTToComponents cfg queryAST
      let sqlQuery = maybeToMonoid qc.finalSummarizeQuery
      liftIO $ fetchMetricsData respDataType sqlQuery now fromD toD authCtx


fetchMetricsData :: DataType -> Text -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> AuthContext -> IO MetricsData
fetchMetricsData respDataType sqlQuery now fromD toD authCtx = do
  let pool = if authCtx.env.enableTimefusionReads then authCtx.timefusionPgPool else authCtx.pool
  let baseMetricsData =
        def
          { from = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe (addUTCTime (-86400) now) fromD
          , to = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe now toD
          }

  checkpoint (toAnnotation (respDataType, sqlQuery)) $ case respDataType of
    DTFloat -> do
      chartData <- withResource pool \conn -> query_ conn (Query $ encodeUtf8 sqlQuery) :: IO [Only Double]
      pure
        baseMetricsData
          { dataFloat = fromOnly <$> listToMaybe chartData
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
