{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Pages.Charts.Charts (queryMetrics, MetricsData (..), MetricsStats (..), DataType (..)) where

import Control.Exception.Annotated (checkpoint)
import Data.Aeson qualified as AE
import Data.Annotation (toAnnotation)
import Data.Default
import Data.List (maximum)
import Data.Map.Strict qualified as M
import Data.Semigroup (Max (..))
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Simple.Types (Only (..), Query (Query))
import Database.PostgreSQL.Transact qualified as DBT
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.State.Static.Local qualified as State
import Effectful.Time qualified as Time
import Language.Haskell.TH.Syntax qualified as THS
import Models.Projects.Projects qualified as Projects
import Pkg.Components qualified as Components
import Pkg.DashboardUtils qualified as DashboardUtils
import Pkg.Parser (
  QueryComponents (finalTimechartQuery),
  SqlQueryCfg (dateRange),
  defSqlQueryCfg,
  pSource,
  parseQueryToAST,
  queryASTToComponents,
 )
import Pkg.Parser qualified as Parser
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (FromHttpApiData (..))
import System.Types
import Text.Megaparsec (parseMaybe)
import Utils (JSONHttpApiData (..))


pivot' :: V.Vector (Int, Double, Text) -> (V.Vector Text, V.Vector (V.Vector (Maybe Double)), Double, Double)
pivot' rows
  | V.null rows = (V.empty, V.empty, 0.0, 0.0)
  | otherwise =
      let extractHeaders vec = V.uniq . V.map thd3 . V.modify (\mvec -> VA.sortBy (comparing thd3) mvec) $ vec
          headers = extractHeaders rows
          grouped =
            V.groupBy (\a b -> fst3 a == fst3 b)
              $ V.modify (\mvec -> VA.sortBy (comparing fst3) mvec) rows
          ngrouped = map (transform headers) grouped
          totalSum = V.sum $ V.map snd3 rows

          -- Calculate rate (rows per minute)
          timeVec = V.map fst3 rows
          minTime = V.minimum timeVec
          maxTime = V.maximum timeVec
          timeSpanMinutes = fromIntegral (maxTime - minTime) / 60.0
          numRows = fromIntegral $ V.length rows
          rate = if timeSpanMinutes > 0 then numRows / timeSpanMinutes else 0.0
       in (headers, V.fromList ngrouped, totalSum, rate)


transform :: V.Vector Text -> V.Vector (Int, Double, Text) -> V.Vector (Maybe Double)
transform fields tuples =
  V.cons (Just timestamp) (V.map getValue fields)
  where
    getValue field = V.find (\(_, _, b) -> b == field) tuples >>= \(_, a, _) -> Just a
    timestamp = fromIntegral $ fromMaybe 0 $ fst3 <$> V.find (const True) tuples


statsTriple :: V.Vector (Int, Double, Text) -> MetricsStats
statsTriple v
  | V.null v = MetricsStats 0 0 0 0 0 0 0
  | otherwise = MetricsStats mn mx tot cnt (tot / fromIntegral cnt) mode maxGroupSum
  where
    -- Extract the Double values from each tuple
    doubles = V.map (\(_, d, _) -> d) v

    (!mn, !mx, !tot, !cnt, !freq, !timestampMap) =
      V.foldl'
        ( \(a, b, c, d, m, tsMap) (ts, x, _) ->
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
        else maximum $ M.elems timestampMap

    mode =
      fst
        $ M.foldlWithKey'
          ( \acc@(_, cnt') k c ->
              if c > cnt' then (k, c) else acc
          )
          (V.head doubles, 0)
          freq


type M = Maybe


data MetricsStats = MetricsStats
  { min :: Double
  , max :: Double
  , sum :: Double
  , count :: Int
  , mean :: Double
  , mode :: Double
  , maxGroupSum :: Double
  }
  deriving (Show, Generic, THS.Lift)
  deriving anyclass (NFData, Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricsStats


data MetricsData = MetricsData
  { dataset :: V.Vector (V.Vector (Maybe Double))
  , dataFloat :: Maybe Double
  , dataJSON :: V.Vector (V.Vector AE.Value)
  , dataText :: V.Vector (V.Vector Text)
  , headers :: V.Vector Text
  , rowsCount :: Double
  , rowsPerMin :: Maybe Double
  , from :: Maybe Int
  , to :: Maybe Int
  , stats :: Maybe MetricsStats
  }
  deriving (Show, Generic)
  deriving anyclass (NFData, Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricsData


data DataType = DTMetric | DTJson | DTFloat | DTText
  deriving stock (Show, Eq, Generic, Enum, Bounded, Ord, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "DT", DAE.CamelToSnake]] DataType
  deriving (FromHttpApiData) via Utils.JSONHttpApiData DataType
  deriving (Semigroup, Monoid) via (Max DataType)


-- Helper function: converts Just "" to Nothing.
nonNull :: Maybe Text -> Maybe Text
nonNull Nothing = Nothing
nonNull (Just "") = Nothing
nonNull x = x


queryMetrics :: (State.State TriggerEvents :> es, Time.Time :> es, DB :> es, Log :> es) => M DataType -> M Projects.ProjectId -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> [(Text, Maybe Text)] -> Eff es MetricsData
queryMetrics (maybeToMonoid -> respDataType) pidM (nonNull -> queryM) (nonNull -> queryASTM) (nonNull -> querySQLM) (nonNull -> sinceM) (nonNull -> fromM) (nonNull -> toM) (nonNull -> sourceM) allParams = do
  now <- Time.currentTime
  let (fromD, toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  let mappng = DashboardUtils.variablePresets (maybe "" (.toText) pidM) fromD toD allParams
  let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST $ DashboardUtils.replacePlaceholders mappng q)

  sqlQuery <- case (queryM, queryASTM, querySQLM) of
    (_, _, Just querySQL) -> do
      queryAST <-
        maybe
          (parseQuery $ maybeToMonoid queryM)
          (either (const $ parseQuery $ maybeToMonoid queryM) pure . AE.eitherDecode . encodeUtf8)
          queryASTM
      let sqlQueryComponents =
            (defSqlQueryCfg (Unsafe.fromJust pidM) now (parseMaybe pSource =<< sourceM) Nothing)
              { dateRange = (fromD, toD)
              }
      let (_, qc) = Parser.queryASTToComponents sqlQueryComponents queryAST

      let mappng' = mappng <> M.fromList [("query_ast_filters", maybe "" (" AND " <>) qc.whereClause)]
      pure $ DashboardUtils.replacePlaceholders mappng' querySQL -- FIXME: risk of sql injection and many other attacks
    _ -> do
      queryAST <-
        maybe
          (parseQuery $ maybeToMonoid queryM)
          (either (const $ parseQuery $ maybeToMonoid queryM) pure . AE.eitherDecode . encodeUtf8)
          queryASTM
      let sqlQueryComponents =
            (defSqlQueryCfg (Unsafe.fromJust pidM) now (parseMaybe pSource =<< sourceM) Nothing)
              { dateRange = (fromD, toD)
              }
      let (_, qc) = queryASTToComponents sqlQueryComponents queryAST
      pure $ maybeToMonoid qc.finalTimechartQuery

  let baseMetricsData =
        def
          { from = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe (addUTCTime (-86400) now) fromD
          , to = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe now toD
          }
  case respDataType of
    DTFloat -> do
      chartData <- checkpoint (toAnnotation sqlQuery) $ dbtToEff $ DBT.queryOne_ (Query $ encodeUtf8 sqlQuery)
      pure
        baseMetricsData
          { dataFloat = chartData <&> \(Only v) -> v
          , rowsCount = 1
          }
    DTMetric -> do
      chartData <- checkpoint (toAnnotation sqlQuery) $ dbtToEff $ DBT.query_ (Query $ encodeUtf8 sqlQuery)
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
      chartData <- checkpoint (toAnnotation sqlQuery) $ dbtToEff $ DBT.query_ (Query $ encodeUtf8 sqlQuery)
      pure
        baseMetricsData
          { dataText = V.fromList chartData
          , rowsCount = fromIntegral $ length chartData
          }
    DTJson -> do
      chartData <- checkpoint (toAnnotation sqlQuery) $ dbtToEff $ DBT.query_ (Query $ encodeUtf8 sqlQuery)
      pure
        baseMetricsData
          { dataJSON = V.fromList chartData
          , rowsCount = fromIntegral $ length chartData
          }
