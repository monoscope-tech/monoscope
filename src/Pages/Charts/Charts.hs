{-# LANGUAGE DerivingVia,NoFieldSelectors #-}

module Pages.Charts.Charts (chartsGetH, ChartType (..), lazy, ChartExp (..), QueryBy (..), GroupBy (..), queryMetrics, queryFloat, MetricsData (..)) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Simple.Types (Query (Query), Only(..))
import Database.PostgreSQL.Transact qualified as DBT
import Data.Map.Strict qualified as M
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.State.Static.Local qualified as State
import Effectful.Time qualified as Time
import Lucid (Html, class_, div_, id_, script_)
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_)
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURI)
import Pkg.Components qualified as Components
import Pkg.Parser (
  QueryComponents (finalTimechartQuery),
  SqlQueryCfg (dateRange),
  defSqlQueryCfg,
  pSource,
  parseQueryToAST,
  queryASTToComponents,
 )
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (FromHttpApiData (..))
import System.Types
import Text.Megaparsec (parseMaybe)
import Utils (DBField (MkDBField), formatUTC)


pivot' :: V.Vector (Int, Double, Text) -> (V.Vector Text, V.Vector (V.Vector (Maybe Double)), Double, Double)
pivot' rows =
  let extractHeaders vec = V.uniq . V.map thd3 . V.modify (\mvec -> VA.sortBy (comparing thd3) mvec) $ vec
      headers = extractHeaders rows
      grouped = V.groupBy (\a b -> fst3 a == fst3 b)
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
  | V.null v  = MetricsStats 0 0 0 0 0 0
  | otherwise = MetricsStats mn mx tot cnt (tot / fromIntegral cnt) mode
  where
    -- Extract the Double values from each tuple
    doubles = V.map (\(_, d, _) -> d) v

    (!mn, !mx, !tot, !cnt, !freq) =
      V.foldl' (\(a, b, c, d, m) x ->
                  ( min a x
                  , max b x
                  , c + x
                  , d + 1
                  , M.insertWith (+) x 1 m ))
               (V.head doubles, V.head doubles, 0, 0, M.empty)
               doubles

    mode = fst $ M.foldlWithKey' (\acc@(_, cnt') k c ->
                                    if c > cnt' then (k, c) else acc)
                                  (V.head doubles, 0)
                                  freq


-- test the query generation
-- >>> buildReqDumpSQL [TypeE BarCT, GByE GBEndpoint, QByE [QBStatusCodeGT 201, QBShapeHash "hash", QBFormatHash "hash"]]
-- ("SELECT extract(epoch from time_bucket('3600 seconds', created_at))::integer as timeB, COALESCE(COUNT(*), 0) total_count ,method||' '||url_path as g FROM apis.request_dumps WHERE status_code>? AND shape_hash=? AND ?=ANY(format_hashes) GROUP BY timeB ,method, url_path LIMIT 1000",[MkDBField 201,MkDBField "hash",MkDBField "hash"])
--
--
-- >>> import Data.Time.Format
-- >>> let from = Unsafe.fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" "2023-01-01 12:00:00 +0000" :: ZonedTime
-- >>> let to = Unsafe.fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" "2023-01-14 12:00:00 +0000" :: ZonedTime
-- >>> let pid = Projects.ProjectId $ Unsafe.fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
-- >>> buildReqDumpSQL $ [GByE GBStatusCode, QByE [QBPId pid, QBStatusCodeGT 400, QBFrom from, QBTo to] , SlotsE 10, ShowLegendE, Theme "roma"]
-- ("SELECT extract(epoch from time_bucket('112320 seconds', created_at))::integer as timeB, COALESCE(COUNT(*), 0) total_count ,status_code::text as status_code FROM apis.request_dumps WHERE project_id=? AND status_code>? AND created_at>=? AND created_at<? GROUP BY timeB ,status_code LIMIT 1000",[MkDBField ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000},MkDBField 400,MkDBField 2023-01-01 12:00:00 +0000,MkDBField 2023-01-14 12:00:00 +0000])
--
--
-- >>> buildReqDumpSQL $ [GByE GBStatusCode, QByE [QBPId pid, QBStatusCodeGT 400, QBFrom from, QBTo to] , SlotsE 120, ShowLegendE, Theme "roma"]
-- ("SELECT extract(epoch from time_bucket('9360 seconds', created_at))::integer as timeB, COALESCE(COUNT(*), 0) total_count ,status_code::text as status_code FROM apis.request_dumps WHERE project_id=? AND status_code>? AND created_at>=? AND created_at<? GROUP BY timeB ,status_code LIMIT 1000",[MkDBField ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000},MkDBField 400,MkDBField 2023-01-01 12:00:00 +0000,MkDBField 2023-01-14 12:00:00 +0000])
--
--
buildReqDumpSQL :: [ChartExp] -> (Text, [DBField], Maybe UTCTime, Maybe UTCTime)
buildReqDumpSQL exps = (q, join qByArgs, mFrom, mTo)
  where
    (slots, groupByFields, gBy, queryBy, limit, q, extr) = foldr go (120, "" :: Text, "" :: Text, [], 10000, qDefault, "" :: Text) exps
    go (SlotsE n) (_, gbF, gb, qb, l, qd, _) = (n, gbF, gb, qb, l, qd, "")
    go (GByE GBEndpoint) (i, _, gb, qb, l, qd, _) = (i, ",method, url_path", ",method||' '||url_path as g", qb, l, qd, "g")
    go (GByE GBStatusCode) (i, _, gb, qb, l, qd, _) = (i, ",status_code", ",status_code::text as status_code", qb, l, qd, "status_code")
    go (GByE GBDurationPercentile) (i, gbF, gb, qb, l, qd, _) = (i, gbF, gb, qb, l, qForDurationPercentile, "")
    go (QByE qb) (i, gbF, gb, qbOld, l, qd, _) = (i, gbF, gb, qb ++ qbOld, l, qd, "")
    go (TypeE _) options = options
    go (LimitE lm) (i, gbF, gb, qb, _, qd, _) = (i, gbF, gb, qb, lm, qd, "")
    go _ acc = acc

    (qByTxtList, qByArgs) = unzip $ runQueryBySql <$> queryBy

    qDefault =
      T.concat
        [ "WITH RankedGroups AS ( SELECT extract(epoch from time_bucket('"
        , show $ fromMaybe 3600 $ calculateIntervalFromQuery slots (mFrom, mTo) queryBy
        , " seconds', created_at))::integer as timeB, "
        , "COALESCE(COUNT(*), 0) total_count "
        , toText gBy
        , ", ROW_NUMBER() OVER (PARTITION BY extract(epoch from time_bucket('"
        , show $ fromMaybe 3600 $ calculateIntervalFromQuery slots (mFrom, mTo) queryBy
        , " seconds', created_at))::integer ORDER BY COUNT(*) DESC) as group_rank"
        , " FROM apis.request_dumps"
        , if null qByTxtList then "" else " WHERE " <> T.intercalate " AND " qByTxtList
        , " GROUP BY timeB "
        , toText groupByFields
        , " )"
        , "SELECT timeB, total_count, "
        , toText extr
        , " FROM RankedGroups WHERE group_rank <= 20"
        , " LIMIT " <> show limit
        ]

    qForDurationPercentile =
      T.concat
        [ "WITH Percentiles AS ( "
        , "SELECT extract(epoch from time_bucket('"
        , show $ fromMaybe 10080 $ calculateIntervalFromQuery slots (mFrom, mTo) queryBy
        , " seconds', created_at))::integer as timeB, "
        , "PERCENTILE_CONT(ARRAY[0.5, 0.75, 0.9]) WITHIN GROUP (ORDER BY duration_ns) as percentiles "
        , "FROM apis.request_dumps "
        , if null qByTxtList then "" else " WHERE " <> T.intercalate " AND " qByTxtList
        , " GROUP BY timeB "
        , ") SELECT timeB, "
        , "(unnest(percentiles)/1000000)::integer as percentile_value, "
        , "unnest(ARRAY['p50', 'p75', 'p90']) as percentile_type "
        , "FROM Percentiles "
        , "LIMIT " <> show limit
        ]

    -- >>> runQueryBy (QBEndpointHash "hash")
    -- "endpoint_hash=hash"
    runQueryBySql :: QueryBy -> (Text, [DBField])
    runQueryBySql (QBPId t) = ("project_id=?", [MkDBField t])
    runQueryBySql (QBEndpointHash t) = ("endpoint_hash=?", [MkDBField t])
    runQueryBySql (QBShapeHash t) = ("shape_hash=?", [MkDBField t])
    runQueryBySql (QBFormatHash t) = ("?=ANY(format_hashes)", [MkDBField t])
    runQueryBySql (QBStatusCodeGT t) = ("status_code>?", [MkDBField t])
    runQueryBySql (QBFrom t) = ("created_at>=?", [MkDBField t])
    runQueryBySql (QBTo t) = ("created_at<?", [MkDBField t])
    runQueryBySql (QBHost t) = ("host=?", [MkDBField t])
    runQueryBySql (QBAnd a b) =
      let (txt1, arg1) = runQueryBySql a
          (txt2, arg2) = runQueryBySql b
       in (" ( " <> txt1 <> " AND " <> txt2 <> " ) ", arg1 ++ arg2)

    dateRangeFromQueryBy :: [QueryBy] -> (Maybe UTCTime, Maybe UTCTime)
    dateRangeFromQueryBy = foldl' goDateRange (Nothing, Nothing)
      where
        goDateRange :: (Maybe UTCTime, Maybe UTCTime) -> QueryBy -> (Maybe UTCTime, Maybe UTCTime)
        goDateRange acc@(Just _from, Just _to) _ = acc -- Both from and to found, no need to continue
        goDateRange acc (QBAnd a b) = acc -- TODO: support checking QBAnd for date range foldl' goDateRange (mFrom, mTo) [a, b]
        goDateRange (_mFrom, mTop) (QBFrom from_) = (Just from_, mTop)
        goDateRange (mFromp, _mTo) (QBTo to_) = (mFromp, Just to_)
        goDateRange acc _ = acc -- Ignore all other constructors
    calcInterval numSlotsE' fromE' toE' = floor (diffUTCTime toE' fromE') `div` numSlotsE'

    (mFrom, mTo) = dateRangeFromQueryBy queryBy

    calculateIntervalFromQuery :: Int -> (Maybe UTCTime, Maybe UTCTime) -> [QueryBy] -> Maybe Int
    calculateIntervalFromQuery numSlots (mFrom', mTo') queryList = do
      to_ <- mTo'
      from_ <- mFrom'
      return $ calcInterval numSlots from_ to_


type M = Maybe


chartsGetH :: M ChartType -> M Text -> M Text -> M Projects.ProjectId -> M GroupBy -> M [QueryBy] -> M Int -> M Int -> M Text -> M Text -> M Bool -> M Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
chartsGetH typeM Nothing Nothing pidM groupByM queryByM slotsM limitsM themeM idM showLegendM showAxesM sinceM fromM toM sourceM = chartsGetDef typeM Nothing pidM groupByM queryByM slotsM limitsM themeM idM showLegendM showAxesM sinceM fromM toM sourceM
chartsGetH typeM queryRawM queryASTM pidM groupByM queryByM slotsM limitsM themeM idM showLegendM showAxesM sinceM fromM toM sourceM = chartsGetRaw typeM queryRawM queryASTM pidM groupByM queryByM slotsM limitsM themeM idM showLegendM showAxesM sinceM fromM toM sourceM

data MetricsStats = MetricsStats 
  { min ::Double 
  , max :: Double 
  , sum :: Double 
  , count :: Int
  , mean :: Double 
  , mode :: Double
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricsStats

data MetricsData = MetricsData
  { dataset :: V.Vector (V.Vector (Maybe Double))
  , dataFloat :: Maybe Double
  , headers :: V.Vector Text
  , rowsCount ::Double 
  , rowsPerMin :: Maybe Double
  , from :: Maybe Int
  , to :: Maybe Int
  , stats :: Maybe MetricsStats
  }
  deriving (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetricsData


queryMetrics :: (State.State TriggerEvents :> es, Time.Time :> es, DB :> es, Log :> es) => M Projects.ProjectId -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> Eff es MetricsData
queryMetrics pidM queryM queryASTM querySQLM sinceM fromM toM sourceM= do
  now <- Time.currentTime
  let (fromD, toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  sqlQuery <- case (queryM, queryASTM, querySQLM) of
    (_, _, Just querySQL) -> pure querySQL -- FIXME: risk of sql injection and many other attacks
    _ -> do
      let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST q)
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
      pure $ fromMaybe "" qc.finalTimechartQuery

  chartData <- dbtToEff $ DBT.query_ (Query $ encodeUtf8 $ sqlQuery)
  let chartsDataV = V.fromList chartData
  let (headers, groupedData, rowsCount, rowsPerMin) = pivot' chartsDataV
  pure $
    MetricsData
      { dataset = groupedData
      , dataFloat = Nothing
      , headers = V.cons "timestamp" headers
      , rowsCount
      , rowsPerMin = Just rowsPerMin
      , from = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe (addUTCTime (-86400) now) fromD
      , to = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe now toD
      , stats = Just $ statsTriple chartsDataV
      }


queryFloat :: (State.State TriggerEvents :> es, Time.Time :> es, DB :> es, Log :> es) => M Projects.ProjectId -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> M Text -> Eff es MetricsData
queryFloat pidM queryM queryASTM querySQLM sinceM fromM toM sourceM = do
  now <- Time.currentTime
  let (fromD, toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  sqlQuery <- case (queryM, queryASTM, querySQLM) of
    (_, _, Just querySQL) -> do
      pure querySQL -- FIXME: risk of sql injection and many other attacks
    _ -> do
      let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST q)
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
      pure $ fromMaybe "" qc.finalTimechartQuery

  chartData <- dbtToEff $ DBT.queryOne_ (Query $ encodeUtf8 $ sqlQuery)
  pure $
    MetricsData
      { dataset = V.empty
      , dataFloat = chartData <&> \(Only v) -> v
      , headers = V.empty
      , rowsCount = 1
      , rowsPerMin = Nothing 
      , from = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe (addUTCTime (-86400) now) fromD
      , to = Just $ round . utcTimeToPOSIXSeconds $ fromMaybe now toD
      , stats = Nothing
      }


--  FIXME: chartsGetRaw and chartGetDef should be refactored and merged.
chartsGetRaw :: M ChartType -> M Text -> M Text -> M Projects.ProjectId -> M GroupBy -> M [QueryBy] -> M Int -> M Int -> M Text -> M Text -> M Bool -> M Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
chartsGetRaw typeM queryM queryASTM pidM groupByM queryByM slotsM limitsM themeM idM showLegendM showAxesM sinceM fromM toM sourceM = do
  randomID <- liftIO UUIDV4.nextRandom

  let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST q)
  queryAST <-
    maybe
      (parseQuery $ maybeToMonoid queryM)
      (either (const $ parseQuery $ maybeToMonoid queryM) pure . AE.eitherDecode . encodeUtf8)
      queryASTM

  now <- Time.currentTime
  let (fromD, toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)

  let sqlQueryComponents =
        (defSqlQueryCfg (Unsafe.fromJust pidM) now (parseMaybe pSource =<< sourceM) Nothing)
          { dateRange = (fromD, toD)
          }
  let (_, qc) = queryASTToComponents sqlQueryComponents queryAST
  chartData <- dbtToEff $ DBT.query_ (Query $ encodeUtf8 $ fromMaybe "" qc.finalTimechartQuery)
  let (headers, groupedData, _, _) = pivot' $ V.fromList chartData
      headersJSON = decodeUtf8 $ AE.encode headers
      groupedDataJSON = decodeUtf8 $ AE.encode $ transpose $ V.toList $ V.toList <$> groupedData
      idAttr = fromMaybe (UUID.toText randomID) idM
      showLegend = T.toLower $ show $ fromMaybe False showLegendM
      showAxes = T.toLower $ show $ fromMaybe True showAxesM
      chartThemeTxt = fromMaybe "" themeM
      fromDStr = maybe "" formatUTC fromD
      toDStr = maybe "" formatUTC toD
      cType = case fromMaybe BarCT typeM of
        BarCT -> "bar"
        LineCT -> "line"
      scriptContent = [text| throughputEChartTable("$idAttr",$headersJSON, $groupedDataJSON, ["Endpoint"], $showLegend, $showAxes, "$chartThemeTxt", "$fromDStr", "$toDStr", "$cType") |]
  addRespHeaders do
    div_ [id_ $ toText idAttr, class_ "w-full h-full"] ""
    script_ scriptContent


chartsGetDef :: M ChartType -> M Text -> M Projects.ProjectId -> M GroupBy -> M [QueryBy] -> M Int -> M Int -> M Text -> M Text -> M Bool -> M Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
chartsGetDef typeM queryRaw pidM groupByM queryByM slotsM limitsM themeM idM showLegendM showAxesM sinceM _fromM _toM sourceM = do
  let chartExps =
        catMaybes
          [ TypeE <$> typeM
          , GByE <$> groupByM
          , QByE <$> queryByM
          , SlotsE <$> slotsM
          , LimitE <$> limitsM
          , Theme <$> themeM
          , IdE <$> idM
          , showLegendM >>= (\x -> if x then Just ShowLegendE else Nothing)
          ]
  randomID <- liftIO UUIDV4.nextRandom
  let (q, args, fromM, toM) = case groupByM of
        Just GBDurationPercentile -> buildReqDumpSQL chartExps
        _ -> buildReqDumpSQL chartExps
  chartData <- dbtToEff $ query Select (Query $ encodeUtf8 q) args

  let (headers, groupedData, _, _) = pivot' $ chartData
      headersJSON = decodeUtf8 $ AE.encode headers
      groupedDataJSON = decodeUtf8 $ AE.encode $ transpose $ V.toList $ V.toList <$> groupedData
      idAttr = fromMaybe (UUID.toText randomID) idM
      showLegend = T.toLower $ show $ fromMaybe False showLegendM
      showAxes = T.toLower $ show $ fromMaybe True showAxesM
      chartThemeTxt = fromMaybe "" themeM
      fromDStr = maybe "" formatUTC fromM
      toDStr = maybe "" formatUTC toM
      cType = case fromMaybe BarCT typeM of
        BarCT -> "bar"
        LineCT -> "line"
      scriptContent = [text| throughputEChartTable("$idAttr",$headersJSON, $groupedDataJSON, ["Endpoint"], $showLegend, $showAxes, "$chartThemeTxt", "$fromDStr", "$toDStr", "$cType") |]

  addRespHeaders do
    div_ [id_ $ toText idAttr, class_ "w-full h-full"] ""
    script_ scriptContent


data QueryBy
  = QBPId Projects.ProjectId
  | QBEndpointHash Text
  | QBShapeHash Text
  | QBFormatHash Text
  | QBStatusCodeGT Int
  | QBFrom UTCTime
  | QBTo UTCTime
  | QBHost Text
  | QBAnd QueryBy QueryBy
  deriving stock (Show, Read)


instance FromHttpApiData QueryBy where
  parseQueryParam :: Text -> Either Text QueryBy
  parseQueryParam = readEither . toString


instance FromHttpApiData [QueryBy] where
  parseQueryParam :: Text -> Either Text [QueryBy]
  parseQueryParam = readEither . toString


data GroupBy
  = GBEndpoint
  | GBHost
  | GBStatusCode
  | GBDurationPercentile
  deriving stock (Show, Read)


instance FromHttpApiData GroupBy where
  parseQueryParam :: Text -> Either Text GroupBy
  parseQueryParam = readEither . toString


data ChartType
  = BarCT
  | LineCT
  deriving stock (Show, Read)


instance FromHttpApiData ChartType where
  parseQueryParam :: Text -> Either Text ChartType
  parseQueryParam = readEither . toString


data ChartExp
  = TypeE ChartType
  | GByE GroupBy
  | QByE [QueryBy]
  | SlotsE Int
  | LimitE Int
  | Theme Text
  | IdE Text
  | ShowLegendE
  | HideAexs
  | RawE Text
  deriving stock (Show)


-- lazy Chart rendered based on a list of chart expressions
-- >>> lazy [TypeE BarCT, GByE GBEndpoint]
-- <div data-hx-get="/charts_html?chart_type=BarCT&amp;group_by=GBEndpoint" data-hx-trigger="intersect" data-hx-swap="outerHTML" class="w-full h-full"></div>
--
-- >>> lazy [TypeE BarCT, GByE GBEndpoint, QByE(QBStatusCodeGT 201), QByE(QBAnd (QBShapeHash "hash") (QBFormatHash "hash"))]
-- <div data-hx-get="/charts_html?chart_type=BarCT&amp;group_by=GBEndpoint&amp;query_by=QBStatusCodeGT%20201&amp;query_by=QBAnd%20(QBShapeHash%20%22hash%22)%20(QBFormatHash%20%22hash%22)" data-hx-trigger="intersect" data-hx-swap="outerHTML" class="w-full h-full"></div>
--
lazy :: [ChartExp] -> Html ()
lazy queries =
  div_
    [ hxGet_ $ "/charts_html?" <> qParams
    , hxTrigger_ "intersect"
    , hxSwap_ "outerHTML"
    , class_ "w-full h-full"
    ]
    ""
  where
    qParams :: Text
    qParams = toText $ escapeURIString isUnescapedInURI $ intercalate "&" $ map runChartExp queries

    runChartExp :: ChartExp -> String
    runChartExp (TypeE ct) = "chart_type=" <> show ct
    runChartExp (GByE gb) = "group_by=" <> show gb
    runChartExp (QByE qb) = "query_by=" <> show qb
    runChartExp (SlotsE limit) = "num_slots=" <> show limit
    runChartExp (LimitE limit) = "limit=" <> show limit
    runChartExp (Theme theme) = toString $ "theme=" <> theme
    runChartExp (IdE ide) = "id=" <> toString ide
    runChartExp (RawE q) = "query_raw=" <> toString q
    runChartExp ShowLegendE = "show_legend=true"
    runChartExp HideAexs = "show_axes=false"
