{-# LANGUAGE DerivingVia #-}

module Pages.Charts.Charts (chartsGetH, ChartType (..), throughput, throughputEndpointHTML, lazy, ChartExp (..), QueryBy (..), GroupBy (..)) where

import Data.Aeson qualified as AE
import Data.List (groupBy, lookup)
import Data.Text (toLower)
import Data.Text qualified as T
import Data.Time (
  UTCTime,
  ZonedTime,
  addUTCTime,
  defaultTimeLocale,
  diffUTCTime,
  formatTime,
  getCurrentTime,
  secondsToNominalDiffTime,
  utc,
  utcToZonedTime,
  zonedTimeToUTC,
 )
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Tuple.Extra (fst3, thd3)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, query_, withPool)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask, asks)
import Lucid
import Lucid.Htmx
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURI)
import Pages.NonMember
import Pkg.Parser
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import Servant (FromHttpApiData (..))
import System.Config
import System.Config (DashboardM, pool)
import System.Types
import Utils (DBField (MkDBField), userIsProjectMember)
import Witch (from)


transform :: [String] -> [(Int, Int, String)] -> [Maybe Int]
transform fields tuples =
  Just timestamp : map getValue fields
  where
    getValue field = lookup field (map swap_ tuples)
    swap_ (_, a, b) = (b, a)
    timestamp = fst3 $ Unsafe.head tuples


pivot' :: [(Int, Int, String)] -> ([String], [[Maybe Int]])
pivot' rows = do
  let extractHeaders = ordNub . map thd3 . sortOn thd3
  let headers = extractHeaders rows
  let grouped = groupBy (\a b -> fst3 a == fst3 b) $ sortOn fst3 rows
  let ngrouped = map (transform headers) grouped
  (headers, ngrouped)


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
buildReqDumpSQL :: [ChartExp] -> (Text, [DBField], Maybe ZonedTime, Maybe ZonedTime)
buildReqDumpSQL exps = (q, join qByArgs, mFrom, mTo)
  where
    (slots, groupByFields, gBy, queryBy, limit, q) = foldr go (120, "" :: Text, "" :: Text, [], 10000, qDefault) exps
    go (SlotsE n) (_, gbF, gb, qb, l, qd) = (n, gbF, gb, qb, l, qd)
    go (GByE GBEndpoint) (i, _, gb, qb, l, qd) = (i, ",method, url_path", ",method||' '||url_path as g", qb, l, qd)
    go (GByE GBStatusCode) (i, _, gb, qb, l, qd) = (i, ",status_code", ",status_code::text as status_code", qb, l, qd)
    go (GByE GBDurationPercentile) (i, gbF, gb, qb, l, qd) = (i, gbF, gb, qb, l, qForDurationPercentile)
    go (QByE qb) (i, gbF, gb, qbOld, l, qd) = (i, gbF, gb, qb ++ qbOld, l, qd)
    go (TypeE _) options = options
    go (LimitE lm) (i, gbF, gb, qb, _, qd) = (i, gbF, gb, qb, lm, qd)
    go _ acc = acc

    (qByTxtList, qByArgs) = unzip $ runQueryBySql <$> queryBy

    qDefault =
      T.concat
        [ "SELECT extract(epoch from time_bucket('"
        , show $ fromMaybe 3600 $ calculateIntervalFromQuery slots (mFrom, mTo) queryBy
        , " seconds', created_at))::integer as timeB, "
        , "COALESCE(COUNT(*), 0) total_count "
        , toText gBy
        , " FROM apis.request_dumps"
        , if null qByTxtList then "" else " WHERE " <> T.intercalate " AND " qByTxtList
        , " GROUP BY timeB "
        , toText groupByFields
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

    dateRangeFromQueryBy :: [QueryBy] -> (Maybe ZonedTime, Maybe ZonedTime)
    dateRangeFromQueryBy = foldl' goDateRange (Nothing, Nothing)
      where
        goDateRange :: (Maybe ZonedTime, Maybe ZonedTime) -> QueryBy -> (Maybe ZonedTime, Maybe ZonedTime)
        goDateRange acc@(Just _from, Just _to) _ = acc -- Both from and to found, no need to continue
        goDateRange acc@(mFrom, mTo) (QBAnd a b) = acc -- TODO: support checking QBAnd for date range foldl' goDateRange (mFrom, mTo) [a, b]
        goDateRange (mFrom, mTo) (QBFrom from_) = (Just from_, mTo)
        goDateRange (mFrom, mTo) (QBTo to_) = (mFrom, Just to_)
        goDateRange acc _ = acc -- Ignore all other constructors
    calcInterval numSlotsE' fromE' toE' = floor (diffUTCTime (zonedTimeToUTC toE') (zonedTimeToUTC fromE')) `div` numSlotsE'

    (mFrom, mTo) = dateRangeFromQueryBy queryBy

    calculateIntervalFromQuery :: Int -> (Maybe ZonedTime, Maybe ZonedTime) -> [QueryBy] -> Maybe Int
    calculateIntervalFromQuery numSlots (mFrom', mTo') queryList = do
      to_ <- mTo'
      from_ <- mFrom'
      return $ calcInterval numSlots from_ to_


type M = Maybe


formatZonedTimeAsUTC :: ZonedTime -> Text
formatZonedTimeAsUTC zonedTime = formatUTC (zonedTimeToUTC zonedTime)


formatUTC :: UTCTime -> Text
formatUTC utcTime =
  toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (utcTime)


chartsGetH :: M ChartType -> M Text -> M Projects.ProjectId -> M GroupBy -> M [QueryBy] -> M Int -> M Int -> M Text -> M Text -> M Bool -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (Html ())
chartsGetH typeM Nothing pidM groupByM queryByM slotsM limitsM themeM idM showLegendM sinceM fromM toM = chartsGetDef typeM Nothing pidM groupByM queryByM slotsM limitsM themeM idM showLegendM sinceM fromM toM
chartsGetH typeM (Just queryRaw) pidM groupByM queryByM slotsM limitsM themeM idM showLegendM sinceM fromM toM = chartsGetRaw typeM queryRaw pidM groupByM queryByM slotsM limitsM themeM idM showLegendM sinceM fromM toM


--  chartsGetRaw and chartGetDef should be refactored and merged.
chartsGetRaw :: M ChartType -> Text -> M Projects.ProjectId -> M GroupBy -> M [QueryBy] -> M Int -> M Int -> M Text -> M Text -> M Bool -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (Html ())
chartsGetRaw typeM queryRaw pidM groupByM queryByM slotsM limitsM themeM idM showLegendM sinceM fromM toM = do
  let cType = case fromMaybe BarCT typeM of
        BarCT -> "bar"
        LineCT -> "line"

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
  now <- liftIO getCurrentTime
  -- Temp workaround to get access to fromM and toM
  -- let (_, _, fromM, toM) =  buildReqDumpSQL chartExps

  let (fromD, toD, currentRange) = case sinceM of
        Just "1H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime 3600) now, Just now, Just "Last Hour")
        Just "24H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24) now, Just $ now, Just "Last 24 Hours")
        Just "7D" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 7) now, Just now, Just "Last 7 Days")
        Just "14D" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 14) now, Just now, Just "Last 14 Days")
        _ -> do
          let f = (iso8601ParseM (from @Text $ fromMaybe "" fromM) :: Maybe UTCTime)
          let t = (iso8601ParseM (from @Text $ fromMaybe "" toM) :: Maybe UTCTime)
          let start = toText . formatTime defaultTimeLocale "%F %T" <$> f
          let end = toText . formatTime defaultTimeLocale "%F %T" <$> t
          let range = case (start, end) of
                (Just s, Just e) -> Just (s <> "-" <> e)
                _ -> Nothing
          (f, t, range)

  traceShowM "==="
  traceShowM fromD
  traceShowM sinceM
  traceShowM "==="

  let Right (_, qc) = parseQueryToComponents (defSqlQueryCfg $ Unsafe.fromJust pidM) queryRaw
  chartData <- dbtToEff $ query_ Select (Query $ encodeUtf8 $ Unsafe.fromJust qc.finalTimechartQuery)

  let (headers, groupedData) = pivot' $ toList chartData
      headersJSON = decodeUtf8 $ AE.encode headers
      groupedDataJSON = decodeUtf8 $ AE.encode $ transpose groupedData
      idAttr = fromMaybe (UUID.toText randomID) idM
      showLegend = toLower $ show $ fromMaybe False showLegendM
      chartThemeTxt = fromMaybe "" themeM
      fromDStr = maybe "" formatUTC fromD
      toDStr = maybe "" formatUTC toD
      cType = case fromMaybe BarCT typeM of
        BarCT -> "bar"
        LineCT -> "line"
      scriptContent = [text| throughputEChartTable("$idAttr",$headersJSON, $groupedDataJSON, ["Endpoint"], $showLegend, "$chartThemeTxt", "$fromDStr", "$toDStr", "$cType") |]
  pure do
    div_ [id_ $ toText idAttr, class_ "w-full h-full"] ""
    script_ scriptContent


chartsGetDef :: M ChartType -> M Text -> M Projects.ProjectId -> M GroupBy -> M [QueryBy] -> M Int -> M Int -> M Text -> M Text -> M Bool -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (Html ())
chartsGetDef typeM queryRaw pidM groupByM queryByM slotsM limitsM themeM idM showLegendM sinceM fromM toM = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

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

  let (headers, groupedData) = pivot' $ toList chartData
      headersJSON = decodeUtf8 $ AE.encode headers
      groupedDataJSON = decodeUtf8 $ AE.encode $ transpose groupedData
      idAttr = fromMaybe (UUID.toText randomID) idM
      showLegend = toLower $ show $ fromMaybe False showLegendM
      chartThemeTxt = fromMaybe "" themeM
      fromDStr = maybe "" formatZonedTimeAsUTC fromM
      toDStr = maybe "" formatZonedTimeAsUTC toM
      cType = case fromMaybe BarCT typeM of
        BarCT -> "bar"
        LineCT -> "line"
      scriptContent = [text| throughputEChartTable("$idAttr",$headersJSON, $groupedDataJSON, ["Endpoint"], $showLegend, "$chartThemeTxt", "$fromDStr", "$toDStr", "$cType") |]

  pure do
    div_ [id_ $ toText idAttr, class_ "w-full h-full"] ""
    script_ scriptContent


data QueryBy
  = QBPId Projects.ProjectId
  | QBEndpointHash Text
  | QBShapeHash Text
  | QBFormatHash Text
  | QBStatusCodeGT Int
  | QBFrom ZonedTime
  | QBTo ZonedTime
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


---------------------------- OLD functions
--

throughputEndpointHTML :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (Html ())
throughputEndpointHTML pid idM groupBy_ endpointHash shapeHash formatHash statusCodeGT numSlotsM limitM showLegend_ fromDStrM toDStrM chartTheme = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  isMember <- dbtToEff $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      let fromDStr = fromMaybe "" fromDStrM
      let toDStr = fromMaybe "" toDStrM
      let entityId = fromMaybe "" idM
      let fromD = utcToZonedTime utc <$> (iso8601ParseM (from @Text fromDStr) :: Maybe UTCTime)
      let toD = utcToZonedTime utc <$> (iso8601ParseM (from @Text toDStr) :: Maybe UTCTime)

      let groupByField = maybe "" (\x -> "\"" <> x <> "\"") groupBy_
      let showLegend = T.toLower $ show (Just True == showLegend_)
      let chartThemeTxt = fromMaybe "" chartTheme

      script <- case groupBy_ of
        Just _ -> do
          chartData <- dbtToEff $ RequestDumps.throughputBy' pid groupBy_ endpointHash shapeHash formatHash statusCodeGT (fromMaybe 0 numSlotsM) limitM Nothing (fromD, toD)
          let (headers, groupedData) = pivot' $ toList chartData
          let headersJSON = decodeUtf8 $ AE.encode headers
          let groupedDataJSON = decodeUtf8 $ AE.encode $ transpose groupedData
          pure [text| throughputEChartTable("id-$entityId",$headersJSON, $groupedDataJSON, [$groupByField], $showLegend, "$chartThemeTxt", "$fromDStr", "$toDStr") |]
        Nothing -> do
          chartData <- dbtToEff $ RequestDumps.throughputBy pid groupBy_ endpointHash shapeHash formatHash statusCodeGT (fromMaybe 0 numSlotsM) limitM Nothing (fromD, toD)
          pure [text| throughputEChart("id-$entityId", $chartData, [$groupByField], $showLegend, "$chartThemeTxt") |]
      pure do
        div_ [id_ $ "id-" <> entityId, class_ "w-full h-full"] ""
        script_ script


-- TODO: Delete after migrating to new chart strategy
-- >>> runQueryBy (QBEndpointHash "hash")
-- "endpoint_hash=hash"
runQueryBy :: QueryBy -> Text
runQueryBy (QBPId t) = "project_id=" <> show t
runQueryBy (QBEndpointHash t) = "endpoint_hash=" <> t
runQueryBy (QBShapeHash t) = "shape_hash=" <> t
runQueryBy (QBFormatHash t) = "format_hash=" <> t
runQueryBy (QBStatusCodeGT t) = "status_code_gt=" <> show t
runQueryBy (QBHost t) = "host=" <> t
runQueryBy (QBFrom t) = ""
runQueryBy (QBTo t) = ""
runQueryBy (QBAnd a b) = runQueryBy a <> "&" <> runQueryBy b


-- TODO: Delete after migrating to new chart strategy
runGroupBy :: GroupBy -> Text
runGroupBy GBEndpoint = "group_by=endpoint"
runGroupBy GBStatusCode = "group_by=status_code"
runGroupBy GBDurationPercentile = "group_by=duration_percentile"
runGroupBy GBHost = "group_by=host"


-- This endpoint will return a throughput chart partial
throughput :: Projects.ProjectId -> Text -> Maybe QueryBy -> Maybe GroupBy -> Int -> Maybe Int -> Bool -> (Maybe ZonedTime, Maybe ZonedTime) -> Maybe Text -> Html ()
throughput pid elemID qByM gByM numSlots' limit' showLegend' (fromD, toD) chartTheme = do
  let pidT = pid.toText
  let queryBy = runQueryBy <$> qByM
  let gBy = runGroupBy <$> gByM
  let queryStr = [queryBy, gBy] & catMaybes & T.intercalate "&"
  let showLegend = T.toLower $ show showLegend'
  let numSlots = show numSlots'
  let limit = maybe "" (\x -> "limit=" <> show x) limit'
  let fromDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) fromD
  let toDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) toD
  let theme = fromMaybe "" chartTheme

  div_
    [ id_ $ "id-" <> elemID
    , class_ "w-full h-full"
    , hxGet_ [text| /p/$pidT/charts_html/throughput?id=$elemID&show_legend=$showLegend&num_slots=$numSlots&$limit&$queryStr&from=$fromDStr&to=$toDStr&theme=$theme |]
    , hxTrigger_ "intersect"
    , hxSwap_ "outerHTML"
    ]
    ""
