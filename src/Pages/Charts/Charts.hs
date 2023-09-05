{-# LANGUAGE DerivingVia #-}

module Pages.Charts.Charts (chartsGetH, ChartType (..), throughput, latency, throughputEndpointHTML, latencyEndpointHTML, lazy, ChartExp (..), QueryBy (..), GroupBy (..)) where

import Config (DashboardM, pool)
import Data.Aeson qualified as AE
import Data.List (groupBy, lookup)
import Data.Text (toLower)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, diffUTCTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Tuple.Extra (fst3, thd3)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, withPool)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT)
import Lucid
import Lucid.Htmx
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURI)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (FromHttpApiData (..))
import Utils (DBField (MkDBField))
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

queryReqDump :: [ChartExp] -> DBT IO (Vector (Int, Int, String))
queryReqDump exps = do
  let (q, args) = buildReqDumpSQL exps
  query Select (Query $ encodeUtf8 q) args

-- test the query generation
-- >>> buildReqDumpSQL [TypeE BarCT, GByE GBEndpoint, QByE (QBStatusCodeGT 201), QByE (QBAnd (QBShapeHash "hash") (QBFormatHash "hash"))]
-- ("SELECT extract(epoch from time_bucket('1209600 seconds', created_at))::integer as timeB, COALESCE(COUNT(*), 0) total_count ,method||' '||url_path as g FROM apis.request_dumps WHERE project_id=?  AND  GROUP BY timeB ,method, url_path LIMIT 1000",[MkDBField MkDBField 201,MkDBField MkDBField "hash",MkDBField MkDBField "hash"])
--
-- >>> import Data.Time.Format
-- >>> let from = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" "2023-01-01 12:00:00 +0000" :: Maybe ZonedTime
-- >>> let to = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" "2023-01-14 12:00:00 +0000" :: Maybe ZonedTime
-- >>> let pid = Projects.ProjectId $ Unsafe.fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
-- >>> buildReqDumpSQL $ [GByE GBStatusCode, QByE $ QBAnd (QBPId pid) (QBStatusCodeGT 400) , SlotsE 120, ShowLegendE, Theme "roma"] ++ catMaybes [FromE <$> from, ToE <$> to]
-- ("SELECT extract(epoch from time_bucket('9360 seconds', created_at))::integer as timeB, COALESCE(COUNT(*), 0) total_count ,status_code::text as status_code FROM apis.request_dumps WHERE project_id=?  AND  created_at BETWEEN ? AND ?  GROUP BY timeB ,status_code LIMIT 1000",[MkDBField MkDBField ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000},MkDBField MkDBField 400,MkDBField 2023-01-01 12:00:00 +0000,MkDBField 2023-01-14 12:00:00 +0000])
-- >>> buildReqDumpSQL $ [GByE GBStatusCode, QByE $ QBAnd (QBPId pid) (QBStatusCodeGT 400) , SlotsE 120, ShowLegendE, Theme "roma"] ++ catMaybes [FromE <$> from, ToE <$> to] 

-- 
buildReqDumpSQL :: [ChartExp] -> (Text, [DBField])
buildReqDumpSQL exps = (q, args)
 where
  (intervalT, groupByFields, gBy, queryBy, limit, projectId, fromE, toE) = foldr go (60 * 60 * 24 * 14, ""::Text, ""::Text, [], 1000, Nothing, Nothing, Nothing) exps

  calcInterval numSlotsE' fromE' toE' = floor (diffUTCTime (zonedTimeToUTC toE') (zonedTimeToUTC fromE')) `div` numSlotsE'

  go (SlotsE n) (_, gbF, gb, qb, l, p, Just f, Just t) = (calcInterval n f t, gbF, gb, qb, l, p, Just f, Just t)
  go (SlotsE n) (_, gbF, gb, qb, l, p, f, t) = (n, gbF, gb, qb, l, p, f, t)
  go (GByE GBEndpoint) (i, _, gb, qb, l, p, f, t) = (i, ",method, url_path", ",method||' '||url_path as g", qb, l, p, f, t)
  go (GByE GBStatusCode) (i, _, gb, qb, l, p, f, t) = (i, ",status_code", ",status_code::text as status_code", qb, l, p, f, t)
  go (QByE qb) (i, gbF, gb, qbOld, l, p, f, t) = (i, gbF, gb, qb : qbOld, l, p, f, t)
  go (TypeE _) options = options
  go (LimitE lm) (i, gbF, gb, qb, _, p, f, t) = (i, gbF, gb, qb, lm, p, f, t)
  go (FromE zt) (i, gbF, gb, qb, l, p, _, t) = (i, gbF, gb, qb, l, p, Just zt, t)
  go (ToE zt) (i, gbF, gb, qb, l, p, f, _) = (i, gbF, gb, qb, l, p, f, Just zt)
  go _ acc = acc

  (qByTxtList, qByArgs) = unzip $ runQueryBySql <$> queryBy

  q =
    T.concat
      [ "SELECT extract(epoch from time_bucket('"
      , show intervalT
      , " seconds', created_at))::integer as timeB, "
      , "COALESCE(COUNT(*), 0) total_count "
      , toText gBy
      , " FROM apis.request_dumps WHERE "
      , if not $ null qByTxtList then " AND " else "" <> T.intercalate " AND " qByTxtList
      , dateRangeStr
      , " GROUP BY timeB "
      , toText groupByFields
      , " LIMIT " <> show limit
      ]

  dateRangeStr =
    if isJust fromE || isJust toE
      then " created_at BETWEEN ? AND ? "
      else ""

  args = catMaybes $ [MkDBField <$> projectId] ++ (Just . MkDBField <$> join qByArgs) ++ [MkDBField <$> fromE, MkDBField <$> toE]

  -- >>> runQueryBy (QBEndpointHash "hash")
  -- "endpoint_hash=hash"
  runQueryBySql :: QueryBy -> (Text, [DBField])
  runQueryBySql (QBPId t) = ("project_id=?", [MkDBField t])
  runQueryBySql (QBEndpointHash t) = ("endpoint_hash=?", [MkDBField t])
  runQueryBySql (QBShapeHash t) = ("shape_hash=?", [MkDBField t])
  runQueryBySql (QBFormatHash t) = ("?=ANY(format_hashes)", [MkDBField t])
  runQueryBySql (QBStatusCodeGT t) = ("status_code>?", [MkDBField t])
  runQueryBySql (QBAnd a b) =
    let (txt1, arg1) = runQueryBySql a
        (txt2, arg2) = runQueryBySql b
     in (" ( " <> txt1 <> " AND " <> txt2 <> " ) ", arg1 ++ arg2)

type M = Maybe

chartsGetH :: Sessions.PersistentSession -> M ChartType -> M GroupBy -> M QueryBy -> M Int -> M Int -> M ZonedTime -> M ZonedTime -> M Text -> M Text -> M Bool -> DashboardM (Html ())
chartsGetH _ typeM groupByM queryByM slotsM limitsM fromM toM themeM idM showLegendM = do
  let chartExps =
        catMaybes
          [ TypeE <$> typeM
          , GByE <$> groupByM
          , QByE <$> queryByM
          , SlotsE <$> slotsM
          , LimitE <$> limitsM
          , FromE <$> fromM
          , ToE <$> toM
          , Theme <$> themeM
          , IdE <$> idM
          , showLegendM >>= (\x -> if x then Just ShowLegendE else Nothing)
          ]
  pool <- asks pool
  chartData <- liftIO $ withPool pool $ queryReqDump chartExps
  let (headers, groupedData) = pivot' $ toList chartData
  let headersJSON = decodeUtf8 $ AE.encode headers
  let groupedDataJSON = decodeUtf8 $ AE.encode $ transpose groupedData
  randomID <- liftIO UUIDV4.nextRandom
  let idAttr = fromMaybe (UUID.toText randomID) idM
  let showLegend = toLower $ show $ fromMaybe False showLegendM
  let chartThemeTxt = fromMaybe "" themeM
  let fromDStr = maybe "" show fromM
  let toDStr = maybe "" show toM

  let scriptContent = [text| throughputEChartTable("$idAttr",$headersJSON, $groupedDataJSON, ["Endpoint"], $showLegend, "$chartThemeTxt", "$fromDStr", "$toDStr") |]

  pure $ do
    div_ [id_ $ toText idAttr, class_ "w-full h-full"] ""
    script_ scriptContent

data QueryBy
  = QBPId  Projects.ProjectId
  | QBEndpointHash Text
  | QBShapeHash Text
  | QBFormatHash Text
  | QBStatusCodeGT Int
  | QBAnd QueryBy QueryBy
  deriving stock (Show, Read)

instance FromHttpApiData QueryBy where
  parseQueryParam :: Text -> Either Text QueryBy
  parseQueryParam = readEither . toString

data GroupBy
  = GBEndpoint
  | GBStatusCode
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
  =  TypeE ChartType
  | GByE GroupBy
  | QByE QueryBy
  | SlotsE Int
  | LimitE Int
  | FromE ZonedTime
  | ToE ZonedTime
  | Theme Text
  | IdE Text
  | ShowLegendE
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
  runChartExp (FromE fr) = "from=" <> (iso8601Show . zonedTimeToUTC) fr
  runChartExp (ToE to) = "to=" <> (iso8601Show . zonedTimeToUTC) to
  runChartExp (Theme theme) = toString $ "theme=" <> theme
  runChartExp (IdE ide) = "id=" <> toString ide
  runChartExp ShowLegendE = "show_legend=true"

---------------------------- OLD functions
--

throughputEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Html ())
throughputEndpointHTML _ pid idM groupBy_ endpointHash shapeHash formatHash statusCodeGT numSlotsM limitM showLegend_ fromDStrM toDStrM chartTheme = do
  pool <- asks pool
  let fromDStr = fromMaybe "" fromDStrM
  let toDStr = fromMaybe "" toDStrM
  let fromD = utcToZonedTime utc <$> (iso8601ParseM (from @Text fromDStr) :: Maybe UTCTime)
  let toD = utcToZonedTime utc <$> (iso8601ParseM (from @Text toDStr) :: Maybe UTCTime)

  let entityId = fromMaybe "" idM
  let groupByField = maybe "" (\x -> "\"" <> x <> "\"") groupBy_
  let showLegend = T.toLower $ show (Just True == showLegend_)
  let chartThemeTxt = fromMaybe "" chartTheme

  script <- case groupBy_ of
    Just _ -> do
      chartData <- liftIO $ withPool pool $ RequestDumps.throughputBy' pid groupBy_ endpointHash shapeHash formatHash statusCodeGT (fromMaybe 0 numSlotsM) limitM Nothing (fromD, toD)
      let (headers, groupedData) = pivot' $ toList chartData
      let headersJSON = decodeUtf8 $ AE.encode headers
      let groupedDataJSON = decodeUtf8 $ AE.encode $ transpose groupedData
      pure [text| throughputEChartTable("id-$entityId",$headersJSON, $groupedDataJSON, [$groupByField], $showLegend, "$chartThemeTxt", "$fromDStr", "$toDStr") |]
    Nothing -> do
      chartData <- liftIO $ withPool pool $ RequestDumps.throughputBy pid groupBy_ endpointHash shapeHash formatHash statusCodeGT (fromMaybe 0 numSlotsM) limitM Nothing (fromD, toD)
      pure [text| throughputEChart("id-$entityId", $chartData, [$groupByField], $showLegend, "$chartThemeTxt") |]
  pure $ do
    div_ [id_ $ "id-" <> entityId, class_ "w-full h-full"] ""
    script_ script

latencyEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Html ())
latencyEndpointHTML _ pid idM endpointHash numSlotsM fromDStr toDStr chartTheme = do
  pool <- asks pool
  let fromD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
  let toD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)
  chartData <- liftIO $ withPool pool $ RequestDumps.latencyBy pid endpointHash (fromMaybe 0 numSlotsM) (fromD, toD)
  let chartThemeTxt = fromMaybe "" chartTheme
  let entityId = fromMaybe "" idM
  pure $ do
    div_ [id_ $ "id-" <> entityId, class_ "w-full h-full"] ""
    script_ [text| latencyEChart("id-$entityId", $chartData, "$chartThemeTxt") |]

-- TODO: Delete after migrating to new chart strategy
-- >>> runQueryBy (QBEndpointHash "hash")
-- "endpoint_hash=hash"
runQueryBy :: QueryBy -> Text
runQueryBy (QBPId t) = "project_id=" <> show t
runQueryBy (QBEndpointHash t) = "endpoint_hash=" <> t
runQueryBy (QBShapeHash t) = "shape_hash=" <> t
runQueryBy (QBFormatHash t) = "format_hash=" <> t
runQueryBy (QBStatusCodeGT t) = "status_code_gt=" <> show t
runQueryBy (QBAnd a b) = runQueryBy a <> "&" <> runQueryBy b

-- TODO: Delete after migrating to new chart strategy
runGroupBy :: GroupBy -> Text
runGroupBy GBEndpoint = "group_by=endpoint"
runGroupBy GBStatusCode = "group_by=status_code"

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

latency :: Projects.ProjectId -> Text -> Maybe QueryBy -> Int -> (Maybe ZonedTime, Maybe ZonedTime) -> Maybe Text -> Html ()
latency pid elemID qByM numSlots' (fromD, toD) chartTheme = do
  let pidT = pid.toText
  let queryBy = runQueryBy <$> qByM
  let queryStr = [queryBy] & catMaybes & T.intercalate "&"
  let numSlots = show numSlots'
  let fromDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) fromD
  let toDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) toD
  let theme = fromMaybe "" chartTheme

  div_
    [ id_ $ "id-" <> elemID
    , class_ "w-full h-full"
    , hxGet_ [text| /p/$pidT/charts_html/latency?id=$elemID&num_slots=$numSlots&$queryStr&from=$fromDStr&to=$toDStr&theme=$theme |]
    , hxTrigger_ "intersect"
    , hxSwap_ "outerHTML"
    ]
    ""
