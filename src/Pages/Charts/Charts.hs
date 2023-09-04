{-# LANGUAGE DerivingVia #-}

module Pages.Charts.Charts (chartsGetH, ChartType(..), throughput, latency, throughputEndpointHTML, latencyEndpointHTML,lazy, ChartExp(..), QueryBy (..), GroupBy (..)) where

import Config (DashboardM, pool)
import Data.Aeson qualified as AE
import Data.List (groupBy, lookup)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Traversable (for)
import Data.Tuple.Extra (fst3, thd3)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool, QueryNature (Select), query)
import Lucid
import Lucid.Htmx
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURI, normalizeEscape, normalizePathSegments)
import Relude
import Relude.Unsafe qualified as Unsafe
import Witch (from)
import Data.UUID.V4 qualified as UUIDV4
import Debug.Pretty.Simple (pTraceShowM, pTraceM)
import Servant (FromHttpApiData (..))
import Database.PostgreSQL.Transact (DBT)
import Data.Vector (Vector)
import Utils (DBField (MkDBField))
import Database.PostgreSQL.Simple.Types (Query(Query))
import Data.Text (toLower)

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


buildReqDumpSQL :: [ChartExp] -> (Text, [DBField])
buildReqDumpSQL exps = (q, args)
  where
    (intervalT, groupByFields, gBy, queryBy, limit, projectId, fromE, toE) =
      foldr go ("60", "", "", [], 1000, Nothing, Nothing, Nothing) exps

    go (SlotsE n) (_, gbF, gb, qb, l, p, f, t) = (show n, gbF, gb, qb, l, p, f, t)
    go (GByE GBEndpoint) (i, _, gb, qb, l, p, f, t) = (i, ",method, url_path", ",method||' '||url_path as g", qb, l, p, f, t)
    go (GByE GBStatusCode) (i, _, gb, qb, l, p, f, t) = (i, ",status_code", ",status_code::text", qb, l, p, f, t)
    go (QByE qb) (i, gbF, gb, qbOld, l, p, f, t) = (i, gbF, gb, qb:qbOld, l, p, f, t)
    go (TypeE _) options = options 
    go (LimitE lm) (i, gbF, gb, qb, _, p, f, t) = (i, gbF, gb, qb, lm, p, f, t)
    go (PIdE pid) (i, gbF, gb, qb, l, _, f, t) = (i, gbF, gb, qb, l, Just pid, f, t)
    go (FromE zt) (i, gbF, gb, qb, l, p, _, t) = (i, gbF, gb, qb, l, p, Just $ show zt, t)
    go (ToE zt) (i, gbF, gb, qb, l, p, f, _) = (i, gbF, gb, qb, l, p, f, Just $ show zt)
    go _ acc = acc

    (qByTxtList, qByArgs) = unzip $ runQueryBySql <$> queryBy

    q = T.concat
      [ "SELECT extract(epoch from time_bucket('"
      , toText intervalT
      , " seconds', created_at))::integer as timeB, "
      , "COALESCE(COUNT(*), 0) total_count "
      , toText gBy
      , " FROM apis.request_dumps WHERE project_id=? "
      , T.intercalate " and " qByTxtList
      , dateRangeStr
      , " GROUP BY timeB "
      , toText groupByFields
      , " LIMIT " <> show limit
      ]

    dateRangeStr = if isJust fromE || isJust toE
                   then " AND created_at BETWEEN ? AND ?"
                   else ""

    args = catMaybes $ [MkDBField <$> projectId] ++ (Just . MkDBField <$> (join qByArgs)) ++ [MkDBField <$> fromE, MkDBField <$> toE]

    -- >>> runQueryBy (QBEndpointHash "hash")
    -- "endpoint_hash=hash"
    runQueryBySql :: QueryBy -> (Text, [DBField])
    runQueryBySql (QBEndpointHash t) = ("endpoint_hash=?", [MkDBField t])
    runQueryBySql (QBShapeHash t) = ("shape_hash=?", [MkDBField t])
    runQueryBySql (QBFormatHash t) = ("?=ANY(format_hashes)", [MkDBField t])
    runQueryBySql (QBStatusCodeGT t) = ("status_code>?", [MkDBField t])
    runQueryBySql (QBAnd a b) = let (txt1, arg1) = runQueryBySql a 
                                    (txt2, arg2) = runQueryBySql b
                                in (" ( " <> txt1 <> " AND " <> txt2 <> " ) ", arg1 ++ arg2) 




type M = Maybe

chartsGetH ::Sessions.PersistentSession ->M Projects.ProjectId -> M ChartType ->M GroupBy ->M QueryBy ->M Int ->M Int ->M ZonedTime ->M ZonedTime ->M Text ->M Text ->M Bool ->DashboardM (Html ())
chartsGetH _ pidM typeM groupByM queryByM slotsM limitsM fromM toM themeM idM showLegendM= do
  let chartExps = catMaybes [ PIdE <$> pidM
          , TypeE <$> typeM
          , GByE <$> groupByM
          , QByE <$> queryByM
          , SlotsE <$> slotsM
          , LimitE <$> limitsM
          , FromE <$> fromM
          , ToE <$> toM
          , Theme <$> themeM
          , IdE <$> idM
          , showLegendM >>= (\x-> if x then Just ShowLegendE else Nothing)
        ]
  pool <- asks pool
  chartData <- liftIO $ withPool pool $ queryReqDump chartExps
  let (headers, groupedData) = pivot' $ toList chartData
  let headersJSON = decodeUtf8 $ AE.encode headers
  let groupedDataJSON = decodeUtf8 $ AE.encode $ transpose groupedData
  randomID <- liftIO $ UUIDV4.nextRandom
  let idAttr = fromMaybe (UUID.toText randomID) idM
  let groupByField = maybe "" show groupByM
  let showLegend = toLower $ show $ fromMaybe False showLegendM
  let chartThemeTxt = fromMaybe "" themeM
  let fromDStr = maybe "" show fromM 
  let toDStr = maybe "" show toM

  let scriptContent = [text| throughputEChartTable("$idAttr",$headersJSON, $groupedDataJSON, ["Endpoint"], $showLegend, "$chartThemeTxt", "$fromDStr", "$toDStr") |]

  
  pure $ do
    div_ [id_ $ toText idAttr, class_ "w-full h-full"] ""
    script_ scriptContent

data QueryBy
  = QBEndpointHash Text
  | QBShapeHash Text
  | QBFormatHash Text
  | QBStatusCodeGT Int
  | QBAnd QueryBy QueryBy
  deriving stock (Show, Read)

instance FromHttpApiData QueryBy where
    parseQueryParam =  readEither . toString


data GroupBy
  = GBEndpoint
  | GBStatusCode
  deriving stock (Show, Read)

instance FromHttpApiData GroupBy where
  parseQueryParam = readEither . toString


data ChartType
  = BarCT
  | LineCT
  deriving stock (Show, Read)

instance FromHttpApiData ChartType where
    parseQueryParam =  readEither . toString


data ChartExp
  = PIdE Projects.ProjectId
  | TypeE ChartType
  | GByE GroupBy
  | QByE QueryBy
  | SlotsE Int
  | LimitE Int
  | FromE ZonedTime
  | ToE ZonedTime
  | Theme Text
  | IdE Text
  | ShowLegendE
  deriving stock Show

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
  runChartExp (PIdE pid) = toString $ "pid=" <> pid.toText
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
