module Pages.Charts.Charts (throughput, latency, throughputEndpointHTML, latencyEndpointHTML, QueryBy (..), GroupBy (..)) where

import Config (DashboardM, pool)
import Data.Aeson qualified as AE
import Data.List (groupBy, lookup)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Tuple.Extra (fst3, thd3)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Relude
import Relude.Unsafe qualified as Unsafe
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

throughputEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Html ())
throughputEndpointHTML _ pid idM groupBy_ endpointHash shapeHash formatHash statusCodeGT numSlotsM limitM showLegend_ fromDStr toDStr chartTheme = do
  pool <- asks pool
  let fromD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
  let toD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)

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
      pure [text| throughputEChartTable("id-$entityId",$headersJSON, $groupedDataJSON, [$groupByField], $showLegend, "$chartThemeTxt") |]
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

data QueryBy
  = QBEndpointHash Text
  | QBShapeHash Text
  | QBFormatHash Text
  | QBStatusCodeGT Int
  | QBAnd QueryBy QueryBy

runQueryBy :: QueryBy -> Text
runQueryBy (QBEndpointHash t) = "endpoint_hash=" <> t
runQueryBy (QBShapeHash t) = "shape_hash=" <> t
runQueryBy (QBFormatHash t) = "format_hash=" <> t
runQueryBy (QBStatusCodeGT t) = "status_code_gt=" <> show t
runQueryBy (QBAnd a b) = runQueryBy a <> "&" <> runQueryBy b

data GroupBy
  = GBEndpoint
  | GBStatusCode

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
