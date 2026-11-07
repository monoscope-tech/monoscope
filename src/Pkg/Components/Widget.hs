module Pkg.Components.Widget (Widget (..), VizType (..), vizWidget, vizIsChart, vizFor, ModelWidget, modelWidget, ThresholdLines (..), LegendSize (..), ColumnType (..), ProgressScale (..), WidgetCta (..), WidgetMarker (..), SqlOrder, mkSqlOrder, PngProfile (..), pngExportSize, WidgetDataset (..), chartQuery, tableQuery, toWidgetDataset, foldSeries, widget_, widgetValueSlot_, widgetValueSlotAs_, infraTimeseries, gridStackAttrs, normalizeWidgetLayouts, Layout (..), WidgetType (..), TableColumn (..), RowClickAction (..), mapChartTypeToWidgetType, mapWidgetTypeToChartType, widgetToECharts, WidgetAxis (..), SummarizeBy (..), statScalar, formatStatValue, widgetPostH, renderTableWithDataAndParams, signWidgetUrl, sqlSignature, verifySqlSignature, signWidgetSql, widgetPngUrl, stampPngUrl, decodeWidgetZ, widgetFetchUrl) where

import Codec.Compression.GZip qualified as GZip
import Control.Exception.Safe qualified as Safe
import Control.Lens
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as AE.KeyMap
import Data.Base64.Types qualified as B64
import Data.ByteArray qualified as BA
import Data.Char (isAlpha, isDigit, isHexDigit)
import Data.Default
import Data.Effectful.Hasql qualified as Hasql
import Data.Generics.Labels ()
import Data.List (findIndex, lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, parseTimeM, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import Effectful.Reader.Static qualified
import Language.Haskell.TH.Syntax qualified as THS
import Log qualified
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (termRaw)
import Lucid.Htmx (hxGet_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.LogQueries qualified as LogQueries (ExplorerUrl (..), LogEndpoint (..))
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Pages.Charts.Charts qualified as Charts
import Pages.Components (EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), copyButton_, emDash_, emptyState_, localTimeFmt_, searchInput_)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.DeriveUtils (JsonValueSchema (..), UUIDId (..), WrappedEnumSC (..))
import Pkg.Parser (ResultShape (..), ToQueryText (..), parseQueryToAST, replacePlaceholders, resultShape)
import Pkg.Parser.Expr (Subject (..))
import Pkg.Parser.Stats (BinFunction (..), ByClauseItem (..), Section (..), SummarizeByClause (..))
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Text.Printf (printf)
import Text.Slugify (slugify)
import Utils
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData (..))
import Web.Links
import "base16-bytestring" Data.ByteString.Base16 qualified as B16
import "base64" Data.ByteString.Base64.URL qualified as B64URL
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


-- | 'Charts.MetricsStats' lives in @shared/@, which cannot see 'JSONHttpApiData',
-- so the deriving-via is written here at the one place that needs it.
deriving via JSONHttpApiData Charts.MetricsStats instance FromHttpApiData Charts.MetricsStats


data Query = Query
  { query :: Maybe Text
  , sql :: Maybe Text
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Query


data Layout = Layout
  { x :: Maybe Int
  , y :: Maybe Int
  , w :: Maybe Int
  , h :: Maybe Int
  , minWidth :: Maybe Int
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Layout
  deriving (FromHttpApiData) via JSONHttpApiData Layout


-- | The kind of visualisation a widget renders.
--
-- Regression guard for the query-param parsing. This type used to get its
-- 'FromHttpApiData' from a blanket @FromJSON a => FromHttpApiData a@ orphan in this
-- module, which ran the raw parameter through 'AE.eitherDecodeStrict' — so
-- @widget_type=top_list@, which is what a form actually submits, did not parse, and only
-- the quoted @"top_list"@ did. It now derives via 'WrappedEnumSC', matching the JSON
-- encoding above.
--
-- >>> import Web.HttpApiData (parseQueryParam)
-- >>> parseQueryParam "top_list" :: Either Text WidgetType
-- Right WTTopList
-- >>> parseQueryParam "timeseries_line" :: Either Text WidgetType
-- Right WTTimeseriesLine
--
-- An unknown spelling is rejected rather than silently defaulting:
--
-- >>> isLeft (parseQueryParam "not_a_widget" :: Either Text WidgetType)
-- True
data WidgetType
  = WTGroup
  | WTLogs
  | WTTimeseries
  | WTTimeseriesLine
  | WTTimeseriesStat
  | WTStat
  | WTList -- https://docs.datadoghq.com/dashboards/widgets/list/ not supported yet
  | WTTopList
  | WTDistribution
  | WTGeomap
  | WTFunnel
  | WTTreeMap
  | WTPieChart
  | WTAnomalies
  | WTTable
  | WTFlamegraph
  | WTServiceMap -- Service dependency graph visualization
  | WTHeatmap -- Latency distribution heatmap
  -- Bounded so every widget type can be enumerated ([minBound ..]) rather than listed by
  -- hand: adding a constructor then extends the round-trip/render specs automatically
  -- instead of silently shipping untested.
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "WT", DAE.CamelToSnake]] WidgetType
  deriving (Display, FromHttpApiData) via WrappedEnumSC 'Nothing "WT" WidgetType


-- | A view the explorer's query box switches between. Its wire spelling (URL @viz_type@, the
-- tab values, the AI's @visualization_type@) is its widget type's, plus the explorer-only
-- @patterns@ and @sessions@ regions.
--
-- >>> import Data.Text.Display (display)
-- >>> map display [minBound .. maxBound :: VizType]
-- ["logs","timeseries","timeseries_line","top_list","patterns","sessions"]
-- >>> [display w | Just w <- map vizWidget [minBound ..]] == map display [VizLogs .. VizTopList]
-- True
data VizType = VizLogs | VizTimeseries | VizTimeseriesLine | VizTopList | VizPatterns | VizSessions
  deriving stock (Bounded, Enum, Eq, Read, Show)
  deriving (AE.ToJSON, Display, FromHttpApiData, ToSchema) via WrappedEnumSC 'Nothing "Viz" VizType


-- | The widget that draws a view. The explorer-only regions have none, so a dashboard can't hold them.
vizWidget :: VizType -> Maybe WidgetType
vizWidget = \case
  VizLogs -> Just WTLogs
  VizTimeseries -> Just WTTimeseries
  VizTimeseriesLine -> Just WTTimeseriesLine
  VizTopList -> Just WTTopList
  VizPatterns -> Nothing
  VizSessions -> Nothing


-- | Drawn by the widget container rather than the explorer's own log list or regions.
vizIsChart :: VizType -> Bool
vizIsChart = maybe False (/= WTLogs) . vizWidget


-- | Whether a view draws what a query returns. The time charts bin events and a scalar
-- ('chartQuery'), but a grouped result is a ranking: re-binning it by time answers another
-- question, so "line chart of the top 5" still lands on the top list. A tab clicked by hand
-- is not refused; this only decides which view a query opens on.
vizDraws :: VizType -> ResultShape -> Bool
vizDraws = \case
  VizTimeseries -> (/= Categories)
  VizTimeseriesLine -> (/= Categories)
  VizTopList -> (== Categories)
  VizLogs -> (== Events)
  VizPatterns -> (== Events)
  VizSessions -> (== Events)


-- | The view for a query: the requested one when it can draw the result, else the result's own.
--
-- >>> [vizFor s Nothing | s <- [Events, Scalar, TimeSeries, Categories]]
-- [VizLogs,VizTimeseries,VizTimeseries,VizTopList]
-- >>> (vizFor Categories (Just VizLogs), vizFor Categories (Just VizTimeseriesLine), vizFor Events (Just VizSessions), vizFor Events (Just VizTimeseriesLine))
-- (VizTopList,VizTopList,VizSessions,VizTimeseriesLine)
--
-- The view picked always draws the result:
--
-- >>> and [vizDraws (vizFor s v) s | s <- [Events, Scalar, TimeSeries, Categories], v <- Nothing : map Just [minBound ..]]
-- True
vizFor :: ResultShape -> Maybe VizType -> VizType
vizFor shape = fromMaybe byShape . mfilter (`vizDraws` shape)
  where
    byShape = case shape of
      Events -> VizLogs
      Scalar -> VizTimeseries
      TimeSeries -> VizTimeseries
      Categories -> VizTopList


data SummarizeBy
  = SBSum
  | SBMax
  | SBMin
  | SBCount
  | SBMean
  | SBRate
  | SBLast
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "SB", DAE.CamelToSnake]] SummarizeBy
  deriving (FromHttpApiData) via WrappedEnumSC 'Nothing "SB" SummarizeBy


-- | Prefix shown before a stat's big number. The value itself is computed and
-- formatted client-side (statScalar/formatStatValue in stat-value.ts) from the
-- same stats the chart uses, so the two paths can't disagree. Explicit clauses
-- (not a wildcard) so a new constructor must declare its prefix.
summarizeByPrefix :: SummarizeBy -> Text
summarizeByPrefix SBMax = "<"
summarizeByPrefix SBMin = ">"
summarizeByPrefix SBSum = ""
summarizeByPrefix SBCount = ""
summarizeByPrefix SBMean = ""
summarizeByPrefix SBRate = ""
summarizeByPrefix SBLast = ""


data PngProfile = PngStandard | PngSlack
  deriving stock (Eq, Generic, Read, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData) via WrappedEnumSC 'Nothing "Png" PngProfile


-- | When a chart draws its alert/warning threshold lines.
data ThresholdLines = TLAlways | TLOnBreach | TLNever
  deriving stock (Eq, Generic, Read, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromHttpApiData) via WrappedEnumSC 'Nothing "TL" ThresholdLines


data LegendSize = LSXs | LSSm | LSMd | LSLg
  deriving stock (Eq, Generic, Read, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData) via WrappedEnumSC 'Nothing "LS" LegendSize


data ColumnType = CTNumber | CTDuration | CTText
  deriving stock (Eq, Generic, Read, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData) via WrappedEnumSC 'Nothing "CT" ColumnType


-- | A progress cell's scale: share of the column's largest value, or the value itself as 0–100.
data ProgressScale = ColumnPercent | ValuePercent
  deriving stock (Eq, Generic, Read, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData) via WrappedEnumSC 'Nothing "" ProgressScale


-- | Export dimensions are part of the profile carried by the signed widget.
--
-- >>> pngExportSize Nothing
-- (900,300)
-- >>> pngExportSize (Just PngSlack)
-- (960,320)
pngExportSize :: Maybe PngProfile -> (Int, Int)
pngExportSize profile = case fromMaybe PngStandard profile of
  PngStandard -> (900, 300)
  PngSlack -> (960, 320)


-- | A trusted SQL ORDER BY expression from dashboard configuration.
-- User-provided table sorts never inhabit this type; they are represented by
-- a declared column plus a closed direction.
newtype SqlOrder = SqlOrder {unSqlOrder :: Text}
  deriving stock (Eq, Generic, Show, THS.Lift)
  deriving newtype (AE.ToJSON, NFData)


-- | Construct a non-empty SQL ordering expression.
--
-- >>> isJust $ mkSqlOrder "duration ASC"
-- True
-- >>> mkSqlOrder "   "
-- Nothing
mkSqlOrder :: Text -> Maybe SqlOrder
mkSqlOrder = fmap SqlOrder . nonEmptyT . Just . T.strip


instance AE.FromJSON SqlOrder where
  parseJSON = AE.withText "SqlOrder" $ maybe (fail "SQL order must not be empty") pure . mkSqlOrder


instance FromHttpApiData SqlOrder where
  parseQueryParam = maybeToRight "SQL order must not be empty" . mkSqlOrder


data WidgetCta = WidgetCta
  { title :: Text
  , url :: Text
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake WidgetCta
  deriving (FromHttpApiData) via JSONHttpApiData WidgetCta


-- | A labelled instant drawn as a vertical line, e.g. the release an issue first appeared in.
data WidgetMarker = WidgetMarker {label :: Text, at :: Text}
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake WidgetMarker


-- when processing widgets we'll do them async, so eager queries are loaded upfront
data Widget = Widget
  { wType :: WidgetType -- Widget type: "timeseries", "table", etc.
  , id :: Maybe Text
  , naked :: Maybe Bool
  , showTooltip :: Maybe Bool
  , title :: Maybe Text -- Widget title
  , subtitle :: Maybe Text
  , hideSubtitle :: Maybe Bool
  , hideValue :: Maybe Bool
  , icon :: Maybe Text
  , sql :: Maybe Text
  , rollupSql :: Maybe Text
  -- ^ Postgres SQL over an hourly rollup of the same series as @query@, answerable
  -- from @rollupFrom@ (ISO-8601) on; the server decides per request whether to use it
  -- (see 'Web.Routes.rollupRoute').
  , rollupFrom :: Maybe Text
  , defaultSort :: Maybe SqlOrder -- Trusted default ORDER BY for the {{table_sort}} SQL slot
  , rawQuery :: Maybe Text -- Original KQL query with {{const-...}} placeholders (for editor display)
  , summarizeBy :: Maybe SummarizeBy
  , query :: Maybe Text
  , queries :: Maybe [Query] -- Multiple queries for combined visualizations
  , layout :: Maybe Layout -- Layout (x, y, w, h)
  , xAxis :: Maybe WidgetAxis
  , yAxis :: Maybe WidgetAxis -- Optional y-axis label
  , unit :: Maybe Text
  , decimals :: Maybe Int -- Fixed precision for the headline value (Apdex 0.99 / 1)
  , value :: Maybe Double -- value could represent a number or a count
  , wData :: Maybe AE.Value
  , hideLegend :: Maybe Bool
  , legendPosition :: Maybe Text -- Legend position: "top", "bottom", "top-right", "top-left", "bottom-right", "bottom-left", or "right" (a name/value table beside the plot)
  , legendSize :: Maybe LegendSize -- default LSSm
  , theme :: Maybe Text
  , dataset :: Maybe WidgetDataset
  , -- eager
    eager :: Maybe Bool
  , timeFrom :: Maybe Text
  -- ^ Pins this widget's own query window instead of inheriting the page's. A
  -- related-metrics chart beside a span is about that span's minutes, not whatever
  -- range the explorer happens to be showing.
  , timeTo :: Maybe Text
  , highlightFrom :: Maybe Text
  -- ^ Sub-interval shaded on the chart — the subject's own extent inside the padded
  -- window. Every vendor surveyed draws this; without it the reader cannot tell
  -- whether their request was inside the spike or merely near it.
  , highlightTo :: Maybe Text
  , markers :: Maybe [WidgetMarker]
  , _projectId :: Maybe Projects.ProjectId
  , _dashboardId :: Maybe Text -- Dashboard ID for context
  , _tabSlug :: Maybe Text -- Slug of the dashboard tab holding the widget
  , _isNested :: Maybe Bool
  , _centerTitle :: Maybe Bool
  , expandBtnFn :: Maybe Text
  , expandPushUrl :: Maybe Text
  , cta :: Maybe WidgetCta
  , groupByOptions :: Maybe [Text]
  , groupBySelected :: Maybe Text
  , groupByUrl :: Maybe Text
  , groupByTarget :: Maybe Text
  , children :: Maybe [Widget]
  , html :: Maybe LText
  , standalone :: Maybe Bool -- Not used in a grid stack
  , allowZoom :: Maybe Bool -- Allow zooming in the chart
  , columns :: Maybe [TableColumn] -- Table columns
  , searchable :: Maybe Bool
  , onRowClick :: Maybe RowClickAction -- Action when table row is clicked
  -- Alert fields (populated from QueryMonitor at render time)
  , alertId :: Maybe Text -- Linked QueryMonitor ID
  , alertThreshold :: Maybe Double -- For threshold line rendering
  , warningThreshold :: Maybe Double
  , showThresholdLines :: Maybe ThresholdLines
  , seriesIntent :: Maybe Text
  -- ^ What a lone aggregate series *means*, when the series name cannot say. A bare
  -- @count(*)@ is error volume on one chart and healthy throughput on another, and the
  -- widget cannot tell them apart from the name — so red was being applied by a hash of
  -- the string. @\"error\"@ opts a chart into the error colour; anything else stays brand.
  -- Grouped series are named by their group value and keep their own mapping.
  , alertStatus :: Maybe Monitors.MonitorStatus -- runtime
  , description :: Maybe Text -- Help text shown in info icon tooltip
  , pngUrl :: Maybe Text -- Pre-signed PNG download URL (runtime)
  , sqlSig :: Maybe Text -- runtime: 'sqlSignature' of sql/rollupSql, so a viewer's request may run them
  , pngProfile :: Maybe PngProfile
  , dbSource :: Maybe Hasql.SqlSource -- Nothing = default routing
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.RejectUnknownFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake, DAE.Rename "arning_threshold" "warning_threshold"]] Widget
  -- Widget's nested types have no ToSchema; document it as an open JSON value.
  deriving (ToSchema) via JsonValueSchema Widget


instance ToHtml Widget where
  toHtml w = toHtml $ widget_ w
  toHtmlRaw = toHtml


-- | Model visualizations retain their wire shape without server execution fields.
--
-- >>> import Relude
-- >>> import Data.Aeson qualified as AE
-- >>> let wire = "{\"type\":\"stat\",\"query\":\"summarize count()\",\"title\":\"Requests\"}"
-- >>> fmap AE.toJSON (AE.eitherDecode @ModelWidget wire) == AE.eitherDecode @AE.Value wire
-- True
-- >>> map (isLeft . AE.eitherDecode @ModelWidget) ["{\"type\":\"stat\",\"sql\":\"SELECT 1\"}", "{\"type\":\"stat\",\"html\":\"<b>untrusted</b>\"}", "{\"type\":\"timeseries\",\"queries\":[{\"sql\":\"SELECT 1\"}]}", "{\"type\":\"group\",\"children\":[{\"type\":\"stat\",\"sql\":\"SELECT 1\"}]}", "{\"type\":\"stat\",\"_project_id\":\"00000000-0000-0000-0000-000000000000\"}"]
-- [True,True,True,True,True]
-- >>> isRight $ AE.eitherDecode @ModelWidget "{\"type\":\"timeseries\",\"queries\":[{\"query\":\"summarize count()\"}]}"
-- True
-- >>> isLeft $ AE.eitherDecode @ModelWidget "{\"type\":\"table\",\"columns\":[{\"field\":\"name\",\"title\":\"Name\",\"link\":\"javascript:alert(1)\"}]}"
-- True
newtype ModelWidget = ModelWidget Widget
  deriving stock (Show)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON ModelWidget where
  parseJSON value = do
    widget <- AE.parseJSON value
    unless (safe widget) $ fail "Model widgets cannot contain SQL, executable content, or server context"
    pure $ ModelWidget widget
    where
      safe widget =
        not (or [isJust widget.sql, isJust widget.rollupSql, isJust widget.sqlSig, isJust widget.html, isJust widget.expandBtnFn, isJust widget.expandPushUrl, isJust widget.groupByUrl, isJust widget.groupByTarget, isJust widget.onRowClick, isJust widget.cta, isJust widget._projectId, isJust widget._dashboardId, isJust widget._tabSlug, isJust widget.dbSource, isJust widget.pngUrl])
          && all (isNothing . (.sql)) (fold widget.queries)
          && all (isNothing . (.link)) (fold widget.columns)
          && all safe (fold widget.children)


modelWidget :: ModelWidget -> Widget
modelWidget (ModelWidget widget) = widget


data WidgetDataset = WidgetDataset
  { source :: AE.Value
  , rowsPerMin :: Maybe Double
  , value :: Maybe Double
  , from :: Maybe Int
  , to :: Maybe Int
  , stats :: Maybe Charts.MetricsStats
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] WidgetDataset
  deriving (FromHttpApiData) via JSONHttpApiData WidgetDataset


-- | The query to run for a widget, paired with the row shape it decodes into.
-- The two have to be chosen together: each decoder accepts exactly one column
-- count, so shaping the query without picking the matching 'Charts.DataType'
-- (or vice versa) fails the whole widget with a column-count mismatch.
--
--   * plotted widgets count filters per time bin, or add time bins to the
--     final aggregation without changing its measurement;
--   * a stat reads one scalar, so it only needs an aggregation when the query
--     has none; binning it would yield a three-column series;
--   * row-oriented widgets project their own columns and are passed through.
--
-- >>> chartQuery (def & #query ?~ "severity==\"ERROR\"")
-- (Just "severity==\"ERROR\" | summarize count(*) by bin_auto(timestamp)",DTMetric)
-- >>> chartQuery (def & #query ?~ "summarize count(*) by bin_auto(timestamp)")
-- (Just "summarize count(*) by bin_auto(timestamp)",DTMetric)
-- >>> chartQuery (def & #query ?~ "metrics | summarize max(value)")
-- (Just "metrics | summarize max(value) by bin_auto(timestamp)",DTMetric)
-- >>> chartQuery (def & #query ?~ "metrics | summarize max(value) | summarize min(value)")
-- (Just "metrics | summarize max(value) | summarize min(value) by bin_auto(timestamp)",DTMetric)
-- >>> chartQuery (def & #query ?~ "metrics | summarize max(value) by bin(timestamp, 60) | summarize min(value)")
-- (Just "metrics | summarize max(value) by bin(timestamp, 60) | summarize min(value) by bin_auto(timestamp)",DTMetric)
-- >>> chartQuery (def & #wType .~ WTStat & #query ?~ "kind == \"server\"")
-- (Just "kind == \"server\" | summarize count(*)",DTFloat)
-- >>> chartQuery (def & #wType .~ WTStat & #query ?~ "name != null | summarize dcount(name)")
-- (Just "name != null | summarize dcount(name)",DTFloat)
-- >>> chartQuery (def & #wType .~ WTStat & #query ?~ "metrics | where metric_name ==")
-- (Just "metrics | where metric_name ==",DTFloat)
-- >>> chartQuery (def & #wType .~ WTTable & #query ?~ "summarize count(*) by service")
-- (Just "summarize count(*) by service",DTText)
-- >>> chartQuery (def :: Widget)
-- (Nothing,DTMetric)
-- >>> chartQuery (def & #query ?~ "")
-- (Just "summarize count(*) by bin_auto(timestamp)",DTMetric)
-- >>> chartQuery (def & #query ?~ "name == \"GET\" | summarize count() by name")
-- (Just "name == \"GET\" | summarize count() by bin_auto(timestamp), name",DTMetric)
-- >>> chartQuery (def & #wType .~ WTTopList & #query ?~ "name == \"GET\" | summarize count() by name")
-- (Just "name == \"GET\" | summarize count() by name",DTText)
-- >>> chartQuery (def & #wType .~ WTDistribution & #query ?~ "summarize count() by resource.service.name")
-- (Just "summarize count() by resource.service.name",DTText)
chartQuery :: Widget -> (Maybe Text, Charts.DataType)
chartQuery w
  | w.wType `elem` [WTLogs, WTTable, WTTopList] = (w.query, Charts.DTText)
  -- A categorical chart of a grouped result plots the groups; binning it would plot time.
  | categoricalWidget w.wType, Just Categories <- resultShape <$> (rightToMaybe . parseQueryToAST =<< w.query) = (w.query, Charts.DTText)
  | otherwise = (shape <$> w.query, dataType)
  where
    isStat = w.wType == WTStat
    (suffix, dataType) = if isStat then ("", Charts.DTFloat) else (" by bin_auto(timestamp)", Charts.DTMetric)
    shape q = case parseQueryToAST q of
      -- Let the query handler report errors in the original malformed query.
      Left _ -> q
      Right ast ->
        let (summarized, binned) = mapAccumR addBin False ast
         in if not summarized then foldMap (<> " | ") (nonEmptyT (Just (T.strip q))) <> "summarize count(*)" <> suffix else if binned == ast then q else toQText binned
    addBin False section@(SummarizeCommand aggs by)
      | isStat || resultShape [section] == TimeSeries = (True, section)
      | otherwise = (True, SummarizeCommand aggs $ Just $ SummarizeByClause $ ByBinFunc (BinAuto $ Subject "timestamp" "timestamp" []) : foldMap coerce by)
    addBin seen section = (seen, section)


-- | Convert MetricsData to WidgetDataset (timestamps already in ms from queryMetrics). A grouped
-- result arrives as text rows, a label then its values, and is charted as categories.
toWidgetDataset :: Charts.MetricsData -> WidgetDataset
toWidgetDataset md =
  WidgetDataset
    { source = AE.toJSON $ V.cons (AE.toJSON <$> md.headers) $ if V.null md.dataset then V.map (V.imap textCell) md.dataText else AE.toJSON <<$>> md.dataset
    , rowsPerMin = md.rowsPerMin
    , value = Just md.rowsCount
    , from = md.from
    , to = md.to
    , stats = md.stats
    }
  where
    textCell 0 c = AE.String c
    textCell _ c = maybe (AE.String c) AE.toJSON (readMaybe @Double (toString c))


-- | A PNG can't scroll its legend, so past @n@ series the ones with the lowest peaks fold into
-- one "Other (k)" series. Each bucket's total is unchanged. The browser's collapseLongTail
-- does the same for live charts.
--
-- >>> let md = def{Charts.headers = V.fromList ["timestamp", "a", "b", "c"], Charts.dataset = V.fromList [V.fromList [Just 1, Just 5, Just 1, Just 2], V.fromList [Just 2, Nothing, Just 9, Just 1]]} :: Charts.MetricsData
-- >>> let folded = foldSeries 1 md
-- >>> (folded.headers, folded.dataset)
-- (["timestamp","b","Other (2)"],[[Just 1.0,Just 1.0,Just 7.0],[Just 2.0,Just 9.0,Just 1.0]])
-- >>> (foldSeries 3 md).headers == md.headers
-- True
foldSeries :: Int -> Charts.MetricsData -> Charts.MetricsData
foldSeries n md = case V.toList md.headers of
  x : names
    | length names > n ->
        let peak i = foldl' max 0 $ mapMaybe (join . (V.!? i)) (V.toList md.dataset)
            (kept, rest) = splitAt n $ sortOn (Down . peak . snd) (zip names [1 ..])
            restSum r = viaNonEmpty sum $ mapMaybe (\(_, i) -> join (r V.!? i)) rest
         in md
              { Charts.headers = V.fromList $ x : map fst kept <> ["Other (" <> show (length rest) <> ")"]
              , Charts.dataset = md.dataset <&> \r -> V.fromList $ join (r V.!? 0) : map (join . (r V.!?) . snd) kept <> [restSum r]
              }
  _ -> md


data WidgetAxis = WidgetAxis
  { label :: Maybe Text
  , showAxisLabel :: Maybe Bool
  , series :: Maybe [WidgetAxis]
  , showOnlyMaxLabel :: Maybe Bool
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] WidgetAxis
  deriving (FromHttpApiData) via JSONHttpApiData WidgetAxis


data TableColumn = TableColumn
  { field :: Text
  , title :: Text
  , unit :: Maybe Text
  , sortable :: Maybe Bool
  , copyable :: Maybe Bool
  , link :: Maybe Text
  , width :: Maybe Text
  , align :: Maybe Text
  , progress :: Maybe ProgressScale
  , progressVariant :: Maybe Text -- "default", "info", "error", etc.
  , columnType :: Maybe ColumnType -- default CTText
  , decimals :: Maybe Int -- Fixed decimals for a number column, so one column reads at one precision (Apdex 1.00 / 0.99)
  , hidden :: Maybe Bool -- Carried for links (@{{row.<field>}}@) but not rendered
  , description :: Maybe Text -- Header tooltip, e.g. what an abbreviation stands for
  , tooltip :: Maybe Text -- Cell tooltip; a template over @{{row.<field>}}@
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake TableColumn


data RowClickAction = RowClickAction
  { setVariable :: Maybe Text
  , value :: Maybe Text
  , navigateToTab :: Maybe Text
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake RowClickAction
  deriving (FromHttpApiData) via JSONHttpApiData RowClickAction


-- | Widget's project id as Text, or empty when absent.
projectIdText :: Widget -> Text
projectIdText w = foldMap (.toText) w._projectId


-- | The project and dashboard a saved widget lives on, as route ids.
dashboardIds :: Widget -> Maybe (Projects.ProjectId, UUIDId "dashboard")
dashboardIds w = (,) <$> w._projectId <*> (UUIDId <$> (UUID.fromText =<< w._dashboardId))


-- | HTMX fetch URL for a (already eager-prepared) widget: the project-scoped
-- /widget endpoint carrying the widget as a compressed @widgetZ@ blob.
--
-- Url-encoding the raw JSON put a table widget's multi-KB SQL on the request line;
-- past ~9.6KB nginx drops the whole HTTP/2 connection rather than just that stream, so
-- Cloudflare answered 520 for the oversized request *and* every sibling multiplexed
-- onto the same connection — which is what made dashboard charts fail in scattered,
-- retry-able subsets. Compression keeps these an order of magnitude under the limit.
widgetFetchUrl :: Widget -> Text
widgetFetchUrl w = foldMap (\pid -> urls.widgetGet pid Nothing (Just $ encodeWidgetZ $ encodeText w) Nothing Nothing Nothing []) w._projectId


-- | gzip + base64url, the wire form of a widget on a request line. 'decodeWidgetZ' inverts it.
encodeWidgetZ :: Text -> Text
encodeWidgetZ = B64.extractBase64 . B64URL.encodeBase64 . toStrict . GZip.compress . encodeUtf8


-- | Inverse of 'encodeWidgetZ'; 'Nothing' for anything that isn't a well-formed blob.
-- Forced here so a corrupt payload fails as a decode miss rather than as a lazy
-- 'GZip.decompress' bomb detonating deep inside the handler.
decodeWidgetZ :: MonadIO m => Text -> m (Maybe Text)
decodeWidgetZ z = case B64URL.decodeBase64Untyped (encodeUtf8 z) of
  Left _ -> pure Nothing
  Right bs -> liftIO $ Safe.handleAny (const $ pure Nothing) $ Just <$> evaluateWHNF (decodeUtf8 @Text $ toStrict $ GZip.decompress $ fromStrict bs)


-- Used when converting a widget json to its html representation. Eg in a query chart builder
widgetPostH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Widget -> ATAuthCtx (RespHeaders Widget)
widgetPostH pid sinceM fromM toM widget = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  addRespHeaders =<< stampPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl pid sinceM fromM toM (widget & #_projectId ?~ pid)


-- | Nothing when the URL would exceed 8000 chars (logged), since chat/email clients reject it.
widgetPngUrl :: Log :> es => Text -> Text -> Projects.ProjectId -> Widget -> Maybe Text -> Maybe Text -> Maybe Text -> Eff es (Maybe Text)
widgetPngUrl secret hostUrl pid widget since fromM toM =
  let widgetJson = encodeText widget
      compressed = encodeWidgetZ widgetJson
      sig = signWidgetUrl secret pid widgetJson
      url = hostPath hostUrl $ widgetPngPath pid compressed since fromM toM sig
   in if T.length url > 8000 then Log.logAttention "Widget PNG URL too large" (AE.object ["projectId" AE..= pid, "urlLength" AE..= T.length url]) $> Nothing else pure (Just url)


stampPngUrl :: Log :> es => Text -> Text -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Widget -> Eff es Widget
stampPngUrl secret hostUrl pid since fromM toM w = widgetPngUrl secret hostUrl pid w since fromM toM <&> \url -> w{pngUrl = url <|> w.pngUrl}


signWidgetUrl :: Text -> Projects.ProjectId -> Text -> Text
signWidgetUrl secret pid widgetJson =
  let payload = pid.toText <> ":" <> widgetJson
   in decodeUtf8 @Text $ B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) (encodeUtf8 payload :: ByteString) :: HMAC.HMAC SHA256)


-- | Proof that the server rendered this SQL for @pid@. The browser echoes it back with the
-- statement, and only an editor may run SQL that does not carry it.
--
-- >>> import Data.UUID qualified as UUID
-- >>> import Pkg.DeriveUtils (UUIDId (..))
-- >>> let pid = UUIDId UUID.nil
-- >>> sqlSignature "k" pid (Just "SELECT 1") Nothing == sqlSignature "k" pid (Just "SELECT 1") Nothing
-- True
-- >>> sqlSignature "k" pid (Just "SELECT 1") Nothing == sqlSignature "k" pid (Just "SELECT 2") Nothing
-- False
-- >>> sqlSignature "k" pid (Just "a") (Just "b") == sqlSignature "k" pid (Just "ab") Nothing
-- False
sqlSignature :: Text -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Text
sqlSignature secret pid sql rollup = signWidgetUrl secret pid $ "sql\0" <> fold sql <> "\0" <> fold rollup


-- | Constant-time check of a signature the browser echoed back.
verifySqlSignature :: Text -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Bool
verifySqlSignature secret pid sql rollup = maybe False \sig -> BA.constEq (encodeUtf8 sig :: ByteString) (encodeUtf8 (sqlSignature secret pid sql rollup) :: ByteString)


-- | Stamp 'sqlSig' on a widget and its children, for those carrying SQL.
signWidgetSql :: Text -> Projects.ProjectId -> Widget -> Widget
signWidgetSql secret pid = transformOf (#children . _Just . traverse) \w -> w{sqlSig = sqlSignature secret pid w.sql w.rollupSql <$ (w.sql <|> w.rollupSql)}


-- | Standalone timeseries panel with the shared infrastructure-page chrome:
-- no subtitle or headline value, compact legend top-right, half-width 6x4 cell.
-- Used by the host and container pages so their charts stay visually identical;
-- callers layer on only what differs (e.g. a per-unit description).
infraTimeseries :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Widget
infraTimeseries pid wid title unit query =
  def
    { id = Just wid
    , wType = WTTimeseriesLine
    , title = Just title
    , query = Just query
    , unit = Just unit
    , _projectId = Just pid
    , standalone = Just True
    , hideSubtitle = Just True
    , hideValue = Just True
    , legendPosition = Just "top-right"
    , legendSize = Just LSXs
    , layout = Just def{w = Just 6, h = Just 4}
    }


-- use either index or the xxhash as id
widget_ :: Widget -> Html ()
widget_ w' = case w.wType of
  WTAnomalies ->
    wgtCard_ $ withCardFrame False w (w._projectId <&> \pid -> ("View all", (urls.issues pid).listGet Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] [])) $ div_ [class_ "h-full overflow-auto p-3"] $ whenJust w.html toHtmlRaw
  -- rounded-3xl: its cards are rounded-2xl inside the grid's ~8px margin, so corners stay concentric.
  WTGroup -> gridItem_ $ div_ [class_ "h-full flex flex-col border border-strokeWeak rounded-3xl surface-raised overflow-hidden group/wgt"] do
    -- Header: auto height (no flex), group-header class for CSS targeting when collapsed
    div_ [class_ $ "group-header py-2 px-4 flex items-center justify-between " <> gridStackHandleClassFor w] do
      div_ [class_ "inline-flex gap-2 items-center group/h"] do
        span_ [class_ "hidden group-hover/h:inline-flex cursor-move"] $ Utils.faSprite_ "grip-dots-vertical" "regular" "w-4 h-4"
        whenJust w.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-5 h-5"
        span_ ([class_ "text-lg font-medium"] <> varTemplateAttr w.title) $ toHtml $ maybeToMonoid w.title
        descIcon_ w.description ""
      -- Collapse chevron: only for full-width groups. A hidden checkbox drives both the
      -- nested grid's visibility and the chevron rotation purely in CSS.
      when isFullWidth $ label_ [class_ "p-2 rounded hover:bg-fillWeak transition-colors cursor-pointer tap-target has-[:focus-visible]:ring-2 has-[:focus-visible]:ring-strokeBrand-strong", Aria.label_ "Toggle group"] do
        input_ [type_ "checkbox", class_ "sr-only peer wgt-collapse"]
        Utils.faSprite_ "chevron-up" "regular" "w-5 h-5 transition-transform peer-checked:rotate-180"
    -- Nested grid: flex-1 fills remaining space
    div_ (class_ "grid-stack grid-stack-preloaded nested-grid flex-1 group-has-[.wgt-collapse:checked]/wgt:hidden" : [data_ "stat-grid" (bool "" "plain" (all ((== WTStat) . (.wType)) normalizedChildren)) | not (null normalizedChildren) && all isStatWidget normalizedChildren] <> gridStackAttrs normalizedChildren) $ forM_ normalizedChildren (\wChild -> widget_ (wChild{_isNested = Just True}))
  WTTable -> wgtCard_ $ renderTable w
  WTTopList -> wgtCard_ $ renderTable w
  WTLogs -> wgtCard_ $ renderLogsWidget w
  WTServiceMap -> wgtCard_ $ withCardFrame True w (ctaAction w) $ div_ [class_ "h-full overflow-auto p-3"] $ whenJust w.html toHtmlRaw
  WTFlamegraph -> gridItem_ $ div_ [class_ "h-full "] $ div_ [class_ "p-3"] "Flamegraph widget coming soon"
  _ -> gridItem_ $ div_ [class_ " w-full h-full group/wgt "] $ renderChart w
  where
    normalizedChildren = fold $ listToMaybe (normalizeWidgetLayouts [w']) >>= (.children)
    w = (w' & #children .~ (normalizedChildren <$ w'.children)) & #id %~ (<|> (slugify <$> w'.title))
    effectiveWidth = min 12 $ max 1 $ fromMaybe defaultWidgetWidth $ w.layout >>= (.w)
    isFullWidth = effectiveWidth == 12
    effectiveHeight = case w.wType of
      WTGroup -> Just $ widgetHeightForWidth effectiveWidth w
      _ -> w.layout >>= (.h)
    (lx, ly) = (w.layout >>= (.x), w.layout >>= (.y))
    gs :: Text -> Maybe Int -> [Attribute]
    gs n = foldMap \v -> [term n (show v)]
    attrs =
      gs "gs-x" lx
        <> gs "gs-y" ly
        <> gs "gs-w" (Just effectiveWidth)
        <> gs "gs-h" effectiveHeight
        <> [ style_
               $ T.intercalate ";"
               $ catMaybes
                 [ ("--grid-preload-left:" <>) . gridPercent <$> lx
                 , ("--grid-preload-top:" <>) . gridRem <$> ly
                 , Just $ "--grid-preload-width:" <> gridPercent effectiveWidth
                 , ("--grid-preload-height:" <>) . gridRem <$> effectiveHeight
                 ]
           ]
    widgetJson = encodeText w
    autoFitAttr = memptyIfFalse (w.wType `elem` [WTAnomalies, WTGroup, WTTable, WTTopList, WTLogs, WTFlamegraph]) [data_ "mobile-autofit" ""]
    gridItem_ =
      if or w.naked
        then Relude.id
        else div_ ([class_ "grid-stack-item h-full flex-1 !overflow-visible has-[details[open]]:z-50 [.nested-grid_&]:overflow-hidden ", id_ $ maybeToMonoid w.id <> "_widgetEl", data_ "widget" widgetJson] <> attrs <> autoFitAttr) . div_ [class_ "grid-stack-item-content h-full !overflow-visible [.grid-stack_&]:h-auto"]
    wgtCard_ = gridItem_ . div_ [class_ "h-full group/wgt "]


-- | Emit GridStack's final geometry before its deferred script loads.
gridStackAttrs :: [Widget] -> [Attribute]
gridStackAttrs widgets =
  [ style_
      $ "--gs-columns:12;--gs-column-width:8.3333333333%;--gs-cell-height:5rem;"
      <> "--gs-item-margin-top:1rem;--gs-item-margin-right:0.5rem;"
      <> "--gs-item-margin-bottom:1rem;--gs-item-margin-left:0.5rem;"
      <> "height:"
      <> gridRem rows
  ]
    <> [data_ "min-width" (show minWidth) | minWidth > 0]
  where
    minWidth = foldl' max 0 [pixels * 12 `div` max 1 (fromMaybe defaultWidgetWidth layout.w) | widget <- widgets, layout <- toList widget.layout, pixels <- toList layout.minWidth]
    rows = if null widgets then 2 else layoutRows widgets


-- $setup
-- >>> import Data.Default (def)
-- >>> let testWidget kind position = (def :: Widget){wType = kind, layout = Just position}
-- >>> let coordinates widget = fmap (\position -> (position.x, position.y, position.w, position.h)) widget.layout


-- | Resolve incomplete, out-of-bounds, and overlapping layouts before HTML is
-- emitted. GridStack then hydrates the exact same collision-free coordinates.
--
-- Missing positions, oversized widths, overlaps, and row packing:
--
-- >>> let widgets = [testWidget WTList (def :: Layout){x = Just 0, y = Just 2, w = Just 20, h = Just 2}, testWidget WTList (def :: Layout){x = Just 0, y = Just 0, w = Just 6, h = Just 1}, testWidget WTList (def :: Layout){w = Just 6, h = Just 1}]
-- >>> map coordinates (normalizeWidgetLayouts widgets)
-- [Just (Just 0,Just 0,Just 6,Just 1),Just (Just 6,Just 0,Just 6,Just 1),Just (Just 0,Just 1,Just 12,Just 2)]
--
-- Saved visual order wins over YAML array order, and a clamped full-width group
-- derives its height from its children:
--
-- >>> let lower = testWidget WTList (def :: Layout){x = Just 0, y = Just 4, w = Just 6, h = Just 2}
-- >>> let golden = (testWidget WTGroup (def :: Layout){x = Just 0, y = Just 0, w = Just 20, h = Just 10}){children = Just []}
-- >>> map coordinates (normalizeWidgetLayouts [lower, golden])
-- [Just (Just 0,Just 0,Just 12,Just 2),Just (Just 0,Just 2,Just 6,Just 2)]
--
-- Nested group children are normalized recursively:
--
-- >>> let child position = testWidget WTList position
-- >>> let groupWidget = (testWidget WTGroup (def :: Layout){x = Just 0, y = Just 0, w = Just 12, h = Just 1}){children = Just [child (def :: Layout){x = Just 0, y = Just 0, w = Just 8, h = Just 1}, child (def :: Layout){x = Just 0, y = Just 0, w = Just 8, h = Just 1}]}
-- >>> fmap coordinates (listToMaybe $ normalizeWidgetLayouts [groupWidget])
-- Just (Just (Just 0,Just 0,Just 12,Just 3))
-- >>> map coordinates (fold $ listToMaybe (normalizeWidgetLayouts [groupWidget]) >>= (.children))
-- [Just (Just 0,Just 0,Just 8,Just 1),Just (Just 0,Just 1,Just 8,Just 1)]
--
-- A saved four-stat row with shrunken cards still fills its group:
--
-- >>> let stat x width height = testWidget WTStat (def :: Layout){x = Just x, y = Just 0, w = Just width, h = Just height}
-- >>> let overview = (testWidget WTGroup (def :: Layout){x = Just 0, y = Just 0, w = Just 12, h = Just 3}){children = Just [stat 0 3 2, stat 3 3 2, stat 6 2 1, stat 9 2 2]}
-- >>> map coordinates (fold $ listToMaybe (normalizeWidgetLayouts [overview]) >>= (.children))
-- [Just (Just 0,Just 0,Just 3,Just 2),Just (Just 3,Just 0,Just 3,Just 2),Just (Just 6,Just 0,Just 3,Just 2),Just (Just 9,Just 0,Just 3,Just 2)]
-- >>> let html = toStrict $ renderText $ widget_ overview
-- >>> (T.count "gs-w=\"3\"" html, T.count "gs-h=\"2\"" html)
-- (4,4)
normalizeWidgetLayouts :: [Widget] -> [Widget]
normalizeWidgetLayouts widgets =
  map snd
    $ sortOn visualPriority
    $ snd
    $ foldl' placeWidget ([], [])
    $ sortOn placementPriority
    $ zip [0 :: Int ..] widgets
  where
    placementPriority (idx, widget) = case widget.layout of
      Just Layout{x = Just x, y = Just y} -> (False, max 0 y, max 0 x, idx)
      _ -> (True, maxBound, maxBound, idx)

    visualPriority (idx, widget) =
      ( fromMaybe maxBound $ widget.layout >>= (.y)
      , fromMaybe maxBound $ widget.layout >>= (.x)
      , idx
      )

    placeWidget (occupied, acc) (idx, widget) =
      let layout = fromMaybe def widget.layout
          width = min 12 $ max 1 $ fromMaybe defaultWidgetWidth layout.w
          groupChildren = fold widget.children
          fourStatRow =
            widget.wType
              == WTGroup
              && width
              == 12
              && all isStatWidget groupChildren
              && map (\child -> (child.layout >>= (.x), child.layout >>= (.y))) groupChildren
              == [(Just x, Just 0) | x <- [0, 3, 6, 9]]
          rowHeight = max 1 $ layoutRows groupChildren
          alignedChildren =
            if fourStatRow
              then map (\child -> child{layout = Just (fromMaybe def child.layout){w = Just 3, h = Just rowHeight}}) groupChildren
              else groupChildren
          normalizedChildren = normalizeWidgetLayouts alignedChildren
          normalizedWidget = widget{children = normalizedChildren <$ widget.children}
          height = widgetHeightForWidth width normalizedWidget
          requestedX = min (12 - width) $ max 0 $ fromMaybe 0 layout.x
          requestedY = max 0 $ fromMaybe 0 layout.y
          hasPosition = isJust layout.x && isJust layout.y
          candidates =
            if hasPosition
              then [(y, requestedX) | y <- [0 .. requestedY]] <> [(y, x) | y <- [requestedY + 1 ..], x <- [0 .. 12 - width]]
              else [(y, x) | y <- [0 ..], x <- [0 .. 12 - width]]
          (placedY, placedX) = fromMaybe (requestedY, requestedX) $ find (isFree occupied width height) candidates
          rect = (placedX, placedY, width, height)
          widget' = normalizedWidget{layout = Just layout{x = Just placedX, y = Just placedY, w = Just width, h = Just height}}
       in (rect : occupied, (idx, widget') : acc)

    isFree occupied width height (y, x) = not (any (overlaps (x, y, width, height)) occupied)
    overlaps (x1, y1, w1, h1) (x2, y2, w2, h2) = x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2


defaultWidgetWidth :: Int
defaultWidgetWidth = 3


isStatWidget :: Widget -> Bool
isStatWidget w = w.wType `elem` [WTStat, WTTimeseriesStat]


layoutRows :: [Widget] -> Int
layoutRows = foldl' (\rows widget -> max rows $ fromMaybe 0 (widget.layout >>= (.y)) + fromMaybe 1 (widget.layout >>= (.h))) 0


widgetHeightForWidth :: Int -> Widget -> Int
widgetHeightForWidth width widget = case widget.wType of
  WTGroup ->
    let requiredHeight = 1 + max 1 (layoutRows $ fold widget.children)
     in if width == 12 then requiredHeight else maybe requiredHeight (max requiredHeight) (widget.layout >>= (.h))
  WTTimeseriesLine -> max 3 $ fromMaybe 1 $ widget.layout >>= (.h)
  _ -> max 1 $ fromMaybe 1 $ widget.layout >>= (.h)


gridRem :: Int -> Text
gridRem value = show (value * 5) <> "rem"


gridPercent :: Int -> Text
gridPercent value = toText (printf "%.10f%%" ((fromIntegral value :: Double) * 100 / 12) :: String)


gridStackHandleClassFor :: Widget -> Text
gridStackHandleClassFor w = bool "grid-stack-handle" "nested-grid-stack-handle" (or w._isNested)


-- | Marks text that still contains @{{var-…}}@ placeholders so the client can
-- re-render it when dashboard variables change.
varTemplateAttr :: Maybe Text -> [Attribute]
varTemplateAttr = foldMap \t -> [data_ "var-template" t | "{{var-" `T.isInfixOf` t]


-- | Hover-revealed info icon carrying the widget description tooltip.
descIcon_ :: Maybe Text -> Text -> Html ()
descIcon_ descM extraCls = whenJust descM \desc ->
  span_ [class_ "hidden group-hover/wgt:inline-flex items-center", data_ "tippy-content" desc] $ Utils.faSprite_ "circle-info" "regular" ("w-4 h-4" <> extraCls)


renderDottedTitle :: Text -> Html ()
renderDottedTitle t = case T.breakOn "." t of
  (_, "") -> span_ [class_ "truncate"] $ toHtml t
  (seg1, rest) -> do
    span_ [class_ "shrink-0 text-textWeak font-normal"] $ toHtml seg1
    case T.breakOnEnd "." (T.drop 1 rest) of
      ("", lastSeg) -> span_ [class_ "shrink-0"] $ toHtml ("." <> lastSeg)
      (mid, lastSeg) -> do
        span_ [class_ "truncate text-textWeak font-normal"] $ toHtml ("." <> mid)
        span_ [class_ "shrink-0"] $ toHtml lastSeg


-- | Pick the same representative value used by the browser for a stat tile.
-- Percentiles and rates are not additive: rendering their bucket sum during SSR
-- and replacing it with a mean after the first refresh made @5.9K ms@ flash into
-- @6.6s@ for the same latency tile.
--
-- >>> let s = Charts.MetricsStats 50 200 1200 10 120 50 200 (Just 70)
-- >>> statScalar SBMean (Just 0) (Just 600000) s
-- Just 120.0
-- >>> statScalar SBRate (Just 0) (Just 600000) s
-- Just 120.0
-- >>> statScalar SBRate Nothing Nothing s
-- Nothing
-- >>> statScalar SBLast Nothing Nothing s
-- Just 70.0
statScalar :: SummarizeBy -> Maybe Int -> Maybe Int -> Charts.MetricsStats -> Maybe Double
statScalar summarize fromM toM stats
  | stats.count < 1 = Nothing
  | otherwise = case summarize of
      SBRate -> do
        rangeStart <- fromM
        rangeEnd <- toM
        let minutes = fromIntegral (rangeEnd - rangeStart) / 60000
        if minutes > 0 then Just (stats.sum / minutes) else Nothing
      SBMean -> Just stats.mean
      SBMax -> Just stats.max
      SBMin -> Just stats.min
      SBCount -> Just $ fromIntegral stats.count
      SBSum -> Just stats.sum
      SBLast -> stats.last


-- | Format a stat value by the meaning of its declared unit. This mirrors
-- @formatStatValue@ in @web-components/src/stat-value.ts@: duration units choose
-- a readable time scale, byte units use binary multiples, and only ordinary
-- counts receive decimal K/M/B suffixes.
--
-- >>> formatStatValue Nothing 5900 "ms"
-- "5.9s"
-- >>> formatStatValue Nothing 45 "ms"
-- "45.0ms"
-- >>> formatStatValue Nothing 1500 ""
-- "1.5K"
-- >>> formatStatValue Nothing 2048 "By"
-- "2 KiB"
--
-- A non-zero rate below the display precision is shown as such, never as 0:
-- >>> (formatStatValue Nothing 0.0012 "%", formatStatValue Nothing 0 "%")
-- ("<0.01 %","0 %")
--
-- A count of one names its unit in the singular:
-- >>> map (\v -> formatStatValue Nothing v "systems") [1, 2]
-- ["1 system","2 systems"]
--
-- A fixed precision keeps a bounded index honest near its ceiling (Apdex 0.991 is not "1"):
-- >>> map (\v -> formatStatValue (Just 2) v "/ 1") [0.991, 1]
-- ["0.99 / 1","1.00 / 1"]
formatStatValue :: Maybe Int -> Double -> Text -> Text
formatStatValue decimals value unit
  | isNaN value || isInfinite value = "N/A"
  | Just factor <- unitNanos unit = formatDuration $ value * factor
  | isByteUnit unit = formatBytes value
  | Just d <- decimals = showFFloat' d value <> displayUnit unit
  | value > 0, value < 0.01 = "<0.01" <> displayUnit unit
  | value == 1, T.length unit > 2, T.all isAlpha unit, Just unitOne <- T.stripSuffix "s" unit = "1 " <> unitOne
  | otherwise = formatNumber value <> displayUnit unit
  where
    durationSteps :: [(Double, Text)]
    durationSteps = [(3.6e12, "h"), (6e10, "m"), (1e9, "s"), (1e6, "ms"), (1e3, "μs")]
    formatDuration ns = case find ((<= ns) . fst) durationSteps of
      Just (scale, suffix) -> showFFloat' 1 (ns / scale) <> suffix
      Nothing -> show (round ns :: Integer) <> "ns"

    formatNumber n
      | n >= 1e9 = showFFloat' 1 (n / 1e9) <> "B"
      | n >= 1e6 = showFFloat' 1 (n / 1e6) <> "M"
      | n >= 1e3 = showFFloat' 1 (n / 1e3) <> "K"
      | n == fromInteger (truncate n) = show (truncate n :: Integer)
      | n >= 100 = show (round n :: Integer)
      | n >= 10 = trimFixed 1 n
      | otherwise = trimFixed 2 n

    trimFixed places = T.dropWhileEnd (== '.') . T.dropWhileEnd (== '0') . showFFloat' places

    displayUnit u = memptyIfFalse (u `notElem` ["", "1", "{}"]) (" " <> u)


isByteUnit :: Text -> Bool
isByteUnit = (`elem` ["By", "by", "bytes", "byte", "B"])


-- | The badge holding a widget's representative scalar for the current range —
-- the sum for additive counts, a mean/rate/min/max per @summarizeBy@ (see
-- @statScalar@ in widgets.ts).
--
-- It starts hidden and is filled by @setStatValue@ on every chart-data fetch,
-- which keys off the @\<widget id\>Value@ id alone. @renderWidgetHeader@ draws it
-- for widgets that own their header; it is exported so a caller supplying its own
-- header around a @naked@ chart shows the same number the widget would have,
-- rather than computing a second total that could disagree with the chart.
widgetValueSlot_ :: Text -> Maybe Text -> Html ()
widgetValueSlot_ = valueSlot_ "bg-fillWeak text-sm font-medium px-2 py-1 rounded-lg leading-none text-textWeak whitespace-nowrap max-md:hidden"


-- | 'widgetValueSlot_' styled as a headline rather than as a badge beside the title.
widgetValueSlotAs_ :: Text -> Maybe Text -> Html ()
widgetValueSlotAs_ = valueSlot_ "text-2xl font-semibold text-textStrong tabular-nums leading-none"


valueSlot_ :: Text -> Text -> Maybe Text -> Html ()
valueSlot_ cls wid valueM = span_ [class_ $ cls <> bool " hidden" "" (isJust valueM), id_ $ wid <> "Value"] $ whenJust valueM toHtml


renderWidgetHeader :: Widget -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Text, Text) -> Html ()
renderWidgetHeader widget valueM subValueM expandBtnFn ctaM = div_ [class_ $ "min-h-8 px-1 leading-none flex justify-between items-center " <> bool "grid-stack-handle" "" (or widget.standalone), id_ $ wId <> "_header"] do
  when (or widget._centerTitle) $ div_ ""
  div_ [class_ "inline-flex gap-3 items-center group/h min-w-0"] do
    span_ [class_ "text-sm text-textStrong font-semibold flex items-center gap-1 min-w-0"] do
      unless (or widget.standalone) $ span_ [class_ "hidden group-hover/h:inline-flex"] $ Utils.faSprite_ "grip-dots-vertical" "regular" "w-4 h-4"
      whenJust widget.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-4 h-4"
      span_ ([class_ "flex min-w-0 overflow-hidden", title_ $ maybeToMonoid widget.title] <> varTemplateAttr widget.title) $ renderDottedTitle $ maybeToMonoid widget.title
      descIcon_ widget.description ""
    widgetValueSlot_ wId (bool valueM Nothing (or widget.hideValue))
    span_ ([class_ $ "text-textWeak widget-subtitle text-sm max-md:hidden " <> bool "" "hidden" (or widget.hideSubtitle), id_ $ wId <> "Subtitle"] <> varTemplateAttr subValueM) $ toHtml $ maybeToMonoid subValueM
    -- Add hidden loader with specific ID that can be toggled from JS
    span_ [class_ "hidden", id_ $ wId <> "_loader"] Utils.reloadSpinner_
  div_ [class_ "text-iconNeutral flex items-center gap-0.5"] do
    -- Alert status indicator (visible on hover, always visible when alerting/warning)
    when (isJust widget.alertId)
      $ let (iconColor, iconType, tooltip, visibilityClass) = case widget.alertStatus of
              Just Monitors.MSAlerting -> ("text-fillError-strong", "bell-exclamation", "Monitor triggered", "")
              Just Monitors.MSWarning -> ("text-fillWarning-strong", "bell", "Warning threshold exceeded", "")
              Just Monitors.MSNormal -> configured
              Nothing -> configured
         in span_
              [ class_ $ "p-1 transition-opacity " <> visibilityClass
              , data_ "tippy-content" tooltip
              , id_ $ wId <> "_alert_indicator"
              ]
              $ Utils.faSprite_ iconType "regular" ("w-3.5 h-3.5 " <> iconColor)

    -- Brand colour says it's a link; the underline waits for hover so it doesn't outweigh the title.
    whenJust ctaM \(ctaTitle, uri) -> a_ [class_ "tap-target px-1 text-xs font-medium text-textBrand underline-offset-2 hover:underline", href_ uri] $ toHtml ctaTitle
    whenJust ((,) "SQL" <$> Utils.nonEmptyT widget.sql <|> (,) "KQL" <$> Utils.nonEmptyT widget.query) \(label, query) -> do
      let popId = wId <> "-query-preview"
      div_
        [ class_ "inline-block"
        , [__|on mouseleave or focusout wait 150ms then if not (me.matches(':hover') or me.matches(':focus-within')) call (the first <[popover]/> in me).hidePopover() end|]
        ]
        do
          button_
            ( [ type_ "button"
              , class_ "p-2 cursor-pointer hover:bg-fillWeak rounded-lg tap-target opacity-0 group-hover/wgt:opacity-100 focus-visible:opacity-100 touch:opacity-100 transition-opacity"
              , Aria.label_ "View widget query"
              , [__|on mouseenter or focus or click call (the next <[popover]/>).showPopover() then if event.type is 'click' halt end|]
              ]
                <> Utils.popoverTrigger_ popId
            )
            $ Utils.faSprite_ "code" "regular" "w-3.5 h-3.5"
          div_
            ( [ class_ "dropdown bg-bgRaised text-textStrong border border-strokeWeak rounded-xl shadow-lg p-0 w-[min(36rem,calc(100vw-2rem))] max-h-[min(32rem,70vh)] overflow-auto text-left"
              , role_ "region"
              , Aria.label_ "Widget query"
              ]
                <> Utils.popoverPanel_ popId
            )
            do
              div_ [class_ "px-4 py-2 border-b border-strokeWeak text-xs font-medium text-textWeak"] $ toHtml label
              termWith "query-preview" [term "language" $ T.toLower label, class_ "block px-2"]
                $ pre_ [class_ "p-2 font-mono text-xs whitespace-pre-wrap break-words leading-relaxed"]
                $ toHtml query

    whenJust expandBtnFn \url ->
      button_
        ( Utils.drawerLoadAttrs_ url
            <> foldMap (pure . hxPushUrl_) widget.expandPushUrl
            <> [ class_ "p-2 cursor-pointer tap-target"
               , Aria.label_ "Expand widget"
               , data_ "tippy-content" "Expand widget"
               ]
        )
        $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    whenJust (dashboardIds widget) \(p, d) ->
      button_
        ( [ class_ "p-2 cursor-pointer opacity-0 group-hover/wgt:opacity-100 focus-visible:opacity-100 touch:opacity-100 tap-target transition-opacity"
          , Aria.label_ "Expand widget"
          , data_ "tippy-content" "Expand widget"
          , data_ "expand-btn" wId
          , -- Only the URL stamp is genuinely client-side; drawerLoadAttrs_ owns the
            -- fetch, the target, the loading skeleton and opening the drawer (and
            -- installs the focus trap the old hand-rolled chain skipped). The stamp
            -- has to stay hyperscript because it appends to whatever query string the
            -- page already has, which the server can't know. Reloading that URL now
            -- reopens the drawer server-side — Pages.Dashboards renders the trigger.
            term "_" [text|on click js { const url = new URL(window.location); url.searchParams.set('expand', '${wId}'); history.replaceState({}, '', url); } end|]
          , term "hx-disabled-elt" "this"
          ]
            <> Utils.drawerLoadAttrs_ (urls.dashboardWidgetExpandGet p d wId Nothing)
        )
        $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    let widgetMenuPop = wId <> "-widget-menu"
    div_ [class_ "inline-block"] do
      button_ ([type_ "button", class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg tap-target", Aria.label_ "Widget menu", data_ "tippy-content" "Widget Menu"] <> Utils.popoverTrigger_ widgetMenuPop)
        $ Utils.faSprite_ "ellipsis" "regular" "w-4 h-4"
      ul_ ([class_ "dropdown text-textStrong dropdown-end menu menu-md bg-bgRaised rounded-box p-2 w-52 shadow-lg leading-none border border-strokeWeak"] <> Utils.popoverPanel_ widgetMenuPop) do
        -- A widget on a dashboard is copied from it by id. One that is not — the log
        -- explorer's chart — has no server-side row to copy, so its own JSON goes with
        -- it and the picker PUTs that as a new widget.
        let onDashboard = isJust widget._dashboardId
            dashId = maybeToMonoid widget._dashboardId
            pid = projectIdText widget
            dashboardScope = foldMap ("&dashboard_id=" <>) widget._dashboardId
        menuItem_
          (bool "Add this widget to a dashboard" "Copy this widget to another dashboard" onDashboard)
          [ id_ $ wId <> "_copy_link"
          , term
              "_"
              [text|
              on click
              set widgetEl to the closest <[data-widget]/>
              then set #dashboards-modal-widget-id.value to "${wId}"
              then set #dashboards-modal-source-dashboard-id.value to "${dashId}"
              then set #dashboards-modal-widget-json.value to (widgetEl.dataset.widget or '')
              then set #dashboards-modal.checked to true
              then trigger loadDashboards on #dashboards-modal-content
              then call (the closest <[popover]/>).hidePopover()
            |]
          ]
          (bool "Add to dashboard" "Copy to dashboard" onDashboard)
        menuItem_
          "Copy generated SQL to clipboard"
          [ term
              "_"
              [text|
              on click
              set widgetEl to the closest <[data-widget]/>
              set widgetData to JSON.parse(widgetEl.dataset.widget)
              set txt to widgetData.sql
              if not txt and widgetData.query then
                fetch ('/p/${pid}/widget/sql-text?query=' + encodeURIComponent(widgetData.query) + '${dashboardScope}') as text
                set txt to it
              end
              if not txt then set txt to 'No SQL available' end
              if 'clipboard' in window.navigator then
                call navigator.clipboard.writeText(txt)
                send successToast(value:['SQL copied to clipboard']) to <body/>
              end
            |]
          ]
          "Copy SQL"
        menuItem_
          "Copy KQL query to clipboard"
          [copyToClipboardAttr_ "JSON.parse(this.closest('[data-widget]').dataset.widget).query || 'No KQL available'" "KQL copied to clipboard"]
          "Copy KQL"
        whenJust widget.pngUrl \url ->
          menuItem_
            "Download widget as PNG image"
            [href_ url, download_ $ maybeToMonoid widget.title <> ".png", target_ "_blank"]
            "Download PNG"

        -- A read-only dashboard (data-read-only on the page) keeps copying out, never edits in place.
        when onDashboard do
          li_ [class_ "in-[[data-read-only]]:hidden"]
            $ a_
              ( [class_ "p-2 w-full text-left block cursor-pointer", data_ "tippy-content" "Create a copy of this widget"]
                  <> [hxPost_ (urls.dashboardDuplicateWidget p d wId Nothing) | (p, d) <- toList (dashboardIds widget)]
                  <> [ hxTarget_ "closest .grid-stack"
                     , hxSwap_ "beforeend"
                     , -- htmx appends the rendered widget and processes it (scripts, hx-*, and
                       -- hyperscript, which hooks htmx:load itself); only the grid adoption is
                       -- left to do. No size is passed: makeWidget reads the gs-w/gs-h the
                       -- widget already renders, and addWidget's options are dropped since v11.
                       -- after:swap, not after:request — under htmx 4 afterRequest fires before
                       -- the swap, when the new element is not yet in the DOM.
                       [__|on click call (the closest <[popover]/>).hidePopover()
                    on htmx:after:swap
                       set g to the closest <.grid-stack/> then call g.gridstack.makeWidget(g.lastElementChild)
                 |]
                     ]
              )
              "Duplicate widget"
          li_ [class_ "in-[[data-read-only]]:hidden"]
            $ button_
              [ class_ "p-2 w-full text-left text-textError cursor-pointer"
              , data_ "tippy-content" "Permanently delete this widget"
              , onpointerdown_
                  [text|
                  if (confirm('Are you sure you want to delete this widget? This action cannot be undone.')) {
                    document.getElementById('${wId}_widgetEl')?.dispatchEvent(
                      new CustomEvent('widget-remove-requested', { bubbles: true, detail: { widgetId: '${wId}' } }));
                  }
                  return false;
                |]
              ]
              "Delete widget"
  where
    wId = maybeToMonoid widget.id
    configured = ("text-iconNeutral", "bell", "Monitor configured", "opacity-0 group-hover/wgt:opacity-100 touch:opacity-50")
    menuItem_ :: Text -> [Attribute] -> Html () -> Html ()
    menuItem_ tip extraAttrs = li_ . a_ ([class_ "p-2 w-full text-left block cursor-pointer", data_ "tippy-content" tip] <> extraAttrs)


-- | Shared card-shell for renderLogsWidget / renderTable:
-- outer flex-col wrapper, optional widget header, flex-1 fill, surface-raised
-- card with @id="<wid>_bordered"@.  When @flexCol@ is True the inner card adds
-- @flex flex-col@. Pass @extraAction@ to attach an action button to the header.
withCardFrame :: Bool -> Widget -> Maybe (Text, Text) -> Html () -> Html ()
withCardFrame flexCol widget extraAction body = do
  let naked = or widget.naked
  div_ [class_ "gap-0.5 flex flex-col h-full"] do
    unless naked $ renderWidgetHeader widget Nothing Nothing Nothing extraAction
    div_ [class_ "flex-1 flex min-h-0"]
      $ div_
        [ class_ $ "h-full w-full " <> bool "" "flex flex-col " flexCol <> bool "surface-raised rounded-2xl" "" naked
        , id_ $ maybeToMonoid widget.id <> "_bordered"
        ]
        body


-- | A template's card link, with its project placeholder resolved.
ctaAction :: Widget -> Maybe (Text, Text)
ctaAction w = (\c -> (c.title, replacePlaceholders (one ("project_id", projectIdText w)) c.url)) <$> w.cta


renderLogsWidget :: Widget -> Html ()
renderLogsWidget widget = do
  let wId = maybeToMonoid widget.id
      pid = projectIdText widget
      fetchUrl = foldMap (\p -> explorerUrl p LogQueries.Data def{LogQueries.query = widget.query, LogQueries.layout = Just "1"}) widget._projectId
      action = widget._projectId <&> \p -> ("Open in Explorer", (urls.logExplorer p).logExplorerGet widget.query Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  withCardFrame False widget action
    $ termRaw
      "log-list"
      [ id_ wId
      , class_ "w-full flex flex-col h-full min-w-0"
      , term "projectId" pid
      , term "initialFetchUrl" fetchUrl
      ]
      ("" :: Text)


-- | Sorting is opt-in; handwritten SQL must place the ordering slot before LIMIT.
sortableColumn :: Widget -> TableColumn -> Bool
sortableColumn widget col = maybe (col.sortable /= Just False) (\sql -> col.sortable == Just True && isJust widget.defaultSort && "{{table_sort}}" `T.isInfixOf` sql) widget.sql


data SortDirection = SortAscending | SortDescending


data TableSort = TableSort Text SortDirection


-- | Resolve only a declared field; quote it as an identifier, never as SQL syntax.
--
-- >>> tableQuery (def & #query ?~ "foo") (Just "-unknown")
-- (Just "foo",Nothing)
-- >>> tableQuery (def & #sql ?~ "SELECT 1 ORDER BY {{table_sort}}") Nothing
-- (Nothing,Nothing)
tableQuery :: Widget -> Maybe Text -> (Maybe Text, Maybe Text)
tableQuery widget sortParam = (query, sql)
  where
    renderDirection SortAscending = "ASC"
    renderDirection SortDescending = "DESC"
    renderTableSort (TableSort name direction) = "\"" <> T.replace "\"" "\"\"" name <> "\" " <> renderDirection direction <> " NULLS LAST"
    renderKqlSort (TableSort name direction) = name <> " " <> T.toLower (renderDirection direction)
    selected = do
      raw <- sortParam
      (sign, name) <- T.uncons raw
      guard (sign == '+' || sign == '-')
      col <- find (\c -> c.field == name && sortableColumn widget c) (fold widget.columns)
      pure $ TableSort col.field $ if sign == '-' then SortDescending else SortAscending
    order = renderTableSort <$> selected <|> ((.unSqlOrder) <$> widget.defaultSort)
    sql =
      widget.sql >>= \rawSql ->
        if "{{table_sort}}" `T.isInfixOf` rawSql
          then (\resolvedOrder -> T.replace "{{table_sort}}" resolvedOrder rawSql) <$> order
          else Just rawSql
    query = case (widget.sql, selected) of
      (Nothing, Just tableSort) -> Just $ fromMaybe "" widget.query <> " | sort by " <> renderKqlSort tableSort
      _ -> widget.query


sortableTableHead_ :: Widget -> [TableColumn] -> Maybe Text -> Html ()
sortableTableHead_ widget columns selected =
  thead_ [class_ "sticky top-0 z-10"]
    $ tr_ []
    $ forM_ (filter (not . or . (.hidden)) columns) \col -> do
      let (direction, ariaSort, icon)
            | selected == Just ("+" <> col.field) = ("asc", "ascending", "arrow-up")
            | selected == Just ("-" <> col.field) = ("desc", "descending", "arrow-down")
            | otherwise = ("none", "none", "sort") :: (Text, Text, Text)
          sortable = sortableColumn widget col
          -- A dotted underline marks a header that explains itself on hover.
          title = span_ ([class_ "underline decoration-dotted decoration-strokeStrong underline-offset-4" | isJust col.description] <> [class_ " text-textStrong" | direction /= "none"]) $ toHtml col.title
          -- A numeric header sits over its right-aligned values, its sort glyph leading it.
          headerRow = div_ [class_ $ "flex items-center gap-1 " <> bool "" "flex-row-reverse" (alignsRight col)]
      th_ ([class_ $ "bg-bgAlternate sticky top-0 border-b border-strokeWeak has-[button:hover]:bg-bgSunken has-[button:focus-visible]:bg-bgSunken " <> bool "text-left" "text-right" (alignsRight col), term "aria-sort" ariaSort] <> foldMap (\d -> [term "data-tippy-content" d, term "aria-description" d]) col.description)
        $ if sortable
          then button_
            [ type_ "button"
            , -- Stretched over the whole cell, so the padding is clickable too.
              class_ "w-full cursor-pointer group after:absolute after:inset-0"
            , data_ "sort-field" col.field
            , data_ "sort-direction" direction
            , [__|on click
                  set fetcher to the closest <[data-table-fetch]/>
                  if not fetcher halt end
                  set sort to '+' + my @data-sort-field
                  if my @data-sort-direction is 'asc' set sort to '-' + my @data-sort-field end
                  set fetcher.dataset.tableSort to sort
                  trigger 'table-sort' on fetcher
                |]
            ]
            $ headerRow do
              title
              -- Every column offering ↕ at once was noise; it shows where the pointer or focus is.
              Utils.faSprite_ icon "regular" $ "size-3 shrink-0 " <> bool "text-iconBrand" "text-iconNeutral opacity-0 group-hover:opacity-100 group-focus-visible:opacity-100" (direction == "none")
          else headerRow title


renderTable :: Widget -> Html ()
renderTable widget = do
  let tableId = maybeToMonoid widget.id
      eagerWidget = widget & #eager ?~ True & #pngUrl .~ Nothing & #html .~ Nothing & #dataset .~ Nothing
  withCardFrame True widget (ctaAction widget)
    $ div_
      [ -- No top padding: the sticky header pins to the scroll edge, so rows can't show above it.
        class_ "dashboard-table-wrap h-full overflow-auto rounded-[inherit] pb-2"
      , hxGet_ $ widgetFetchUrl eagerWidget
      , hxTrigger_ $ bool "intersect once, update-query from:window, table-sort" "update-query from:window, table-sort" (isJust widget.html) <> memptyIfFalse (or widget.searchable) ", input delay:250ms, search"
      , hxTarget_ $ "#" <> tableId
      , hxSelect_ $ "#" <> tableId
      , hxSwap_ "outerHTML"
      , term "hx-sync" "this:replace"
      , term "hx-vals" "js:{...Object.fromEntries(new URLSearchParams(location.search)), ...JSON.parse(this.closest('[data-constants]')?.dataset.constants || '{}'), 'table-sort': this.dataset.tableSort || '', 'table-search': this.querySelector('input[type=search]')?.value || ''}"
      , data_ "table-fetch" ""
      ]
    $ do
      when (or widget.searchable) $ searchInput_ "" ("Search " <> fromMaybe "rows" widget.title) []
      case widget.html of
        Just html -> toHtmlRaw html
        Nothing -> div_ [class_ "flex h-full min-h-32 items-center justify-center", id_ tableId] $ loadingIndicator_ LdSM LdSpinner


-- | Render stat widget content with HTMX lazy loading support
-- Always includes HTMX attributes so widget can refresh on update-query events
renderStatContent :: Widget -> Maybe Text -> Html ()
renderStatContent widget valueM = do
  let chartId = maybeToMonoid widget.id
      statContentId = chartId <> "_stat"
      hasData = or widget.eager || isJust (widget.dataset >>= (.value))
  div_
    [ id_ statContentId
    , -- A row of plain stats has no sparkline baseline to share, so the value centres in its card.
      class_ $ "stat-widget px-3 flex flex-col [[data-stat-grid=plain]_&]:my-auto " <> bool "py-3 " "py-2 " (or widget._isNested)
    , hxGet_ $ widgetFetchUrl (widget & #eager ?~ True)
    , hxTrigger_ $ bool "intersect once, update-query from:window" "update-query from:window" hasData
    , hxTarget_ $ "#" <> statContentId
    , hxSelect_ $ "#" <> statContentId
    , hxSwap_ "outerHTML"
    , term "hx-vals" "js:{...Object.fromEntries(new URLSearchParams(location.search)), ...JSON.parse(this.closest('[data-constants]')?.dataset.constants || '{}')}"
    ]
    $ div_ [class_ "flex flex-col gap-1"] do
      strong_ [class_ "text-textStrong text-lg font-semibold tabular-nums", id_ $ chartId <> "Value"]
        $ if hasData then whenJust valueM toHtml else span_ [class_ "skeleton-shimmer inline-block h-[0.9em] w-[4em] rounded-md align-middle", role_ "status", Aria.label_ "Loading"] ""
      div_ [class_ "stat-label inline-flex gap-1 items-center text-xs"] do
        whenJust widget.icon \icon -> Utils.faSprite_ icon "regular" "stat-icon w-4 h-4 text-iconBrand"
        toHtml $ maybeToMonoid widget.title
        descIcon_ widget.description " text-iconNeutral"


renderChart :: Widget -> Html ()
renderChart widget = do
  let unit = fromMaybe "" widget.unit
      -- Summing latencies or percentages ("5.3s" of p99s) means nothing; unless asked, those headline their peak.
      summarize = fromMaybe (bool SBSum SBMax (isJust (unitNanos unit) || unit == "%")) widget.summarizeBy
      summed = summarize == SBSum
      rateM = guard summed *> widget.dataset >>= (.rowsPerMin) <&> \r -> Utils.prettyPrintCount (round r) <> "/min"
      chartId = maybeToMonoid widget.id
      isTimeseriesStat = widget.wType == WTTimeseriesStat
      -- A plain stat's value is its query's one result, not a sum over bins: always shown.
      fallback = guard (widget.wType == WTStat || summed) *> widget.dataset >>= (.value) <&> \value -> formatStatValue widget.decimals value unit
      valueM =
        ( do
            guard isTimeseriesStat
            dataset <- widget.dataset
            stats <- dataset.stats
            value <- statScalar summarize dataset.from dataset.to stats
            let prefix = summarizeByPrefix summarize
            pure $ prefix <> memptyIfFalse (not $ T.null prefix) " " <> formatStatValue widget.decimals value unit
        )
          <|> fallback
      chartConfig =
        AE.object
          [ "chartId" AE..= chartId
          , "echartOpt" AE..= encodeText (widgetToECharts widget)
          , "chartType" AE..= mapWidgetTypeToChartType widget.wType
          , "widgetType" AE..= widget.wType
          , "query" AE..= fst (chartQuery widget)
          , "querySQL" AE..= maybeToMonoid widget.sql
          , "rollupSQL" AE..= widget.rollupSql
          , "sqlSig" AE..= widget.sqlSig
          , "rollupFrom" AE..= widget.rollupFrom
          , "dbSource" AE..= widget.dbSource
          , "theme" AE..= fromMaybe "default" widget.theme
          , "yAxisLabel" AE..= fromMaybe unit (widget.yAxis >>= (.label))
          , "pid" AE..= (widget._projectId <&> (.toText))
          , "summarizeBy" AE..= summarize
          , "summarizeByPrefix" AE..= summarizeByPrefix summarize
          , "legendPosition" AE..= fromMaybe "bottom" widget.legendPosition
          , "unit" AE..= unit
          , "decimals" AE..= widget.decimals
          , "alertThreshold" AE..= widget.alertThreshold
          , "seriesIntent" AE..= widget.seriesIntent
          , "warningThreshold" AE..= widget.warningThreshold
          , "hideValue" AE..= or widget.hideValue
          , "timeFrom" AE..= widget.timeFrom
          , "timeTo" AE..= widget.timeTo
          , "highlightFrom" AE..= widget.highlightFrom
          , "highlightTo" AE..= widget.highlightTo
          , "markers" AE..= widget.markers
          , "dashboardId" AE..= widget._dashboardId
          ]
      isStat = isStatWidget widget
  div_ [class_ "gap-0.5 flex flex-col h-full justify-end"] do
    unless (or widget.naked || isStat)
      $ renderWidgetHeader widget valueM rateM widget.expandBtnFn Nothing
    div_ [class_ $ "flex-1 flex min-h-0 " <> bool "" (gridStackHandleClassFor widget) isStat] do
      div_
        [ class_
            $ "h-full w-full flex flex-col "
            <> case widget.wType of
              -- Top-aligned like the sparkline stats, so a row of mixed stat tiles shares a baseline.
              WTStat -> "justify-start "
              WTTimeseriesStat -> "justify-start "
              _ -> "justify-end "
            <> bool "min-h-0 " "" isStat
            <> if or widget.naked then "" else "surface-raised rounded-2xl relative"
        , id_ $ chartId <> "_bordered"
        ]
        do
          -- Failure banner: one line at the top of the card, above the content,
          -- rather than a centred overlay printed over the chart it describes.
          -- widgets.ts fills the message and unhides it. @data-stale@ (data still shown, refreshes
          -- failing) is a warning, not a failure; on a stat tile it is a corner chip, because the
          -- full-width banner pushed the value down and the sparkline out of the card.
          div_
            [ id_ $ chartId <> "_error"
            , class_ $ "group/err hidden shrink-0 rounded-t-2xl border-b border-strokeError-weak bg-fillError-weak px-3 py-1.5 data-stale:border-strokeWarning-weak " <> bool "data-stale:bg-fillWarning-weak" "data-stale:absolute data-stale:right-2 data-stale:top-2 data-stale:z-10 data-stale:rounded-lg data-stale:border data-stale:bg-bgRaised data-stale:px-1.5 data-stale:py-0.5 data-stale:shadow-sm" isStat
            ]
            $ div_ [class_ "flex items-center gap-1.5 text-xs text-textError group-data-stale/err:text-textWarning"] do
              Utils.faSprite_ "circle-exclamation" "solid" "w-3 h-3 shrink-0"
              span_ [class_ $ "min-w-0 break-words " <> bool "group-data-stale/err:truncate" "group-data-stale/err:sr-only" isStat, id_ $ chartId <> "_errorMsg", role_ "status"] ""
              button_ [type_ "button", id_ $ chartId <> "_retry", hidden_ "", class_ "shrink-0 underline px-2 disabled:opacity-50"] "Retry"
          whenJust ((,) <$> widget.groupByOptions <*> widget.groupByUrl) \(options, url) ->
            let selectedLabel = fromMaybe "All values" $ guarded (/= "all") =<< widget.groupBySelected
             in div_ [class_ "flex shrink-0 justify-end px-3 pt-3"]
                  $ details_ [class_ "dropdown dropdown-end relative max-w-[calc(100%-1.5rem)]"] do
                    summary_ [class_ "btn btn-xs min-w-0 cursor-pointer justify-between gap-1 border-strokeWeak bg-bgRaised px-2 text-left text-textWeak opacity-100 hover:bg-fillWeak", data_ "tippy-content" "Group by"] do
                      span_ [class_ "shrink-0 text-textDisabled"] "Group by"
                      span_ [class_ "min-w-0 truncate text-left text-textStrong"] $ toHtml selectedLabel
                      Utils.faSprite_ "chevron-down" "regular" "w-3 shrink-0"
                    ul_ [class_ "dropdown-content absolute right-0 top-full z-50 mt-1 flex max-h-72 max-w-[calc(100vw-2rem)] flex-col overflow-y-auto rounded-lg border border-strokeWeak bg-bgRaised p-1 opacity-100 shadow-lg"] do
                      let item value label =
                            li_
                              $ button_
                                [ class_ $ "w-full cursor-pointer whitespace-nowrap rounded px-2 py-1.5 text-left text-xs hover:bg-fillWeak " <> bool "" "bg-fillWeak text-textStrong" (widget.groupBySelected == Just value)
                                , hxGet_ url
                                , name_ "label"
                                , value_ value
                                , hxTarget_ $ fromMaybe "" widget.groupByTarget
                                , hxSwap_ "outerHTML settle:0"
                                ]
                              $ toHtml label
                      item "all" "All values"
                      forM_ options \label -> item label label
          when isStat $ renderStatContent widget valueM
          unless (widget.wType == WTStat) $ div_ [data_ "chart-config" $ encodeText chartConfig, class_ $ "relative h-0 max-h-full overflow-hidden w-full flex-1 min-h-0" <> if isStat then "" else " p-3"] do
            div_ [class_ "chart-render-slot h-full min-h-full w-full", id_ chartId, data_ "chart-widget" "", term "hx-morph-skip" ""] ""
            -- A stat tile's value already says "0"; the two-line overlay only clips inside it.
            unless isStat
              $ div_
                [ id_ $ chartId <> "_empty"
                , class_ "chart-no-data hidden absolute inset-3 z-10 flex items-center justify-center bg-bgRaised px-4 text-center"
                , role_ "status"
                , Aria.live_ "polite"
                , Aria.atomic_ "true"
                ]
              $ div_ [class_ "max-w-sm"] do
                -- An error chart with no rows is good news, not missing data.
                let (headline, hint) = bool ("No data in this time range", "Try a wider time range or adjust the filters.") ("No errors in this time range", "Nothing failed in this window.") (widget.seriesIntent == Just "error")
                strong_ [class_ "text-sm font-semibold text-textStrong"] headline
                p_ [class_ "mt-1 text-xs leading-5 text-textWeak"] hint


-----------------------------------------------------------------------------
-- Echarts Logic
-----------------------------------------------------------------------------

-- | Whether a free-text unit word ("rows", "requests") is spelled out after the number.
-- The y axis drops it: repeating it down every tick — @3.0K rows@ over @0 rows@ — widens the
-- axis gutter by a word the widget's title already carries, and on a log-explorer-sized chart
-- that gutter is plotting area. Duration and byte units are unaffected either way, because
-- there the suffix is the number's own magnitude (@10.0m@, @1 GiB@), not a label.
data UnitWord = KeepUnitWord | DropUnitWord
  deriving stock (Eq)


-- | JS expression formatting a numeric @value@ for a widget's unit: duration
-- units convert+format, everything else uses formatNumber. Shared by the
-- tooltip valueFormatter and the yAxis axisLabel formatter.
--
-- >>> unitValueExprJS DropUnitWord (Just "rows")
-- "formatNumber(value)"
-- >>> unitValueExprJS KeepUnitWord (Just "rows")
-- "formatNumber(value) + \" rows\""
--
-- A duration formats identically on both, so an axis keeps reading @10.0m@ / @0ns@:
--
-- >>> unitValueExprJS DropUnitWord (Just "ms") == unitValueExprJS KeepUnitWord (Just "ms")
-- True
unitValueExprJS :: UnitWord -> Maybe Text -> Text
unitValueExprJS unitWord unitM
  | Just u <- durationUnit = "formatDuration(convertToNanoseconds(value, '" <> u <> "'))"
  | isBytes = "formatBytes(value)"
  | otherwise = "formatNumber(value)" <> foldMap (\u -> " + " <> encodeText (" " <> u)) unitWordM
  where
    unitWordM = case unitWord of
      KeepUnitWord -> unitM
      DropUnitWord -> Nothing
    durationUnit = guarded (`elem` ["ns", "μs", "us", "ms", "s", "m", "h"]) =<< unitM
    -- Bytes are their own case because formatNumber's suffixes are decimal magnitudes: a memory
    -- chart's axis read "1.0B" for a gigabyte -- B for "billion" -- directly above a table
    -- reading "1 GiB". The client-side @formatBytes@ mirrors 'Utils.formatBytes'.
    isBytes = any isByteUnit unitM


-- | Widgets that plot groups against each other rather than against time.
categoricalWidget :: WidgetType -> Bool
categoricalWidget = (`elem` [WTDistribution, WTPieChart, WTTopList, WTTreeMap, WTFunnel])


-- Function to convert Widget to ECharts options
widgetToECharts :: Widget -> AE.Value
widgetToECharts widget
  | widget.wType == WTStat =
      AE.object
        [ "graphic"
            AE..= AE.object
              [ "type" AE..= ("text" :: Text)
              , "left" AE..= ("center" :: Text)
              , "top" AE..= ("middle" :: Text)
              , "style"
                  AE..= AE.object
                    [ "text" AE..= maybe "N/A" (\value -> formatStatValue widget.decimals value $ fromMaybe "" widget.unit) (widget.dataset >>= (.value))
                    , "fontSize" AE..= (48 :: Int)
                    , "fontWeight" AE..= (600 :: Int)
                    , "fill" AE..= (if widget.theme == Just "dark" then "#cbd5e1" else "#334155" :: Text)
                    ]
              ]
        ]
widgetToECharts widget =
  let isStat = widget.wType == WTTimeseriesStat
      axisVisibility = not isStat
      legendVisibility = not isStat && not (or widget.hideLegend)
      -- Series names from dataset source (headers[1:])
      seriesNames = case widget.dataset <&> (.source) of
        Just (AE.Array arr) | Just (AE.Array headers) <- arr V.!? 0 -> mapMaybe (\case AE.String t -> Just t; _ -> Nothing) $ drop 1 $ V.toList headers
        _ -> []
      yMax = (.maxGroupSum) <$> (widget.dataset >>= (.stats))
      maxLabel = if or (widget.yAxis >>= (.showOnlyMaxLabel)) then encodeText <$> yMax else Nothing
      -- Detect categorical widget types (no time axis)
      isCategorical = categoricalWidget widget.wType
      xAxisType = if isCategorical then "category" else "time"
      legendTop = any (T.isPrefixOf "top") widget.legendPosition
      legendBottom = maybe True (T.isPrefixOf "bottom") widget.legendPosition
   in AE.object
        [ "tooltip"
            AE..= AE.object
              [ "show" AE..= fromMaybe True widget.showTooltip
              , "trigger" AE..= ("axis" :: Text)
              , "axisPointer"
                  AE..= AE.object
                    ["type" AE..= ("shadow" :: Text)]
              , "valueFormatter"
                  AE..= ("function(value) { return " <> unitValueExprJS KeepUnitWord widget.unit <> "; }")
              ]
        , "legend"
            AE..= AE.object
              ( let pos = fromMaybe "bottom" widget.legendPosition
                    (vPos, hPos) = case T.splitOn "-" pos of
                      [v, h] | h == "right" || h == "left" -> (v, Just h)
                      [v] -> (v, Nothing)
                      _ -> ("bottom", Nothing)
                    (fontSize, itemSize, itemGap, pad) = case fromMaybe LSSm widget.legendSize of
                      LSXs -> (10 :: Int, 6 :: Int, 6 :: Int, [2, 4, 2, 4] :: [Int])
                      LSSm -> (12, 9, 9, [3, 6, 3, 6])
                      LSMd -> (14, 12, 12, [4, 8, 4, 8])
                      LSLg -> (16, 14, 14, [5, 10, 5, 10])
                    legendOffset = case vPos of
                      "top" -> ["top" AE..= (0 :: Int)]
                      "right" -> ["orient" AE..= ("vertical" :: Text)]
                      _ -> ["bottom" AE..= (2 :: Int)]
                 in [ "show" AE..= legendVisibility
                    , "textStyle" AE..= AE.object ["fontSize" AE..= fontSize, "padding" AE..= ([0, 0, 0, -2] :: [Int])]
                    , "itemWidth" AE..= itemSize
                    , "itemHeight" AE..= itemSize
                    , "itemGap" AE..= itemGap
                    , "padding" AE..= pad
                    , "tooltip" AE..= AE.object ["show" AE..= True]
                    , "data" AE..= maybe seriesNames (map (fromMaybe "Unnamed Series" . (.query))) widget.queries -- Use series names from dataset if no explicit queries
                    ]
                      <> legendOffset
                      <> [K.fromText h AE..= (0 :: Int) | Just h <- [hPos]]
              )
        , "grid"
            AE..= AE.object
              [ "left" AE..= ("0%" :: Text)
              , -- Room for the last x-axis label, which is centred on the edge and was clipped.
                "right" AE..= (16 :: Int)
              , "top" AE..= if legendTop && legendVisibility then if widget.legendSize == Just LSXs then (20 :: Int) else 28 else if or widget.naked then (16 :: Int) else (8 :: Int)
              , "bottom" AE..= if legendBottom && legendVisibility then (36 :: Int) else if or widget.standalone then (0 :: Int) else (8 :: Int)
              , "containLabel" AE..= True
              , "show" AE..= False
              ]
        , "xAxis"
            AE..= AE.object
              ( [ "type" AE..= xAxisType
                , "scale" AE..= True
                , "boundaryGap" AE..= if isCategorical then AE.Bool True else AE.Array (V.fromList [AE.Number 0, AE.Number 0.01])
                , "splitLine" AE..= AE.object ["show" AE..= False]
                , "axisLine" AE..= AE.object ["show" AE..= axisVisibility, "lineStyle" AE..= AE.object ["color" AE..= "#000833A6", "type" AE..= "solid", "opacity" AE..= 0.1]]
                , "axisLabel" AE..= AE.object ["show" AE..= (axisVisibility && fromMaybe True (widget.xAxis >>= (.showAxisLabel))), "margin" AE..= (8 :: Int), "hideOverlap" AE..= True]
                , "show" AE..= (axisVisibility || or (widget.xAxis >>= (.showAxisLabel)))
                ]
                  <> if isCategorical
                    then [] -- For categorical, ECharts will derive categories from dataset
                    else
                      [ "min" AE..= (widget.dataset >>= (.from))
                      , "max" AE..= (widget.dataset >>= (.to))
                      ]
              )
        , "yAxis"
            AE..= AE.object
              [ "type" AE..= ("value" :: Text)
              , -- A sparkline shows shape, not magnitude (the headline carries that): fit it to the data,
                -- or a metric steady near its ceiling (Apdex 0.995) fills the card as a flat block.
                "min" AE..= if isStat then AE.String "function(v) { var pad = (v.max - v.min) * 0.1; return v.min >= 0 ? Math.max(0, v.min - pad) : v.min - pad; }" else AE.Number 0
              , "max" AE..= yMax
              , "splitLine"
                  AE..= AE.object
                    [ "show" AE..= axisVisibility
                    , "lineStyle" AE..= AE.object ["type" AE..= "dotted", "color" AE..= "#0011661A"]
                    , "interval"
                        AE..= maybe AE.Null (\n -> AE.String $ "function(index, value) { return value === " <> n <> "; }") maxLabel
                    ]
              , "axisTick" AE..= AE.object ["show" AE..= False]
              , "axisLine" AE..= AE.object ["show" AE..= False]
              , "axisLabel"
                  AE..= AE.object
                    [ "show" AE..= (axisVisibility && fromMaybe True (widget.yAxis >>= (.showAxisLabel)))
                    , "inside" AE..= False
                    , "margin" AE..= (8 :: Int)
                    , "hideOverlap" AE..= True
                    , -- The axis max is the data's peak ("656"), crowding the nice tick under it; max-only axes keep it.
                      "showMaxLabel" AE..= isJust maxLabel
                    , "formatter"
                        AE..= let fmt = unitValueExprJS DropUnitWord widget.unit
                               in maybe ("function(value, index) { return " <> fmt <> "; }") (\n -> "function(value, index) { return (value === " <> n <> " || value == 0) ? " <> fmt <> " : ''; }") maxLabel
                    ]
              , "show" AE..= axisVisibility
              ]
        , "dataset"
            AE..= AE.object
              ["source" AE..= ((.source) <$> widget.dataset)]
        , "series" AE..= addMarkLinesToFirstSeries widget (zipWith (createSeries widget.wType) [1 ..] seriesNames)
        , "animation" AE..= False
        , if or widget.allowZoom
            then
              "toolbox"
                AE..= AE.object
                  [ "feature" AE..= AE.object ["dataZoom" AE..= AE.object ["show" AE..= True, "yAxisIndex" AE..= "none", "icon" AE..= AE.object ["zoom" AE..= "none", "back" AE..= "none"]]]
                  ]
            else "toolbox" AE..= AE.object []
        ]


addMarkLinesToFirstSeries :: Widget -> [AE.Value] -> [AE.Value]
addMarkLinesToFirstSeries widget series
  | shouldShowLines, not (null markLineData) = series & _head %~ addMarkLine
  | otherwise = series
  where
    shouldShowLines = case fromMaybe TLAlways widget.showThresholdLines of
      TLNever -> False
      TLOnBreach -> any (/= Monitors.MSNormal) widget.alertStatus
      TLAlways -> isJust widget.alertThreshold || isJust widget.warningThreshold

    mkMarkLine color label threshold =
      AE.object
        [ "yAxis" AE..= threshold
        , "lineStyle" AE..= AE.object ["color" AE..= (color :: Text), "type" AE..= ("dashed" :: Text), "width" AE..= (2 :: Int)]
        , "label" AE..= AE.object ["formatter" AE..= (("function(params) { var value = params.value; return " <> encodeText (label <> ": " :: Text) <> " + " <> unitValueExprJS KeepUnitWord widget.unit <> "; }") :: Text), "position" AE..= ("insideEndTop" :: Text)]
        ]

    markLineData :: [AE.Value]
    markLineData =
      catMaybes
        [ widget.alertThreshold <&> mkMarkLine "#dc2626" "Alert"
        , widget.warningThreshold <&> mkMarkLine "#f59e0b" "Warning"
        ]

    addMarkLine :: AE.Value -> AE.Value
    addMarkLine (AE.Object obj) = AE.Object $ AE.KeyMap.insert "markLine" (AE.object ["silent" AE..= True, "symbol" AE..= ("none" :: Text), "data" AE..= markLineData]) obj
    addMarkLine v = v


createSeries :: WidgetType -> Int -> Text -> AE.Value
createSeries widgetType colIdx name =
  let isStat = widgetType == WTTimeseriesStat
      seriesColor = getSeriesColorHex name
      gradientStyle =
        AE.object
          [ "color"
              AE..= AE.object
                [ "type" AE..= ("linear" :: Text)
                , "x" AE..= (0 :: Int)
                , "y" AE..= (0 :: Int)
                , "x2" AE..= (0 :: Int)
                , "y2" AE..= (1 :: Int)
                , "colorStops" AE..= ([AE.object ["offset" AE..= (0 :: Int), "color" AE..= seriesColor], AE.object ["offset" AE..= (1 :: Int), "color" AE..= ("rgba(0,0,0,0)" :: Text)]] :: [AE.Value])
                ]
          ]
   in AE.object
        [ "name" AE..= name
        , "type" AE..= mapWidgetTypeToChartType widgetType
        , "stack" AE..= ("Stack" :: Text)
        , "encode" AE..= AE.object ["x" AE..= (0 :: Int), "y" AE..= colIdx]
        , "itemStyle" AE..= AE.object ["color" AE..= seriesColor, "borderRadius" AE..= ([2, 2, 0, 0] :: [Int])]
        , "barCategoryGap" AE..= ("30%" :: Text)
        , "barMaxWidth" AE..= (10 :: Int)
        , "areaStyle" AE..= if isStat then gradientStyle else AE.Null
        , "lineStyle" AE..= AE.object ["width" AE..= if isStat then 0 else 1]
        ]


-- | Map widget type to ECharts chart type
--
-- >>> mapWidgetTypeToChartType WTTimeseries
-- "bar"
-- >>> mapWidgetTypeToChartType WTTimeseriesLine
-- "line"
-- >>> mapWidgetTypeToChartType WTDistribution
-- "bar"
-- >>> mapWidgetTypeToChartType WTHeatmap
-- "heatmap"
-- >>> mapWidgetTypeToChartType WTServiceMap
-- "graph"
mapWidgetTypeToChartType :: WidgetType -> Text
mapWidgetTypeToChartType WTTimeseriesLine = "line"
mapWidgetTypeToChartType WTTimeseriesStat = "line"
mapWidgetTypeToChartType WTServiceMap = "graph" -- ECharts force-directed graph
mapWidgetTypeToChartType WTHeatmap = "heatmap" -- ECharts heatmap
mapWidgetTypeToChartType _ = "bar"


mapChartTypeToWidgetType :: Text -> WidgetType
mapChartTypeToWidgetType "line" = WTTimeseriesLine
mapChartTypeToWidgetType "timeseries_line" = WTTimeseriesLine
mapChartTypeToWidgetType _ = WTTimeseries


renderTableWithDataAndParams :: Widget -> V.Vector Text -> V.Vector (V.Vector Text) -> [(Text, Maybe Text)] -> Html ()
renderTableWithDataAndParams widget headers dataRows params = do
  let columns = fromMaybe (inferColumns headers dataRows) widget.columns
      tableId = maybeToMonoid widget.id
      param k = join $ lookup k params
      currentVar = widget.onRowClick >>= (.setVariable) >>= param . ("var-" <>)
      -- URL-encoded link substitutions; links into other pages keep the window the table was read over.
      linkSubs = M.fromList [("project_id", projectIdText widget), ("time_range", TimePicker.rangeQuery $ TimePicker.TimePicker (param "since") (param "from") (param "to"))]
      -- Rows navigate server-side to the @navigate_to_tab@ tab, else the widget's own tab
      -- (or the whole tabless dashboard), with the row's variable set.
      rowNavAttrs value = fold do
        action <- widget.onRowClick
        (pid, did) <- dashboardIds widget
        let tabM = slugify <$> action.navigateToTab <|> widget._tabSlug
            varKey = ("var-" <>) <$> action.setVariable
            vars = [(k, v) | (k, Just v) <- params, "var-" `T.isPrefixOf` k, Just k /= varKey] <> [(k, value) | k <- toList varKey]
        pure
          $ dashboardContentNavAttrs (maybe WholePage (const TabPanel) tabM) (maybe (urls.dashboardsGet pid did) (urls.dashboardTabGet pid did) tabM Nothing Nothing Nothing Nothing (second Just <$> vars))
          <> [term "hx-preload" "mousedown"]
          <> foldMap (\var -> [name_ "dashboard-variable-option", data_ "variable-key" var, data_ "variable-value" value, data_ "variable-label" value, term "hx-on:htmx:before:request" "syncDashboardVariable(this, true)", term "hx-on:htmx:finally:request" "syncDashboardVariable(this, false)"]) action.setVariable

      -- A breakdown (a query's own columns, or a top list) colours each row as its series is
      -- coloured on a chart; a configured table's rows are records, not series, and stay plain.
      breakdown = isNothing widget.columns || widget.wType == WTTopList
      -- @summarize … by a, b@ on a top list: the first column heads a section of its rows.
      grouped = widget.wType == WTTopList && length (takeWhile (not . isNumericCol) columns) >= 2
      visible = filter (not . or . (.hidden)) $ drop labelIdx columns
      labelIdx = bool 0 1 grouped
      maxValues = M.fromList [(col.field, V.foldl' max 0 $ V.mapMaybe (readMaybe . toString . getRowValue idx) dataRows) | V.length dataRows > 1, (col, idx) <- zip columns [0 ..], col.progress == Just ColumnPercent]
      -- One bar per row: on the column the reader sorted by, else the first that offers one.
      sortedField = T.drop 1 <$> param "table-sort"
      barField = (.field) <$> (find (\c -> isJust c.progress && Just c.field == sortedField) visible <|> find (isJust . (.progress)) visible)
      -- Every column sizes to its widest value but long text (>40 chars), which shares what is left;
      -- with none, the widest text column takes the slack instead of the first one.
      widestOf col = foldl' max (T.length col.title + 2) [T.length (cellText col (getRowValue (colIdx col) r)) | r <- toList dataRows]
      -- What a cell shows: an ID's short form, else the formatted value.
      cellText col v = fromMaybe (formatColumnValue col v) (guard (not $ isNumericCol col) *> shortId v)
      texts = sortOn (Down . widestOf) $ filter (not . alignsRight) visible
      flexFields = map (.field) $ case filter ((> 40) . widestOf) texts of [] -> take 1 texts; long -> long
      colWidth col = col.width <|> (("calc(" <> show (widestOf col) <> "ch + " <> bool "2rem" "7rem" (Just col.field == barField) <> ")") <$ guard (col.field `notElem` flexFields))
      colIdx col = fromMaybe 0 $ findIndex ((== col.field) . (.field)) columns
      -- The next standard window wider than the one the table read, offered as one click.
      widen = maybe ESNone (\(k, (_, label)) -> ESCustom $ button_ [type_ "button", class_ "btn btn-sm btn-ghost w-max mx-auto text-textBrand", term "_" [text|on click call updateTimePicker({since: '${k}'}) then call dispatchQueryUpdate()|]] $ toHtml ("Show " <> T.toLower label)) case TimePicker.parseTimeRange (posixSecondsToUTCTime 0) (TimePicker.TimePicker (param "since") (param "from") (param "to")) of
        (Just a, Just b, _) -> find (\(k, (secs, _)) -> k `elem` ["24H", "7D", "30D"] && fromInteger secs > diffUTCTime b a) sinceWindows
        _ -> Nothing
      sections = bool [(Nothing, toList dataRows)] [(Just (getRowValue 0 (head g)), toList g) | g <- NE.groupBy ((==) `on` getRowValue 0) (toList dataRows)] grouped

  table_ [class_ "dashboard-table table table-sm w-full relative [&_:is(th,td):first-child]:pl-4 [&_:is(th,td):last-child]:pr-4 [&_td]:border-b [&_td]:border-strokeWeak/50 [&_tr:last-child_td]:border-b-0", id_ tableId] do
    colgroup_ $ forM_ visible $ col_ . foldMap (\w -> [style_ $ "width: " <> w]) . colWidth
    unless (widget.wType == WTTopList) $ sortableTableHead_ widget columns (param "table-sort")
    tbody_ [] $ forM_ sections \(section, rows) -> do
      whenJust section $ tr_ [] . th_ [colspan_ (show $ length visible), scope_ "colgroup", class_ "pt-3 pb-1 text-left text-xs font-medium text-textWeak"] . toHtml
      forM_ rows \row -> do
        let firstColValue = memptyIfFalse (not $ null columns) (getRowValue 0 row)
            rowSubs = M.fromList [("row." <> c.field, getRowValue i row) | (c, i) <- zip columns [0 ..]]
            rowValue = maybe firstColValue (replacePlaceholders rowSubs) (widget.onRowClick >>= (.value))
            cellLinkSubs = linkSubs <> fmap toUriStr rowSubs
            isCurrent = Just rowValue == currentVar
            series = getRowValue (bool labelIdx 0 grouped) row
        tr_
          ( [class_ $ "hover" <> memptyIfFalse (isJust widget.onRowClick) " cursor-pointer" <> memptyIfFalse isCurrent " bg-fillBrand-weak font-semibold"]
              <> [style_ $ "--series:" <> getSeriesColorHex series | breakdown]
              <> [term "aria-current" "true" | isCurrent]
              <> rowNavAttrs rowValue
              <> memptyIfFalse (isJust widget.onRowClick) [tabindex_ "0", Aria.label_ $ "Filter by " <> rowValue, term "_" " on keydown[key=='Enter' or key==' '] halt the event then call me.click() end "]
          )
          $ forM_ visible \col -> do
            let idx = colIdx col
            td_ ([class_ $ bool (fromMaybe "" col.align) "text-right tabular-nums" (alignsRight col), data_ "label" col.title] <> foldMap (\t -> let tip = replacePlaceholders rowSubs t in [term "data-tippy-content" tip, term "aria-description" tip]) col.tooltip)
              $ if
                | isJust col.progress, Just col.field == barField -> renderProgressCell col (getRowValue idx row) maxValues
                | breakdown && idx == labelIdx -> span_ [class_ "flex items-center gap-2 min-w-0"] do
                    span_ [class_ "size-2.5 shrink-0 rounded-sm bg-(--series)", Aria.hidden_ "true"] ""
                    span_ [class_ "min-w-0"] $ renderLongTextOr widget cellLinkSubs col (getRowValue idx row)
                | otherwise -> renderLongTextOr widget cellLinkSubs col (getRowValue idx row)
      when (V.null dataRows) $ tr_ [] $ td_ [colspan_ (show $ max 1 $ length visible), class_ "py-10"] case param "table-search" of
        Just q | not (T.null q) -> emptyState_ def{size = ESCompact, icon = Just "magnifying-glass"} ("No rows match “" <> q <> "”") "Try a different search, or widen the time range."
        _ -> emptyState_ def{size = ESCompact, icon = Just "table", action = widen} "Nothing in this time range" "Widen the time range or loosen the dashboard filters to look further."


-- | A query's own result as columns: the names it returned, numbers right-aligned with a bar
-- against the column's largest value. Sorting needs configured columns, so these don't offer it.
--
-- >>> map (\c -> (c.field, c.columnType, c.progress)) $ inferColumns (V.fromList ["name", "count_"]) (V.fromList [V.fromList ["GET", "12"], V.fromList ["POST", "3.5"]])
-- [("name",Nothing,Nothing),("count_",Just CTNumber,Just ColumnPercent)]
inferColumns :: V.Vector Text -> V.Vector (V.Vector Text) -> [TableColumn]
inferColumns headers rows = zipWith column [0 ..] (toList headers)
  where
    numeric i = not (V.null rows) && all (isJust . readMaybe @Double . toString . getRowValue i) rows
    column i h =
      (def :: TableColumn){field = h, title = h, sortable = Just False}
        & bool Relude.id (\c -> c{columnType = Just CTNumber, align = Just "text-right", progress = Just ColumnPercent}) (numeric i)


getRowValue :: Int -> V.Vector Text -> Text
getRowValue idx row = fromMaybe "" $ row V.!? idx


-- | The value over a bar filling its share of the cell, so a column reads as a ranking at a
-- glance. A breakdown row's bar takes its series colour; others take the column's variant.
renderProgressCell :: TableColumn -> Text -> M.Map Text Double -> Html ()
renderProgressCell col value maxValues = div_ [class_ "relative isolate -mx-2 px-2 py-0.5 text-right tabular-nums"] do
  let numValue = fromMaybe 0 (readMaybe $ toString value) :: Double
      percentage = case col.progress of
        Just ValuePercent -> min 100 (max 0 numValue)
        Just ColumnPercent | Just maxVal <- M.lookup col.field maxValues, maxVal > 0 -> (numValue / maxVal) * 100
        _ -> 0
  span_
    [ class_
        $ "absolute inset-y-0 left-0 -z-10 rounded-sm "
        <> case col.progressVariant of
          Just "error" -> "bg-fillError-weak"
          Just "warning" -> "bg-fillWarning-weak"
          Just "success" -> "bg-fillSuccess-weak"
          _ -> "bg-[color-mix(in_oklch,var(--series,var(--color-fillBrand-strong))_22%,transparent)]"
    , style_ $ "width: " <> show (round percentage :: Int) <> "%"
    , Aria.hidden_ "true"
    ]
    ""
  toHtml $ formatColumnValue col value


isNumericCol :: TableColumn -> Bool
isNumericCol col = col.columnType `elem` [Just CTNumber, Just CTDuration]


-- | Numbers line up on the right, and so do their headers, whether or not the column says so.
alignsRight :: TableColumn -> Bool
alignsRight col = isNumericCol col || isJust col.progress || any ("text-right" `T.isInfixOf`) col.align


-- | A byte column reads in binary units, as the chart axes do, not as a raw count with "By" after it.
--
-- >>> formatColumnValue (def{columnType = Just CTNumber, unit = Just "By"}) "415100518"
-- "395.9 MiB"
formatColumnValue :: TableColumn -> Text -> Text
formatColumnValue col value = case fromMaybe CTText col.columnType of
  CTNumber -> maybe (value <> unitSuffix) (bool ((<> unitSuffix) . fmtNumber) formatBytes (any isByteUnit col.unit)) (readMaybe (toString value))
  CTDuration -> maybe (value <> unitSuffix) (\ns -> bool (T.replace ".0 " " " $ getDurationNSMS ns) "0" (ns == 0)) (readMaybe (toString value) >>= \n -> round . (n *) <$> maybe (Just 1) unitNanos col.unit)
  CTText -> maybe value (toText . formatTime defaultTimeLocale "%b %-d %H:%M:%S") (parseTimestamp value) <> unitSuffix
  where
    unitSuffix = foldMap (" " <>) col.unit
    fmtNumber n
      | isJust col.decimals || (n < 100 && n /= fromIntegral (round n :: Int)) = formatStatValue col.decimals n ""
      | otherwise = prettyPrintCount (round n)


-- | Nanoseconds per duration unit; micro accepts both the micro sign and Greek mu.
--
-- >>> map unitNanos ["ms", "\181s", "\956s", "parsecs"]
-- [Just 1000000.0,Just 1000.0,Just 1000.0,Nothing]
unitNanos :: Text -> Maybe Double
unitNanos = (`lookup` [("h", 3.6e12), ("m", 6e10), ("s", 1e9), ("ms", 1e6), ("\181s", 1e3), ("\956s", 1e3), ("us", 1e3), ("ns", 1)])


-- | An ID (UUID, trace or span id) is matched and copied, not read: its first 8 characters, git-style.
--
-- >>> map shortId ["d85973c0-8129-4c80-af0a-b81d7f509c29", "4bf92f3577b34da6a3ce929d0e0e4736", "1234567890123456", "deadbeef", "api.shipbubble.com"]
-- [Just "d85973c0\8230",Just "4bf92f35\8230",Nothing,Nothing,Nothing]
shortId :: Text -> Maybe Text
shortId v = T.take 8 v <> "…" <$ guard (T.length v >= 16 && T.all (\c -> isHexDigit c || c == '-') v && T.any isAlpha v && T.count "-" v `elem` ([0, 4] :: [Int]))


-- | A cell value that is a PostgreSQL or ISO-8601 timestamp. Short offsets (+00) are widened for %z.
--
-- >>> map parseTimestamp ["2026-11-06T23:21:20.435Z", "2026-03-19 09:05:00+00", "GET /api/cart"]
-- [Just 2026-11-06 23:21:20.435 UTC,Just 2026-03-19 09:05:00 UTC,Nothing]
parseTimestamp :: Text -> Maybe UTCTime
parseTimestamp (T.strip -> val)
  | T.length val < 19 = Nothing
  | otherwise = zonedTimeToUTC <$> parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%z" (toString normalized) <|> iso8601ParseM (toString val)
  where
    normalized = case T.breakOnEnd "+" val of
      (pfx, tz) | not (T.null pfx), T.length tz < 4 -> pfx <> tz <> T.replicate (4 - T.length (T.filter isDigit tz)) "0"
      _ -> val


-- | Render a text cell, parsing summary tags (field;style⇒value) into badges.
-- Long values get a truncated single-line view with a tippy tooltip (delegated
-- body-wide in BodyWrapper) showing the full text on hover. Number/duration
-- columns skip the wrapper since they're always short.
renderLongTextOr :: Widget -> M.Map Text Text -> TableColumn -> Text -> Html ()
renderLongTextOr widget linkSubs col value
  -- A missing value is absence, not data: never a "null" link or a copy target.
  | T.null (T.strip value) || value == "null" = emDash_
  -- The button floats over the cell's end on hover rather than reserving a column on every row.
  | or col.copyable || isJust shortened = div_ [class_ "group relative min-w-0"] do
      shown
      copyButton_ "btn btn-xs absolute right-0 top-1/2 -translate-y-1/2 gap-1 bg-bgRaised border-strokeWeak shadow-sm opacity-0 transition-[opacity,scale] duration-150 ease-out group-hover:opacity-100 focus-visible:opacity-100 active:scale-[0.96]" "w-3 h-3" "my @data-copy" [data_ "copy" value]
  | otherwise = shown
  where
    -- Long text wraps to two lines (code at any character) with the whole value on hover:
    -- two queries sharing a prefix differ by the second line, not by an ellipsis.
    shown
      | not (isNumericCol col), T.length value > 40 = div_ [class_ $ "line-clamp-2 " <> bool "break-words" "break-all" code, term "data-tippy-content" (sqlLines $ T.take 1500 value), term "data-tippy-placement" "top-start"] cell
      | otherwise = cell
    code = or col.copyable
    shortened = guard (not $ isNumericCol col) *> shortId value
    formatted = formatColumnValue col value
    content
      | T.isInfixOf "⇒" formatted = renderSummaryTags formatted
      -- In the viewer's zone, as the log list shows it; the exact UTC instant on hover.
      | Just short <- shortened = code_ [class_ "font-mono text-xs", term "data-tippy-content" value] $ toHtml short
      | not (isNumericCol col), Just t <- parseTimestamp value = span_ [class_ "tabular-nums whitespace-nowrap", term "data-tippy-content" (toText $ iso8601Show t)] $ localTimeFmt_ "MMM d HH:mm:ss" t
      | code = code_ [class_ "font-mono text-xs"] $ toHtml formatted
      | otherwise = toHtml formatted
    cell = maybe content (\template -> a_ [href_ $ tableLink template, class_ "text-textBrand hover:underline focus:outline-none focus-visible:ring-2 focus-visible:ring-strokeBrand-strong rounded-sm"] content) col.link
    tableLink = replacePlaceholders linkSubs


-- | A SQL statement broken before its clauses, for reading in a tooltip. Display only: a word
-- touching a paren (subquery, window) stays inline, and non-SQL text is returned unchanged.
--
-- >>> sqlLines "select a from t left join u on t.id = u.id where x = 1 and y = 2 order by a"
-- "select a\nfrom t\nleft join u\non t.id = u.id\nwhere x = 1\n  and y = 2\norder by a"
-- >>> sqlLines "insert into t values ($1); select 1"
-- "insert into t\nvalues ($1);\nselect 1"
-- >>> sqlLines "connection reset from 10.0.0.4 where pool was full"
-- "connection reset from 10.0.0.4 where pool was full"
sqlLines :: Text -> Text
sqlLines sql = case words sql of
  w : ws | T.toUpper w `elem` ["SELECT", "INSERT", "UPDATE", "DELETE", "WITH"] -> T.concat $ w : zipWith (\p x -> sep (T.toUpper p) (T.toUpper x) <> x) (w : ws) ws
  _ -> sql
  where
    sep p x
      | ";" `T.isSuffixOf` p = "\n"
      | x `elem` ["AND", "OR"] = "\n  "
      | x `elem` ["FROM", "WHERE", "GROUP", "ORDER", "HAVING", "LIMIT", "OFFSET", "JOIN", "LEFT", "RIGHT", "INNER", "FULL", "CROSS", "ON", "VALUES", "SET", "RETURNING", "UNION", "EXCEPT", "INTERSECT"]
      , p `notElem` ["LEFT", "RIGHT", "INNER", "FULL", "CROSS", "OUTER"] =
          "\n"
      | otherwise = " "
