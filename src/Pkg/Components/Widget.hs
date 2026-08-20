module Pkg.Components.Widget (Widget (..), WidgetDataset (..), chartQuery, toWidgetDataset, widget_, gridStackAttrs, normalizeWidgetLayouts, Layout (..), WidgetType (..), TableColumn (..), RowClickAction (..), mapChatTypeToWidgetType, mapWidgetTypeToChartType, widgetToECharts, WidgetAxis (..), SummarizeBy (..), widgetPostH, renderTraceDataTable, renderTableWithDataAndParams, signWidgetUrl, widgetPngUrl, getSpanJson, encodeText) where

import Codec.Compression.GZip qualified as GZip
import Control.Lens
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as AE.KeyMap
import Data.Base64.Types qualified as B64
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.Char (isDigit)
import Data.Default
import Data.Generics.Labels ()
import Data.HashMap.Lazy qualified as HM
import Data.Map.Strict qualified as M
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Data.Time (ZonedTime, defaultTimeLocale, parseTimeM)
import Data.Time.Format (formatTime)
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
import Lucid.Htmx (hxExt_, hxGet_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation
import Network.HTTP.Types (urlEncode)
import Pages.Charts.Charts qualified as Charts
import Pages.Components (headerRow_)
import Pages.LogExplorer.LogItem (getServiceName, spanHasErrors)
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Text.Printf (printf)
import Text.Slugify (slugify)
import Utils
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)
import "base64" Data.ByteString.Base64.URL qualified as B64URL
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


-- Generic instance for parsing JSON arrays from form data
instance AE.FromJSON a => FromHttpApiData [a] where
  parseQueryParam = first toText . AE.eitherDecodeStrict . encodeUtf8


-- Generic instance for parsing JSON values from form data
instance {-# OVERLAPPABLE #-} AE.FromJSON a => FromHttpApiData a where
  parseQueryParam = first toText . AE.eitherDecodeStrict . encodeUtf8


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
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Layout


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
  | WTTraces
  | WTFlamegraph
  | WTServiceMap -- Service dependency graph visualization
  | WTHeatmap -- Latency distribution heatmap
  -- Bounded so every widget type can be enumerated ([minBound ..]) rather than listed by
  -- hand: adding a constructor then extends the round-trip/render specs automatically
  -- instead of silently shipping untested.
  deriving stock (Bounded, Enum, Eq, Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "WT", DAE.CamelToSnake]] WidgetType


data SummarizeBy
  = SBSum
  | SBMax
  | SBMin
  | SBCount
  | SBMean
  | SBRate
  deriving stock (Enum, Eq, Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "SB", DAE.CamelToSnake]] SummarizeBy


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


-- when processing widgets we'll do them async, so eager queries are loaded upfront
data Widget = Widget
  { wType :: WidgetType -- Widget type: "timeseries", "table", etc.
  , id :: Maybe Text
  , naked :: Maybe Bool
  , showTooltip :: Maybe Bool
  , title :: Maybe Text -- Widget title
  , subtitle :: Maybe Text
  , hideSubtitle :: Maybe Bool
  , icon :: Maybe Text
  , timeseriesStatAggregate :: Maybe Text -- average, min, max, sum, etc
  , sql :: Maybe Text
  , rawQuery :: Maybe Text -- Original KQL query with {{const-...}} placeholders (for editor display)
  , summarizeBy :: Maybe SummarizeBy
  , query :: Maybe Text
  , queries :: Maybe [Query] -- Multiple queries for combined visualizations
  , layout :: Maybe Layout -- Layout (x, y, w, h)
  , xAxis :: Maybe WidgetAxis
  , yAxis :: Maybe WidgetAxis -- Optional y-axis label
  , unit :: Maybe Text
  , value :: Maybe Double -- value could represent a number or a count
  , wData :: Maybe AE.Value
  , hideLegend :: Maybe Bool
  , legendPosition :: Maybe Text -- Legend position: "top", "bottom", "top-right", "top-left", "bottom-right", "bottom-left"
  , legendSize :: Maybe Text -- Legend size: "xs" (default), "sm", "md"
  , theme :: Maybe Text
  , dataset :: Maybe WidgetDataset
  , -- eager
    eager :: Maybe Bool
  , _projectId :: Maybe Projects.ProjectId
  , _dashboardId :: Maybe Text -- Dashboard ID for context
  , _isNested :: Maybe Bool
  , _centerTitle :: Maybe Bool
  , expandBtnFn :: Maybe Text
  , expandPushUrl :: Maybe Text
  , groupByOptions :: Maybe [Text]
  , groupBySelected :: Maybe Text
  , groupByUrl :: Maybe Text
  , groupByTarget :: Maybe Text
  , children :: Maybe [Widget]
  , html :: Maybe LText
  , standalone :: Maybe Bool -- Not used in a grid stack
  , allowZoom :: Maybe Bool -- Allow zooming in the chart
  , showMarkArea :: Maybe Bool -- Show mark area in the chart
  , columns :: Maybe [TableColumn] -- Table columns
  , onRowClick :: Maybe RowClickAction -- Action when table row is clicked
  -- Alert fields (populated from QueryMonitor at render time)
  , alertId :: Maybe Text -- Linked QueryMonitor ID
  , alertThreshold :: Maybe Double -- For threshold line rendering
  , warningThreshold :: Maybe Double
  , showThresholdLines :: Maybe Text -- 'always' | 'on_breach' | 'never'
  , alertStatus :: Maybe Text -- 'normal' | 'warning' | 'alerting' (runtime)
  , description :: Maybe Text -- Help text shown in info icon tooltip
  , pngUrl :: Maybe Text -- Pre-signed PNG download URL (runtime)
  , _staticRender :: Maybe Bool -- For PNG export: disables scroll legend
  , dbSource :: Maybe Text -- "postgres" or "timefusion"; Nothing = default routing
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] Widget


instance ToHtml Widget where
  toHtml w = toHtml $ widget_ w
  toHtmlRaw = toHtml


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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetDataset


-- | The query to run for a widget, paired with the row shape it decodes into.
-- The two have to be chosen together: each decoder accepts exactly one column
-- count, so shaping the query without picking the matching 'Charts.DataType'
-- (or vice versa) fails the whole widget with a column-count mismatch.
--
--   * plotted widgets need a series, so a widget whose KQL is only a filter
--     gets the same default aggregation the browser renderer applies
--     (@updateChartData@ in web-components/src/widgets.ts) — one count per
--     auto-sized time bin;
--   * a stat reads one scalar, so it only needs an aggregation when the query
--     has none; binning it would yield a three-column series;
--   * row-oriented widgets project their own columns and are passed through.
--
-- >>> chartQuery (def & #query ?~ "severity==\"ERROR\"")
-- (Just "severity==\"ERROR\" | summarize count(*) by bin_auto(timestamp)",DTMetric)
-- >>> chartQuery (def & #query ?~ "summarize count(*) by bin_auto(timestamp)")
-- (Just "summarize count(*) by bin_auto(timestamp)",DTMetric)
-- >>> chartQuery (def & #wType .~ WTStat & #query ?~ "kind == \"server\"")
-- (Just "kind == \"server\" | summarize count(*)",DTFloat)
-- >>> chartQuery (def & #wType .~ WTStat & #query ?~ "name != null | summarize dcount(name)")
-- (Just "name != null | summarize dcount(name)",DTFloat)
-- >>> chartQuery (def & #wType .~ WTTable & #query ?~ "summarize count(*) by service")
-- (Just "summarize count(*) by service",DTText)
-- >>> chartQuery (def :: Widget)
-- (Nothing,DTMetric)
chartQuery :: Widget -> (Maybe Text, Charts.DataType)
chartQuery w = case w.wType of
  WTStat -> (w.query <&> \q -> if hasSummarize q then q else q <> " | summarize count(*)", Charts.DTFloat)
  WTLogs -> asIs
  WTTable -> asIs
  WTTopList -> asIs
  WTTraces -> asIs
  _ -> (w.query <&> \q -> if hasSummarize q && hasBinning q then q else q <> " | summarize count(*) by bin_auto(timestamp)", Charts.DTMetric)
  where
    asIs = (w.query, Charts.DTText)
    hasSummarize = T.isInfixOf "summarize" . T.toLower
    hasBinning = T.isInfixOf " by bin" . T.toLower


-- | Convert MetricsData to WidgetDataset (timestamps already in ms from queryMetrics)
toWidgetDataset :: Charts.MetricsData -> WidgetDataset
toWidgetDataset md =
  WidgetDataset
    { source = AE.toJSON $ V.cons (AE.toJSON <$> md.headers) (AE.toJSON <<$>> md.dataset)
    , rowsPerMin = md.rowsPerMin
    , value = Just md.rowsCount
    , from = md.from
    , to = md.to
    , stats = md.stats
    }


data WidgetAxis = WidgetAxis
  { label :: Maybe Text
  , showAxisLabel :: Maybe Bool
  , series :: Maybe [WidgetAxis]
  , showOnlyMaxLabel :: Maybe Bool
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetAxis


data TableColumn = TableColumn
  { field :: Text
  , title :: Text
  , unit :: Maybe Text
  , clickable :: Maybe Bool
  , link :: Maybe Text
  , width :: Maybe Text
  , align :: Maybe Text
  , progress :: Maybe Text -- "column_percent" or "value_percent"
  , progressVariant :: Maybe Text -- "default", "info", "error", etc.
  , columnType :: Maybe Text -- "number", "duration", "text" (default)
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


-- | Encode a value as JSON Text (used for data attributes, widget JSON, etc.)
encodeText :: AE.ToJSON a => a -> Text
encodeText = decodeUtf8 . fromLazy . AE.encode


-- | Widget's project id as Text, or empty when absent.
projectIdText :: Widget -> Text
projectIdText w = foldMap (.toText) w._projectId


-- | Widget boolean flags round-trip as @Maybe Bool@ (Nothing = false); this reads them.
isTrue :: Maybe Bool -> Bool
isTrue = (== Just True)


-- | HTMX fetch URL for a (already eager-prepared) widget: the project-scoped
-- /widget endpoint with the widget JSON url-encoded as a query param.
widgetFetchUrl :: Widget -> Text
widgetFetchUrl w = "/p/" <> projectIdText w <> "/widget?widgetJSON=" <> decodeUtf8 (urlEncode True $ encodeUtf8 $ encodeText w)


-- | Data attributes for table row click handling (delegated via global JS handler in widgets.ts)
rowClickTableAttrs :: Widget -> [Attribute]
rowClickTableAttrs widget = foldMap (\action -> [data_ "on-row-click" (encodeText action)]) widget.onRowClick


-- Used when converting a widget json to its html representation. Eg in a query chart builder
widgetPostH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Widget -> ATAuthCtx (RespHeaders Widget)
widgetPostH pid sinceM fromM toM widget = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let widgetWithPid = widget & #_projectId ?~ pid
  pngUrl <- widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl pid widgetWithPid sinceM fromM toM
  addRespHeaders $ if T.null pngUrl then widgetWithPid else widgetWithPid{pngUrl = Just pngUrl}


widgetPngUrl :: Log :> es => Text -> Text -> Projects.ProjectId -> Widget -> Maybe Text -> Maybe Text -> Maybe Text -> Eff es Text
widgetPngUrl secret hostUrl pid widget since fromM toM =
  let widgetJson = encodeText widget
      compressed = B64.extractBase64 $ B64URL.encodeBase64 $ toStrict $ GZip.compress $ encodeUtf8 widgetJson
      sig = signWidgetUrl secret pid widgetJson
      timeParams = mconcat $ catMaybes [("&since=" <>) . toUriStr <$> since, ("&from=" <>) . toUriStr <$> fromM, ("&to=" <>) . toUriStr <$> toM]
      url = hostUrl <> "p/" <> pid.toText <> "/widget.png?widgetZ=" <> compressed <> timeParams <> "&sig=" <> sig
   in if T.length url > 8000 then Log.logAttention "Widget PNG URL too large" (AE.object ["projectId" AE..= pid, "urlLength" AE..= T.length url]) $> "" else pure url


signWidgetUrl :: Text -> Projects.ProjectId -> Text -> Text
signWidgetUrl secret pid widgetJson =
  let payload = pid.toText <> ":" <> widgetJson
   in decodeUtf8 @Text $ B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) (encodeUtf8 payload :: ByteString) :: HMAC.HMAC SHA256)


-- use either index or the xxhash as id
widget_ :: Widget -> Html ()
widget_ w' = case w.wType of
  WTAnomalies ->
    wgtCard_ $ withCardFrame False w (Just ("View all", "/p/" <> projectIdText w <> "/issues")) $ div_ [class_ "h-full overflow-auto p-3"] $ whenJust w.html toHtmlRaw
  WTGroup -> gridItem_ $ div_ [class_ "h-full flex flex-col border border-strokeWeak rounded-lg surface-raised overflow-hidden group/wgt"] do
    -- Header: auto height (no flex), group-header class for CSS targeting when collapsed
    div_ [class_ $ "group-header py-2 px-4 flex items-center justify-between " <> gridStackHandleClassFor w] do
      div_ [class_ "inline-flex gap-2 items-center group/h"] do
        span_ [class_ "hidden group-hover/h:inline-flex cursor-move"] $ Utils.faSprite_ "grip-dots-vertical" "regular" "w-4 h-4"
        whenJust w.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-5 h-5"
        span_ ([class_ "text-lg font-medium"] <> varTemplateAttr w.title) $ toHtml $ maybeToMonoid w.title
        descIcon_ w.description ""
      -- Collapse chevron: only for full-width groups
      when isFullWidth $ button_ [class_ "collapse-toggle p-2 rounded hover:bg-fillWeak transition-colors cursor-pointer tap-target", Aria.label_ "Toggle group", [__|on click toggle .hidden on .nested-grid in closest .grid-stack-item then toggle .collapsed on closest .grid-stack-item|]] $ Utils.faSprite_ "chevron-up" "regular" "w-5 h-5 transition-transform"
    -- Nested grid: flex-1 fills remaining space
    div_ (class_ "grid-stack grid-stack-preloaded nested-grid flex-1" : gridStackAttrs normalizedChildren) $ forM_ normalizedChildren (\wChild -> widget_ (wChild{_isNested = Just True}))
  WTTable -> wgtCard_ $ renderTable w
  WTLogs -> wgtCard_ $ renderLogsWidget w
  WTTraces -> wgtCard_ $ renderTraceTable w
  WTFlamegraph -> gridItem_ $ div_ [class_ "h-full "] $ div_ [class_ "p-3"] "Flamegraph widget coming soon"
  _ -> gridItem_ $ div_ [class_ " w-full h-full group/wgt "] $ renderChart w
  where
    normalizedChildren = normalizeWidgetLayouts $ fromMaybe [] w'.children
    w = (w' & #children .~ (normalizedChildren <$ w'.children)) & #id %~ (<|> (slugify <$> w'.title))
    isFullWidth = (== Just 12) $ w.layout >>= (.w)
    effectiveHeight = case w.wType of
      WTGroup -> Just $ widgetHeightForWidth (fromMaybe 1 $ w.layout >>= (.w)) w
      _ -> w.layout >>= (.h)
    layoutFields = [("x", (.x)), ("y", (.y)), ("w", (.w))] :: [(Text, Layout -> Maybe Int)]
    attrs =
      foldMap (\(name, field) -> foldMap (\v -> [term ("gs-" <> name) (show v)]) (w.layout >>= field)) layoutFields
        <> foldMap (\h -> [term "gs-h" (show h)]) effectiveHeight
        <> [ style_
               $ T.intercalate ";"
               $ catMaybes
                 [ ("--grid-preload-left:" <>) . gridPercent <$> (w.layout >>= (.x))
                 , ("--grid-preload-top:" <>) . gridRem <$> (w.layout >>= (.y))
                 , ("--grid-preload-width:" <>) . gridPercent <$> (w.layout >>= (.w))
                 , ("--grid-preload-height:" <>) . gridRem <$> effectiveHeight
                 ]
           ]
    widgetJson = encodeText w
    autoFitAttr = memptyIfFalse (w.wType `elem` [WTAnomalies, WTGroup, WTTable, WTLogs, WTTraces, WTFlamegraph]) [data_ "mobile-autofit" ""]
    gridItem_ =
      if isTrue w.naked
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
  where
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
-- >>> let widgets = [testWidget WTList Layout{x = Just 0, y = Just 2, w = Just 20, h = Just 2}, testWidget WTList Layout{x = Just 0, y = Just 0, w = Just 6, h = Just 1}, testWidget WTList Layout{x = Nothing, y = Nothing, w = Just 6, h = Just 1}]
-- >>> map coordinates (normalizeWidgetLayouts widgets)
-- [Just (Just 0,Just 0,Just 6,Just 1),Just (Just 6,Just 0,Just 6,Just 1),Just (Just 0,Just 1,Just 12,Just 2)]
--
-- Saved visual order wins over YAML array order, and a clamped full-width group
-- derives its height from its children:
--
-- >>> let lower = testWidget WTList Layout{x = Just 0, y = Just 4, w = Just 6, h = Just 2}
-- >>> let golden = (testWidget WTGroup Layout{x = Just 0, y = Just 0, w = Just 20, h = Just 10}){children = Just []}
-- >>> map coordinates (normalizeWidgetLayouts [lower, golden])
-- [Just (Just 0,Just 0,Just 12,Just 2),Just (Just 0,Just 2,Just 6,Just 2)]
--
-- Nested group children are normalized recursively:
--
-- >>> let child position = testWidget WTList position
-- >>> let groupWidget = (testWidget WTGroup Layout{x = Just 0, y = Just 0, w = Just 12, h = Just 1}){children = Just [child Layout{x = Just 0, y = Just 0, w = Just 8, h = Just 1}, child Layout{x = Just 0, y = Just 0, w = Just 8, h = Just 1}]}
-- >>> fmap coordinates (listToMaybe $ normalizeWidgetLayouts [groupWidget])
-- Just (Just (Just 0,Just 0,Just 12,Just 3))
-- >>> map coordinates (fold $ listToMaybe (normalizeWidgetLayouts [groupWidget]) >>= (.children))
-- [Just (Just 0,Just 0,Just 8,Just 1),Just (Just 0,Just 1,Just 8,Just 1)]
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
          width = min 12 $ max 1 $ fromMaybe 1 layout.w
          normalizedChildren = normalizeWidgetLayouts $ fromMaybe [] widget.children
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
          widget' = normalizedWidget{layout = Just Layout{x = Just placedX, y = Just placedY, w = Just width, h = Just height}}
       in (rect : occupied, (idx, widget') : acc)

    isFree occupied width height (y, x) = all (not . overlaps (x, y, width, height)) occupied
    overlaps (x1, y1, w1, h1) (x2, y2, w2, h2) = x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2


layoutRows :: [Widget] -> Int
layoutRows = foldl' (\rows widget -> max rows $ fromMaybe 0 (widget.layout >>= (.y)) + fromMaybe 1 (widget.layout >>= (.h))) 0


widgetHeightForWidth :: Int -> Widget -> Int
widgetHeightForWidth width widget = case widget.wType of
  WTGroup ->
    let requiredHeight = 1 + max 1 (layoutRows $ fromMaybe [] widget.children)
     in if width == 12 then requiredHeight else maybe requiredHeight (max requiredHeight) (widget.layout >>= (.h))
  _ -> max 1 $ fromMaybe 1 $ widget.layout >>= (.h)


gridRem :: Int -> Text
gridRem value = show (value * 5) <> "rem"


gridPercent :: Int -> Text
gridPercent value = toText (printf "%.10f%%" ((fromIntegral value :: Double) * 100 / 12) :: String)


gridStackHandleClassFor :: Widget -> Text
gridStackHandleClassFor w = bool "grid-stack-handle" "nested-grid-stack-handle" (isTrue w._isNested)


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


displayUnit :: Text -> Text
displayUnit = \case
  "" -> ""
  "1" -> ""
  "{}" -> ""
  "By" -> " bytes"
  u -> " " <> u


renderWidgetHeader :: Widget -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Text, Text) -> Html ()
renderWidgetHeader widget valueM subValueM expandBtnFn ctaM = div_ [class_ $ "leading-none flex justify-between items-center  " <> bool "grid-stack-handle" "" (isTrue widget.standalone), id_ $ wId <> "_header"] do
  when (isTrue widget._centerTitle) $ div_ ""
  div_ [class_ "inline-flex gap-3 items-center group/h min-w-0"] do
    span_ [class_ "text-sm text-textStrong font-semibold flex items-center gap-1 min-w-0"] do
      unless (isTrue widget.standalone) $ span_ [class_ "hidden group-hover/h:inline-flex"] $ Utils.faSprite_ "grip-dots-vertical" "regular" "w-4 h-4"
      whenJust widget.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-4 h-4"
      span_ ([class_ "flex min-w-0 overflow-hidden", title_ $ maybeToMonoid widget.title] <> varTemplateAttr widget.title) $ renderDottedTitle $ maybeToMonoid widget.title
      descIcon_ widget.description ""
    span_ [class_ $ "bg-fillWeak border border-strokeWeak text-sm font-semibold px-2 py-1 rounded-3xl leading-none text-textWeak max-md:hidden whitespace-nowrap " <> if isJust valueM then "" else "hidden", id_ $ wId <> "Value"]
      $ whenJust valueM toHtml
    span_ ([class_ $ "text-textDisabled widget-subtitle text-sm max-md:hidden " <> bool "" "hidden" (isTrue widget.hideSubtitle), id_ $ wId <> "Subtitle"] <> varTemplateAttr subValueM) $ toHtml $ maybeToMonoid subValueM
    -- Add hidden loader with specific ID that can be toggled from JS
    span_ [class_ "hidden", id_ $ wId <> "_loader"] $ Utils.faSprite_ "spinner" "regular" "w-4 h-4 animate-spin"
  div_ [class_ "text-iconNeutral flex items-center gap-0.5"] do
    -- Alert status indicator (visible on hover, always visible when alerting/warning)
    when (isJust widget.alertId)
      $ let (iconColor, iconType, tooltip, visibilityClass) = case widget.alertStatus of
              Just "alerting" -> ("text-fillError-strong", "bell-exclamation", "Monitor triggered", "")
              Just "warning" -> ("text-fillWarning-strong", "bell", "Warning threshold exceeded", "")
              _ -> ("text-iconNeutral", "bell", "Monitor configured", "opacity-0 group-hover/wgt:opacity-100 touch:opacity-50")
         in span_
              [ class_ $ "p-1 transition-opacity " <> visibilityClass
              , data_ "tippy-content" tooltip
              , id_ $ wId <> "_alert_indicator"
              ]
              $ Utils.faSprite_ iconType "regular" ("w-3.5 h-3.5 " <> iconColor)

    whenJust ctaM \(ctaTitle, uri) -> a_ [class_ "underline underline-offset-2 text-textBrand", href_ uri] $ toHtml ctaTitle
    whenJust expandBtnFn \url ->
      button_
        ( Utils.drawerLoadAttrs_ url
            <> maybe [] (pure . hxPushUrl_) widget.expandPushUrl
            <> [ class_ "p-2 cursor-pointer tap-target"
               , data_ "tippy-content" "Expand widget"
               ]
        )
        $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    when (isJust widget._dashboardId)
      $ let pid = projectIdText widget
            dashId = maybeToMonoid widget._dashboardId
         in button_
              [ class_ "p-2 cursor-pointer opacity-0 group-hover/wgt:opacity-100 touch:opacity-100 tap-target transition-opacity"
              , title_ "Expand widget"
              , data_ "tippy-content" "Expand widget"
              , data_ "expand-btn" wId
              , term
                  "_"
                  [text| on pointerdown or click
            add .pointer-events-none to me
            set :icon to my.querySelector('svg')
            if :icon then add .animate-spin to :icon end
            js { const url = new URL(window.location); url.searchParams.set('expand', '${wId}'); history.replaceState({}, '', url); } end
            set #global-data-drawer.checked to true
            then set #global-data-drawer-content.innerHTML to #loader-tmp.innerHTML
            then fetch `/p/${pid}/dashboards/${dashId}/widgets/${wId}/expand`
            then if :icon then remove .animate-spin from :icon end
            then remove .pointer-events-none from me
            then set #global-data-drawer-content.innerHTML to it
            then htmx.process(#global-data-drawer-content)
            then _hyperscript.processNode(#global-data-drawer-content)
            then window.evalScriptsFromContent(#global-data-drawer-content)
         |]
              ]
              $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    let widgetMenuPop = wId <> "-widget-menu"
    div_ [class_ "inline-block"] do
      button_ ([type_ "button", class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg tap-target", Aria.label_ "Widget menu", data_ "tippy-content" "Widget Menu"] <> Utils.popoverTrigger_ widgetMenuPop)
        $ Utils.faSprite_ "ellipsis" "regular" "w-4 h-4"
      ul_ ([class_ "dropdown text-textStrong dropdown-end menu menu-md bg-bgRaised rounded-box p-2 w-52 shadow-lg leading-none border border-strokeWeak"] <> Utils.popoverPanel_ widgetMenuPop) do
        -- Only show the "Move to dashboard" option if we're in a dashboard context

        let dashId = maybeToMonoid widget._dashboardId
        li_
          $ a_
            [ class_ "p-2 w-full text-left block cursor-pointer"
            , data_ "tippy-content" "Copy this widget to another dashboard"
            , id_ $ wId <> "_copy_link"
            , term
                "_"
                [text|
              on click
              set #dashboards-modal-widget-id.value to "${wId}"
              then set #dashboards-modal-source-dashboard-id.value to "${dashId}"
              then set #dashboards-modal.checked to true
              then trigger loadDashboards on #dashboards-modal-content
              then call (the closest <[popover]/>).hidePopover()
            |]
            ]
            "Copy to dashboard"
        let copyItem tip label expr =
              li_
                $ a_
                  [ class_ "p-2 w-full text-left block cursor-pointer"
                  , data_ "tippy-content" tip
                  , term
                      "_"
                      [text|
                on click
                set widgetEl to the closest <[data-widget]/>
                set widgetData to JSON.parse(widgetEl.dataset.widget)
                set txt to ${expr}
                if 'clipboard' in window.navigator then
                  call navigator.clipboard.writeText(txt)
                  send successToast(value:['${label} copied to clipboard']) to <body/>
                end
              |]
                  ]
                  (toHtml $ "Copy " <> label)
        let pid = projectIdText widget
        li_
          $ a_
            [ class_ "p-2 w-full text-left block cursor-pointer"
            , data_ "tippy-content" "Copy generated SQL to clipboard"
            , term
                "_"
                [text|
              on click
              set widgetEl to the closest <[data-widget]/>
              set widgetData to JSON.parse(widgetEl.dataset.widget)
              set txt to widgetData.sql
              if not txt and widgetData.query then
                fetch ('/p/${pid}/widget/sql-text?query=' + encodeURIComponent(widgetData.query)) as text
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
        copyItem "Copy KQL query to clipboard" "KQL" "widgetData.query or 'No KQL available'"
        whenJust widget.pngUrl \url ->
          li_
            $ a_
              [ class_ "p-2 w-full text-left block cursor-pointer"
              , data_ "tippy-content" "Download widget as PNG image"
              , href_ url
              , download_ $ maybeToMonoid widget.title <> ".png"
              , target_ "_blank"
              ]
              "Download PNG"

        -- Only show the "Duplicate widget" option if we're in a dashboard context
        when (isJust widget._dashboardId) do
          li_
            $ a_
              [ class_ "p-2 w-full text-left block cursor-pointer"
              , data_ "tippy-content" "Create a copy of this widget"
              , hxPost_ ("/p/" <> projectIdText widget <> "/dashboards/" <> dashId <> "/widgets/" <> wId <> "/duplicate")
              , hxTrigger_ "click"
              , hxTarget_ "closest .grid-stack"
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
              "Duplicate widget"
          li_
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


-- | Shared card-shell for renderLogsWidget / renderTraceTable / renderTable:
-- outer flex-col wrapper, optional widget header, flex-1 fill, surface-raised
-- card with @id="<wid>_bordered"@.  When @flexCol@ is True the inner card adds
-- @flex flex-col@. Pass @extraAction@ to attach an action button to the header.
withCardFrame :: Bool -> Widget -> Maybe (Text, Text) -> Html () -> Html ()
withCardFrame flexCol widget extraAction body = do
  let naked = isTrue widget.naked
  div_ [class_ "gap-0.5 flex flex-col h-full"] do
    unless naked $ renderWidgetHeader widget Nothing Nothing Nothing extraAction
    div_ [class_ "flex-1 flex min-h-0"]
      $ div_
        [ class_ $ "h-full w-full " <> bool "" "flex flex-col " flexCol <> bool "surface-raised rounded-2xl" "" naked
        , id_ $ maybeToMonoid widget.id <> "_bordered"
        ]
        body


renderLogsWidget :: Widget -> Html ()
renderLogsWidget widget = do
  let wId = maybeToMonoid widget.id
      pid = projectIdText widget
      queryParam = maybe "" (\q -> "&query=" <> decodeUtf8 (urlEncode True (encodeUtf8 q))) widget.query
      fetchUrl = "/p/" <> pid <> "/log_explorer/data?json=true&layout=1" <> queryParam
      action = Just ("Open in Explorer", "/p/" <> pid <> "/log_explorer" <> maybe "" ("?query=" <>) widget.query)
  withCardFrame False widget action
    $ termRaw
      "log-list"
      [ id_ wId
      , class_ "w-full flex flex-col h-full min-w-0"
      , term "projectId" pid
      , term "initialFetchUrl" fetchUrl
      ]
      ("" :: Text)


-- | Shared sticky table @thead@ with sortable column headers. Each column is a
-- (title, optional align-class) pair. When @sortable@ is True the @th@ gets the
-- @window.sortTable@ onclick + a hover sort-arrow; otherwise it's a plain header.
sortableTableHead_ :: Text -> Bool -> [(Text, Maybe Text)] -> Html ()
sortableTableHead_ tableId sortable cols =
  thead_ [class_ "sticky top-0 z-10 before:content-[''] before:absolute before:left-0 before:right-0 before:bottom-0 before:h-px before:bg-strokeWeak"]
    $ tr_ []
    $ ifor_ cols \idx (title, align) ->
      th_
        ( [ class_ $ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group " <> fromMaybe "" align
          , data_ "sort-direction" "none"
          ]
            <> [onclick_ $ "window.sortTable('" <> tableId <> "', " <> show (idx :: Int) <> ", this)" | sortable]
        )
        $ headerRow_ [] do
          toHtml title
          when sortable $ span_ [class_ "sort-arrow ml-1 text-iconNeutral opacity-0 group-hover:opacity-100", data_ "sort" "none"] "↕"


renderTraceTable :: Widget -> Html ()
renderTraceTable widget = renderTableShell widget [(c, Nothing) | c <- ["Resource", "Span name", "Duration", "Latency breakdown"] :: [Text]] []


renderTable :: Widget -> Html ()
renderTable widget = renderTableShell widget [(col.title, col.align) | col <- fromMaybe [] widget.columns] (rowClickTableAttrs widget)


-- | Shared eager-fetch shell for renderTable / renderTraceTable: HTMX-loaded
-- card whose body is either the already-rendered @widget.html@ or a loading
-- table whose @thead@ uses the given column headers (+ optional extra @table@
-- attrs, e.g. row-click data attributes).
renderTableShell :: Widget -> [(Text, Maybe Text)] -> [Attribute] -> Html ()
renderTableShell widget headerCols tableAttrs = do
  let tableId = maybeToMonoid widget.id
      eagerWidget = widget & #eager ?~ True & #pngUrl .~ Nothing & #html .~ Nothing & #dataset .~ Nothing
  withCardFrame True widget Nothing
    $ div_
      [ class_ "h-full overflow-auto p-3"
      , hxGet_ $ widgetFetchUrl eagerWidget
      , hxTrigger_ "intersect once, update-query from:window"
      , hxTarget_ $ "#" <> tableId
      , hxSelect_ $ "#" <> tableId
      , hxSwap_ "outerHTML"
      , hxExt_ "forward-page-params"
      ]
    $ case widget.html of
      Just html -> toHtmlRaw html
      Nothing -> table_ ([class_ "table table-zebra table-sm w-full relative", id_ tableId] <> tableAttrs) do
        sortableTableHead_ tableId True headerCols
        tbody_ []
          $ tr_ []
          $ td_ [colspan_ "100", class_ "text-center py-8"]
          $ loadingIndicator_ LdSM LdSpinner


-- | Render stat widget content with HTMX lazy loading support
-- Always includes HTMX attributes so widget can refresh on update-query events
renderStatContent :: Widget -> Maybe Text -> Html ()
renderStatContent widget valueM = do
  let chartId = maybeToMonoid widget.id
      statContentId = chartId <> "_stat"
      hasData = isTrue widget.eager || isJust (widget.dataset >>= (.value))
  div_
    [ id_ statContentId
    , class_ $ "px-3 flex flex-col " <> bool "py-3 " "py-2 " (isTrue widget._isNested)
    , hxGet_ $ widgetFetchUrl (widget & #eager ?~ True)
    , hxTrigger_ $ bool "intersect once, update-query from:window" "update-query from:window" hasData
    , hxTarget_ $ "#" <> statContentId
    , hxSelect_ $ "#" <> statContentId
    , hxSwap_ "outerHTML"
    , hxExt_ "forward-page-params"
    ]
    $ div_ [class_ "flex flex-col gap-1"] do
      strong_ [class_ "text-textStrong text-4xl font-bold tabular-nums", id_ $ chartId <> "Value"]
        $ if hasData then whenJust valueM toHtml else loadingIndicator_ LdSM LdSpinner
      div_ [class_ "inline-flex gap-1 items-center text-sm"] do
        whenJust widget.icon \icon -> Utils.faSprite_ icon "regular" "w-4 h-4 text-iconBrand"
        toHtml $ maybeToMonoid widget.title
        descIcon_ widget.description " text-iconNeutral"


renderChart :: Widget -> Html ()
renderChart widget = do
  let rateM = widget.dataset >>= (.rowsPerMin) <&> \r -> Utils.prettyPrintCount (round r) <> "/min"
      chartId = maybeToMonoid widget.id
      unitSuffix = foldMap displayUnit widget.unit
      valueM = widget.dataset >>= (.value) <&> \x -> Utils.prettyPrintCount (round x) <> unitSuffix
      isStat = widget.wType `elem` [WTTimeseriesStat, WTStat]
  div_ [class_ "gap-0.5 flex flex-col h-full justify-end "] do
    unless (isTrue widget.naked || isStat)
      $ renderWidgetHeader widget valueM rateM widget.expandBtnFn Nothing
    div_ [class_ $ "flex-1 flex min-h-0 " <> bool "" (gridStackHandleClassFor widget) isStat] do
      div_
        [ class_
            $ "h-full w-full flex flex-col justify-end "
            <> bool "min-h-0 " "" isStat
            <> if isTrue widget.naked then "" else "surface-raised rounded-2xl relative"
        , id_ $ chartId <> "_bordered"
        ]
        do
          -- Failure banner: one line at the top of the card, above the content,
          -- rather than a centred overlay printed over the chart it describes.
          -- widgets.ts fills the message and unhides it.
          div_ [id_ $ chartId <> "_error", class_ "hidden shrink-0 rounded-t-2xl border-b border-strokeError-weak bg-fillError-weak px-2.5 py-1"]
            $ div_ [class_ "flex items-center gap-1.5 text-xs text-textError"] do
              Utils.faSprite_ "circle-exclamation" "solid" "w-3 h-3 shrink-0"
              span_ [class_ "truncate", id_ $ chartId <> "_errorMsg"] ""
          whenJust ((,) <$> widget.groupByOptions <*> widget.groupByUrl) \(options, url) ->
            let selectedLabel = fromMaybe "All values" $ guarded (/= "all") =<< widget.groupBySelected
             in div_ [class_ "flex shrink-0 justify-end px-2 pt-2"]
                  $ details_ [class_ "dropdown dropdown-end relative max-w-[calc(100%-1rem)]"] do
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
                                , hxSwap_ "outerHTML"
                                ]
                              $ toHtml label
                      item "all" "All values"
                      forM_ options \label -> item label label
          when isStat $ renderStatContent widget valueM
          unless (widget.wType == WTStat) $ div_ [class_ $ "h-0 max-h-full overflow-hidden w-full flex-1 min-h-0" <> bool " p-2" "" isStat] do
            div_ [class_ "chart-render-slot h-full min-h-full w-full", id_ chartId, data_ "chart-widget" ""] ""
            let sumBy = fromMaybe SBSum widget.summarizeBy
                theme = fromMaybe "default" widget.theme
                echartOpt = encodeText $ widgetToECharts widget
                yAxisLabel = fromMaybe (maybeToMonoid widget.unit) (widget.yAxis >>= (.label))
                query = encodeText widget.query
                pid = encodeText $ widget._projectId <&> (.toText)
                querySQL = maybeToMonoid widget.sql
                chartType = mapWidgetTypeToChartType widget.wType
                summarizeBy = T.toLower $ T.drop 2 $ show sumBy
                summarizeByPfx = summarizeByPrefix sumBy
                wType = encodeText widget.wType
                legendPos = fromMaybe "bottom" widget.legendPosition
                widgetUnit = maybeToMonoid widget.unit
                alertThresholdJS = maybe "null" show widget.alertThreshold
                warningThresholdJS = maybe "null" show widget.warningThreshold
                -- Mirrors chartWidget's `!opt.dataset.source` test: a widget with eager
                -- server data never calls /chart_data, so prefetching one would be a
                -- wasted request. See prefetchChartData in web-components/src/widgets.ts.
                willFetchJS = if maybe True ((== AE.Null) . (.source)) widget.dataset then "true" else "false" :: Text
            script_
              [type_ "text/javascript"]
              [text|

              // IIFE to avoid global variable conflicts
              (function() {
                // Configuration for this specific widget
                const config = {
                  chartId: "${chartId}",
                  echartOpt: `${echartOpt}`,
                  chartType: '${chartType}',
                  widgetType: ${wType},
                  query: ${query},
                  querySQL: `${querySQL}`,
                  theme: "${theme}",
                  yAxisLabel: "${yAxisLabel}",
                  pid: ${pid},
                  summarizeBy: '${summarizeBy}',
                  summarizeByPrefix: '${summarizeByPfx}',
                  legendPosition: "${legendPos}",
                  unit: "${widgetUnit}",
                  alertThreshold: ${alertThresholdJS},
                  warningThreshold: ${warningThresholdJS}
                };

                // Start the data request during HTML parse rather than after echarts,
                // the stagger queue and chart construction (~2.2s later on the log
                // explorer). widgets.ts drains this and swaps in a live prefetcher.
                if (${willFetchJS}) (window.__chartPrefetch = window.__chartPrefetch || []).push(config);

                function parseAndInit() {
                  const echartOpt = JSON.parse(config.echartOpt, (key, value) => {
                    if (typeof value === 'string' && value.trim().startsWith("function(")) {
                      try { return eval('(' + value + ')'); }
                      catch (e) { return value; }
                    }
                    return value;
                  });
                  const chartEl = document.getElementById(config.chartId);
                  if (!chartEl) return;
                  const existing = window.echarts && window.echarts.getInstanceByDom(chartEl);
                  if (existing) existing.dispose();
                  window.bindFunctionsToObjects(echartOpt, echartOpt);
                  window.chartWidget({ ...config, opt: echartOpt });
                }

                // Stagger initialization via queue to avoid blocking main thread
                function initializeThisWidget() {
                  if (!window.widgetDepsReady) {
                    if (document.readyState === 'loading') {
                      document.addEventListener('DOMContentLoaded', initializeThisWidget, { once: true });
                    } else {
                      setTimeout(initializeThisWidget, 50);
                    }
                    return;
                  }
                  if (window.queueChartInit) window.queueChartInit(parseAndInit, config.chartId);
                  else parseAndInit();
                }

                if (document.readyState === 'loading') {
                  window.addEventListener('DOMContentLoaded', initializeThisWidget);
                } else {
                  initializeThisWidget();
                }

              })();
            
            |]


-----------------------------------------------------------------------------
-- Echarts Logic
-----------------------------------------------------------------------------

-- Helper: Extract series names from dataset source (headers[1:])
extractSeriesNamesFromDataset :: Maybe WidgetDataset -> [Text]
extractSeriesNamesFromDataset ds = case ds <&> (.source) of
  Just (AE.Array arr) | Just (AE.Array headers) <- arr V.!? 0 -> mapMaybe (\case AE.String t -> Just t; _ -> Nothing) $ drop 1 $ V.toList headers
  _ -> []


-- | JS expression formatting a numeric @value@ for a widget's unit: duration
-- units convert+format, everything else uses formatNumber. Shared by the
-- tooltip valueFormatter and the yAxis axisLabel formatter.
unitValueExprJS :: Maybe Text -> Text
unitValueExprJS unitM = case guarded (`elem` ["ns", "μs", "us", "ms", "s", "m", "h"]) =<< unitM of
  Just u -> "formatDuration(convertToNanoseconds(value, '" <> u <> "'))"
  Nothing -> "formatNumber(value)"


-- Function to convert Widget to ECharts options
widgetToECharts :: Widget -> AE.Value
widgetToECharts widget =
  let isStat = widget.wType == WTTimeseriesStat
      axisVisibility = not isStat
      legendVisibility = not isStat && not (isTrue widget.hideLegend)
      seriesNames = extractSeriesNamesFromDataset widget.dataset
      -- Detect categorical widget types (no time axis)
      isCategorical = widget.wType `elem` [WTDistribution, WTPieChart, WTTopList, WTTreeMap, WTFunnel]
      xAxisType = if isCategorical then "category" else "time"
   in AE.object
        [ "tooltip"
            AE..= AE.object
              [ "show" AE..= fromMaybe True widget.showTooltip
              , "trigger" AE..= ("axis" :: Text)
              , "appendToBody" AE..= True
              , "axisPointer"
                  AE..= AE.object
                    ["type" AE..= ("shadow" :: Text)]
              , "valueFormatter"
                  AE..= ("function(value) { return " <> unitValueExprJS widget.unit <> "; }")
              ]
        , "legend"
            AE..= AE.object
              ( let pos = fromMaybe "bottom" widget.legendPosition
                    (vPos, hPos) = case T.splitOn "-" pos of
                      [v, h] | h == "right" || h == "left" -> (v, Just h)
                      [v] -> (v, Nothing)
                      _ -> ("bottom", Nothing)
                    (fontSize, itemSize, itemGap, pad) = case fromMaybe "sm" widget.legendSize of
                      "xs" -> (10 :: Int, 6 :: Int, 6 :: Int, [2, 4, 2, 4] :: [Int])
                      "md" -> (14, 12, 12, [4, 8, 4, 8])
                      "lg" -> (16, 14, 14, [5, 10, 5, 10])
                      _ -> (12, 9, 9, [3, 6, 3, 6]) -- sm (default)
                    isStatic = isTrue widget._staticRender
                    legendOffset = if vPos == "top" then ["top" AE..= (0 :: Int)] else ["bottom" AE..= (2 :: Int)]
                 in [ "show" AE..= legendVisibility
                    , "type" AE..= if isStatic then "plain" else "scroll"
                    , "textStyle" AE..= AE.object ["fontSize" AE..= AE.Number (fromIntegral fontSize), "padding" AE..= AE.Array [AE.Number 0, AE.Number 0, AE.Number 0, AE.Number (-2)]]
                    , "itemWidth" AE..= AE.Number (fromIntegral itemSize)
                    , "itemHeight" AE..= AE.Number (fromIntegral itemSize)
                    , "itemGap" AE..= AE.Number (fromIntegral itemGap)
                    , "padding" AE..= AE.Array (V.fromList $ map (AE.Number . fromIntegral) pad)
                    , "data" AE..= maybe seriesNames (map (fromMaybe "Unnamed Series" . (.query))) widget.queries -- Use series names from dataset if no explicit queries
                    ]
                      <> legendOffset
                      <> [K.fromText h AE..= (0 :: Int) | Just h <- [hPos]]
              )
        , "grid"
            AE..= AE.object
              [ "width" AE..= ("100%" :: Text)
              , "left" AE..= ("0%" :: Text)
              , "top" AE..= if maybe False (T.isPrefixOf "top") widget.legendPosition && legendVisibility then (28 :: Int) else if isTrue widget.naked then (16 :: Int) else (8 :: Int)
              , "bottom" AE..= if not (maybe False (T.isPrefixOf "top") widget.legendPosition) && legendVisibility then (36 :: Int) else (8 :: Int)
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
                , "axisLabel" AE..= AE.object ["show" AE..= (axisVisibility && fromMaybe True (widget ^? #xAxis . _Just . #showAxisLabel . _Just)), "margin" AE..= (8 :: Int), "hideOverlap" AE..= True]
                , "show" AE..= (axisVisibility || fromMaybe False (widget ^? #xAxis . _Just . #showAxisLabel . _Just))
                ]
                  <> if isCategorical
                    then [] -- For categorical, ECharts will derive categories from dataset
                    else
                      [ "min" AE..= maybe AE.Null (AE.Number . fromIntegral) (widget ^? #dataset . _Just . #from . _Just)
                      , "max" AE..= maybe AE.Null (AE.Number . fromIntegral) (widget ^? #dataset . _Just . #to . _Just)
                      ]
              )
        , "yAxis"
            AE..= AE.object
              [ "type" AE..= ("value" :: Text)
              , "min" AE..= (0 :: Int)
              , "max" AE..= maybe AE.Null (AE.Number . fromFloatDigits) (widget ^? #dataset . _Just . #stats . _Just . #maxGroupSum)
              , "splitLine"
                  AE..= AE.object
                    [ "show" AE..= axisVisibility
                    , "lineStyle" AE..= AE.object ["type" AE..= "dotted", "color" AE..= "#0011661A"]
                    , "interval"
                        AE..= if fromMaybe False $ widget ^? #yAxis . _Just . #showOnlyMaxLabel . _Just
                          then "function(index, value) { return value === this.yAxis.max }"
                          else AE.Null
                    ]
              , "axisTick" AE..= AE.object ["show" AE..= False]
              , "axisLine" AE..= AE.object ["show" AE..= False]
              , "axisLabel"
                  AE..= AE.object
                    [ "show" AE..= (axisVisibility && fromMaybe True (widget ^? #yAxis . _Just . #showAxisLabel . _Just))
                    , "inside" AE..= False
                    , "margin" AE..= (8 :: Int)
                    , "hideOverlap" AE..= True
                    , "formatter"
                        AE..= let fmt = unitValueExprJS widget.unit
                                  showOnlyMax = fromMaybe False $ widget ^? #yAxis . _Just . #showOnlyMaxLabel . _Just
                               in if showOnlyMax
                                    then "function(value, index) { return (value === this.yAxis.max || value == 0) ? " <> fmt <> " : ''; }"
                                    else "function(value, index) { return " <> fmt <> "; }"
                    ]
              , "show" AE..= axisVisibility
              ]
        , "dataset"
            AE..= AE.object
              ["source" AE..= maybe AE.Null (.source) widget.dataset]
        , "series" AE..= addMarkLinesToFirstSeries widget (zipWith (createSeries widget.wType) [1 ..] seriesNames)
        , "animation" AE..= False
        , if isTrue widget.allowZoom
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
    shouldShowLines = case widget.showThresholdLines of
      Just "never" -> False
      Just "on_breach" -> widget.alertStatus == Just "warning" || widget.alertStatus == Just "alerting"
      _ -> isJust widget.alertThreshold || isJust widget.warningThreshold

    mkMarkLine color label threshold =
      AE.object
        [ "yAxis" AE..= threshold
        , "lineStyle" AE..= AE.object ["color" AE..= (color :: Text), "type" AE..= ("dashed" :: Text), "width" AE..= (2 :: Int)]
        , "label" AE..= AE.object ["formatter" AE..= ((label <> ": {c}") :: Text), "position" AE..= ("insideEndTop" :: Text)]
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
        , "itemStyle" AE..= AE.object ["color" AE..= seriesColor, "borderRadius" AE..= AE.Array [AE.Number 2, AE.Number 2, AE.Number 0, AE.Number 0]]
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
mapWidgetTypeToChartType WTTimeseries = "bar"
mapWidgetTypeToChartType WTTimeseriesLine = "line"
mapWidgetTypeToChartType WTTimeseriesStat = "line"
mapWidgetTypeToChartType WTDistribution = "bar"
mapWidgetTypeToChartType WTServiceMap = "graph" -- ECharts force-directed graph
mapWidgetTypeToChartType WTHeatmap = "heatmap" -- ECharts heatmap
mapWidgetTypeToChartType _ = "bar"


mapChatTypeToWidgetType :: Text -> WidgetType
mapChatTypeToWidgetType "line" = WTTimeseriesLine
mapChatTypeToWidgetType "timeseries_line" = WTTimeseriesLine
mapChatTypeToWidgetType _ = WTTimeseries


renderTableWithDataAndParams :: Widget -> V.Vector (V.Vector Text) -> [(Text, Maybe Text)] -> Html ()
renderTableWithDataAndParams widget dataRows params = do
  let columns = fromMaybe [] widget.columns
      tableId = maybeToMonoid widget.id
      currentVar = widget.onRowClick >>= (.setVariable) >>= \var -> find ((== "var-" <> var) . fst) params >>= snd

  table_
    ( [class_ "table table-zebra table-sm w-full relative", id_ tableId]
        <> rowClickTableAttrs widget
    )
    do
      sortableTableHead_ tableId True [(col.title, col.align) | col <- columns]
      tbody_ [] do
        let maxValues = calculateMaxValues columns dataRows
            -- max formatted width per progress column, so bars line up
            valueWidths =
              M.fromList
                [ (col.field, V.foldl' (\acc row -> max acc (T.length $ formatColumnValue col (getRowValue idx row))) 5 dataRows)
                | (col, idx) <- zip columns [0 ..]
                , isJust col.progress
                ]
        V.forM_ dataRows \row -> do
          let firstColValue = memptyIfFalse (not $ null columns) (getRowValue 0 row)
              rowValue = maybe firstColValue (T.replace "{{row.resource_name}}" firstColValue) (widget.onRowClick >>= (.value))
          tr_
            [ class_ $ "hover cursor-pointer" <> memptyIfFalse (Just rowValue == currentVar) " bg-fillBrand-strong/20 border-l-4 border-strokeBrand-strong"
            , data_ "row" $ encodeText $ AE.object [(K.fromText col.field, AE.String $ getRowValue idx row) | (col, idx) <- zip columns [0 ..]]
            ]
            $ ifor_ columns \idx col ->
              td_ [class_ $ cellClass col]
                $ if isJust col.progress
                  then renderProgressCell col (getRowValue idx row) maxValues valueWidths
                  else renderLongTextOr col (getRowValue idx row)


renderTraceDataTable :: Widget -> V.Vector (V.Vector Text) -> HashMap Text [(Text, Int, Int)] -> HashMap Text [Telemetry.SpanRecord] -> Text -> Html ()
renderTraceDataTable widget dataRows spGroup spansGrouped colorsJson = do
  let columns = fromMaybe [] widget.columns
      tableId = maybeToMonoid widget.id
  table_ [class_ "table table-sm w-full relative", id_ tableId] do
    sortableTableHead_ tableId False [(col.title, col.align) | col <- columns]
    tbody_ [] do
      V.forM_ dataRows \row -> do
        let val = getRowValue (V.length row - 1) row
            cdrn = fromMaybe [] $ HM.lookup val spGroup
            spjson = encodeText $ fromMaybe [] (HM.lookup val spansGrouped) <&> \x -> getSpanJson ((\(_, d, c) -> (d, c)) <$> find (\(n, _, _) -> n == x.spanName) cdrn) x
            clcFun = [text|on click toggle .hidden on the next <tr/> then call flameGraphChart($spjson, "$val", $colorsJson)|]
        tr_ [term "_" clcFun, class_ "cursor-pointer"]
          $ ifor_ columns \idx col ->
            if col.field == "latency_breakdown"
              then td_ [class_ "py-2"] $ renderLatencyBreakdown cdrn
              else td_ [class_ $ cellClass col] $ toHtml $ formatColumnValue col (getRowValue idx row)
        tr_ [class_ "hidden"] do
          td_ [colspan_ "100%"] do
            when (isJust widget._projectId)
              $ div_
                [ class_ "w-full group px-2 pt-4 border relative flex flex-col rounded-lg overflow-hidden"
                , id_ $ "flame-graph-container-" <> val
                ]
              $ renderFlameGraph val


getSpanJson :: Maybe (Int, Int) -> Telemetry.SpanRecord -> AE.Value
getSpanJson tgtM sp =
  AE.object
    [ "spanId" AE..= sp.spanId
    , "name" AE..= sp.spanName
    , "value" AE..= maybe sp.spanDurationNs (\(d, _) -> fromIntegral d) tgtM
    , "start" AE..= utcTimeToNanoseconds sp.startTime
    , "parentId" AE..= sp.parentSpanId
    , "serviceName" AE..= getServiceName sp.resource
    , "hasErrors" AE..= spanHasErrors sp
    , "totalSpans" AE..= maybe 1 snd tgtM
    ]


renderFlameGraph :: Text -> Html ()
renderFlameGraph trId = do
  div_ [class_ "w-full sticky top-0 border-b border-b-strokeWeak h-6 text-xs relative", id_ $ "time-container-" <> trId] pass
  div_ [class_ "w-full overflow-x-hidden min-h-56 h-full relative", id_ $ "a" <> trId] pass
  div_ [class_ "h-full top-0  absolute z-50 hidden", id_ $ "time-bar-indicator-" <> trId] do
    div_ [class_ "relative h-full"] do
      div_ [class_ "text-xs top-[-18px] absolute -translate-x-1/2 whitespace-nowrap", id_ $ "line-time-" <> trId] "2 ms"
      div_ [class_ "h-[calc(100%-24px)] mt-[24px] w-[1px] bg-strokeWeak"] pass


renderLatencyBreakdown :: [(Text, Int, Int)] -> Html ()
renderLatencyBreakdown groups = div_ [class_ "flex h-5 overflow-hidden relative bg-fillWeak rounded", style_ "width:150px"] do
  let durs = [fromIntegral d | (_, d, _) <- groups] :: [Double]
      colors = getServiceColors $ V.fromList [n | (n, _, _) <- groups]
      total = max 1 (sum durs) -- max 1 keeps an all-zero breakdown from producing NaN offsets
      scale d = d / total * 150.0
  forM_ (zip3 groups durs (scanl (+) 0 durs)) \((name, dur, _), d, offset) ->
    div_
      [ class_ $ "h-full absolute top-0 border  " <> fromMaybe "bg-black" (HM.lookup name colors)
      , title_ $ name <> ": " <> show (dur `div` 1000000) <> " ms"
      , style_ $ "width:" <> show (scale d) <> "px;" <> "left:" <> show (scale offset) <> "px;"
      ]
      pass


getRowValue :: Int -> V.Vector Text -> Text
getRowValue idx row = fromMaybe "" $ row V.!? idx


-- Calculate max values for column percentage progress bars
calculateMaxValues :: [TableColumn] -> V.Vector (V.Vector Text) -> M.Map Text Double
calculateMaxValues columns dataRows =
  M.fromList
    [ (col.field, maxVal)
    | (col, idx) <- zip columns [0 ..]
    , col.progress == Just "column_percent"
    , let values = V.mapMaybe (\row -> row V.!? idx >>= readMaybe . toString) dataRows
    , let maxVal = if V.null values then 0 else V.maximum values
    ]


renderProgressCell :: TableColumn -> Text -> M.Map Text Double -> M.Map Text Int -> Html ()
renderProgressCell col value maxValues valueWidths = div_ [class_ "flex items-center gap-2"] do
  let numValue = fromMaybe 0 (readMaybe $ toString value) :: Double
      percentage = case col.progress of
        Just "value_percent" -> min 100 (max 0 numValue)
        Just "column_percent" | Just maxVal <- M.lookup col.field maxValues, maxVal > 0 -> (numValue / maxVal) * 100
        _ -> 0
  span_ [class_ "inline-block text-left monospace", style_ $ "width: " <> show (M.findWithDefault 8 col.field valueWidths) <> "ch"]
    $ toHtml
    $ formatColumnValue col value
  progress_
    [ class_ $ "progress w-12 ml-2 " <> case col.progressVariant of
        Just "error" -> "progress-error"
        Just "warning" -> "progress-warning"
        Just "success" -> "progress-success"
        _ -> "progress-brand"
    , value_ (show percentage)
    , max_ "100"
    ]
    ""


isNumericCol :: TableColumn -> Bool
isNumericCol col = col.columnType `elem` [Just ("number" :: Text), Just "duration"]


-- | @td@ classes: column alignment plus monospace for numeric/duration columns.
cellClass :: TableColumn -> Text
cellClass col = fromMaybe "" col.align <> memptyIfFalse (isNumericCol col) " monospace"


formatColumnValue :: TableColumn -> Text -> Text
formatColumnValue col value = case col.columnType of
  Just "number" -> maybe value fmtNumber (readMaybe (toString value) :: Maybe Double) <> unitSuffix
  Just "duration" -> maybe (value <> unitSuffix) (\v -> toText $ getDurationNSMS (fromIntegral (round v :: Int))) (readMaybe (toString value) :: Maybe Double)
  _ -> fromMaybe value (formatTimestampValue value) <> unitSuffix
  where
    unitSuffix = foldMap (" " <>) col.unit
    -- keep significant digits for small non-integral numbers, pretty-print the rest
    fmtNumber n
      | n < 100, n /= fromIntegral (round n :: Int) = toText (printf "%.2g" n :: String)
      | otherwise = prettyPrintCount (round n)


-- | Try to parse a PostgreSQL timestamp and format as "Mar 19, 09:05"
-- Normalizes short timezone offsets (+00 → +0000) that %z can't parse
formatTimestampValue :: Text -> Maybe Text
formatTimestampValue (T.strip -> val)
  | T.length val < 19 = Nothing
  | otherwise = fmt <$> (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%z" (toString normalized) :: Maybe ZonedTime)
  where
    normalized = case T.breakOnEnd "+" val of
      (pfx, tz) | not (T.null pfx), T.length tz < 4 -> pfx <> tz <> T.replicate (4 - T.length (T.filter isDigit tz)) "0"
      _ -> val
    fmt = toText . formatTime defaultTimeLocale "%b %d, %H:%M"


-- | Render a text cell, parsing summary tags (field;style⇒value) into badges.
-- Long values get a truncated single-line view with a tippy tooltip (delegated
-- body-wide in BodyWrapper) showing the full text on hover. Number/duration
-- columns skip the wrapper since they're always short.
renderLongTextOr :: TableColumn -> Text -> Html ()
renderLongTextOr col value
  | not (isNumericCol col), T.length value > 60 = div_ [class_ "truncate max-w-2xl", term "data-tippy-content" value] cell
  | otherwise = cell
  where
    formatted = formatColumnValue col value
    cell = if T.isInfixOf "⇒" formatted then renderSummaryTags formatted else toHtml formatted
