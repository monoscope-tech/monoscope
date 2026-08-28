-- | Infrastructure inventory views built from the same bounded OpenTelemetry snapshot.
-- Hosts, images, Kubernetes resources, and the host map are projections of that snapshot,
-- so a filter, table row, map cell, and inspector cannot disagree about identity or usage.
module Pages.Infrastructure (
  hostsGetH,
  hostDetailGetH,
  imagesGetH,
  imageDetailGetH,
  kubernetesGetH,
  kubernetesDetailGetH,
  hostMapGetH,
  HostsGet (..),
  ImagesGet (..),
  KubernetesGet (..),
  HostMapGet (..),
  HostDetailGet (..),
  ImageDetailGet (..),
  KubernetesDetailGet (..),
) where

import Data.Default (def)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers (ContainerRow (..), Runtime (..), Scope (..), containersInWindow, cpuPctOfLimit, memPctOfLimit, runtimeOf)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (factGrid_, metaChip_)
import Pages.Containers qualified as Containers
import Pages.LogExplorer.Log qualified as Log
import Pkg.Components.Table (Column, Config (..), Features (..), SearchMode (..), Table (..), ZeroState (..), col, facetActions, facetValues, singleSelectFilter, withAttrs)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Relude
import Relude.Extra.Tuple (dup)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (drawerLoadAttrs_, drawerRowAttrs_, faSprite_, infrastructureNavTabs_, kqlQuoted, toUriStr)


infraUrl :: Projects.ProjectId -> Text -> [(Text, Text)] -> TimePicker.TimeWindow -> Text
infraUrl pid path = TimePicker.windowUrl ("/p/" <> pid.toText <> path)


data InfraIntegration = OpenTelemetryIntegration | KubernetesIntegration | DockerIntegration
  deriving stock (Eq, Ord, Show)


integrationLabel :: InfraIntegration -> Text
integrationLabel = \case
  OpenTelemetryIntegration -> "OpenTelemetry"
  KubernetesIntegration -> "Kubernetes"
  DockerIntegration -> "Docker"


integrationIcon :: InfraIntegration -> Text
integrationIcon = \case
  OpenTelemetryIntegration -> "arrows-turn-right"
  KubernetesIntegration -> "cube"
  DockerIntegration -> "layer-group"


data HostRow = HostRow
  { name :: Text
  , provider :: Maybe Text
  , region :: Maybe Text
  , osType :: Maybe Text
  , architecture :: Maybe Text
  , cpuPct :: Maybe Double
  , memoryPct :: Maybe Double
  , storagePct :: Maybe Double
  , load1 :: Maybe Double
  , uptime :: Maybe Double
  , containers :: Int
  , integrations :: [InfraIntegration]
  }
  deriving stock (Eq, Show)


hostsFromRows :: V.Vector ContainerRow -> V.Vector HostRow
hostsFromRows = V.fromList . map build . M.toAscList . V.foldl' add M.empty
  where
    add acc r = maybe acc (\n -> M.insertWith (<>) n [r] acc) r.nodeName
    build (name, rows) =
      HostRow
        { name
        , provider = firstJust (.provider) rows
        , region = firstJust (.region) rows
        , osType = firstJust (.osType) rows
        , architecture = firstJust (.architecture) rows
        , cpuPct = hostValue cpuPctOfLimit rows
        , memoryPct = hostValue memPctOfLimit rows
        , storagePct = hostValue (.storagePct) rows
        , load1 = hostValue (.load1) rows
        , uptime = hostValue (.uptime) rows
        , containers = length [() | r <- rows, r.scope /= ScopeHost]
        , integrations =
            [OpenTelemetryIntegration]
              <> [KubernetesIntegration | any ((== Kubernetes) . runtimeOf) rows]
              <> [DockerIntegration | any ((== Docker) . runtimeOf) rows]
        }
    firstJust f = listToMaybe . mapMaybe f
    hostValue f = listToMaybe . mapMaybe (\r -> if r.scope == ScopeHost then f r else Nothing)


data HostFilters = HostFilters
  { provider :: Maybe Text
  , region :: Maybe Text
  , osType :: Maybe Text
  , integration :: Maybe Text
  }


applyHostFilters :: HostFilters -> V.Vector HostRow -> V.Vector HostRow
applyHostFilters f = V.filter \h ->
  matches f.provider h.provider
    && matches f.region h.region
    && matches f.osType h.osType
    && maybe True (\v -> T.null v || any ((== v) . integrationLabel) h.integrations) f.integration
  where
    matches selected actual = maybe True (\v -> T.null v || Just v == actual) selected


data HostGroup = GroupNone | GroupProvider | GroupRegion | GroupOS | GroupIntegration
  deriving stock (Eq, Show)


parseHostGroup :: Maybe Text -> HostGroup
parseHostGroup = \case
  Just "provider" -> GroupProvider
  Just "region" -> GroupRegion
  Just "os" -> GroupOS
  Just "integration" -> GroupIntegration
  Just _ -> GroupNone
  Nothing -> GroupNone


hostGroupParam :: HostGroup -> Text
hostGroupParam = \case
  GroupNone -> ""
  GroupProvider -> "provider"
  GroupRegion -> "region"
  GroupOS -> "os"
  GroupIntegration -> "integration"


data HostListRow = HostGroupRow Text Int | HostItem HostRow


hostEntries :: HostGroup -> V.Vector HostRow -> V.Vector HostListRow
hostEntries GroupNone = V.map HostItem
hostEntries grouping = V.fromList . concatMap renderGroup . M.toAscList . V.foldl' add M.empty
  where
    add acc host = M.insertWith (<>) (groupValue host) [host] acc
    renderGroup (label, hosts) = HostGroupRow label (length hosts) : map HostItem (sortOn (.name) hosts)
    groupValue host = case grouping of
      GroupProvider -> fromMaybe "Unknown provider" host.provider
      GroupRegion -> fromMaybe "Unknown region" host.region
      GroupOS -> fromMaybe "Unknown OS" host.osType
      GroupIntegration -> maybe "OpenTelemetry" integrationLabel $ listToMaybe $ drop 1 host.integrations


hostsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders HostsGet)
hostsGetH pid providerM regionM osM integrationM groupM fromParam toParam sinceParam = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let window = TimePicker.mkTimeWindow now fromParam toParam sinceParam
  snapshot <- containersInWindow appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
  let filters = HostFilters providerM regionM osM integrationM
      hosts = applyHostFilters filters $ hostsFromRows snapshot
      grouping = parseHostGroup groupM
      table = hostsTable pid window filters grouping hosts (hostsFromRows snapshot)
  addRespHeaders $ HostsPage $ PageCtx (infrastructureBW pid "Hosts" window bw) table


newtype HostsGet = HostsPage (PageCtx (Table HostListRow))


instance ToHtml HostsGet where
  toHtml (HostsPage page) = toHtml page
  toHtmlRaw = toHtml


hostsTable :: Projects.ProjectId -> TimePicker.TimeWindow -> HostFilters -> HostGroup -> V.Vector HostRow -> V.Vector HostRow -> Table HostListRow
hostsTable pid window filters grouping hosts allHosts =
  Table
    { config =
        def
          { elemID = "hostsForm"
          , containerId = Just "hostsContainer"
          , addPadding = True
          , renderAsTable = True
          , bulkActionsInHeader = Just 0
          , containerClasses = "w-full mx-auto space-y-4 group/columns"
          }
    , columns = hostColumns pid
    , rows = hostEntries grouping hosts
    , features =
        def
          { search = Just ClientSide
          , searchPlaceholder = Just "Search hosts"
          , rowAttrs = Just $ \case HostGroupRow _ _ -> []; HostItem host -> drawerRowAttrs_ $ hostDetailUrl pid host.name
          , header = Just $ hostGroupControl pid window filters grouping (V.length hosts)
          , showFilterRail = True
          , resultSummary = Just $ "Showing " <> show (V.length hosts) <> " of " <> show (V.length allHosts) <> " hosts"
          , exportName = Just "hosts"
          , tableHeaderActions =
              Just
                $ facetActions
                  (hostBaseUrl pid window grouping)
                  "hostsContainer"
                  [ singleSelectFilter "Provider" "provider" filters.provider $ facetValues (.provider) allHosts
                  , singleSelectFilter "Region" "region" filters.region $ facetValues (.region) allHosts
                  , singleSelectFilter "OS" "os" filters.osType $ facetValues (.osType) allHosts
                  , singleSelectFilter "Integration" "integration" filters.integration $ map integrationLabel $ ordNub $ concatMap (.integrations) $ V.toList allHosts
                  ]
          , zeroState =
              Just
                ZeroState
                  { icon = "server"
                  , title = "No hosts reporting"
                  , description = "Enable the OpenTelemetry hostmetrics receiver or Kubernetes node telemetry to populate this inventory."
                  , actionText = "Host monitoring guide"
                  , destination = Right "https://monoscope.tech/docs/sdks/infrastructure/"
                  }
          }
    }


hostBaseUrl :: Projects.ProjectId -> TimePicker.TimeWindow -> HostGroup -> Text
hostBaseUrl pid window grouping = infraUrl pid "/infrastructure/hosts" [("group", hostGroupParam grouping) | grouping /= GroupNone] window


hostGroupControl :: Projects.ProjectId -> TimePicker.TimeWindow -> HostFilters -> HostGroup -> Int -> Html ()
hostGroupControl pid window filters grouping count =
  div_ [class_ "flex flex-wrap items-center justify-between gap-2 border-b border-strokeWeak px-3 py-2"] do
    span_ [class_ "text-xs text-textWeak", role_ "status", Aria.live_ "polite"] $ toHtml $ show count <> " hosts"
    form_ [method_ "get", action_ $ "/p/" <> pid.toText <> "/infrastructure/hosts", class_ "flex shrink-0 items-center gap-2 whitespace-nowrap"] do
      TimePicker.timeHiddenInputs_ window.fromQuery window.toQuery window.sinceQuery
      forM_ ([("provider", filters.provider), ("region", filters.region), ("os", filters.osType), ("integration", filters.integration)] :: [(Text, Maybe Text)]) \(field, valueM) ->
        whenJust valueM \value -> input_ [type_ "hidden", name_ field, value_ value]
      label_ [Lucid.for_ "hosts-group", class_ "shrink-0 text-xs text-textWeak"] "Group by"
      select_ [id_ "hosts-group", name_ "group", class_ "select select-xs shrink-0 cursor-pointer border-strokeWeak bg-bgBase", onchange_ "this.form.requestSubmit()"]
        $ forM_ ([("", "None"), ("provider", "Provider"), ("region", "Region"), ("os", "Operating system"), ("integration", "Integration")] :: [(Text, Text)]) \(value, label) ->
          option_ ([value_ value] <> [selected_ "" | value == hostGroupParam grouping]) $ toHtml label


hostColumns :: Projects.ProjectId -> [Column HostListRow]
hostColumns pid =
  [ col "Host" nameCell & withAttrs [class_ "min-w-52"]
  , col "Configuration" configCell & withAttrs [class_ "w-44 max-lg:hidden"]
  , col "System" systemCell & withAttrs [class_ "w-32 max-xl:hidden"]
  , col "CPU" (metricCell (.cpuPct)) & withAttrs [class_ "w-32"]
  , col "Memory" (metricCell (.memoryPct)) & withAttrs [class_ "w-32"]
  , col "Storage" (metricCell (.storagePct)) & withAttrs [class_ "host-col-storage w-32 max-lg:hidden"]
  , col "Load (1m)" (numberCell (.load1)) & withAttrs [class_ "host-col-load w-24 text-right max-xl:hidden"]
  , col "Uptime" uptimeCell & withAttrs [class_ "w-24 max-xl:hidden"]
  , col "Containers" containersCell & withAttrs [class_ "w-24 text-right"]
  , col "Integrations" integrationsCell & withAttrs [class_ "w-64 max-md:hidden"]
  ]
  where
    nameCell = \case
      HostGroupRow label count -> div_ [class_ "flex items-center gap-2 py-1 font-semibold text-textStrong"] $ toHtml label >> span_ [class_ "badge badge-xs badge-ghost"] (toHtml $ show count)
      HostItem host -> button_ ([class_ "flex cursor-pointer items-center gap-2 font-medium text-textStrong hover:text-textBrand", type_ "button", onclick_ "event.stopPropagation()"] <> drawerLoadAttrs_ (hostDetailUrl pid host.name)) do
        faSprite_ "server" "solid" "h-3.5 w-3.5 text-iconNeutral"
        span_ [class_ "truncate"] $ toHtml host.name
    configCell = itemOnly \host -> div_ [class_ "flex flex-wrap gap-1"] do
      whenJust host.provider $ metadataChip "cloud" . titleCase
      whenJust host.region $ metadataChip "location-dot"
    systemCell = itemOnly \host -> div_ [class_ "flex items-center gap-1 text-xs text-textWeak"] do
      faSprite_ "server" "regular" "h-3 w-3"
      toHtml $ T.intercalate " · " $ catMaybes [host.osType, host.architecture]
    metricCell getter = itemOnly $ utilizationCell . getter
    numberCell getter = itemOnly $ plainCell . fmap (Containers.showFFloat' 2) . getter
    uptimeCell = itemOnly $ plainCell . fmap formatUptime . (.uptime)
    containersCell = itemOnly $ span_ [class_ "tabular-nums text-textStrong"] . toHtml . show . (.containers)
    integrationsCell = itemOnly \host -> div_ [class_ "flex flex-wrap gap-1"] $ forM_ host.integrations \integration -> span_ [class_ "inline-flex items-center gap-1 whitespace-nowrap rounded-md border border-strokeWeak bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak"] do
      faSprite_ (integrationIcon integration) "regular" "h-3 w-3"
      toHtml $ integrationLabel integration
    itemOnly render = \case HostGroupRow _ _ -> mempty; HostItem host -> render host
    metadataChip icon value = span_ [class_ "inline-flex items-center gap-1 rounded bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak"] $ faSprite_ icon "regular" "h-3 w-3" >> toHtml value
    titleCase = T.toTitle


hostDetailUrl :: Projects.ProjectId -> Text -> Text
hostDetailUrl pid name = "/p/" <> pid.toText <> "/infrastructure/hosts/detail?host=" <> toUriStr name


data HostDetailGet = HostDetailMissing | HostDetail Projects.ProjectId HostRow


instance ToHtml HostDetailGet where
  toHtml = toHtmlRaw . hostDetailGet_
  toHtmlRaw = toHtml


hostDetailGet_ :: HostDetailGet -> Html ()
hostDetailGet_ HostDetailMissing = div_ [class_ "flex min-h-64 flex-col items-center justify-center gap-3 p-6 text-center"] do
  faSprite_ "server" "regular" "h-8 w-8 text-iconNeutral"
  div_ [class_ "space-y-1"] do
    h2_ [class_ "font-semibold text-textStrong"] "Host not found in this time range"
    p_ [class_ "max-w-md text-sm text-textWeak"] "Monoscope did not find this host in the current telemetry window. Return to Hosts to choose another host or time range."
  a_ [href_ "./hosts", class_ "btn btn-sm"] "Return to Hosts"
hostDetailGet_ (HostDetail pid host) = hostDetail_ pid host


hostDetailGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders HostDetailGet)
hostDetailGetH pid hostM = do
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let window = TimePicker.mkTimeWindow now Nothing Nothing Nothing
  hosts <- hostsFromRows <$> containersInWindow appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
  addRespHeaders $ maybe HostDetailMissing (HostDetail pid) $ V.find ((== hostM) . Just . (.name)) hosts


hostDetail_ :: Projects.ProjectId -> HostRow -> Html ()
hostDetail_ pid host = div_ [class_ "-mx-8 -mb-4 min-h-full"] do
  header_ [class_ "border-b border-strokeBrand-weak bg-fillBrand-weak px-5 py-4 pr-14"] do
    div_ [class_ "flex flex-wrap items-start justify-between gap-3"] do
      div_ [class_ "min-w-0"] do
        div_ [class_ "flex items-center gap-2"] do
          faSprite_ "server" "solid" "h-4 w-4 text-iconBrand"
          h2_ [id_ "host-detail-title", data_ "drawer-title" "true", class_ "break-words text-lg font-semibold text-textStrong"] $ toHtml host.name
        div_ [class_ "mt-1.5 flex flex-wrap gap-1.5"] do
          forM_ metadata $ uncurry metaChip_
          forM_ host.integrations $ span_ [class_ "badge badge-sm badge-ghost"] . toHtml . integrationLabel
      a_ ([href_ containersUrl, class_ "btn btn-xs gap-1.5 text-textBrand max-sm:h-11"] <> navTabAttrs) do
        faSprite_ "cube" "regular" "h-3.5 w-3.5"
        "View containers"
  nav_ [class_ "sticky top-0 z-20 border-b border-strokeWeak bg-bgRaised px-4 py-2", Aria.label_ "Host detail sections"] do
    div_ [class_ "flex gap-1 overflow-x-auto"] do
      sectionLink "host-summary" "server" "Summary"
      sectionLink "host-logs" "file-lines" "Recent logs"
      sectionLink "host-metrics" "chart-line" "Metrics"
  main_ [class_ "min-w-0 space-y-5 px-5 py-4"] do
    section_ [id_ "host-summary", class_ "space-y-3"] do
      div_ [class_ "flex flex-wrap items-center justify-between gap-2"] do
        h3_ [class_ "font-semibold text-textStrong"] "Host summary"
        span_ [class_ $ "inline-flex items-center gap-1.5 rounded-md px-2 py-1 text-xs font-medium " <> coverageClass] do
          span_ [class_ $ "h-2 w-2 rounded-full " <> bool "bg-fillWarning-strong" "bg-fillSuccess-strong" (availableSignals == 4), Aria.hidden_ "true"] ""
          toHtml $ "Metrics coverage: " <> show availableSignals <> " of 4"
      factGrid_
        (bool "grid-cols-2 bg-bgBase max-sm:grid-cols-1" "grid-cols-6 bg-bgBase max-xl:grid-cols-3 max-sm:grid-cols-2" (availableSignals > 0))
        summaryFacts
      when (availableSignals > 0 && availableSignals < 4) $ div_ [class_ "flex flex-wrap items-center justify-between gap-3 rounded-md border border-strokeWarning-weak bg-fillWarning-weak px-3 py-2 text-xs text-textWeak"] do
        span_ [class_ "flex min-w-0 items-start gap-2"] do
          faSprite_ "triangle-exclamation" "regular" "mt-0.5 h-3.5 w-3.5 shrink-0 text-iconWarning"
          span_ [class_ "max-w-3xl"] "Some host metrics are missing from this time range. Available signals remain visible below."
        a_ [href_ "https://monoscope.tech/docs/sdks/infrastructure/", target_ "_blank", rel_ "noopener noreferrer", class_ "shrink-0 font-medium text-textBrand hover:underline"] "Set up host metrics"
    section_ [id_ "host-logs", class_ "space-y-3 border-t border-strokeWeak pt-5"] do
      div_ [class_ "flex flex-wrap items-start justify-between gap-3"] do
        div_ [class_ "space-y-0.5"] do
          h3_ [class_ "font-semibold text-textStrong"] "Recent logs"
          p_ [class_ "text-xs text-textWeak"] "Latest events reported by this host."
        a_ ([href_ logExplorerUrl, class_ "btn btn-xs max-sm:hidden"] <> navTabAttrs) "View logs in Explorer"
      div_ [class_ "h-64 min-h-64 overflow-auto rounded-lg border border-strokeWeak bg-bgBase max-sm:hidden"] $ Log.virtualTable pid (Just logDataUrl) Nothing
      div_ [class_ "hidden flex-col items-start gap-3 rounded-lg border border-strokeWeak bg-bgBase p-4 max-sm:flex"] do
        p_ [class_ "text-sm text-textWeak"] "Open this host in Explorer to search, filter, and inspect its logs."
        a_ ([href_ logExplorerUrl, class_ "btn btn-sm btn-outline max-sm:h-11"] <> navTabAttrs) "Open logs in Explorer"
    section_ [id_ "host-metrics", class_ "space-y-3 border-t border-strokeWeak pt-4"] do
      div_ [class_ "flex flex-wrap items-center justify-between gap-2"] do
        h3_ [class_ "font-semibold text-textStrong"] "Metrics"
        when (availableSignals > 0) $ a_ ([href_ metricsUrl, class_ "btn btn-xs"] <> navTabAttrs) "View in Metrics"
      if availableSignals == 0
        then div_ [class_ "rounded-lg border border-strokeWarning-weak bg-fillWarning-weak p-5", role_ "status"] do
          div_ [class_ "flex items-start gap-3"] do
            div_ [class_ "flex h-9 w-9 shrink-0 items-center justify-center rounded-md bg-bgRaised text-iconWarning"] $ faSprite_ "chart-line" "regular" "h-4 w-4"
            div_ [class_ "min-w-0 space-y-1"] do
              h4_ [class_ "font-semibold text-textStrong"] "No host metrics in this time range"
              p_ [class_ "max-w-xl text-sm text-textWeak"] "Monoscope found no CPU, memory, filesystem, or load samples for this host. Expand the time range or check the collector setup."
          div_ [class_ "mt-4 flex flex-wrap gap-2"] do
            a_ [href_ $ "/p/" <> pid.toText <> "/infrastructure/host-map?since=1H", class_ "btn btn-sm max-sm:h-11"] "Try last 1 hour"
            a_ [href_ "https://monoscope.tech/docs/sdks/infrastructure/", target_ "_blank", rel_ "noopener noreferrer", class_ "btn btn-sm btn-primary max-sm:h-11"] "Set up host metrics"
        else div_ [class_ "grid grid-cols-2 gap-3 max-xl:grid-cols-1"] $ forM_ (hostWidgets pid host) $ div_ [class_ "min-h-56"] . Widget.widget_
  where
    metadata = [(label, value) | (label, Just value) <- [("Provider", host.provider), ("Region", host.region), ("OS", host.osType), ("Architecture", host.architecture)]]
    availableSignals = length $ catMaybes [host.cpuPct, host.memoryPct, host.storagePct, host.load1]
    coverageClass = bool "bg-fillWarning-weak text-textWarning" "bg-fillSuccess-weak text-textSuccess" (availableSignals == 4)
    summaryFacts
      | availableSignals == 0 = [("Containers", show host.containers)]
      | otherwise =
          [ ("CPU usage", pct host.cpuPct)
          , ("Memory usage", pct host.memoryPct)
          , ("Storage usage", pct host.storagePct)
          , ("Load (1m)", maybe "—" (Containers.showFFloat' 2) host.load1)
          , ("Uptime", maybe "—" formatUptime host.uptime)
          , ("Containers", show host.containers)
          ]
    pct = maybe "—" (\v -> Containers.showFFloat' 0 (v * 100) <> "%")
    query = "resource.host.name==" <> kqlQuoted host.name
    path = "/p/" <> pid.toText
    logExplorerUrl = path <> "/log_explorer?query=" <> toUriStr query <> "&since=15M&source=logs"
    logDataUrl = path <> "/log_explorer/data?query=" <> toUriStr query <> "&since=15M&source=logs"
    metricsUrl = path <> "/metrics?metric_prefix=system."
    containersUrl = path <> "/infrastructure/containers?node=" <> toUriStr host.name
    sectionClass = "flex items-center gap-1.5 whitespace-nowrap rounded-md px-2.5 py-1.5 text-sm text-textWeak hover:bg-fillBrand-weak hover:text-textBrand max-sm:min-h-11"
    sectionLink anchor icon label = a_ [href_ $ "#" <> anchor, class_ sectionClass] $ faSprite_ icon "regular" "h-3.5 w-3.5" >> toHtml label


hostWidgets :: Projects.ProjectId -> HostRow -> [Widget.Widget]
hostWidgets pid host =
  catMaybes
    [ widget host.cpuPct "host-cpu" "CPU usage" "%" $ "metrics | where metric_name == \"system.cpu.utilization\" and resource.host.name == " <> quoted <> " and attributes.cpu.mode != \"idle\" | summarize sum(value) * 100 by bin_auto(timestamp)"
    , widget host.memoryPct "host-memory" "Memory usage" "bytes" $ "metrics | where metric_name == \"system.memory.usage\" and resource.host.name == " <> quoted <> " and attributes.system.memory.state == \"used\" | summarize max(value) by bin_auto(timestamp)"
    , widget host.storagePct "host-storage" "Max storage usage" "%" $ "metrics | where metric_name == \"system.filesystem.utilization\" and resource.host.name == " <> quoted <> " | summarize max(value) * 100 by bin_auto(timestamp)"
    , widget host.load1 "host-load" "Load average (1m)" "" $ "metrics | where metric_name == \"system.cpu.load_average.1m\" and resource.host.name == " <> quoted <> " | summarize max(value) by bin_auto(timestamp)"
    ]
  where
    quoted = kqlQuoted host.name
    widget signal ident title unit query =
      signal
        $> (def :: Widget.Widget)
          { Widget.id = Just ident
          , Widget.wType = Widget.WTTimeseriesLine
          , Widget.title = Just title
          , Widget.query = Just query
          , Widget.unit = Just unit
          , Widget._projectId = Just pid
          , Widget.standalone = Just True
          , Widget.hideSubtitle = Just True
          , Widget.hideValue = Just True
          , Widget.legendPosition = Just "top-right"
          , Widget.legendSize = Just "xs"
          , Widget.layout = Just def{Widget.w = Just 6, Widget.h = Just 4}
          }


data ImageRow = ImageRow
  { image :: Text
  , registry :: Text
  , tags :: [Text]
  , running :: Int
  , runtimes :: [Runtime]
  , cpuCores :: Maybe Double
  , memoryBytes :: Maybe Double
  }
  deriving stock (Eq, Show)


imagesFromRows :: V.Vector ContainerRow -> V.Vector ImageRow
imagesFromRows = V.fromList . map build . M.toAscList . V.foldl' add M.empty
  where
    add acc row = maybe acc (\image -> M.insertWith (<>) image [row] acc) row.image
    build (image, rows) =
      ImageRow
        { image
        , registry = imageRegistry image
        , tags = Relude.sort $ ordNub $ mapMaybe (.imageTag) rows
        , running = length rows
        , runtimes = Relude.sort $ ordNub $ map runtimeOf rows
        , cpuCores = sumPresent (.cpuCores) rows
        , memoryBytes = sumPresent (.memBytes) rows
        }


imageRegistry :: Text -> Text
imageRegistry image = case T.breakOn "/" image of
  (_, rest) | T.null rest -> "Docker Hub"
  (registry, _) | "." `T.isInfixOf` registry || ":" `T.isInfixOf` registry || registry == "localhost" -> registry
  _ -> "Docker Hub"


imagesGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ImagesGet)
imagesGetH pid runtimeM registryM fromParam toParam sinceParam = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let window = TimePicker.mkTimeWindow now fromParam toParam sinceParam
  allImages <- imagesFromRows <$> containersInWindow appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
  let images = V.filter (\image -> maybe True (\wanted -> any ((== wanted) . Containers.runtimeLabel) image.runtimes) runtimeM && maybe True (== image.registry) registryM) allImages
      table = imagesTable pid window runtimeM registryM images allImages
  addRespHeaders $ ImagesPage $ PageCtx (infrastructureBW pid "Images" window bw) table


newtype ImagesGet = ImagesPage (PageCtx (Table ImageRow))


instance ToHtml ImagesGet where
  toHtml (ImagesPage page) = toHtml page
  toHtmlRaw = toHtml


imagesTable :: Projects.ProjectId -> TimePicker.TimeWindow -> Maybe Text -> Maybe Text -> V.Vector ImageRow -> V.Vector ImageRow -> Table ImageRow
imagesTable pid window runtimeM registryM images allImages =
  Table
    { config = def{elemID = "imagesForm", containerId = Just "imagesContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
    , columns =
        [ col "Image" (\image -> div_ [class_ "flex items-center gap-2 min-w-0"] $ faSprite_ "layer-group" "solid" "h-3.5 w-3.5 shrink-0 text-iconNeutral" >> span_ [class_ "truncate font-medium text-textStrong"] (toHtml image.image)) & withAttrs [class_ "min-w-72 w-full"]
        , col "Running" (\image -> span_ [class_ "badge badge-sm badge-success whitespace-nowrap"] $ toHtml $ show image.running <> " running") & withAttrs [class_ "w-28"]
        , col "Source" (\image -> span_ [class_ "text-textWeak"] $ toHtml image.registry) & withAttrs [class_ "w-36"]
        , col "Tags" renderTags & withAttrs [class_ "w-64 max-lg:hidden"]
        , col "Runtime" (\image -> span_ [class_ "text-textWeak"] $ toHtml $ T.intercalate ", " $ map Containers.runtimeLabel image.runtimes) & withAttrs [class_ "w-28 max-xl:hidden"]
        , col "CPU" (\image -> plainCell $ fmap (<> " cores") $ Containers.showFFloat' 2 <$> image.cpuCores) & withAttrs [class_ "w-24 text-right"]
        , col "Memory" (\image -> plainCell $ Containers.formatBytes <$> image.memoryBytes) & withAttrs [class_ "w-24 text-right"]
        , col "Security" (const $ span_ [class_ "inline-flex whitespace-nowrap rounded-md border border-strokeWeak bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak", term "data-tippy-content" "Connect an SBOM and vulnerability scanner to populate security findings"] "SBOM unavailable") & withAttrs [class_ "w-36 max-md:hidden"]
        ]
    , rows = images
    , features =
        def
          { search = Just ClientSide
          , searchPlaceholder = Just "Search images"
          , rowAttrs = Just $ drawerRowAttrs_ . imageDetailUrl pid window . (.image)
          , tableHeaderActions =
              Just
                $ facetActions
                  (infraUrl pid "/infrastructure/images" [] window)
                  "imagesContainer"
                  [ singleSelectFilter "Runtime" "runtime" runtimeM $ Relude.sort $ ordNub $ map Containers.runtimeLabel $ concatMap (.runtimes) $ V.toList allImages
                  , singleSelectFilter "Registry" "registry" registryM $ facetValues (Just . (.registry)) allImages
                  ]
          , zeroState = Just $ ZeroState "layer-group" "No container images reporting" "Images appear when container telemetry includes container.image.name." "Container setup guide" (Right "https://monoscope.tech/docs/sdks/infrastructure/kubernetes")
          , showFilterRail = True
          , resultSummary = Just $ "Showing " <> show (V.length images) <> " of " <> show (V.length allImages) <> " images"
          , exportName = Just "container-images"
          }
    }
  where
    renderTags image = div_ [class_ "flex max-w-72 flex-nowrap gap-1 overflow-hidden"] do
      forM_ (take 2 image.tags) $ \tag -> span_ [class_ "badge badge-xs badge-ghost shrink-0"] $ toHtml tag
      when (length image.tags > 2) $ span_ [class_ "badge badge-xs badge-outline shrink-0"] $ toHtml $ "+" <> show (length image.tags - 2)


imageDetailUrl :: Projects.ProjectId -> TimePicker.TimeWindow -> Text -> Text
imageDetailUrl pid window image = infraUrl pid "/infrastructure/images/detail" [("image", image)] window


data ImageDetailGet = ImageDetailMissing | ImageDetail Projects.ProjectId ImageRow


instance ToHtml ImageDetailGet where
  toHtml = toHtmlRaw . imageDetailGet_
  toHtmlRaw = toHtml


imageDetailGet_ :: ImageDetailGet -> Html ()
imageDetailGet_ ImageDetailMissing = div_ [class_ "p-5 text-textWeak"] "This image is no longer present in the selected time range."
imageDetailGet_ (ImageDetail pid image) = imageDetail_ pid image


imageDetailGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ImageDetailGet)
imageDetailGetH pid imageM fromParam toParam sinceParam = do
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let window = TimePicker.mkTimeWindow now fromParam toParam sinceParam
  images <- imagesFromRows <$> containersInWindow appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
  addRespHeaders $ maybe ImageDetailMissing (ImageDetail pid) $ V.find ((== imageM) . Just . (.image)) images


imageDetail_ :: Projects.ProjectId -> ImageRow -> Html ()
imageDetail_ pid image = div_ [class_ "-mx-8 -mb-4 min-h-full"] do
  header_ [class_ "border-b border-strokeWeak px-5 py-4 pr-14"] do
    div_ [class_ "flex items-center gap-2"] $ faSprite_ "layer-group" "solid" "h-4 w-4 text-iconNeutral" >> h2_ [class_ "break-words text-lg font-semibold text-textStrong"] (toHtml image.image)
    div_ [class_ "mt-2 flex flex-wrap gap-1.5"] do
      span_ [class_ "badge badge-sm badge-ghost"] $ toHtml image.registry
      forM_ image.runtimes $ \runtime -> span_ [class_ "badge badge-sm badge-ghost"] $ toHtml $ Containers.runtimeLabel runtime
  main_ [class_ "space-y-5 p-5"] do
    section_ [class_ "space-y-3"] do
      h3_ [class_ "font-semibold text-textStrong"] "Image summary"
      factGrid_
        "grid-cols-3 max-sm:grid-cols-1 max-sm:divide-x-0 max-sm:divide-y"
        [ ("Running containers", show image.running)
        , ("CPU used", maybe "—" (\value -> Containers.showFFloat' 2 value <> " cores") image.cpuCores)
        , ("Memory used", maybe "—" Containers.formatBytes image.memoryBytes)
        ]
    section_ [class_ "space-y-2 border-t border-strokeWeak pt-4"] do
      h3_ [class_ "font-semibold text-textStrong"] "Image tags"
      if null image.tags then p_ [class_ "text-sm text-textWeak"] "No image tags were reported." else div_ [class_ "flex flex-wrap gap-1.5"] $ forM_ image.tags $ \tag -> span_ [class_ "badge badge-sm badge-ghost"] $ toHtml tag
    section_ [class_ "space-y-2 border-t border-strokeWeak pt-4"] do
      h3_ [class_ "font-semibold text-textStrong"] "Security coverage"
      p_ [class_ "flex items-start gap-2 rounded-md bg-fillInformation-weak px-3 py-2 text-sm text-textWeak"] do
        faSprite_ "shield-check" "regular" "mt-0.5 h-4 w-4 shrink-0 text-iconInformation"
        "No SBOM or vulnerability findings are connected for this image. Monoscope will not infer a clean result from missing scanner data."
    div_ [class_ "flex flex-wrap gap-2 border-t border-strokeWeak pt-4"] do
      a_ [href_ $ "/p/" <> pid.toText <> "/infrastructure/containers?image=" <> toUriStr image.image, class_ "btn btn-sm"] "View containers"
      a_ [href_ $ "/p/" <> pid.toText <> "/metrics?metric_prefix=container.", class_ "btn btn-sm"] "View metrics"


-- | Constructor order is tab order: @[minBound ..]@ is what the resource tab strip renders,
-- so adding a view here adds it everywhere.
data KubeResource = KubePods | KubeClusters | KubeNamespaces | KubeNodes | KubeWorkloads
  deriving stock (Bounded, Enum, Eq, Show)


-- | URL param and singular label, in one ladder so the two can't drift apart.
kubeResourceNames :: KubeResource -> (Text, Text)
kubeResourceNames = \case
  KubePods -> ("pods", "Pod")
  KubeClusters -> ("clusters", "Cluster")
  KubeNamespaces -> ("namespaces", "Namespace")
  KubeNodes -> ("nodes", "Node")
  KubeWorkloads -> ("workloads", "Workload")


parseKubeResource :: Maybe Text -> KubeResource
parseKubeResource param = fromMaybe KubePods $ find ((== param) . Just . kubeResourceParam) [minBound ..]


kubeResourceParam :: KubeResource -> Text
kubeResourceParam = fst . kubeResourceNames


resourceLabel :: KubeResource -> Text
resourceLabel = snd . kubeResourceNames


data KubeStatus = KubeReady | KubeNotReady | KubeUnknown
  deriving stock (Eq, Ord, Show)


kubeStatusLabel :: KubeStatus -> Text
kubeStatusLabel = \case
  KubeReady -> "Ready"
  KubeNotReady -> "Not ready"
  KubeUnknown -> "Unknown"


data KubeRow = KubeRow
  { name :: Text
  , status :: KubeStatus
  , cluster :: Maybe Text
  , namespace :: Maybe Text
  , node :: Maybe Text
  , workload :: Maybe Text
  , containers :: Int
  , cpuCores :: Maybe Double
  , cpuPct :: Maybe Double
  , memoryBytes :: Maybe Double
  , memoryPct :: Maybe Double
  , restarts :: Maybe Double
  }
  deriving stock (Eq, Show)


kubeRowsFromRows :: KubeResource -> V.Vector ContainerRow -> V.Vector KubeRow
kubeRowsFromRows resource = V.fromList . map build . M.toAscList . V.foldl' add M.empty . V.filter ((== Kubernetes) . runtimeOf)
  where
    add acc row = maybe acc (\key -> M.insertWith (<>) key [row] acc) $ kubeIdentity row
    kubeIdentity row = case resource of
      KubePods -> (,,,) <$> row.podName <*> pure row.cluster <*> pure row.namespace <*> pure row.nodeName
      KubeClusters -> (,,,) <$> row.cluster <*> pure row.cluster <*> pure Nothing <*> pure Nothing
      KubeNamespaces -> (,,,) <$> row.namespace <*> pure row.cluster <*> pure row.namespace <*> pure Nothing
      KubeNodes -> (,,,) <$> row.nodeName <*> pure row.cluster <*> pure Nothing <*> pure row.nodeName
      KubeWorkloads -> (,,,) <$> row.workload <*> pure row.cluster <*> pure row.namespace <*> pure Nothing
    build ((name, cluster, namespace, node), rows) =
      let readyValues = mapMaybe (.ready) rows
          cpu = sumPresent (.cpuCores) rows
          cpuLimit = sumPresent (.cpuLimit) rows
          memory = sumPresent (.memBytes) rows
          memoryLimit = sumPresent (.memLimit) rows
       in KubeRow
            { name
            , status = if any (<= 0) readyValues then KubeNotReady else if null readyValues then KubeUnknown else KubeReady
            , cluster
            , namespace
            , node
            , workload = firstJust (.workload) rows
            , containers = max 1 $ length [() | row <- rows, row.scope == ScopeContainer]
            , cpuCores = cpu
            , cpuPct = ratio cpu cpuLimit
            , memoryBytes = memory
            , memoryPct = ratio memory memoryLimit
            , restarts = sumPresent (.restarts) rows
            }
    firstJust f = listToMaybe . mapMaybe f


kubernetesGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders KubernetesGet)
kubernetesGetH pid resourceM clusterM namespaceM statusM fromParam toParam sinceParam = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let resource = parseKubeResource resourceM
      window = TimePicker.mkTimeWindow now fromParam toParam sinceParam
  allRows <- kubeRowsFromRows resource <$> containersInWindow appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
  let rows = V.filter (\row -> matches clusterM row.cluster && matches namespaceM row.namespace && maybe True (\wanted -> wanted == kubeStatusLabel row.status) statusM) allRows
      table = kubernetesTable pid window resource clusterM namespaceM statusM rows allRows
  addRespHeaders $ KubernetesPage $ PageCtx (infrastructureBW pid "Kubernetes" window bw) table
  where
    matches selected actual = maybe True (\value -> T.null value || Just value == actual) selected


newtype KubernetesGet = KubernetesPage (PageCtx (Table KubeRow))


instance ToHtml KubernetesGet where
  toHtml (KubernetesPage page) = toHtml page
  toHtmlRaw = toHtml


kubernetesTable :: Projects.ProjectId -> TimePicker.TimeWindow -> KubeResource -> Maybe Text -> Maybe Text -> Maybe Text -> V.Vector KubeRow -> V.Vector KubeRow -> Table KubeRow
kubernetesTable pid window resource clusterM namespaceM statusM rows allRows =
  Table
    { config = def{elemID = "kubernetesForm", containerId = Just "kubernetesContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
    , columns =
        [ col (resourceLabel resource) (\row -> div_ [class_ "flex items-center gap-2"] $ faSprite_ (if resource `elem` [KubeClusters, KubeNodes] then "server" else "cube") "solid" "h-3.5 w-3.5 text-iconNeutral" >> span_ [class_ "font-medium text-textStrong"] (toHtml row.name)) & withAttrs [class_ "min-w-56 w-full"]
        , col "Status" (statusBadge . (.status)) & withAttrs [class_ "w-28"]
        , col "Cluster" (\row -> plainCell row.cluster) & withAttrs [class_ "w-36 max-lg:hidden"]
        , col "Namespace" (\row -> plainCell row.namespace) & withAttrs [class_ "w-32 max-lg:hidden"]
        , col "Node" (\row -> plainCell row.node) & withAttrs [class_ "w-36 max-xl:hidden"]
        , col "Containers" (\row -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ show row.containers) & withAttrs [class_ "w-24 text-right"]
        , col "CPU" (\row -> plainCell $ fmap (<> " cores") $ Containers.showFFloat' 2 <$> row.cpuCores) & withAttrs [class_ "w-24 text-right"]
        , col "CPU limit used" (utilizationCell . (.cpuPct)) & withAttrs [class_ "w-36 max-md:hidden"]
        , col "Memory" (\row -> plainCell $ Containers.formatBytes <$> row.memoryBytes) & withAttrs [class_ "w-24 text-right"]
        , col "Memory limit used" (utilizationCell . (.memoryPct)) & withAttrs [class_ "w-40 max-md:hidden"]
        , col "Restarts" (\row -> plainCell $ Containers.showFFloat' 0 <$> row.restarts) & withAttrs [class_ "w-20 text-right max-md:hidden"]
        ]
    , rows
    , features =
        def
          { search = Just ClientSide
          , searchPlaceholder = Just $ "Search " <> kubeResourceParam resource
          , rowAttrs = Just $ drawerRowAttrs_ . kubeDetailUrl pid window resource
          , header = Just $ kubeResourceNav pid window resource (V.length rows)
          , showFilterRail = True
          , resultSummary = Just $ "Showing " <> show (V.length rows) <> " resources"
          , exportName = Just $ "kubernetes-" <> kubeResourceParam resource
          , tableHeaderActions =
              Just
                $ facetActions
                  (infraUrl pid "/infrastructure/kubernetes" [("resource", kubeResourceParam resource)] window)
                  "kubernetesContainer"
                  [ singleSelectFilter "Cluster" "cluster" clusterM $ facetValues (.cluster) allRows
                  , singleSelectFilter "Namespace" "namespace" namespaceM $ facetValues (.namespace) allRows
                  , singleSelectFilter "Status" "status" statusM $ facetValues (Just . kubeStatusLabel . (.status)) allRows
                  ]
          , zeroState = Just $ ZeroState "cube" "No Kubernetes resources reporting" "Enable kubeletstats and k8s_cluster receivers to populate pods, workloads, and nodes." "Kubernetes setup guide" (Right "https://monoscope.tech/docs/sdks/infrastructure/kubernetes")
          }
    }


kubeResourceNav :: Projects.ProjectId -> TimePicker.TimeWindow -> KubeResource -> Int -> Html ()
kubeResourceNav pid window current count = div_ [class_ "flex flex-wrap items-center justify-between gap-2 border-b border-strokeWeak px-3 py-2"] do
  div_ [class_ "tabs tabs-box tabs-outline tabs-sm", role_ "tablist", Aria.label_ "Kubernetes resource"] $ forM_ [minBound ..] \resource ->
    a_ ([href_ $ infraUrl pid "/infrastructure/kubernetes" [("resource", kubeResourceParam resource)] window, role_ "tab", class_ $ "tab" <> bool "" " tab-active" (resource == current)] <> navTabAttrs) $ toHtml $ resourceLabel resource <> "s"
  span_ [class_ "text-xs text-textWeak", role_ "status", Aria.live_ "polite"] $ toHtml $ show count <> " resources"


kubeDetailUrl :: Projects.ProjectId -> TimePicker.TimeWindow -> KubeResource -> KubeRow -> Text
kubeDetailUrl pid window resource row =
  infraUrl
    pid
    "/infrastructure/kubernetes/detail"
    ( [("resource", kubeResourceParam resource), ("name", row.name)]
        <> [("cluster", cluster) | cluster <- maybeToList row.cluster]
        <> [("namespace", namespace) | namespace <- maybeToList row.namespace]
    )
    window


data KubernetesDetailGet = KubernetesDetailMissing | KubernetesDetail Projects.ProjectId KubeResource KubeRow


instance ToHtml KubernetesDetailGet where
  toHtml = toHtmlRaw . kubernetesDetailGet_
  toHtmlRaw = toHtml


kubernetesDetailGet_ :: KubernetesDetailGet -> Html ()
kubernetesDetailGet_ KubernetesDetailMissing = div_ [class_ "p-5 text-textWeak"] "This Kubernetes resource is no longer present in the selected time range."
kubernetesDetailGet_ (KubernetesDetail pid resource row) = kubernetesDetail_ pid resource row


kubernetesDetailGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders KubernetesDetailGet)
kubernetesDetailGetH pid resourceM nameM clusterM namespaceM fromParam toParam sinceParam = do
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let resource = parseKubeResource resourceM
      window = TimePicker.mkTimeWindow now fromParam toParam sinceParam
  rows <- kubeRowsFromRows resource <$> containersInWindow appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
  addRespHeaders $ maybe KubernetesDetailMissing (KubernetesDetail pid resource) $ V.find (\row -> Just row.name == nameM && maybe True (\cluster -> row.cluster == Just cluster) clusterM && maybe True (\namespace -> row.namespace == Just namespace) namespaceM) rows


kubernetesDetail_ :: Projects.ProjectId -> KubeResource -> KubeRow -> Html ()
kubernetesDetail_ pid resource row = div_ [class_ "-mx-8 -mb-4 min-h-full"] do
  header_ [class_ "border-b border-strokeWeak px-5 py-4 pr-14"] do
    div_ [class_ "flex flex-wrap items-center gap-2"] do
      faSprite_ (if resource `elem` [KubeClusters, KubeNodes] then "server" else "cube") "solid" "h-4 w-4 text-iconNeutral"
      h2_ [class_ "break-words text-lg font-semibold text-textStrong"] $ toHtml row.name
      statusBadge row.status
    div_ [class_ "mt-2 flex flex-wrap gap-1.5"] $ forM_ metadata $ uncurry metaChip_
  main_ [class_ "space-y-5 p-5"] do
    section_ [class_ "space-y-3"] do
      h3_ [class_ "font-semibold text-textStrong"] $ toHtml $ resourceLabel resource <> " summary"
      factGrid_
        "grid-cols-5 max-lg:grid-cols-3 max-sm:grid-cols-2"
        [ ("Containers", show row.containers)
        , ("CPU", maybe "—" (\value -> Containers.showFFloat' 2 value <> " cores") row.cpuCores)
        , ("CPU / limit", maybe "—" (\value -> Containers.showFFloat' 0 (value * 100) <> "%") row.cpuPct)
        , ("Memory", maybe "—" Containers.formatBytes row.memoryBytes)
        , ("Restarts", maybe "—" (Containers.showFFloat' 0) row.restarts)
        ]
      when (isNothing row.cpuCores || isNothing row.memoryBytes) $ p_ [class_ "rounded-md bg-fillInformation-weak px-3 py-2 text-sm text-textWeak"] "Usage is incomplete in this time range. Enable the kubeletstats receiver's node, pod, and container metric groups to fill the missing signals."
    div_ [class_ "flex flex-wrap gap-2 border-t border-strokeWeak pt-4"] do
      whenJust row.namespace $ \namespace -> a_ [href_ $ "/p/" <> pid.toText <> "/infrastructure/containers?namespace=" <> toUriStr namespace, class_ "btn btn-sm"] "View containers"
      a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr (kubeQuery resource row), class_ "btn btn-sm"] "View logs"
      a_ [href_ $ "/p/" <> pid.toText <> "/metrics?metric_prefix=k8s.", class_ "btn btn-sm"] "View metrics"
  where
    metadata = [(label, value) | (label, Just value) <- [("Cluster", row.cluster), ("Namespace", row.namespace), ("Node", row.node), ("Workload", row.workload)]]
    kubeQuery kind resourceRow = field <> "==" <> kqlQuoted resourceRow.name
      where
        field = case kind of
          KubePods -> "resource.k8s.pod.name"
          KubeClusters -> "resource.k8s.cluster.name"
          KubeNamespaces -> "resource.k8s.namespace.name"
          KubeNodes -> "resource.k8s.node.name"
          KubeWorkloads -> "coalesce(resource.k8s.deployment.name, resource.k8s.statefulset.name, resource.k8s.daemonset.name, resource.k8s.job.name, resource.k8s.cronjob.name)"


data HostMapFill = FillCPU | FillMemory | FillStorage
  deriving stock (Eq, Show)


parseHostMapFill :: Maybe Text -> HostMapFill
parseHostMapFill = \case Just "memory" -> FillMemory; Just "storage" -> FillStorage; Just _ -> FillCPU; Nothing -> FillCPU


hostMapFillParam :: HostMapFill -> Text
hostMapFillParam = \case FillCPU -> "cpu"; FillMemory -> "memory"; FillStorage -> "storage"


data HostMapData = HostMapData
  { pid :: Projects.ProjectId
  , window :: TimePicker.TimeWindow
  , fill :: HostMapFill
  , grouping :: HostGroup
  , filters :: HostFilters
  , allHosts :: V.Vector HostRow
  , groups :: [(Text, [HostRow])]
  }


newtype HostMapGet = HostMapPage (PageCtx HostMapData)


instance ToHtml HostMapGet where
  toHtml (HostMapPage page) = toHtml page
  toHtmlRaw = toHtml


instance ToHtml HostMapData where
  toHtml = toHtmlRaw . hostMap_
  toHtmlRaw = toHtml


hostMapGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders HostMapGet)
hostMapGetH pid fillM groupM providerM regionM osM fromParam toParam sinceParam = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let window = TimePicker.mkTimeWindow now fromParam toParam sinceParam
  snapshot <- containersInWindow appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
  let allHosts = hostsFromRows snapshot
      filters = HostFilters providerM regionM osM Nothing
      hosts = applyHostFilters filters allHosts
      grouping = parseHostGroup groupM
      groups = hostMapGroups grouping hosts
  addRespHeaders $ HostMapPage $ PageCtx (infrastructureBW pid "Host Map" window bw) HostMapData{pid, window, fill = parseHostMapFill fillM, grouping, filters, allHosts, groups}


hostMapGroups :: HostGroup -> V.Vector HostRow -> [(Text, [HostRow])]
hostMapGroups GroupNone hosts
  | V.null hosts = []
  | otherwise = [("All hosts", V.toList hosts)]
hostMapGroups grouping hosts = M.toAscList $ V.foldl' (\acc host -> M.insertWith (<>) (groupValue host) [host] acc) M.empty hosts
  where
    groupValue host = case grouping of
      GroupProvider -> fromMaybe "Unknown provider" host.provider
      GroupRegion -> fromMaybe "Unknown region" host.region
      GroupOS -> fromMaybe "Unknown OS" host.osType
      GroupIntegration -> maybe "OpenTelemetry" integrationLabel $ listToMaybe $ drop 1 host.integrations


hostMap_ :: HostMapData -> Html ()
hostMap_ page = div_ [class_ "flex min-h-full flex-col bg-bgSunken"] do
  form_ [method_ "get", action_ $ "/p/" <> page.pid.toText <> "/infrastructure/host-map", class_ "flex flex-wrap items-end gap-3 border-b border-strokeWeak bg-bgRaised px-4 py-3"] do
    TimePicker.timeHiddenInputs_ page.window.fromQuery page.window.toQuery page.window.sinceQuery
    mapSelect "fill" "Fill by" (hostMapFillParam page.fill) [("cpu", "CPU usage"), ("memory", "Memory usage"), ("storage", "Storage usage")]
    mapSelect "group" "Group by" (hostGroupParam page.grouping) [("", "None"), ("provider", "Provider"), ("region", "Region"), ("os", "Operating system"), ("integration", "Integration")]
    mapSelect "provider" "Provider" (fromMaybe "" page.filters.provider) $ ("", "All") : map dup (facetValues (.provider) page.allHosts)
    mapSelect "region" "Region" (fromMaybe "" page.filters.region) $ ("", "All") : map dup (facetValues (.region) page.allHosts)
    div_ [class_ "ml-auto flex flex-wrap items-center gap-3 text-xs text-textWeak", Aria.label_ "Utilization legend"] do
      legend "bg-fillSuccess-strong" "Below 60%"
      legend "bg-fillWarning-strong" "60–85%"
      legend "bg-fillError-strong" "Above 85%"
      legend "bg-fillNeutral-strong" "No data"
  if null page.groups
    then div_ [class_ "m-auto flex max-w-md flex-col items-center gap-2 p-8 text-center"] do
      div_ [class_ "mb-1 flex h-12 w-12 items-center justify-center rounded-lg bg-fillBrand-weak text-iconBrand"] $ faSprite_ "server" "regular" "h-5 w-5"
      h2_ [class_ "font-semibold text-textStrong"] "No hosts reporting"
      p_ [class_ "text-sm text-textWeak"] "Enable host metrics or Kubernetes node telemetry to populate the map."
      a_ [href_ "https://monoscope.tech/docs/sdks/infrastructure/", target_ "_blank", rel_ "noopener noreferrer", class_ "btn btn-sm btn-primary mt-2"] "Set up host monitoring"
    else div_ [class_ "flex flex-wrap items-start gap-4 p-4"] $ forM_ page.groups \(label, hosts) -> section_ [class_ $ "rounded-lg border border-strokeWeak bg-bgRaised p-3 shadow-sm " <> bool "min-w-80 flex-1" "w-fit min-w-72" (length hosts <= 12)] do
      h2_ [class_ "mb-3 flex items-center gap-2 text-sm font-semibold text-textStrong"] $ toHtml label >> span_ [class_ "rounded-full bg-fillBrand-weak px-2 py-0.5 text-xs font-medium text-textStrong"] (toHtml $ show (length hosts) <> " hosts")
      div_ [class_ $ "flex flex-wrap " <> bool "gap-1" "gap-2" (length hosts <= 12)] $ forM_ (sortOn (.name) hosts) $ hostHex page.pid page.fill (length hosts <= 12)
  where
    legend colour label = span_ [class_ "inline-flex items-center gap-1.5"] $ span_ [class_ $ "h-3 w-3 " <> colour, style_ "clip-path:polygon(25% 0,75% 0,100% 50%,75% 100%,25% 100%,0 50%)"] mempty >> toHtml label


mapSelect :: Text -> Text -> Text -> [(Text, Text)] -> Html ()
mapSelect field label current options = label_ [class_ "flex flex-col gap-1 text-xs text-textWeak"] do
  toHtml label
  select_ [name_ field, class_ "select select-sm min-w-44 border-strokeWeak bg-bgBase text-sm text-textStrong max-sm:h-11", onchange_ "this.form.requestSubmit()"] $ forM_ options \(value, title) -> option_ ([value_ value] <> [selected_ "" | value == current]) $ toHtml title


hostHex :: Projects.ProjectId -> HostMapFill -> Bool -> HostRow -> Html ()
hostHex pid fill enlarged host = div_ [class_ $ "flex flex-col items-center gap-1 " <> bool "" "w-24" enlarged] do
  button_
    ( [ class_ $ "inline-flex items-center justify-center text-textInverse-strong transition-transform hover:-translate-y-0.5 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong motion-reduce:transform-none " <> utilizationClass value
      , type_ "button"
      , style_ $ (if enlarged then "height:54px;width:49px;" else "height:44px;width:40px;") <> "clip-path:polygon(25% 0,75% 0,100% 50%,75% 100%,25% 100%,0 50%)"
      , term "data-tippy-content" $ host.name <> " · " <> maybe "No data" (\v -> Containers.showFFloat' 0 (v * 100) <> "%") value
      , Aria.label_ $ host.name <> ", " <> fillLabel fill <> ": " <> maybe "no data" (\v -> Containers.showFFloat' 0 (v * 100) <> " percent") value
      ]
        <> drawerLoadAttrs_ (hostDetailUrl pid host.name)
    )
    $ faSprite_ "server" "solid"
    $ bool "h-3 w-3" "h-4 w-4" enlarged
  when enlarged $ span_ [data_ "visible-host-label" host.name, class_ "w-full truncate text-center text-xs text-textWeak", Aria.hidden_ "true"] $ toHtml host.name
  where
    value = case fill of FillCPU -> host.cpuPct; FillMemory -> host.memoryPct; FillStorage -> host.storagePct


fillLabel :: HostMapFill -> Text
fillLabel = \case FillCPU -> "CPU usage"; FillMemory -> "memory usage"; FillStorage -> "storage usage"


utilizationClass :: Maybe Double -> Text
utilizationClass = \case
  Nothing -> "bg-fillNeutral-strong"
  Just value | value < 0.6 -> "bg-fillSuccess-strong"
  Just value | value < 0.85 -> "bg-fillWarning-strong"
  Just _ -> "bg-fillError-strong"


infrastructureBW :: Projects.ProjectId -> Text -> TimePicker.TimeWindow -> BWConfig -> BWConfig
infrastructureBW pid title window bw =
  bw
    { prePageTitle = Just "Infrastructure"
    , pageTitle = title
    , menuItem = Just "Infrastructure"
    , navTabs = Just $ infrastructureNavTabs_ pid title window.fromQuery window.toQuery window.sinceQuery
    , pageActions = Just $ div_ [class_ "inline-flex items-center gap-2", data_ "default-window" "5M"] do
        TimePicker.timepicker_ Nothing window.currentRange Nothing
        TimePicker.refreshButton_
    , needsGridStack = True
    }


sumPresent :: (a -> Maybe Double) -> [a] -> Maybe Double
sumPresent getter values = case mapMaybe getter values of [] -> Nothing; present -> Just $ sum present


ratio :: Maybe Double -> Maybe Double -> Maybe Double
ratio usage limit = do
  used <- usage
  capacity <- limit
  guard $ capacity > 0
  pure $ used / capacity


plainCell :: Maybe Text -> Html ()
plainCell = maybe (span_ [class_ "text-textWeak"] "—") (\value -> span_ [class_ "block truncate whitespace-nowrap text-textStrong tabular-nums", term "data-tippy-content" value] $ toHtml value)


utilizationCell :: Maybe Double -> Html ()
utilizationCell = \case
  Nothing -> plainCell Nothing
  Just value -> div_ [class_ "flex min-w-24 items-center gap-1.5"] do
    span_ [class_ "w-10 shrink-0 text-right tabular-nums text-textStrong"] $ toHtml $ Containers.showFFloat' 0 (value * 100) <> "%"
    div_ [class_ "h-1.5 grow overflow-hidden rounded-full bg-fillWeak"] $ div_ [class_ $ "h-full rounded-full " <> utilizationClass (Just value), style_ $ "width:" <> Containers.showFFloat' 0 (min 1 value * 100) <> "%"] mempty


statusBadge :: KubeStatus -> Html ()
statusBadge status = span_ [class_ $ "badge badge-sm whitespace-nowrap " <> case status of { KubeReady -> "badge-success"; KubeNotReady -> "badge-error"; KubeUnknown -> "badge-ghost" }] $ toHtml $ kubeStatusLabel status


formatUptime :: Double -> Text
formatUptime seconds
  | seconds >= 86400 = Containers.showFFloat' 0 (seconds / 86400) <> "d"
  | seconds >= 3600 = Containers.showFFloat' 0 (seconds / 3600) <> "h"
  | otherwise = Containers.showFFloat' 0 (seconds / 60) <> "m"
