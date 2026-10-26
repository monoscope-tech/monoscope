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
  DetailGet (..),
) where

import Data.Default (def)
import Data.List.Extra (firstJust)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Lucid
import Lucid.Aria qualified as Aria
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers (ContainerRow (..), Runtime (..), Scope (..), cpuPctOfLimit, memPctOfLimit, ratio, runtimeOf)
import Pages.BodyWrapper (PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (BadgeColor (..), Deferred (..), EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), RefreshingDeferred (..), callout_, detailSection_, emptyState_, entityHeader_, factGrid_, metaChip_, pivotLinks_, tableSkeleton_, timeRefreshListener_, tonePill_, withDeferredBody)
import Pages.Containers (deferUrl, infraSnapshot, infraUrl, infrastructureBW, matchesAny, matchesFilter, mkWindow, pctText)
import Pages.Containers qualified as Containers
import Pages.LogExplorer.Log qualified as Log
import Pkg.Components.Facets qualified as Facets
import Pkg.Components.Table (Features (..), SelectControl (..), Table (..), TableTabs (..), ZeroState (..), col, countedFilter, facetActions, facetsConfig, inventoryTable, selectControl_, withAttrs)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (WrappedEnumSC (..))
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (drawerLoadAttrs_, drawerRowAttrs_, faSprite_, formatBytes, showFFloat')


-- | Group by an optional key, dropping rows without one. Per-key lists come out in reverse
-- snapshot order, so @firstJust@ over them picks the most recently seen row.
groupRows :: Ord k => (a -> Maybe k) -> V.Vector a -> [(k, [a])]
groupRows key = M.toAscList . V.foldl' (\acc r -> maybe acc (\k -> M.insertWith (<>) k [r] acc) (key r)) M.empty


-- | Resolve a URL param against an enum's param spellings; an unknown value is the default.
parseParam :: (Bounded a, Enum a) => a -> (a -> Text) -> Maybe Text -> a
parseParam dflt spelling param = fromMaybe dflt $ find ((== param) . Just . spelling) [minBound ..]


data InfraIntegration = OpenTelemetryIntegration | KubernetesIntegration | DockerIntegration
  deriving stock (Eq, Ord, Show)


integrationLabel :: InfraIntegration -> Text
integrationLabel = \case
  OpenTelemetryIntegration -> "OpenTelemetry"
  KubernetesIntegration -> "Kubernetes"
  DockerIntegration -> "Docker"


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
hostsFromRows = V.fromList . map build . groupRows (.nodeName)
  where
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
    hostValue f = firstJust f . filter ((== ScopeHost) . (.scope))


data HostFilters = HostFilters
  { provider :: Maybe Text
  , region :: Maybe Text
  , osType :: Maybe Text
  , integration :: Maybe Text
  }


applyHostFilters :: HostFilters -> V.Vector HostRow -> V.Vector HostRow
applyHostFilters f = V.filter \h ->
  matchesFilter f.provider h.provider
    && matchesFilter f.region h.region
    && matchesFilter f.osType h.osType
    && matchesAny f.integration (map integrationLabel h.integrations)


-- | Constructor order is the "Group by" option order; the param doubles as the option value.
data HostGroup = GroupNone | GroupProvider | GroupRegion | GroupOS | GroupIntegration
  deriving stock (Bounded, Enum, Eq, Show)


hostGroupParam :: HostGroup -> Text
hostGroupParam = fst . hostGroupOption


hostGroupOption :: HostGroup -> (Text, Text)
hostGroupOption = \case
  GroupNone -> ("", "None")
  GroupProvider -> ("provider", "Provider")
  GroupRegion -> ("region", "Region")
  GroupOS -> ("os", "Operating system")
  GroupIntegration -> ("integration", "Integration")


hostGroupLabel :: HostGroup -> HostRow -> Text
hostGroupLabel grouping host = case grouping of
  GroupNone -> "All hosts"
  GroupProvider -> fromMaybe "Unknown provider" host.provider
  GroupRegion -> fromMaybe "Unknown region" host.region
  GroupOS -> fromMaybe "Unknown OS" host.osType
  GroupIntegration -> maybe "OpenTelemetry" integrationLabel $ listToMaybe $ drop 1 host.integrations


hostsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders HostsGet)
hostsGetH pid providerM regionM osM integrationM groupM fromParam toParam sinceParam deferredM = do
  (_, _, bw) <- mkPageCtx pid
  window <- mkWindow fromParam toParam sinceParam
  let filters = HostFilters providerM regionM osM integrationM
      grouping = parseParam GroupNone hostGroupParam groupM
      url = deferUrl pid "/infrastructure/hosts" [("provider", providerM), ("region", regionM), ("os", osM), ("integration", integrationM), ("group", groupM)] window
  body <- withDeferredBody deferredM "hostsContainer" url (tableSkeleton_ 8) do
    allHosts <- hostsFromRows <$> infraSnapshot pid window
    pure $ hostsTable pid window url filters grouping (applyHostFilters filters allHosts) allHosts
  addRespHeaders $ HostsPage $ PageCtx (infrastructureBW pid "Hosts" window bw) body


newtype HostsGet = HostsPage (PageCtx (Deferred (Table HostRow)))
  deriving (ToHtml) via (PageCtx (RefreshingDeferred (Table HostRow)))


hostsTable :: Projects.ProjectId -> TimePicker.TimeWindow -> Text -> HostFilters -> HostGroup -> V.Vector HostRow -> V.Vector HostRow -> Table HostRow
hostsTable pid window url filters grouping hosts allHosts =
  table
    { columns = hostColumns
    , rows = V.fromList $ sortOn (\h -> (hostGroupLabel grouping h, h.name)) $ V.toList hosts
    , features =
        table.features
          { rowAttrs = Just $ drawerRowAttrs_ . hostDetailUrl pid window . (.name)
          , rowGroup = if grouping == GroupNone then Nothing else Just $ Just . hostGroupLabel grouping
          , grouping = Just $ SelectControl "group" "Group by" (hostGroupParam grouping) (map hostGroupOption [minBound ..]) url "hostsContainer"
          , resultSummary = Just $ "Showing " <> show (V.length hosts) <> " of " <> show (V.length allHosts) <> " hosts"
          , facets =
              Just
                $ facetActions
                  (infraUrl pid "/infrastructure/hosts" [("group", hostGroupParam grouping) | grouping /= GroupNone] window)
                  "hostsContainer"
                  [ countedFilter "Provider" "provider" filters.provider (maybeToList . (.provider)) allHosts
                  , countedFilter "Region" "region" filters.region (maybeToList . (.region)) allHosts
                  , countedFilter "OS" "os" filters.osType (maybeToList . (.osType)) allHosts
                  , countedFilter "Integration" "integration" filters.integration (map integrationLabel . (.integrations)) allHosts
                  ]
          , zeroState =
              Just
                ZeroState
                  { icon = "server"
                  , title = "No hosts reporting"
                  , description = "Enable the OpenTelemetry hostmetrics receiver or Kubernetes node telemetry to populate this inventory."
                  , action = ESLink "https://monoscope.tech/docs/sdks/infrastructure/" "Host monitoring guide"
                  }
          }
    }
  where
    table = inventoryTable "hosts" url
    -- Configuration only earns its width when some host reports a provider or region; it is
    -- decided over the unfiltered inventory so a facet never toggles a column.
    hostColumns =
      [col "Host" nameCell & withAttrs [class_ "min-w-52"]]
        <> [col "Configuration" configCell & withAttrs [class_ "w-44 max-lg:hidden"] | any (\h -> isJust h.provider || isJust h.region) allHosts]
        <> [ col "System" systemCell & withAttrs [class_ "w-32 max-xl:hidden"]
           , col "CPU" (metricCell (.cpuPct)) & withAttrs [class_ "w-32"]
           , col "Memory" (metricCell (.memoryPct)) & withAttrs [class_ "w-32"]
           , col "Storage" (metricCell (.storagePct)) & withAttrs [class_ "host-col-storage w-32 max-lg:hidden"]
           , col "Load (1m)" (plainCell . fmap (showFFloat' 2) . (.load1)) & withAttrs [class_ "host-col-load w-24 text-right max-xl:hidden"]
           , col "Uptime" uptimeCell & withAttrs [class_ "w-24 max-xl:hidden"]
           , col "Containers" containersCell & withAttrs [class_ "w-24 text-right"]
           , col "Integrations" integrationsCell & withAttrs [class_ "w-56 max-md:hidden"]
           ]
      where
        -- The row itself opens the drawer; a button in the name would be a second tab stop for the same action.
        nameCell host = div_ [class_ "flex min-w-0 items-center gap-2 font-medium text-textStrong"] do
          faSprite_ "server" "solid" "h-3.5 w-3.5 shrink-0 text-iconNeutral"
          span_ [class_ "truncate"] $ toHtml host.name
        configCell host = div_ [class_ "flex flex-wrap gap-1"] do
          whenJust host.provider $ metadataChip "cloud" . T.toTitle
          whenJust host.region $ metadataChip "location-dot"
        systemCell host = span_ [class_ "text-xs text-textWeak"] $ toHtml $ T.intercalate " · " $ catMaybes [host.osType, host.architecture]
        metricCell getter = utilizationCell . getter
        uptimeCell = plainCell . fmap formatUptime . (.uptime)
        containersCell = span_ [class_ "tabular-nums text-textStrong"] . toHtml . show . (.containers)
        integrationsCell host = div_ [class_ "flex flex-nowrap gap-1"] $ forM_ host.integrations \integration -> span_ [class_ "inline-flex items-center gap-1 whitespace-nowrap rounded-md border border-strokeWeak bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak"] do
          faSprite_ (case integration of OpenTelemetryIntegration -> "arrows-turn-right"; KubernetesIntegration -> "cube"; DockerIntegration -> "layer-group") "regular" "h-3 w-3"
          toHtml $ integrationLabel integration
        metadataChip icon value = span_ [class_ "inline-flex items-center gap-1 rounded bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak"] $ faSprite_ icon "regular" "h-3 w-3" >> toHtml value


hostDetailUrl :: Projects.ProjectId -> TimePicker.TimeWindow -> Text -> Text
hostDetailUrl pid window name = infraUrl pid "/infrastructure/hosts/detail" [("host", name)] window


-- | Drawer payload shared by the three infrastructure detail routes: the rendered
-- detail, or the section's empty state when the entity left the time window.
newtype DetailGet = DetailGet (Html ())
  deriving newtype (ToHtml)


detailFrom :: EmptyStateCfg -> Text -> Text -> (a -> Html ()) -> Maybe a -> DetailGet
detailFrom cfg title sub render = DetailGet . maybe (emptyState_ cfg title sub) render


-- | The drawer reads the same window the table row was rendered from. Anything else shows a
-- host's usage from a range the user did not pick, and re-runs the snapshot query the page
-- has already run instead of reusing it.
hostDetailGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders DetailGet)
hostDetailGetH pid hostM fromParam toParam sinceParam = do
  window <- mkWindow fromParam toParam sinceParam
  hosts <- hostsFromRows <$> infraSnapshot pid window
  addRespHeaders
    $ detailFrom def{icon = Just "server", action = ESLink (infraUrl pid "/infrastructure/hosts" [] window) "Return to Hosts"} "Host not found in this time range" "Monoscope did not find this host in the current telemetry window. Return to Hosts to choose another host or time range." (hostDetail_ pid window)
    $ V.find ((== hostM) . Just . (.name)) hosts


hostDetail_ :: Projects.ProjectId -> TimePicker.TimeWindow -> HostRow -> Html ()
hostDetail_ pid window host = div_ [class_ "-mx-8 -mb-4 min-h-full"] do
  entityHeader_
    (faSprite_ "server" "solid" "h-4 w-4 text-iconNeutral")
    host.name
    ( a_ ([href_ containersUrl, class_ "btn btn-xs ms-auto gap-1.5 text-textBrand max-sm:h-11"] <> navTabAttrs) do
        faSprite_ "cube" "regular" "h-3.5 w-3.5"
        "View containers"
    )
    do
      forM_ metadata $ uncurry metaChip_
      forM_ host.integrations $ span_ [class_ "badge badge-sm badge-ghost"] . toHtml . integrationLabel
  nav_ [class_ "sticky top-0 z-20 border-b border-strokeWeak bg-bgRaised px-4 py-2", Aria.label_ "Host detail sections"] do
    div_ [class_ "flex gap-1 overflow-x-auto"] do
      sectionLink "host-summary" "server" "Summary"
      sectionLink "host-logs" "file-lines" "Recent logs"
      sectionLink "host-metrics" "chart-line" "Metrics"
  main_ [class_ "min-w-0 space-y-5 p-5 [&>*+*]:border-t [&>*+*]:border-strokeWeak [&>*+*]:pt-4"] do
    detailSection_ [id_ "host-summary"] "Host summary" (tonePill_ (bool WarningBadge SuccessBadge (availableSignals == 4)) $ "Metrics coverage: " <> show availableSignals <> " of 4") do
      factGrid_
        (bool "grid-cols-2 bg-bgBase max-sm:grid-cols-1" "grid-cols-6 bg-bgBase max-xl:grid-cols-3 max-sm:grid-cols-2" (availableSignals > 0))
        summaryFacts
      when (availableSignals > 0 && availableSignals < 4) $ callout_ WarningBadge Nothing do
        "Some host metrics are missing from this time range. Available signals remain visible below. "
        a_ [href_ "https://monoscope.tech/docs/sdks/infrastructure/", target_ "_blank", rel_ "noopener noreferrer", class_ "font-medium text-textBrand hover:underline"] "Set up host metrics"
    detailSection_ [id_ "host-logs"] "Recent logs" (a_ ([href_ logExplorerUrl, class_ "btn btn-xs max-sm:hidden"] <> navTabAttrs) "View logs in Explorer") do
      p_ [class_ "text-xs text-textWeak"] "Latest events reported by this host."
      div_ [class_ "flex flex-col max-h-64 overflow-auto rounded-lg border border-strokeWeak bg-bgBase max-sm:hidden"] $ Log.virtualTable pid (Just logDataUrl) Nothing
      div_ [class_ "hidden flex-col items-start gap-3 rounded-lg border border-strokeWeak bg-bgBase p-4 max-sm:flex"] do
        p_ [class_ "text-sm text-textWeak"] "Open this host in Explorer to search, filter, and inspect its logs."
        a_ ([href_ logExplorerUrl, class_ "btn btn-sm btn-outline max-sm:h-11"] <> navTabAttrs) "Open logs in Explorer"
    detailSection_ [id_ "host-metrics"] "Metrics" (when (availableSignals > 0) $ a_ ([href_ metricsUrl, class_ "btn btn-xs"] <> navTabAttrs) "View in Metrics")
      $ if availableSignals == 0
        then div_ [role_ "status"] do
          emptyState_
            def
              { icon = Just "chart-line"
              , action = ESCustom $ div_ [class_ "flex flex-wrap justify-center gap-2"] do
                  button_ ([type_ "button", class_ "btn btn-sm max-sm:h-11"] <> drawerLoadAttrs_ (hostDetailUrl pid window{TimePicker.fromQuery = Nothing, TimePicker.toQuery = Nothing, TimePicker.sinceQuery = Just "1H"} host.name)) "Try last 1 hour"
                  a_ [href_ "https://monoscope.tech/docs/sdks/infrastructure/", target_ "_blank", rel_ "noopener noreferrer", class_ "btn btn-sm btn-primary max-sm:h-11"] "Set up host metrics"
              }
            "No host metrics in this time range"
            ("This host reported no CPU, memory, filesystem, or load samples in the final " <> Containers.freshnessLabel window <> " of the range. Check that the collector's hostmetrics receiver is enabled.")
        else div_ [class_ "grid grid-cols-2 gap-3 max-xl:grid-cols-1"] $ forM_ hostWidgets $ div_ [class_ "min-h-56"] . Widget.widget_
  where
    metadata = [(label, value) | (label, Just value) <- [("Provider", host.provider), ("Region", host.region), ("OS", host.osType), ("Architecture", host.architecture)]]
    availableSignals = length $ catMaybes [host.cpuPct, host.memoryPct, host.storagePct, host.load1]
    summaryFacts
      | availableSignals == 0 = [("Containers", show host.containers)]
      | otherwise =
          [ ("CPU usage", pct host.cpuPct)
          , ("Memory usage", pct host.memoryPct)
          , ("Storage usage", pct host.storagePct)
          , ("Load (1m)", maybe "—" (showFFloat' 2) host.load1)
          , ("Uptime", maybe "—" formatUptime host.uptime)
          , ("Containers", show host.containers)
          ]
    pct = maybe "—" pctText
    query = Containers.kqlFilter [(Containers.nodeField, Just host.name)]
    logExplorerUrl = infraUrl pid "/log_explorer" [("query", query), ("source", "logs")] window
    logDataUrl = infraUrl pid "/log_explorer/data" [("query", query), ("source", "logs")] window
    metricsUrl = infraUrl pid "/metrics" [("metric_prefix", "system.")] window
    containersUrl = infraUrl pid "/infrastructure/containers" [("node", host.name)] window
    sectionLink anchor icon label = a_ [href_ $ "#" <> anchor, class_ "flex items-center gap-1.5 whitespace-nowrap rounded-md px-2.5 py-1.5 text-sm text-textWeak hover:bg-fillBrand-weak hover:text-textBrand max-sm:min-h-11"] $ faSprite_ icon "regular" "h-3.5 w-3.5" >> toHtml label
    hostWidgets =
      catMaybes
        [ widget host.cpuPct "host-cpu" "CPU usage" "%" $ "metrics | where metric_name == \"system.cpu.utilization\" and " <> query <> " and attributes.cpu.mode == \"idle\" | summarize 100 - avg(value) * 100 by bin_auto(timestamp)"
        , widget host.memoryPct "host-memory" "Memory usage" "bytes" $ "metrics | where metric_name == \"system.memory.usage\" and " <> query <> " and attributes.system.memory.state == \"used\" | summarize max(value) by bin_auto(timestamp)"
        , widget host.storagePct "host-storage" "Max storage usage" "%" $ "metrics | where metric_name == \"system.filesystem.utilization\" and " <> query <> " | summarize max(value) * 100 by bin_auto(timestamp)"
        , widget host.load1 "host-load" "Load average (1m)" "" $ "metrics | where metric_name == \"system.cpu.load_average.1m\" and " <> query <> " | summarize max(value) by bin_auto(timestamp)"
        ]
    widget signal ident title unit q = signal $> Widget.infraTimeseries pid ident title unit q


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
imagesFromRows = V.fromList . map build . groupRows (.image)
  where
    build (image, rows) =
      ImageRow
        { image
        , registry = imageRegistry image
        , tags = sortNub $ mapMaybe (.imageTag) rows
        , running = length rows
        , runtimes = sortNub $ map runtimeOf rows
        , cpuCores = sumPresent (.cpuCores) rows
        , memoryBytes = sumPresent (.memBytes) rows
        }
    -- A leading segment is a registry host only when it looks like one; otherwise it's a Docker Hub namespace.
    imageRegistry image = case T.breakOn "/" image of
      (registry, rest) | not (T.null rest), T.any (`elem` (".:" :: [Char])) registry || registry == "localhost" -> registry
      _ -> "Docker Hub"


imagesGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ImagesGet)
imagesGetH pid runtimeM registryM fromParam toParam sinceParam deferredM = do
  (_, _, bw) <- mkPageCtx pid
  window <- mkWindow fromParam toParam sinceParam
  let url = deferUrl pid "/infrastructure/images" [("runtime", runtimeM), ("registry", registryM)] window
  body <- withDeferredBody deferredM "imagesContainer" url (tableSkeleton_ 8) do
    allImages <- imagesFromRows <$> infraSnapshot pid window
    let images = V.filter (\image -> matchesAny runtimeM (map Containers.runtimeLabel image.runtimes) && matchesFilter registryM (Just image.registry)) allImages
    pure $ imagesTable pid window url runtimeM registryM images allImages
  addRespHeaders $ ImagesPage $ PageCtx (infrastructureBW pid "Images" window bw) body


newtype ImagesGet = ImagesPage (PageCtx (Deferred (Table ImageRow)))
  deriving (ToHtml) via (PageCtx (RefreshingDeferred (Table ImageRow)))


imagesTable :: Projects.ProjectId -> TimePicker.TimeWindow -> Text -> Maybe Text -> Maybe Text -> V.Vector ImageRow -> V.Vector ImageRow -> Table ImageRow
imagesTable pid window url runtimeM registryM images allImages =
  table
    { columns =
        [ col "Image" (\image -> div_ [class_ "flex items-center gap-2 min-w-0"] $ faSprite_ "layer-group" "solid" "h-3.5 w-3.5 shrink-0 text-iconNeutral" >> span_ [class_ "truncate font-medium text-textStrong"] (toHtml image.image)) & withAttrs [class_ "min-w-72 w-full"]
        , col "Running" (\image -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ show image.running) & withAttrs [class_ "w-24 text-right"]
        , col "Source" (\image -> span_ [class_ "whitespace-nowrap text-textWeak"] $ toHtml image.registry) & withAttrs [class_ "w-36"]
        , col "Tags" renderTags & withAttrs [class_ "w-64 max-lg:hidden"]
        , col "Runtime" (\image -> span_ [class_ "text-textWeak"] $ toHtml $ T.intercalate ", " $ map Containers.runtimeLabel image.runtimes) & withAttrs [class_ "w-28 max-xl:hidden"]
        , col "CPU (cores)" (plainCell . fmap (showFFloat' 3) . (.cpuCores)) & withAttrs [class_ "w-24 text-right"]
        , col "Memory" (plainCell . fmap formatBytes . (.memoryBytes)) & withAttrs [class_ "w-24 text-right"]
        , col "Security" (const $ span_ [class_ "inline-flex whitespace-nowrap rounded-md border border-strokeWeak bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak", term "data-tippy-content" "Connect an SBOM and vulnerability scanner to populate security findings"] "SBOM unavailable") & withAttrs [class_ "w-36 max-md:hidden"]
        ]
    , rows = images
    , features =
        table.features
          { rowAttrs = Just \image -> drawerRowAttrs_ $ infraUrl pid "/infrastructure/images/detail" [("image", image.image)] window
          , facets =
              Just
                $ facetActions
                  (infraUrl pid "/infrastructure/images" [] window)
                  "imagesContainer"
                  [ countedFilter "Runtime" "runtime" runtimeM (map Containers.runtimeLabel . (.runtimes)) allImages
                  , countedFilter "Registry" "registry" registryM (one . (.registry)) allImages
                  ]
          , zeroState = Just $ ZeroState "layer-group" "No container images reporting" "Images appear when container telemetry includes container.image.name." (ESLink "https://monoscope.tech/docs/sdks/infrastructure/kubernetes" "Container setup guide")
          , resultSummary = Just $ "Showing " <> show (V.length images) <> " of " <> show (V.length allImages) <> " images"
          , exportName = Just "container-images"
          }
    }
  where
    table = inventoryTable "images" url
    -- Two tags and a "+N" pill wrap rather than clip: a long tag used to cut the pill to "(".
    renderTags image = div_ [class_ "flex max-w-72 flex-wrap gap-1"] do
      forM_ (take 2 image.tags) $ \tag -> span_ [class_ "badge badge-xs badge-ghost shrink-0"] $ toHtml tag
      when (length image.tags > 2) $ span_ [class_ "badge badge-xs badge-outline shrink-0"] $ toHtml $ "+" <> show (length image.tags - 2)


imageDetailGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders DetailGet)
imageDetailGetH pid imageM fromParam toParam sinceParam = do
  window <- mkWindow fromParam toParam sinceParam
  images <- imagesFromRows <$> infraSnapshot pid window
  addRespHeaders
    $ detailFrom def{icon = Just "layer-group", action = ESNone} "This image is no longer present in the selected time range." "" (imageDetail_ pid window)
    $ V.find ((== imageM) . Just . (.image)) images


imageDetail_ :: Projects.ProjectId -> TimePicker.TimeWindow -> ImageRow -> Html ()
imageDetail_ pid window image = div_ [class_ "-mx-8 -mb-4 min-h-full"] do
  entityHeader_ (faSprite_ "layer-group" "solid" "h-4 w-4 text-iconNeutral") image.image pass do
    span_ [class_ "badge badge-sm badge-ghost"] $ toHtml image.registry
    forM_ image.runtimes $ \runtime -> span_ [class_ "badge badge-sm badge-ghost"] $ toHtml $ Containers.runtimeLabel runtime
  main_ [class_ "space-y-5 p-5 [&>*+*]:border-t [&>*+*]:border-strokeWeak [&>*+*]:pt-4"] do
    detailSection_ [] "Image summary" pass
      $ factGrid_
        "grid-cols-3 max-sm:grid-cols-1 max-sm:divide-x-0 max-sm:divide-y"
        [ ("Running containers", show image.running)
        , ("CPU used", maybe "—" Containers.coresText image.cpuCores)
        , ("Memory used", maybe "—" formatBytes image.memoryBytes)
        ]
    detailSection_ [] "Image tags" pass
      $ if null image.tags then emptyState_ def{size = ESCompact, icon = Just "hashtag"} "No image tags were reported." "" else div_ [class_ "flex flex-wrap gap-1.5"] $ forM_ image.tags $ \tag -> span_ [class_ "badge badge-sm badge-ghost"] $ toHtml tag
    detailSection_ [] "Security coverage" pass $ callout_ BrandBadge Nothing "No SBOM or vulnerability findings are connected for this image. Monoscope will not infer a clean result from missing scanner data."
    pivotLinks_ [("View containers", infraUrl pid "/infrastructure/containers" [("image", image.image)] window), ("View metrics", infraUrl pid "/metrics" [("metric_prefix", "container.")] window)]


-- | Constructor order is tab order: @[minBound ..]@ is what the resource tab strip renders,
-- so adding a view here adds it everywhere. 'display' is its URL param (@pods@, @clusters@, …).
--
-- >>> map display [minBound .. maxBound :: KubeResource]
-- ["pods","clusters","namespaces","nodes","workloads"]
data KubeResource = KubePods | KubeClusters | KubeNamespaces | KubeNodes | KubeWorkloads
  deriving stock (Bounded, Enum, Eq, Show)
  deriving (Display) via WrappedEnumSC 'Nothing "Kube" KubeResource


resourceLabel :: KubeResource -> Text
resourceLabel = \case
  KubePods -> "Pod"
  KubeClusters -> "Cluster"
  KubeNamespaces -> "Namespace"
  KubeNodes -> "Node"
  KubeWorkloads -> "Workload"


-- | Cluster- and node-scoped resources are machines; everything else is a workload unit.
kubeIcon :: KubeResource -> Text
kubeIcon resource = bool "cube" "server" (resource `elem` [KubeClusters, KubeNodes])


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
kubeRowsFromRows resource = V.fromList . map build . groupRows kubeIdentity . V.filter ((== Kubernetes) . runtimeOf)
  where
    kubeIdentity row = case resource of
      KubePods -> (,row.cluster,row.namespace,row.nodeName) <$> row.podName
      KubeClusters -> (,row.cluster,Nothing,Nothing) <$> row.cluster
      KubeNamespaces -> (,row.cluster,row.namespace,Nothing) <$> row.namespace
      KubeNodes -> (,row.cluster,Nothing,row.nodeName) <$> row.nodeName
      KubeWorkloads -> (,row.cluster,row.namespace,Nothing) <$> row.workload
    build ((name, cluster, namespace, node), rows) =
      let readyValues = mapMaybe (.ready) rows
          cpu = sumPresent (.cpuCores) rows
          cpuLimit = sumPresent (.cpuLimit) rows
          memory = sumPresent (.memBytes) rows
          memoryLimit = sumPresent (.memLimit) rows
       in KubeRow
            { name
            , status = maybe KubeUnknown (bool KubeReady KubeNotReady . any (<= 0)) (nonEmpty readyValues)
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


kubernetesGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders KubernetesGet)
kubernetesGetH pid resourceM clusterM namespaceM statusM fromParam toParam sinceParam deferredM = do
  (_, _, bw) <- mkPageCtx pid
  window <- mkWindow fromParam toParam sinceParam
  let resource = parseParam KubePods display resourceM
      url = deferUrl pid "/infrastructure/kubernetes" [("resource", resourceM), ("cluster", clusterM), ("namespace", namespaceM), ("status", statusM)] window
  body <- withDeferredBody deferredM "kubernetesContainer" url (tableSkeleton_ 8) do
    snapshot <- infraSnapshot pid window
    let allRows = kubeRowsFromRows resource snapshot
        rows = V.filter (\row -> matchesFilter clusterM row.cluster && matchesFilter statusM (Just $ kubeStatusLabel row.status)) $ kubeRowsFromRows resource $ V.filter (matchesFilter namespaceM . (.namespace)) snapshot
    pure $ kubernetesTable pid window url resource clusterM namespaceM statusM rows allRows
  addRespHeaders $ KubernetesPage $ PageCtx (infrastructureBW pid "Kubernetes" window bw) body


newtype KubernetesGet = KubernetesPage (PageCtx (Deferred (Table KubeRow)))
  deriving (ToHtml) via (PageCtx (RefreshingDeferred (Table KubeRow)))


kubernetesTable :: Projects.ProjectId -> TimePicker.TimeWindow -> Text -> KubeResource -> Maybe Text -> Maybe Text -> Maybe Text -> V.Vector KubeRow -> V.Vector KubeRow -> Table KubeRow
kubernetesTable pid window url resource clusterM namespaceM statusM rows allRows =
  table
    { columns =
        [ col (resourceLabel resource) (\row -> div_ [class_ "flex min-w-0 items-center gap-2"] $ faSprite_ (kubeIcon resource) "solid" "h-3.5 w-3.5 shrink-0 text-iconNeutral" >> span_ [class_ "min-w-0 truncate font-medium text-textStrong", term "data-tippy-content" row.name] (toHtml $ bool id clusterLabel (resource == KubeClusters) row.name)) & withAttrs [class_ "min-w-56 w-full"]
        , col "Status" (statusBadge . (.status)) & withAttrs [class_ "w-28"]
        , col "Cluster" (\row -> maybe Containers.emDash_ (\value -> span_ [class_ "block truncate whitespace-nowrap text-textStrong", term "data-tippy-content" value] $ toHtml $ clusterLabel value) row.cluster) & withAttrs [class_ "w-36 max-lg:hidden"]
        , col "Namespace" (plainCell . (.namespace)) & withAttrs [class_ "w-32 max-lg:hidden"]
        , col "Node" (plainCell . (.node)) & withAttrs [class_ "w-36 max-xl:hidden"]
        , col "Containers" (\row -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ show row.containers) & withAttrs [class_ "w-24 text-right"]
        , col "CPU (cores)" (plainCell . fmap (showFFloat' 3) . (.cpuCores)) & withAttrs [class_ "w-24 text-right"]
        , col "CPU limit used" (utilizationCell . (.cpuPct)) & withAttrs [class_ "w-36 max-md:hidden"]
        , col "Memory" (plainCell . fmap formatBytes . (.memoryBytes)) & withAttrs [class_ "w-24 text-right"]
        , col "Memory limit used" (utilizationCell . (.memoryPct)) & withAttrs [class_ "w-40 max-md:hidden"]
        , col "Restarts" (plainCell . fmap (showFFloat' 0) . (.restarts)) & withAttrs [class_ "w-20 text-right max-md:hidden"]
        ]
    , rows
    , features =
        table.features
          { searchPlaceholder = Just $ "Search " <> display resource
          , rowAttrs = Just $ drawerRowAttrs_ . kubeDetailUrl
          , tabs = Just $ TableTabs "Kubernetes resource" [(resourceLabel r <> "s", infraUrl pid "/infrastructure/kubernetes" ([("resource", display r)] <> mapMaybe sequenceA [("cluster", clusterM), ("namespace", namespaceM), ("status", statusM)]) window, r == resource) | r <- [minBound ..]]
          , resultSummary = Just $ "Showing " <> show (V.length rows) <> " resources"
          , exportName = Just $ "kubernetes-" <> display resource
          , facets =
              Just
                $ facetActions
                  (infraUrl pid "/infrastructure/kubernetes" [("resource", display resource)] window)
                  "kubernetesContainer"
                  [ countedFilter "Cluster" "cluster" clusterM (maybeToList . (.cluster)) allRows
                  , countedFilter "Namespace" "namespace" namespaceM (maybeToList . (.namespace)) allRows
                  , countedFilter "Status" "status" statusM (one . kubeStatusLabel . (.status)) allRows
                  ]
          , zeroState = Just $ ZeroState "cube" "No Kubernetes resources reporting" "Enable kubeletstats and k8s_cluster receivers to populate pods, workloads, and nodes." (ESLink "https://monoscope.tech/docs/sdks/infrastructure/kubernetes" "Kubernetes setup guide")
          }
    }
  where
    table = inventoryTable "kubernetes" url
    kubeDetailUrl row = infraUrl pid "/infrastructure/kubernetes/detail" ([("resource", display resource), ("name", row.name)] <> mapMaybe sequenceA [("cluster", row.cluster), ("namespace", row.namespace)]) window


kubernetesDetailGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders DetailGet)
kubernetesDetailGetH pid resourceM nameM clusterM namespaceM fromParam toParam sinceParam = do
  window <- mkWindow fromParam toParam sinceParam
  let resource = parseParam KubePods display resourceM
  rows <- kubeRowsFromRows resource <$> infraSnapshot pid window
  addRespHeaders
    $ detailFrom def{icon = Just "cube", action = ESNone} "This Kubernetes resource is no longer present in the selected time range." "" (kubernetesDetail_ pid window resource)
    $ V.find (\row -> Just row.name == nameM && matchesFilter clusterM row.cluster && matchesFilter namespaceM row.namespace) rows


kubernetesDetail_ :: Projects.ProjectId -> TimePicker.TimeWindow -> KubeResource -> KubeRow -> Html ()
kubernetesDetail_ pid window resource row = div_ [class_ "-mx-8 -mb-4 min-h-full"] do
  entityHeader_ (faSprite_ (kubeIcon resource) "solid" "h-4 w-4 text-iconNeutral") row.name (statusBadge row.status) $ forM_ metadata $ uncurry metaChip_
  main_ [class_ "space-y-5 p-5 [&>*+*]:border-t [&>*+*]:border-strokeWeak [&>*+*]:pt-4"] do
    detailSection_ [] (resourceLabel resource <> " summary") pass do
      factGrid_
        "grid-cols-5 max-lg:grid-cols-3 max-sm:grid-cols-2"
        [ ("Containers", show row.containers)
        , ("CPU", maybe "—" Containers.coresText row.cpuCores)
        , ("CPU / limit", maybe "—" pctText row.cpuPct)
        , ("Memory", maybe "—" formatBytes row.memoryBytes)
        , ("Restarts", maybe "—" (showFFloat' 0) row.restarts)
        ]
      when (isNothing row.cpuCores || isNothing row.memoryBytes) $ callout_ BrandBadge Nothing "Usage is incomplete in this time range. Enable the kubeletstats receiver's node, pod, and container metric groups to fill the missing signals."
    pivotLinks_
      $ [("View containers", infraUrl pid "/infrastructure/containers" ([("namespace", namespace)] <> [("cluster", cluster) | cluster <- toList row.cluster]) window) | namespace <- toList row.namespace]
      <> [("View logs", infraUrl pid "/log_explorer" [("query", kubeQuery)] window), ("View metrics", infraUrl pid "/metrics" [("metric_prefix", "k8s.")] window)]
  where
    metadata = [(label, value) | (label, Just value) <- [("Cluster", row.cluster), ("Namespace", row.namespace), ("Node", row.node), ("Workload", row.workload)]]
    kubeQuery =
      Containers.kqlFilter
        $ ( case resource of
              KubePods -> "resource.k8s.pod.name"
              KubeClusters -> Containers.clusterField
              KubeNamespaces -> "resource.k8s.namespace.name"
              KubeNodes -> "resource.k8s.node.name"
              KubeWorkloads -> "coalesce(resource.k8s.deployment.name, resource.k8s.statefulset.name, resource.k8s.daemonset.name, resource.k8s.job.name, resource.k8s.cronjob.name)"
          , Just row.name
          )
        : [(Containers.clusterField, row.cluster) | resource /= KubeClusters, isJust row.cluster]
          <> [("resource.k8s.namespace.name", row.namespace) | resource /= KubeNamespaces, isJust row.namespace]


-- | 'display' is the URL param.
--
-- >>> map display [minBound .. maxBound :: HostMapFill]
-- ["cpu","memory","storage"]
data HostMapFill = FillCPU | FillMemory | FillStorage
  deriving stock (Bounded, Enum, Eq, Show)
  deriving (Display) via WrappedEnumSC 'Nothing "Fill" HostMapFill


data HostMapData = HostMapData
  { pid :: Projects.ProjectId
  , window :: TimePicker.TimeWindow
  , fill :: HostMapFill
  , grouping :: HostGroup
  , filters :: HostFilters
  , allHosts :: V.Vector HostRow
  , groups :: [(Text, [HostRow])]
  }


newtype HostMapGet = HostMapPage (PageCtx (Deferred HostMapData))
  deriving (ToHtml) via (PageCtx (RefreshingDeferred HostMapData))


instance ToHtml HostMapData where
  toHtml = toHtmlRaw . hostMap_
  toHtmlRaw = toHtml


hostMapGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders HostMapGet)
hostMapGetH pid fillM groupM providerM regionM osM fromParam toParam sinceParam deferredM = do
  (_, _, bw) <- mkPageCtx pid
  window <- mkWindow fromParam toParam sinceParam
  let filters = HostFilters providerM regionM osM Nothing
      grouping = parseParam GroupNone hostGroupParam groupM
      url = deferUrl pid "/infrastructure/host-map" [("fill", fillM), ("group", groupM), ("provider", providerM), ("region", regionM), ("os", osM)] window
  body <- withDeferredBody deferredM "hostMapContainer" url hostMapSkeleton_ do
    allHosts <- hostsFromRows <$> infraSnapshot pid window
    let groups = groupRows (Just . hostGroupLabel grouping) $ applyHostFilters filters allHosts
    pure HostMapData{pid, window, fill = parseParam FillCPU display fillM, grouping, filters, allHosts, groups}
  addRespHeaders $ HostMapPage $ PageCtx (infrastructureBW pid "Host Map" window bw) body
  where
    hostMapSkeleton_ = div_ [class_ "flex min-h-full flex-col bg-bgBase", role_ "status", Aria.label_ "Loading host map"] do
      div_ [class_ "flex flex-wrap gap-3 border-b border-strokeWeak bg-bgRaised px-4 py-3"] $ replicateM_ 4 $ div_ [class_ "h-9 w-44 rounded-lg skeleton-shimmer"] ""
      div_ [class_ "m-4 flex flex-wrap gap-2 rounded-lg border border-strokeWeak bg-bgRaised p-3"] $ replicateM_ 18 $ div_ [class_ "h-11 w-10 skeleton-shimmer", style_ hexClipPath] ""


hostMap_ :: HostMapData -> Html ()
hostMap_ page = div_ [id_ "hostMapContainer", class_ "flex min-h-full flex-col bg-bgBase"] do
  timeRefreshListener_ "hostMapContainer" (deferUrl page.pid "/infrastructure/host-map" [("fill", Just $ display page.fill), ("group", Just $ hostGroupParam page.grouping), ("provider", page.filters.provider), ("region", page.filters.region), ("os", page.filters.osType)] page.window)
  div_ [class_ "flex flex-wrap items-center gap-3 border-b border-strokeWeak bg-bgRaised px-4 py-3 max-sm:grid max-sm:grid-cols-2"] do
    selectControl_ $ SelectControl "fill" "Fill by" (display page.fill) [(display f, case f of { FillCPU -> "CPU usage"; FillMemory -> "Memory usage"; FillStorage -> "Storage usage" }) | f <- [minBound ..]] mapUrl "hostMapContainer"
    selectControl_ $ SelectControl "group" "Group by" (hostGroupParam page.grouping) (map hostGroupOption [minBound ..]) mapUrl "hostMapContainer"
    div_ [class_ "ml-auto flex flex-wrap items-center gap-3 text-xs text-textWeak max-sm:col-span-2 max-sm:ml-0", Aria.label_ "Utilization legend"] do
      legend "bg-fillSuccess-strong" "Below 60%"
      legend "bg-fillWarning-strong" "60–85%"
      legend "bg-fillError-strong" "Above 85%"
      legend "bg-fillNeutral-strong" "No data"
  div_ [class_ "flex items-start gap-4 p-4 max-lg:flex-col"] do
    Facets.sidebar_
      . facetsConfig
      $ facetActions
        mapUrl
        "hostMapContainer"
        [ countedFilter "Provider" "provider" page.filters.provider (maybeToList . (.provider)) page.allHosts
        , countedFilter "Region" "region" page.filters.region (maybeToList . (.region)) page.allHosts
        , countedFilter "OS" "os" page.filters.osType (maybeToList . (.osType)) page.allHosts
        ]
    div_ [class_ "min-w-0 w-full flex-1"] do
      if null page.groups
        then
          div_ [class_ "m-auto"]
            $ emptyState_
              def{icon = Just "server", action = ESLink "https://monoscope.tech/docs/sdks/infrastructure/" "Set up host monitoring"}
              "No hosts reporting"
              "Enable host metrics or Kubernetes node telemetry to populate the map."
        else div_ [class_ "flex flex-wrap items-start gap-4"] $ forM_ page.groups \(label, hosts) ->
          let enlarged = length hosts <= 12
           in section_ [class_ $ "rounded-lg border border-strokeWeak bg-bgRaised p-3 shadow-sm " <> bool "min-w-80 flex-1" "w-fit min-w-72" enlarged] do
                h2_ [class_ "mb-3 flex items-center gap-2 text-sm font-semibold text-textStrong"] $ toHtml label >> span_ [class_ "badge badge-xs badge-ghost"] (toHtml $ show (length hosts) <> " hosts")
                div_ [class_ $ "flex flex-wrap " <> bool "gap-1" "gap-2" enlarged] $ forM_ (sortOn (.name) hosts) $ hostHex enlarged
  where
    mapUrl = infraUrl page.pid "/infrastructure/host-map" ([("fill", display page.fill), ("group", hostGroupParam page.grouping)] <> mapMaybe sequenceA [("provider", page.filters.provider), ("region", page.filters.region), ("os", page.filters.osType)]) page.window
    legend colour label = span_ [class_ "inline-flex items-center gap-1.5"] $ span_ [class_ $ "h-3 w-3 " <> colour, style_ hexClipPath] mempty >> toHtml label
    hostHex enlarged host = div_ [class_ $ "flex flex-col items-center gap-1 " <> bool "" "w-24" enlarged] do
      let value = case page.fill of FillCPU -> host.cpuPct; FillMemory -> host.memoryPct; FillStorage -> host.storagePct
      button_
        ( [ class_ $ "inline-flex items-center justify-center text-textInverse-strong transition-transform hover:-translate-y-0.5 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong motion-reduce:transform-none " <> utilizationClass value
          , type_ "button"
          , style_ $ bool "height:44px;width:40px;" "height:54px;width:49px;" enlarged <> hexClipPath
          , term "data-tippy-content" $ host.name <> " · " <> maybe "No data" pctText value
          , Aria.label_ $ host.name <> ", " <> (case page.fill of FillCPU -> "CPU usage"; FillMemory -> "memory usage"; FillStorage -> "storage usage") <> ": " <> maybe "no data" (\v -> showFFloat' 0 (v * 100) <> " percent") value
          ]
            <> drawerLoadAttrs_ (hostDetailUrl page.pid page.window host.name)
        )
        $ faSprite_ "server" "solid"
        $ bool "h-3 w-3" "h-4 w-4" enlarged
      when enlarged $ span_ [data_ "visible-host-label" host.name, class_ "w-full truncate text-center text-xs text-textWeak", Aria.hidden_ "true"] $ toHtml host.name


-- | The hexagon cutout shared by the host-map legend, skeleton, and cells.
hexClipPath :: Text
hexClipPath = "clip-path:polygon(25% 0,75% 0,100% 50%,75% 100%,25% 100%,0 50%)"


utilizationClass :: Maybe Double -> Text
utilizationClass = \case
  Nothing -> "bg-fillNeutral-strong"
  Just value | value < 0.6 -> "bg-fillSuccess-strong"
  Just value | value < 0.85 -> "bg-fillWarning-strong"
  Just _ -> "bg-fillError-strong"


sumPresent :: (a -> Maybe Double) -> [a] -> Maybe Double
sumPresent getter = viaNonEmpty sum . mapMaybe getter


plainCell :: Maybe Text -> Html ()
plainCell = maybe Containers.emDash_ (\value -> span_ [class_ "block truncate whitespace-nowrap text-textStrong tabular-nums", term "data-tippy-content" value] $ toHtml value)


-- | A cluster known only by its uid gets a compact label — 36 hex characters repeated down
-- a column identify nothing. A real name (a collector that sets @k8s.cluster.name@) passes
-- through untouched; the full uid stays in the cell tooltip.
--
-- >>> clusterLabel "13269e1e-e624-4d51-a30d-86524c5041f5"
-- "cluster-13269e1e"
-- >>> clusterLabel "prod-eu"
-- "prod-eu"
clusterLabel :: Text -> Text
clusterLabel value
  | isJust $ UUID.fromText value = "cluster-" <> T.take 8 value
  | otherwise = value


utilizationCell :: Maybe Double -> Html ()
utilizationCell = \case
  Nothing -> plainCell Nothing
  Just value -> div_ [class_ "flex min-w-24 items-center gap-1.5"] do
    span_ [class_ "w-10 shrink-0 text-right tabular-nums text-textStrong"] $ toHtml $ pctText value
    div_ [class_ "h-1.5 grow overflow-hidden rounded-full bg-fillWeak"] $ div_ [class_ $ "h-full rounded-full " <> utilizationClass (Just value), style_ $ "width:" <> pctText (min 1 value)] mempty


statusBadge :: KubeStatus -> Html ()
statusBadge = \case
  KubeReady -> Containers.readyBadge_ True
  KubeNotReady -> Containers.readyBadge_ False
  KubeUnknown -> span_ [class_ "badge badge-sm badge-ghost whitespace-nowrap"] "Unknown"


formatUptime :: Double -> Text
formatUptime seconds
  | seconds >= 86400 = showFFloat' 0 (seconds / 86400) <> "d"
  | seconds >= 3600 = showFFloat' 0 (seconds / 3600) <> "h"
  | otherwise = showFFloat' 0 (seconds / 60) <> "m"
