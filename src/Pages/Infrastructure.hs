-- | Infrastructure inventory views built from the same bounded OpenTelemetry snapshot.
-- Hosts, images, Kubernetes resources, and the host map are projections of that snapshot,
-- so a filter, table row, map cell, and inspector cannot disagree about identity or usage.
module Pages.Infrastructure (
  hostsGetH,
  hostDetailGetH,
  imagesGetH,
  kubernetesGetH,
  hostMapGetH,
  HostsGet (..),
  ImagesGet (..),
  KubernetesGet (..),
  HostMapGet (..),
  HostDetailGet (..),
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
import Pages.Containers qualified as Containers
import Pkg.Components.Table (Column, Config (..), Features (..), FilterMenu (..), FilterOption (..), SearchMode (..), Table (..), TableHeaderActions (..), ZeroState (..), col, withAttrs)
import Pkg.Components.Widget (WidgetType (WTTimeseriesLine))
import Pkg.Components.Widget qualified as Widget
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (drawerLoadAttrs_, faSprite_, infrastructureNavTabs_, popoverPanel_, popoverTrigger_, toUriStr)


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


hostsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders HostsGet)
hostsGetH pid providerM regionM osM integrationM groupM = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  snapshot <- containersInWindow appCtx.env.enableTimefusionReads pid now
  let filters = HostFilters providerM regionM osM integrationM
      hosts = applyHostFilters filters $ hostsFromRows snapshot
      grouping = parseHostGroup groupM
      table = hostsTable pid filters grouping hosts (hostsFromRows snapshot)
  addRespHeaders $ HostsPage $ PageCtx (infrastructureBW pid "Hosts" bw) table


newtype HostsGet = HostsPage (PageCtx (Table HostListRow))


instance ToHtml HostsGet where
  toHtml (HostsPage page) = toHtml page
  toHtmlRaw = toHtml


hostsTable :: Projects.ProjectId -> HostFilters -> HostGroup -> V.Vector HostRow -> V.Vector HostRow -> Table HostListRow
hostsTable pid filters grouping hosts allHosts =
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
          , header = Just $ hostGroupControl pid filters grouping (V.length hosts)
          , tableHeaderActions =
              Just
                TableHeaderActions
                  { baseUrl = hostBaseUrl pid grouping
                  , targetId = "hostsContainer"
                  , sortOptions = []
                  , currentSort = ""
                  , filterMenus =
                      [ FilterMenu
                          { label = "Provider"
                          , paramName = "provider"
                          , options = [FilterOption value value (Just value == filters.provider) | value <- facetValues (.provider) allHosts]
                          , multiSelect = False
                          }
                      , FilterMenu
                          { label = "Region"
                          , paramName = "region"
                          , options = [FilterOption value value (Just value == filters.region) | value <- facetValues (.region) allHosts]
                          , multiSelect = False
                          }
                      , FilterMenu
                          { label = "OS"
                          , paramName = "os"
                          , options = [FilterOption value value (Just value == filters.osType) | value <- facetValues (.osType) allHosts]
                          , multiSelect = False
                          }
                      , FilterMenu
                          { label = "Integration"
                          , paramName = "integration"
                          , options = [FilterOption (integrationLabel v) (integrationLabel v) (Just (integrationLabel v) == filters.integration) | v <- ordNub $ concatMap (.integrations) $ V.toList allHosts]
                          , multiSelect = False
                          }
                      ]
                  , activeFilters =
                      [ (label, [value])
                      | (label, valueM) <- [("Provider", filters.provider), ("Region", filters.region), ("OS", filters.osType), ("Integration", filters.integration)]
                      , value <- maybeToList valueM
                      , not $ T.null value
                      ]
                  , headerExtra = Just hostColumnPicker
                  }
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


hostBaseUrl :: Projects.ProjectId -> HostGroup -> Text
hostBaseUrl pid grouping = "/p/" <> pid.toText <> "/infrastructure/hosts" <> if grouping == GroupNone then "" else "?group=" <> hostGroupParam grouping


hostGroupControl :: Projects.ProjectId -> HostFilters -> HostGroup -> Int -> Html ()
hostGroupControl pid filters grouping count =
  div_ [class_ "flex flex-wrap items-center justify-between gap-2 border-b border-strokeWeak px-3 py-2"] do
    span_ [class_ "text-xs text-textWeak", role_ "status", Aria.live_ "polite"] $ toHtml $ show count <> " hosts"
    form_ [method_ "get", action_ $ "/p/" <> pid.toText <> "/infrastructure/hosts", class_ "flex items-center gap-2"] do
      forM_ ([("provider", filters.provider), ("region", filters.region), ("os", filters.osType), ("integration", filters.integration)] :: [(Text, Maybe Text)]) \(field, valueM) ->
        whenJust valueM \value -> input_ [type_ "hidden", name_ field, value_ value]
      label_ [Lucid.for_ "hosts-group", class_ "text-xs text-textWeak"] "Group by"
      select_ [id_ "hosts-group", name_ "group", class_ "select select-xs border-strokeWeak bg-bgBase", onchange_ "this.form.requestSubmit()"]
        $ forM_ ([("", "None"), ("provider", "Provider"), ("region", "Region"), ("os", "Operating system"), ("integration", "Integration")] :: [(Text, Text)]) \(value, label) ->
          option_ ([value_ value] <> [selected_ "" | value == hostGroupParam grouping]) $ toHtml label


hostColumnPicker :: Html ()
hostColumnPicker = do
  button_ ([class_ "btn btn-xs border border-strokeWeak bg-transparent font-normal text-textWeak shadow-none", type_ "button"] <> popoverTrigger_ "host-columns") do
    faSprite_ "table-columns" "regular" "h-3 w-3"
    "Columns"
  div_ (popoverPanel_ "host-columns" <> [class_ "dropdown dropdown-end z-50 mt-1 w-52 rounded-md border border-strokeWeak bg-bgRaised p-2 text-sm font-normal normal-case shadow-lg"]) do
    span_ [class_ "block px-2 pb-1 text-xs font-semibold text-textStrong"] "Metric columns"
    columnToggle "host-col-storage" "Storage" True
    columnToggle "host-col-load" "Load (1m)" True
    columnToggle "host-col-uptime" "Uptime" False
  where
    columnToggle ident label enabled = label_ [class_ "flex cursor-pointer items-center gap-2 rounded px-2 py-1.5 hover:bg-fillWeak"] do
      input_ $ [id_ ident, type_ "checkbox", class_ "checkbox checkbox-xs", onchange_ $ "document.querySelectorAll('." <> ident <> "').forEach(element => element.classList.toggle('hidden', !this.checked))"] <> [checked_ | enabled]
      toHtml label


hostColumns :: Projects.ProjectId -> [Column HostListRow]
hostColumns pid =
  [ col "Host" nameCell & withAttrs [class_ "min-w-52"]
  , col "Configuration" configCell & withAttrs [class_ "w-44 max-lg:hidden"]
  , col "System" systemCell & withAttrs [class_ "w-32 max-xl:hidden"]
  , col "CPU" (metricCell (.cpuPct)) & withAttrs [class_ "w-32"]
  , col "Memory" (metricCell (.memoryPct)) & withAttrs [class_ "w-32"]
  , col "Storage" (metricCell (.storagePct)) & withAttrs [class_ "host-col-storage w-32 max-lg:hidden"]
  , col "Load" (numberCell (.load1)) & withAttrs [class_ "host-col-load w-20 text-right max-xl:hidden"]
  , col "Uptime" uptimeCell & withAttrs [class_ "host-col-uptime hidden w-24 max-xl:hidden"]
  , col "Containers" containersCell & withAttrs [class_ "w-24 text-right"]
  , col "Integrations" integrationsCell & withAttrs [class_ "w-64 max-md:hidden"]
  ]
  where
    nameCell = \case
      HostGroupRow label count -> div_ [class_ "flex items-center gap-2 py-1 font-semibold text-textStrong"] $ toHtml label >> span_ [class_ "badge badge-xs badge-ghost"] (toHtml $ show count)
      HostItem host -> button_ ([class_ "flex items-center gap-2 font-medium text-textStrong hover:text-textBrand", type_ "button"] <> drawerLoadAttrs_ (hostDetailUrl pid host.name)) do
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
hostDetailGet_ HostDetailMissing = div_ [class_ "p-4 text-textWeak"] "This host is no longer reporting."
hostDetailGet_ (HostDetail pid host) = hostDetail_ pid host


hostDetailGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders HostDetailGet)
hostDetailGetH pid hostM = do
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  hosts <- hostsFromRows <$> containersInWindow appCtx.env.enableTimefusionReads pid now
  addRespHeaders $ maybe HostDetailMissing (HostDetail pid) $ V.find ((== hostM) . Just . (.name)) hosts


hostDetail_ :: Projects.ProjectId -> HostRow -> Html ()
hostDetail_ pid host = div_ [class_ "flex flex-col gap-4 p-4"] do
  header_ [class_ "flex flex-col gap-2 border-b border-strokeWeak pb-3"] do
    div_ [class_ "flex items-center gap-2"] do
      faSprite_ "server" "solid" "h-4 w-4 text-iconNeutral"
      h2_ [class_ "break-all text-lg font-semibold text-textStrong"] $ toHtml host.name
    div_ [class_ "flex flex-wrap gap-1"] $ forM_ host.integrations $ span_ [class_ "badge badge-sm badge-ghost"] . toHtml . integrationLabel
  dl_ [class_ "grid grid-cols-2 gap-x-4 gap-y-2 text-sm"] $ forM_ facts \(label, value) -> dt_ [class_ "text-textWeak"] (toHtml label) >> dd_ [class_ "break-all text-textStrong"] (toHtml value)
  div_ [class_ "grid grid-cols-2 gap-3 max-xl:grid-cols-1"] $ forM_ (hostWidgets pid host.name) $ div_ [style_ "min-height:220px"] . Widget.widget_
  div_ [class_ "flex flex-wrap gap-2"] do
    a_ ([href_ $ "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr ("resource.host.name==\"" <> host.name <> "\""), class_ "btn btn-sm btn-outline"] <> navTabAttrs) "View logs"
    a_ ([href_ $ "/p/" <> pid.toText <> "/metrics?metric_prefix=system.", class_ "btn btn-sm btn-outline"] <> navTabAttrs) "View metrics"
  where
    facts =
      [(label, value) | (label, Just value) <- [("Provider", host.provider), ("Region", host.region), ("Operating system", host.osType), ("Architecture", host.architecture)]]
        <> [("Containers", show host.containers)]


hostWidgets :: Projects.ProjectId -> Text -> [Widget.Widget]
hostWidgets pid host =
  [ widget "host-cpu" "CPU usage" "%" $ "metrics | where metric_name == \"system.cpu.utilization\" and resource.host.name == " <> quoted <> " and attributes.cpu.mode != \"idle\" | summarize sum(value) * 100 by bin_auto(timestamp)"
  , widget "host-memory" "Memory used" "bytes" $ "metrics | where metric_name == \"system.memory.usage\" and resource.host.name == " <> quoted <> " and attributes.system.memory.state == \"used\" | summarize max(value) by bin_auto(timestamp)"
  , widget "host-storage" "Max storage usage" "%" $ "metrics | where metric_name == \"system.filesystem.utilization\" and resource.host.name == " <> quoted <> " | summarize max(value) * 100 by bin_auto(timestamp)"
  , widget "host-load" "Load average (1m)" "" $ "metrics | where metric_name == \"system.cpu.load_average.1m\" and resource.host.name == " <> quoted <> " | summarize max(value) by bin_auto(timestamp)"
  ]
  where
    quoted = "\"" <> T.replace "\"" "\\\"" host <> "\""
    widget ident title unit query =
      (def :: Widget.Widget)
        { Widget.id = Just ident
        , Widget.wType = WTTimeseriesLine
        , Widget.title = Just title
        , Widget.query = Just query
        , Widget.unit = Just unit
        , Widget._projectId = Just pid
        , Widget.standalone = Just True
        , Widget.hideSubtitle = Just True
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


imagesGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ImagesGet)
imagesGetH pid runtimeM registryM = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  allImages <- imagesFromRows <$> containersInWindow appCtx.env.enableTimefusionReads pid now
  let images = V.filter (\image -> maybe True (\wanted -> any ((== wanted) . Containers.runtimeLabel) image.runtimes) runtimeM && maybe True (== image.registry) registryM) allImages
      table = imagesTable pid runtimeM registryM images allImages
  addRespHeaders $ ImagesPage $ PageCtx (infrastructureBW pid "Images" bw) table


newtype ImagesGet = ImagesPage (PageCtx (Table ImageRow))


instance ToHtml ImagesGet where
  toHtml (ImagesPage page) = toHtml page
  toHtmlRaw = toHtml


imagesTable :: Projects.ProjectId -> Maybe Text -> Maybe Text -> V.Vector ImageRow -> V.Vector ImageRow -> Table ImageRow
imagesTable pid runtimeM registryM images allImages =
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
          , tableHeaderActions =
              Just
                TableHeaderActions
                  { baseUrl = "/p/" <> pid.toText <> "/infrastructure/images"
                  , targetId = "imagesContainer"
                  , sortOptions = []
                  , currentSort = ""
                  , filterMenus =
                      [ FilterMenu
                          { label = "Runtime"
                          , paramName = "runtime"
                          , options = [FilterOption runtime runtime (Just runtime == runtimeM) | runtime <- Relude.sort $ ordNub $ map Containers.runtimeLabel $ concatMap (.runtimes) $ V.toList allImages]
                          , multiSelect = False
                          }
                      , FilterMenu
                          { label = "Registry"
                          , paramName = "registry"
                          , options = [FilterOption registry registry (Just registry == registryM) | registry <- facetValues (Just . (.registry)) allImages]
                          , multiSelect = False
                          }
                      ]
                  , activeFilters = [(label, [value]) | (label, valueM) <- [("Runtime", runtimeM), ("Registry", registryM)], value <- maybeToList valueM]
                  , headerExtra = Nothing
                  }
          , zeroState = Just $ ZeroState "layer-group" "No container images reporting" "Images appear when container telemetry includes container.image.name." "Container setup guide" (Right "https://monoscope.tech/docs/sdks/infrastructure/kubernetes")
          }
    }
  where
    renderTags image = div_ [class_ "flex max-w-72 flex-wrap gap-1"] do
      forM_ (take 3 image.tags) $ \tag -> span_ [class_ "badge badge-xs badge-ghost"] $ toHtml tag
      when (length image.tags > 3) $ span_ [class_ "badge badge-xs badge-outline"] $ toHtml $ "+" <> show (length image.tags - 3)


data KubeResource = KubePods | KubeWorkloads | KubeNodes
  deriving stock (Eq, Show)


parseKubeResource :: Maybe Text -> KubeResource
parseKubeResource = \case
  Just "workloads" -> KubeWorkloads
  Just "nodes" -> KubeNodes
  Just _ -> KubePods
  Nothing -> KubePods


kubeResourceParam :: KubeResource -> Text
kubeResourceParam = \case KubePods -> "pods"; KubeWorkloads -> "workloads"; KubeNodes -> "nodes"


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
      KubeWorkloads -> (,,,) <$> row.workload <*> pure row.cluster <*> pure row.namespace <*> pure Nothing
      KubeNodes -> (,,,) <$> row.nodeName <*> pure row.cluster <*> pure Nothing <*> pure row.nodeName
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
            , containers = length rows
            , cpuCores = cpu
            , cpuPct = ratio cpu cpuLimit
            , memoryBytes = memory
            , memoryPct = ratio memory memoryLimit
            , restarts = sumPresent (.restarts) rows
            }
    firstJust f = listToMaybe . mapMaybe f


kubernetesGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders KubernetesGet)
kubernetesGetH pid resourceM clusterM namespaceM statusM = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let resource = parseKubeResource resourceM
  allRows <- kubeRowsFromRows resource <$> containersInWindow appCtx.env.enableTimefusionReads pid now
  let rows = V.filter (\row -> matches clusterM row.cluster && matches namespaceM row.namespace && maybe True (\wanted -> wanted == kubeStatusLabel row.status) statusM) allRows
      table = kubernetesTable pid resource clusterM namespaceM statusM rows allRows
  addRespHeaders $ KubernetesPage $ PageCtx (infrastructureBW pid "Kubernetes" bw) table
  where
    matches selected actual = maybe True (\value -> T.null value || Just value == actual) selected


newtype KubernetesGet = KubernetesPage (PageCtx (Table KubeRow))


instance ToHtml KubernetesGet where
  toHtml (KubernetesPage page) = toHtml page
  toHtmlRaw = toHtml


kubernetesTable :: Projects.ProjectId -> KubeResource -> Maybe Text -> Maybe Text -> Maybe Text -> V.Vector KubeRow -> V.Vector KubeRow -> Table KubeRow
kubernetesTable pid resource clusterM namespaceM statusM rows allRows =
  Table
    { config = def{elemID = "kubernetesForm", containerId = Just "kubernetesContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
    , columns =
        [ col (case resource of KubePods -> "Pod"; KubeWorkloads -> "Workload"; KubeNodes -> "Node") (\row -> div_ [class_ "flex items-center gap-2"] $ faSprite_ (if resource == KubeNodes then "server" else "cube") "solid" "h-3.5 w-3.5 text-iconNeutral" >> span_ [class_ "font-medium text-textStrong"] (toHtml row.name)) & withAttrs [class_ "min-w-56 w-full"]
        , col "Status" (statusBadge . (.status)) & withAttrs [class_ "w-28"]
        , col "Cluster" (\row -> plainCell row.cluster) & withAttrs [class_ "w-36 max-lg:hidden"]
        , col "Namespace" (\row -> plainCell row.namespace) & withAttrs [class_ "w-32 max-lg:hidden"]
        , col "Node" (\row -> plainCell row.node) & withAttrs [class_ "w-36 max-xl:hidden"]
        , col "Containers" (\row -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ show row.containers) & withAttrs [class_ "w-24 text-right"]
        , col "CPU" (\row -> plainCell $ fmap (<> " cores") $ Containers.showFFloat' 2 <$> row.cpuCores) & withAttrs [class_ "w-24 text-right"]
        , col "CPU % lim" (utilizationCell . (.cpuPct)) & withAttrs [class_ "w-32 max-md:hidden"]
        , col "Memory" (\row -> plainCell $ Containers.formatBytes <$> row.memoryBytes) & withAttrs [class_ "w-24 text-right"]
        , col "Mem % lim" (utilizationCell . (.memoryPct)) & withAttrs [class_ "w-32 max-md:hidden"]
        , col "Restarts" (\row -> plainCell $ Containers.showFFloat' 0 <$> row.restarts) & withAttrs [class_ "w-20 text-right max-md:hidden"]
        ]
    , rows
    , features =
        def
          { search = Just ClientSide
          , header = Just $ kubeResourceNav pid resource (V.length rows)
          , tableHeaderActions =
              Just
                TableHeaderActions
                  { baseUrl = "/p/" <> pid.toText <> "/infrastructure/kubernetes?resource=" <> kubeResourceParam resource
                  , targetId = "kubernetesContainer"
                  , sortOptions = []
                  , currentSort = ""
                  , filterMenus =
                      [ FilterMenu
                          { label = "Cluster"
                          , paramName = "cluster"
                          , options = [FilterOption value value (Just value == clusterM) | value <- facetValues (.cluster) allRows]
                          , multiSelect = False
                          }
                      , FilterMenu
                          { label = "Namespace"
                          , paramName = "namespace"
                          , options = [FilterOption value value (Just value == namespaceM) | value <- facetValues (.namespace) allRows]
                          , multiSelect = False
                          }
                      , FilterMenu
                          { label = "Status"
                          , paramName = "status"
                          , options = [FilterOption value value (Just value == statusM) | value <- facetValues (Just . kubeStatusLabel . (.status)) allRows]
                          , multiSelect = False
                          }
                      ]
                  , activeFilters = [(label, [value]) | (label, valueM) <- [("Cluster", clusterM), ("Namespace", namespaceM), ("Status", statusM)], value <- maybeToList valueM]
                  , headerExtra = Nothing
                  }
          , zeroState = Just $ ZeroState "cube" "No Kubernetes resources reporting" "Enable kubeletstats and k8s_cluster receivers to populate pods, workloads, and nodes." "Kubernetes setup guide" (Right "https://monoscope.tech/docs/sdks/infrastructure/kubernetes")
          }
    }


kubeResourceNav :: Projects.ProjectId -> KubeResource -> Int -> Html ()
kubeResourceNav pid current count = div_ [class_ "flex flex-wrap items-center justify-between gap-2 border-b border-strokeWeak px-3 py-2"] do
  div_ [class_ "tabs tabs-box tabs-outline tabs-sm", role_ "tablist", Aria.label_ "Kubernetes resource"] $ forM_ [(KubePods, "Pods"), (KubeWorkloads, "Workloads"), (KubeNodes, "Nodes")] \(resource, label) ->
    a_ ([href_ $ "/p/" <> pid.toText <> "/infrastructure/kubernetes?resource=" <> kubeResourceParam resource, role_ "tab", class_ $ "tab" <> bool "" " tab-active" (resource == current)] <> navTabAttrs) $ toHtml label
  span_ [class_ "text-xs text-textWeak", role_ "status", Aria.live_ "polite"] $ toHtml $ show count <> " resources"


data HostMapFill = FillCPU | FillMemory | FillStorage
  deriving stock (Eq, Show)


parseHostMapFill :: Maybe Text -> HostMapFill
parseHostMapFill = \case Just "memory" -> FillMemory; Just "storage" -> FillStorage; Just _ -> FillCPU; Nothing -> FillCPU


hostMapFillParam :: HostMapFill -> Text
hostMapFillParam = \case FillCPU -> "cpu"; FillMemory -> "memory"; FillStorage -> "storage"


data HostMapData = HostMapData
  { pid :: Projects.ProjectId
  , fill :: HostMapFill
  , grouping :: HostGroup
  , groups :: [(Text, [HostRow])]
  }


newtype HostMapGet = HostMapPage (PageCtx HostMapData)


instance ToHtml HostMapGet where
  toHtml (HostMapPage page) = toHtml page
  toHtmlRaw = toHtml


instance ToHtml HostMapData where
  toHtml = toHtmlRaw . hostMap_
  toHtmlRaw = toHtml


hostMapGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders HostMapGet)
hostMapGetH pid fillM groupM providerM regionM osM = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  snapshot <- containersInWindow appCtx.env.enableTimefusionReads pid now
  let hosts = applyHostFilters (HostFilters providerM regionM osM Nothing) $ hostsFromRows snapshot
      grouping = parseHostGroup groupM
      groups = hostMapGroups grouping hosts
  addRespHeaders $ HostMapPage $ PageCtx (infrastructureBW pid "Host Map" bw) HostMapData{pid, fill = parseHostMapFill fillM, grouping, groups}


hostMapGroups :: HostGroup -> V.Vector HostRow -> [(Text, [HostRow])]
hostMapGroups GroupNone hosts = [("All hosts", V.toList hosts)]
hostMapGroups grouping hosts = M.toAscList $ V.foldl' (\acc host -> M.insertWith (<>) (groupValue host) [host] acc) M.empty hosts
  where
    groupValue host = case grouping of
      GroupProvider -> fromMaybe "Unknown provider" host.provider
      GroupRegion -> fromMaybe "Unknown region" host.region
      GroupOS -> fromMaybe "Unknown OS" host.osType
      GroupIntegration -> maybe "OpenTelemetry" integrationLabel $ listToMaybe $ drop 1 host.integrations


hostMap_ :: HostMapData -> Html ()
hostMap_ page = div_ [class_ "flex min-h-full flex-col"] do
  form_ [method_ "get", action_ $ "/p/" <> page.pid.toText <> "/infrastructure/host-map", class_ "flex flex-wrap items-end gap-3 border-b border-strokeWeak bg-bgBase px-4 py-3"] do
    mapSelect "fill" "Fill by" (hostMapFillParam page.fill) [("cpu", "CPU usage"), ("memory", "Memory usage"), ("storage", "Storage usage")]
    mapSelect "group" "Group by" (hostGroupParam page.grouping) [("", "None"), ("provider", "Provider"), ("region", "Region"), ("os", "Operating system"), ("integration", "Integration")]
    button_ [class_ "btn btn-sm btn-primary", type_ "submit"] "Apply"
    div_ [class_ "ml-auto flex flex-wrap items-center gap-3 text-xs text-textWeak", Aria.label_ "Utilization legend"] do
      legend "bg-fillSuccess-strong" "Below 60%"
      legend "bg-fillWarning-strong" "60–85%"
      legend "bg-fillError-strong" "Above 85%"
      legend "bg-fillWeak" "No data"
  if null page.groups
    then div_ [class_ "m-auto flex max-w-md flex-col items-center gap-2 p-8 text-center"] $ faSprite_ "server" "regular" "h-8 w-8 text-iconNeutral" >> h2_ [class_ "font-semibold text-textStrong"] "No hosts reporting" >> p_ [class_ "text-sm text-textWeak"] "Enable hostmetrics or Kubernetes node telemetry to populate the map."
    else div_ [class_ "grid auto-rows-min grid-cols-[repeat(auto-fit,minmax(18rem,1fr))] gap-4 p-4"] $ forM_ page.groups \(label, hosts) -> section_ [class_ "rounded-lg border border-strokeWeak bg-bgSunken p-3"] do
      h2_ [class_ "mb-3 flex items-baseline gap-2 text-sm font-semibold text-textStrong"] $ toHtml label >> span_ [class_ "text-xs font-normal text-textWeak"] (toHtml $ show (length hosts) <> " hosts")
      div_ [class_ "flex flex-wrap gap-1"] $ forM_ (sortOn (.name) hosts) $ hostHex page.pid page.fill
  where
    legend colour label = span_ [class_ "inline-flex items-center gap-1.5"] $ span_ [class_ $ "h-3 w-3 " <> colour, style_ "clip-path:polygon(25% 0,75% 0,100% 50%,75% 100%,25% 100%,0 50%)"] mempty >> toHtml label


mapSelect :: Text -> Text -> Text -> [(Text, Text)] -> Html ()
mapSelect field label current options = label_ [class_ "flex flex-col gap-1 text-xs text-textWeak"] do
  toHtml label
  select_ [name_ field, class_ "select select-sm min-w-44 border-strokeWeak bg-bgBase text-sm text-textStrong"] $ forM_ options \(value, title) -> option_ ([value_ value] <> [selected_ "" | value == current]) $ toHtml title


hostHex :: Projects.ProjectId -> HostMapFill -> HostRow -> Html ()
hostHex pid fill host =
  button_
    ( [ class_ $ "inline-flex h-11 w-10 items-center justify-center text-textInverse-strong transition-transform hover:-translate-y-0.5 focus-visible:outline-2 focus-visible:outline-offset-2 " <> utilizationClass value
      , type_ "button"
      , style_ "height:44px;width:40px;clip-path:polygon(25% 0,75% 0,100% 50%,75% 100%,25% 100%,0 50%)"
      , term "data-tippy-content" $ host.name <> " · " <> maybe "No data" (\v -> Containers.showFFloat' 0 (v * 100) <> "%") value
      , Aria.label_ $ host.name <> ", " <> fillLabel fill <> ": " <> maybe "no data" (\v -> Containers.showFFloat' 0 (v * 100) <> " percent") value
      ]
        <> drawerLoadAttrs_ (hostDetailUrl pid host.name)
    )
    $ faSprite_ "server" "solid" "h-3 w-3"
  where
    value = case fill of FillCPU -> host.cpuPct; FillMemory -> host.memoryPct; FillStorage -> host.storagePct


fillLabel :: HostMapFill -> Text
fillLabel = \case FillCPU -> "CPU usage"; FillMemory -> "memory usage"; FillStorage -> "storage usage"


utilizationClass :: Maybe Double -> Text
utilizationClass = \case
  Nothing -> "bg-bgRaised text-iconNeutral"
  Just value | value < 0.6 -> "bg-fillSuccess-strong"
  Just value | value < 0.85 -> "bg-fillWarning-strong"
  Just _ -> "bg-fillError-strong"


infrastructureBW :: Projects.ProjectId -> Text -> BWConfig -> BWConfig
infrastructureBW pid title bw = bw{prePageTitle = Just "Infrastructure", pageTitle = title, menuItem = Just "Infrastructure", navTabs = Just $ infrastructureNavTabs_ pid title}


facetValues :: (a -> Maybe Text) -> V.Vector a -> [Text]
facetValues getter = Relude.sort . ordNub . filter (not . T.null) . mapMaybe getter . V.toList


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
