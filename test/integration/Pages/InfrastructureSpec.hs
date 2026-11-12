-- | Infrastructure and telemetry pages end to end: OTLP container/host metrics → the
-- inventory, hosts, images, Kubernetes and host-map views and their drilldowns; spans →
-- service-map rollups and the trace view; the metrics catalogue, exemplars and KQL series;
-- and Prometheus scrape targets.
module Pages.InfrastructureSpec (spec) where

import Control.Exception (evaluate)
import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.ByteString.Base16 qualified as B16
import Data.Cache qualified as Cache
import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.List qualified as L
import Data.Map qualified as Map
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful.Error.Static (catchError)
import Hasql.Interpolate qualified as HI
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers (ContainerRow (..), ContainerSnapshotKey, Runtime (..), Scope (..), containersInWindow, cpuPctOfLimit, memPctOfLimit, runtimeOf)
import Models.Telemetry.ServiceGraph (Collapse (..), EdgeSample (..), EndpointRollupEdge (..), MapStats (..), NodeKind (..), ServiceEdge (..), ServiceGraph (..), ServiceNode (..), buildServiceGraph, drawnEdges, drawnNodes, endpointDependencyGraphForRange, projectsWithSpansInRange, rollupEndpointDependencyEdges, rollupServiceEdges, serviceGraphForRange, serviceMapFanout, serviceMapNodeCap, singletonLatency, upsertEndpointDependencyEdges, upsertServiceDependencyEdges)
import Models.Telemetry.Telemetry qualified as Telemetry
import Network.GRPC.Common.Protobuf (Proto (..))
import Numeric (showHex)
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Charts.Charts qualified as Charts
import Pages.Containers qualified as Containers
import Pages.Infrastructure (ImageRow (..), InfraIntegration (..), KubeRow (..), KubeStatus (..))
import Pages.Infrastructure qualified as Infrastructure
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.ServiceMap qualified as ServiceMap
import Pages.Settings (PrometheusForm (..))
import Pages.Settings qualified as Settings
import Pages.Telemetry qualified as Trace
import Pkg.Components.Table (Features (customizeColumns, exportName, resultSummary), Column (..), Table (..))
import Pkg.Components.Widget qualified as Widget
import Data.Effectful.Hasql (setTestTime)
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as MS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as PC
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as PM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Servant (ServerError (..), getResponse)
import Servant.API (ResponseHeader (..), lookupResponseHeader, parseUrlPiece)
import System.Config (AuthContext (..))
import System.Config qualified as Config
import Text.Read (read)
import Utils (formatUTC, lookupIcon)


spec :: Spec
spec = do
  around withTfChartResources do
    it "containers and hosts: one inventory for both runtimes, projected into hosts, images, Kubernetes and the host map, with drilldowns that keep the window" \tr -> do
      pid <- createTestProject tr "infrastructure"
      let page runtime namespace node image cluster = Containers.containersGetH pid runtime namespace node image cluster Nothing Nothing Nothing (Just "1")
          allContainers = page Nothing Nothing Nothing Nothing Nothing
          inWindow from to = rowsByName <$> runTestBg frozenTime tr (containersInWindow False pid (addUTCTime from frozenTime) (addUTCTime to frozenTime))
          tableOf h = call tr h >>= \(Containers.ContainersPage (PageCtx _ b)) -> deferredBody b
          names h = map (.row.containerName) . V.toList . (.rows) <$> tableOf h
          hostPage = Infrastructure.hostMapGetH pid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "1")

      -- Zero state, not an empty table.
      inWindow (-900) 0 `shouldReturn` []
      emptyPage@(Containers.ContainersPage (PageCtx emptyConf emptyBody)) <- call tr allContainers
      (V.null emptyConf.serviceOptions, emptyConf.pageTitle, emptyConf.prePageTitle) `shouldBe` (True, "Infrastructure", Nothing)
      (V.length . (.rows) <$> deferredBody emptyBody) `shouldReturn` 0
      renderT emptyPage `shouldContainAll` ["No containers reporting"]
      (renderT <$> call tr hostPage) >>= (`shouldContainAll` ["No hosts reporting", "Set up host monitoring"])

      key <- createTestAPIKey tr pid "containers-key"
      ingestInventory tr key
      byName <- inWindow (-900) 0
      -- The noise metric carries no container identity, and checkout's pod rollup loses to its container.
      map fst byName `shouldMatchList` ["checkout", "coredns", "postgres", "frontend-544d9b6f4-2xk7z", "vps-bare-01", "vps-d6d7e318", redpanda, swarm]
      let row n = maybe (fail $ "no container named " <> toString n) pure (lookup n byName)
      -- Node, image and workload live only in the resource blob (the JSONB accessor path).
      k8s <- row "checkout"
      (runtimeOf k8s, k8s.podName, k8s.namespace, k8s.nodeName, k8s.image, k8s.imageTag, k8s.workload, k8s.cpuCores, k8s.memBytes, k8s.restarts, cpuPctOfLimit k8s, memPctOfLimit k8s)
        `shouldBe` (Kubernetes, Just "checkout-7fb5b4f859-nlcjs", Just "default", Just "vps-d6d7e318", Just "ghcr.io/open-telemetry/demo", Just "2.2.0", Just "checkout", Just 0.5, Just 536870912, Just 3, Just 0.25, Just 0.5)
      -- Docker's percent-of-one-core lands on the same cores axis; its limit arrives on
      -- container.memory.usage.limit; no CPU limit or restarts; the tag is split off the image name.
      docker <- row redpanda
      (runtimeOf docker, docker.podName, docker.nodeName, docker.cpuCores, memPctOfLimit docker, cpuPctOfLimit docker, docker.restarts, docker.image, docker.imageTag)
        `shouldBe` (Docker, Nothing, Just "0ce201583b04", Just 0.8, Just 0.75, Nothing, Nothing, Just "docker.redpanda.com/redpandadata/redpanda", Just "v25.1.4")
      -- Every controller kind names the workload; container.memory.usage stands in for working_set.
      (((.workload) &&& (.memBytes)) <$> row "postgres") `shouldReturn` (Just "postgres", Just 16777216)
      -- Pod-only kubeletstats stands in for containers we cannot see.
      podOnly <- row "frontend-544d9b6f4-2xk7z"
      (runtimeOf podOnly, podOnly.scope, podOnly.cpuCores, podOnly.memBytes, podOnly.workload, podOnly.image) `shouldBe` (Kubernetes, ScopePod, Just 0.75, Just 268435456, Just "frontend", Nothing)
      -- 0.125 busy on each of four cores is 0.5 of a 4-core box (MAX over per-core series would
      -- read a core's 0.875 idle); the collector's own pod is never stamped on the node it measures.
      node <- row "vps-bare-01"
      (runtimeOf node, node.cpuCores, node.cpuLimit, cpuPctOfLimit node, node.memBytes, node.memLimit, memPctOfLimit node, node.podName, node.namespace, node.nodeName)
        `shouldBe` (Host, Just 0.5, Just 4, Just 0.125, Just 2147483648, Just 4294967296, Just 0.5, Nothing, Nothing, Just "vps-bare-01")
      -- Swarm sends no service attribute: the workload is read out of <service>.<slot>.<task>.
      ((runtimeOf &&& (.workload)) <$> row swarm) `shouldReturn` (Docker, Just "srv-captain--api")
      -- No CPU limit yields no percentage, never a fabricated 0%.
      (((.cpuLimit) &&& cpuPctOfLimit) <$> row "coredns") `shouldReturn` (Nothing, Nothing)

      containersPage@(Containers.ContainersPage (PageCtx _ body)) <- call tr allContainers
      table <- deferredBody body
      V.length table.rows `shouldBe` 8
      -- Columns follow what the inventory reports: all of them for this mix; under Docker alone, no pod,
      -- namespace, readiness or restarts column of dashes (nor limits it never sent).
      let columnNames = map \Column{name} -> name
      columnNames table.columns `shouldBe` ["Container", "Pod", "Namespace", "Node", "CPU cores", "CPU limit", "Memory", "Memory limit", "Restarts", "Ready" :: Text]
      dockerPid <- createTestProject tr "docker only"
      dockerKey <- createTestAPIKey tr dockerPid "docker-key"
      ingestMetric tr dockerKey (dockerResource "solo") [] "container.cpu.utilization" 40 frozenTime
      columnNames . (.columns) <$> tableOf (Containers.containersGetH dockerPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "1")) `shouldReturn` ["Container", "Node", "CPU cores", "Memory" :: Text]
      (V.length <<$>> Cache.lookup tr.trATCtx.infrastructureCache ((pid, Nothing, Nothing, Just "5M") :: ContainerSnapshotKey)) `shouldReturn` Just 8
      let html = renderT containersPage
          render features = renderT table{features}
          customizeOnly = render table.features{resultSummary = Nothing, exportName = Nothing, customizeColumns = True}
          exportOnly = render table.features{resultSummary = Nothing, exportName = Just "containers", customizeColumns = False}
      html `shouldContainAll` ["checkout", redpanda, "kube-system", "otel-demo", "vps-bare-01", "Search containers", "CPU limit used", "Memory limit used", "Common Filters", "Last 5 mins", "Not ready", "Latest cumulative restart count reported within the selected time range. This is not the number of restarts during the range."]
      (T.count "data-component=\"facet-section\" open>" html, map (`T.isInfixOf` html) ["group/summary", "Usage over time", "min-height:", "data-widget="]) `shouldBe` (6, [False, False, False, False])
      (map (`T.isInfixOf` customizeOnly) [">Export</button>", "Reset columns", "[data-forced]:not(:checked)) [data-column-key=\"Pod\"]{display:none}"], map (`T.isInfixOf` exportOnly) ["Customize", "containers.csv"]) `shouldBe` ([False, True, True], [False, True])
      sinceOf "/infrastructure/containers/detail" html `shouldBe` [Just "5M"]
      -- "Not ready" wrapped across the badge border; the export icon is the correctly framed symbol.
      goldenParts "infrastructure-containers"
        . ordNub
        $ tagsWith ["data-header-actions", "data-live-mode=", "data-component=\"facet-rail\"", "value=\"otel-demo\"", "facet-count", "w-full max-w-0 min-w-64 overflow-hidden", "hidden max-md:inline-flex", "bg-fillWarning-strong", "badge-error", "M288 32c0-17.7", "Runtime: kubernetes", "Container: checkout", "Image: demo:2.2.0", "Workload: checkout", "CPU limit used: 25%", "Memory limit used: 50%", "Latest reported readiness: Ready"] html
        <> tagsWith ["popovertarget=\"containersForm-columns\""] customizeOnly
        <> tagsWith ["containers.csv"] exportOnly

      -- Facets narrow independently; an unknown value is empty, never "all". Cluster prefers the
      -- name and falls back to the uid; a bare node is its own runtime.
      traverse
        (fmap sort . names)
        [ page Nothing (Just "default") Nothing Nothing Nothing
        , page (Just "docker") Nothing Nothing Nothing Nothing
        , page (Just "kubernetes") Nothing Nothing Nothing Nothing
        , page Nothing Nothing (Just "0ce201583b04") Nothing Nothing
        , page Nothing (Just "no-such-namespace") Nothing Nothing Nothing
        , page Nothing Nothing Nothing Nothing (Just "otel-demo")
        , page Nothing Nothing Nothing Nothing (Just clusterUid)
        , page (Just "host") Nothing Nothing Nothing Nothing
        ]
        `shouldReturn` map
          sort
          [["checkout"], [swarm, redpanda], ["checkout", "coredns", "postgres", "frontend-544d9b6f4-2xk7z"], [swarm, redpanda], [], ["checkout"], ["coredns", "postgres", "frontend-544d9b6f4-2xk7z"], ["vps-bare-01", "vps-d6d7e318"]]

      -- The same telemetry projected into hosts. The in-cluster agent's system.* series (host.name
      -- = its own pod, node only in k8s.node.name) lands on the node it measures: 0.5 of 2 cores.
      hosts@(Infrastructure.HostsPage (PageCtx hostsBW hostsBody)) <- call tr $ Infrastructure.hostsGetH pid Nothing Nothing Nothing Nothing (Just "integration") Nothing Nothing Nothing (Just "1")
      (V.null hostsBW.serviceOptions, hostsBW.pageTitle, hostsBW.prePageTitle) `shouldBe` (True, "Infrastructure", Nothing)
      hostRows <- deferredBody hostsBody
      sort [(r.name, r.cpuPct, r.containers, r.integrations) | r <- V.toList hostRows.rows]
        `shouldBe` [("0ce201583b04", Nothing, 2, [OpenTelemetryIntegration, DockerIntegration]), ("vps-bare-01", Just 0.125, 0, [OpenTelemetryIntegration]), ("vps-d6d7e318", Just 0.25, 4, [OpenTelemetryIntegration, KubernetesIntegration])]
      let hostsHtml = renderT hosts
      hostsHtml `shouldContainAll` ["Infrastructure", "Hosts", "vps-bare-01", "Kubernetes", "Docker", "Storage", "Load (1m)", "Group by", "Customize", "Last 5 mins", "Previous time window", "Pause live updates", "Refresh interval", "Turn off automatic refresh", "Export", "Showing 3 of 3 hosts"]
      (T.count "role=\"button\"" (renderT hostRows), T.count "<table" (renderT hostRows), T.isInfixOf "otel-collector-77cd857677-qb24l" hostsHtml) `shouldBe` (3, 1, False)
      images <- call tr $ Infrastructure.imagesGetH pid Nothing Nothing Nothing Nothing Nothing (Just "1")
      kubernetes <- call tr $ Infrastructure.kubernetesGetH pid (Just "pods") Nothing Nothing Nothing Nothing Nothing Nothing (Just "1")
      hostMap <- call tr $ Infrastructure.hostMapGetH pid (Just "storage") Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "1")
      let Infrastructure.ImagesPage (PageCtx _ imagesBody) = images
          Infrastructure.KubernetesPage (PageCtx _ kubeBody) = kubernetes
      ((\t -> sort [(r.image, r.tags, r.running, r.runtimes) | r <- V.toList t.rows]) <$> deferredBody imagesBody)
        `shouldReturn` [("docker.redpanda.com/redpandadata/redpanda", ["v25.1.4"], 2, [Docker]), ("ghcr.io/open-telemetry/demo", ["2.2.0"], 3, [Kubernetes])]
      ((\t -> sort [(r.name, r.status, r.cluster, r.namespace, r.node, r.workload) | r <- V.toList t.rows]) <$> deferredBody kubeBody)
        `shouldReturn` [ ("checkout-7fb5b4f859-nlcjs", KubeReady, Just "otel-demo", Just "default", Just "vps-d6d7e318", Just "checkout")
                       , ("coredns-c6d9fc49c-bj5sw", KubeNotReady, Just clusterUid, Just "kube-system", Just "vps-d6d7e318", Just "coredns")
                       , ("frontend-544d9b6f4-2xk7z", KubeUnknown, Just clusterUid, Just "shop", Just "vps-d6d7e318", Just "frontend")
                       , ("postgres-0", KubeUnknown, Just clusterUid, Just "data", Just "vps-d6d7e318", Just "postgres")
                       ]
      renderT images `shouldContainAll` ["Images", "SBOM unavailable"]
      -- A cluster known only by uid renders a compact label; the full uid stays in the tooltip.
      renderT kubernetes `shouldContainAll` ["Pods", "Clusters", "Namespaces", "Workloads", "Nodes", "CPU limit", "Memory limit", "Showing ", " resources", "cluster-9b1deb4d"]
      renderT hostMap `shouldContainAll` ["Host Map", "Fill by", "Group by", "vps-bare-01", "Provider", "Region", "OS"]
      goldenParts "infrastructure-views"
        . ordNub
        $ tagsWith ["data-table-group=", "name=\"group\"", "data-column-key=\"CPU\"", "data-refresh-interval-menu", "ms-auto flex shrink-0", "grid w-full grid-cols-[minmax(0,1fr)_auto]", "data-nav-tab-strip", "data-header-actions"] hostsHtml
        <> tagsWith ["data-tippy-content=\"" <> clusterUid, "badge-error"] (renderT kubernetes)
        <> tagsWith ["bg-fillNeutral-strong\" style=\"clip-path", "name=\"fill\"", "data-component=\"facet-rail\""] (renderT hostMap)
      [sinceOf path h | (path, h) <- [("/infrastructure/hosts/detail", hostsHtml), ("/infrastructure/images/detail", renderT images), ("/infrastructure/kubernetes/detail", renderT kubernetes)]] `shouldBe` replicate 3 [Just "5M"]
      (T.isInfixOf ">Apply</button>" (renderT hostMap), map isJust [lookupIcon "solid" "download", lookupIcon "regular" "cube"]) `shouldBe` (False, [True, True])

      hostDetail <- shellHtml tr $ Infrastructure.hostDetailGetH pid (Just "vps-bare-01") Nothing Nothing Nothing
      hostDetail `shouldContainAll` ["Host summary", "CPU usage", "Memory usage", "Recent logs", "View logs in Explorer", "View in Metrics", "Metrics coverage: 2 of 4"]
      (lookup "query" <$> linkParams "/log_explorer/data" hostDetail) `shouldBe` [Just "coalesce(resource.k8s.node.name, resource.host.name)==\"vps-bare-01\""]
      (shellHtml tr (Infrastructure.imageDetailGetH pid (Just "ghcr.io/open-telemetry/demo") Nothing Nothing Nothing)) >>= (`shouldContainAll` ["Image summary", "Running containers", "Security coverage", "View containers"])
      (shellHtml tr (Infrastructure.kubernetesDetailGetH pid (Just "pods") (Just "checkout-7fb5b4f859-nlcjs") (Just "otel-demo") (Just "default") Nothing Nothing Nothing)) >>= (`shouldContainAll` ["Pod summary", "CPU / limit", "View containers", "View logs", "View metrics"])

      -- Drilldowns keep the selected window, relative or absolute.
      let window path h = ordNub [(lookup "since" ps, lookup "from" ps, lookup "to" ps) | ps <- linkParams path h]
          rel = [(Just "24H", Nothing, Nothing)]
          historical = [(Nothing, Just "2024-12-31T23:00:00Z", Just "2025-01-01T00:00:00Z")]
          prefixOf h = ordNub $ mapMaybe (lookup "metric_prefix") (linkParams "/metrics" h)
      containerHtml <- shellHtml tr $ Containers.containerDetailGetH pid (Just "checkout") (Just "checkout-7fb5b4f859-nlcjs") Nothing Nothing Nothing Nothing Nothing Nothing (Just "24H")
      historicalContainer <- shellHtml tr $ Containers.containerDetailGetH pid (Just "checkout") (Just "checkout-7fb5b4f859-nlcjs") Nothing Nothing Nothing Nothing (Just "2024-12-31T23:00:00Z") (Just "2025-01-01T00:00:00Z") Nothing
      hostHtml <- shellHtml tr $ Infrastructure.hostDetailGetH pid (Just "vps-bare-01") Nothing Nothing (Just "24H")
      historicalHost <- shellHtml tr $ Infrastructure.hostDetailGetH pid (Just "vps-bare-01") (Just "2024-12-31T23:00:00Z") (Just "2025-01-01T00:00:00Z") Nothing
      imageHtml <- shellHtml tr $ Infrastructure.imageDetailGetH pid (Just "ghcr.io/open-telemetry/demo") Nothing Nothing (Just "24H")
      kubeHtml <- shellHtml tr $ Infrastructure.kubernetesDetailGetH pid (Just "pods") (Just "checkout-7fb5b4f859-nlcjs") (Just "otel-demo") (Just "default") Nothing Nothing (Just "24H")
      let windows = map (uncurry window) :: [(Text, Text)] -> [[(Maybe Text, Maybe Text, Maybe Text)]]
      windows [("/log_explorer", containerHtml), ("/metrics", containerHtml), ("/log_explorer", historicalContainer), ("/metrics", historicalContainer)] `shouldBe` [rel, rel, historical, historical]
      windows [("/infrastructure/containers", hostHtml), ("/metrics", hostHtml), ("/log_explorer", hostHtml), ("/log_explorer/data", hostHtml), ("/infrastructure/containers", historicalHost)] `shouldBe` [rel, rel, rel, rel, historical]
      windows [("/infrastructure/containers", imageHtml), ("/metrics", imageHtml), ("/infrastructure/containers", kubeHtml), ("/log_explorer", kubeHtml), ("/metrics", kubeHtml)] `shouldBe` replicate 5 rel
      map prefixOf [containerHtml, historicalContainer, hostHtml, imageHtml, kubeHtml] `shouldBe` [["k8s."], ["k8s."], ["system."], ["container."], ["k8s."]]
      (map (lookup "node") (linkParams "/infrastructure/containers" hostHtml), map (lookup "namespace") (linkParams "/infrastructure/containers" kubeHtml), isJust . lookup "image" <$> linkParams "/infrastructure/containers" imageHtml)
        `shouldBe` ([Just "vps-bare-01"], [Just "default"], [True])
      containerHtml `shouldContainAll` ["Requests and limits", "vps-d6d7e318", "View logs", "resource.k8s.pod.name"]
      goldenParts "infrastructure-container-drawer" . ordNub $ tagsWith ["Pod: checkout-7fb5b4f859-nlcjs", "widget-subtitle"] containerHtml
      ((\w -> (Widget.id w, Widget.hideValue w, Widget.standalone w, Widget.legendPosition w)) <$> renderedWidget containerHtml) `shouldReturn` (Just "container-detail-cpu", Just True, Just True, Just "top-right")
      (shellHtml tr (Containers.containerDetailGetH pid (Just "ghost") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)) >>= (`shouldContainAll` ["no longer reporting"])

      -- The first request renders chrome and a self-fetching skeleton, not rows.
      firstLoads <-
        sequence
          [ shellHtml tr $ Infrastructure.hostsGetH pid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          , shellHtml tr $ Containers.containersGetH pid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          , shellHtml tr $ Infrastructure.kubernetesGetH pid (Just "pods") (Just "otel-demo") (Just "default") Nothing Nothing Nothing Nothing Nothing
          , shellHtml tr $ Infrastructure.imagesGetH pid Nothing Nothing Nothing Nothing Nothing Nothing
          , shellHtml tr $ Infrastructure.hostMapGetH pid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          ]
      map (\h -> map (`T.isInfixOf` h) ["vps-bare-01", "checkout-7fb5b4f859-nlcjs", "data-visible-host-label"]) firstLoads `shouldBe` replicate 5 [False, False, False]
      goldenParts "infrastructure-skeletons" $ concatMap (ordNub . tagsWith ["hx-trigger=\"load, update-query from:window\"", "data-deferred-shell", "Loading host map"]) firstLoads

      -- Host CPU charts normalise across logical cores and repeated samples; a host with no idle
      -- series (or no host metrics at all) gets the empty state, not a chart.
      forM_ ([-60, -120] :: [NominalDiffTime]) \offset -> forM_ (["cpu0", "cpu1", "cpu2", "cpu3"] :: [Text]) \core -> forM_ ([("idle", 0.875), ("user", 0.125)] :: [(Text, Double)]) \(mode, value) ->
        ingestMetric tr key [mkAttr "host.name" "four-core-host"] [mkAttr "cpu.logical_number" core, mkAttr "cpu.mode" mode] "system.cpu.utilization" value (addUTCTime offset frozenTime)
      ingestMetric tr key [mkAttr "host.name" "host-without-idle"] [mkAttr "cpu.logical_number" "cpu0", mkAttr "cpu.mode" "user"] "system.cpu.utilization" 0.5 (addUTCTime (-60) frozenTime)
      forM_ ([("four-core-host", Just 12.5), ("vps-d6d7e318", Just 25), ("host-without-idle", Nothing), ("0ce201583b04", Nothing)] :: [(Text, Maybe Double)]) \(host, expected) -> do
        detail <- shellHtml tr $ Infrastructure.hostDetailGetH pid (Just host) Nothing Nothing (Just "2H")
        case expected of
          Just pct -> chartPlotsOnly tr pid "host-cpu" pct detail
          Nothing -> do
            detail `shouldContainAll` ["No host metrics in this time range", "Try last 1 hour", "Set up host metrics", "View logs", "Metrics coverage: 0 of 4"]
            detail `shouldNotSatisfy` T.isInfixOf "host-cpu"

      -- Kubernetes log drilldowns keep namespace and cluster, falling back to the uid.
      let otherCluster = "8b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d"
      forM_ (zip ([1 ..] :: [Int]) [("scope-a", clusterUid), ("scope-b", clusterUid), ("scope-a", otherCluster)]) \(i, (namespace, cluster)) -> do
        let resource = [mkAttr "k8s.pod.name" "same-pod", mkAttr "k8s.container.name" ("scope-" <> show i), mkAttr "k8s.namespace.name" namespace, mkAttr "k8s.cluster.uid" cluster]
        ingestMetric tr key resource [] "container.cpu.usage" 0.5 (addUTCTime (-60) frozenTime)
        ingestSpanReq tr $ mkSpanRequest (T.justifyRight 32 '0' $ show i) (T.justifyRight 16 '0' $ show i) Nothing "kubernetes event" [] Nothing [] (mkResource key resource) (addUTCTime (-60) frozenTime)
      forM_ ([("clusters", clusterUid, Nothing, 2), ("pods", "same-pod", Just "scope-a", 1)] :: [(Text, Text, Maybe Text, Double)]) \(resource, name, namespace, expected) -> do
        detail <- shellHtml tr $ Infrastructure.kubernetesDetailGetH pid (Just resource) (Just name) (Just clusterUid) namespace Nothing Nothing (Just "3H")
        logLinkCount tr pid (Just Hasql.SqlPostgres) detail `shouldReturn` Just expected

      -- The pivot reads a freshness slice at the END of the window (scanning a day's worth of
      -- datapoints cost the statement timeout), so a container gone for 3h does not linger, yet a
      -- window ending when it reported still shows it.
      ingestMetric tr key (dockerResource "long-gone") [] "container.cpu.usage" 9 (addUTCTime (-10800) frozenTime)
      dayRows <- inWindow (-86400) 0
      thenRows <- inWindow (-86400) (-10800)
      (elem "long-gone" (map fst dayRows), elem "long-gone" (map fst thenRows)) `shouldBe` (False, True)
      -- A restart is a new series with the same identity; the newest sample wins, not a MAX.
      forM_ ([("old-id", 9, -120), ("new-id", 2, -60)] :: [(Text, Double, NominalDiffTime)]) \(cid, cpu, offset) ->
        ingestMetric tr key (mkAttr "container.id" cid : k8sResource "restart" "restart-pod" "restarted") [] "container.cpu.usage" cpu (addUTCTime offset frozenTime)
      ((.cpuCores) <<$>> lookup "restarted" <$> inWindow (-900) 0) `shouldReturn` Just (Just 2)
      -- The summary names the effective slice at the end of the selected window.
      forM_ ([("summary-recent", -15), ("summary-ten-min", -600), ("summary-historical", -10815)] :: [(Text, NominalDiffTime)]) \(name, offset) ->
        ingestMetric tr key (k8sResource "summary-window" name name) [] "container.cpu.usage" 1 (addUTCTime offset frozenTime)
      Cache.purge tr.trATCtx.infrastructureCache
      forM_
        ( [ (Nothing, Nothing, Just "5M", "5m", ["summary-recent"])
          , (Nothing, Nothing, Just "24H", "15m", ["summary-recent", "summary-ten-min"])
          , (Just "2024-12-31T20:00:00Z", Just "2024-12-31T21:00:00Z", Nothing, "15m", ["summary-historical"])
          , (Nothing, Nothing, Just "30S", "30s", ["summary-recent"])
          ]
            :: [(Maybe Text, Maybe Text, Maybe Text, Text, [Text])]
        )
        \(from, to, since, duration, expected) -> do
          summary@(Containers.ContainersPage (PageCtx _ result)) <- call tr $ Containers.containersGetH pid Nothing (Just "summary-window") Nothing Nothing Nothing from to since (Just "1")
          (map (.row.containerName) . V.toList . (.rows) <$> deferredBody result) >>= (`shouldMatchList` expected)
          renderT summary `shouldContainAll` ["containers reporting in the final " <> duration <> " of this range"]

    it "same-named containers in different clusters, namespaces and nodes stay distinct through every drilldown" \tr -> do
      pid <- createTestProject tr "container-scopes"
      key <- createTestAPIKey tr pid "container-scopes"
      let otherCluster = "8b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d"
          ago = addUTCTime (-60) frozenTime
          resource namespace cluster node container = [mkAttr "k8s.pod.name" "shared-pod", mkAttr "k8s.container.name" container, mkAttr "k8s.namespace.name" namespace, mkAttr "k8s.cluster.uid" cluster, mkAttr "k8s.node.name" node]
          span' i attrs = ingestSpanReq tr $ mkSpanRequest ("c" <> T.justifyRight 31 '0' (show (i :: Int))) ("c" <> T.justifyRight 15 '0' (show i)) Nothing "container event" [] Nothing [] (mkResource key attrs) ago
      forM_ (zip [1 ..] ([("scope-a", clusterUid, "scope-node-a", 1), ("scope-b", clusterUid, "scope-node-a", 3), ("scope-a", otherCluster, "scope-node-a", 5), ("scope-a", clusterUid, "scope-node-b", 7)] :: [(Text, Text, Text, Double)])) \(i, (namespace, cluster, node, value)) -> do
        let attrs = resource namespace cluster node "duplicate-api"
        forM_ ([-60, -120] :: [NominalDiffTime]) \offset -> ingestMetric tr key attrs [] "container.cpu.usage" value (addUTCTime offset frozenTime)
        span' i attrs
      ingestMetric tr key (resource "scope-a" clusterUid "scope-node-a" "duplicate-z-sidecar") [] "container.cpu.usage" 9 ago
      let missingNamespace = [mkAttr "k8s.pod.name" "shared-pod", mkAttr "k8s.container.name" "duplicate-api", mkAttr "k8s.cluster.uid" clusterUid, mkAttr "k8s.node.name" "scope-node-a"]
      ingestMetric tr key missingNamespace [] "container.cpu.usage" 13 ago
      span' 5 missingNamespace
      ingestMetric tr key [mkAttr "k8s.pod.name" "shared-pod", mkAttr "k8s.namespace.name" "scope-c", mkAttr "k8s.cluster.uid" clusterUid, mkAttr "k8s.node.name" "scope-node-a"] [] "k8s.pod.cpu.usage" 11 ago
      -- A namespace-level kubernetes drilldown links to a containers list scoped to that cluster too.
      forM_ (zip ([1 ..] :: [Int]) ([clusterUid, otherCluster, clusterUid] :: [Text])) \(i, cluster) ->
        ingestMetric tr key [mkAttr "k8s.container.name" ("namespace-api-" <> show i), mkAttr "k8s.pod.name" ("namespace-pod-" <> show i), mkAttr "k8s.namespace.name" "shared-namespace", mkAttr "k8s.cluster.uid" cluster] [] "container.cpu.usage" (fromIntegral i) ago
      nsHtml <- shellHtml tr $ Infrastructure.kubernetesDetailGetH pid (Just "namespaces") (Just "shared-namespace") (Just clusterUid) (Just "shared-namespace") Nothing Nothing (Just "4H")
      params : _ <- pure $ linkParams "/infrastructure/containers" nsHtml
      let param = (`lookup` params)
          rowsOf h = call tr h >>= \(Containers.ContainersPage (PageCtx _ b)) -> V.toList . (.rows) <$> deferredBody b
      (sort . map (.row.containerName) <$> rowsOf (Containers.containersGetH pid (param "runtime") (param "namespace") (param "node") (param "image") (param "cluster") (param "from") (param "to") (param "since") (Just "1")))
        `shouldReturn` ["namespace-api-1", "namespace-api-3"]
      param "since" `shouldBe` Just "4H"

      allRows <- rowsOf $ Containers.containersGetH pid Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "4H") (Just "1")
      (sort [r.row.cpuCores | r <- allRows, r.row.containerName == "duplicate-api"], length [() | r <- allRows, r.row.containerName == "shared-pod"]) `shouldBe` (map Just [1, 3, 5, 7, 13], 1)
      html <- shellHtml tr $ Containers.containersGetH pid (Just "kubernetes") Nothing (Just "scope-node-a") Nothing (Just clusterUid) Nothing Nothing (Just "4H") (Just "1")
      forM_ ([(Just "scope-a", 1), (Nothing, 13)] :: [(Maybe Text, Double)]) \(namespace, expected) -> do
        ps <- maybe (fail "container row has no scoped drawer link") pure $ find (\ps -> lookup "container" ps == Just "duplicate-api" && lookup "namespace" ps == namespace) $ linkParams "/infrastructure/containers/detail" html
        let p = (`lookup` ps)
        scope <- either (fail . toString) pure $ traverse (parseUrlPiece @Scope) $ p "scope"
        detail <- shellHtml tr $ Containers.containerDetailGetH pid (p "container") (p "pod") scope (p "cluster") (p "namespace") (p "node") (p "from") (p "to") (p "since")
        detail `shouldContainAll` ["Node / host: scope-node-a", "Cluster: " <> clusterUid]
        logLinkCount tr pid Nothing detail `shouldReturn` Just 1
        chartPlotsOnly tr pid "container-detail-cpu" expected detail
      ambiguous <- shellHtml tr $ Containers.containerDetailGetH pid (Just "duplicate-api") (Just "shared-pod") Nothing Nothing Nothing Nothing Nothing Nothing (Just "4H")
      ambiguous `shouldContainAll` ["Several containers match this link."]
      (sinceOf "/infrastructure/containers" ambiguous, T.isInfixOf "data-widget=" ambiguous) `shouldBe` ([Just "4H"], False)

    it "rate / increase / last are counter-aware per series; rateif / lastif divide filtered series, on Postgres and TimeFusion" \tr -> do
      -- Points every 30s from base-10m to base+10m: two cumulative series of one counter at 100/s
      -- ("a" restarts at +120s), a DELTA counter at 100/s and two gauges, written straight to Postgres.
      pid <- UUIDId <$> nextRandom
      dbExec
        tr
        [sql| INSERT INTO otel_metrics (project_id, timestamp, start_timestamp, id, series_id, metric_name, metric_type, aggregation_temporality, attributes, value)
                SELECT ?, ?::timestamptz + make_interval(secs => s), CASE WHEN m.temporality = 'DELTA' THEN ?::timestamptz + make_interval(secs => s - 30) END,
                       gen_random_uuid(), m.series, m.name, m.mtype, m.temporality, jsonb_build_object('t', m.t),
                       CASE m.series WHEN 'ca' THEN CASE WHEN s < 120 THEN 100 * (s + 600) ELSE 100 * (s - 90) END
                                     WHEN 'cb' THEN 100 * (s + 600) WHEN 'd' THEN 3000 WHEN 'g1' THEN 1000 + s ELSE 2000 + s END
                FROM generate_series(-600, 600, 30) AS s,
                     (VALUES ('ca', 'c.total', 'SUM', 'CUMULATIVE', 'a'), ('cb', 'c.total', 'SUM', 'CUMULATIVE', 'b'), ('d', 'd.total', 'SUM', 'DELTA', 'a'),
                             ('g1', 'g.bytes', 'GAUGE', NULL, 'a'), ('g2', 'g.bytes', 'GAUGE', NULL, 'b')) AS m(series, name, mtype, temporality, t) |]
        (pid.toText, kqlBase, kqlBase)
      let chart db p dt q = runQueryEffect tr $ Charts.queryMetrics (Just db) dt (Just p) (Just q) Nothing Nothing (Just $ kqlAt 0) (Just $ kqlAt 290) (Just "metrics") Nothing []
          series q = do
            md <- chart Hasql.SqlPostgres pid (Just Charts.DTMetric) q
            md.error `shouldBe` Nothing
            pure (V.toList (V.drop 1 md.headers), map (V.toList . V.drop 1) (V.toList md.dataset))
          rows = fmap snd . series
          scalar q = (.dataFloat) <$> chart Hasql.SqlPostgres pid (Just Charts.DTFloat) q
          counter = "metrics | where metric_name == \"c.total\" | summarize "
      -- One point per 10s bin; each bin's first point reads its predecessor, and the restart reads as 100/s.
      series (counter <> "rate(value) by bin(timestamp, 10s), attributes.t") `shouldReturn` (["a", "b"], replicate 10 [Just 100, Just 100])
      rows (counter <> "rate(value) by bin(timestamp, 10s)") `shouldReturn` replicate 10 [Just 200]
      scalar (counter <> "rate(value)") `shouldReturn` Just 200
      (fst <$> series "metrics | where metric_name in (\"c.total\", \"d.total\") | summarize rate(value) by bin(timestamp, 10s), metric_name, attributes.t") `shouldReturn` ["c.total / a", "c.total / b", "d.total / a"]
      -- increase over the window = value at the end - value before it, through the reset.
      scalar (counter <> "increase(value)") `shouldReturn` Just 60000
      (sum . concatMap catMaybes <$> rows (counter <> "increase(value) by bin(timestamp, 1m)")) `shouldReturn` 60000
      rows "metrics | where metric_name == \"d.total\" | summarize rate(value) by bin(timestamp, 10s)" `shouldReturn` replicate 10 [Just 100]
      (take 1 <$> rows "metrics | where metric_name == \"g.bytes\" | summarize last(value) by bin(timestamp, 1m)") `shouldReturn` [[Just 3060]]

      -- Gauges through OTLP reach every configured backend: hits at 10/s (tier a) and 30/s (b), misses at 60/s (a).
      key <- createTestAPIKey tr testPid "kql-ratio"
      tag <- T.take 8 . UUID.toText <$> nextRandom
      let hits = "kql.hits." <> tag
          misses = "kql.misses." <> tag
          isHit = "metric_name == \"" <> hits <> "\""
          ratio db aggs extend = do
            md <- chart db testPid (Just Charts.DTMetric) $ "metrics | where metric_name in (\"" <> hits <> "\", \"" <> misses <> "\") | summarize " <> aggs <> " by bin(timestamp, 1m)" <> extend
            md.error `shouldBe` Nothing
            pure $ map (V.toList . V.drop 1) (V.toList md.dataset)
      for_ [-300, -270 .. 290] \s -> for_ ([(hits, "a", 10), (hits, "b", 30), (misses, "a", 60)] :: [(Text, Text, Double)]) \(name, tier, perSec) ->
        ingestMetric tr key [] [mkAttr "tier" tier] name (perSec * realToFrac (s + 300)) (addUTCTime s kqlBase)
      tfUrl <- lookupEnv "TIMEFUSION_PG_TEST_URL"
      for_ (Hasql.SqlPostgres : [Hasql.SqlTimefusion | isJust tfUrl]) \db -> do
        -- The chart cache is keyed on the query, not the backend.
        dbExec tr "DELETE FROM query_cache" ()
        ratio db ("hits = rateif(value, " <> isHit <> "), total = rate(value)") " | extend hit_pct = 100.0 * hits / total" `shouldReturn` replicate 5 [Just 40]
        ratio db "100.0 * rateif(value, attributes.tier == \"a\") / rate(value)" "" `shouldReturn` replicate 5 [Just 70]
        ratio db ("lastif(value, attributes.tier == \"b\" and " <> isHit <> ") / lastif(value, attributes.tier == \"a\" and " <> isHit <> ")") "" `shouldReturn` replicate 5 [Just 3]
        ratio db "rate(value)" ", attributes.tier" `shouldReturn` replicate 5 [Just 70, Just 30]

  around withTestResources do
    -- Peers on a shared registrable domain collapse into one head (hundreds of per-tenant hosts
    -- crushed the layout into a column); an unrelated long tail folds under its busiest caller;
    -- both levels travel so the client expands without a refetch.
    it "service map grouping: domain heads, a drawn-node cap, long-tail folds, never on a trace map, without quadratic cost" \_ -> do
      let tenants = ["http:t" <> show n <> ".myshopify.com" | n <- [1 :: Int .. 3]]
          grp = "grp:http:myshopify.com"
          graph = buildServiceGraph 60 serviceMapNodeCap serviceMapFanout Nothing $ [hop t NKExternal 100 | t <- tenants] <> [hop "http:api.paystack.co" NKExternal 50, hop "db:redis" NKDatabase 900]
      (nodeBy graph grp <&> \n -> (n.label, n.memberCount, n.stats.requests)) `shouldBe` Just ("myshopify.com", Just 3, 300)
      map (.groupKey) (mapMaybe (nodeBy graph) tenants) `shouldBe` replicate 3 (Just grp)
      -- A group of one is not a group, and a database is already named by its system.
      map (\k -> nodeBy graph k <&> \n -> (n.memberCount, n.groupKey)) ["http:api.paystack.co", "db:redis"] `shouldBe` [Just (Nothing, Nothing), Just (Nothing, Nothing)]
      -- The legend's icons must exist in the sprite, or they silently render nothing.
      map isJust [lookupIcon "regular" "diagram-project", lookupIcon "regular" "arrow-right-to-bracket"] `shouldBe` [True, True]
      edgePairs graph `shouldBe` L.sort ([("api", t) | t <- tenants] <> [("api", grp), ("api", "http:api.paystack.co"), ("api", "db:redis")])

      -- The cap counts drawn nodes, so a collapsed group costs one slot however many peers it holds.
      let capped = buildServiceGraph 60 3 serviceMapFanout Nothing [hop ("http:t" <> show n <> ".myshopify.com") NKExternal 1 | n <- [1 :: Int .. 40]]
      (nodeBy capped grp <&> (.memberCount), capped.truncated, V.length (V.filter (isNothing . (.groupKey)) capped.nodes)) `shouldBe` (Just (Just 40), False, 2)

      -- Every peer a different merchant's domain: only a volume-ranked fold bounds that. The
      -- counts describe the picture, not the payload ("234 services" for a map of 150).
      let peers = [(n, "http:merchant" <> show n <> ".com") | n <- [1 :: Int .. 20]]
          folded = buildServiceGraph 60 serviceMapNodeCap (CollapseTo 5) Nothing [hop p NKExternal (fromIntegral (100 - n)) | (n, p) <- peers]
      (nodeBy folded "rest:api" <&> \n -> (n.label, n.memberCount, n.stats.requests)) `shouldBe` Just ("15 more dependencies", Just 15, sum [fromIntegral (100 - n) | (n, _) <- drop 5 peers])
      (V.length (drawnNodes folded), V.length folded.nodes, V.length (drawnEdges folded)) `shouldBe` (7, 22, 6)
      map (.label) (V.toList (V.filter (\n -> n.memberCount == Just 15) (drawnNodes folded))) `shouldBe` ["15 more dependencies"]

      -- A trace map is one request's path: folding its hops would hide what the reader opened.
      let traceMap = buildServiceGraph 0 serviceMapNodeCap CollapseOff (Just 1_000_000) [hop p NKExternal 1 | (_, p) <- peers]
      (V.length (drawnNodes traceMap), V.toList (V.filter (isJust . (.memberCount)) traceMap.nodes)) `shouldBe` (21, [])

      -- A domain head that falls into the tail is a head AND a member: expansion nests.
      let nested = buildServiceGraph 60 serviceMapNodeCap (CollapseTo 2) Nothing $ [hop t NKExternal 1 | t <- tenants] <> [hop ("http:loud" <> show n <> ".example" <> show n <> ".com") NKExternal 5000 | n <- [1 :: Int .. 4]]
      (nodeBy nested grp <&> \n -> (n.memberCount, isJust n.groupKey)) `shouldBe` Just (Just 3, True)
      map (.groupKey) (mapMaybe (nodeBy nested) tenants) `shouldBe` replicate 3 (Just grp)

      -- The bug had no wrong output, only a cost: the collapse map rebuilt per lookup was
      -- quadratic, and a 221-node project spent seconds in the fold.
      start <- getCurrentTime
      large <- evaluate $ buildServiceGraph 60 serviceMapNodeCap serviceMapFanout Nothing [hop ("http:merchant" <> show n <> ".example" <> show (n `mod` 40) <> ".com") NKExternal (fromIntegral n) | n <- [1 :: Int .. 400]]
      _ <- evaluate (V.length large.nodes)
      elapsed <- flip diffUTCTime start <$> getCurrentTime
      (V.length (drawnNodes large) > 0, elapsed < 2) `shouldBe` (True, True)

    it "service map rollups: inferred dependencies, idempotent re-rolls, range, errors, environments, endpoint scope and the rendered page" \tr -> do
      let roll pid ts = runTestBg ts tr do
            edges <- rollupServiceEdges False pid (addUTCTime (-300) ts) (addUTCTime 300 ts)
            upsertServiceDependencyEdges pid ts edges
            serviceGraphForRange pid Nothing (addUTCTime (-600) ts) (addUTCTime 600 ts)
          graphFor pid env = runTestBg frozenTime tr $ serviceGraphForRange pid env (addUTCTime (-600) frozenTime) (addUTCTime 600 frozenTime)
          seeded title = do
            pid <- createTestProject tr title
            (pid,) <$> createTestAPIKey tr pid title
          pageOf pid env endpoint = setTestTime tr.trTestClock (addUTCTime 600 frozenTime) >> call tr (ServiceMap.serviceMapGetH pid Nothing Nothing (Just "1H") env endpoint)
          checkoutFlow = [("", "gateway"), ("checkout", "db:orders"), ("checkout", "queue:orders.v1"), ("gateway", "checkout")]

      -- An empty project is an empty graph with no error — `error` exists so a failure can't pose as "no services".
      blank <- createTestProject tr "service-map-empty"
      ((\g -> (V.toList g.nodes, V.toList g.edges, g.error)) <$> graphFor blank Nothing) `shouldReturn` ([], [], Nothing)

      -- The map shows dependencies nobody instruments: no span says service.name=orders.
      (main', mainKey) <- seeded "service-map"
      ingestCheckoutTrace tr mainKey frozenTime
      g <- roll main' frozenTime
      edgePairs g `shouldBe` checkoutFlow
      L.sort [(n.key, n.kind, n.inferred, n.label) | n <- V.toList g.nodes]
        `shouldBe` [("", NKEntry, False, ""), ("checkout", NKService, False, "checkout"), ("db:orders", NKDatabase, True, "orders"), ("gateway", NKService, False, "gateway"), ("queue:orders.v1", NKQueue, True, "orders.v1")]
      g.truncated `shouldBe` False
      -- Catch-up passes and DLQ replays re-roll existing buckets: replace, never accumulate.
      ((\g' -> [e.stats.requests | e <- V.toList g'.edges]) <$> roll main' frozenTime) `shouldReturn` [e.stats.requests | e <- V.toList g.edges]

      (old, oldKey) <- seeded "service-map-old"
      ingestCheckoutTrace tr oldKey (addUTCTime (-86400) frozenTime)
      (V.toList . (.edges) <$> roll old frozenTime) `shouldReturn` []

      (errs, errKey) <- seeded "service-map-errors"
      trId <- hexId
      [gwSid, gwClientSid, coSid] <- replicateM 3 hexId
      let svc s = mkResource errKey [mkAttr "service.name" s]
      ingestSpanReq tr $ withSpanKind PT.Span'SPAN_KIND_SERVER $ mkSpanRequest trId gwSid Nothing "GET /checkout" [] Nothing [] (svc "gateway") frozenTime
      ingestSpanReq tr $ withSpanKind PT.Span'SPAN_KIND_CLIENT $ mkSpanRequest trId gwClientSid (Just gwSid) "POST checkout" [] Nothing [] (svc "gateway") frozenTime
      ingestSpanReq tr $ withSpanStatus PT.Status'STATUS_CODE_ERROR $ withSpanKind PT.Span'SPAN_KIND_SERVER $ mkSpanRequest trId coSid (Just gwClientSid) "POST /checkout" [] Nothing [] (svc "checkout") frozenTime
      ((\g' -> L.sort [(e.source, e.target, e.stats.errors) | e <- V.toList g'.edges]) <$> roll errs frozenTime) `shouldReturn` [("", "gateway", 0), ("gateway", "checkout", 1)]

      -- Env is a rollup dimension: read off the resource, offered sorted, honoured as a filter;
      -- an environment nobody reported is empty rather than silently "all".
      (envs, envKey) <- seeded "service-map-env"
      ingestEnvFixture tr envKey frozenTime
      ((.environments) <$> roll envs frozenTime) `shouldReturn` V.fromList ["prod", "staging"]
      traverse (fmap edgePairs . graphFor envs . Just) (["prod", "staging", "qa"] :: [Text])
        `shouldReturn` [[("", "gateway"), ("checkout", "db:postgresql"), ("gateway", "checkout")], [("", "gateway"), ("gateway", "checkout")], []]

      -- The dispatcher fans out over projects that sent spans in the bucket, never all active ones (1726 in prod).
      runTestBg frozenTime tr (traverse (uncurry (projectsWithSpansInRange False)) [(addUTCTime (-300) frozenTime, addUTCTime 300 frozenTime), (addUTCTime (-3600) frozenTime, addUTCTime (-3300) frozenTime)])
        >>= (`shouldBe` [L.sort [main', errs, envs], []]) . map L.sort

      -- What a user meets: the dependency table is legible without JavaScript, and the canvas
      -- hydrates from the embedded payload.
      mapPage@(ServiceMap.ServiceMapPage (PageCtx conf pd)) <- pageOf main' Nothing Nothing
      (conf.pageTitle, conf.prePageTitle, edgePairs pd.graph) `shouldBe` ("Explorer", Nothing, checkoutFlow)
      let mapHtml = renderT mapPage
      mapHtml `shouldContainAll` ["Entry point", "gateway", "checkout", "orders", "orders.v1", "Dependencies (4)", "p95 latency", "Events", "Metrics", "Service Map", "View repositories"]
      envHtml <- renderT <$> pageOf envs (Just "prod") Nothing
      envHtml `shouldContainAll` [">All<"]
      -- The filter once used a dashed `send` hyperscript parses as an identifier path: the
      -- attribute was dropped and filtering silently did nothing in prod.
      goldenParts "service-map-page"
        . ordNub
        $ tagsWith ["data-service-map", "global-service-map-data", "global-service-map-colors", "aria-label=\"Service colors\"", "data-service-color=", "data-menu-action=\"repositories\"", "aria-label=\"Filter services\""] mapHtml
        <> tagsWith ["?env="] envHtml

      -- An endpoint's map is a rollup read with one synthetic entry and its direct edge only.
      (ep, epKey) <- seeded "endpoint dependency rollup"
      replicateM_ 2 $ ingestCheckoutTrace tr epKey frozenTime -- two traces, so trace_count is a distinct count
      let endpointHash = "endpoint-checkout"
          endpointHashes = [endpointHash, "endpoint-checkout-alias", "err:ignored"] :: [Text]
          relevant edges = L.sort [(e.endpointHash, e.targetKey, e.targetKind, e.reqCount, e.traceCount) | e <- edges, e.endpointHash `elem` endpointHashes]
          tfReads = Config.enableTimefusionReads $ Config.env tr.trATCtx
      dbExec tr [sql| UPDATE otel_logs_and_spans SET hashes = ? WHERE project_id = ? AND name = 'GET /checkout' |] (PGArray endpointHashes, ep)
      when tfReads $ runQueryEffect tr $ Hasql.withHasqlTimefusion True $ Hasql.interpExecute_ [HI.sql| UPDATE otel_logs_and_spans SET hashes = #{endpointHashes} WHERE project_id = #{ep.toText} AND name = 'GET /checkout' |]
      edges <- runTestBg frozenTime tr do
        rolled <- rollupEndpointDependencyEdges False ep (addUTCTime (-300) frozenTime) (addUTCTime 300 frozenTime)
        upsertEndpointDependencyEdges ep frozenTime rolled
        pure rolled
      relevant edges `shouldBe` [(endpointHash, "checkout", NKService, 2, 2), ("endpoint-checkout-alias", "checkout", NKService, 2, 2)]
      when tfReads $ (relevant <$> runTestBg frozenTime tr (rollupEndpointDependencyEdges True ep (addUTCTime (-300) frozenTime) (addUTCTime 300 frozenTime))) `shouldReturn` relevant edges
      graph <- runTestBg frozenTime tr $ endpointDependencyGraphForRange ep endpointHash (addUTCTime (-600) frozenTime) (addUTCTime 600 frozenTime)
      edgePairs graph `shouldBe` [("endpoint:" <> endpointHash, "checkout")]
      (nodeBy graph ("endpoint:" <> endpointHash) <&> \n -> (n.label, n.kind, n.inferred)) `shouldBe` Just ("Selected endpoint", NKEntry, False)
      (nodeBy graph "checkout" <&> (.stats.p95Ns)) `shouldSatisfy` maybe False (> 0)
      -- The shareable broader-map URL keeps the endpoint scope; its empty state says what is absent.
      scoped@(ServiceMap.ServiceMapPage (PageCtx scopedConf scopedPd)) <- pageOf ep Nothing (Just endpointHash)
      (scopedConf.pageTitle, edgePairs scopedPd.graph) `shouldBe` ("Endpoint Dependency Map", [("endpoint:" <> endpointHash, "checkout")])
      renderT scoped `shouldContainAll` ["endpoint-service-map-data", "Selected endpoint", "checkout", "Dependencies (1)"]
      (renderT <$> pageOf ep Nothing (Just "no-such-endpoint")) >>= (`shouldContainAll` ["No direct dependencies in this range"])

    -- The overlay projection once selected neither kind nor status_code, pinning every trace's
    -- Errors stat to 0; a 1300-span trace was 4.4MB of HTML, so the view pages spans.
    it "trace view: span kind and status, error summary, the Map tab carrying this trace's flow, and paged spans" \tr -> do
      pid <- createTestProject tr "trace-view"
      apiKey <- createTestAPIKey tr pid "trace-view"
      trId <- hexId
      [gwSid, gwClientSid, coSid, dbSid] <- replicateM 4 hexId
      let svc s = mkResource apiKey [mkAttr "service.name" s]
          span' sid parent name kind s attrs = ingestSpanReq tr $ withSpanKind kind $ mkSpanRequest trId sid parent name [] Nothing attrs (svc s) frozenTime
      span' gwSid Nothing "GET /checkout" PT.Span'SPAN_KIND_SERVER "gateway" []
      span' gwClientSid (Just gwSid) "POST checkout" PT.Span'SPAN_KIND_CLIENT "gateway" []
      span' coSid (Just gwClientSid) "POST /checkout" PT.Span'SPAN_KIND_SERVER "checkout" []
      ingestSpanReq tr
        $ withSpanStatus PT.Status'STATUS_CODE_ERROR
        $ withSpanKind PT.Span'SPAN_KIND_CLIENT
        $ mkSpanRequest trId dbSid (Just coSid) "pg.query" [mkSpanEvent "exception" [] frozenTime] Nothing [mkAttr "db.system.name" "postgresql", mkAttr "db.namespace" "orders", mkAttr "db.statement" "\n    SELECT <unsafe>\n      FROM checkout\n    \n"] (svc "checkout") frozenTime
      spans <- runTestBg frozenTime tr (Telemetry.getTraceDetailsForView False pid trId (Just frozenTime) frozenTime Nothing) >>= maybe (fail "trace not found") (\(_, sps, _) -> pure sps)
      sortOn (\(n, _, _) -> n) [(s.spanName, show @Text s.kind, s.status) | s <- V.toList spans]
        `shouldBe` [("GET /checkout", "Just SKServer", Just Telemetry.SSUnset), ("POST /checkout", "Just SKServer", Just Telemetry.SSUnset), ("POST checkout", "Just SKClient", Just Telemetry.SSUnset), ("pg.query", "Just SKClient", Just Telemetry.SSError)]
      html <- renderT <$> call tr (Trace.traceH pid trId (Just frozenTime) Nothing Nothing Nothing Nothing)
      html `shouldContainAll` ["Waterfall", "Timeline", "Services", ">Map<", "GET /checkout", "pg.query", ">Errors</", ">1<", "<template data-trace-tooltip", "Span information", "Error span", "No error message available in this preview."]
      -- The graph is serialised into the page (a shared link needs no fetch): both services, the
      -- inferred database, and each service's share of execution time.
      html `shouldContainAll` ["trace-service-map-", "\"gateway\"", "\"checkout\"", "\"orders\"", "duration_share"]
      -- The Map tab must stay wired to its panel through the radio and a literal CSS variant;
      -- the statement is escaped and dedented.
      goldenParts "trace-view"
        . ordNub
        $ tagsWith ["id=\"tab-map\"", "group-has-[#tab-map:checked]/tt:block", "aria-label=\"Search spans\"", "aria-label=\"Previous matching span\"", "aria-label=\"Next matching span\""] html
        <> filter (T.isPrefixOf "<p") (elementsWith ["SELECT &lt;unsafe&gt;"] html)

      -- 60 spans under one root, one export (a request per span overflows the extraction queue);
      -- the root's id sits outside 0..59 so span n keeps id lpad(hex n).
      paged <- hexId
      let rootSid = "00000000000000f0"
          spanSid i = T.takeEnd 16 $ "000000000000000" <> toText (showHex i "")
      ingestSpanReq tr $ mergeSpanRequests [mkSpanRequest paged (if i == 0 then rootSid else spanSid i) (if i == 0 then Nothing else Just rootSid) ("span-" <> show i) [] Nothing [] (mkResource apiKey []) (addUTCTime (fromIntegral i / 1000) frozenTime) | i <- [0 .. 59 :: Int]]
      let traceAt sid nav embedded limit = call tr $ Trace.traceH pid paged (Just frozenTime) sid nav embedded limit
          loaded = \case
            Trace.TraceDetails _ _ sps _ moreUrl -> pure (V.length sps, moreUrl)
            _ -> fail "expected a paged trace view"
      firstPage <- traceAt Nothing Nothing Nothing (Just 50)
      fullPage <- traceAt Nothing Nothing Nothing (Just 100)
      -- The next page asks for a bigger one; following it yields the whole trace and the offer goes.
      traverse loaded [firstPage, fullPage] `shouldReturn` [(50 :: Int, Just ("/p/" <> pid.toText <> "/traces/" <> paged <> "?timestamp=2025-01-01T00%3A00%3A00Z&spans=100")), (60, Nothing)]
      renderT firstPage `shouldContainAll` [">50+<", "Load more spans"]
      renderT fullPage `shouldNotSatisfy` T.isInfixOf "Load more spans"
      -- Nav looks a span up by id, so one past the first page is still findable.
      traceAt (Just "000000000000003b") (Just "next") Nothing (Just 50) >>= \case
        Trace.SpanDetails _ target _ -> target.name `shouldBe` Just "span-59"
        _ -> expectationFailure "expected the 59th span, not a fallback to the root"
      -- A Load-more pull must not re-arm the auto-open marker, which dragged the panel back to the first span.
      let autoOpen = T.isInfixOf "load[document.getElementById(&#39;trace_span_container&#39;).clientWidth" . renderT
      (,) <$> (autoOpen <$> traceAt Nothing Nothing (Just "true") Nothing) <*> (autoOpen <$> traceAt Nothing Nothing (Just "true") (Just 100)) `shouldReturn` (True, False)

    it "metrics overview: datapoint counts in the shared drawer, source overflow, pagination, deferred related metrics, server-side service search" \tr -> do
      [Trace.metricDetailUrl testPid "system.cpu.utilization" "accounting" Nothing, Trace.metricDetailUrl testPid "container.cpu.time" "accounting" (Just "attributes.service.name"), Trace.metricExpandUrl testPid "container.cpu.time" "accounting" (Just "attributes.service.name")]
        `shouldBe` map (("/p/" <> testPid.toText) <>) ["/metrics/details/system.cpu.utilization?metric_source=accounting", "/metrics/details/container.cpu.time?metric_source=accounting&label=attributes.service.name", "/metrics?tab=charts&metric_source=accounting&expand=container.cpu.time&label=attributes.service.name"]
      let overview pid tab range cursor expand q = call tr $ Trace.metricsOverViewGetH pid (Just tab) range range range Nothing Nothing cursor expand Nothing q Nothing
          datapointsOf = \case
            Trace.MetricsOVDataPointMain (PageCtx _ (_, dps, _, countUrl, page, pageUrl, _)) -> pure (dps, countUrl, page, pageUrl)
            _ -> fail "expected the datapoints overview"

      -- Counts are deferred to their own request; the toolbar opens the one shared drawer.
      dp <- createTestProject tr "datapoints"
      key <- createTestAPIKey tr dp "datapoints"
      for_ ([("datapoint.current", 0), ("datapoint.stale", -7200), ("datapoint.recent", -1800)] :: [(Text, NominalDiffTime)]) \(name, offset) -> ingestMetric tr key [] [] name 1 (addUTCTime offset frozenTime)
      runTestBg frozenTime tr $ Telemetry.flushMetricCatalog tr.trATCtx.metricCatalogBuffer
      main' <- overview dp "datapoints" Nothing Nothing Nothing Nothing
      (dps, countUrl, page, pageUrl) <- datapointsOf main'
      let dpBase = "/p/" <> dp.toText
      (countUrl, page, pageUrl, Map.fromList [(m.metricName, m.dataPointsCount) | m <- V.toList dps])
        `shouldBe` (dpBase <> "/metrics/datapoints/counts?since=1H", 0, dpBase <> "/metrics?tab=datapoints&since=1H", Map.fromList [("datapoint.current", Nothing), ("datapoint.recent", Nothing), ("datapoint.stale", Nothing)])
      let mainHtml = renderT main'
      (T.count "placeholder=\"Search metrics\"" mainHtml, T.isInfixOf "placeholder=\"Search\"" mainHtml, T.count "id=\"global-data-drawer\"" mainHtml) `shouldBe` (1, False, 1)
      expanded <- renderT <$> overview dp "datapoints" Nothing Nothing (Just "datapoint.current") Nothing
      -- Expanding a metric opens the drawer on load.
      [any (T.isInfixOf " checked ") (tagsWith ["id=\"global-data-drawer\""] h) | h <- [mainHtml, expanded]] `shouldBe` [False, True]
      counts <- shellHtml tr $ Trace.dataPointCountsGetH dp Nothing Nothing Nothing
      goldenParts "metrics-datapoints"
        . ordNub
        $ tagsWith ["placeholder=\"Search metrics\"", "border-b border-strokeWeak", "fixed right-0 top-0", "data-hx-push-url"] mainHtml
        <> elementsWith ["metric-datapoint-count-"] counts
      -- A cleared picker submits since=&from=&to=: that is "no range", not now..now with every metric empty.
      shellHtml tr (Trace.dataPointCountsGetH dp (Just "") (Just "") (Just "")) `shouldReturn` counts
      ((\(_, c, _, u) -> (c, u)) <$> (datapointsOf =<< overview dp "datapoints" (Just "") Nothing Nothing Nothing)) `shouldReturn` (countUrl, pageUrl)

      -- A metric reported by many sources lists three and defers the rest to its details.
      overflow <- metaProject tr "source-overflow" [("source.overflow", "source-" <> show @Text i, [], Nothing) | i <- [1 .. 4 :: Int]]
      overflowHtml <- renderT <$> overview overflow "datapoints" Nothing Nothing Nothing Nothing
      overflowHtml `shouldContainAll` ["source-1", "source-2", "source-3", "View 1 more source in details", Trace.metricDetailUrl overflow "source.overflow" "all" Nothing]
      overflowHtml `shouldNotSatisfy` T.isInfixOf "source-4"

      paged <- metaProject tr "datapoint-pages" [("page.metric." <> show @Text n, "gateway", [], Nothing) | n <- [1 .. 30 :: Int]]
      firstHtml <- renderT <$> overview paged "datapoints" Nothing Nothing Nothing Nothing
      (T.isInfixOf "page.metric.30" firstHtml, [lookup "cursor" ps | ps <- linkParams "/metrics" firstHtml, isNothing (lookup "expand" ps), isJust (lookup "cursor" ps)]) `shouldBe` (False, [Just "1"])
      (renderT <$> overview paged "datapoints" Nothing (Just 1) Nothing Nothing) >>= (`shouldContainAll` ["page.metric.30"])

      -- Related metrics wait until their tab is opened.
      related <- metaProject tr "related-metrics" [("process.cpu.usage", "gateway", ["state"], Nothing), ("process.cpu.time", "gateway", ["state"], Nothing)]
      detail <- shellHtml tr $ Trace.metricDetailsGetH related "process.cpu.usage" Nothing Nothing Nothing Nothing Nothing
      (T.isInfixOf "metric-related-content" detail, T.isInfixOf "process.cpu.time" detail) `shouldBe` (True, False)
      (shellHtml tr (Trace.metricRelatedGetH related "process.cpu.usage" Nothing)) >>= (`shouldContainAll` ["process.cpu.time"])

      -- The service filter inlined every service (2.3k <option>s, ~117KB on the demo project);
      -- it is now searched and capped server-side.
      services <- metaProject tr "metric-services" $ ("svc.metric", "billing-api", [], Nothing) : [("svc.metric", "checkout-" <> show @Text i, [], Nothing) | i <- [1 .. 80 :: Int]]
      [allSvcs, matched] <- runTestBg frozenTime tr $ traverse (\q -> Telemetry.getMetricServiceNames services q 50) ([Nothing, Just "billing"] :: [Maybe Text])
      (length allSvcs, matched) `shouldBe` (50, ["billing-api"])
      (shellHtml tr (Trace.metricServicesGetH services (Just "billing") Nothing)) >>= (`shouldContainAll` ["billing-api", "All Services"])
      chartsHtml <- renderT <$> overview services "charts" Nothing Nothing Nothing Nothing
      T.count "checkout-" chartsHtml `shouldBe` 0
      goldenParts "metrics-charts" . ordNub $ tagsWith ["/metrics/services", "gap-x-4 gap-y-5 pb-4", "h-60"] chartsHtml

    -- The /metrics list query was rewritten to de-duplicate then aggregate (640ms of ~850ms), and the
    -- overview reads one page of active metrics instead of the whole catalogue.
    it "metric catalogue: one row per metric with merged labels, paged active metrics with an inactive tail, literal search, default chart dimensions" \tr -> do
      let cutoff = addUTCTime (-(7 * 24 * 3600)) frozenTime
          names = map (.metricName) . V.toList

      -- Both filters are spliced into separate CTEs, so each must apply.
      list <- metaProject tr "chart-list" [("http.server.duration", "gateway", ["http.method", "http.status"], Nothing), ("http.server.duration", "checkout", ["http.status", "http.route"], Nothing), ("process.cpu.time", "gateway", ["state"], Nothing)]
      (map (map (\r -> (r.metricName, V.toList r.metricLabels))) <$> runTestBg frozenTime tr (traverse (uncurry (Telemetry.getMetricChartListData list)) [(Nothing, Nothing), (Nothing, Just "process."), (Just "checkout", Nothing)]))
        `shouldReturn` [[("http.server.duration", ["http.method", "http.route", "http.status"]), ("process.cpu.time", ["state"])], [("process.cpu.time", ["state"])], [("http.server.duration", ["http.route", "http.status"])]]

      let fresh = Just $ addUTCTime (-3600) frozenTime
          stale = Just $ addUTCTime (-(30 * 24 * 3600)) frozenTime
      catalogue <- metaProject tr "catalogue" $ [(n, "gateway", [], fresh) | n <- ["a.one", "a.two", "a.three", "a.four", "a.five"]] <> [(n, "gateway", [], stale) | n <- ["z.old.one", "z.old.two"]]
      -- Page 2 continues the ordering with the same total, but must not drag the inactive tail along.
      pages <- runTestBg frozenTime tr $ sequence [Telemetry.getMetricCatalogPage catalogue Nothing Nothing Nothing cutoff 2 0 Telemetry.ActivePageWithInactive, Telemetry.getMetricCatalogPage catalogue Nothing Nothing Nothing cutoff 2 2 Telemetry.ActivePage]
      [(names p.active, p.activeTotal, names p.inactive) | p <- pages] `shouldBe` [(["a.five", "a.four"], 5, ["z.old.one", "z.old.two"]), (["a.one", "a.three"], 5, [])]

      -- Search runs before pagination and treats LIKE metacharacters literally.
      search <- metaProject tr "catalogue-search" [("a.unrelated", "billing", [], Nothing), ("runtime.one", "checkout", ["attributes.region"], Nothing), ("runtime.two", "checkout", ["attributes.zone"], Nothing), ("runtime.two", "billing", ["attributes.other"], Nothing), ("literal%_&+", "checkout", [], Nothing)]
      matches <- runTestBg frozenTime tr $ traverse (\off -> Telemetry.getMetricCatalogPage search (Just "checkout") Nothing (Just "RUNTIME") cutoff 1 off Telemetry.ActivePage) [0, 1]
      [(names p.active, p.activeTotal, map (V.toList . (.metricLabels)) (V.toList p.active)) | p <- matches] `shouldBe` [(["runtime.one"], 2, [["attributes.region"]]), (["runtime.two"], 2, [["attributes.zone"]])]
      (names . (.active) <$> runTestBg frozenTime tr (Telemetry.getMetricCatalogPage search Nothing Nothing (Just "%_&+") cutoff 12 0 Telemetry.ActivePage)) `shouldReturn` ["literal%_&+"]
      runTestBg frozenTime tr (Telemetry.getMetricGroups search) >>= (`shouldContain` ["runtime."])
      searched <- renderT <$> call tr (Trace.metricsOverViewGetH search (Just "charts") Nothing Nothing (Just "6H") Nothing Nothing Nothing Nothing Nothing (Just "%_&+") Nothing)
      (T.isInfixOf "q=%25_%26%2B" searched, T.isInfixOf "a.unrelated" searched) `shouldBe` (True, False)

      -- A histogram card averages sum/count over non-empty buckets, scoped to its service, and groups by
      -- the most useful dimension available unless one is chosen ("all" turns grouping off).
      cards <- createTestProject tr "metric-cards"
      dbExec tr [sql| INSERT INTO otel_metrics_meta (project_id,metric_name,metric_type,metric_unit,metric_description,service_name,scope_name,metric_labels,first_seen_at,last_seen_at,first_timestamp,last_timestamp)
                      VALUES (?,'request.duration','HISTOGRAM','s','latency','checkout','test','{}',now(),now(),now(),now()) |] (Only cards)
      let dimensions = ["name", "resource.host.name", "resource.service.name", "attributes.zone", "attributes.region"] :: [Text]
      for_
        ( [ (dimensions, Nothing, Just "attributes.region")
          , (dimensions, Just "resource.host.name", Just "resource.host.name")
          , (dimensions, Just "invalid", Just "attributes.region")
          , (dimensions, Just "all", Nothing)
          , (take 3 dimensions, Nothing, Just "resource.service.name")
          , (take 2 dimensions, Nothing, Just "resource.host.name")
          , (["name"], Nothing, Just "name")
          , ([], Nothing, Nothing)
          ]
            :: [([Text], Maybe Text, Maybe Text)]
        )
        \(labels, requested, expected) -> do
          dbExec tr [sql| UPDATE otel_metrics_meta SET metric_labels = ? WHERE project_id = ? AND metric_name = 'request.duration' |] (PGArray labels, cards)
          card <- shellHtml tr $ Trace.metricCardGetH cards "request.duration" requested (Just "checkout")
          query <- fromMaybe "" . (.query) <$> renderedWidget card
          card `shouldContainAll` ["sum(distribution_sum) / sum(distribution_count)", "distribution_count &gt; 0", "resource.service.name", "checkout", "&quot;hideValue&quot;:true"]
          (T.isInfixOf "by bin_auto(timestamp)," query, ordNub (mapMaybe (lookup "label") (linkParams "/metrics/details/request.duration" card)))
            `shouldBe` (isJust expected, maybeToList expected)
          for_ expected \label -> query `shouldSatisfy` T.isInfixOf ("by bin_auto(timestamp)," <> label)
          (T.isInfixOf (">" <> fromMaybe "All values" expected <> "</span>") card, T.isInfixOf ">Group by</span>" card) `shouldBe` (not (null labels), not (null labels))

    -- Exemplars were parsed at ingestion but read back nowhere: the trace id in otel_metrics.exemplars reached no surface.
    it "exemplars link a metric to the trace it was recorded in and that trace back to its metrics" \tr -> do
      pid <- createTestProject tr "exemplars"
      apiKey <- createTestAPIKey tr pid "exemplars"
      ingestSpanLinked tr apiKey exemplarTrace exemplarSpan Nothing "GET /exemplar" [] frozenTime
      ingestMetricWithExemplar tr apiKey
      let isoAt offset = toText $ iso8601Show $ addUTCTime offset frozenTime
          exemplarsIn p name from = call tr $ Trace.metricExemplarsGetH p name (Just $ isoAt from) (Just $ isoAt 3600) Nothing
          -- Read off the JSON the chart overlay consumes, so its diamonds and these rows agree.
          points x = [(o Map.!? "trace_id", o Map.!? "span_id", o Map.!? "value") | o <- fromMaybe [] (AE.decode @[Map.Map Text AE.Value] (AE.encode x))]
          firstRow = maybe (fail "ingested row missing") pure . viaNonEmpty head
      -- metric -> trace: rows deep-link into the trace overlay with the timestamp (without one,
      -- traceH widens to a 3-day scan); the chart overlay reads the same handler as JSON.
      exemplars <- exemplarsIn pid exemplarMetric (-3600)
      points exemplars `shouldBe` [(Just (AE.String exemplarTrace), Just (AE.String exemplarSpan), Just (AE.Number 42.5))]
      ordNub (lookup "showTrace" <$> linkParams "/log_explorer" (renderT exemplars)) `shouldBe` [Just (exemplarTrace <> "/?timestamp=2025-01-01T00:00:00Z")]
      -- An exemplar whose own timestamp is outside the window is dropped though its row is inside
      -- (cumulative histograms re-export weeks-old exemplars past retention).
      (points <$> exemplarsIn pid exemplarMetric 600) `shouldReturn` []

      -- span -> metrics: the metric recorded inside this trace, plus the service's metrics charted over
      -- a window padded around the span with the span's own extent shaded. The catalogue fills
      -- through an async buffer this test does not drive, so it is seeded.
      (rid, ts) <- firstRow =<< dbQuery @_ @(UUID.UUID, UTCTime) tr [sql| SELECT id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND context___trace_id = ? |] (pid, exemplarTrace)
      seedMeta tr pid exemplarMetric "test-service" [] (Just ts)
      relatedHtml <- shellHtml tr $ Trace.relatedMetricsGetH pid rid ts
      relatedHtml `shouldContainAll` ["Recorded in this trace", exemplarMetric, "Metrics from test-service", "timeFrom", "highlightFrom", formatUTC (addUTCTime (-120) ts), formatUTC ts]
      -- Opening a span pays for no metrics lookup: the panel ships a placeholder whose content is another shell.
      item <- shellHtml tr $ LogItem.expandAPIlogItemH pid rid ts Nothing Nothing Nothing False
      metricsTab <- shellHtml tr $ LogItem.expandAPIlogItemH pid rid ts Nothing (Just "tab-metrics") Nothing True
      (T.isInfixOf exemplarMetric item, T.isInfixOf exemplarMetric metricsTab, T.isInfixOf "/related_metrics" metricsTab) `shouldBe` (False, False, True)
      goldenParts "exemplar-metrics-tab" . ordNub $ tagsWith ["group-has-[.tab-metrics:checked]/dtab:block", "tab=tab-metrics&amp;partial"] item

      -- A log carries no trace context: still the service tier. A metric without exemplars reports none.
      logs <- createTestProject tr "exemplar-logs"
      logKey <- createTestAPIKey tr logs "exemplar-logs"
      ingestLog tr logKey "a log with no trace" frozenTime
      ingestMetricWithExemplar tr logKey
      ingestMetric tr logKey [] [] "test.no.exemplars" 1 frozenTime
      none <- exemplarsIn logs "test.no.exemplars" (-3600)
      (points none, T.isInfixOf "No exemplars" (renderT none)) `shouldBe` ([], True)
      (logRid, logTs) <- firstRow =<< dbQuery @_ @(UUID.UUID, UTCTime) tr [sql| SELECT id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND kind = 'log' |] (Only logs)
      (shellHtml tr (Trace.relatedMetricsGetH logs logRid logTs)) >>= (`shouldContainAll` ["No metric reported a datapoint from this trace", "Metrics from test-service"])

    it "prometheus targets: leased claims, scraped bodies as metrics, duplicate saves kept as drafts, viewers locked out" \tr -> do
      let insert p kind name url interval = void $ runQueryEffect tr $ PromCfg.insertConfig p kind name url interval Nothing (AE.object []) Nothing
          configs p = V.toList <$> runQueryEffect tr (PromCfg.configsByProjectId p PromCfg.CKPrometheus)
          claim = length <$> runQueryEffect tr (PromCfg.claimDueConfigs 50)
      -- Each due target is claimed once and leased (multi-node safe); a disabled one never is.
      insert testPid PromCfg.CKPrometheus "svc-a" "http://a/metrics" 3600
      insert testPid PromCfg.CKPrometheus "svc-b" "http://b/metrics" 3600
      sequence [claim, claim] `shouldReturn` [2, 0]
      insert testPid PromCfg.CKPrometheus "svc-c" "http://c/metrics" 1
      svcC <- maybe (fail "svc-c missing") pure . find ((== "svc-c") . (.name)) =<< configs testPid
      void $ runQueryEffect tr $ PromCfg.setEnabled testPid svcC.id False
      claim `shouldReturn` 0

      -- 2 counter series + 1 gauge; the +Inf sample is dropped.
      n <- runTestBg frozenTime tr $ PromCfg.ingestScrapedBody svcC frozenTime promBody
      n `shouldBe` 3
      (sort <$> dbQuery @_ @(Text, Text, Maybe Text) tr [sql| SELECT metric_name, metric_type, aggregation_temporality FROM otel_metrics WHERE project_id = ? |] (Only testPid.toText))
        `shouldReturn` [("http_requests_total", "SUM", Just "CUMULATIVE"), ("http_requests_total", "SUM", Just "CUMULATIVE"), ("temperature_celsius", "GAUGE", Nothing)]

      -- A duplicate name hits UNIQUE(project_id, name) (migration 0103) and becomes an inline error
      -- that keeps the draft — not a 500, not a redirect, not a second row; so does a private URL.
      let form = PrometheusForm{name = "dup", url = "http://example.com/metrics", scrapeInterval = Nothing, authHeader = Nothing, extraLabels = Nothing, clearAuth = Nothing}
      _ <- testServant tr $ Settings.prometheusPostH testPid form
      (rejected, rejection) <- testServant tr $ Settings.prometheusPostH testPid form
      (lookupResponseHeader @"HX-Redirect" @Text rejected, reswapOf rejected) `shouldBe` (MissingHeader, Just "none")
      renderT rejection `shouldContainAll` ["already exists"]
      goldenParts "prometheus-duplicate" [renderT rejection]
      (reswapOf . fst <$> testServant tr (Settings.prometheusPostH testPid form{url = "http://127.0.0.1/metrics"})) `shouldReturn` Just "none"
      (length . filter ((== "dup") . (.name)) <$> configs testPid) `shouldReturn` 1

      -- A viewer can neither change targets nor make the server probe an endpoint, and never sees the auth header.
      viewer <- createTestProject tr "prometheus-viewer"
      void $ runQueryEffect tr $ PromCfg.insertConfig viewer PromCfg.CKPrometheus "Saved target" "https://metrics.example.com/metrics" 60 (Just "Bearer saved-secret") (AE.object []) Nothing
      [saved] <- configs viewer
      let uid = (getResponse tr.trSessAndHeader).user.id
          replacement = PrometheusForm "Replacement target" "https://metrics.example.com/replacement" Nothing (Just "Bearer replacement-secret") Nothing Nothing
      dbExec tr [sql| UPDATE projects.project_members SET permission = 'view' WHERE project_id = ? AND user_id = ? |] (viewer, uid)
      requests <- newIORef (0 :: Int)
      statuses <- forM
        [ void $ Settings.prometheusPostH viewer replacement
        , void $ Settings.prometheusUpdateH viewer saved.id replacement
        , void $ Settings.prometheusToggleH viewer saved.id
        , void $ Settings.prometheusDeleteH viewer saved.id
        , void $ Settings.prometheusTestH viewer replacement
        ]
        \mutation -> runAuthHandler tr $ withHTTPResponses (\_ _ -> modifyIORef' requests (+ 1) $> Just promBody) $ catchError @ServerError (mutation $> 200) (\_ err -> pure err.errHTTPCode)
      (statuses, ) <$> readIORef requests `shouldReturn` (replicate 5 403, 0)
      ((\c -> (c.name, c.url, c.enabled, c.authHeader)) <<$>> configs viewer) `shouldReturn` [(saved.name, saved.url, saved.enabled, saved.authHeader)]
      viewerHtml <- shellHtml tr $ Settings.prometheusGetH viewer
      (T.isInfixOf "Saved target" viewerHtml, filter (`T.isInfixOf` viewerHtml) ["Add target", "Edit Prometheus target", "hx-patch", "hx-delete", "saved-secret"]) `shouldBe` (True, [])

    -- Unlike every other metrics handler, the card and breakdown never called sessionAndProject.
    it "non-members cannot read container details, metric cards or breakdowns" \tr -> do
      victim <- createTestProject tr "victim"
      seedMeta tr victim "victim.secret" "checkout" ["attributes.region"] Nothing
      (shellHtml tr (Trace.metricCardGetH victim "victim.secret" Nothing Nothing)) >>= (`shouldContainAll` ["victim.secret"])
      assertNonMemberDenied tr victim $ Containers.containerDetailGetH victim (Just "checkout") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      assertNonMemberDenied tr victim $ Trace.metricCardGetH victim "victim.secret" Nothing Nothing
      assertNonMemberDenied tr victim $ Trace.metricBreakdownGetH victim "victim.secret" Nothing
  where
    redpanda = "srv-captain--redpanda-0.1.tt13bkp5"
    swarm = "srv-captain--api.2.wcxx3mlmh1q1jpwk9ohyjjv7c"
    sinceOf path html = ordNub $ lookup "since" <$> linkParams path html
    -- The drawer's first log link, counted through the chart pipeline.
    logLinkCount tr pid source html = do
      params : _ <- pure $ linkParams "/log_explorer" html
      query <- maybe (fail "drawer has no log query") pure $ lookup "query" params
      response <- runQueryEffect tr $ Charts.queryMetrics source (Just Charts.DTFloat) (Just pid) (Just $ query <> " | summarize count()") Nothing (lookup "since" params) Nothing Nothing (Just "spans") Nothing []
      response.error `shouldBe` Nothing
      pure response.dataFloat
    -- The page's first chart, run through the chart pipeline, plots only @expected@.
    chartPlotsOnly tr pid widgetId expected html = do
      widget <- renderedWidget html
      Widget.id widget `shouldBe` Just (widgetId :: Text)
      query <- maybe (fail "widget has no query") pure $ Widget.query widget
      response <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just query) Nothing Nothing (Just "2024-12-31T23:55:00Z") (Just "2025-01-01T00:01:00Z") (Just "metrics") Nothing []
      response.error `shouldBe` Nothing
      V.toList (V.mapMaybe ((V.!? 1) >=> id) response.dataset) `shouldSatisfy` \values -> not (null values) && all (\value -> abs (value - expected) < 0.001) values


-- | Mixed receiver families as an OpenTelemetry Collector's kubeletstats/k8s_cluster,
-- docker_stats and hostmetrics emit them: absent limits, controller fallback, pod-only
-- telemetry, a pod rollup that must lose to its containers, an in-cluster agent, a Swarm task
-- and a metric with no container identity at all.
ingestInventory :: TestResources -> Text -> IO ()
ingestInventory tr key = do
  let emit res dp (name, value) = ingestMetric tr key res dp name value frozenTime
      k8sChecked = mkAttr "k8s.cluster.name" "otel-demo" : k8sResource "default" "checkout-7fb5b4f859-nlcjs" "checkout"
      cpuDp core mode = [mkAttr "cpu.logical_number" core, mkAttr "cpu.mode" mode]
  forM_
    ( [ (k8sChecked, [("container.cpu.usage", 0.5), ("container.memory.working_set", 536870912), ("k8s.container.cpu_limit", 2), ("k8s.container.cpu_request", 1), ("k8s.container.memory_limit", 1073741824), ("k8s.container.memory_request", 268435456), ("k8s.container.restarts", 3), ("k8s.container.ready", 1)])
      , (k8sResource "kube-system" "coredns-c6d9fc49c-bj5sw" "coredns", [("container.cpu.usage", 0.25), ("container.memory.working_set", 33554432), ("k8s.container.ready", 0)])
      , (dockerResource "srv-captain--redpanda-0.1.tt13bkp5", [("container.cpu.utilization", 80), ("container.memory.usage.total", 3221225472), ("container.memory.usage.limit", 4294967296)])
      , -- k8sattributes names the controller after its kind: a StatefulSet pod has no k8s.deployment.name.
        (mkAttr "k8s.statefulset.name" "postgres" : k8sPodResource "data" "postgres-0" "postgres", [("container.cpu.usage", 0.1), ("container.memory.usage", 16777216)])
      , (mkAttr "k8s.deployment.name" "frontend" : k8sPodResource "shop" "frontend-544d9b6f4-2xk7z" "frontend", [("k8s.pod.cpu.usage", 0.75), ("k8s.pod.memory.working_set", 268435456)])
      , (k8sChecked, [("k8s.pod.cpu.usage", 9.9), ("k8s.pod.memory.working_set", 9999999)])
      , -- docker_stats reports a Swarm task by name with no service attribute.
        (dockerResource "srv-captain--api.2.wcxx3mlmh1q1jpwk9ohyjjv7c", [("container.cpu.utilization", 40), ("container.memory.usage.total", 1073741824)])
      , ([], [("http.server.duration", 12)])
      ]
        :: [([PC.KeyValue], [(Text, Double)])]
    )
    \(resource, metrics) -> mapM_ (emit resource []) metrics
  -- A bare 4-core node reporting idle and user per core; memory per state, the limit their sum.
  forM_ (["cpu0", "cpu1", "cpu2", "cpu3"] :: [Text]) \core -> forM_ ([("idle", 0.875), ("user", 0.125)] :: [(Text, Double)]) \(mode, v) -> emit [mkAttr "host.name" "vps-bare-01"] (cpuDp core mode) ("system.cpu.utilization", v)
  forM_ ([("used", 2147483648), ("free", 1073741824), ("cached", 1073741824)] :: [(Text, Double)]) \(st, v) -> emit [mkAttr "host.name" "vps-bare-01"] [mkAttr "system.memory.state" st] ("system.memory.usage", v)
  -- hostmetrics from an in-cluster agent: host.name is the agent pod, the node only in k8s.node.name.
  -- It must land on the node's row, not mint a phantom "otel-collector-…" host.
  forM_ (["cpu0", "cpu1"] :: [Text]) \core -> forM_ ([("idle", 0.75), ("user", 0.25)] :: [(Text, Double)]) \(mode, v) -> emit [mkAttr "host.name" "otel-collector-77cd857677-qb24l", mkAttr "k8s.node.name" "vps-d6d7e318"] (cpuDp core mode) ("system.cpu.utilization", v)


k8sResource :: Text -> Text -> Text -> [PC.KeyValue]
k8sResource ns pod container = mkAttr "k8s.deployment.name" container : k8sPodResource ns pod container


-- | Nothing can supply a cluster's name, so the query coalesces it to the uid; only checkout gets a name.
k8sPodResource :: Text -> Text -> Text -> [PC.KeyValue]
k8sPodResource ns pod container = [mkAttr "k8s.namespace.name" ns, mkAttr "k8s.pod.name" pod, mkAttr "k8s.container.name" container, mkAttr "k8s.node.name" "vps-d6d7e318", mkAttr "container.image.name" "ghcr.io/open-telemetry/demo", mkAttr "container.image.tag" "2.2.0", mkAttr "k8s.cluster.uid" clusterUid]


clusterUid :: Text
clusterUid = "9b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d"


-- | docker_stats: no pod or namespace, the host in host.name, CPU as percent of one core.
dockerResource :: Text -> [PC.KeyValue]
dockerResource name = [mkAttr "container.name" name, mkAttr "container.runtime" "docker", mkAttr "container.id" "26550399cdefd1a6", mkAttr "container.image.name" "docker.redpanda.com/redpandadata/redpanda:v25.1.4", mkAttr "host.name" "0ce201583b04"]


rowsByName :: V.Vector ContainerRow -> [(Text, ContainerRow)]
rowsByName = map (\r -> (r.containerName, r)) . V.toList


-- | (entry) -> gateway -client-> checkout -client-> postgres "orders" [inferred db]
--                                         -producer-> "orders.v1"     [inferred queue]
ingestCheckoutTrace :: TestResources -> Text -> UTCTime -> IO ()
ingestCheckoutTrace tr apiKey ts = do
  trId <- hexId
  [gw, gwc, co, db, q] <- replicateM 5 hexId
  let span' sid parent name kind s attrs = ingestSpanReq tr $ withSpanKind kind $ mkSpanRequest trId sid parent name [] Nothing attrs (mkResource apiKey [mkAttr "service.name" s]) ts
  span' gw Nothing "GET /checkout" PT.Span'SPAN_KIND_SERVER "gateway" []
  span' gwc (Just gw) "POST checkout" PT.Span'SPAN_KIND_CLIENT "gateway" []
  span' co (Just gwc) "POST /checkout" PT.Span'SPAN_KIND_SERVER "checkout" []
  span' db (Just co) "pg.query" PT.Span'SPAN_KIND_CLIENT "checkout" [mkAttr "db.system.name" "postgresql", mkAttr "db.namespace" "orders"]
  span' q (Just co) "publish" PT.Span'SPAN_KIND_PRODUCER "checkout" [mkAttr "server.address" "orders.v1"]


-- | prod: gateway -> checkout -> postgres; staging: the same two services, one hop.
ingestEnvFixture :: TestResources -> Text -> UTCTime -> IO ()
ingestEnvFixture tr apiKey ts = do
  let span' trId sid parent name kind s env attrs = ingestSpanReq tr $ withSpanKind kind $ mkSpanRequest trId sid parent name [] Nothing attrs (mkResource apiKey [mkAttr "service.name" s, mkAttr "deployment.environment.name" env]) ts
  for_ ([("prod", True), ("staging", False)] :: [(Text, Bool)]) \(env, withDb) -> do
    trId <- hexId
    [gw, gwc, co, db] <- replicateM 4 hexId
    span' trId gw Nothing "GET /checkout" PT.Span'SPAN_KIND_SERVER "gateway" env []
    span' trId gwc (Just gw) "POST checkout" PT.Span'SPAN_KIND_CLIENT "gateway" env []
    span' trId co (Just gwc) "POST /checkout" PT.Span'SPAN_KIND_SERVER "checkout" env []
    when withDb $ span' trId db (Just co) "pg.query" PT.Span'SPAN_KIND_CLIENT "checkout" env [mkAttr "db.system.name" "postgresql"]


-- | One hop from @api@ into an uninstrumented peer, to exercise the fold without OTLP ingestion.
hop :: Text -> NodeKind -> Int64 -> EdgeSample
hop target kind reqs = EdgeSample ("api", NKService) (target, kind) reqs 0 (singletonLatency 1_000_000) 0


nodeBy :: ServiceGraph -> Text -> Maybe ServiceNode
nodeBy g k = V.find (\n -> n.key == k) g.nodes


edgePairs :: ServiceGraph -> [(Text, Text)]
edgePairs g = L.sort [(e.source, e.target) | e <- V.toList g.edges]


kqlBase :: UTCTime
kqlBase = read "2025-01-01 00:00:00 UTC"


kqlAt :: NominalDiffTime -> Text
kqlAt s = toText $ iso8601Show $ addUTCTime s kqlBase


-- | A catalogue row, seen at @seen@ (default: the database's now).
seedMeta :: TestResources -> Projects.ProjectId -> Text -> Text -> [Text] -> Maybe UTCTime -> IO ()
seedMeta tr pid name svc labels seen =
  dbExec
    tr
    [sql| INSERT INTO otel_metrics_meta (project_id, metric_name, metric_type, metric_unit, metric_description, service_name, scope_name, metric_labels, first_seen_at, last_seen_at, first_timestamp, last_timestamp)
          SELECT ?, ?, 'GAUGE', '1', 'seeded', ?, 'test', ?, t, t, t, t FROM (SELECT COALESCE(?::timestamptz, now()) AS t) s
          ON CONFLICT DO NOTHING |]
    (pid, name, svc, PGArray labels, seen)


metaProject :: TestResources -> Text -> [(Text, Text, [Text], Maybe UTCTime)] -> IO Projects.ProjectId
metaProject tr title metrics = do
  pid <- createTestProject tr title
  for_ metrics \(name, svc, labels, seen) -> seedMeta tr pid name svc labels seen
  pure pid


-- | Ingestion hex-decodes span/trace ids and hex-encodes exemplar ids, so both sides store these literals.
exemplarTrace, exemplarSpan, exemplarMetric :: Text
exemplarTrace = "a1b2c3d4e5f60718293a4b5c6d7e8f90"
exemplarSpan = "0102030405060708"
exemplarMetric = "test.exemplar.gauge"


-- | One gauge datapoint carrying one exemplar naming 'exemplarTrace'. The proto's trace_id is
-- raw bytes, not hex: hex here would be hex-encoded again and the join would never match.
ingestMetricWithExemplar :: TestResources -> Text -> IO ()
ingestMetricWithExemplar tr apiKey = do
  let nanos = floor (utcTimeToPOSIXSeconds frozenTime * 1000000000) :: Word64
      bytes = B16.decodeLenient . encodeUtf8
      exemplar = defMessage & PMF.timeUnixNano .~ nanos & PMF.asDouble .~ 42.5 & PMF.traceId .~ bytes exemplarTrace & PMF.spanId .~ bytes exemplarSpan :: PM.Exemplar
      point = defMessage & PMF.timeUnixNano .~ nanos & PMF.asDouble .~ 42.5 & PMF.exemplars .~ [exemplar] :: PM.NumberDataPoint
      metric = defMessage & PMF.name .~ exemplarMetric & PMF.gauge .~ (defMessage & PMF.dataPoints .~ [point]) :: PM.Metric
      request = defMessage & MSF.resourceMetrics .~ [defMessage & PMF.resource .~ mkResource apiKey [] & PMF.scopeMetrics .~ [defMessage & PMF.metrics .~ [metric]]] :: MS.ExportMetricsServiceRequest
  void $ OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto request)


-- | A /metrics body: a counter family (two series), a gauge, and a non-finite sample to drop.
promBody :: LByteString
promBody =
  "# HELP http_requests_total Total requests\n# TYPE http_requests_total counter\n\
  \http_requests_total{method=\"get\",code=\"200\"} 42\nhttp_requests_total{method=\"post\",code=\"500\"} 7\n\
  \# TYPE temperature_celsius gauge\ntemperature_celsius 21.5\nbroken_metric +Inf\n"

