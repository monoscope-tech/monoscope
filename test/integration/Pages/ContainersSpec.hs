-- | Container inventory: ingest OTLP metrics shaped exactly as an OpenTelemetry Collector's
-- @kubeletstats@/@k8s_cluster@ and @docker_stats@ receivers emit them, then assert the page
-- handler renders them the way Datadog's Containers Explorer would — one table, both runtimes.
module Pages.ContainersSpec (spec) where

import Data.Cache qualified as Cache
import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (addUTCTime)
import Data.Vector qualified as V
import Lucid qualified
import Models.Telemetry.Containers (ContainerRow (..), ContainerSnapshotKey, Runtime (..), Scope (..), containersInWindow, cpuPctOfLimit, memPctOfLimit, runtimeOf)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (Deferred (..))
import Pages.Containers qualified as Containers
import Pages.Infrastructure qualified as Infrastructure
import Pkg.Components.Table (Table (..))
import Pkg.Icons qualified as Icons
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as PC
import Relude
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders)
import Test.Hspec


-- | The containers handler's facets, so a test names only the one it varies and a new
-- query param does not have to be threaded through every call site as another @Nothing@.
data ContainerQuery = ContainerQuery
  { runtime :: Maybe Text
  , namespace :: Maybe Text
  , node :: Maybe Text
  , image :: Maybe Text
  , cluster :: Maybe Text
  }


noContainerFilters :: ContainerQuery
noContainerFilters = ContainerQuery Nothing Nothing Nothing Nothing Nothing


-- | The loaded body of a deferred response. A shell here means the handler skipped the query
-- the test is asserting on, which is a failure worth naming rather than a pattern-match crash.
deferredBody :: Deferred a -> IO a
deferredBody = \case
  DeferredBody body -> pure body
  DeferredShell{} -> fail "handler answered with the deferred shell; expected the loaded body"


-- | What a first, non-deferred request answers with: page chrome and a skeleton, no query.
shellHtml :: Lucid.ToHtml a => TestResources -> ATAuthCtx (RespHeaders a) -> IO Text
shellHtml tr render = LT.toStrict . Lucid.renderText . Lucid.toHtml . snd <$> testServant tr render


containersPage :: ContainerQuery -> ATAuthCtx (RespHeaders Containers.ContainersGet)
containersPage q = Containers.containersGetH testPid q.runtime q.namespace q.node q.image q.cluster Nothing Nothing Nothing (Just "1")


-- | A pod from the OpenTelemetry demo, as kubeletstats + k8s_cluster describe it: usage in
-- cores and bytes from the @container.*@ family, requests/limits/restarts/ready from
-- @k8s.container.*@, and the node only ever present inside the resource blob.
k8sResource :: Text -> Text -> Text -> [PC.KeyValue]
k8sResource ns pod container = mkAttr "k8s.deployment.name" container : k8sPodResource ns pod container


-- | The same pod without any controller attribute. k8sattributes names the controller after
-- its kind, so a StatefulSet or DaemonSet pod carries no @k8s.deployment.name@ whatsoever.
k8sPodResource :: Text -> Text -> Text -> [PC.KeyValue]
k8sPodResource ns pod container =
  [ mkAttr "k8s.namespace.name" ns
  , mkAttr "k8s.pod.name" pod
  , mkAttr "k8s.container.name" container
  , mkAttr "k8s.node.name" "vps-d6d7e318"
  , mkAttr "container.image.name" "ghcr.io/open-telemetry/demo"
  , mkAttr "container.image.tag" "2.2.0"
  , -- k8sattributes can supply the uid; nothing can supply the cluster's name, which is
    -- why the query coalesces one to the other. Only the checked pod gets a name below,
    -- so both sides of that fallback are exercised.
    mkAttr "k8s.cluster.uid" clusterUid
  ]


clusterUid :: Text
clusterUid = "9b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d"


-- | A Swarm task as docker_stats describes it: no pod, no namespace, the host in
-- @host.name@, and CPU reported with Docker's percent-of-a-single-core convention.
dockerResource :: Text -> [PC.KeyValue]
dockerResource name =
  [ mkAttr "container.name" name
  , mkAttr "container.runtime" "docker"
  , mkAttr "container.id" "26550399cdefd1a6"
  , mkAttr "container.image.name" "docker.redpanda.com/redpandadata/redpanda:v25.1.4"
  , mkAttr "host.name" "0ce201583b04"
  ]


ingestFixture :: TestResources -> Text -> IO ()
ingestFixture tr key = do
  let emit :: [PC.KeyValue] -> (Text, Double) -> IO ()
      emit res (name, value) = ingestMetric tr key res [] name value frozenTime
      -- Dimensioned datapoints: hostmetrics reports CPU per logical core per mode and memory
      -- per state, so these are many series under one metric name, not one value.
      emitDim res dp (name, value) = ingestMetric tr key res dp name value frozenTime
      k8sChecked = mkAttr "k8s.cluster.name" "otel-demo" : k8sResource "default" "checkout-7fb5b4f859-nlcjs" "checkout"
      k8sOther = k8sResource "kube-system" "coredns-c6d9fc49c-bj5sw" "coredns"
      k8sStateful = mkAttr "k8s.statefulset.name" "postgres" : k8sPodResource "data" "postgres-0" "postgres"

  -- 0.5 cores against a 2-core limit and 512 MiB against 1 GiB: both percentages land on
  -- round numbers so a unit slip anywhere in the pipeline is obvious rather than plausible.
  forM_
    ( [ ("container.cpu.usage", 0.5)
      , ("container.memory.working_set", 536870912)
      , ("k8s.container.cpu_limit", 2)
      , ("k8s.container.cpu_request", 1)
      , ("k8s.container.memory_limit", 1073741824)
      , ("k8s.container.memory_request", 268435456)
      , ("k8s.container.restarts", 3)
      , ("k8s.container.ready", 1)
      ]
        :: [(Text, Double)]
    )
    (emit k8sChecked)

  -- A second namespace, and deliberately no CPU limit — the case Datadog documents as
  -- "cannot infer the usage percentage", which must stay blank rather than become 0%.
  forM_
    ( [ ("container.cpu.usage", 0.25)
      , ("container.memory.working_set", 33554432)
      , ("k8s.container.ready", 0)
      ]
        :: [(Text, Double)]
    )
    (emit k8sOther)

  -- Docker: 80 means 0.8 cores under Docker's own formula, and the memory limit arrives on a
  -- different metric name from the Kubernetes one.
  forM_
    ( [ ("container.cpu.utilization", 80)
      , ("container.memory.usage.total", 3221225472)
      , ("container.memory.usage.limit", 4294967296)
      ]
        :: [(Text, Double)]
    )
    (emit $ dockerResource "srv-captain--redpanda-0.1.tt13bkp5")

  -- A StatefulSet pod, and memory on kubeletstats' container.memory.usage rather than
  -- working_set: neither the workload nor the memory reading survives without a fallback.
  forM_ ([("container.cpu.usage", 0.1), ("container.memory.usage", 16777216)] :: [(Text, Double)]) (emit k8sStateful)

  -- A pod whose collector runs kubeletstats without the container metric group: only
  -- k8s.pod.* arrives, so the pod itself has to stand in for its containers.
  forM_
    ([("k8s.pod.cpu.usage", 0.75), ("k8s.pod.memory.working_set", 268435456)] :: [(Text, Double)])
    (emit $ mkAttr "k8s.deployment.name" "frontend" : k8sPodResource "shop" "frontend-544d9b6f4-2xk7z" "frontend")

  -- The same pod metrics for a pod that DOES report container metrics: the rollup must lose
  -- to its containers rather than appear beside them.
  forM_
    ([("k8s.pod.cpu.usage", 9.9), ("k8s.pod.memory.working_set", 9999999)] :: [(Text, Double)])
    (emit $ k8sPodResource "default" "checkout-7fb5b4f859-nlcjs" "checkout")

  -- A bare node. Four logical cores, each reporting idle and user; 0.25 busy on each of two
  -- of them is 0.5 cores of a 4-core box. Memory is per state and the limit is their sum.
  let host = [mkAttr "host.name" "vps-bare-01"]
      cpuDp core mode = [mkAttr "cpu.logical_number" core, mkAttr "cpu.mode" mode]
  forM_ (["cpu0", "cpu1", "cpu2", "cpu3"] :: [Text]) \core -> do
    emitDim host (cpuDp core "idle") ("system.cpu.utilization", 0.875)
    emitDim host (cpuDp core "user") ("system.cpu.utilization", 0.125)
  forM_ ([("used", 2147483648), ("free", 1073741824), ("cached", 1073741824)] :: [(Text, Double)]) \(st, v) ->
    emitDim host [mkAttr "system.memory.state" st] ("system.memory.usage", v)

  -- A Docker Swarm task. docker_stats reports the task name and no service attribute, so the
  -- service has to be read back out of the name.
  forM_
    ([("container.cpu.utilization", 40), ("container.memory.usage.total", 1073741824)] :: [(Text, Double)])
    (emit $ dockerResource "srv-captain--api.2.wcxx3mlmh1q1jpwk9ohyjjv7c")

  -- Noise the inventory must ignore: a metric with no container identity at all.
  ingestMetric tr key [] [] "http.server.duration" 12 frozenTime


rowsByName :: V.Vector ContainerRow -> [(Text, ContainerRow)]
rowsByName = map (\r -> (r.containerName, r)) . V.toList


-- Fails the example by name rather than by pattern-match exception, so a missing container
-- reads as "no container named coredns" instead of "Irrefutable pattern failed".
containerNamed :: [(Text, ContainerRow)] -> Text -> IO ContainerRow
containerNamed byName n = maybe (fail $ "no container named " <> toString n) pure (lookup n byName)


-- @shouldSatisfy (T.isInfixOf s)@ on a rendered page dumps the whole document and never says
-- which string was missing.
shouldContainAll :: Text -> [Text] -> Expectation
shouldContainAll haystack needles =
  case filter (not . (`T.isInfixOf` haystack)) needles of
    [] -> pass
    missing -> expectationFailure $ "missing from rendered page: " <> show missing


spec :: Spec
-- Sequential and aroundAll: the fixture is ingested once and the later examples read it back
-- through the handler, which is the point — they assert on the same rows a user would see.
spec = sequential $ aroundAll withTestResources do
  describe "Containers page" do
    it "emptyProject_rendersZeroStateRatherThanAnEmptyTable" \tr -> do
      rows <- runTestBg frozenTime tr $ containersInWindow False testPid (addUTCTime (-900) frozenTime) frozenTime
      V.toList rows `shouldBe` []

      (_, page) <- testServant tr $ containersPage noContainerFilters
      let Containers.ContainersPage (PageCtx conf body) = page
      table <- deferredBody body
      conf.pageTitle `shouldBe` "Containers"
      V.length table.rows `shouldBe` 0
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      html `shouldContainAll` ["No containers reporting"]

      (_, hostMap) <- testServant tr $ Infrastructure.hostMapGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "1")
      LT.toStrict (Lucid.renderText $ Lucid.toHtml hostMap)
        `shouldContainAll` ["No hosts reporting", "Set up host monitoring"]

    it "kubernetesAndDockerContainers_landInOneListWithNormalisedCpuAndMemory" \tr -> do
      key <- createTestAPIKey tr testPid "containers-key"
      ingestFixture tr key
      rows <- runTestBg frozenTime tr $ containersInWindow False testPid (addUTCTime (-900) frozenTime) frozenTime
      let byName = rowsByName rows

      -- The noise metric carries no container identity, so it must not become a row.
      -- checkout also reports k8s.pod.* metrics; the pod rollup must not appear beside it.
      map fst byName
        `shouldMatchList` [ "checkout"
                          , "coredns"
                          , "postgres"
                          , "frontend-544d9b6f4-2xk7z"
                          , "vps-bare-01"
                          , "srv-captain--redpanda-0.1.tt13bkp5"
                          , "srv-captain--api.2.wcxx3mlmh1q1jpwk9ohyjjv7c"
                          ]

      k8s <- containerNamed byName "checkout"
      runtimeOf k8s `shouldBe` Kubernetes
      k8s.podName `shouldBe` Just "checkout-7fb5b4f859-nlcjs"
      k8s.namespace `shouldBe` Just "default"
      -- Node, image and workload live only in the resource blob, so this is also the
      -- assertion that the Postgres JSONB accessor path works.
      k8s.nodeName `shouldBe` Just "vps-d6d7e318"
      k8s.image `shouldBe` Just "ghcr.io/open-telemetry/demo"
      k8s.imageTag `shouldBe` Just "2.2.0"
      k8s.workload `shouldBe` Just "checkout"
      k8s.cpuCores `shouldBe` Just 0.5
      k8s.memBytes `shouldBe` Just 536870912
      k8s.restarts `shouldBe` Just 3
      cpuPctOfLimit k8s `shouldBe` Just 0.25
      memPctOfLimit k8s `shouldBe` Just 0.5

      -- Docker's percent-of-a-single-core is normalised onto the same cores axis as
      -- Kubernetes, which is the whole reason one CPU column is honest for both runtimes.
      docker <- containerNamed byName "srv-captain--redpanda-0.1.tt13bkp5"
      runtimeOf docker `shouldBe` Docker
      docker.podName `shouldBe` Nothing
      docker.nodeName `shouldBe` Just "0ce201583b04"
      docker.cpuCores `shouldBe` Just 0.8
      -- The Docker memory limit arrives on container.memory.usage.limit, not the k8s name.
      memPctOfLimit docker `shouldBe` Just 0.75
      -- Docker reports no CPU limit and no restart count; both must stay absent.
      cpuPctOfLimit docker `shouldBe` Nothing
      docker.restarts `shouldBe` Nothing
      -- docker_stats sends the tag inside the image name and no container.image.tag, so the
      -- tag is split off rather than left to make the Image column disagree between runtimes.
      docker.image `shouldBe` Just "docker.redpanda.com/redpandadata/redpanda"
      docker.imageTag `shouldBe` Just "v25.1.4"

      -- Every controller kind names the workload, not just Deployment, and kubeletstats'
      -- container.memory.usage stands in when working_set is absent.
      stateful <- containerNamed byName "postgres"
      stateful.workload `shouldBe` Just "postgres"
      stateful.memBytes `shouldBe` Just 16777216

      -- kubeletstats without the container metric group: the pod stands in for containers we
      -- cannot see, and reads as Kubernetes with no image of its own.
      podOnly <- containerNamed byName "frontend-544d9b6f4-2xk7z"
      runtimeOf podOnly `shouldBe` Kubernetes
      podOnly.scope `shouldBe` ScopePod
      podOnly.cpuCores `shouldBe` Just 0.75
      podOnly.memBytes `shouldBe` Just 268435456
      podOnly.workload `shouldBe` Just "frontend"
      podOnly.image `shouldBe` Nothing

      -- A bare node: 0.125 busy on each of four cores is 0.5 cores of a 4-core box, and the
      -- node's capacity is its limit. Reading MAX over the per-core series instead would pick
      -- up a core's 0.875 idle and call the machine busy.
      node <- containerNamed byName "vps-bare-01"
      runtimeOf node `shouldBe` Host
      node.cpuCores `shouldBe` Just 0.5
      node.cpuLimit `shouldBe` Just 4
      cpuPctOfLimit node `shouldBe` Just 0.125
      node.memBytes `shouldBe` Just 2147483648
      node.memLimit `shouldBe` Just 4294967296
      memPctOfLimit node `shouldBe` Just 0.5
      -- The collector's own pod must never be stamped on the node it is measuring.
      node.podName `shouldBe` Nothing
      node.namespace `shouldBe` Nothing
      node.nodeName `shouldBe` Just "vps-bare-01"

      -- Swarm names a task <service>.<slot>.<taskid> and sends no service attribute, so every
      -- replica looked like an unrelated container with no workload.
      swarm <- containerNamed byName "srv-captain--api.2.wcxx3mlmh1q1jpwk9ohyjjv7c"
      runtimeOf swarm `shouldBe` Docker
      swarm.workload `shouldBe` Just "srv-captain--api"

      -- A container with no CPU limit yields no percentage — never a fabricated 0%.
      noLimit <- containerNamed byName "coredns"
      noLimit.cpuLimit `shouldBe` Nothing
      cpuPctOfLimit noLimit `shouldBe` Nothing

    it "facets_narrowTheListAndTheHandlerRendersBothRuntimes" \tr -> do
      (_, page) <- testServant tr $ containersPage noContainerFilters
      let Containers.ContainersPage (PageCtx _ body) = page
      table <- deferredBody body
      V.length table.rows `shouldBe` 7
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
          cacheKey = (testPid, Nothing, Nothing, Just "5M") :: ContainerSnapshotKey
      cached <- Cache.lookup tr.trATCtx.infrastructureCache cacheKey :: IO (Maybe (V.Vector ContainerRow))
      V.length <$> cached `shouldBe` Just 7
      -- Both runtimes in one table, with the facet menus the filter dropdown is built from.
      html `shouldContainAll` ["checkout", "srv-captain--redpanda-0.1.tt13bkp5", "kube-system", "namespace=", "runtime=", "cluster=", "otel-demo", "vps-bare-01", "Search containers", "CPU limit used", "Memory limit used", "text-xs font-semibold leading-none", "&quot;hide_value&quot;:true", "data-component=\"facet-rail\"", "data-component=\"facet-section\"", "data-component=\"facet-option\"", "Last 5 mins", "bg-bgAlternate sticky", "container-usage-chart bg-bgRaised px-2 pt-2", "bg-fillWarning-strong", "hidden max-md:inline-flex", "flex min-w-0 items-center text-textStrong max-md:hidden", "text-textWeak widget-subtitle", "/infrastructure/containers/detail?since=5M"]
      T.isInfixOf "group/summary" html `shouldBe` False
      T.isInfixOf "Usage over time" html `shouldBe` False
      T.isInfixOf "min-height:" html `shouldBe` False
      T.count "class=\"container-usage-chart " html `shouldBe` 2
      html `shouldContainAll` ["px-2 pt-2", "\"bottom\":0", "\"tooltip\":{\"show\":true}"]
      T.count "data-component=\"facet-section\" open>" html `shouldBe` 1
      -- The screenshot regression: "Not ready" wrapped onto two lines in the narrow status
      -- column, crossing the badge border. The label is one indivisible status.
      html `shouldContainAll` ["Not ready", "class=\"badge badge-sm badge-error whitespace-nowrap\""]
      -- The regular download symbol has 24px path coordinates in a 16px viewBox and renders
      -- as a clipped stroke. The table export uses the correctly framed Font Awesome symbol.
      html `shouldContainAll` ["M288 32c0-17.7-14.3-32-32-32s-32 14.3-32 32"]

      -- Each facet narrows independently, and an unknown value yields an empty list rather
      -- than silently falling back to "all".
      let listWith f = do
            (_, p) <- testServant tr f
            let Containers.ContainersPage (PageCtx _ b) = p
            t <- deferredBody b
            pure $ V.toList $ V.map (\vm -> vm.row.containerName) t.rows
      listWith (containersPage noContainerFilters{namespace = Just "default"}) >>= (`shouldBe` ["checkout"])
      listWith (containersPage noContainerFilters{runtime = Just "docker"}) >>= (`shouldMatchList` ["srv-captain--api.2.wcxx3mlmh1q1jpwk9ohyjjv7c", "srv-captain--redpanda-0.1.tt13bkp5"])
      listWith (containersPage noContainerFilters{runtime = Just "kubernetes"}) >>= (`shouldMatchList` ["checkout", "coredns", "postgres", "frontend-544d9b6f4-2xk7z"])
      listWith (containersPage noContainerFilters{node = Just "0ce201583b04"}) >>= (`shouldMatchList` ["srv-captain--api.2.wcxx3mlmh1q1jpwk9ohyjjv7c", "srv-captain--redpanda-0.1.tt13bkp5"])
      listWith (containersPage noContainerFilters{namespace = Just "no-such-namespace"}) >>= (`shouldBe` [])
      -- Cluster prefers the name where the collector set one, and falls back to the uid
      -- where it did not, so the facet is usable before anyone edits a collector config.
      listWith (containersPage noContainerFilters{cluster = Just "otel-demo"}) >>= (`shouldBe` ["checkout"])
      listWith (containersPage noContainerFilters{cluster = Just clusterUid}) >>= (`shouldMatchList` ["coredns", "postgres", "frontend-544d9b6f4-2xk7z"])
      -- A bare node is its own runtime, so it is reachable and does not pollute the others.
      listWith (containersPage noContainerFilters{runtime = Just "host"}) >>= (`shouldBe` ["vps-bare-01"])

    -- Keep both identity columns readable when image and workload names are long.
    it "longContainerMetadata_doesNotOverlapPodColumn" \tr -> do
      (_, page) <- testServant tr $ containersPage noContainerFilters
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      html
        `shouldContainAll` [ "w-80 max-w-80 overflow-hidden"
                           , "w-72 max-w-72 overflow-hidden"
                           , "min-w-0 flex-1 truncate"
                           , "max-w-36 truncate"
                           , "data-tip=\"Runtime: kubernetes\""
                           , "data-tippy-content=\"Container: checkout\""
                           , "data-tippy-content=\"Image: demo:2.2.0\""
                           , "data-tippy-content=\"Workload: checkout\""
                           , "data-tippy-content=\"CPU limit used: "
                           , "data-tippy-content=\"Memory limit used: "
                           , "data-tippy-content=\"Latest reported readiness: Ready\""
                           , "Latest cumulative restart count reported within the selected time range. This is not the number of restarts during the range."
                           ]

    it "infrastructureViews_projectTheSameTelemetryIntoHostsImagesKubernetesAndMap" \tr -> do
      (_, hosts) <- testServant tr $ Infrastructure.hostsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "1")
      LT.toStrict (Lucid.renderText $ Lucid.toHtml hosts)
        `shouldContainAll` ["<h1", "Infrastructure", "Hosts", "vps-bare-01", "Kubernetes", "Docker", "Storage", "Load (1m)", "Group by", "Customize", "LIVE", "Last 5 mins", "Previous time window", "Pause live updates", "Export", "Showing 3 of 3 hosts", "flex shrink-0 items-center gap-2 whitespace-nowrap", "/infrastructure/hosts/detail?since=5M"]
      Icons.lookupIcon "solid" "download" `shouldSatisfy` isJust
      Icons.lookupIcon "regular" "cube" `shouldSatisfy` isJust

      (_, images) <- testServant tr $ Infrastructure.imagesGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "1")
      LT.toStrict (Lucid.renderText $ Lucid.toHtml images)
        `shouldContainAll` ["Images", "ghcr.io/open-telemetry/demo", "docker.redpanda.com/redpandadata/redpanda", "SBOM unavailable", "/infrastructure/images/detail", "role=\"button\""]

      (_, kubernetes) <- testServant tr $ Infrastructure.kubernetesGetH testPid (Just "pods") Nothing Nothing Nothing Nothing Nothing Nothing (Just "1")
      LT.toStrict (Lucid.renderText $ Lucid.toHtml kubernetes)
        `shouldContainAll` ["Pods", "Clusters", "Namespaces", "Workloads", "Nodes", "checkout-7fb5b4f859-nlcjs", "Not ready", "CPU limit used", "Memory limit used", "whitespace-nowrap", "Showing ", " resources", "/infrastructure/kubernetes/detail"]

      (_, hostMap) <- testServant tr $ Infrastructure.hostMapGetH testPid (Just "storage") Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "1")
      let hostMapHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml hostMap
      hostMapHtml
        `shouldContainAll` ["Host Map", "Fill by", "Group by", "vps-bare-01", "clip-path:polygon", "bg-fillNeutral-strong", "onchange=\"this.form.requestSubmit()\""]
      hostMapHtml `shouldNotSatisfy` T.isInfixOf ">Apply</button>"

      (_, hostDetail) <- testServant tr $ Infrastructure.hostDetailGetH testPid (Just "vps-bare-01") Nothing Nothing Nothing
      LT.toStrict (Lucid.renderText $ Lucid.toHtml hostDetail)
        `shouldContainAll` [ "Host summary"
                           , "CPU usage"
                           , "Memory usage"
                           , "Recent logs"
                           , "View logs in Explorer"
                           , "id=\"host-logs\""
                           , "<log-list"
                           , "initialFetchUrl=\"/p/00000000-0000-0000-0000-000000000000/log_explorer/data?"
                           , "resource.host.name%3D%3D%22vps-bare-01%22"
                           , "View in Metrics"
                           , "Metrics coverage: 2 of 4"
                           ]

      (_, imageDetail) <- testServant tr $ Infrastructure.imageDetailGetH testPid (Just "ghcr.io/open-telemetry/demo") Nothing Nothing Nothing
      LT.toStrict (Lucid.renderText $ Lucid.toHtml imageDetail)
        `shouldContainAll` ["Image summary", "Running containers", "Security coverage", "View containers"]

      (_, kubeDetail) <- testServant tr $ Infrastructure.kubernetesDetailGetH testPid (Just "pods") (Just "checkout-7fb5b4f859-nlcjs") (Just "otel-demo") (Just "default") Nothing Nothing Nothing
      LT.toStrict (Lucid.renderText $ Lucid.toHtml kubeDetail)
        `shouldContainAll` ["Pod summary", "CPU / limit", "View containers", "View logs", "View metrics"]

    it "firstRequest_rendersChromeAndASelfFetchingSkeleton_notRows" \tr -> do
      -- Every infrastructure view is one multi-second metrics pivot. The request that paints
      -- the page must not wait for it: it answers with nav, tabs and a skeleton that fetches
      -- the body itself. Rows appearing here again would mean the deferral was undone and
      -- navigating the section is back to seconds per click.
      hostsShell <- shellHtml tr $ Infrastructure.hostsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      hostsShell `shouldContainAll` ["Infrastructure", "Hosts", "id=\"hostsContainer\"", "hx-trigger=\"load\"", "/infrastructure/hosts?", "deferred=1"]
      hostsShell `shouldNotSatisfy` T.isInfixOf "vps-bare-01"

      containersShell <- shellHtml tr $ Containers.containersGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      containersShell `shouldContainAll` ["id=\"containersContainer\"", "hx-trigger=\"load\"", "deferred=1"]
      containersShell `shouldNotSatisfy` T.isInfixOf "checkout-7fb5b4f859-nlcjs"

      -- The filters the shell was asked for have to survive the round trip, or the rows that
      -- arrive would not be the rows the visible filter chips claim.
      filteredShell <- shellHtml tr $ Infrastructure.kubernetesGetH testPid (Just "pods") (Just "otel-demo") (Just "default") Nothing Nothing Nothing Nothing Nothing
      filteredShell `shouldContainAll` ["resource=pods", "cluster=otel-demo", "namespace=default", "deferred=1"]

    it "hostDetail_withoutHostMetrics_collapsesChartsIntoRecoveryState" \tr -> do
      (_, hostWithoutMetrics) <- testServant tr $ Infrastructure.hostDetailGetH testPid (Just "vps-d6d7e318") Nothing Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml hostWithoutMetrics
      html `shouldContainAll` ["No host metrics in this time range", "Try last 1 hour", "Set up host metrics", "View logs", "Metrics coverage: 0 of 4"]
      html `shouldNotSatisfy` T.isInfixOf "host-cpu"

    it "detailDrawer_showsRequestsAndLimitsAndPivots" \tr -> do
      (_, html') <- testServant tr $ Containers.containerDetailGetH testPid (Just "checkout") (Just "checkout-7fb5b4f859-nlcjs") Nothing Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml html'
      html `shouldContainAll` ["Requests and limits", "vps-d6d7e318", "View logs", "resource.k8s.pod.name", "data-tippy-content=\"Pod: checkout-7fb5b4f859-nlcjs\""]

      -- A container that stopped reporting must say so, not render a blank panel.
      (_, gone) <- testServant tr $ Containers.containerDetailGetH testPid (Just "ghost") Nothing Nothing Nothing Nothing
      LT.toStrict (Lucid.renderText $ Lucid.toHtml gone) `shouldContainAll` ["no longer reporting"]


    -- The pivot reads the newest datapoint per series, so scanning the picker's whole window
    -- can only turn up older points it then discards -- while paying to sort every one. Over a
    -- day that cost the statement timeout, which meant the snapshot never cached and every load
    -- failed. It now reads a freshness slice at the END of the window instead.
    it "wideWindow_readsOnlyTheFreshnessSlice_soStaleContainersDoNotLingerWithStaleNumbers" \tr -> do
      key <- createTestAPIKey tr testPid "containers-stale-key"
      -- Reported three hours ago and never since: outside the freshness slice of any window.
      ingestMetric tr key (dockerResource "long-gone") [] "container.cpu.usage" 9 (addUTCTime (-10800) frozenTime)

      -- A day-wide window contains that datapoint, but the slice at its end does not.
      dayRows <- runTestBg frozenTime tr $ containersInWindow False testPid (addUTCTime (-86400) frozenTime) frozenTime
      map fst (rowsByName dayRows) `shouldNotContain` ["long-gone"]

      -- ...and a window that ENDS when it reported still shows it, which is what makes this a
      -- slice of the selected range rather than a hard-coded "last 15 minutes from now".
      thenRows <- runTestBg frozenTime tr $ containersInWindow False testPid (addUTCTime (-86400) frozenTime) (addUTCTime (-10800) frozenTime)
      map fst (rowsByName thenRows) `shouldContain` ["long-gone"]
