-- | Container inventory: ingest OTLP metrics shaped exactly as an OpenTelemetry Collector's
-- @kubeletstats@/@k8s_cluster@ and @docker_stats@ receivers emit them, then assert the page
-- handler renders them the way Datadog's Containers Explorer would — one table, both runtimes.
module Pages.ContainersSpec (spec) where

import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Vector qualified as V
import Lucid qualified
import Models.Telemetry.Containers (ContainerRow (..), Runtime (..), containersInWindow, cpuPctOfLimit, memPctOfLimit, runtimeOf)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Containers qualified as Containers
import Pkg.Components.Table (Table (..))
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as PC
import Relude
import Test.Hspec


-- | A pod from the OpenTelemetry demo, as kubeletstats + k8s_cluster describe it: usage in
-- cores and bytes from the @container.*@ family, requests/limits/restarts/ready from
-- @k8s.container.*@, and the node only ever present inside the resource blob.
k8sResource :: Text -> Text -> Text -> [PC.KeyValue]
k8sResource ns pod container =
  [ mkAttr "k8s.namespace.name" ns
  , mkAttr "k8s.pod.name" pod
  , mkAttr "k8s.container.name" container
  , mkAttr "k8s.node.name" "vps-d6d7e318"
  , mkAttr "k8s.deployment.name" container
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
      emit res (name, value) = ingestMetric tr key res name value frozenTime
      k8sChecked = mkAttr "k8s.cluster.name" "otel-demo" : k8sResource "default" "checkout-7fb5b4f859-nlcjs" "checkout"
      k8sOther = k8sResource "kube-system" "coredns-c6d9fc49c-bj5sw" "coredns"

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

  -- Noise the inventory must ignore: a metric with no container identity at all.
  ingestMetric tr key [] "http.server.duration" 12 frozenTime


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
      rows <- runTestBg frozenTime tr $ containersInWindow False testPid frozenTime
      V.toList rows `shouldBe` []

      (_, page) <- testServant tr $ Containers.containersGetH testPid Nothing Nothing Nothing Nothing Nothing
      let Containers.ContainersPage (PageCtx conf table) = page
      conf.pageTitle `shouldBe` "Containers"
      V.length table.rows `shouldBe` 0
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      html `shouldContainAll` ["No containers reporting"]

    it "kubernetesAndDockerContainers_landInOneListWithNormalisedCpuAndMemory" \tr -> do
      key <- createTestAPIKey tr testPid "containers-key"
      ingestFixture tr key
      rows <- runTestBg frozenTime tr $ containersInWindow False testPid frozenTime
      let byName = rowsByName rows

      -- The noise metric carries no container identity, so it must not become a row.
      map fst byName `shouldMatchList` ["checkout", "coredns", "srv-captain--redpanda-0.1.tt13bkp5"]

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

      -- A container with no CPU limit yields no percentage — never a fabricated 0%.
      noLimit <- containerNamed byName "coredns"
      noLimit.cpuLimit `shouldBe` Nothing
      cpuPctOfLimit noLimit `shouldBe` Nothing

    it "facets_narrowTheListAndTheHandlerRendersBothRuntimes" \tr -> do
      (_, page) <- testServant tr $ Containers.containersGetH testPid Nothing Nothing Nothing Nothing Nothing
      let Containers.ContainersPage (PageCtx _ table) = page
      V.length table.rows `shouldBe` 3
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      -- Both runtimes in one table, with the facet menus the filter dropdown is built from.
      html `shouldContainAll` ["checkout", "srv-captain--redpanda-0.1.tt13bkp5", "kube-system", "namespace=", "runtime=", "cluster=", "otel-demo"]

      -- Each facet narrows independently, and an unknown value yields an empty list rather
      -- than silently falling back to "all".
      let listWith f = do
            (_, p) <- testServant tr f
            let Containers.ContainersPage (PageCtx _ t) = p
            pure $ V.toList $ V.map (\vm -> vm.row.containerName) t.rows
      listWith (Containers.containersGetH testPid Nothing (Just "default") Nothing Nothing Nothing) >>= (`shouldBe` ["checkout"])
      listWith (Containers.containersGetH testPid (Just "docker") Nothing Nothing Nothing Nothing) >>= (`shouldBe` ["srv-captain--redpanda-0.1.tt13bkp5"])
      listWith (Containers.containersGetH testPid (Just "kubernetes") Nothing Nothing Nothing Nothing) >>= (`shouldMatchList` ["checkout", "coredns"])
      listWith (Containers.containersGetH testPid Nothing Nothing (Just "0ce201583b04") Nothing Nothing) >>= (`shouldBe` ["srv-captain--redpanda-0.1.tt13bkp5"])
      listWith (Containers.containersGetH testPid Nothing (Just "no-such-namespace") Nothing Nothing Nothing) >>= (`shouldBe` [])
      -- Cluster prefers the name where the collector set one, and falls back to the uid
      -- where it did not, so the facet is usable before anyone edits a collector config.
      listWith (Containers.containersGetH testPid Nothing Nothing Nothing Nothing (Just "otel-demo")) >>= (`shouldBe` ["checkout"])
      listWith (Containers.containersGetH testPid Nothing Nothing Nothing Nothing (Just clusterUid)) >>= (`shouldBe` ["coredns"])

    it "detailDrawer_showsRequestsAndLimitsAndPivots" \tr -> do
      (_, html') <- testServant tr $ Containers.containerDetailGetH testPid (Just "checkout") (Just "checkout-7fb5b4f859-nlcjs")
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml html'
      html `shouldContainAll` ["Requests and limits", "vps-d6d7e318", "View logs", "resource.k8s.pod.name"]

      -- A container that stopped reporting must say so, not render a blank panel.
      (_, gone) <- testServant tr $ Containers.containerDetailGetH testPid (Just "ghost") Nothing
      LT.toStrict (Lucid.renderText $ Lucid.toHtml gone) `shouldContainAll` ["no longer reporting"]
