module Models.DashboardTemplatesSpec (spec) where

import Data.List qualified as L (isSuffixOf)
import Data.Text qualified as T
import Models.Projects.Dashboards qualified as Dashboards
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (parseQueryToAST)
import Relude
import System.Directory (listDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe, shouldSatisfy)


templatesDir :: FilePath
templatesDir = "static/public/dashboards"


allWidgets :: Dashboards.Dashboard -> [Widget.Widget]
allWidgets d = concatMap flatten (d.widgets <> maybe [] (concatMap (.widgets)) d.tabs)
  where
    flatten w = w : maybe [] (concatMap flatten) w.children


-- | Replace {{placeholder}} spans with a literal so template queries parse standalone.
stripPlaceholders :: Text -> Text
stripPlaceholders t = case T.splitOn "{{" t of
  (prefix : rest) -> prefix <> mconcat [(\(_, after) -> "x" <> T.drop 2 after) (T.breakOn "}}" seg) | seg <- rest]
  [] -> t


-- | Templates whose integration is detected from ingested metric names.
autoProvisionable :: [Text]
autoProvisionable = ["postgresql.yaml", "mysql.yaml", "redis.yaml", "docker.yaml", "kubernetes.yaml", "host-prometheus.yaml"]


spec :: Spec
spec = describe "dashboard templates" do
  files <- runIO $ sort . filter (".yaml" `L.isSuffixOf`) <$> listDirectory templatesDir
  templates <- runIO $ Dashboards.readDashboardsFromDisk templatesDir
  let withEndpointTemplate action = maybe (expectationFailure "missing template: endpoint-stats.yaml") action $ find (\d -> d.file == Just "endpoint-stats.yaml") templates

  it "every template YAML parses into a Dashboard" do
    map (.file) templates `shouldBe` map (Just . fromString) files

  it "every KQL widget query parses" do
    let broken =
          [ (d.file, q)
          | d <- templates
          , w <- allWidgets d
          , Just q <- [w.query]
          , Left _ <- [parseQueryToAST (stripPlaceholders q)]
          ]
    unless (null broken) $ expectationFailure $ "unparseable widget queries: " <> show broken

  it "integration templates declare discovery_metrics and query the metrics source" do
    forM_ autoProvisionable \file ->
      case find (\d -> d.file == Just file) templates of
        Nothing -> expectationFailure $ "missing template: " <> toString file
        Just d -> do
          d.discoveryMetrics `shouldSatisfy` isJust
          let metricQueries = [q | w <- allWidgets d, Just q <- [w.query], "metrics" `T.isPrefixOf` T.strip q]
          metricQueries `shouldSatisfy` (not . null)

  it "RUM scopes Web Vitals by application and counts each session once" do
    case find (\d -> d.file == Just "rum.yaml") templates of
      Nothing -> expectationFailure "missing template: rum.yaml"
      Just d -> do
        let widgets = allWidgets d
            metricSql = [sql | w <- widgets, Just sql <- [w.sql], "FROM otel_metrics" `T.isInfixOf` sql]
            sessionQueries = [q | w <- widgets, w.title == Just "Sessions", Just q <- [w.query]]
        metricSql `shouldSatisfy` (not . null)
        forM_ metricSql (`shouldSatisfy` T.isInfixOf "resource___service___name = '{{var-app}}'")
        sessionQueries `shouldSatisfy` (not . any (T.isInfixOf "dcount"))

  it "endpoint latency summaries average percentile buckets and declare milliseconds" $ withEndpointTemplate \d -> do
    let flatten w = w : maybe [] (concatMap flatten) w.children
        latencyWidgets = maybe [] (concatMap flatten . (.widgets)) $ find ((== "Latency") . (.name)) (fromMaybe [] d.tabs)
        summaryTitles = ["P50 Latency", "P75 Latency", "P95 Latency", "P99 Latency"]
        summaries = filter (\w -> w.title `elem` (Just <$> summaryTitles)) latencyWidgets
        trend = find (\w -> w.title == Just "Latency Percentiles Over Time") latencyWidgets
    length summaries `shouldBe` length summaryTitles
    forM_ summaries \w -> do
      w.summarizeBy `shouldBe` Just Widget.SBMean
      w.unit `shouldBe` Just "ms"
    (trend >>= (.unit)) `shouldBe` Just "ms"

  it "endpoint analytics ships a direct-dependency investigation map" $ withEndpointTemplate \d -> do
    let maps = filter (\w -> w.wType == Widget.WTServiceMap) (allWidgets d)
    maps `shouldSatisfy` (not . null)
    forM_ maps \w -> do
      w.title `shouldBe` Just "Endpoint Dependency Map"
      (w.layout >>= (.w)) `shouldBe` Just 12

  it "endpoint analytics only advertises replay when its session index has a recording" $ withEndpointTemplate \d -> do
    let sessionWidget = find (\w -> w.title == Just "Endpoint Sessions") (allWidgets d)
    (sessionWidget >>= (.dbSource)) `shouldBe` Just "postgres"
    (sessionWidget >>= (.sql)) `shouldSatisfy` maybe False (T.isInfixOf "projects.replay_sessions")
    (sessionWidget >>= (.sql)) `shouldSatisfy` maybe False (T.isInfixOf "'Available'")

  it "endpoint analytics only joins Web Vitals with an explicit browser session correlation" $ withEndpointTemplate \d -> do
    let vitalsWidget = find (\w -> w.title == Just "Request-linked Web Vitals") (allWidgets d)
    (vitalsWidget >>= (.dbSource)) `shouldBe` Just "postgres"
    (vitalsWidget >>= (.sql)) `shouldSatisfy` maybe False (T.isInfixOf "endpoint_sessions")
    (vitalsWidget >>= (.sql)) `shouldSatisfy` maybe False (T.isInfixOf "endpoint_sessions.session_id = vital_samples.session_id")

  it "endpoint analytics derives browser cohorts only from observed browser telemetry" $ withEndpointTemplate \d -> do
    let cohortWidget = find (\w -> w.title == Just "Browser Cohorts") (allWidgets d)
    (cohortWidget >>= (.dbSource)) `shouldBe` Just "postgres"
    (cohortWidget >>= (.sql)) `shouldSatisfy` maybe False (T.isInfixOf "resource___user_agent___original")
    (cohortWidget >>= (.sql)) `shouldSatisfy` maybe False (T.isInfixOf "COUNT(DISTINCT NULLIF(attributes___session___id, ''))")
