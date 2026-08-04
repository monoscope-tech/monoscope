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
          fromMaybe [] d.discoveryMetrics `shouldSatisfy` (not . null)
          let metricQueries = [q | w <- allWidgets d, Just q <- [w.query], "metrics" `T.isPrefixOf` T.strip q]
          metricQueries `shouldSatisfy` (not . null)
