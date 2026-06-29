module Models.Telemetry.MetricServiceNameSpec (spec) where

import Data.Aeson qualified as AE
import Models.Telemetry.Telemetry (metricServiceNameFromResource)
import Relude
import Test.Hspec

spec :: Spec
spec = describe "metric service name extraction" do
  it "reads nested service.name resources produced by OTLP dot-notation expansion" do
    let resource = AE.object ["service" AE..= AE.object ["name" AE..= ("vaults3" :: Text)]]
    metricServiceNameFromResource "vaults3_requests_total" resource `shouldBe` "vaults3"

  it "preserves flat service.name resources for backwards compatibility" do
    let resource = AE.object ["service.name" AE..= ("api" :: Text)]
    metricServiceNameFromResource "http.server.duration" resource `shouldBe` "api"

  it "uses container.name for container-scoped metrics without service.name" do
    let resource = AE.object ["container" AE..= AE.object ["name" AE..= ("frigate" :: Text)]]
    metricServiceNameFromResource "container.cpu.utilization" resource `shouldBe` "frigate"

  it "uses compose_service when no service or container name is present" do
    let resource = AE.object ["compose_service" AE..= ("backend" :: Text)]
    metricServiceNameFromResource "container.memory.usage" resource `shouldBe` "backend"

  it "groups host system metrics under SYSTEM instead of unknown" do
    metricServiceNameFromResource "system.cpu.time" (AE.object []) `shouldBe` "SYSTEM"

  it "falls back to unknown when no meaningful source is present" do
    metricServiceNameFromResource "custom.metric" (AE.object []) `shouldBe` "unknown"
