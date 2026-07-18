module Pages.TelemetrySpec (spec) where

import Pages.Telemetry (metricDetailUrl, metricExpandUrl)
import Pkg.TestUtils (testPid)
import Relude
import Test.Hspec


spec :: Spec
spec =
  describe "metric chart drawer" do
    it "preserves the selected metric source in the detail request" do
      metricDetailUrl testPid "system.cpu.utilization" "accounting" Nothing
        `shouldBe` "/p/" <> testPid.toText <> "/metrics/details/system.cpu.utilization/?metric_source=accounting"

    it "preserves the selected label in the detail request" do
      metricDetailUrl testPid "container.cpu.time" "accounting" (Just "attributes.service.name")
        `shouldBe` "/p/" <> testPid.toText <> "/metrics/details/container.cpu.time/?metric_source=accounting&label=attributes.service.name"

    it "shares an expanded chart through the metrics overview" do
      metricExpandUrl testPid "container.cpu.time" "accounting" (Just "attributes.service.name")
        `shouldBe` "/p/" <> testPid.toText <> "/metrics?tab=charts&metric_source=accounting&expand=container.cpu.time&label=attributes.service.name"
