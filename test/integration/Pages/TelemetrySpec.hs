module Pages.TelemetrySpec (spec) where

import Pages.Telemetry (metricDetailUrl)
import Pkg.TestUtils (testPid)
import Relude
import Test.Hspec


spec :: Spec
spec =
  describe "metric chart drawer" do
    it "preserves the selected metric source in the detail request" do
      metricDetailUrl testPid "system.cpu.utilization" "accounting"
        `shouldBe` "/p/" <> testPid.toText <> "/metrics/details/system.cpu.utilization/?metric_source=accounting"
