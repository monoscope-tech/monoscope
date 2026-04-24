module BackgroundJobs.SpikeDetectionSpec (spec) where

import BackgroundJobs (aboveVolumeFloor, detectSpikeOrDrop, dropMinBaselineRate, isAlertableLogLevel, spikeMinAbsoluteDelta, spikeMinBaselineRate, spikeZScoreThreshold)
import Models.Apis.Issues (RateChangeDirection (..), SpikeResult (..))
import Relude
import Test.Hspec


spec :: Spec
spec = describe "detectSpikeOrDrop" do
  it "emits Spike when z-score > threshold and delta > minAbsolute" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 5 200 -- z=20, delta=100
    fmap (.direction) result `shouldBe` Just Spike

  it "emits Drop when z-score < -threshold and delta > minAbsolute" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 200 5 100 -- z=-20, delta=100
    fmap (.direction) result `shouldBe` Just Drop

  it "returns Nothing when z-score is within threshold" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 50 110 `shouldBe` Nothing -- z=0.2 < 3

  it "returns Nothing when delta is below minimum" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 1 140 `shouldBe` Nothing -- z=40 but delta=40 < 50

  it "uses MAD floor of 1.0 when mad is zero (stable patterns can still spike)" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 0 200 -- effectiveMad=1, z=100, delta=100
    fmap (.direction) result `shouldBe` Just Spike

  it "uses MAD floor of 1.0 when mad is negative" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 (-1) 200 -- effectiveMad=1, z=100, delta=100
    fmap (.direction) result `shouldBe` Just Spike

  it "spike at exact threshold boundary" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 10 181 -- z=8.1 > 3, delta=81 > 50
    fmap (.direction) result `shouldBe` Just Spike

  it "returns z-score in result" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 5 200 -- z=(200-100)/5=20
    fmap (.zScore) result `shouldBe` Just 20.0

  it "no spike at just-below threshold" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 50 110 `shouldBe` Nothing -- z=0.2 < 3


  describe "aboveVolumeFloor" do
    let mkSpike mean = SpikeResult{currentRate = mean * 2, mean = mean, mad = 1, zScore = 10, direction = Spike}
        mkDrop mean = SpikeResult{currentRate = 0, mean = mean, mad = 1, zScore = -10, direction = Drop}

    it "rejects spikes whose baseline is below the spike floor" do
      aboveVolumeFloor (mkSpike (spikeMinBaselineRate - 1)) `shouldBe` False

    it "accepts spikes whose baseline meets the spike floor" do
      aboveVolumeFloor (mkSpike spikeMinBaselineRate) `shouldBe` True

    it "rejects drops whose baseline is below the drop floor" do
      aboveVolumeFloor (mkDrop (dropMinBaselineRate - 1)) `shouldBe` False

    it "accepts drops whose baseline meets the drop floor" do
      aboveVolumeFloor (mkDrop dropMinBaselineRate) `shouldBe` True

    it "drops held to a stricter floor than spikes" do
      -- 1000 is plenty for a spike but too small for a drop
      aboveVolumeFloor (mkSpike 1000) `shouldBe` True
      aboveVolumeFloor (mkDrop 1000) `shouldBe` False


  describe "isAlertableLogLevel" do
    it "accepts ERROR / WARN / WARNING / FATAL / CRITICAL (any case)" do
      isAlertableLogLevel (Just "ERROR") `shouldBe` True
      isAlertableLogLevel (Just "error") `shouldBe` True
      isAlertableLogLevel (Just "Warn") `shouldBe` True
      isAlertableLogLevel (Just "warning") `shouldBe` True
      isAlertableLogLevel (Just "FATAL") `shouldBe` True
      isAlertableLogLevel (Just "critical") `shouldBe` True

    it "rejects INFO / DEBUG / TRACE / NOTICE (any case)" do
      isAlertableLogLevel (Just "INFO") `shouldBe` False
      isAlertableLogLevel (Just "info") `shouldBe` False
      isAlertableLogLevel (Just "DEBUG") `shouldBe` False
      isAlertableLogLevel (Just "debug") `shouldBe` False
      isAlertableLogLevel (Just "TRACE") `shouldBe` False
      isAlertableLogLevel (Just "notice") `shouldBe` False

    it "rejects unknown level (Nothing) — largest source of historical noise" do
      isAlertableLogLevel Nothing `shouldBe` False

    it "rejects empty string" do
      isAlertableLogLevel (Just "") `shouldBe` False
