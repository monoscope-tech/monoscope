module BackgroundJobs.SpikeDetectionSpec (spec) where

import BackgroundJobs (SpikeResult (..), detectSpikeOrDrop, spikeMinAbsoluteDelta, spikeZScoreThreshold)
import Models.Apis.Issues (RateChangeDirection (..))
import Relude
import Test.Hspec


spec :: Spec
spec = describe "detectSpikeOrDrop" do
  it "emits Spike when z-score > threshold and delta > minAbsolute" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 10 1 24 -- rate=24, z=(24-10)/1=14
    fmap (.direction) result `shouldBe` Just Spike

  it "emits Drop when z-score < -threshold and delta > minAbsolute" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 5 70 -- rate=70, z=(70-100)/5=-6
    fmap (.direction) result `shouldBe` Just Drop

  it "returns Nothing when z-score is within threshold" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 5 110 `shouldBe` Nothing -- z=2 < 3

  it "returns Nothing when delta is below minimum" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 2 1 8 `shouldBe` Nothing -- z=6 but delta=6 < 10

  it "returns Nothing when mad is zero" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 0 200 `shouldBe` Nothing

  it "returns Nothing when mad is negative" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 (-1) 200 `shouldBe` Nothing

  it "spike at exact threshold boundary" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 10 131 -- z=3.1 > 3, delta=31 > 10
    fmap (.direction) result `shouldBe` Just Spike

  it "returns z-score in result" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 10 1 24 -- z=(24-10)/1=14
    fmap (.zScore) result `shouldBe` Just 14.0

  it "no spike at just-below threshold" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 10 129 `shouldBe` Nothing -- z=2.9 < 3
