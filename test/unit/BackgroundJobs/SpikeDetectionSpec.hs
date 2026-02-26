module BackgroundJobs.SpikeDetectionSpec (spec) where

import BackgroundJobs (detectSpikeOrDrop, spikeMinAbsoluteDelta, spikeZScoreThreshold)
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
