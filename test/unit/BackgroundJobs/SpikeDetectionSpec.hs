module BackgroundJobs.SpikeDetectionSpec (spec) where

import BackgroundJobs (detectSpikeOrDrop, spikeMinAbsoluteDelta, spikeZScoreThreshold)
import Models.Apis.Issues (RateChangeDirection (..))
import Relude
import Test.Hspec


spec :: Spec
spec = describe "detectSpikeOrDrop" do
  it "emits Spike when z-score > threshold and delta > minAbsolute" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 10 1 24 -- rate=24, z=(24-10)/1=14
    fmap (\(_, _, _, _, d) -> d) result `shouldBe` Just Spike

  it "emits Drop when z-score < -threshold and delta > minAbsolute" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 5 70 -- rate=70, z=(70-100)/5=-6
    fmap (\(_, _, _, _, d) -> d) result `shouldBe` Just Drop

  it "returns Nothing when z-score is within threshold" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 5 110 -- z=(110-100)/5=2 < 3
    result `shouldBe` Nothing

  it "returns Nothing when delta is below minimum" do
    -- z=(8-2)/1=6 > threshold, but delta=6 < spikeMinAbsoluteDelta=10
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 2 1 8
    result `shouldBe` Nothing

  it "returns Nothing when mad is zero" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 0 200 `shouldBe` Nothing

  it "returns Nothing when mad is negative" do
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 (-1) 200 `shouldBe` Nothing

  it "spike at exact threshold boundary" do
    -- mean=100, mad=10, rate=131: z=(131-100)/10=3.1 > 3, delta=31 > 10
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 10 131
    fmap (\(_, _, _, _, d) -> d) result `shouldBe` Just Spike

  it "returns z-score in result" do
    let result = detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 10 1 24 -- z=(24-10)/1=14
    fmap (\(_, _, _, z, _) -> z) result `shouldBe` Just 14.0

  it "no spike at just-below threshold" do
    -- mean=100, mad=10, rate=129: z=(129-100)/10=2.9 < 3
    detectSpikeOrDrop spikeZScoreThreshold spikeMinAbsoluteDelta 100 10 129 `shouldBe` Nothing
