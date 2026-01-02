module Pkg.AI.AgenticSpec (spec) where

import Pkg.AI
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
  describe "ToolLimits" do
    it "defaultLimits has reasonable values" do
      defaultLimits.maxFieldValues `shouldBe` 20
      defaultLimits.maxSampleLogs `shouldBe` 5
      defaultLimits.maxServices `shouldBe` 20
      defaultLimits.defaultFieldLimit `shouldBe` 10
      defaultLimits.defaultSampleLimit `shouldBe` 3
      defaultLimits.maxQueryResults `shouldBe` 100
      defaultLimits.maxDisplayRows `shouldBe` 20
      defaultLimits.maxBodyPreview `shouldBe` 100
