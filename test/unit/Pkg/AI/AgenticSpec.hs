module Pkg.AI.AgenticSpec (spec) where

import Pkg.AI.Agentic
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
  describe "suggestAgenticMode" do
    it "returns FastMode for DashboardWidget regardless of query" do
      suggestAgenticMode DashboardWidget "compare errors" `shouldBe` FastMode
      suggestAgenticMode DashboardWidget "investigate issues" `shouldBe` FastMode
      suggestAgenticMode DashboardWidget "show logs" `shouldBe` FastMode

    it "returns AgenticMode for complex queries" do
      suggestAgenticMode WebExplorer "compare errors between services" `shouldBe` AgenticMode
      suggestAgenticMode WebExplorer "investigate the issue" `shouldBe` AgenticMode
      suggestAgenticMode WebExplorer "analyze performance" `shouldBe` AgenticMode
      suggestAgenticMode SlackBot "show me sample logs" `shouldBe` AgenticMode
      suggestAgenticMode DiscordBot "what changed recently" `shouldBe` AgenticMode

    it "returns FastMode for simple queries" do
      suggestAgenticMode WebExplorer "show errors" `shouldBe` FastMode
      suggestAgenticMode WebExplorer "list logs" `shouldBe` FastMode
      suggestAgenticMode SlackBot "find slow requests" `shouldBe` FastMode

    it "is case insensitive for pattern matching" do
      suggestAgenticMode WebExplorer "COMPARE errors" `shouldBe` AgenticMode
      suggestAgenticMode WebExplorer "Compare Errors" `shouldBe` AgenticMode
      suggestAgenticMode WebExplorer "INVESTIGATE" `shouldBe` AgenticMode

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
