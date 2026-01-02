module Pkg.AI.AgenticSpec (spec) where

import Data.Text qualified as T
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

  describe "SQL validation helpers" do
    it "dangerous keywords at start of query are detected" do
      containsDangerousSQL "DROP TABLE users" `shouldBe` True
      containsDangerousSQL "DELETE FROM logs" `shouldBe` True
      containsDangerousSQL "UPDATE users SET" `shouldBe` True

    it "dangerous keywords after whitespace are detected" do
      containsDangerousSQL "SELECT * FROM users; DROP TABLE" `shouldBe` True
      containsDangerousSQL "SELECT 1\nDELETE FROM users" `shouldBe` True
      containsDangerousSQL "SELECT 1\tUPDATE users" `shouldBe` True

    it "bypass patterns are detected" do
      containsBypassPattern "SELECT * UNION SELECT * FROM other" `shouldBe` True
      containsBypassPattern "SELECT * EXCEPT SELECT" `shouldBe` True
      containsBypassPattern "SELECT * INTERSECT SELECT" `shouldBe` True

    it "safe queries pass validation" do
      containsDangerousSQL "SELECT * FROM users WHERE id = 1" `shouldBe` False
      containsDangerousSQL "SELECT count(*) FROM logs" `shouldBe` False
      containsBypassPattern "SELECT * FROM users" `shouldBe` False


-- SQL validation helpers for testing (these could be exported from Agentic.hs if needed)
containsDangerousSQL :: Text -> Bool
containsDangerousSQL query =
  let lowerQuery = T.toLower query
      hasKeyword kw = kw `T.isPrefixOf` lowerQuery || any (`T.isInfixOf` lowerQuery) [" " <> kw, "\n" <> kw, "\t" <> kw]
   in any hasKeyword ["drop", "delete", "truncate", "update", "insert", "alter", "create"]


containsBypassPattern :: Text -> Bool
containsBypassPattern query =
  let lowerQuery = T.toLower query
      hasKeyword kw = kw `T.isPrefixOf` lowerQuery || any (`T.isInfixOf` lowerQuery) [" " <> kw, "\n" <> kw, "\t" <> kw]
   in any hasKeyword ["union", "except", "intersect"]
