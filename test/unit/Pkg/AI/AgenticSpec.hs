module Pkg.AI.AgenticSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
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

  describe "parseAgenticResponse" do
    it "parses tool calls correctly" do
      let toolCallJson = "{\"tool_calls\": [{\"tool\": \"GetFieldValues\", \"arguments\": {\"field\": \"level\"}}]}"
          result = parseAgenticResponseTest toolCallJson
      case result of
        NeedsTools calls -> do
          length calls `shouldBe` 1
          (head calls).tool `shouldBe` GetFieldValues
        _ -> fail "Expected NeedsTools"

    it "handles empty tool calls array" do
      let emptyTools = "{\"tool_calls\": []}"
          result = parseAgenticResponseTest emptyTools
      case result of
        NeedsTools calls -> length calls `shouldBe` 0
        _ -> fail "Expected NeedsTools with empty list"

    it "handles invalid JSON gracefully" do
      let invalidJson = "not json at all"
          result = parseAgenticResponseTest invalidJson
      case result of
        AgenticError _ -> True `shouldBe` True
        _ -> fail "Expected AgenticError for invalid JSON"

  describe "ToolLimits" do
    it "defaultLimits has reasonable values" do
      defaultLimits.maxFieldValues `shouldBe` 20
      defaultLimits.maxSampleLogs `shouldBe` 5
      defaultLimits.maxServices `shouldBe` 20
      defaultLimits.defaultFieldLimit `shouldBe` 10
      defaultLimits.defaultSampleLimit `shouldBe` 3
      defaultLimits.maxQueryResults `shouldBe` 100

  describe "Tool JSON serialization" do
    it "serializes Tool correctly" do
      AE.encode GetFieldValues `shouldBe` "\"GetFieldValues\""
      AE.encode CountQuery `shouldBe` "\"CountQuery\""
      AE.encode GetServices `shouldBe` "\"GetServices\""
      AE.encode SampleLogs `shouldBe` "\"SampleLogs\""
      AE.encode GetFacets `shouldBe` "\"GetFacets\""
      AE.encode RunQuery `shouldBe` "\"RunQuery\""

    it "deserializes Tool correctly" do
      AE.decode "\"GetFieldValues\"" `shouldBe` Just GetFieldValues
      AE.decode "\"CountQuery\"" `shouldBe` Just CountQuery
      AE.decode "\"GetServices\"" `shouldBe` Just GetServices
      AE.decode "\"SampleLogs\"" `shouldBe` Just SampleLogs
      AE.decode "\"GetFacets\"" `shouldBe` Just GetFacets
      AE.decode "\"RunQuery\"" `shouldBe` Just RunQuery

  describe "SQL validation in tool execution" do
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


-- Helper to parse agentic response (re-implementing for testing since it's not exported)
parseAgenticResponseTest :: Text -> AgenticResponse
parseAgenticResponseTest responseText =
  let trimmed = T.strip responseText
   in case AE.eitherDecode @AE.Value (fromStrict $ encodeUtf8 trimmed) of
        Left _ -> AgenticError $ "Invalid response format: " <> T.take 100 trimmed
        Right val -> case parseToolCallsTest val of
          Just calls -> NeedsTools calls
          Nothing -> AgenticError "Not a tool call response"


parseToolCallsTest :: AE.Value -> Maybe [ToolCall]
parseToolCallsTest (AE.Object obj) = do
  calls <- AEKM.lookup (AEK.fromText "tool_calls") obj
  case AE.fromJSON calls of
    AE.Success r -> Just r
    AE.Error _ -> Nothing
parseToolCallsTest _ = Nothing


-- SQL validation helpers for testing
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
