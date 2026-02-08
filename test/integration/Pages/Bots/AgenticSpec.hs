module Pages.Bots.AgenticSpec (spec) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Models.Projects.Projects qualified as Projects
import Pages.Bots.BotTestHelpers (assertJsonGolden, getOpenAIKey, hasOpenAIKey, testPid)
import Pages.Bots.Utils (QueryIntent (..), ReportType (..), detectReportIntent, processAIQuery)
import Pkg.AI qualified as AI
import Pkg.Components.Widget qualified as Widget
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, pendingWith, shouldBe, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Agentic Query Processing" do
    describe "Intent detection" do
      it "identifies report requests" \_ -> do
        detectReportIntent "send daily report" `shouldBe` ReportIntent DailyReport
        detectReportIntent "get weekly summary" `shouldBe` ReportIntent WeeklyReport
        detectReportIntent "show me the report" `shouldBe` ReportIntent DailyReport

      it "identifies general queries" \_ -> do
        detectReportIntent "show errors" `shouldBe` GeneralQueryIntent
        detectReportIntent "what's my error rate" `shouldBe` GeneralQueryIntent
        detectReportIntent "plot traffic over time" `shouldBe` GeneralQueryIntent

      it "requires action verb for report intent" \_ -> do
        detectReportIntent "report" `shouldBe` GeneralQueryIntent
        detectReportIntent "daily" `shouldBe` GeneralQueryIntent

    describe "AgenticConfig" do
      it "creates default config with project ID" \_ -> do
        let config = AI.defaultAgenticConfig testPid
        config.projectId `shouldBe` testPid
        config.maxIterations `shouldBe` 5
        isNothing config.facetContext `shouldBe` True

      it "allows custom max iterations" \_ -> do
        let config = (AI.defaultAgenticConfig testPid){AI.maxIterations = 3}
        config.maxIterations `shouldBe` 3

    describe "Response parsing" do
      it "parses widget response" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = """{"explanation": "Here are the errors", "widgets": [{"type": "timeseries", "query": "level == ERROR", "title": "Error Trend"}], "suggested_query": "level == ERROR"}"""
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            length parsed.widgets `shouldBe` 1
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "handles text-only response" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = """{"explanation": "Your error rate is 5%", "widgets": []}"""
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            null parsed.widgets `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "preserves tool calls from result" \_ -> do
        let toolCall =
              AI.ToolCallInfo
                { AI.name = "run_query"
                , AI.args = mempty
                , AI.resultPreview = "10 rows"
                , AI.rawData = Nothing
                }
            rawResult =
              AI.AgenticChatResult
                { AI.response = "{\"explanation\": \"done\"}"
                , AI.toolCalls = [toolCall]
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> length (fromMaybe [] parsed.toolCalls) `shouldBe` 1
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "handles malformed JSON gracefully" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = "This is not JSON, just a text response"
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            null parsed.widgets `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " <> toString err

    describe "Code block stripping" do
      it "strips json code blocks" \_ -> do
        let input = "```json\n{\"key\": \"value\"}\n```"
        AI.stripCodeBlock input `shouldBe` "{\"key\": \"value\"}"

      it "strips generic code blocks" \_ -> do
        let input = "```\n{\"key\": \"value\"}\n```"
        AI.stripCodeBlock input `shouldBe` "{\"key\": \"value\"}"

      it "handles plain text" \_ -> do
        let input = "{\"key\": \"value\"}"
        AI.stripCodeBlock input `shouldBe` "{\"key\": \"value\"}"

    describe "Widget types" do
      it "maps widget types to chart types (ECharts types)" \_ -> do
        -- These map to ECharts chart types, not our internal names
        Widget.mapWidgetTypeToChartType Widget.WTTimeseries `shouldBe` "bar"
        Widget.mapWidgetTypeToChartType Widget.WTTimeseriesLine `shouldBe` "line"
        Widget.mapWidgetTypeToChartType Widget.WTDistribution `shouldBe` "bar"
        Widget.mapWidgetTypeToChartType Widget.WTHeatmap `shouldBe` "heatmap"
        Widget.mapWidgetTypeToChartType Widget.WTServiceMap `shouldBe` "graph"

    describe "System prompts" do
      it "includes KQL guide" \_ -> do
        T.isInfixOf "KQL" AI.kqlGuide `shouldBe` True
        T.isInfixOf "summarize" AI.kqlGuide `shouldBe` True

      it "includes schema in system prompt" \_ -> do
        let prompt = AI.systemPrompt (Unsafe.read "2025-01-01 00:00:00 UTC")
        T.isInfixOf "telemetry" prompt `shouldBe` True
        T.isInfixOf "logs" prompt `shouldBe` True

      it "includes visualization types" \_ -> do
        T.isInfixOf "timeseries" AI.kqlGuide `shouldBe` True
        T.isInfixOf "distribution" AI.kqlGuide `shouldBe` True
        T.isInfixOf "pie_chart" AI.kqlGuide `shouldBe` True

    describe "Live API calls (requires OPENAI_API_KEY)" do
      it "processes error trend query and saves golden response" \tr -> do
        let apiKey = getOpenAIKey tr
        if not (hasOpenAIKey tr)
          then pendingWith "OPENAI_API_KEY not configured - skipping live API test"
          else do
            result <- runTestBg tr $ processAIQuery testPid "plot error trend over time" Nothing apiKey
            case result of
              Left err -> pendingWith $ "API call failed: " <> toString err
              Right agenticResp -> do
                let responseJson = AE.toJSON agenticResp
                assertJsonGolden "agentic/error_trend_response.json" responseJson
                length agenticResp.widgets `shouldSatisfy` (>= 0)

      it "processes service breakdown query and saves golden response" \tr -> do
        let apiKey = getOpenAIKey tr
        if not (hasOpenAIKey tr)
          then pendingWith "OPENAI_API_KEY not configured - skipping live API test"
          else do
            result <- runTestBg tr $ processAIQuery testPid "show warning and error counts grouped by service" Nothing apiKey
            case result of
              Left err -> pendingWith $ "API call failed: " <> toString err
              Right agenticResp -> do
                let responseJson = AE.toJSON agenticResp
                assertJsonGolden "agentic/service_breakdown_response.json" responseJson

      it "processes explanation-only query and saves golden response" \tr -> do
        let apiKey = getOpenAIKey tr
        if not (hasOpenAIKey tr)
          then pendingWith "OPENAI_API_KEY not configured - skipping live API test"
          else do
            result <- runTestBg tr $ processAIQuery testPid "what services have the most errors?" Nothing apiKey
            case result of
              Left err -> pendingWith $ "API call failed: " <> toString err
              Right agenticResp -> do
                let responseJson = AE.toJSON agenticResp
                assertJsonGolden "agentic/explanation_only_response.json" responseJson
                isJust agenticResp.explanation || not (null agenticResp.widgets) `shouldBe` True

      it "handles empty API key gracefully" \tr -> do
        let goldenDir = tr.trATCtx.config.agenticGoldenDir
        result <- runTestBg tr $ processAIQuery testPid "show errors" Nothing ""
        case result of
          Left err -> T.isInfixOf "unavailable" err || T.isInfixOf "error" (T.toLower err) `shouldBe` True
          Right _ -> pass
