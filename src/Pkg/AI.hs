module Pkg.AI (callOpenAIAPI, callOpenAIAPIEff, systemPrompt, getNormalTupleReponse, getAskLLMResponse, ChatLLMResponse (..)) where

import Data.Aeson qualified as AE
import Data.Effectful.LLM (callOpenAIAPI)
import Data.Effectful.LLM qualified as ELLM
import Data.List qualified as L
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Models.Telemetry.Schema qualified as Schema
import Relude


data ChatLLMResponse = ChatLLMResponse
  { query :: Text
  , visualization :: Maybe Text
  , timeRange :: Maybe [Text] -- [From, To] in ISO8601 format
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Effectful version that uses the LLM effect (supports caching)
callOpenAIAPIEff :: ELLM.LLM :> es => Text -> Text -> Eff es (Either Text Text)
callOpenAIAPIEff = ELLM.callLLM


getNormalTupleReponse :: Text -> Either Text (Text, Maybe Text)
getNormalTupleReponse response =
  let lines' = lines $ T.strip response
      queryLine = fromMaybe "" (viaNonEmpty head lines')

      -- Check if a visualization type is specified
      vizTypeM =
        if length lines' > 1
          then parseVisualizationType (lines' L.!! 1)
          else Nothing

      -- Clean the query by removing any code block markup and language identifiers
      cleanedQuery =
        T.strip
          $ if "```" `T.isPrefixOf` queryLine
            then
              let withoutFirstLine = maybe "" (unlines . toList) $ viaNonEmpty tail (lines queryLine)
                  withoutBackticks = T.takeWhile (/= '`') withoutFirstLine
               in T.strip withoutBackticks
            else queryLine
   in -- Check if the response indicates an invalid query
      if "Please provide a query"
        `T.isInfixOf` cleanedQuery
        || "I need more"
        `T.isInfixOf` cleanedQuery
        || "Could you please"
        `T.isInfixOf` cleanedQuery
        || T.length cleanedQuery
        < 3
        then Left "INVALID_QUERY_ERROR"
        else Right (cleanedQuery, vizTypeM)


getAskLLMResponse :: Text -> Either Text ChatLLMResponse
getAskLLMResponse response =
  let responseBS = encodeUtf8 response
      decoded = AE.eitherDecode (fromStrict responseBS)
   in case decoded of
        Left err -> Left $ "JSON Decode Error: " <> toText err
        Right apiResponse -> do
          let vType = parseVisualizationType $ fromMaybe "" apiResponse.visualization
              apiResponse' = apiResponse{visualization = vType}
           in Right apiResponse'


-- Parse visualization type from the response
parseVisualizationType :: Text -> Maybe Text
parseVisualizationType v = do
  -- Map to known visualization types
  case v of
    "bar" -> Just "timeseries"
    "line" -> Just "timeseries_line"
    "logs" -> Nothing
    "timeseries" -> Just "timeseries"
    "timeseries_line" -> Just "timeseries_line"
    "bar chart" -> Just "timeseries"
    "line chart" -> Just "timeseries_line"
    "time series" -> Just "timeseries"
    "time series line" -> Just "timeseries_line"
    _ -> Nothing -- Unknown visualization type


systemPrompt :: Text
systemPrompt =
  unlines
    [ "You are a helpful assistant that converts natural language queries to KQL (Kusto Query Language) filter expressions."
    , ""
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , ""
    , "Available Operators:"
    , "- Comparison: == != > < >= <="
    , "- Set operations: in !in (e.g., method in (\"GET\", \"POST\"))"
    , "- Text search: has !has (case-insensitive word search)"
    , "- Text collections: has_any has_all (e.g., tags has_any [\"urgent\", \"critical\"])"
    , "- String operations: contains !contains startswith !startswith endswith !endswith"
    , "- Pattern matching: matches =~ (regex, e.g., email matches /.*@company\\.com/)"
    , "- Logical: AND OR (or lowercase and or)"
    , "- Duration values: 100ms 5s 2m 1h (nanoseconds, microseconds, milliseconds, seconds, minutes, hours)"
    , ""
    , "VISUALIZATION TYPES:"
    , "If the query is best visualized as a chart rather than logs, specify the visualization type on a new line after the query:"
    , "- logs: For displaying log entries (default)"
    , "- timeseries (bar): For bar chart time-based visualization"
    , "- timeseries_line (line): For line chart time-based visualization"
    , ""
    , "When to use different visualization types:"
    , "- Use 'logs' for filtering specific log entries or when detailed log information is needed"
    , "- Use 'timeseries' (bar chart) for queries that count occurrences over time, like error counts, status code distribution"
    , "- Use 'timeseries_line' (line chart) for continuous metrics over time, like response times, latency, throughput"
    , ""
    , "IMPORTANT RULES FOR VISUALIZATION:"
    , "1. If the user mentions 'bar chart', 'bar graph', or wants to visualize counts/distributions, ALWAYS add 'visualization: timeseries' on the second line"
    , "2. If the user mentions 'line chart', 'line graph', or wants to visualize trends/metrics over time, ALWAYS add 'visualization: timeseries_line' on the second line"
    , "3. If the query contains a 'summarize' statement, you MUST specify a visualization type on the second line"
    , "4. For chart visualizations (timeseries or timeseries_line), you MUST include a 'summarize' statement in your query"
    , ""
    , "Chart queries follow standard KQL syntax and typically include:"
    , "1. [filters] | summarize <aggregation> by bin_auto(timestamp), [optional field]"
    , -- , "2. Optional: | sort by <field> [asc|desc]"
      -- , "3. Optional: | take N"
      ""
    , "The summarize statement can use various aggregation functions like count(), sum(...), avg(...), min(...), max(...), median(...), etc."
    , ""
    , "TIME BINNING:"
    , "- Use bin_auto(timestamp) by DEFAULT - the system will automatically determine the appropriate bin size based on the time range"
    , "- Only use bin(timestamp, <size>) when the user EXPLICITLY specifies a time interval (e.g., 'by hour', 'per minute', 'in 5m intervals')"
    , "- Examples of when to use hardcoded bins: 'show errors by hour' -> bin(timestamp, 1h), 'count per 30 seconds' -> bin(timestamp, 30s)"
    , "- Examples of when to use bin_auto: 'show error trend', 'graph response times', 'chart requests over time' -> bin_auto(timestamp)"
    , ""
    , "Examples of chart queries:"
    , "- \"Show errors by hour\": level == \"ERROR\" | summarize count() by bin(timestamp, 1h)"
    , "- \"Graph request counts by kind in 2h blocks\": | summarize count() by bin(timestamp, 2h), kind"
    , "- \"Line chart of p95 durations by method\": | summarize p95(duration) by bin_auto(timestamp), attributes.http.request.method"
    , ""
    , "Examples:"
    , "- \"show me errors\" -> level == \"ERROR\""
    , "- \"POST requests\" -> attributes.http.request.method == \"POST\""
    , "- \"slow requests\" -> duration > 500ms"
    , "- \"500 errors\" -> attributes.http.response.status_code == \"500\""
    , "- \"GET or POST requests\" -> method in (\"GET\", \"POST\")"
    , "- \"messages containing error\" -> message contains \"error\""
    , "- \"logs with urgent or critical tags\" -> tags has_any [\"urgent\", \"critical\"]"
    , "- \"paths starting with /api\" -> path startswith \"/api\""
    , "- \"emails from company.com\" -> email matches /.*@company\\.com/"
    , "- \"requests taking more than 1 second\" -> duration > 1s"
    , "- \"Show me error count over time\" -> level == \"ERROR\" | summarize count() by bin_auto(timestamp)\nvisualization: timeseries"
    , "- \"Graph of response times\" -> | summarize avg(duration) by bin_auto(timestamp)\nvisualization: timeseries_line"
    , "- \"make a bar chart of requests by kind\" -> | summarize count() by bin_auto(timestamp), kind\nvisualization: timeseries"
    , "- \"line chart of requests over time\" -> | summarize count() by bin_auto(timestamp)\nvisualization: timeseries_line"
    , ""
    , "Time range:"
    , "If the query includes a time range, specify it using the 'timeRange' field in the JSON response:"
    , "{"
    , "  \"timeRange\": [ \"<From: ISO8601>\",  \"<To: ISO8601>\"]"
    , "}"
    , ""
    , "Time range rules:"
    , "DO NOT include timeRange if not requsted by the user"
    , "DO NOT make up a time range if not specified by the user"
    , "DO NOT include timeRange field if the array is empty or incomplete"
    , "DO NOT use code blocks or backticks in your response. Return the raw query directly."
    , "Response format:"
    , "Always return JSON in the following structure:"
    , "{"
    , "  \"query\": \"<KQL filter/query string>\","
    , "  \"visualization\": \"<logs|timeseries|timeseries_line>\","
    , "  \"timeRange\": [\"<From: ISO8601>\", \"<To: ISO8601>\"]"
    , "}"
    , ""
    , "IMPORTANT: Do not include timeRange if not requested by the user"
    , "IMPORTANT: If timerange is empty or incomplete, do not include the timeRange field"
    , "IMPORTANT: Do not use code blocks or backticks in your response. Return the raw query directly."
    ]
