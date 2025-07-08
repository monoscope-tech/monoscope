module Pkg.AI (callOpenAIAPI, systemPrompt) where

import Data.List qualified as L
import Data.Text qualified as T
import Langchain.LLM.Core qualified as LLM
import Langchain.LLM.OpenAI
import Models.Telemetry.Schema qualified as Schema
import Relude


callOpenAIAPI :: Text -> Text -> IO (Either Text (Text, Maybe Text))
callOpenAIAPI fullPrompt apiKey = do
  let openAI =
        OpenAI
          { apiKey = apiKey
          , openAIModelName = "gpt-4o-mini"
          , callbacks = []
          , baseUrl = Nothing
          }
  -- Use langchain-hs to generate response
  result <- liftIO $ LLM.generate openAI fullPrompt Nothing
  case result of
    Left err -> pure $ Left $ "LLM Error: " <> toText err
    Right response -> do
      -- Parse the response for query and visualization type
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
            then pure $ Left "INVALID_QUERY_ERROR"
            else pure $ Right (cleanedQuery, vizTypeM)


-- Parse visualization type from the response
parseVisualizationType :: Text -> Maybe Text
parseVisualizationType line = do
  let lowerLine = T.toLower $ T.strip line
      prefixes = ["visualization:", "visualization type:", "viz:", "viz type:", "chart:", "chart type:", "type:"]
      -- Try to match any of the prefixes
      matchedPrefix = find (`T.isPrefixOf` lowerLine) prefixes

  matchedPrefix >>= \prefix -> do
    -- Extract the visualization type after the prefix
    let rawType = T.strip $ T.drop (T.length prefix) lowerLine

    -- Map to known visualization types
    case rawType of
      "bar" -> Just "timeseries"
      "line" -> Just "timeseries_line"
      "logs" -> Just "logs"
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
    , "1. [filters] | summarize <aggregation> by bin(timestamp, <time>), [optional field]"
    , -- , "2. Optional: | sort by <field> [asc|desc]"
      -- , "3. Optional: | take N"
      ""
    , "The summarize statement can use various aggregation functions like count(), sum(...), avg(...), min(...), max(...), median(...), etc."
    , ""
    , "Examples of chart queries:"
    , "- \"Show errors by hour\": level == \"ERROR\" | summarize count() by bin(timestamp, 1h)"
    , "- \"Graph request counts by kind in 2h blocks\": | summarize count() by bin(timestamp, 2h), kind"
    , "- \"Line chart of p95 durations by method\": | summarize p95(duration) by bin(timestamp, 30m), attributes.http.request.method"
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
    , "- \"Show me error count over time\" -> level == \"ERROR\" | summarize count() by bin(timestamp, 1h)\nvisualization: timeseries"
    , "- \"Graph of response times\" -> | summarize avg(duration) by bin(timestamp, 5m)\nvisualization: timeseries_line"
    , "- \"make a bar chart of requests by kind\" -> | summarize count() by bin(timestamp, 1h), kind\nvisualization: timeseries"
    , "- \"line chart of requests over time\" -> | summarize count() by bin(timestamp, 1h)\nvisualization: timeseries_line"
    , ""
    , "Return ONLY the KQL filter expression on the first line without any backticks, markdown formatting, or code blocks. If applicable, specify the visualization type on the second line with format 'visualization: [type]'"
    , ""
    , "IMPORTANT: Do not use code blocks or backticks in your response. Return the raw query directly."
    ]
