{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.Telemetry.SummaryGenerator
  ( generateSummary
  ) where

import qualified Data.Aeson as AE
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, catMaybes)
import Models.Telemetry.Telemetry

-- | Generate summary array for an OtelLogsAndSpans record
-- Format: "field;style⇒value" where:
--   - field is optional (can be empty for plain text)
--   - style can be: info-strong, info-weak, error-strong, error-weak, 
--                   warning-strong, warning-weak, success-strong, success-weak, neutral
--   - special styles: "right" for right-aligned items
--   - plain text has no semicolon separator
generateSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateSummary otel@OtelLogsAndSpans{..} =
  case kind of
    Just "log" -> generateLogSummary otel
    _ -> generateSpanSummary otel

-- | Generate summary for log entries
generateLogSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateLogSummary OtelLogsAndSpans{..} = V.fromList $ catMaybes
  [ -- Severity
    case severity of
      Just Severity{severity_text = Just sev} ->
        Just $ "severity_text;" <> severityStyle sev <> "⇒" <> T.toUpper (T.pack $ show sev)
      _ -> Nothing
  , -- Body
    case body of
      Just (AE.String txt) -> Just txt
      Just val -> Just $ T.take 200 $ T.pack $ show val
      Nothing -> Nothing
  ]
  where
    severityStyle sev = case sev of
      DEBUG -> "neutral"
      INFO -> "info-weak"
      WARN -> "warning-strong"
      ERROR -> "error-strong"
      FATAL -> "error-strong"

-- | Generate summary for span entries
generateSpanSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateSpanSummary OtelLogsAndSpans{..} = V.fromList $ catMaybes $
  [ -- Kind indicator (for internal spans)
    case kind of
      Just "internal" -> Just "kind;neutral⇒internal"
      _ -> Nothing
  ] ++ 
  -- HTTP attributes
  (case (kind, attributes) of
    (Just k, Just attrs) | k `elem` ["server", "client"] ->
      case Map.lookup "http" attrs of
        Just (AE.Object httpObj) -> catMaybes
          [ -- Request type indicator
            case k of
              "server" -> Just "request_type;neutral⇒incoming"
              "client" -> Just "request_type;neutral⇒outgoing"
              _ -> Nothing
          , -- Status code
            case AE.parseMaybe (AE..: "status_code") httpObj of
              Just (code :: Int) -> Just $ "status_code;" <> statusCodeStyle code <> "⇒" <> T.pack (show code)
              _ -> Nothing
          , -- Method
            case AE.parseMaybe (AE..: "method") httpObj of
              Just (method :: T.Text) -> Just $ "method;" <> methodStyle method <> "⇒" <> method
              _ -> Nothing
          , -- URL
            case AE.parseMaybe (AE..: "url") httpObj of
              Just (url :: T.Text) -> Just $ "url;neutral⇒" <> url
              _ -> Nothing
          ]
        _ -> []
    _ -> []
  ) ++
  -- Database attributes
  (case attributes of
    Just attrs ->
      case Map.lookup "db" attrs of
        Just (AE.Object dbObj) -> catMaybes
          [ -- Database type
            case AE.parseMaybe (AE..: "system") dbObj of
              Just (system :: T.Text) -> Just $ "db.system;neutral⇒" <> system
              _ -> Nothing
          , -- Query
            case AE.parseMaybe (AE..: "statement") dbObj of
              Just (stmt :: T.Text) -> Just $ "db.statement;neutral⇒" <> T.take 200 stmt
              _ -> Nothing
          ]
        _ -> []
    _ -> []
  ) ++
  -- RPC attributes
  (case (kind, attributes) of
    (Just k, Just attrs) | k `elem` ["server", "client"] ->
      case Map.lookup "rpc" attrs of
        Just (AE.Object rpcObj) -> catMaybes
          [ -- Request type indicator
            case k of
              "server" -> Just "request_type;neutral⇒incoming"
              "client" -> Just "request_type;neutral⇒outgoing"
              _ -> Nothing
          , -- RPC method
            case AE.parseMaybe (AE..: "method") rpcObj of
              Just (method :: T.Text) -> Just $ "rpc.method;neutral⇒" <> method
              _ -> Nothing
          ]
        _ -> []
    _ -> []
  ) ++
  [ -- Status
    case status_code of
      Just "ERROR" -> Just "status;error-strong⇒ERROR"
      Just "OK" -> Nothing -- Don't show OK status
      _ -> Nothing
  , -- Span name
    case name of
      Just n -> Just $ "span_name;neutral⇒" <> n
      _ -> Nothing
  ]

statusCodeStyle :: Int -> T.Text
statusCodeStyle code
  | code < 200 = "neutral"
  | code < 300 = "success-weak"
  | code < 400 = "warning-weak"
  | code >= 400 = "error-weak"
  | otherwise = "neutral"

methodStyle :: T.Text -> T.Text
methodStyle method = case method of
  "GET" -> "info-weak"
  "POST" -> "info-strong"
  "PUT" -> "warning-weak"
  "DELETE" -> "error-weak"
  "PATCH" -> "warning-strong"
  _ -> "neutral"