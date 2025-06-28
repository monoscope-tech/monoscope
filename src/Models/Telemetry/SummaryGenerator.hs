{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Models.Telemetry.SummaryGenerator (
  generateSummary,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AE
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Models.Telemetry.Telemetry
import Relude


-- | Generate summary array for an OtelLogsAndSpans record
-- Format: "field;style⇒value" where:
--   - field is optional (can be empty for plain text)
--   - style can be: info-strong, info-weak, error-strong, error-weak,
--                   warning-strong, warning-weak, success-strong, success-weak, neutral
--   - special styles: "right" for right-aligned items
--   - plain text has no semicolon separator
generateSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateSummary otel =
  case otel.kind of
    Just "log" -> generateLogSummary otel
    _ -> generateSpanSummary otel


-- | Generate summary for log entries
generateLogSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateLogSummary otel =
  V.fromList
    $ catMaybes
      [ -- Severity
        case otel.severity of
          Just Severity{severity_text = Just sev} ->
            Just $ "severity_text;" <> severityStyle sev <> "⇒" <> severityText sev
          _ -> Nothing
      , -- Body
        case otel.body of
          Just (AE.String txt) -> Just txt
          Just val -> Just $ T.take 200 $ T.pack $ show val
          Nothing -> Nothing
      , -- Attributes (limited to avoid excessive length)
        case otel.attributes of
          Just attrs | not (Map.null attrs) -> 
            let attrText = TE.decodeUtf8 $ BSL.toStrict $ AE.encode attrs
                -- Limit attributes to 500 characters
                truncated = if T.length attrText > 500 
                           then T.take 497 attrText <> "..."
                           else attrText
            in Just $ "attributes;text-weak⇒" <> truncated
          _ -> Nothing
      ]
  where
    severityStyle sev = case sev of
      SLDebug -> "badge-neutral"
      SLInfo -> "badge-info"  -- Blue background for INFO
      SLWarn -> "badge-warning"
      SLError -> "badge-error"
      SLFatal -> "badge-fatal"
    
    severityText sev = case sev of
      SLDebug -> "DEBUG"
      SLInfo -> "INFO"
      SLWarn -> "WARN"
      SLError -> "ERROR"
      SLFatal -> "FATAL"


-- | Generate summary for span entries
generateSpanSummary :: OtelLogsAndSpans -> V.Vector T.Text
generateSpanSummary otel =
  let
    -- Extract HTTP attributes once to avoid repeated lookups
    httpAttrs = case (otel.kind, otel.attributes) of
      (Just k, Just attrs) | k `elem` ["server", "client"] ->
        case Map.lookup "http" attrs of
          Just (AE.Object httpObj) -> Just httpObj
          _ -> Nothing
      _ -> Nothing
    
    -- Build elements in correct order
    elements = catMaybes $
      -- 1. Request type indicators (icons)
      [ case (otel.kind, otel.attributes) of
          -- HTTP server/client spans
          (Just "server", Just attrs) | Map.member "http" attrs -> Just "request_type;neutral⇒incoming"
          (Just "client", Just attrs) | Map.member "http" attrs -> Just "request_type;neutral⇒outgoing"
          -- RPC server/client spans
          (Just "server", Just attrs) | Map.member "rpc" attrs -> Just "request_type;neutral⇒incoming"
          (Just "client", Just attrs) | Map.member "rpc" attrs -> Just "request_type;neutral⇒outgoing"
          -- Database spans
          (_, Just attrs) | Map.member "db" attrs -> Just "kind;neutral⇒database"
          -- Internal spans
          (Just "internal", _) -> Just "kind;neutral⇒internal"
          _ -> Nothing
      ]
      ++
      -- 2. HTTP Status code (comes before method)
      [ case httpAttrs of
          Just httpObj ->
            case AE.parseMaybe (AE..: "status_code") httpObj of
              Just (code :: Int) -> Just $ "status_code;" <> statusCodeStyle code <> "⇒" <> T.pack (show code)
              _ -> Nothing
          _ -> Nothing
      ]
      ++
      -- 3. HTTP Method
      [ case httpAttrs of
          Just httpObj ->
            case AE.parseMaybe (AE..: "method") httpObj of
              Just (method :: T.Text) -> Just $ "method;" <> methodStyle method <> "⇒" <> method
              _ -> Nothing
          _ -> Nothing
      ]
      ++
      -- 4. URL or Route
      [ case httpAttrs of
          Just httpObj ->
            case (AE.parseMaybe (AE..: "route") httpObj, AE.parseMaybe (AE..: "url") httpObj) of
              (Just (route :: T.Text), _) -> Just $ "route;neutral⇒" <> route
              (_, Just (url :: T.Text)) -> Just $ "url;neutral⇒" <> url
              _ -> Nothing
          _ -> Nothing
      ]
      ++
      -- 5. Database attributes
      ( case otel.attributes of
          Just attrs ->
            case Map.lookup "db" attrs of
              Just (AE.Object dbObj) ->
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
      )
      ++
      -- 6. RPC attributes
      ( case (otel.kind, otel.attributes) of
          (Just k, Just attrs) | k `elem` ["server", "client"] ->
            case Map.lookup "rpc" attrs of
              Just (AE.Object rpcObj) ->
                [ -- RPC method
                  case AE.parseMaybe (AE..: "method") rpcObj of
                    Just (method :: T.Text) -> Just $ "rpc.method;neutral⇒" <> method
                    _ -> Nothing
                , -- RPC service
                  case AE.parseMaybe (AE..: "service") rpcObj of
                    Just (service :: T.Text) -> Just $ "rpc.service;neutral⇒" <> service
                    _ -> Nothing
                ]
              _ -> []
          _ -> []
      )
      ++ 
      -- 7. Span name (if not HTTP with URL/route)
      [ case (otel.name, httpAttrs) of
          (Just n, Nothing) -> Just $ "span_name;neutral⇒" <> n
          (Just n, Just httpObj) ->
            -- Only show span name if no URL/route was shown
            case (AE.parseMaybe (AE..: "route") httpObj :: Maybe T.Text, 
                  AE.parseMaybe (AE..: "url") httpObj :: Maybe T.Text) of
              (Nothing, Nothing) -> Just $ "span_name;neutral⇒" <> n
              _ -> Nothing
          _ -> Nothing
      ]
      ++
      -- 8. Status (ERROR only)
      [ case otel.status_code of
          Just "ERROR" -> Just "status;badge-error⇒ERROR"
          Just "OK" -> Nothing -- Don't show OK status
          _ -> Nothing
      ]
      ++
      -- 9. Attributes (limited to avoid excessive length)
      [ case otel.attributes of
          Just attrs | not (Map.null attrs) -> 
            let attrText = TE.decodeUtf8 $ BSL.toStrict $ AE.encode attrs
                -- Limit attributes to 500 characters
                truncated = if T.length attrText > 500 
                           then T.take 497 attrText <> "..."
                           else attrText
            in Just $ "attributes;text-weak⇒" <> truncated
          _ -> Nothing
      ]
  in V.fromList elements

-- | Format duration from nanoseconds to human readable format
formatDuration :: Int64 -> T.Text
formatDuration ns
  | ns >= 1000000000 = T.pack (show (ns `div` 1000000000)) <> "s"
  | ns >= 1000000 = T.pack (show (ns `div` 1000000)) <> "ms"
  | ns >= 1000 = T.pack (show (ns `div` 1000)) <> "μs"
  | otherwise = T.pack (show ns) <> "ns"


statusCodeStyle :: Int -> T.Text
statusCodeStyle code
  | code < 200 = "badge-neutral"
  | code < 300 = "badge-2xx"
  | code < 400 = "badge-3xx"
  | code < 500 = "badge-4xx"
  | otherwise = "badge-5xx"


methodStyle :: T.Text -> T.Text
methodStyle method = case method of
  "GET" -> "badge-GET"
  "POST" -> "badge-POST"
  "PUT" -> "badge-PUT"
  "DELETE" -> "badge-DELETE"
  "PATCH" -> "badge-PATCH"
  _ -> "badge-neutral"
