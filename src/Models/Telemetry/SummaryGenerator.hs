{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Telemetry.SummaryGenerator (
  generateSummary,
) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Models.Telemetry.Telemetry (OtelLogsAndSpans (..), Severity (..), SeverityLevel (..), atMapInt, atMapText)
import Relude
import Utils (getDurationNSMS)


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
          Just attrs
            | not (Map.null attrs) ->
                let attrText = TE.decodeUtf8 $ BSL.toStrict $ AE.encode attrs
                    -- Limit attributes to 500 characters
                    truncated =
                      if T.length attrText > 500
                        then T.take 497 attrText <> "..."
                        else attrText
                 in Just $ "attributes;text-textWeak⇒" <> truncated
          _ -> Nothing
      ]
  where
    severityStyle sev = case sev of
      SLDebug -> "badge-neutral"
      SLInfo -> "badge-info" -- Blue background for INFO
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
    -- Check if this span has HTTP attributes
    hasHttp = case otel.attributes of
      Just attrs ->
        isJust (atMapText "http.request.method" (Just attrs))
          || isJust (atMapInt "http.response.status_code" (Just attrs))
      _ -> False

    -- Build elements in correct order
    elements =
      catMaybes
        $
        -- 1. Request type indicators (icons)
        [ case (otel.kind, hasHttp, atMapText "component" otel.attributes) of
            -- HTTP server/client spans (check kind first)
            (Just "server", True, _) -> Just "request_type;neutral⇒incoming"
            (Just "client", True, _) -> Just "request_type;neutral⇒outgoing"
            -- If no kind but has component "proxy" and HTTP, it's likely incoming
            (_, True, Just comp) | "proxy" `T.isInfixOf` comp -> Just "request_type;neutral⇒incoming"
            -- If has HTTP attributes but no specific kind, default to outgoing for frontend
            (_, True, Just "frontend") -> Just "request_type;neutral⇒outgoing"
            -- Generic HTTP spans without clear direction
            (_, True, _) -> Just "request_type;neutral⇒outgoing"
            -- RPC server/client spans
            (Just "server", _, _) | isJust (atMapText "rpc.method" otel.attributes) -> Just "request_type;neutral⇒incoming"
            (Just "client", _, _) | isJust (atMapText "rpc.method" otel.attributes) -> Just "request_type;neutral⇒outgoing"
            -- Database spans
            (_, _, _) | isJust (atMapText "db.system" otel.attributes) -> Just "kind;neutral⇒database"
            -- Internal spans
            (Just "internal", _, _) -> Just "kind;neutral⇒internal"
            _ -> Nothing
        ]
          ++
          -- 2. HTTP Status code (comes before method)
          [ case atMapInt "http.response.status_code" otel.attributes of
              Just code -> Just $ "status_code;" <> statusCodeStyle code <> "⇒" <> T.pack (show code)
              _ -> Nothing
          ]
          ++
          -- 3. HTTP Method
          [ case atMapText "http.request.method" otel.attributes of
              Just method -> Just $ "method;" <> methodStyle method <> "⇒" <> method
              _ -> Nothing
          ]
          ++
          -- 4. URL or Route
          [ case (atMapText "http.route" otel.attributes, atMapText "url.path" otel.attributes) of
              (Just route, _) -> Just $ "route;neutral⇒" <> route
              (_, Just url) -> Just $ "url;neutral⇒" <> url
              _ -> Nothing
          ]
          ++
          -- 5. Database attributes
          [ -- Database type
            case atMapText "db.system" otel.attributes of
              Just system -> Just $ "db.system;neutral⇒" <> system
              _ -> Nothing
          , -- Query text (if db.query.text exists for db types)
            case (atMapText "db.system" otel.attributes, atMapText "db.query.text" otel.attributes) of
              (Just _, Just queryText) -> Just $ "db.query.text;text-textStrong⇒" <> T.take 200 queryText
              _ -> Nothing
          , -- Query statement
            case atMapText "db.statement" otel.attributes of
              Just stmt -> Just $ "db.statement;neutral⇒" <> T.take 200 stmt
              _ -> Nothing
          ]
          ++
          -- 6. RPC attributes
          [ -- RPC method
            case atMapText "rpc.method" otel.attributes of
              Just method -> Just $ "rpc.method;neutral⇒" <> method
              _ -> Nothing
          , -- RPC service
            case atMapText "rpc.service" otel.attributes of
              Just service -> Just $ "rpc.service;neutral⇒" <> service
              _ -> Nothing
          ]
          ++
          -- 7. Span name (if not HTTP with URL/route)
          [ case otel.name of
              Just n ->
                -- Only show span name if no URL/route was shown
                case (atMapText "http.route" otel.attributes, atMapText "url.path" otel.attributes) of
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
              Just attrs
                | not (Map.null attrs) ->
                    let attrText = TE.decodeUtf8 $ BSL.toStrict $ AE.encode attrs
                        -- Limit attributes to 500 characters
                        truncated =
                          if T.length attrText > 500
                            then T.take 497 attrText <> "..."
                            else attrText
                     in Just $ "attributes;text-textWeak⇒" <> truncated
              _ -> Nothing
          ]
          ++
          -- 10. Right-aligned badges for latency breakdown
          -- These use "right" style and will be extracted by frontend
          [ -- has a session id
            case atMapText "session.id" otel.attributes of
              Just v -> Just $ "session;right-badge-neutral⇒" <> v
              _ -> Nothing
          , case atMapText "user.email" otel.attributes of
              Just eml -> Just $ "user email;right-badge-neutral⇒" <> eml
              _ -> case atMapText "user.id" otel.attributes of
                Just s -> Just $ "user name;right-badge-neutral⇒" <> s
                _ -> Nothing
          , case atMapText "user.full_name" otel.attributes of
              Just s -> Just $ "user name;right-badge-neutral⇒" <> s
              _ -> case atMapText "user.name" otel.attributes of
                Just s -> Just $ "user name;right-badge-neutral⇒" <> s
                _ -> Nothing
          , -- Error status (if ERROR)
            case otel.status_code of
              Just "ERROR" -> Just "status;right-badge-error⇒ERROR"
              _ -> Nothing
          , -- Database system with brand colors
            case atMapText "db.system" otel.attributes of
              Just "postgresql" -> Just "db.system;right-badge-postgres⇒postgres"
              Just "mysql" -> Just "db.system;right-badge-mysql⇒mysql"
              Just "redis" -> Just "db.system;right-badge-redis⇒redis"
              Just "mongodb" -> Just "db.system;right-badge-mongo⇒mongodb"
              Just "elasticsearch" -> Just "db.system;right-badge-elastic⇒elastic"
              Just system -> Just $ "db.system;right-badge-neutral⇒" <> system
              _ -> Nothing
          , -- HTTP indicator (if has HTTP attributes)
            if hasHttp then Just "protocol;right-badge-neutral⇒http" else Nothing
          , -- RPC indicator
            case atMapText "rpc.method" otel.attributes of
              Just _ -> Just "protocol;right-badge-neutral⇒rpc"
              _ -> Nothing
          , -- Duration (always show)
            case otel.duration of
              Just dur -> Just $ "duration;right-badge-neutral⇒" <> toText (getDurationNSMS (fromIntegral dur))
              _ ->
                Nothing
          ]
   in
    V.fromList elements


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
