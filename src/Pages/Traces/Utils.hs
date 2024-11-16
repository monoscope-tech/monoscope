module Pages.Traces.Utils (getServiceName, getServiceColor, getRequestDetails, spanHasErrors, getSpanErrors, getErrorDetails)
where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as KEM
import Data.HashMap.Strict hiding (null)
import Data.HashMap.Strict qualified as HM
import Data.Scientific (toBoundedInteger)
import Data.Text hiding (null)
import Data.Vector qualified as V
import Models.Telemetry.Telemetry qualified as Telemetry
import Relude


getServiceName :: Telemetry.SpanRecord -> Text
getServiceName sp = case sp.resource of
  AE.Object r -> maybe "Unknown" serviceNameString $ KEM.lookup "service.name" r
  _ -> "Unknown"
  where
    serviceNameString :: AE.Value -> Text
    serviceNameString (AE.String s) = s
    serviceNameString _ = "Unknown"


getServiceColor :: Text -> HashMap Text Text -> Text
getServiceColor s serviceColors = fromMaybe "bg-black" $ HM.lookup s serviceColors


getRequestDetails :: Telemetry.SpanRecord -> Maybe (Text, Text, Text, Int)
getRequestDetails spanRecord = case spanRecord.attributes of
  AE.Object r -> case KEM.lookup "http.method" r of
    Just (AE.String method) -> Just ("HTTP", method, fromMaybe "/" $ getText "http.url" r, fromMaybe 0 $ getInt "http.status_code" r)
    _ -> case KEM.lookup "rpc.system" r of
      Just (AE.String "grpc") -> Just ("GRPC", fromMaybe "" $ getText "rpc.service" r, fromMaybe "" $ getText "rpc.method" r, fromMaybe 0 $ getInt "rpc.grpc.status_code" r)
      _ -> case KEM.lookup "http.request.method" r of
        Just (AE.String method) -> Just ("HTTP", method, fromMaybe "/" $ getText "http.request.url" r, fromMaybe 0 $ getInt "http.response.status_code" r)
        _ -> Nothing
  _ -> Nothing
  where
    getText :: Text -> AE.Object -> Maybe Text
    getText key v = case KEM.lookup (AEKey.fromText key) v of
      Just (AE.String s) -> Just s
      _ -> Nothing
    getInt :: Text -> AE.Object -> Maybe Int
    getInt key v = case KEM.lookup (AEKey.fromText key) v of
      Just (AE.Number n) -> toBoundedInteger n
      Just (AE.String s) -> readMaybe $ toString s
      _ -> Nothing


spanHasErrors :: Telemetry.SpanRecord -> Bool
spanHasErrors spanRecord = case spanRecord.events of
  AE.Array a ->
    let hasExceptionEvent event = case event of
          AE.Object obj -> KEM.lookup "event_name" obj == Just (AE.String "exception")
          _ -> False
     in Relude.any hasExceptionEvent (V.toList a)
  _ -> False


getSpanErrors :: Telemetry.SpanRecord -> [AE.Value]
getSpanErrors spanRecord = case spanRecord.events of
  AE.Array a ->
    let events = V.toList a
        hasExceptionEvent :: AE.Value -> Bool
        hasExceptionEvent event = case event of
          AE.Object obj -> KEM.lookup "event_name" obj == Just (AE.String "exception")
          _ -> False
     in Relude.filter hasExceptionEvent events
  _ -> []


getErrorDetails :: AE.Value -> (Text, Text, Text)
getErrorDetails ae = case ae of
  AE.Object obj -> case KEM.lookup "event_attributes" obj of
    Just (AE.Object j) -> (fromMaybe "" $ getText "exception.type" j, fromMaybe "" $ getText "exception.message" j, fromMaybe "" $ getText "exception.stacktrace" j)
    _ -> ("", "", "")
  _ -> ("", "", "")
  where
    getText :: Text -> AE.Object -> Maybe Text
    getText key v = case KEM.lookup (AEKey.fromText key) v of
      Just (AE.String s) -> Just s
      Just vl -> Just $ show vl
      _ -> Nothing
