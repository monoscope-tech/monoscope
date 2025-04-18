{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module OtelProtoTest (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Base16 qualified as B16
import Data.ProtoLens (defMessage, messageName, showMessage)
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.ProtoLens.Field
import Data.ProtoLens.Labels ()
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Lens.Family2 ((^.), (.~), (&), (^?))
import Relude
import Test.Hspec

-- Import the generated modules
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService 
  ( ExportTraceServiceRequest(..)
  , ExportTraceServiceResponse(..)
  )
import Proto.Opentelemetry.Proto.Common.V1.Common 
  ( AnyValue(..)
  , KeyValue(..)
  , InstrumentationScope(..)
  )
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields 
  ( key
  , stringValue
  , value
  , name
  , version
  )
import Proto.Opentelemetry.Proto.Resource.V1.Resource 
  ( Resource(..)
  )
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields 
  ( attributes
  )
import Proto.Opentelemetry.Proto.Trace.V1.Trace 
  ( ResourceSpans(..)
  , ScopeSpans(..)
  , Span(..)
  , Status(..)
  , Span'SpanKind(..)
  , Status'StatusCode(..)
  )
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields
  ( attributes
  , endTimeUnixNano
  , name
  , parentSpanId
  , resource
  , resourceSpans
  , scope
  , scopeSpans
  , spanId
  , spans
  , startTimeUnixNano
  , traceId
  , kind
  , status
  , statusCode
  , statusMessage
  )

-- Helper function to create a string KeyValue
createStringAttribute :: Text -> Text -> KeyValue
createStringAttribute key val = defMessage
  & #key .~ key
  & #value .~ (defMessage & #stringValue .~ val)

-- Helper function to create a span
createSpan :: Text -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Word64 -> Word64 -> Span'SpanKind -> V.Vector KeyValue -> Span
createSpan name' traceId' spanId' parentSpanId' startTime endTime kind' attrs = 
  defMessage
    & #name .~ name'
    & #traceId .~ traceId'
    & #spanId .~ spanId'
    & #parentSpanId .~ parentSpanId'
    & #startTimeUnixNano .~ startTime
    & #endTimeUnixNano .~ endTime
    & #kind .~ kind'
    & #attributes .~ attrs
    & #status .~ (defMessage & #statusCode .~ Status'STATUS_CODE_OK & #statusMessage .~ "")

-- Create a complete trace request with resource and scope
createCompleteTraceRequest :: ExportTraceServiceRequest
createCompleteTraceRequest = defMessage
  & #resourceSpans .~ V.singleton (
      defMessage 
        & #resource .~ Just (
            defMessage 
              & #attributes .~ V.fromList [
                  createStringAttribute "service.name" "test-service",
                  createStringAttribute "service.version" "1.0.0"
                ]
          )
        & #scopeSpans .~ V.singleton (
            defMessage 
              & #scope .~ Just (
                  defMessage 
                    & #name .~ "opentelemetry.sdk"
                    & #version .~ "1.0.0"
                )
              & #spans .~ V.fromList [
                  -- Root span
                  createSpan 
                    "GET /api/users" 
                    "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F\x10" 
                    "\x01\x02\x03\x04\x05\x06\x07\x08"
                    "" 
                    1619712000000000000 
                    1619712001000000000 
                    Span'SPAN_KIND_SERVER
                    (V.fromList [
                      createStringAttribute "http.method" "GET",
                      createStringAttribute "http.url" "/api/users",
                      createStringAttribute "http.status_code" "200"
                    ]),
                  
                  -- Child span
                  createSpan 
                    "DB Query: SELECT * FROM users" 
                    "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F\x10" 
                    "\x11\x12\x13\x14\x15\x16\x17\x18"
                    "\x01\x02\x03\x04\x05\x06\x07\x08" 
                    1619712000100000000 
                    1619712000900000000 
                    Span'SPAN_KIND_INTERNAL
                    (V.fromList [
                      createStringAttribute "db.type" "postgresql",
                      createStringAttribute "db.statement" "SELECT * FROM users"
                    ])
                ]
          )
    )

-- Utility function to hex encode a bytestring for display
hexEncode :: BS.ByteString -> Text
hexEncode = TE.decodeUtf8 . B16.encode

spec :: Spec
spec = do
  describe "OpenTelemetry Protobuf Generated Code" $ do
    it "should encode and decode a minimal span" $ do
      let span = defMessage
            & #name .~ "test-span"
            & #traceId .~ "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F\x10"
            & #spanId .~ "\x01\x02\x03\x04\x05\x06\x07\x08"
            & #startTimeUnixNano .~ 1619712000000000000
            & #endTimeUnixNano .~ 1619712001000000000
          
          encoded = encodeMessage span
          decoded = decodeMessage encoded
      
      decoded `shouldBe` Right span
    
    it "should encode and decode a complete trace request" $ do
      let request = createCompleteTraceRequest
          encoded = encodeMessage request
          decoded = decodeMessage encoded
      
      decoded `shouldBe` Right request
    
    it "should properly access nested fields after decoding" $ do
      let request = createCompleteTraceRequest
          encoded = encodeMessage request
          decoded = decodeMessage encoded
      
      case decoded of
        Left err -> fail $ "Decoding failed: " <> show err
        Right req -> do
          -- Test resource spans
          let resourceSpans = req ^. #resourceSpans
          V.length resourceSpans `shouldBe` 1
          
          -- Test resource attributes
          let rs = V.head resourceSpans
          let resourceM = rs ^. #resource
          resourceM `shouldSatisfy` isJust
          
          let Just resource' = resourceM
          let resourceAttrs = resource' ^. #attributes
          V.length resourceAttrs `shouldBe` 2
          
          -- Find service.name attribute
          let serviceNameAttr = V.find (\kv -> kv ^. #key == "service.name") resourceAttrs
          serviceNameAttr `shouldSatisfy` isJust
          let Just sn = serviceNameAttr
          sn ^? #value . #stringValue `shouldBe` Just "test-service"
          
          -- Test scope spans
          let scopeSpans' = rs ^. #scopeSpans
          V.length scopeSpans' `shouldBe` 1
          let ss = V.head scopeSpans'
          
          -- Test scope
          let scopeM = ss ^. #scope
          scopeM `shouldSatisfy` isJust
          let Just scope' = scopeM
          scope' ^. #name `shouldBe` "opentelemetry.sdk"
          scope' ^. #version `shouldBe` "1.0.0"
          
          -- Test spans
          let spans' = ss ^. #spans
          V.length spans' `shouldBe` 2
          
          -- Test root span
          let rootSpan = spans' V.! 0
          rootSpan ^. #name `shouldBe` "GET /api/users"
          rootSpan ^. #kind `shouldBe` Span'SPAN_KIND_SERVER
          rootSpan ^. #parentSpanId `shouldBe` ""
          
          -- Test child span
          let childSpan = spans' V.! 1
          childSpan ^. #name `shouldBe` "DB Query: SELECT * FROM users"
          childSpan ^. #kind `shouldBe` Span'SPAN_KIND_INTERNAL
          childSpan ^. #parentSpanId `shouldBe` "\x01\x02\x03\x04\x05\x06\x07\x08"
          
          -- Verify parent-child relationship
          childSpan ^. #parentSpanId `shouldBe` rootSpan ^. #spanId