{-# LANGUAGE ScopedTypeVariables #-}

module ProcessMessage (
  processMessages,
  processSpanToEntities,
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Extra (lodashMerge)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens (key, _String, _Object)
import Control.Lens ((^?))
import Data.Aeson.Types (KeyValue ((.=)), object)
import Data.Aeson.Types qualified as AE
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Effectful.UUID (UUIDEff)
import Data.Effectful.UUID qualified as UUID
import Data.Text qualified as T
import Data.Time (addUTCTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled (..))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Log qualified
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (Context (trace_state))
import Models.Telemetry.Telemetry qualified as Telemetry
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import RequestMessages qualified
import RequestMessages (fieldsToFieldDTO, sortVector, valueToFields)
import System.Config qualified as Config
import Utils (b64ToJson, eitherStrToText, nestedJsonFromDotNotation, toXXHash)


{--
  Exploring how the inmemory cache could be shaped for performance, and low footprint ability to skip hitting the postgres database when not needed.

  -- All vectors here should be sorted (or we could use sets), then we would be able to do conttainment and prefix searches via binary search (Works fast on sorted lists)
  -- It will likely be fine to just insert these projects into the cache with a short (eg 5mins) TTL, and then reload if from the db every 5 mins as needed.
  -- - This might not be most efficient, but we don't need to worry too much about keeping this data in sync but updating values in the cache based on live requests.
  -- - Over inserting into the database within that 5minute timeline should probably be a non-issue in comparison.
  -- Implementation should just be a function that accepts the cache type and a project id, then it checks if the project id exists or not. if it doesn't exist,
  -- it would run the query to get the project from the db in the exact shape we need, and fill up the cache before returning the value

  -- NOTE: It might be worth it to explore using sets as well for the project cache

  Project Cache structure.
 <projectID> =
   {
      -- Used for the dashboards on every page. The title is displayed on the sidebar.
      -- Title  is also not requeired since in the sidebar, we also show the list of projects,
      -- and it's easier to just deal with that via a db call for list of projects, and use that for all project disply actions.
      Title ""
      -- We need this hosts to mirrow all the hosts in the endpoints table, and could use this for validation purposes to skip inserting endpoints just because of hosts
      -- if endpoint exists but host is not in this list, then we have a query specifically for inserting hosts.
      hosts []
      -- maybe we don't need this? See the next point.
      endpoint_hashes []
      - Since shapes always have the endpoints hash prepended to them, maybe we don't need to store the hash of endpoints, since we can derive that from the shapes.
      shape_hashes []
      --
      -- We could also only hold the hash of formatids.
      -- Since all the hashes are computed and concatenated making it a clear heirarchy/prefix tree?. T
      -- his might be more space efficient to store, at the cost of compute/complexity in searching for them.
      -- Also, maybe instead of shapes being the important unit for checking that we have a shape already, we could just use format_hashes for that. TO make sure every format is not new as well.
      format_hashes []
      --
      -- We check if every request is part of the redact list, so it's better if we don't need to  hit the db for them with each request.
      redact_fields_list []
     }
      -- The only problem is, how do we deal with counts for these format_hashes to make sure we have enough examples for that format_hash?
      -- Should we postfix the count of field examples to it when building this list?
      -- Then only actually skip inserting them if the postfixed number is equal to the maax number of examples per field which we expect.
      -- Next issue with this mode is:
      --    - How do we deal with fields which have a fixed number of examples or value set, which is less than the total number of max examples?
      --      Eg a field has 2 possible answers: yes or no, but then our max is 10, so we keep checking if it has up to 10 fields, but then because it doesn't, we never skip the database operation. 🤔
      --
      -- A solution to this issue could be to have another hash map, but this time, linking:
      <shape_hash> => <ioref counter>
      -- So i can track the count of for that shape in the current session, and allo up to 100 or 200 or more requests per shape but cap it there.

  How to deal with tracking preventing sending field and format updates when we don't need them.
  ==============================
  A workaround to maintaining the count of processed items for a shape, is to intead, deal with endpoints and just maintain number of tracked calls per endpoint.
  It can be a sufficiently large number, to workaround for the probabilities.
  So, we could maintain another inmemory cache that holds endpoints and their counts in memory. It's easy to deterministically  get the hash of an endpoint,
  so we could simply calculate the endpoint hash much earlier, then use it to lookup and incremement the count and return the number. atomically.
  Also, instead of passing this count into the requestMessage function, we can instead pass precalculate everything and just pass in a boolean in there as an argument.
    The advantage of doing this is that we can do other calculations, such as throwing a dice and randomly deciding to still send the request to the db.
    Sending random requests to the DB might still help us catch new fields(formats) that we didnt know about, etc.

    We could also maintain hashes of all the formats in the cache, and check each field format within this list.🤔
 --}
processMessages
  -- :: (Reader.Reader Config.AuthContext :> es, Time.Time :> es, DB :> es, Log :> es, IOE :> es)
  :: (DB :> es, IOE :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" DB :> es, Log :> es, Reader.Reader Config.AuthContext :> es, Time.Time :> es, UUIDEff :> es)
  => [(Text, ByteString)]
  -> HashMap Text Text
  -> Eff es [Text]
processMessages [] _ = pure []
processMessages msgs attrs = do
  let msgs' =
        msgs <&> \(ackId, msg) -> do
          let sanitizedJsonStr = replaceNullChars $ decodeUtf8 msg
          recMsg <- eitherStrToText $ AE.eitherDecode $ BL.fromStrict $ encodeUtf8 sanitizedJsonStr
          Right (ackId, recMsg)

  unless (null $ lefts msgs') do
    let leftMsgs = [(a, b) | (Left a, b) <- zip msgs' msgs]
    forM_ leftMsgs \(a, (ackId, msg)) -> Log.logAttention "Error parsing json msgs" (object ["AckId" .= ackId, "Error" .= a, "OriginalMsg" .= decodeUtf8 @Text msg])

  if null $ rights msgs'
    then pure []
    else do
      spans <- forM (rights msgs') \(rmAckId, msg) -> do
        spanId <- UUID.genUUID
        trId <- UUID.toText <$> UUID.genUUID
        pure $ convertRequestMessageToSpan msg (spanId, trId)
      let spanVec = V.fromList spans
      unless (V.null spanVec)
        $ void
        $ Telemetry.bulkInsertOtelLogsAndSpansTF spanVec

      pure $ map fst (rights msgs')


-- Replace null characters in a Text
replaceNullChars :: Text -> Text
replaceNullChars = T.replace "\\u0000" ""


-- Convert JSON value to Map
jsonToMap :: AE.Value -> Maybe (Map Text AE.Value)
jsonToMap (AE.Object o) = Just $ AEKM.toMapText o
jsonToMap _ = Nothing


-- | Process a single span to extract entities
processSpanToEntities :: Projects.ProjectCache -> Telemetry.OtelLogsAndSpans -> UUID.UUID -> (Maybe Endpoints.Endpoint, Maybe Shapes.Shape, V.Vector Fields.Field, V.Vector Formats.Format, V.Vector Text)
processSpanToEntities pjc span dumpId =
  let projectId = Projects.ProjectId $ Unsafe.fromJust $ UUID.fromText span.project_id

      -- Extract HTTP attributes from nested JSON structure
      attributes = fromMaybe mempty span.attributes
      attrValue = AE.Object $ AEKM.fromMapText attributes

      -- Navigate nested JSON to extract values using lens
      method = fromMaybe "GET" $ T.toUpper <$> attrValue ^? key "http" . key "request" . key "method" . _String
      
      path = fromMaybe "/" $ attrValue ^? key "http" . key "request" . key "path" . _String
      
      statusCode = fromMaybe 200 $ do
        statusStr <- attrValue ^? key "http" . key "response" . key "status_code" . _String
        readMaybe $ T.unpack statusStr
      
      host = fromMaybe "" $ attrValue ^? key "net" . key "host" . key "name" . _String

      -- Extract request/response bodies from span body
      bodyValue = fromMaybe AE.Null span.body
      requestBody = fromMaybe AE.Null $ bodyValue ^? key "request_body"
      responseBody = fromMaybe AE.Null $ bodyValue ^? key "response_body"

      -- Generate endpoint hash
      endpointHash = toXXHash $ method <> path

      -- Extract fields from bodies using existing valueToFields function
      reqBodyFields = valueToFields requestBody
      respBodyFields = valueToFields responseBody

      -- Build shape hash
      representativeKP = sortVector $ V.map fst reqBodyFields <> V.map fst respBodyFields
      combinedKeyPathStr = T.concat $ V.toList representativeKP
      shapeHash = endpointHash <> show statusCode <> toXXHash combinedKeyPathStr

      -- Convert to field DTOs
      reqBodyFieldsDTO = V.map (fieldsToFieldDTO Fields.FCRequestBody projectId endpointHash) reqBodyFields
      respBodyFieldsDTO = V.map (fieldsToFieldDTO Fields.FCResponseBody projectId endpointHash) respBodyFields
      fieldsDTO = reqBodyFieldsDTO <> respBodyFieldsDTO

      (fields, formats) = V.unzip fieldsDTO
      fieldHashes = sortVector $ V.map (.hash) fields

      -- Build endpoint if not in cache
      endpoint =
        if endpointHash `elem` pjc.endpointHashes || statusCode == 404
          then Nothing
          else
            Just
              $ Endpoints.Endpoint
                { createdAt = span.timestamp
                , updatedAt = span.timestamp
                , id = Endpoints.EndpointId dumpId
                , projectId = projectId
                , urlPath = path
                , urlParams = AE.emptyObject
                , method = method
                , host = host
                , hash = endpointHash
                , outgoing = span.kind == Just "client"
                , description = ""
                }

      -- Build shape if not in cache
      shape =
        if shapeHash `elem` pjc.shapeHashes || statusCode == 404
          then Nothing
          else
            Just
              $ Shapes.Shape
                { id = Shapes.ShapeId dumpId
                , createdAt = span.timestamp
                , updatedAt = span.timestamp
                , approvedOn = Nothing
                , projectId = projectId
                , endpointHash = endpointHash
                , queryParamsKeypaths = V.empty
                , requestBodyKeypaths = V.map fst reqBodyFields
                , responseBodyKeypaths = V.map fst respBodyFields
                , requestHeadersKeypaths = V.empty
                , responseHeadersKeypaths = V.empty
                , fieldHashes = fieldHashes
                , hash = shapeHash
                , statusCode = statusCode
                , responseDescription = ""
                , requestDescription = ""
                }

      fields' = if statusCode == 404 then V.empty else fields
      formats' = if statusCode == 404 then V.empty else formats

      -- Collect hashes to update span with
      hashes =
        V.fromList
          $ catMaybes
            [ Just endpointHash
            , if isJust shape then Just shapeHash else Nothing
            ]
            <> V.toList fieldHashes
   in (endpoint, shape, fields', formats', hashes)


convertRequestMessageToSpan :: RequestMessages.RequestMessage -> (UUID.UUID, Text) -> Telemetry.OtelLogsAndSpans
convertRequestMessageToSpan rm (spanId, trId) =
  Telemetry.OtelLogsAndSpans
    { id = UUID.nil
    , project_id = UUID.toText rm.projectId
    , timestamp = zonedTimeToUTC rm.timestamp
    , parent_id = (Just . UUID.toText) =<< rm.parentId
    , context = Just $ Telemetry.Context{trace_id = Just trId, span_id = Just $ UUID.toText spanId, trace_state = Nothing, trace_flags = Nothing, is_remote = Nothing}
    , name = Just $ rm.method <> maybe "" (" " <>) rm.urlPath
    , start_time = zonedTimeToUTC rm.timestamp
    , end_time = Just $ addUTCTime (realToFrac (fromIntegral rm.duration / 1000000000)) (zonedTimeToUTC rm.timestamp)
    , kind = Just $ if T.isSuffixOf "Outgoing" (show rm.sdkType) then "client" else "server"
    , level = Nothing
    , body = Just $ AE.object ["request_body" AE..= b64ToJson rm.requestBody, "response_body" AE..= b64ToJson rm.responseBody]
    , severity = Nothing
    , status_message = Just $ case rm.statusCode of
        sc
          | sc >= 400 -> "Error"
          | otherwise -> "OK"
    , status_code = Just $ show rm.statusCode
    , hashes = []
    , observed_timestamp = Just $ zonedTimeToUTC rm.timestamp
    , attributes = jsonToMap $ createSpanAttributes rm
    , events = Just $ AE.Array V.empty
    , links = Just ""
    , resource =
        jsonToMap
          $ nestedJsonFromDotNotation
            [ ("service.name", AE.String $ fromMaybe "unknown" rm.host)
            , ("telemetry.sdk.language", AE.String "apitoolkit")
            , ("telemetry.sdk.name", AE.String $ show rm.sdkType)
            ]
    , duration = Just $ fromIntegral rm.duration
    , date = zonedTimeToUTC rm.timestamp
    }


-- Using nestedJsonFromDotNotation from Utils module

-- Helper function to merge JSON objects
-- Now using lodashMerge from aeson-extra which properly handles nested objects
mergeJsonObjects :: AE.Value -> AE.Value -> AE.Value
mergeJsonObjects = lodashMerge


createSpanAttributes :: RequestMessages.RequestMessage -> AE.Value
createSpanAttributes rm =
  let baseAttrs =
        nestedJsonFromDotNotation
          [ ("net.host.name", AE.String $ fromMaybe "" rm.host)
          , ("http.request.method", AE.String rm.method)
          , ("http.request.path_params", AE.String $ Relude.decodeUtf8 $ AE.encode rm.pathParams)
          , ("http.request.query_params", AE.String $ Relude.decodeUtf8 $ AE.encode rm.queryParams)
          , ("apitoolkit.msg_id", AE.String $ maybe "" UUID.toText rm.msgId)
          , ("apitoolkit.parent_id", AE.String $ maybe "" UUID.toText rm.parentId)
          , ("http.response.status_code", AE.Number $ fromIntegral rm.statusCode)
          , ("apitoolkit.sdk_type", AE.String $ show rm.sdkType)
          , ("http.route", maybe (AE.String (T.takeWhile (/= '?') rm.rawUrl)) AE.String rm.urlPath)
          , ("url.path", AE.String rm.rawUrl)
          ]
   in baseAttrs
        `lodashMerge` refererObj
        `lodashMerge` errorsObj
        `lodashMerge` headersObj
  where
    -- Process referer
    refererObj = case rm.referer of
      Just (Left text) -> nestedJsonFromDotNotation [("http.request.headers.referer", AE.String text)]
      Just (Right texts) -> nestedJsonFromDotNotation [("http.request.headers.referer", AE.String $ T.intercalate "," texts)]
      Nothing -> AE.object []

    -- Process errors
    errorsObj = case rm.errors of
      Just errs -> nestedJsonFromDotNotation [("apitoolkit.errors", AE.String $ Relude.decodeUtf8 $ AE.encode errs)]
      Nothing -> AE.object []

    -- Process headers
    headersObj =
      let
        -- Convert request headers using lens
        reqHeaders = maybe (AE.object []) id $ rm.requestHeaders ^? _Object >>= \obj ->
          let pairs = [("http.request.headers." <> AEK.toText k, v) | (k, v) <- AEKM.toList obj]
           in Just $ nestedJsonFromDotNotation pairs

        -- Convert response headers using lens
        respHeaders = maybe (AE.object []) id $ rm.responseHeaders ^? _Object >>= \obj ->
          let pairs = [("http.response.headers." <> AEK.toText k, v) | (k, v) <- AEKM.toList obj]
           in Just $ nestedJsonFromDotNotation pairs
       in
        reqHeaders `mergeJsonObjects` respHeaders
