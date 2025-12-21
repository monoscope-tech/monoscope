{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module ProcessMessage (
  processMessages,
  processSpanToEntities,
)
where

import Control.Lens ((^?))
import Control.Parallel.Strategies (parList, rpar, using)
import Data.Aeson qualified as AE
import Data.Aeson.Extra (lodashMerge)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens (key, _Object, _String)
import Data.Aeson.Types (KeyValue ((.=)), object)
import Data.Aeson.Types qualified as AE
import Data.ByteString.Lazy.Char8 qualified as BL
import Pkg.DeriveUtils (AesonText (..), UUIDId (..), unAesonTextMaybe)

import Data.Cache qualified as Cache
import Data.Effectful.UUID (UUIDEff)
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Data.Time (addUTCTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Labeled (Labeled (..))
import Effectful.Log (Log)
import Effectful.PostgreSQL (WithConnection)
import Effectful.Reader.Static qualified as Eff
import System.Types (DB)

import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.SummaryGenerator (generateSummary)
import Models.Telemetry.Telemetry (Context (trace_state), OtelLogsAndSpans (..))
import Models.Telemetry.Telemetry qualified as Telemetry
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import RequestMessages (ensureUrlParams, fieldsToFieldDTO, redactJSON, sortVector, valueToFields)
import RequestMessages qualified
import System.Config (AuthContext (..))
import System.Logging qualified as Log
import Utils (b64ToJson, eitherStrToText, freeTierDailyMaxEvents, nestedJsonFromDotNotation, toXXHash)


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

defaultProjectCache :: Projects.ProjectCache
defaultProjectCache =
  Projects.ProjectCache
    { hosts = []
    , endpointHashes = []
    , shapeHashes = []
    , redactFieldslist = []
    , dailyEventCount = 0
    , dailyMetricCount = 0
    , paymentPlan = "Free"
    }


processMessages
  :: (Concurrent :> es, DB es, Eff.Reader AuthContext :> es, Labeled "timefusion" WithConnection :> es, Log :> es, UUIDEff :> es)
  => [(Text, ByteString)]
  -> HashMap Text Text
  -> Eff es [Text]
processMessages [] _ = pure []
processMessages msgs attrs = do
  appCtx <- Eff.ask @AuthContext
  let msgs' =
        msgs <&> \(ackId, msg) -> do
          let sanitizedJsonStr = replaceNullChars $ decodeUtf8 msg
          recMsg <- eitherStrToText $ AE.eitherDecode $ BL.fromStrict $ encodeUtf8 sanitizedJsonStr
          Right (ackId, recMsg)

  unless (null $ lefts msgs') do
    let leftMsgs = [(a, b) | (Left a, b) <- zip msgs' msgs]
    forM_ leftMsgs \(a, (ackId, msg)) -> Log.logAttention "Error parsing json msgs" (object ["AckId" .= ackId, "Error" .= a, "OriginalMsg" .= decodeUtf8 @Text msg])

  let rMsgs = rights msgs'
  if null rMsgs
    then pure []
    else do
      caches <- liftIO $ do
        -- Use parallel evaluation for cache fetching
        let projectIds = (\(_, m) -> UUIDId m.projectId) <$> rMsgs
        cachePairs <- forM projectIds $ \pid ->
          Cache.fetchWithCache appCtx.projectCache pid $ \pid' -> do
            mpjCache <- Projects.projectCacheByIdIO appCtx.jobsPool pid'
            pure $! fromMaybe defaultProjectCache mpjCache
        -- Force evaluation of cache pairs
        pure (zip projectIds cachePairs `using` parList rpar)
      let projectCaches = HashMap.fromList caches

      spans <- forM rMsgs \(rmAckId, msg) -> do
        let pid = UUIDId msg.projectId
        case HashMap.lookup pid projectCaches of
          Just cache ->
            -- Check if project has exceeded daily limit for free tier
            let !totalDailyEvents = fromIntegral cache.dailyEventCount + fromIntegral cache.dailyMetricCount
                !isFreeTier = cache.paymentPlan == "Free"
                !hasExceededLimit = isFreeTier && totalDailyEvents >= freeTierDailyMaxEvents
             in if hasExceededLimit
                  then pure Nothing -- Discard events for projects that exceeded limits
                  else do
                    spanId <- UUID.genUUID
                    trId <- UUID.toText <$> UUID.genUUID
                    pure $ Just $ convertRequestMessageToSpan msg (spanId, trId)
          Nothing -> pure Nothing

      let spanVec = V.fromList spans
      unless (V.null spanVec)
        $ Telemetry.bulkInsertOtelLogsAndSpansTF (V.catMaybes spanVec)

      pure $ map fst rMsgs


-- Replace null characters in a Text
replaceNullChars :: Text -> Text
replaceNullChars = T.replace "\NUL" ""


-- Convert JSON value to Map
jsonToMap :: AE.Value -> Maybe (Map Text AE.Value)
jsonToMap (AE.Object o) = Just $ AEKM.toMapText o
jsonToMap _ = Nothing


-- | Process a single span to extract entities for anomaly detection
-- This function is the core of the anomaly detection system. It extracts:
-- 1. Endpoints - New API routes (triggers endpoint anomalies)
-- 2. Shapes - Request/response structures (triggers shape anomalies)
-- 3. Fields - Individual data fields (triggers field anomalies)
-- 4. Formats - Field value patterns (triggers format anomalies)
--
-- The extracted entities are compared against the project cache to avoid
-- redundant database operations. New entities will trigger database inserts
-- which fire PostgreSQL triggers to create anomaly records.
processSpanToEntities :: Projects.ProjectCache -> Telemetry.OtelLogsAndSpans -> UUID.UUID -> (Maybe Endpoints.Endpoint, Maybe Shapes.Shape, V.Vector Fields.Field, V.Vector Fields.Format, V.Vector Text)
processSpanToEntities pjc otelSpan dumpId =
  let !projectId = UUIDId $ Unsafe.fromJust $ UUID.fromText otelSpan.project_id

      -- Extract HTTP attributes from nested JSON structure
      !attributes = maybeToMonoid (unAesonTextMaybe otelSpan.attributes)
      !attrValue = AE.Object $ AEKM.fromMapText attributes

      -- Navigate nested JSON to extract values using lens
      !method = T.toUpper $ fromMaybe "GET" $ attrValue ^? key "http" . key "request" . key "method" . _String

      !routePath = fromMaybe "/" $ attrValue ^? key "http" . key "route" . _String

      !statusCode = fromMaybe 200 $ do
        statusStr <- attrValue ^? key "http" . key "response" . key "status_code" . _String
        readMaybe $ toString statusStr

      !host = fromMaybe "" $ attrValue ^? key "net" . key "host" . key "name" . _String

      -- Extract SDK type from attributes (needed for URL normalization)
      !sdkTypeStr =
        fromMaybe "unknown"
          $ (attrValue ^? key "monoscope" . key "sdk_type" . _String)
          <|> (attrValue ^? key "apitoolkit" . key "sdk_type" . _String)
      !sdkType = fromMaybe RequestDumps.SDKUnknown $ readMaybe $ toString sdkTypeStr

      -- URL normalization and dynamic path parameter extraction
      !urlPath' = RequestDumps.normalizeUrlPath sdkType statusCode method routePath
      !(!urlPathDyn, !pathParamsDyn, !hasDyn) = ensureUrlParams urlPath'
      !(!urlPath, !pathParams) = if hasDyn then (urlPathDyn, pathParamsDyn) else (urlPath', fromMaybe AE.emptyObject $ attrValue ^? key "http" . key "request" . key "path_params")

      -- Extract query params and headers from attributes
      !queryParams = fromMaybe AE.emptyObject $ attrValue ^? key "http" . key "request" . key "query_params"
      !requestHeaders = fromMaybe AE.emptyObject $ extractHeaders "http.request.headers" attrValue
      !responseHeaders = fromMaybe AE.emptyObject $ extractHeaders "http.response.headers" attrValue

      -- Generate endpoint hash - this uniquely identifies an API endpoint
      -- Hash components: projectId + host + method + urlPath
      -- This hash is used to detect new endpoints (endpoint anomalies)
      !endpointHash = toXXHash $ projectId.toText <> host <> method <> urlPath

      -- Set up redaction to protect sensitive data
      !redactFieldsList = pjc.redactFieldslist V.++ V.fromList [".set-cookie", ".password"]
      !redacted = redactJSON redactFieldsList

      -- Extract request/response bodies from span body
      !bodyValue = fromMaybe AE.Null (unAesonTextMaybe otelSpan.body)
      !requestBody = redacted $ fromMaybe AE.Null $ bodyValue ^? key "request_body"
      !responseBody = redacted $ fromMaybe AE.Null $ bodyValue ^? key "response_body"

      -- Extract and process all field categories
      !pathParamFields = valueToFields $ redacted pathParams
      !queryParamFields = valueToFields $ redacted queryParams
      !reqHeaderFields = valueToFields $ redacted requestHeaders
      !respHeaderFields = valueToFields $ redacted responseHeaders
      !reqBodyFields = valueToFields requestBody
      !respBodyFields = valueToFields responseBody

      -- Extract key paths for shape hash calculation
      !queryParamsKP = V.map fst queryParamFields
      !requestHeadersKP = V.map fst reqHeaderFields
      !responseHeadersKP = V.map fst respHeaderFields
      !requestBodyKP = V.map fst reqBodyFields
      !responseBodyKP = V.map fst respBodyFields

      -- Calculate shape hash - identifies unique request/response structures
      -- A shape represents the "schema" of an API call for a specific status code
      -- Hash components: endpointHash + statusCode + sorted field paths
      -- New shapes trigger shape anomalies indicating API structure changes
      !representativeKP = sortVector $ queryParamsKP <> responseHeadersKP <> requestBodyKP <> responseBodyKP
      !combinedKeyPathStr = T.concat $ V.toList representativeKP
      !shapeHash = endpointHash <> show statusCode <> toXXHash combinedKeyPathStr

      -- Convert all field categories to DTOs
      !pathParamsFieldsDTO = V.map (fieldsToFieldDTO Fields.FCPathParam projectId endpointHash) pathParamFields
      !queryParamsFieldsDTO = V.map (fieldsToFieldDTO Fields.FCQueryParam projectId endpointHash) queryParamFields
      !reqHeadersFieldsDTO = V.map (fieldsToFieldDTO Fields.FCRequestHeader projectId endpointHash) reqHeaderFields
      !respHeadersFieldsDTO = V.map (fieldsToFieldDTO Fields.FCResponseHeader projectId endpointHash) respHeaderFields
      !reqBodyFieldsDTO = V.map (fieldsToFieldDTO Fields.FCRequestBody projectId endpointHash) reqBodyFields
      !respBodyFieldsDTO = V.map (fieldsToFieldDTO Fields.FCResponseBody projectId endpointHash) respBodyFields
      !fieldsDTO = pathParamsFieldsDTO <> queryParamsFieldsDTO <> reqHeadersFieldsDTO <> respHeadersFieldsDTO <> reqBodyFieldsDTO <> respBodyFieldsDTO

      !(!fields, !formats) = V.unzip fieldsDTO
      !fieldHashes = sortVector $ V.map (.hash) fields

      -- Determine if request is outgoing based on span kind
      !outgoing = otelSpan.kind == Just "client"

      -- Build endpoint if not in cache
      -- Only create endpoint entity if:
      -- 1. Not already in project cache (prevents duplicate anomalies)
      -- 2. Not a 404 response (ignore missing routes)
      -- When inserted, will trigger endpoints_created_anomaly in DB
      !endpoint =
        if endpointHash `elem` pjc.endpointHashes || statusCode == 404
          then Nothing
          else
            Just
              $ Endpoints.Endpoint
                { createdAt = otelSpan.timestamp
                , updatedAt = otelSpan.timestamp
                , id = UUIDId dumpId
                , projectId = projectId
                , urlPath = urlPath
                , urlParams = AE.emptyObject -- TODO: Should this use pathParams?
                , method = method
                , host = host
                , hash = endpointHash
                , outgoing = outgoing
                , description = ""
                }

      -- Build shape if not in cache
      -- Shape represents the structure of request/response for a specific endpoint+status
      -- Only create if not cached and not 404
      -- When inserted, will trigger shapes_created_anomaly in DB
      !shape =
        if shapeHash `elem` pjc.shapeHashes || statusCode == 404
          then Nothing
          else
            Just
              $ Shapes.Shape
                { id = UUIDId dumpId
                , createdAt = otelSpan.timestamp
                , updatedAt = otelSpan.timestamp
                , approvedOn = Nothing
                , projectId = projectId
                , endpointHash = endpointHash
                , queryParamsKeypaths = queryParamsKP
                , requestBodyKeypaths = requestBodyKP
                , responseBodyKeypaths = responseBodyKP
                , requestHeadersKeypaths = requestHeadersKP
                , responseHeadersKeypaths = responseHeadersKP
                , fieldHashes = fieldHashes
                , hash = shapeHash
                , statusCode = statusCode
                , responseDescription = ""
                , requestDescription = ""
                }

      !fields' = if statusCode == 404 then V.empty else fields
      !formats' = if statusCode == 404 then V.empty else formats

      -- Collect hashes to update span with
      !hashes =
        V.fromList
          $ catMaybes
            [ Just endpointHash
            , if isJust shape then Just shapeHash else Nothing
            ]
          <> V.toList fieldHashes
   in (endpoint, shape, fields', formats', hashes)
  where
    -- Helper function to extract headers from nested attribute structure
    extractHeaders :: Text -> AE.Value -> Maybe AE.Value
    extractHeaders prefix obj = case obj of
      AE.Object keyMap ->
        let headerPairs = [(T.drop (T.length prefix + 1) (AEK.toText k), v) | (k, v) <- AEKM.toList keyMap, T.isPrefixOf (prefix <> ".") (AEK.toText k)]
         in if null headerPairs
              then Nothing
              else Just $ AE.Object $ AEKM.fromList [(AEK.fromText k, v) | (k, v) <- headerPairs]
      _ -> Nothing


convertRequestMessageToSpan :: RequestMessages.RequestMessage -> (UUID.UUID, Text) -> Telemetry.OtelLogsAndSpans
convertRequestMessageToSpan rm (spanId, trId) =
  let
    -- Convert parent_id, ensuring empty strings become Nothing
    !parentId = case (Just . UUID.toText) =<< rm.parentId of
      Just txt | T.null txt -> Nothing
      other -> other

    otelSpan =
      Telemetry.OtelLogsAndSpans
        { id = UUID.toText UUID.nil
        , project_id = UUID.toText rm.projectId
        , timestamp = zonedTimeToUTC rm.timestamp
        , parent_id = parentId
        , context = Just $ Telemetry.Context{trace_id = Just trId, span_id = Just $ UUID.toText spanId, trace_state = Nothing, trace_flags = Nothing, is_remote = Nothing}
        , name = Just "monoscope.http"
        , start_time = zonedTimeToUTC rm.timestamp
        , end_time = Just $ addUTCTime (realToFrac (fromIntegral rm.duration / 1000000000)) (zonedTimeToUTC rm.timestamp)
        , kind = Just $ if T.isSuffixOf "Outgoing" (show rm.sdkType) then "client" else "server"
        , level = Nothing
        , body = Just $ AesonText $ AE.object ["request_body" AE..= b64ToJson rm.requestBody, "response_body" AE..= b64ToJson rm.responseBody]
        , severity = Nothing
        , status_message = Just $ case rm.statusCode of
            sc
              | sc >= 400 -> "Error"
              | otherwise -> "OK"
        , status_code = Just $ show rm.statusCode
        , hashes = []
        , observed_timestamp = Just $ zonedTimeToUTC rm.timestamp
        , attributes = fmap AesonText $ jsonToMap $ createSpanAttributes rm
        , events = Just $ AesonText $ AE.Array V.empty
        , links = Just ""
        , resource =
            fmap AesonText
              $ jsonToMap
              $ nestedJsonFromDotNotation
                [ ("service.name", AE.String $ fromMaybe "unknown" rm.host)
                , ("service.version", maybe (AE.String "") AE.String rm.serviceVersion)
                , ("telemetry.sdk.language", AE.String "apitoolkit")
                , ("telemetry.sdk.name", AE.String $ show rm.sdkType)
                ]
        , duration = Just $ fromIntegral rm.duration
        , summary = V.empty -- Will be populated below
        , date = zonedTimeToUTC rm.timestamp
        , errors = Nothing
        , log_pattern = Nothing
        , summary_pattern = Nothing
        }
   in
    otelSpan{summary = generateSummary otelSpan}


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
          , ("http.method", AE.String rm.method)
          , ("http.request.method", AE.String rm.method)
          , ("http.request.path_params", rm.pathParams)
          , ("http.request.query_params", rm.queryParams)
          , ("http.request.path", AE.String $ fromMaybe "/" rm.urlPath)
          , ("http.response.status_code", AE.Number $ fromIntegral rm.statusCode)
          , ("http.status_code", AE.Number $ fromIntegral rm.statusCode)
          , ("http.route", AE.String $ fromMaybe (T.takeWhile (/= '?') rm.rawUrl) rm.urlPath)
          , ("http.url", AE.String rm.rawUrl)
          , ("url.path", AE.String $ fromMaybe "/" rm.urlPath)
          , ("url.full", AE.String rm.rawUrl)
          , ("monoscope.msg_id", AE.String $ maybe "" UUID.toText rm.msgId)
          , ("monoscope.parent_id", AE.String $ maybe "" UUID.toText rm.parentId)
          , ("monoscope.sdk_type", AE.String $ show rm.sdkType)
          , ("monoscope.errors", AE.String $ maybe "[]" (Relude.decodeUtf8 . AE.encode) rm.errors)
          ]
   in baseAttrs
        `lodashMerge` refererObj
        `lodashMerge` headersObj
        `lodashMerge` tagsObj
  where
    -- Process tags
    tagsObj = case rm.tags of
      Just tags -> nestedJsonFromDotNotation [("monoscope.tags", AE.Array $ V.fromList $ map AE.String tags)]
      Nothing -> AE.object []

    -- Process referer
    refererObj = case rm.referer of
      Just (Left text) -> nestedJsonFromDotNotation [("http.request.headers.referer", AE.String text)]
      Just (Right texts) -> nestedJsonFromDotNotation [("http.request.headers.referer", AE.String $ T.intercalate "," texts)]
      Nothing -> AE.object []

    -- Process headers
    headersObj =
      let
        -- Convert request headers using lens
        reqHeaders =
          fromMaybe (AE.object [])
            $ rm.requestHeaders
            ^? _Object
              >>= \obj ->
                let pairs = [("http.request.headers." <> AEK.toText k, v) | (k, v) <- AEKM.toList obj]
                 in Just $ nestedJsonFromDotNotation pairs

        -- Convert response headers using lens
        respHeaders =
          fromMaybe (AE.object [])
            $ rm.responseHeaders
            ^? _Object
              >>= \obj ->
                let pairs = [("http.response.headers." <> AEK.toText k, v) | (k, v) <- AEKM.toList obj]
                 in Just $ nestedJsonFromDotNotation pairs
       in
        reqHeaders `mergeJsonObjects` respHeaders
