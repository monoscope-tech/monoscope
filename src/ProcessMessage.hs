{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}

module ProcessMessage (
  processMessages,
  processSpanToEntities,
  RequestMessage (..),
  valueToFormatStr,
  valueToFields,
  redactJSON,
  replaceNullChars,
  fieldsToFieldDTO,
  sortVector,
  ensureUrlParams,
  dedupFields,
)
where

import Control.Lens ((^?))
import Control.Monad.ST (ST, runST)
import Control.Parallel.Strategies (parList, rpar, using)
import Data.Aeson qualified as AE
import Data.Aeson.Extra (lodashMerge)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens (key, _Object, _String)
import Data.Aeson.Types (KeyValue ((.=)), object)
import Data.Aeson.Types qualified as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Cache qualified as Cache
import Data.Effectful.UUID (UUIDEff)
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict qualified as HashMap
import Data.HashTable.Class qualified as HTC
import Data.HashTable.ST.Cuckoo qualified as HT
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (addUTCTime, zonedTimeToUTC)
import Data.Time.LocalTime (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Labeled (Labeled (..))
import Effectful.Log (Log)
import Effectful.PostgreSQL (WithConnection)
import Effectful.Reader.Static qualified as Eff
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.SummaryGenerator (generateSummary)
import Models.Telemetry.Telemetry (Context (trace_state), OtelLogsAndSpans (..))
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (AesonText (..), UUIDId (..), unAesonTextMaybe)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..))
import System.Logging qualified as Log
import System.Types (DB)
import Text.RE.Replace (matched)
import Text.RE.TDFA (RE, re, (?=~))
import Utils (b64ToJson, eitherStrToText, freeTierDailyMaxEvents, nestedJsonFromDotNotation, replaceAllFormats, toXXHash)


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
          let sanitizedJsonStr = stripNulBytes $ decodeUtf8 msg
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


-- Strip literal NUL bytes from Text
stripNulBytes :: Text -> Text
stripNulBytes = T.replace "\NUL" ""


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
      !sdkType = fromMaybe LogQueries.SDKUnknown $ readMaybe $ toString sdkTypeStr

      -- URL normalization and dynamic path parameter extraction
      !urlPath' = LogQueries.normalizeUrlPath sdkType statusCode method routePath
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


convertRequestMessageToSpan :: RequestMessage -> (UUID.UUID, Text) -> Telemetry.OtelLogsAndSpans
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
        }
   in
    otelSpan{summary = generateSummary otelSpan}


-- Using nestedJsonFromDotNotation from Utils module

-- Helper function to merge JSON objects
-- Now using lodashMerge from aeson-extra which properly handles nested objects
mergeJsonObjects :: AE.Value -> AE.Value -> AE.Value
mergeJsonObjects = lodashMerge


createSpanAttributes :: RequestMessage -> AE.Value
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


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as V
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson
-- >>> import Data.Aeson qualified as AE
-- >>> import Utils (replaceAllFormats)
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes


-- | RequestMessage represents a message for a single request pulled from pubsub.
data RequestMessage = RequestMessage
  { duration :: Int -- in nanoseconds
  , host :: Maybe Text
  , method :: Text
  , pathParams :: AE.Value --- key value map of the params to their values in the original urlpath.
  , projectId :: UUID.UUID
  , protoMajor :: Int
  , protoMinor :: Int
  , queryParams :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , rawUrl :: Text -- raw request uri: path?query combination
  , referer :: Maybe (Either Text [Text])
  , requestBody :: Text
  , requestHeaders :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , responseBody :: Text
  , responseHeaders :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , sdkType :: LogQueries.SDKTypes -- convension should be <language>-<router library> eg: go-gin, go-builtin, js-express
  , statusCode :: Int
  , urlPath :: Maybe Text -- became Maybe to support express, which sometimes doesn't send urlPath for root.
  , timestamp :: ZonedTime
  , msgId :: Maybe UUID.UUID -- This becomes the span id.
  , parentId :: Maybe UUID.UUID
  , serviceVersion :: Maybe Text -- allow users track deployments and versions (tags, commits, etc)
  , errors :: Maybe [LogQueries.ATError]
  , tags :: Maybe [Text]
  }
  deriving stock (Generic, Show)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestMessage


-- Custom ToJSON for Either Text [Text]
instance {-# OVERLAPPING #-} AE.ToJSON (Either Text [Text]) where
  toJSON (Left txt) = AE.toJSON txt
  toJSON (Right texts) = AE.toJSON texts


-- Custom FromJSON for Either Text [Text]
instance {-# OVERLAPPING #-} AE.FromJSON (Either Text [Text]) where
  parseJSON value = case value of
    AE.String txt -> return $ Left txt
    AE.Array texts -> Right <$> AE.parseJSON value -- parses an array of Text
    _ -> fail "Expected either a single string or an array of strings"


-- | Walk the JSON once, redact any fields which are in the list of json paths to be redacted.
--
-- >>> redactJSON (V.fromList ["menu.id."]) [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "John")]))])
--
-- >>> redactJSON (V.fromList ["menu.id"]) [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "John")]))])
--
-- >>> redactJSON (V.fromList ["menu.id", "menu.name"]) [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "[REDACTED]")]))])
--
-- >>> redactJSON (V.fromList ["menu.[].id", "menu.[].names.[]"]) [aesonQQ| {"menu":[{"id":"i1", "names":["John","okon"]}, {"id":"i2"}]} |]
-- Object (fromList [("menu",Array [Object (fromList [("id",String "[REDACTED]"),("names",Array [String "[REDACTED]",String "[REDACTED]"])]),Object (fromList [("id",String "[REDACTED]")])])])
redactJSON :: V.Vector Text -> AE.Value -> AE.Value
redactJSON paths' = redactJSON' (V.map stripPrefixDot paths')
  where
    redactJSON' !paths value = case value of
      AET.String v -> if "" `V.elem` paths then AET.String "[REDACTED]" else AET.String v
      AET.Number v -> if "" `V.elem` paths then AET.String "[REDACTED]" else AET.Number v
      AET.Null -> AET.Null
      AET.Bool v -> AET.Bool v
      AET.Object objMap -> AET.Object $ AEKM.fromHashMapText $ HM.mapWithKey (\k v -> redactJSON' (V.mapMaybe (\path -> T.stripPrefix (k <> ".") path <|> T.stripPrefix k path) paths) v) (AEKM.toHashMapText objMap)
      AET.Array jsonList -> AET.Array $ V.map (redactJSON' (V.mapMaybe (\path -> T.stripPrefix "[]." path <|> T.stripPrefix "[]" path) paths)) jsonList

    stripPrefixDot !p = fromMaybe p (T.stripPrefix "." p)


replaceNullChars :: Text -> Text
replaceNullChars = T.replace "\\u0000" ""


sortVector :: Ord a => V.Vector a -> V.Vector a
sortVector vec = runST $ do
  mvec <- V.thaw vec
  VA.sort mvec
  V.freeze mvec


-- valueToFields takes an aeson object and converts it into a vector of paths to
-- each primitive value in the json and the values.
--
-- Regular nested text fields:
-- >>> valueToFields [aesonQQ|{"menu":{"id":"text"}}|]
-- V.fromList [(".menu.id", V.fromList [String "text"])]
--
-- Integer nested field within an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":[{"int_field":22}]}}|]
-- V.fromList [(".menu.id[*].int_field", V.fromList [Number 22.0])]
--
-- Deeper nested field with an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":{"menuitems":[{"key":"value"}]}}}|]
-- V.fromList [(".menu.id.menuitems[*].key", V.fromList [String "value"])]
--
-- Flat array value:
-- >>> valueToFields [aesonQQ|{"menu":["abc", "xyz"]}|]
-- V.fromList [(".menu[*]", V.fromList [String "abc", String "xyz"])]
--
-- Float values and Null
-- >>> valueToFields [aesonQQ|{"fl":1.234, "nl": null}|]
-- V.fromList [(".fl", V.fromList [Number 1.234]), (".nl", V.fromList [Null])]
--
-- Multiple fields with same key via array:
-- >>> valueToFields [aesonQQ|{"menu":[{"id":"text"},{"id":123}]}|]
-- V.fromList [(".menu[*].id", V.fromList [String "text", Number 123.0])]
--
-- >>> valueToFields [aesonQQ|{"menu":[{"c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd":"text"},{"id":123}]}|]
-- V.fromList [(".menu[*].id", V.fromList [Number 123.0]), (".menu[*].{uuid}", V.fromList [String "text"])]
--
-- FIXME: value To Fields should use the redact fields list to actually redact fields
valueToFields :: AE.Value -> V.Vector (Text, V.Vector AE.Value)
valueToFields value = dedupFields $ removeBlacklistedFields $ snd $ valueToFields' value ("", V.empty)
  where
    valueToFields' :: AE.Value -> (Text, V.Vector (Text, AE.Value)) -> (Text, V.Vector (Text, AE.Value))
    valueToFields' (AE.Object v) (!prefix, !acc) =
      HM.foldlWithKey' folder (prefix, acc) (AEKM.toHashMapText v)
      where
        folder (!akkT, !akkL) k val =
          let newPrefix = if T.null akkT then normalizeKey k else akkT <> "." <> normalizeKey k
              (_, newAkkL) = valueToFields' val (newPrefix, akkL)
           in (akkT, newAkkL)
    valueToFields' (AE.Array v) (!prefix, !acc) =
      V.foldl' folder (prefix, acc) v
      where
        folder (!akkT, !akkL) val =
          let (_, newAkkL) = valueToFields' val (akkT <> "[*]", akkL)
           in (akkT, newAkkL)
    valueToFields' v (!prefix, !acc) = (prefix, V.cons (prefix, v) acc)

    normalizeKey :: Text -> Text
    normalizeKey k =
      let formatted = replaceAllFormats k
       in if formatted == k
            then fromMaybe k (valueToFormatStr k)
            else formatted


-- | Merge all fields in the vector of tuples by the first item in the tuple.
--
-- >>> dedupFields (V.fromList [(".menu[*]", AE.String "xyz"),(".menu[*]", AE.String "abc")])
-- [(".menu[*]",[String "abc",String "xyz"])]
--
-- >>> dedupFields (V.fromList [(".menu.[*]", AE.String "xyz"),(".menu.[*]", AE.String "abc"),(".menu.[*]", AE.Number 123)])
-- [(".menu.[*]",[Number 123.0,String "abc",String "xyz"])]
--
-- >>> dedupFields (V.fromList [(".menu.[*].a", AE.String "xyz"),(".menu.[*].b", AE.String "abc"),(".menu.[*].a", AE.Number 123)])
-- [(".menu.[*].b",[String "abc"]),(".menu.[*].a",[Number 123.0,String "xyz"])]
dedupFields :: V.Vector (Text, AE.Value) -> V.Vector (Text, V.Vector AE.Value)
dedupFields fields = runST $ do
  -- Create a mutable hash table
  hashTable <- HT.newSized (V.length fields) :: ST s (HT.HashTable s Text (V.Vector AE.Value))

  -- Populate the hash table
  forM_ fields $ \(k, v) -> do
    existing <- HT.lookup hashTable k
    let newVec = v `V.cons` fromMaybe V.empty existing
    HT.insert hashTable k newVec

  -- Convert the hash table to a list of immutable vectors
  pairs <- HTC.toList hashTable
  V.thaw (V.fromList pairs) >>= V.freeze


-- >>> removeBlacklistedFields [(".menu.password",String "xyz"),(".authorization",String "abc")]
-- [(".menu.password",String "[REDACTED]"),(".authorization",String "[REDACTED]")]
--
-- >>> removeBlacklistedFields [(".menu.password",Null),(".regular",String "abc")]
-- [(".menu.password",String "[REDACTED]"),(".regular",String "abc")]
removeBlacklistedFields :: V.Vector (Text, AE.Value) -> V.Vector (Text, AE.Value)
removeBlacklistedFields = V.map \(k, val) ->
  if or @[]
    [ T.isSuffixOf "password" (T.toLower k)
    , T.isSuffixOf "authorization" (T.toLower k)
    , T.isSuffixOf "cookie" (T.toLower k)
    ]
    then (k, AE.String "[REDACTED]")
    else (k, val)


-- >>> valueToFormat (AET.String "22")
-- "integer"
--
-- >>> valueToFormat (AET.String "22.33")
-- "float"
--
-- >>> valueToFormat (AET.String "22/02/2022")
-- "mm/dd/yyyy"
--
valueToFormat :: AE.Value -> Text
valueToFormat (AET.String val) = case valueToFormatStr val of
  Just fmt -> T.drop 1 $ T.dropEnd 1 fmt -- Remove the curly braces
  Nothing -> "text"
valueToFormat (AET.Number val) = valueToFormatNum val
valueToFormat (AET.Bool _) = "bool"
valueToFormat AET.Null = "null"
valueToFormat (AET.Object _) = "object"
valueToFormat (AET.Array _) = "array"


-- | Common format patterns used by both replaceAllFormats and valueToFormatStr
-- The order matters: more specific patterns should come before more general ones
commonFormatPatterns :: [(RE, Text)]
commonFormatPatterns =
  [ -- UUIDs and hashes (most specific hex patterns first)
    ([re|^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$|], "{uuid}")
  , ([re|^[0-9a-fA-F]{24}$|], "{uuid}") -- Keep as uuid for backward compatibility
  , ([re|^[a-fA-F0-9]{64}$|], "{sha256}")
  , ([re|^[a-fA-F0-9]{40}$|], "{sha1}")
  , ([re|^[a-fA-F0-9]{32}$|], "{md5}")
  , -- Financial (moved up before hex_id to catch credit cards first)
    ([re|^4[0-9]{15}$|], "{credit_card}") -- Visa 16 digits
  , ([re|^4[0-9]{12}$|], "{credit_card}") -- Visa 13 digits
  , ([re|^5[1-5][0-9]{14}$|], "{credit_card}") -- Mastercard
  , ([re|^3[47][0-9]{13}$|], "{credit_card}") -- Amex
  , ([re|^[0-9A-Fa-f]{14,20}$|], "{hex_id}") -- Match hex-like IDs that aren't UUIDs
  -- Authentication & encoding
  , ([re|^eyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+$|], "{jwt}")
  , -- IBAN (moved before base64)
    ([re|^[A-Z]{2}[0-9]{2}[A-Za-z0-9]{4}[0-9]{7}[A-Za-z0-9]{0,16}$|], "{iban}")
  , ([re|^[A-Za-z0-9+/]{20,}={0,2}$|], "{base64}")
  , -- Date patterns (before file paths to avoid conflicts)
    ([re|^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), [0-9]{1,2} (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} [+\-][0-9]{4}$|], "{rfc2822}")
  , ([re|^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?(Z|[+\-][0-9]{2}:[0-9]{2})?$|], "{YYYY-MM-DDThh:mm:ss.sTZD}") -- ISO 8601
  , ([re|^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{YYYY-MM-DD HH:MM:SS}") -- MySQL datetime
  , ([re|^[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{MM/DD/YYYY HH:MM:SS}") -- US datetime
  , ([re|^[0-9]{2}-[0-9]{2}-[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{MM-DD-YYYY HH:MM:SS}")
  , ([re|^[0-9]{2}\.[0-9]{2}\.[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{DD.MM.YYYY HH:MM:SS}") -- European datetime
  , ([re|^[0-9]{4}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{YYYY/MM/DD HH:MM:SS}") -- Japanese datetime
  , ([re|^(0[1-9]|[12][0-9]|3[01])[/](0[1-9]|1[012])[/](19|20)[0-9][0-9]$|], "{dd/mm/yyyy}") -- European date
  , ([re|^(0[1-9]|[12][0-9]|3[01])-(0[1-9]|1[012])-(19|20)[0-9][0-9]$|], "{dd-mm-yyyy}")
  , ([re|^(0[1-9]|[12][0-9]|3[01])\.(0[1-9]|1[012])\.(19|20)[0-9][0-9]$|], "{dd.mm.yyyy}")
  , ([re|^(0[1-9]|1[012])[/](0[1-9]|[12][0-9]|3[01])[/](19|20)[0-9][0-9]$|], "{mm/dd/yyyy}") -- US date
  , ([re|^(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])-(19|20)[0-9][0-9]$|], "{mm-dd-yyyy}")
  , ([re|^(0[1-9]|1[012])\.(0[1-9]|[12][0-9]|3[01])\.(19|20)[0-9][0-9]$|], "{mm.dd.yyyy}")
  , ([re|^[0-9]{4}-[0-9]{2}-[0-9]{2}$|], "{YYYY-MM-DD}") -- ISO date
  , ([re|^[0-9]{4}/[0-9]{2}/[0-9]{2}$|], "{YYYY/MM/DD}") -- Japanese date
  , ([re|^[0-9]{8}$|], "{YYYYMMDD}") -- Compact date
  , ([re|^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]{1,2}, [0-9]{4}$|], "{Mon DD, YYYY}") -- Long month
  , ([re|^[0-9]{1,2}-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-[0-9]{4}$|], "{DD-Mon-YYYY}") -- Oracle date
  -- Time patterns
  , ([re|^[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}$|], "{HH:MM:SS.mmm}") -- Time with milliseconds
  , ([re|^[0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{HH:MM:SS}") -- Time only
  , ([re|^[0-9]{1,2}:[0-9]{2} (AM|PM|am|pm)$|], "{H:MM AM/PM}") -- 12-hour time
  -- Personal identifiers
  , ([re|^[0-9]{3}-[0-9]{2}-[0-9]{4}$|], "{ssn}")
  , ([re|^\+1 \([0-9]{3}\) [0-9]{3}-[0-9]{4}$|], "{phone}")
  , -- Network patterns
    ([re|^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)/([0-9]|[12][0-9]|3[0-2])$|], "{cidr}")
  , ([re|^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$|], "{ipv4}")
  , ([re|^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$|], "{email}")
  , ([re|^https?://[^\s]+$|], "{url}")
  , ([re|^([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4}$|], "{ipv6}")
  , ([re|^([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2}$|], "{mac}")
  , ([re|^:[0-9]{1,5}$|], "{port}")
  , ([re|^[A-Za-z0-9][A-Za-z0-9.-]*\.[A-Za-z]{2,}$|], "{hostname}")
  , -- File paths (after dates to avoid conflicts)
    ([re|^[A-Za-z]:\\\\.*$|], "{file_path}") -- Windows path with double backslash
  , ([re|^[A-Za-z]:\\.*$|], "{file_path}") -- Windows path with single backslash
  , ([re|^/[A-Za-z0-9._/-]+$|], "{file_path}") -- Unix path
  , -- Timestamps
    ([re|^1[0-9]{12}$|], "{epoch_ms}")
  , ([re|^1[0-9]{9}$|], "{epoch_s}")
  , -- Process/thread identifiers
    ([re|^pid[:=]?[0-9]+$|], "{pid}")
  , ([re|^tid[:=]?[0-9]+$|], "{tid}")
  , ([re|^Thread-[0-9]+$|], "{thread}")
  , ([re|^session_[A-Za-z0-9\-]{8,}$|], "{session_id}")
  , -- Numbers (last, as they're most general)
    ([re|^[1-5][0-9]{2}$|], "{http_status}")
  , ([re|^0x[0-9A-Fa-f]+$|], "{hex}")
  , ([re|^[+-]?[0-9]+\.[0-9]+$|], "{float}")
  , ([re|^[0-9]+$|], "{integer}")
  ]


-- valueToFormatStr will take a string and try to find a format which matches that string best.
-- At the moment it takes a text and returns a generic mask that represents the format of that text
--
-- >>> map valueToFormatStr ["22/02/2022", "20-02-2022", "22.02.2022", "2023-10-14", "2023/10/14", "20231014"]
-- [Just "{dd/mm/yyyy}",Just "{dd-mm-yyyy}",Just "{dd.mm.yyyy}",Just "{YYYY-MM-DD}",Just "{YYYY/MM/DD}",Just "{YYYYMMDD}"]
--
-- >>> map valueToFormatStr ["Oct 14, 2023", "14-Oct-2023", "2023-10-14T10:29:38.64522Z", "2023-10-14 10:29:38"]
-- [Just "{Mon DD, YYYY}",Just "{DD-Mon-YYYY}",Just "{YYYY-MM-DDThh:mm:ss.sTZD}",Just "{YYYY-MM-DD HH:MM:SS}"]
--
-- >>> map valueToFormatStr ["15:30:45", "15:30:45.123", "3:30 PM", "Mon, 14 Oct 2023 10:30:00 +0000"]
-- [Just "{HH:MM:SS}",Just "{HH:MM:SS.mmm}",Just "{H:MM AM/PM}",Just "{rfc2822}"]
--
-- >>> map valueToFormatStr ["c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd", "507f1f77bcf86cd799439011", "abc123def456789"]
-- [Just "{uuid}",Just "{uuid}",Nothing]
--
-- >>> map valueToFormatStr ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", "356a192b7913b04c54574d18c28d46e6395428ab", "5d41402abc4b2a76b9719d911017c592"]
-- [Just "{sha256}",Just "{sha1}",Just "{md5}"]
--
-- >>> map valueToFormatStr ["123", "123.456", "-123.456", "0xDEADBEEF", "1634567890", "1634567890123"]
-- [Just "{integer}",Just "{float}",Just "{float}",Just "{hex}",Just "{epoch_s}",Just "{epoch_ms}"]
--
-- >>> map valueToFormatStr ["192.168.1.1", "192.168.1.0/24", "2001:0db8:85a3:0000:0000:8a2e:0370:7334", "00:1B:44:11:3A:B7"]
-- [Just "{ipv4}",Just "{cidr}",Just "{ipv6}",Just "{mac}"]
--
-- >>> map valueToFormatStr [":8080", "user@example.com", "https://api.example.com/v1/users", "api.example.com"]
-- [Just "{port}",Just "{email}",Just "{url}",Just "{hostname}"]
--
-- >>> map valueToFormatStr ["eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c", "dGVzdCBiYXNlNjQgZW5jb2Rpbmc="]
-- [Just "{jwt}",Just "{base64}"]
--
-- >>> map valueToFormatStr ["/usr/local/bin/script.sh", "C:\\Users\\Admin\\Documents", "pid:12345", "tid:67890"]
-- [Just "{file_path}",Just "{file_path}",Just "{pid}",Just "{tid}"]
--
-- >>> map valueToFormatStr ["Thread-42", "session_abc123def456", "4111111111111111", "GB82WEST12345698765432"]
-- [Just "{thread}",Just "{session_id}",Just "{credit_card}",Just "{iban}"]
--
-- >>> map valueToFormatStr ["123-45-6789", "+1 (555) 123-4567", "200", "404", "500"]
-- [Just "{ssn}",Just "{phone}",Just "{http_status}",Just "{http_status}",Just "{http_status}"]
--
-- >>> map valueToFormatStr ["plain text", "", "hello world", "test123test"]
-- [Nothing,Nothing,Nothing,Nothing]
--
-- >>> map valueToFormatStr ["v1"]
--
valueToFormatStr :: Text -> Maybe Text
valueToFormatStr val = checkFormats formatChecks
  where
    checkFormats :: [(RE, Text)] -> Maybe Text
    checkFormats [] = Nothing
    checkFormats ((regex, format) : rest) =
      if matched (val ?=~ regex)
        then Just format
        else checkFormats rest

    -- Use exact match patterns first, then fallback to common patterns
    formatChecks :: [(RE, Text)]
    formatChecks = commonFormatPatterns


ensureUrlParams :: Text -> (Text, AE.Value, Bool)
ensureUrlParams "" = ("", AE.object [], False)
ensureUrlParams url = (parsedUrl, pathParams, hasDyn)
  where
    (segs, vals) = parseUrlSegments (T.splitOn "/" url) ([], [])
    parsedUrl = T.intercalate "/" segs
    dynSegs = filter (T.isPrefixOf "{") segs
    hasDyn = not $ null dynSegs
    pathParams = buildPathParams dynSegs vals (AE.object [])


parseUrlSegments :: [Text] -> ([Text], [Text]) -> ([Text], [Text])
parseUrlSegments [] parsed = parsed
parseUrlSegments (x : xs) (segs, vals) = case valueToFormatStr x of
  Nothing -> parseUrlSegments xs (segs ++ [x], vals)
  Just v
    | v == "{uuid}" -> parseUrlSegments xs (addNewSegment segs "uuid", vals ++ [x])
    | v
        `elem` [ "{mm/dd/yyyy}"
               , "{mm-dd-yyyy}"
               , "{mm.dd.yyyy}"
               , "{dd/mm/yyyy}"
               , "{dd-mm-yyyy}"
               , "{dd.mm.yyyy}"
               , "{YYYY-MM-DD}"
               , "{YYYY/MM/DD}"
               , "{YYYYMMDD}"
               , "{YYYY-MM-DDThh:mm:ss.sTZD}"
               ] ->
        parseUrlSegments xs (addNewSegment segs "date", vals ++ [x])
    | v `elem` ["{ip}", "{ipv6}"] -> parseUrlSegments xs (addNewSegment segs "ip_address", vals ++ [x])
    | v `elem` ["{integer}", "{float}", "{hex}"] -> parseUrlSegments xs (addNewSegment segs "number", vals ++ [x])
    | otherwise -> parseUrlSegments xs (addNewSegment segs "param", vals ++ [x])


addNewSegment :: [Text] -> Text -> [Text]
addNewSegment segs seg = newSegs
  where
    catFilter = filter (T.isPrefixOf ("{" <> seg)) segs
    pos = length catFilter
    newSeg = if pos > 0 then "{" <> seg <> "_" <> show pos <> "}" else "{" <> seg <> "}"
    newSegs = segs ++ [newSeg]


buildPathParams :: [Text] -> [Text] -> AE.Value -> AE.Value
buildPathParams [] _ acc = acc
buildPathParams _ [] acc = acc
buildPathParams (x : xs) (v : vs) acc = buildPathParams xs vs param
  where
    current = AE.object [AEK.fromText (T.tail x) AE..= v]
    param = case (acc, current) of
      (AE.Object a, AE.Object b) -> AE.Object $ a <> b
      _ -> acc


-- >>> valueToFormatNum 22.3
-- "float"
-- >>> valueToFormatNum 22
-- "integer"
valueToFormatNum :: Scientific.Scientific -> Text
valueToFormatNum val
  | Scientific.isFloating val = "float"
  | Scientific.isInteger val = "integer"
  | otherwise = "unknown"


-- fieldsToFieldDTO processes a field from monoscope clients into a field and format record,
-- which can then be converted into separate sql insert queries.
fieldsToFieldDTO :: Fields.FieldCategoryEnum -> Projects.ProjectId -> Text -> (Text, V.Vector AE.Value) -> (Fields.Field, Fields.Format)
fieldsToFieldDTO fieldCategory projectID endpointHash (keyPath, val) =
  ( Fields.Field
      { createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      , updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      , id = Fields.FieldId UUID.nil
      , endpointHash = endpointHash
      , projectId = projectID
      , key = snd $ T.breakOnEnd "." keyPath
      , -- FIXME: We're discarding the field values of the others, if theer was more than 1 value.
        -- FIXME: We should instead take all the fields into consideration
        -- FIXME: when generating the field types and formats
        fieldType = fieldType
      , fieldTypeOverride = Nothing
      , format = format
      , formatOverride = Nothing
      , description = ""
      , keyPath = keyPath
      , fieldCategory = fieldCategory
      , hash = fieldHash
      , isEnum = False
      , isRequired = False
      }
  , Fields.Format
      { id = UUIDId UUID.nil
      , createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      , updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      , projectId = projectID
      , fieldHash = fieldHash
      , fieldType = fieldType
      , fieldFormat = format
      , -- NOTE: A trailing question, is whether to store examples into a separate table.
        -- It requires some more of a cost benefit analysis.
        examples = val
      , hash = formatHash
      }
  )
  where
    aeValueToFieldType :: AE.Value -> Fields.FieldTypes
    aeValueToFieldType (AET.String _) = Fields.FTString
    aeValueToFieldType (AET.Number _) = Fields.FTNumber
    aeValueToFieldType AET.Null = Fields.FTNull
    aeValueToFieldType (AET.Bool _) = Fields.FTBool
    aeValueToFieldType (AET.Object _) = Fields.FTObject
    aeValueToFieldType (AET.Array _) = Fields.FTList

    fieldType :: Fields.FieldTypes
    fieldType = fromMaybe Fields.FTUnknown $ V.map aeValueToFieldType val V.!? 0

    -- field hash is <hash of the endpoint> + <the hash of <field_category><key_path_str><field_type>> (No space or comma between data)
    !fieldHash = endpointHash <> toXXHash (display fieldCategory <> keyPath)
    -- FIXME: We should rethink this value to format logic.
    -- FIXME: Maybe it actually needs machine learning,
    -- FIXME: or maybe it should operate on the entire list, and not just one value.
    format = fromMaybe "" $ V.map valueToFormat val V.!? 0
    !formatHash = fieldHash <> toXXHash format
