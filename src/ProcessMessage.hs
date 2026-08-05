{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- processMessages uses @pure $!@ to force the (ackId, raw, span) tuple to
-- WHNF immediately, preventing thunk accumulation across forM. hlint flags
-- it as "Redundant $!" but it's load-bearing here.
{-# HLINT ignore "Redundant $!" #-}

module ProcessMessage (
  processMessages,
  processSpanToEntities,
  stampHashesAtIngest,
  extractObservation,
  RequestMessage (..),
  valueToFormatStr,
  valueToFields,
  redactJSON,
  ensureUrlParams,
  dedupFields,
  isUrlIdLike,
  matchCanonicalPath,
  pathMatchesTemplate,
  parseCanonicalPaths,
  tokenizeUrlPath,
  hostFromRawUrl,
)
where

import Control.Lens ((^?))
import Control.Monad.ST (ST, runST)
import Data.Aeson qualified as AE
import Data.Aeson.Extra (lodashMerge)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens (key, _Number, _Object, _String)
import Data.Aeson.Types qualified as AE
import Data.ByteString qualified as BS
import Data.Cache qualified as Cache
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isUpper)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID (UUIDEff)
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HM
import Data.HashTable.Class qualified as HTC
import Data.HashTable.ST.Cuckoo qualified as HT
import Data.Text qualified as T
import Data.Time (addUTCTime, zonedTimeToUTC)
import Data.Time.LocalTime (ZonedTime)
import Data.Vector qualified as V
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled (..))
import Effectful.Log (Log)
import Effectful.Reader.Static qualified as Eff
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (Context (trace_state), OtelLogsAndSpans (..), generateSummary)
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (AesonText (..), UUIDId (..), unAesonTextMaybe)
import Pkg.Metrics qualified as Metrics
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot qualified as SchemaHot
import Relude hiding (ask)
import Relude.Extra.Tuple (toSnd)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Tracing (Tracing, batchSpanAttrs, withSpan_)
import System.Types (DB)
import Text.RE.Replace (matched)
import Text.RE.TDFA (RE, re, (?=~))
import Utils (b64ToJson, freeTierDailyMaxEvents, jsonToMap, nestedJsonFromDotNotation, replaceAllFormats, toXXHash)


processMessages
  :: (Concurrent :> es, DB es, Eff.Reader AuthContext :> es, Ki.StructuredConcurrency :> es, Labeled "timefusion" Hasql.Hasql :> es, Log :> es, Tracing :> es, UUIDEff :> es)
  => [(Text, ByteString)]
  -> HM.HashMap Text Text
  -> Eff es (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg]))
processMessages [] _ = pure (Right ([], []))
processMessages msgs attrs =
  withSpan_ "pubsub.process_messages" (batchSpanAttrs (length msgs) attrs) do
    appCtx <- Eff.ask @AuthContext
    (rAckIds, poison, mWrite) <- Metrics.timed Metrics.ingestDecodeHist [] do
      let decoded =
            [ (ackId, msg, AE.eitherDecodeStrict (BS.filter (/= 0) msg))
            | (ackId, msg) <- msgs
            ]
          rMsgs = [(ackId, msg, m) | (ackId, msg, Right m) <- decoded]
          poison = [(ackId, msg, toText err) | (ackId, msg, Left err) <- decoded]
      forM_ poison \(ackId, msg, err) ->
        Log.logAttention "Error parsing json msgs" (AE.object ["AckId" AE..= ackId, "Error" AE..= err, "OriginalMsg" AE..= decodeUtf8 @Text msg])
      if null rMsgs
        then pure ([], poison, Nothing)
        else do
          projectCaches <-
            liftIO $ HM.fromList <$> forM (ordNub $ (\(_, _, m) -> UUIDId m.projectId) <$> rMsgs) \pid ->
              (pid,)
                <$> Cache.fetchWithCache appCtx.projectCache pid (fmap (fromMaybe Projects.defaultProjectCache) . Projects.projectCacheByIdIO appCtx.hasqlJobsPool)

          -- Track (ackId, raw) alongside each emitted span so we can map any
          -- per-row write poison back to the source message for DLQ routing.
          paired <- forM rMsgs \(ackId, raw, msg) -> runMaybeT do
            let pid = UUIDId msg.projectId
                !msgSize = fromIntegral (BS.length raw)
            cache <- hoistMaybe $ HM.lookup pid projectCaches
            let !totalDailyEvents = fromIntegral cache.dailyEventCount + fromIntegral cache.dailyMetricCount
                !isFreeTier = Projects.isFreeTier cache.paymentPlan
            guard $ not (isFreeTier && totalDailyEvents >= freeTierDailyMaxEvents)
            spanId <- lift UUID.genUUID
            trId <- lift $ UUID.toText <$> UUID.genUUID
            pure $! (ackId, raw, convertRequestMessageToSpan msg msgSize (spanId, trId))

          pure (map (\(a, _, _) -> a) rMsgs, poison, Just (projectCaches, V.fromList (catMaybes paired)))

    writeRes <- case mWrite of
      Nothing -> pure (Right ())
      Just (projectCaches, paired) ->
        Metrics.timed Metrics.ingestWriteHist []
          $ Telemetry.insertAndHandOff appCtx.hasqlTimefusionUsesPgTypes (Telemetry.writeTargetFor appCtx.env.enablePostgresTelemetryWrites appCtx.env.enableTimefusionWrites Nothing) appCtx.extractionWorker projectCaches (stampHashesAtIngest projectCaches $ V.map (\(_, _, s) -> s) paired)

    -- On success the whole batch landed on every store (no per-row poison); the
    -- only DLQ entries are the decode failures collected upstream in `poison`.
    pure $ writeRes $> (rAckIds, poison)


-- | Shared HTTP-key derivation. Used by both 'processSpanToEntities' (which
-- writes 'apis.endpoints') and 'extractObservation' (which feeds the
-- schema-learning catalog) so the endpoint hash and url-path canonicalisation
-- match byte-for-byte. Drift here is the root cause of "UNKNOWN /"
-- new-endpoint notifications: when these two functions disagree on the
-- canonical path, the schema-learning anomaly's @target_hash@ never matches
-- @apis.endpoints.hash@ and the join in 'Anomalies.getAnomaliesVM' returns
-- NULL for method/path.
data HttpKey = HttpKey
  { method :: !Text
  , host :: !Text
  , urlPath :: !Text
  -- ^ canonical path: SDK-normalised, dyn-param-rewritten, canonical-template-matched.
  , statusCode :: !Int
  , isHttpSpan :: !Bool
  }


httpKeyOf :: HM.HashMap (Text, Text) [([Text], Text)] -> Telemetry.OtelLogsAndSpans -> HttpKey
httpKeyOf canonicalTemplates otelSpan =
  let !attributes = maybeToMonoid (unAesonTextMaybe otelSpan.attributes)
      !attrValue = AE.Object $ AEKM.fromMapText attributes
      !isHttpSpan = isJust $ attrValue ^? key "http" . key "request" . key "method" . _String
      !method = T.toUpper $ fromMaybe "GET" $ attrValue ^? key "http" . key "request" . key "method" . _String
      !routePath =
        fromMaybe "/"
          $ (attrValue ^? key "http" . key "route" . _String)
          <|> (attrValue ^? key "url" . key "path" . _String)
      !statusCode =
        fromMaybe 200
          $ ( (attrValue ^? key "http" . key "response" . key "status_code" . _String >>= readMaybe @Int . toString)
                <|> (truncate <$> attrValue ^? key "http" . key "response" . key "status_code" . _Number)
            )
          >>= guarded (\c -> c >= 100 && c < 600)
      !host =
        fromMaybe ""
          $ (attrValue ^? key "net" . key "host" . key "name" . _String)
          <|> (attrValue ^? key "server" . key "address" . _String)
          <|> (attrValue ^? key "http" . key "host" . _String)
      !sdkTypeStr =
        fromMaybe "unknown"
          $ (attrValue ^? key "monoscope" . key "sdk_type" . _String)
          <|> (attrValue ^? key "apitoolkit" . key "sdk_type" . _String)
      !sdkType = fromMaybe LogQueries.SDKUnknown $ readMaybe $ toString sdkTypeStr
      !urlPath' = LogQueries.normalizeUrlPath sdkType statusCode method routePath
      !(!urlPathDyn, !_pathParamsDyn, !hasDyn) = ensureUrlParams urlPath'
      !urlPath =
        if hasDyn
          then urlPathDyn
          else fromMaybe urlPath' $ matchCanonicalPath canonicalTemplates method host urlPath'
   in HttpKey{method, host, urlPath, statusCode, isHttpSpan}


-- | Extract entities for hash-stamping. Returns @(mkEndpoint, hashes, normalizedPath)@ —
-- the endpoint is returned as a function of its row id so hash-only callers
-- ('stampHashesAtIngest') never have to conjure a placeholder UUID;
-- the normalized path (@Just@ for HTTP spans) is stamped back onto the span's
-- @attributes.http.route@ and @attributes.url.path@ by the caller so explorer
-- queries match the template stored in @apis.endpoints@. Schema learning flows
-- through 'extractObservation' instead; this only does endpoint discovery.
--
-- The owning 'ProjectId' is threaded in already-parsed (the batch is grouped by project
-- upstream), so we never re-parse the untyped @otelSpan.project_id@ here — that parse used
-- to be a partial @fromJust@ that crashed the whole ingestion batch on one malformed id.
processSpanToEntities :: HM.HashMap (Text, Text) [([Text], Text)] -> Projects.ProjectCache -> Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> (UUID.UUID -> Maybe Endpoints.Endpoint, V.Vector Text, Maybe Text)
processSpanToEntities canonicalTemplates pjc projectId otelSpan =
  let !attributes = maybeToMonoid (unAesonTextMaybe otelSpan.attributes)
      HttpKey{method, host, urlPath, statusCode, isHttpSpan} = httpKeyOf canonicalTemplates otelSpan

      -- Resolve service and environment from OTel resource attrs, falling back to span
      -- attributes for SDKs that put them there.
      !resourceMap = unAesonTextMaybe otelSpan.resource
      lookupAttr k = Telemetry.atMapText k resourceMap <|> Telemetry.atMapText k (Just attributes)
      !serviceName = lookupAttr "service.name"
      !environment = asum $ lookupAttr <$> ["deployment.environment.name", "deployment.environment", "service.namespace", "k8s.namespace.name"]

      -- Generate endpoint hash - this uniquely identifies an API endpoint.
      !endpointHash = toXXHash $ projectId.toText <> host <> method <> urlPath

      endpoint dumpId =
        if endpointHash `elem` pjc.endpointHashes || statusCode == 404
          then Nothing
          else
            Just
              $ Endpoints.Endpoint
                { createdAt = otelSpan.timestamp
                , updatedAt = otelSpan.timestamp
                , id = UUIDId dumpId
                , projectId
                , urlPath
                , urlParams = AE.emptyObject
                , method
                , host
                , hash = endpointHash
                , outgoing = otelSpan.kind == Just "client"
                , description = ""
                , serviceName
                , environment
                }
   in -- One stamped hash per span (the endpoint hash) — field/shape hashes moved to
      -- the schema-learning catalog. The normalized path goes back into both
      -- attributes.http.route and attributes.url.path so notification links and the
      -- catalog UI filter by the same template stored in apis.endpoints.
      (endpoint, V.singleton endpointHash, if isHttpSpan then Just urlPath else Nothing)


-- | Stamp each record's hashes (endpoint identity + error fingerprints) before the
-- insert. The extraction worker re-derives the identical set (same pure functions,
-- same project-cache snapshot handed off with the batch) and skips its hash-merge
-- UPDATE for covered rows — on TimeFusion's merge-on-read store that UPDATE minted
-- a duplicate version of every ingested row, doubling every scan until compaction.
-- Rows whose project cache is absent pass through unstamped; the worker's UPDATE
-- path still covers them.
stampHashesAtIngest :: HM.HashMap Projects.ProjectId Projects.ProjectCache -> V.Vector Telemetry.OtelLogsAndSpans -> V.Vector Telemetry.OtelLogsAndSpans
stampHashesAtIngest caches = V.map \r -> fromMaybe r do
  pid <- Projects.projectIdFromText r.project_id
  pjc <- HM.lookup pid caches
  let templates = HM.lookupDefault HM.empty pid templatesByPid
      (_, spanHashes, _) = processSpanToEntities templates pjc pid r
      errHashes = V.map (\e -> "err:" <> e.hash) (Telemetry.getAllATErrors (V.singleton r))
  pure r{hashes = Just $ spanHashes <> errHashes}
  where
    templatesByPid = HM.map (parseCanonicalPaths . (.canonicalPaths)) caches


-- | Build a 'SchemaHot.ObservationInput' for the schema-learning catalog.
-- Covers HTTP and non-HTTP spans uniformly; the keying tuple distinguishes
-- them. Walks @attributes ∪ resource ∪ body ∪ events@ with redaction
-- applied so PII never enters the catalog.
extractObservation
  :: HM.HashMap (Text, Text) [([Text], Text)]
  -> OtelLogsAndSpans
  -> SchemaHot.ObservationInput
extractObservation canonicalTemplates otelSpan =
  let !projectIdText = otelSpan.project_id
      !attrMap = maybeToMonoid (unAesonTextMaybe otelSpan.attributes)
      !resMap = maybeToMonoid (unAesonTextMaybe otelSpan.resource)
      !attrValue = AE.Object $ AEKM.fromMapText attrMap
      -- Hard-coded ingestion-time redaction. Per-project rules were removed
      -- with `projects.redacted_fields`; the management UI is gone.
      !redacted = redactJSON $ V.fromList [".set-cookie", ".password"]

      -- Shared HTTP-key derivation with 'processSpanToEntities'. Must stay
      -- byte-identical or the schema-learning AKEndpoint target_hash won't
      -- match apis.endpoints.hash and the notification join goes NULL → "UNKNOWN /".
      HttpKey{method, host, urlPath = canonicalPath, statusCode, isHttpSpan} = httpKeyOf canonicalTemplates otelSpan

      !service = Telemetry.atMapText "service.name" (Just resMap) <|> Telemetry.atMapText "service.name" (Just attrMap)
      !spanName = otelSpan.name
      !spanKind = otelSpan.kind

      -- HTTP spans key on the endpoint hash; everything else on service|name|kind.
      !(keyKind, keyHash) =
        if isHttpSpan
          then (Catalog.HttpEndpoint, toXXHash $ projectIdText <> host <> method <> canonicalPath)
          else (Catalog.SpanIdentity, toXXHash $ T.intercalate "|" (projectIdText : map (fromMaybe "") [service, spanName, spanKind]))
      !scope =
        Catalog.Scope
          { Catalog.service = service
          , Catalog.spanName = spanName
          , Catalog.kind = spanKind
          , Catalog.host = guard isHttpSpan >> guarded (not . T.null) host
          , Catalog.method = method <$ guard isHttpSpan
          , Catalog.urlPath = canonicalPath <$ guard isHttpSpan
          , Catalog.statusCodes = if isHttpSpan && statusCode > 0 then V.singleton statusCode else V.empty
          }

      -- Walk every section of the span. For HTTP we keep the legacy
      -- categorisation (header/body/etc.); for non-HTTP we tag attributes
      -- and resource bag fields with FCAttribute / FCResource.
      !bodyValue = fromMaybe AE.Null (unAesonTextMaybe otelSpan.body)
      !eventsValue = fromMaybe AE.Null (unAesonTextMaybe otelSpan.events)

      -- Top-level flat columns: level/kind/name/status_code/status_message
      -- and the severity sub-record. Emitted bare so the facet adapter
      -- doesn't prefix them with @attributes.@ / @resource.@.
      !topLevelValue =
        AE.Object
          $ AEKM.fromList
          $ catMaybes
            [ ("name",) . AE.String <$> otelSpan.name
            , ("kind",) . AE.String <$> otelSpan.kind
            , ("level",) . AE.String <$> otelSpan.level
            , ("status_code",) . AE.String <$> otelSpan.status_code
            , ("status_message",) . AE.String <$> otelSpan.status_message
            , ("severity",) . AE.toJSON <$> otelSpan.severity
            ]

      -- OTel attribute + resource bags use flat dotted keys
      -- (e.g. @{"http.request.method": "GET"}@). 'valueToFields' walks
      -- object keys as single segments and runs 'normalizeKey' on each —
      -- which matches @http.request.method@ against the @{hostname}@
      -- regex and collapses it. Nest the bag by dots first so the walker
      -- sees @http → request → method@ and normalises segment-wise.
      nestObject v = case v of
        AE.Object km ->
          nestedJsonFromDotNotation
            [(AEK.toText k, val) | (k, val) <- AEKM.toList km]
        other -> other
      -- Lazy walk thunk — only forced inside the sample gate.
      -- HTTP branch *adds* specialised sub-walks; it does not replace the
      -- common attribute/resource walk (dropping the latter on HTTP spans
      -- was the bug behind empty facets — http.request.method etc. live
      -- in the top attribute bag, not in any sub-bucket).
      walkThunk () =
        let commonWalk =
              [ tagWalk Catalog.FCAttribute (valueToFields $ redacted $ nestObject attrValue)
              , tagWalk Catalog.FCResource (valueToFields $ redacted $ nestObject (AE.Object $ AEKM.fromMapText resMap))
              , tagWalk Catalog.FCEvent (valueToFields $ redacted eventsValue)
              , tagWalk Catalog.FCTopLevel (valueToFields topLevelValue)
              ]
         in if isHttpSpan
              then
                let pathParams = fromMaybe AE.emptyObject $ attrValue ^? key "http" . key "request" . key "path_params"
                    queryParams = fromMaybe AE.emptyObject $ attrValue ^? key "http" . key "request" . key "query_params"
                    reqHeaders = fromMaybe AE.emptyObject $ extractHeadersV "http.request.headers" attrValue
                    respHeaders = fromMaybe AE.emptyObject $ extractHeadersV "http.response.headers" attrValue
                    reqBody = redacted $ fromMaybe AE.Null $ bodyValue ^? key "request_body"
                    respBody = redacted $ fromMaybe AE.Null $ bodyValue ^? key "response_body"
                 in mconcat
                      $ commonWalk
                      <> [ tagWalk Catalog.FCPathParam (valueToFields $ redacted pathParams)
                         , tagWalk Catalog.FCQueryParam (valueToFields $ redacted queryParams)
                         , tagWalk Catalog.FCRequestHeader (valueToFields $ redacted reqHeaders)
                         , tagWalk Catalog.FCResponseHeader (valueToFields $ redacted respHeaders)
                         , tagWalk Catalog.FCRequestBody (valueToFields reqBody)
                         , tagWalk Catalog.FCResponseBody (valueToFields respBody)
                         ]
              else
                mconcat
                  $ commonWalk
                  <> [tagWalk Catalog.FCRequestBody (valueToFields $ redacted bodyValue)]
   in SchemaHot.ObservationInput{keyKind, keyHash, scope, walk = walkThunk, timestamp = otelSpan.timestamp}
  where
    extractHeadersV :: Text -> AE.Value -> Maybe AE.Value
    extractHeadersV prefix obj = do
      keyMap <- obj ^? _Object
      let headerPairs = mapMaybe (\(k, v) -> (,v) . AEK.fromText <$> T.stripPrefix (prefix <> ".") (AEK.toText k)) (AEKM.toList keyMap)
      AE.Object . AEKM.fromList <$> guarded (not . null) headerPairs

    -- Pair each value with its format hint and tag the whole walk with a
    -- field category. Format hint computation here is what costs us the
    -- regex sweep — only invoked on the slow-path full walk.
    tagWalk
      :: Catalog.FieldCategoryEnum
      -> V.Vector (Text, V.Vector AE.Value)
      -> [(Text, V.Vector (AE.Value, Maybe Text), Catalog.FieldCategoryEnum)]
    tagWalk cat fields0 =
      [ (path, V.map (toSnd formatHint) vs, cat)
      | (path, vs) <- V.toList fields0
      ]

    formatHint :: AE.Value -> Maybe Text
    formatHint (AE.String s) = valueToFormatStr s
    formatHint _ = Nothing


convertRequestMessageToSpan :: RequestMessage -> Int64 -> (UUID.UUID, Text) -> Telemetry.OtelLogsAndSpans
convertRequestMessageToSpan rm msgSize (spanId, trId) =
  let
    !parentId = rm.parentId >>= guarded (not . T.null) . UUID.toText

    otelSpan =
      Telemetry.OtelLogsAndSpans
        { id = UUID.toText spanId
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
        , status_message = Just $ bool "OK" "Error" (rm.statusCode >= 400)
        , status_code = Just $ show rm.statusCode
        , hashes = Just []
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
        , message_size_bytes = msgSize
        }
   in
    otelSpan{summary = generateSummary otelSpan}


createSpanAttributes :: RequestMessage -> AE.Value
createSpanAttributes rm =
  let baseAttrs =
        nestedJsonFromDotNotation
          [ ("net.host.name", AE.String $ fromMaybe "" rm.host)
          , ("server.address", AE.String serverAddress)
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
          , ("monoscope.errors", AE.String $ maybe "[]" (decodeUtf8 . AE.encode) rm.errors)
          ]
   in baseAttrs
        `lodashMerge` refererObj
        `lodashMerge` headersObj
        `lodashMerge` tagsObj
  where
    -- Stable semconv name for what the SDK sends as host (net.host.name is deprecated).
    -- Host resolution reads that column and nothing else, so it is populated here rather
    -- than reconstructed at query time; rawUrl keeps hostless spans attributable.
    serverAddress = fromMaybe (hostFromRawUrl rm.rawUrl) (rm.host >>= guarded (not . T.null))

    tagsObj = maybe (AE.object []) (\tags -> nestedJsonFromDotNotation [("monoscope.tags", AE.Array $ V.fromList $ map AE.String tags)]) rm.tags

    refererObj = maybe (AE.object []) (\r -> nestedJsonFromDotNotation [("http.request.headers.referer", AE.String $ either identity (T.intercalate ",") r)]) rm.referer

    headersObj =
      let extractHeaders prefix = maybe (AE.object []) (nestedJsonFromDotNotation . map (first ((prefix <>) . AEK.toText)) . AEKM.toList) . (^? _Object)
       in extractHeaders "http.request.headers." rm.requestHeaders `lodashMerge` extractHeaders "http.response.headers." rm.responseHeaders


-- | The authority of an absolute URL, minus any port. Relative URLs have no authority,
-- so they yield @""@ rather than being mistaken for a host.
--
-- >>> hostFromRawUrl "https://api.test.com/v1/users?page=1"
-- "api.test.com"
--
-- The port is not part of the host, and neither is a trailing path:
-- >>> hostFromRawUrl "http://api.test.com:8443/v1"
-- "api.test.com"
-- >>> hostFromRawUrl "https://api.test.com"
-- "api.test.com"
--
-- No scheme means no authority to extract -- a path must never be read as a host:
-- >>> hostFromRawUrl "/api/v1/users"
-- ""
-- >>> hostFromRawUrl ""
-- ""
hostFromRawUrl :: Text -> Text
hostFromRawUrl url = case T.breakOn "://" url of
  (_, rest) | not (T.null rest) -> T.takeWhile (\c -> c /= '/' && c /= ':') (T.drop 3 rest)
  _ -> ""


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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake RequestMessage


-- Untagged codec for the SDK's referer field: a bare string or an array of strings
-- (aeson's stock Either instance is @{"Left": …}@-tagged, hence the override).
instance {-# OVERLAPPING #-} AE.ToJSON (Either Text [Text]) where
  toJSON = either AE.toJSON AE.toJSON


instance {-# OVERLAPPING #-} AE.FromJSON (Either Text [Text]) where
  parseJSON v = case v of
    AE.String txt -> pure $ Left txt
    AE.Array{} -> Right <$> AE.parseJSON v
    _ -> fail "Expected either a single string or an array of strings"


-- | Walk the JSON once, redact any fields which are in the list of json paths to be redacted.
--
-- Empty paths short-circuits to structural sharing: subtrees with no live path
-- are returned by pointer instead of rebuilt. Key matching requires a @.@
-- boundary (tightening over a prior prefix-match implementation).
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
--
-- Prefix without a path-boundary does NOT match:
--
-- >>> redactJSON (V.fromList ["password"]) [aesonQQ| {"pass":{"word":"secret"}} |]
-- Object (fromList [("pass",Object (fromList [("word",String "secret")]))])
redactJSON :: V.Vector Text -> AE.Value -> AE.Value
redactJSON ps0 = go [fromMaybe p (T.stripPrefix "." p) | p <- V.toList ps0]
  where
    go [] v = v
    go ps v = case v of
      AE.Object om -> AE.Object $ AEKM.mapWithKey (\k -> go (mapMaybe (matchKey (AEK.toText k)) ps)) om
      AE.Array xs
        | null cps -> v
        | otherwise -> AE.Array $ V.map (go cps) xs
        where
          cps = mapMaybe (\p -> T.stripPrefix "[]." p <|> T.stripPrefix "[]" p) ps
      AE.String{} | "" `elem` ps -> AE.String "[REDACTED]"
      AE.Number{} | "" `elem` ps -> AE.String "[REDACTED]"
      _ -> v

    -- Match @k@ against a path on a @.@ boundary, returning the remainder.
    -- Avoids allocating @k <> "."@ per (key, path) pair.
    matchKey !k path =
      T.stripPrefix k path >>= \rest -> case T.uncons rest of
        Nothing -> Just ""
        Just ('.', rest') -> Just rest'
        _ -> Nothing


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
valueToFields value = dedupFields $ removeBlacklistedFields $ V.fromList $ valueToFields' value "" []
  where
    valueToFields' :: AE.Value -> Text -> [(Text, AE.Value)] -> [(Text, AE.Value)]
    valueToFields' (AE.Object v) !prefix !acc =
      HM.foldlWithKey' folder acc (AEKM.toHashMapText v)
      where
        folder !akkL k val =
          let !newPrefix = if T.null prefix then normalizeKey k else prefix <> "." <> normalizeKey k
           in valueToFields' val newPrefix akkL
    valueToFields' (AE.Array v) !prefix !acc =
      V.foldl' folder acc v
      where
        folder !akkL val = valueToFields' val (prefix <> "[*]") akkL
    valueToFields' v !prefix !acc = (prefix, v) : acc

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
  hashTable <- HT.newSized (V.length fields) :: ST s (HT.HashTable s Text [AE.Value])
  forM_ fields $ \(k, v) -> do
    existing <- HT.lookup hashTable k
    HT.insert hashTable k (v : fromMaybe [] existing)
  pairs <- HTC.toList hashTable
  pure $! V.fromList [(k, V.fromList vs) | (k, vs) <- pairs]


-- >>> removeBlacklistedFields [(".menu.password",String "xyz"),(".authorization",String "abc")]
-- [(".menu.password",String "[REDACTED]"),(".authorization",String "[REDACTED]")]
--
-- >>> removeBlacklistedFields [(".menu.password",Null),(".regular",String "abc")]
-- [(".menu.password",String "[REDACTED]"),(".regular",String "abc")]
removeBlacklistedFields :: V.Vector (Text, AE.Value) -> V.Vector (Text, AE.Value)
removeBlacklistedFields = V.map \(k, val) ->
  (k, bool val (AE.String "[REDACTED]") $ any (`T.isSuffixOf` T.toLower k) ["password", "authorization", "cookie"])


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
valueToFormatStr val = snd <$> find (\(regex, _) -> matched (val ?=~ regex)) commonFormatPatterns


-- | Detect dynamic URL segments and replace them with named parameters.
--
-- >>> let (url, _, hasDyn) = ensureUrlParams "/users/550e8400-e29b-41d4-a716-446655440000/posts" in (url, hasDyn)
-- ("/users/{uuid}/posts",True)
--
-- >>> let (url, _, hasDyn) = ensureUrlParams "/api/v1/items" in (url, hasDyn)
-- ("/api/v1/items",False)
--
-- >>> let (url, _, hasDyn) = ensureUrlParams "/users/12345/orders/67890" in (url, hasDyn)
-- ("/users/{number}/orders/{number_1}",True)
--
-- >>> let (url, _, hasDyn) = ensureUrlParams "/events/2023-01-15/report" in (url, hasDyn)
-- ("/events/{date}/report",True)
--
-- >>> let (url, _, hasDyn) = ensureUrlParams "" in (url, hasDyn)
-- ("",False)
ensureUrlParams :: Text -> (Text, AE.Value, Bool)
ensureUrlParams "" = ("", AE.object [], False)
ensureUrlParams url = (T.intercalate "/" segs, pathParams, not (null dynSegs))
  where
    (segsR, valsR) = foldl' step ([], []) (T.splitOn "/" url)
    step (sAcc, vAcc) x = maybe (x : sAcc, vAcc) (\lbl -> (addNewSegment sAcc lbl, x : vAcc)) (dynSegmentLabel x)
    segs = reverse segsR
    vals = reverse valsR
    dynSegs = filter (T.isPrefixOf "{") segs
    pathParams = AE.object [AEK.fromText (T.tail k) AE..= v | (k, v) <- zip dynSegs vals]


-- | The parameter name a URL segment collapses to, or 'Nothing' if it is static.
dynSegmentLabel :: Text -> Maybe Text
dynSegmentLabel x = case valueToFormatStr x of
  Nothing -> if isUrlIdLike x then Just "param" else Nothing
  Just "{uuid}" -> Just "uuid"
  Just v
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
        Just "date"
    | v `elem` ["{ip}", "{ipv6}"] -> Just "ip_address"
    | v `elem` ["{integer}", "{float}", "{hex}"] -> Just "number"
    | otherwise -> Just "param"


-- | Append a @{label}@ segment, suffixing @_n@ when the label already occurs.
addNewSegment :: [Text] -> Text -> [Text]
addNewSegment segs seg =
  let pfx = "{" <> seg
      pos = length [s | s <- segs, pfx `T.isPrefixOf` s]
   in (if pos > 0 then pfx <> "_" <> show pos <> "}" else pfx <> "}") : segs


-- | Detect ID-like URL segments that valueToFormatStr misses (compound IDs, tokens, etc.)
--
-- >>> map isUrlIdLike ["auth0|69a4b387381d604d626299ec", "google-oauth2|123456789"]
-- [True,True]
--
-- >>> map isUrlIdLike ["type:value", "ns:resource:123"]
-- [True,True]
--
-- >>> map isUrlIdLike ["users", "api", "v2", "profile", "health-check"]
-- [False,False,False,False,False]
--
-- >>> map isUrlIdLike ["aG9sYS1tdW5kb0jb21v3", "dXNlci1wcm9maWxlLWR2dGE1"]
-- [True,True]
--
-- >>> isUrlIdLike "abc123def456ghi789jkl"
-- True
--
-- >>> map isUrlIdLike ["", "a", "v1", ":leading-colon"]
-- [False,False,False,False]
--
-- >>> map isUrlIdLike ["best-restaurants-2025-guide", "GetUserProfileData"]
-- [False,False]
--
-- >>> map isUrlIdLike ["0PUK6V6EV0", "HQTGWGPNH4", "xk7f3m9p", "abc123def456"]
-- [True,True,True,True]
--
-- >>> map isUrlIdLike ["V2", "API", "S3", "route53"]
-- [False,False,False,False]
isUrlIdLike :: Text -> Bool
isUrlIdLike seg
  | T.null seg = False
  | T.elem '|' seg = True -- compound IDs: auth0|abc, google-oauth2|123
  | not (T.isPrefixOf ":" seg) && T.elem ':' seg = True -- namespaced: type:value
  | len > 20 && hasMixed = True -- long mixed alphanumeric (no hyphens — excludes slugs)
  | len > 16 && isBase64Url = True -- base64url-encoded token (requires digits)
  | len >= 6 && hasMixed && isLowVowel = True -- short IDs: mixed alphanumeric with few vowels
  | otherwise = False
  where
    len = T.length seg
    hasMixed = T.any isAlpha seg && T.any isDigit seg && T.all isAlphaNum seg
    isBase64Url = T.all (\c -> isAlphaNum c || c == '-' || c == '_') seg && T.any isUpper seg && T.any isLower seg && T.any isDigit seg
    isLowVowel = T.length (T.filter (`T.elem` "aeiouAEIOU") seg) * 4 < len


-- | Check if a concrete URL path matches a pre-split template with {param} wildcards.
--
-- >>> pathMatchesTemplate (T.splitOn "/" "/api/v2/users/john_doe/profile") (T.splitOn "/" "/api/v2/users/{param}/profile")
-- True
--
-- >>> pathMatchesTemplate (T.splitOn "/" "/api/v2/users/john_doe") (T.splitOn "/" "/api/v2/posts/{param}")
-- False
--
-- >>> pathMatchesTemplate (T.splitOn "/" "/api/v2/users") (T.splitOn "/" "/api/v2/users/{param}")
-- False
pathMatchesTemplate :: [Text] -> [Text] -> Bool
pathMatchesTemplate ps ts = length ps == length ts && and (zipWith (\p t -> t == "{param}" || p == t) ps ts)


-- | Find the first matching canonical template for a request path.
-- Templates are indexed by (method, host) for O(1) lookup per span.
matchCanonicalPath :: HM.HashMap (Text, Text) [([Text], Text)] -> Text -> Text -> Text -> Maybe Text
matchCanonicalPath idx reqMethod reqHost reqPath =
  let !reqSegs = T.splitOn "/" reqPath
   in snd <$> find (pathMatchesTemplate reqSegs . fst) (HM.findWithDefault [] (reqMethod, reqHost) idx)


-- | Parse pipe-delimited canonical path entries ("method|host|template") into a HashMap keyed by (method, host).
-- Call once per ProjectCache, not per span.
parseCanonicalPaths :: V.Vector Text -> HM.HashMap (Text, Text) [([Text], Text)]
parseCanonicalPaths = V.foldl' step HM.empty
  where
    step m t = case T.splitOn "|" t of [method, host, p] -> HM.insertWith (<>) (method, host) [(T.splitOn "/" p, p)] m; _ -> m


-- | Tokenize URL path for Drain: split by "/" and pre-normalize with valueToFormatStr/isUrlIdLike.
tokenizeUrlPath :: Text -> V.Vector Text
tokenizeUrlPath = V.fromList . map normalize . T.splitOn "/"
  where
    normalize seg = fromMaybe (bool seg "<*>" $ isUrlIdLike seg) (valueToFormatStr seg)
