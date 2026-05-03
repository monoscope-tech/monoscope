-- | Model Context Protocol (MCP) handler for the public API.
--
-- One JSON-RPC endpoint exposes every operation in 'Web.Routes.apiV1OpenApiSpec'
-- as an MCP tool. The registry mixes OpenAPI-derived REST tools with hand-written
-- composite (workflow) tools that bundle several internal calls into one
-- agent-friendly verb. REST calls forward into a re-served inner @ApiV1Routes@
-- 'Servant.Application'; composite calls run directly in 'ATBaseCtx'.
module Web.MCP (
  Tool (..),
  Dispatch (..),
  OpenApiBinding (..),
  allTools,
  handleJsonRpc,
) where

import Control.Lens ((^.))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict.InsOrd qualified as IOH
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.OpenApi (OpenApi)
import Data.OpenApi qualified as OA
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Models.Apis.Fields qualified as Fields
import Pkg.DeriveUtils (UUIDId (..))
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WT
import Pkg.AI qualified as AI
import Relude
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATBaseCtx)
import Web.ApiHandlers qualified as ApiH


-- =============================================================================
-- Types
-- =============================================================================

data Tool = Tool
  { name :: !Text
  , description :: !Text
  , inputSchema :: !AE.Value
  , dispatch :: !Dispatch
  }


data Dispatch
  = ViaOpenApi !OpenApiBinding
  | -- | Composite handler runs directly in 'ATBaseCtx' and returns the body of
    -- an MCP tool result envelope. ServerErrors thrown inside are caught by
    -- the dispatcher and surfaced as @isError: true@ tool results.
    ViaComposite !(Projects.ProjectId -> AE.Object -> ATBaseCtx AE.Value)


-- | Everything we need to forward an MCP @tools/call@ into the inner sub-app.
data OpenApiBinding = OpenApiBinding
  { method :: !ByteString
  , path :: !Text
  , pathParams :: ![Text]
  , queryParams :: ![Text]
  , bodyRequired :: !Bool
  }


-- =============================================================================
-- Registry
-- =============================================================================

-- | The full tool registry: every OpenAPI operation plus the composite tools.
-- Composite tools shadow OpenAPI tools of the same name (none should clash —
-- the override map and 'compositeTools' both pick distinct verbs).
allTools :: OpenApi -> Map Text Tool
allTools spec =
  Map.fromList [(t.name, t) | t <- compositeTools] <> mkToolsFromOpenApi spec


mkToolsFromOpenApi :: OpenApi -> Map Text Tool
mkToolsFromOpenApi spec =
  Map.fromList
    [ (t.name, t)
    | (rawPath, item) <- IOH.toList (spec ^. OA.paths)
    , (m, op) <- pathItemOps item
    , let p = toText rawPath
    , (m, p) /= ("POST", "/mcp") -- never expose MCP itself as a tool
    , let t = openApiTool p m op
    ]


openApiTool :: Text -> ByteString -> OA.Operation -> Tool
openApiTool p m op =
  let params = mapMaybe deref (op ^. OA.parameters)
      pathParams' = [q ^. OA.name | q <- params, q ^. OA.in_ == OA.ParamPath]
      queryParams' = [q ^. OA.name | q <- params, q ^. OA.in_ == OA.ParamQuery]
      mrb = op ^. OA.requestBody >>= deref
      bodyReq = maybe False (\rb -> rb ^. OA.required == Just True) mrb
      desc = fromMaybe (decodeUtf8 m <> " " <> p) ((op ^. OA.summary) <|> (op ^. OA.description))
   in Tool
        { name = fromMaybe (deriveNameFromPath p m) (Map.lookup (m, p) toolNameOverrides)
        , description = desc
        , inputSchema = mkInputSchema params mrb bodyReq
        , dispatch =
            ViaOpenApi
              OpenApiBinding
                { method = m
                , path = p
                , pathParams = pathParams'
                , queryParams = queryParams'
                , bodyRequired = bodyReq
                }
        }


-- | Canonical verb-first names for every API v1 operation. Industry-standard
-- naming (Sentry/Grafana/Datadog patterns): @list_X@, @get_X@, @create_X@,
-- @update_X@, @<action>_<resource>@. New routes get a path-derived fallback;
-- to give them a canonical name, add an entry here.
toolNameOverrides :: Map (ByteString, Text) Text
toolNameOverrides =
  Map.fromList
    [ (("GET", "/events"), "list_events")
    , (("POST", "/events/query"), "search_events")
    , (("GET", "/metrics"), "query_metrics")
    , (("GET", "/schema"), "get_schema")
    , (("GET", "/facets"), "list_facets")
    , (("POST", "/rrweb"), "submit_rrweb_event")
    , -- Monitors
      (("GET", "/monitors"), "list_monitors")
    , (("POST", "/monitors"), "create_monitor")
    , (("GET", "/monitors/{monitor_id}"), "get_monitor")
    , (("PUT", "/monitors/{monitor_id}"), "update_monitor")
    , (("PATCH", "/monitors/{monitor_id}"), "patch_monitor")
    , (("DELETE", "/monitors/{monitor_id}"), "delete_monitor")
    , (("POST", "/monitors/{monitor_id}/mute"), "mute_monitor")
    , (("POST", "/monitors/{monitor_id}/unmute"), "unmute_monitor")
    , (("POST", "/monitors/{monitor_id}/resolve"), "resolve_monitor")
    , (("POST", "/monitors/{monitor_id}/toggle_active"), "toggle_monitor_active")
    , (("POST", "/monitors/bulk"), "bulk_monitors")
    , -- Dashboards
      (("GET", "/dashboards"), "list_dashboards")
    , (("POST", "/dashboards"), "create_dashboard")
    , (("POST", "/dashboards/apply"), "apply_dashboard")
    , (("GET", "/dashboards/{dashboard_id}"), "get_dashboard")
    , (("PUT", "/dashboards/{dashboard_id}"), "update_dashboard")
    , (("PATCH", "/dashboards/{dashboard_id}"), "patch_dashboard")
    , (("DELETE", "/dashboards/{dashboard_id}"), "delete_dashboard")
    , (("POST", "/dashboards/{dashboard_id}/duplicate"), "duplicate_dashboard")
    , (("POST", "/dashboards/{dashboard_id}/star"), "star_dashboard")
    , (("DELETE", "/dashboards/{dashboard_id}/star"), "unstar_dashboard")
    , (("GET", "/dashboards/{dashboard_id}/yaml"), "get_dashboard_yaml")
    , (("PUT", "/dashboards/{dashboard_id}/widgets"), "upsert_dashboard_widget")
    , (("DELETE", "/dashboards/{dashboard_id}/widgets/{widget_id}"), "delete_dashboard_widget")
    , (("PATCH", "/dashboards/{dashboard_id}/widgets/order"), "reorder_dashboard_widgets")
    , (("POST", "/dashboards/bulk"), "bulk_dashboards")
    , -- API keys
      (("GET", "/api_keys"), "list_api_keys")
    , (("POST", "/api_keys"), "create_api_key")
    , (("GET", "/api_keys/{key_id}"), "get_api_key")
    , (("DELETE", "/api_keys/{key_id}"), "delete_api_key")
    , (("POST", "/api_keys/{key_id}/activate"), "activate_api_key")
    , (("POST", "/api_keys/{key_id}/deactivate"), "deactivate_api_key")
    , -- /me + project
      (("GET", "/me"), "whoami")
    , (("GET", "/project"), "get_project")
    , (("PATCH", "/project"), "patch_project")
    , -- Endpoints
      (("GET", "/endpoints"), "list_endpoints")
    , (("GET", "/endpoints/{endpoint_id}"), "get_endpoint")
    , -- Log patterns
      (("GET", "/log_patterns"), "list_log_patterns")
    , (("GET", "/log_patterns/{pattern_id}"), "get_log_pattern")
    , (("POST", "/log_patterns/{pattern_id}/ack"), "ack_log_pattern")
    , (("POST", "/log_patterns/bulk"), "bulk_log_patterns")
    , -- Issues
      (("GET", "/issues"), "list_issues")
    , (("GET", "/issues/{issue_id}"), "get_issue")
    , (("POST", "/issues/{issue_id}/ack"), "ack_issue")
    , (("POST", "/issues/{issue_id}/unack"), "unack_issue")
    , (("POST", "/issues/{issue_id}/archive"), "archive_issue")
    , (("POST", "/issues/{issue_id}/unarchive"), "unarchive_issue")
    , (("POST", "/issues/bulk"), "bulk_issues")
    , -- Teams
      (("GET", "/teams"), "list_teams")
    , (("POST", "/teams"), "create_team")
    , (("GET", "/teams/{team_id}"), "get_team")
    , (("PUT", "/teams/{team_id}"), "update_team")
    , (("PATCH", "/teams/{team_id}"), "patch_team")
    , (("DELETE", "/teams/{team_id}"), "delete_team")
    , (("POST", "/teams/bulk"), "bulk_teams")
    , -- Members
      (("GET", "/members"), "list_members")
    , (("POST", "/members"), "add_member")
    , (("GET", "/members/{user_id}"), "get_member")
    , (("PATCH", "/members/{user_id}"), "patch_member")
    , (("DELETE", "/members/{user_id}"), "remove_member")
    , -- Misc
      (("POST", "/share"), "create_share_link")
    ]


-- | Fallback name when no override exists. Drops {id} braces, joins with
-- underscores, and appends the lowercase method so names are unique.
deriveNameFromPath :: Text -> ByteString -> Text
deriveNameFromPath p method =
  let m = T.toLower (decodeUtf8 method)
      cleaned = T.replace "{" "" $ T.replace "}" "" p
      parts = filter (not . T.null) (T.splitOn "/" cleaned)
   in T.intercalate "_" (parts <> [m])


pathItemOps :: OA.PathItem -> [(ByteString, OA.Operation)]
pathItemOps p =
  catMaybes
    [ ("GET",) <$> p ^. OA.get
    , ("POST",) <$> p ^. OA.post
    , ("PUT",) <$> p ^. OA.put
    , ("PATCH",) <$> p ^. OA.patch
    , ("DELETE",) <$> p ^. OA.delete
    , ("HEAD",) <$> p ^. OA.head_
    , ("OPTIONS",) <$> p ^. OA.options
    , ("TRACE",) <$> p ^. OA.trace
    ]


deref :: OA.Referenced a -> Maybe a
deref (OA.Inline a) = Just a
deref _ = Nothing


mkInputSchema :: [OA.Param] -> Maybe OA.RequestBody -> Bool -> AE.Value
mkInputSchema params mrb bodyReq =
  let paramProps =
        KM.fromList [(AK.fromText (q ^. OA.name), paramSchemaJson q) | q <- params]
      paramRequireds = [q ^. OA.name | q <- params, q ^. OA.required == Just True]
      bodySchema = mrb >>= bodyContentSchema
      allProps = maybe paramProps (\b -> KM.insert "body" b paramProps) bodySchema
      requireds = paramRequireds <> [w | w <- ["body" | bodyReq], isJust bodySchema]
   in AE.object
        [ "type" AE..= ("object" :: Text)
        , "properties" AE..= AE.Object allProps
        , "required" AE..= requireds
        ]


paramSchemaJson :: OA.Param -> AE.Value
paramSchemaJson p =
  let s = maybe (AE.object [("type", AE.String "string")]) AE.toJSON (p ^. OA.schema)
   in case (p ^. OA.description, s) of
        (Just d, AE.Object o) -> AE.Object (KM.insert "description" (AE.String d) o)
        _ -> s


bodyContentSchema :: OA.RequestBody -> Maybe AE.Value
bodyContentSchema rb = case IOH.elems (rb ^. OA.content) of
  (mto : _) -> AE.toJSON <$> (mto ^. OA.schema)
  [] -> Nothing


-- =============================================================================
-- JSON-RPC dispatcher
-- =============================================================================

-- | Top-level dispatcher. Handles JSON-RPC requests for the MCP protocol:
-- @initialize@, @notifications/*@, @tools/list@, @tools/call@.
handleJsonRpc
  :: Map Text Tool
  -> (Projects.ProjectId -> Servant.Application)
  -> Projects.ProjectId
  -> AE.Value
  -> ATBaseCtx AE.Value
handleJsonRpc reg buildApp pid req = case parseRpcReq req of
  Nothing -> pure $ rpcError AE.Null (-32600) "Invalid Request"
  Just (mid, m, params)
    | "notifications/" `T.isPrefixOf` m -> pure AE.Null
    | m == "initialize" -> pure $ rpcOk mid initializeResult
    | m == "tools/list" -> pure $ rpcOk mid (toolsListJson reg)
    | m == "tools/call" -> case parseToolCall params of
        Nothing -> pure $ rpcError mid (-32602) "Invalid params"
        Just (toolName, args) -> case Map.lookup toolName reg of
          Nothing -> pure $ rpcOk mid (toolError ("Unknown tool: " <> toolName))
          Just t -> rpcOk mid <$> runTool buildApp pid t args
    | otherwise -> pure $ rpcError mid (-32601) ("Method not found: " <> m)


runTool
  :: (Projects.ProjectId -> Servant.Application)
  -> Projects.ProjectId
  -> Tool
  -> AE.Object
  -> ATBaseCtx AE.Value
runTool buildApp pid t args = case t.dispatch of
  ViaOpenApi b -> liftIO $ callOpenApi (buildApp pid) b args
  ViaComposite run ->
    run pid args
      `Error.catchError` \_cs (e :: Servant.ServerError) ->
        pure $ toolError $ decodeUtf8 $ Servant.errBody e


parseRpcReq :: AE.Value -> Maybe (AE.Value, Text, AE.Value)
parseRpcReq (AE.Object o) = do
  AE.String m <- KM.lookup "method" o
  let mid = fromMaybe AE.Null (KM.lookup "id" o)
      paramsV = fromMaybe (AE.object []) (KM.lookup "params" o)
  pure (mid, m, paramsV)
parseRpcReq _ = Nothing


parseToolCall :: AE.Value -> Maybe (Text, AE.Object)
parseToolCall (AE.Object o) = do
  AE.String n <- KM.lookup "name" o
  let args = case KM.lookup "arguments" o of
        Just (AE.Object a) -> a
        _ -> KM.empty
  pure (n, args)
parseToolCall _ = Nothing


initializeResult :: AE.Value
initializeResult =
  AE.object
    [ "protocolVersion" AE..= ("2025-06-18" :: Text)
    , "capabilities" AE..= AE.object ["tools" AE..= AE.object []]
    , "serverInfo"
        AE..= AE.object
          [ "name" AE..= ("monoscope" :: Text)
          , "version" AE..= ("1.0" :: Text)
          ]
    ]


toolsListJson :: Map Text Tool -> AE.Value
toolsListJson reg = AE.object ["tools" AE..= map descriptor (Map.elems reg)]
  where
    descriptor t =
      AE.object
        [ "name" AE..= t.name
        , "description" AE..= t.description
        , "inputSchema" AE..= t.inputSchema
        ]


toolError :: Text -> AE.Value
toolError msg =
  AE.object
    [ "content" AE..= ([textContent msg] :: [AE.Value])
    , "isError" AE..= True
    ]


okResult :: AE.Value -> AE.Value
okResult v =
  AE.object
    [ "content" AE..= ([textContent (renderJson v)] :: [AE.Value])
    , "isError" AE..= False
    , "structuredContent" AE..= v
    ]


textContent :: Text -> AE.Value
textContent t = AE.object ["type" AE..= ("text" :: Text), "text" AE..= t]


renderJson :: AE.Value -> Text
renderJson = decodeUtf8 . LBS.toStrict . AE.encode


rpcOk :: AE.Value -> AE.Value -> AE.Value
rpcOk mid r = AE.object ["jsonrpc" AE..= ("2.0" :: Text), "id" AE..= mid, "result" AE..= r]


rpcError :: AE.Value -> Int -> Text -> AE.Value
rpcError mid code msg =
  AE.object
    [ "jsonrpc" AE..= ("2.0" :: Text)
    , "id" AE..= mid
    , "error" AE..= AE.object ["code" AE..= code, "message" AE..= msg]
    ]


-- =============================================================================
-- OpenAPI tool execution (forward as synthetic WAI request into sub-app)
-- =============================================================================

callOpenApi :: Servant.Application -> OpenApiBinding -> AE.Object -> IO AE.Value
callOpenApi app b args = do
  let (p, query, body) = splitArgs b args
      url = p <> if T.null query then "" else "?" <> query
      bodyBs = AE.encode body
      hdrs = [(H.hContentType, "application/json")]
      baseReq = Wai.defaultRequest{Wai.requestMethod = b.method, Wai.requestHeaders = hdrs}
      waiReq = WT.setPath baseReq (TE.encodeUtf8 url)
      sreq = WT.SRequest waiReq bodyBs
  resp <- WT.runSession (WT.srequest sreq) app
  let code = H.statusCode (WT.simpleStatus resp)
      bodyLBS = WT.simpleBody resp
      bodyTxt = decodeUtf8 (LBS.toStrict bodyLBS) :: Text
      isErr = code >= 400
      structured = AE.decode bodyLBS :: Maybe AE.Value
      base =
        [ "content" AE..= ([textContent bodyTxt] :: [AE.Value])
        , "isError" AE..= isErr
        ]
  pure $ AE.object $ base <> maybe [] (\v -> ["structuredContent" AE..= v]) structured


splitArgs :: OpenApiBinding -> AE.Object -> (Text, Text, AE.Value)
splitArgs b args =
  let look k = KM.lookup (AK.fromText k) args
      pathStr = foldl' substPath b.path b.pathParams
      substPath acc k = maybe acc (\v -> T.replace ("{" <> k <> "}") (urlEncodeText (jsonToText v)) acc) (look k)
      qs =
        T.intercalate "&"
          [ k <> "=" <> urlEncodeText (jsonToText v)
          | k <- b.queryParams
          , Just v <- [look k]
          , v /= AE.Null
          ]
      body = fromMaybe AE.Null (look "body")
   in (pathStr, qs, body)


jsonToText :: AE.Value -> Text
jsonToText (AE.String s) = s
jsonToText AE.Null = ""
jsonToText (AE.Bool True) = "true"
jsonToText (AE.Bool False) = "false"
jsonToText v = decodeUtf8 (LBS.toStrict (AE.encode v))


urlEncodeText :: Text -> Text
urlEncodeText = TE.decodeUtf8 . H.urlEncode True . TE.encodeUtf8


-- =============================================================================
-- Composite (workflow) tools
-- =============================================================================

-- | Bundled, agent-friendly tools that combine several internal calls. Modeled
-- after Sentry/Grafana Sift patterns: one verb the agent calls in place of an
-- ad-hoc multi-step plan.
compositeTools :: [Tool]
compositeTools = [findErrorPatterns, searchEventsNL, analyzeIssue]


-- | Top log/error patterns ranked by current-hour volume.
findErrorPatterns :: Tool
findErrorPatterns =
  composite "find_error_patterns"
    "Top established log patterns ranked by current-hour event count. Use to answer 'what is blowing up right now' without crafting a query."
    (objSchema [("limit", intProp "Max patterns to return (default 20).")] [])
    \pid args -> do
      now <- Time.currentTime
      pats <- LogPatterns.getPatternsWithCurrentRates pid now
      let lim = clamp 1 200 (fromMaybe 20 (intArg "limit" args))
          sorted = take lim $ sortOn (Down . (.currentHourCount)) pats
      pure $ okResult $ AE.object ["as_of" AE..= now, "patterns" AE..= sorted]


-- | Translate a natural-language description into a KQL query. Mirrors the
-- existing aiSearch handler but returns the query string for the agent to
-- run separately via @search_events@.
searchEventsNL :: Tool
searchEventsNL =
  composite "search_events_nl"
    "Translate a natural-language description (e.g. 'failed payments in the last hour for service checkout') into a KQL query for the events index. Returns the suggested query, time range and an explanation; call search_events with the query to execute."
    ( objSchema
        [ ("input", strProp "The natural-language description of what to find.")
        , ("timezone", strProp "IANA timezone name for relative time interpretation (optional, e.g. 'America/Los_Angeles').")
        ]
        ["input"]
    )
    \pid args -> case T.strip <$> textArg "input" args of
      Nothing -> pure $ toolError "input is required"
      Just inputT
        | T.null inputT -> pure $ toolError "input must be non-empty"
        | otherwise -> do
            authCtx <- Reader.ask @AuthContext
            now <- Time.currentTime
            facets <- Fields.getFacetSummary pid "otel_logs_and_spans" (addUTCTime (-86400) now) now
            let cfg = (AI.defaultAgenticConfig pid){AI.facetContext = facets, AI.timezone = textArg "timezone" args, AI.maxIterations = 2}
            AI.runAgenticQuery cfg inputT authCtx.env.openaiModel authCtx.env.openaiApiKey >>= \case
              Left err -> pure $ toolError ("AI translation failed: " <> err)
              Right resp ->
                pure $ okResult $ AE.object
                  [ "query" AE..= resp.query
                  , "visualization_type" AE..= resp.visualization
                  , "commentary" AE..= resp.explanation
                  , "time_range" AE..= resp.timeRange
                  ]


-- | Fetch an issue and ask the LLM to diagnose it.
analyzeIssue :: Tool
analyzeIssue =
  composite "analyze_issue"
    "Fetch an issue by id and return both the issue payload and an LLM-generated diagnosis (probable cause, key signals, suggested next steps). Costs one LLM call."
    (objSchema [("issue_id", strProp "UUID of the issue to analyze.")] ["issue_id"])
    \pid args -> case textArg "issue_id" args >>= UUID.fromText of
      Nothing -> pure $ toolError "issue_id is required and must be a valid UUID"
      Just uuid -> do
        issue <- ApiH.apiIssueGet pid (UUIDId uuid)
        authCtx <- Reader.ask @AuthContext
        let prompt =
              T.unlines
                [ "You are a senior SRE diagnosing a production issue. Return concise markdown:"
                , "- **Probable cause**: 1-2 sentences."
                , "- **Key signals**: bullet list grounded in the issue payload."
                , "- **Next steps**: 3-5 short, actionable items."
                , ""
                , "Issue payload:"
                , renderJson (AE.toJSON issue)
                ]
        AI.callOpenAIAPIEff authCtx.env.openaiModel prompt authCtx.env.openaiApiKey >>= \case
          Left err -> pure $ toolError ("LLM call failed: " <> err)
          Right analysis -> pure $ okResult $ AE.object ["issue" AE..= issue, "analysis" AE..= analysis]


-- =============================================================================
-- Composite tool helpers
-- =============================================================================

composite :: Text -> Text -> AE.Value -> (Projects.ProjectId -> AE.Object -> ATBaseCtx AE.Value) -> Tool
composite n d schema run = Tool n d schema (ViaComposite run)


objSchema :: [(Text, AE.Value)] -> [Text] -> AE.Value
objSchema props requireds =
  AE.object
    [ "type" AE..= ("object" :: Text)
    , "properties" AE..= AE.object [(AK.fromText k, v) | (k, v) <- props]
    , "required" AE..= requireds
    ]


strProp, intProp :: Text -> AE.Value
strProp d = AE.object ["type" AE..= ("string" :: Text), "description" AE..= d]
intProp d = AE.object ["type" AE..= ("integer" :: Text), "description" AE..= d]


textArg :: Text -> AE.Object -> Maybe Text
textArg k o = KM.lookup (AK.fromText k) o >>= \case AE.String s -> Just s; _ -> Nothing


intArg :: Text -> AE.Object -> Maybe Int
intArg k o = KM.lookup (AK.fromText k) o >>= \case AE.Number n -> Just (round n); _ -> Nothing


clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
