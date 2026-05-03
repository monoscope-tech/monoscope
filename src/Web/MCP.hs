-- | Model Context Protocol (MCP) handler for the public API.
--
-- One JSON-RPC endpoint exposes every operation in 'Web.Routes.apiV1OpenApiSpec'
-- as an MCP tool. The registry is built from the OpenAPI spec at startup; tool
-- calls are dispatched by forwarding a synthesized WAI request into the inner
-- @ApiV1Routes@ application provided by the caller.
module Web.MCP (
  ToolEntry (..),
  mkToolsFromOpenApi,
  handleJsonRpc,
) where

import Control.Lens ((^.))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict.InsOrd qualified as IOH
import Data.Map.Strict qualified as Map
import Data.OpenApi (OpenApi)
import Data.OpenApi qualified as OA
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WT
import Relude
import Servant qualified
import System.Types (ATBaseCtx)


data ToolEntry = ToolEntry
  { teName :: !Text
  , teMethod :: !ByteString
  , tePath :: !Text
  , teDesc :: !Text
  , teInputSchema :: !AE.Value
  , tePathParams :: ![Text]
  , teQueryParams :: ![Text]
  , teBodyRequired :: !Bool
  }


-- | Build the MCP tool registry from an OpenAPI spec. Each operation becomes
-- one verb-first snake_case tool (e.g. @list_monitors@, @mute_monitor@). Names
-- come from a canonical override table keyed by (method, path); operations not
-- in the table fall back to a path-derived name. The MCP endpoint itself is
-- excluded so agents cannot recurse into it.
mkToolsFromOpenApi :: OpenApi -> Map Text ToolEntry
mkToolsFromOpenApi spec =
  Map.fromList
    [ (te.teName, te)
    | (rawPath, item) <- IOH.toList (spec ^. OA.paths)
    , (method, op) <- pathItemOps item
    , let path = toText rawPath
    , (method, path) /= ("POST", "/mcp")
    , Just te <- [toEntry path method op]
    ]
  where
    toEntry path method op =
      let params = mapMaybe deref (op ^. OA.parameters)
          pathParams = [p ^. OA.name | p <- params, p ^. OA.in_ == OA.ParamPath]
          queryParams = [p ^. OA.name | p <- params, p ^. OA.in_ == OA.ParamQuery]
          mrb = op ^. OA.requestBody >>= deref
          bodyReq = maybe False (\rb -> rb ^. OA.required == Just True) mrb
          methodTxt = decodeUtf8 method :: Text
          name = fromMaybe (deriveNameFromPath path method) (Map.lookup (method, path) toolNameOverrides)
          desc =
            fromMaybe (methodTxt <> " " <> path)
              $ (op ^. OA.summary) <|> (op ^. OA.description)
       in Just
            ToolEntry
              { teName = name
              , teMethod = method
              , tePath = path
              , teDesc = desc
              , teInputSchema = mkInputSchema params mrb bodyReq
              , tePathParams = pathParams
              , teQueryParams = queryParams
              , teBodyRequired = bodyReq
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
deriveNameFromPath path method =
  let m = T.toLower (decodeUtf8 method)
      cleaned = T.replace "{" "" $ T.replace "}" "" path
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
        KM.fromList
          [ (AK.fromText (p ^. OA.name), paramSchemaJson p)
          | p <- params
          ]
      paramRequireds = [p ^. OA.name | p <- params, p ^. OA.required == Just True]
      bodySchema = mrb >>= bodyContentSchema
      allProps = case bodySchema of
        Just b -> KM.insert "body" b paramProps
        Nothing -> paramProps
      requireds = paramRequireds <> [if bodyReq then "body" else "" | isJust bodySchema, bodyReq]
   in AE.object
        [ "type" AE..= ("object" :: Text)
        , "properties" AE..= AE.Object allProps
        , "required" AE..= filter (not . T.null) requireds
        ]


paramSchemaJson :: OA.Param -> AE.Value
paramSchemaJson p =
  let s = maybe (AE.object [("type", AE.String "string")]) AE.toJSON (p ^. OA.schema)
   in case (p ^. OA.description, s) of
        (Just d, AE.Object o) -> AE.Object (KM.insert "description" (AE.String d) o)
        _ -> s


bodyContentSchema :: OA.RequestBody -> Maybe AE.Value
bodyContentSchema rb =
  let contents = IOH.elems (rb ^. OA.content)
   in case contents of
        (mto : _) -> AE.toJSON <$> (mto ^. OA.schema)
        [] -> Nothing


-- | Top-level dispatcher. Handles JSON-RPC requests for the MCP protocol:
-- @initialize@, @notifications/*@, @tools/list@, @tools/call@.
handleJsonRpc
  :: Map Text ToolEntry
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
          Just te -> do
            r <- liftIO $ callTool (buildApp pid) te args
            pure $ rpcOk mid r
    | otherwise -> pure $ rpcError mid (-32601) ("Method not found: " <> m)


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


toolsListJson :: Map Text ToolEntry -> AE.Value
toolsListJson reg =
  AE.object ["tools" AE..= [toolJson e | e <- Map.elems reg]]
  where
    toolJson e =
      AE.object
        [ "name" AE..= e.teName
        , "description" AE..= e.teDesc
        , "inputSchema" AE..= e.teInputSchema
        ]


toolError :: Text -> AE.Value
toolError msg =
  AE.object
    [ "content" AE..= ([AE.object ["type" AE..= ("text" :: Text), "text" AE..= msg]] :: [AE.Value])
    , "isError" AE..= True
    ]


rpcOk :: AE.Value -> AE.Value -> AE.Value
rpcOk mid r =
  AE.object
    [ "jsonrpc" AE..= ("2.0" :: Text)
    , "id" AE..= mid
    , "result" AE..= r
    ]


rpcError :: AE.Value -> Int -> Text -> AE.Value
rpcError mid code msg =
  AE.object
    [ "jsonrpc" AE..= ("2.0" :: Text)
    , "id" AE..= mid
    , "error"
        AE..= AE.object
          [ "code" AE..= code
          , "message" AE..= msg
          ]
    ]


-- | Forward a tool call as a synthetic HTTP request into the inner @ApiV1Routes@
-- application, then wrap the response as an MCP tool result.
callTool :: Servant.Application -> ToolEntry -> AE.Object -> IO AE.Value
callTool app te args = do
  let (path, query, body) = splitArgs te args
      url = path <> if T.null query then "" else "?" <> query
      bodyBs = AE.encode body
      hdrs = [(H.hContentType, "application/json")]
      baseReq =
        Wai.defaultRequest
          { Wai.requestMethod = te.teMethod
          , Wai.requestHeaders = hdrs
          }
      waiReq = WT.setPath baseReq (TE.encodeUtf8 url)
      sreq = WT.SRequest waiReq bodyBs
  resp <- WT.runSession (WT.srequest sreq) app
  let code = H.statusCode (WT.simpleStatus resp)
      bodyLBS = WT.simpleBody resp
      bodyTxt = decodeUtf8 (LBS.toStrict bodyLBS) :: Text
      isErr = code >= 400
      structured = AE.decode bodyLBS :: Maybe AE.Value
      base =
        [ "content"
            AE..= ( [ AE.object
                        [ "type" AE..= ("text" :: Text)
                        , "text" AE..= bodyTxt
                        ]
                    ]
                      :: [AE.Value]
                  )
        , "isError" AE..= isErr
        ]
  pure $ AE.object $ base <> maybe [] (\v -> ["structuredContent" AE..= v]) structured


splitArgs :: ToolEntry -> AE.Object -> (Text, Text, AE.Value)
splitArgs te args =
  let look k = KM.lookup (AK.fromText k) args
      pathStr = foldl' substPath te.tePath te.tePathParams
      substPath acc k = case look k of
        Just v -> T.replace ("{" <> k <> "}") (urlEncodeText (jsonToText v)) acc
        Nothing -> acc
      qs =
        T.intercalate "&"
          [ k <> "=" <> urlEncodeText (jsonToText v)
          | k <- te.teQueryParams
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


