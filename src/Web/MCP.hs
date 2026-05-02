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
import Data.Char (isUpper, toLower)
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


-- | Build the MCP tool registry from an OpenAPI spec. Each operation with a
-- non-empty @operationId@ becomes one tool named @monoscope_<snake_case_id>@.
mkToolsFromOpenApi :: OpenApi -> Map Text ToolEntry
mkToolsFromOpenApi spec =
  Map.fromList
    [ (te.teName, te)
    | (path, item) <- IOH.toList (spec ^. OA.paths)
    , (method, op) <- pathItemOps item
    , Just te <- [toEntry (toText path) method op]
    ]
  where
    toEntry path method op =
      let params = mapMaybe deref (op ^. OA.parameters)
          pathParams = [p ^. OA.name | p <- params, p ^. OA.in_ == OA.ParamPath]
          queryParams = [p ^. OA.name | p <- params, p ^. OA.in_ == OA.ParamQuery]
          mrb = op ^. OA.requestBody >>= deref
          bodyReq = maybe False (\rb -> rb ^. OA.required == Just True) mrb
          methodTxt = decodeUtf8 method :: Text
          desc =
            fromMaybe (methodTxt <> " " <> path)
              $ (op ^. OA.summary)
              <|> (op ^. OA.description)
          -- Prefer explicit operationId; otherwise synthesize from path+method
          -- so renames at the route level still produce stable, readable tool ids.
          rawName = case op ^. OA.operationId of
            Just opid -> snakeCase opid
            Nothing -> deriveNameFromPath path method
       in Just
            ToolEntry
              { teName = "monoscope_" <> rawName
              , teMethod = method
              , tePath = path
              , teDesc = desc
              , teInputSchema = mkInputSchema params mrb bodyReq
              , tePathParams = pathParams
              , teQueryParams = queryParams
              , teBodyRequired = bodyReq
              }


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


snakeCase :: Text -> Text
snakeCase t = case T.uncons t of
  Nothing -> t
  Just (h, rest) -> T.cons (toLower h) (T.concatMap step rest)
  where
    step c
      | isUpper c = T.pack ['_', toLower c]
      | otherwise = T.singleton c


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
        T.intercalate
          "&"
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
