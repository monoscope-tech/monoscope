module CLI.Core (
  OutputMode (..),
  detectOutputMode,
  isInteractiveTTY,
  isAgentMode,
  renderJSON,
  renderYAML,
  renderByMode,
  renderWith,
  renderTable,
  printError,
  printDebug,
  isDebugMode,
  APIError (..),
  renderAPIError,
  apiGet,
  apiGetJson,
  apiPostUnauth,
  apiPostJson,
  apiPutJson,
  apiPatchJson,
  apiDelete,
  withAPIResult,
) where

import Relude

import CLI.Config (CLIConfig (..))
import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.Aeson.Encode.Pretty qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.Wreq (HTTP, deleteWith, getWith, patchWith, postWith, putWith, responseBody)
import Data.Effectful.Wreq qualified as W
import Data.Text qualified as T
import Data.Text.IO qualified
import Data.Yaml qualified as Yaml
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Request, host, method, path, port, secure)
import Network.Wreq qualified as Wreq
import Pkg.CLIFormat (colWidths, formatRow)
import System.Console.ANSI qualified as ANSI
import UnliftIO.Exception (catch)


-- Output

data OutputMode = OutputJSON | OutputYAML | OutputTable
  deriving stock (Eq, Show)


detectOutputMode :: (Environment :> es, IOE :> es) => Bool -> Maybe Text -> Eff es OutputMode
detectOutputMode jsonFlag outputM = case (outputM, jsonFlag) of
  (Just "yaml", _) -> pure OutputYAML
  (Just "json", _) -> pure OutputJSON
  (Just "table", _) -> pure OutputTable
  (_, True) -> pure OutputJSON
  _ -> do
    tty <- isInteractiveTTY
    pure $ if tty then OutputTable else OutputJSON


isInteractiveTTY :: (Environment :> es, IOE :> es) => Eff es Bool
isInteractiveTTY = do
  tty <- liftIO $ ANSI.hSupportsANSIColor stdout
  agent <- isAgentMode
  pure $ tty && not agent


-- | True if any of the agent-mode signals is set to a truthy value. We
-- treat empty string and the literal "false"/"0" as off — many dev machines
-- export @CI@ unconditionally (direnv, IDE shells), and a literal
-- @CI=false@ should not flip the CLI into JSON-only mode.
isAgentMode :: Environment :> es => Eff es Bool
isAgentMode = any truthy <$> mapM Env.lookupEnv ["MONOSCOPE_AGENT_MODE", "CLAUDE_CODE", "CI"]
  where
    truthy = maybe False (\v -> not (null v) && v `notElem` ["false", "False", "FALSE", "0"])


renderJSON :: (AE.ToJSON a, IOE :> es) => a -> Eff es ()
renderJSON = liftIO . putLBSLn . AE.encodePretty


renderYAML :: (AE.ToJSON a, IOE :> es) => a -> Eff es ()
renderYAML = liftIO . putBSLn . Yaml.encode


-- | Render using the chosen mode; table mode falls back to JSON if no table rows supplied.
renderByMode :: (AE.ToJSON a, IOE :> es) => OutputMode -> Maybe ([Text], [[Text]]) -> a -> Eff es ()
renderByMode mode tableM v = renderWith mode v (maybe (renderJSON v) (uncurry renderTable) tableM)


-- | Render JSON/YAML uniformly; delegate to a custom action for table mode.
renderWith :: (AE.ToJSON a, IOE :> es) => OutputMode -> a -> Eff es () -> Eff es ()
renderWith mode v tableAction = case mode of
  OutputJSON -> renderJSON v
  OutputYAML -> renderYAML v
  OutputTable -> tableAction


renderTable :: IOE :> es => [Text] -> [[Text]] -> Eff es ()
renderTable headers rows = liftIO $ do
  let allRows = headers : rows
      widths = colWidths allRows
  ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  putTextLn $ formatRow widths headers
  ANSI.setSGR [ANSI.Reset]
  putTextLn $ T.intercalate "  " [T.replicate w "-" | w <- widths]
  mapM_ (putTextLn . formatRow widths) rows


printError :: IOE :> es => Text -> Eff es ()
printError msg = liftIO $ do
  ANSI.hSetSGR stderr [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  Data.Text.IO.hPutStrLn stderr $ "error: " <> msg
  ANSI.hSetSGR stderr [ANSI.Reset]


isDebugMode :: Environment :> es => Eff es Bool
isDebugMode = isJust <$> Env.lookupEnv "MONOSCOPE_DEBUG"


-- | Emit a debug line to stderr when MONOSCOPE_DEBUG is set.
printDebug :: (Environment :> es, IOE :> es) => Text -> Eff es ()
printDebug msg = whenM isDebugMode $ liftIO $ do
  ANSI.hSetSGR stderr [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
  Data.Text.IO.hPutStrLn stderr $ "debug: " <> msg
  ANSI.hSetSGR stderr [ANSI.Reset]


-- Client

data APIError = APIError {statusCode :: Int, message :: Text}
  deriving stock (Show)


-- | Human-readable rendering for end-user error messages.
-- Preferred over 'show' which leaks Haskell record syntax.
renderAPIError :: APIError -> Text
renderAPIError e
  | e.statusCode == 0 = e.message
  | otherwise = "HTTP " <> show e.statusCode <> ": " <> e.message


-- | Build common request options (auth + project header + query params).
reqOpts :: CLIConfig -> [(Text, Text)] -> W.Options
reqOpts cfg params =
  W.defaults
    & W.header "Accept"
    .~ ["application/json"]
      & addAuth cfg.apiKey
      & addProjectId cfg.projectId
      & addParams params


apiGet :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> Eff es (Either APIError LByteString)
apiGet cfg path params = do
  printDebug $ "GET " <> cfg.apiUrl <> path <> renderParams params
  catch
    (Right . (^. responseBody) <$> getWith (reqOpts cfg params) (toString $ cfg.apiUrl <> path))
    (pure . Left . httpExToError)


-- | Explicit @forall@ pins the type-variable order at @es, a@ so call sites
-- can use @apiGetJson \@es \@a@ — keeps @\@_ \@AE.Value@ working regardless of
-- how a formatter sorts constraints.
apiGetJson :: forall es a. (AE.FromJSON a, Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> Eff es (Either APIError a)
apiGetJson cfg path params = decodeBody <$> apiGet cfg path params


-- | Unauthenticated POST for device-code auth bootstrap (pre-token flows).
-- All other callers should use the 'CLIConfig'-carrying variants which attach
-- Bearer + project headers.
apiPostUnauth :: (HTTP :> es, IOE :> es) => Text -> Text -> [(Text, Text)] -> Eff es (Either APIError LByteString)
apiPostUnauth baseUrl path params =
  catch
    ( Right
        . (^. responseBody)
        <$> postWith (W.defaults & W.header "Accept" .~ ["application/json"] & addParams params) (toString $ baseUrl <> path) ("" :: ByteString)
    )
    (pure . Left . httpExToError)


-- | POST JSON with optional query params (wreq URL-encodes them safely).
-- | Explicit @forall@ pins the type-variable order at @es, a, b@ so call
-- sites can use @apiPostJson \@es \@a \@b@ — keeps @\@_ \@_ \@AE.Value@
-- working regardless of how a formatter sorts constraints.
apiPostJson :: forall es a b. (AE.FromJSON b, AE.ToJSON a, Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> a -> Eff es (Either APIError b)
apiPostJson = jsonCall postWith


apiPutJson :: forall es a b. (AE.FromJSON b, AE.ToJSON a, Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> a -> Eff es (Either APIError b)
apiPutJson = jsonCall putWith


apiPatchJson :: forall es a b. (AE.FromJSON b, AE.ToJSON a, Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> a -> Eff es (Either APIError b)
apiPatchJson = jsonCall patchWith


-- | DELETE; discards response body (returns unit on success).
apiDelete :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Text -> Eff es (Either APIError ())
apiDelete cfg path = do
  printDebug $ "DELETE " <> cfg.apiUrl <> path
  catch
    (deleteWith (reqOpts cfg []) (toString $ cfg.apiUrl <> path) $> Right ())
    (pure . Left . httpExToError)


jsonCall
  :: (AE.FromJSON b, AE.ToJSON a, Environment :> es, IOE :> es)
  => (W.Options -> String -> LByteString -> Eff es (Wreq.Response LByteString))
  -> CLIConfig
  -> Text
  -> [(Text, Text)]
  -> a
  -> Eff es (Either APIError b)
jsonCall action cfg path params body = do
  printDebug $ "POST/PUT/PATCH " <> cfg.apiUrl <> path <> renderParams params
  catch
    ( do
        let opts = reqOpts cfg params & W.header "Content-Type" .~ ["application/json"]
            url = toString $ cfg.apiUrl <> path
        resp <- action opts url (AE.encode body)
        pure $ decodeBody (Right (resp ^. responseBody))
    )
    (pure . Left . httpExToError)


-- | Decode a JSON response body. Empty bodies are treated as an explicit error —
-- we used to coerce them to @null@, which masked server bugs for callers that
-- happen to accept @null@ (e.g. 'AE.Value'). Callers that genuinely expect an
-- empty body (like 'apiDelete') return @()@ via the catch path, not this.
decodeBody :: AE.FromJSON a => Either APIError LByteString -> Either APIError a
decodeBody (Left e) = Left e
decodeBody (Right bs)
  | LBS.null bs = Left (APIError 0 "empty response body")
  | otherwise = first (APIError 0 . toText) (AE.eitherDecode bs)


-- | Fetch JSON from API endpoint and apply a handler, printing errors on failure.
withAPIResult :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> (AE.Value -> Eff es ()) -> Eff es ()
withAPIResult cfg path params onSuccess =
  apiGet cfg path params >>= \case
    Left err -> printError (renderAPIError err) >> liftIO exitFailure
    Right bs -> case AE.eitherDecode @AE.Value bs of
      Left err -> printError (toText err) >> liftIO exitFailure
      Right val -> onSuccess val


addAuth :: Maybe Text -> W.Options -> W.Options
addAuth Nothing o = o
addAuth (Just key) o = o & W.header "Authorization" .~ [encodeUtf8 ("Bearer " <> key)]


addProjectId :: Maybe Text -> W.Options -> W.Options
addProjectId Nothing o = o
addProjectId (Just pid) o = o & W.header "X-Project-Id" .~ [encodeUtf8 pid]


addParams :: [(Text, Text)] -> W.Options -> W.Options
addParams ps o = foldl' (\acc (k, v) -> acc & Wreq.param k .~ [v]) o ps


httpExToError :: HttpException -> APIError
httpExToError (HttpExceptionRequest req (StatusCodeException resp body)) =
  let code = resp ^. Wreq.responseStatus . Wreq.statusCode
      statusMsg = decodeUtf8 $ resp ^. Wreq.responseStatus . Wreq.statusMessage
      bodyTxt = T.strip (decodeUtf8 body)
      -- Server validation errors (e.g. KQL parser) are returned in the body;
      -- prefer them over the generic status reason so agents can self-correct.
      msg = if T.null bodyTxt then statusMsg else bodyTxt
   in APIError code (msg <> " (" <> reqSummary req <> ")")
httpExToError (HttpExceptionRequest req (ConnectionFailure _)) =
  APIError 0 ("connection failed — is the server running? (" <> reqSummary req <> ")")
httpExToError (HttpExceptionRequest req content) =
  APIError 0 (show content <> " (" <> reqSummary req <> ")")
httpExToError (InvalidUrlException url reason) =
  APIError 0 ("invalid URL " <> toText url <> ": " <> toText reason)


-- | Debug-only query-string formatter — values are NOT URL-encoded so the
-- KQL/operator characters are readable in the printed line. Auth lives in
-- HTTP headers ('reqOpts'), not query params; if you ever add a credential
-- to a query param, redact it here before it lands in stderr.
renderParams :: [(Text, Text)] -> Text
renderParams [] = ""
renderParams ps = "?" <> T.intercalate "&" [k <> "=" <> v | (k, v) <- ps]


-- | Compact "METHOD scheme://host:port/path" for error context.
reqSummary :: Request -> Text
reqSummary r =
  decodeUtf8 (method r)
    <> " "
    <> (if secure r then "https" else "http")
    <> "://"
    <> decodeUtf8 (host r)
    <> (if port r `elem` [80, 443] then "" else ":" <> show (port r))
    <> decodeUtf8 (path r)
