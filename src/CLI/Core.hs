module CLI.Core (
  OutputMode (..),
  detectOutputMode,
  isInteractiveTTY,
  -- | Deprecated; kept as an alias for back-compat in case any caller still
  -- imports it. Always returns False — the CLI no longer auto-detects agents.
  isAgentMode,
  setOutputMode,
  getOutputMode,
  renderJSON,
  renderYAML,
  renderByMode,
  renderWith,
  renderTable,
  printError,
  printDebug,
  isDebugMode,
  isJsonOutput,
  APIError (..),
  ErrorPayload (..),
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
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.Wreq (HTTP, deleteWith, getWith, patchWith, postWith, putWith, responseBody)
import Data.Effectful.Wreq qualified as W

-- IORef is re-exported from Relude (including atomicModifyIORef').
-- Don't import Data.IORef directly — Relude's atomicModifyIORef' lifts into
-- MonadIO and our import would shadow it.
import Data.Text qualified as T
import Data.Text.IO qualified
import Data.Yaml qualified as Yaml
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Request, host, method, path, port, responseTimeoutMicro, secure)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq qualified as Wreq
import Pkg.CLIFormat (colWidths, formatRow)
import System.Console.ANSI qualified as ANSI
import System.IO.Unsafe (unsafePerformIO)
import System.Random qualified as Random
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (catch)


-- Output

data OutputMode = OutputJSON | OutputYAML | OutputTable
  deriving stock (Eq, Show)


-- | Pick an output mode from three mutually-exclusive boolean flags. Precedence
-- when more than one is set: @--json@ > @--yaml@ > @--table@. With none set,
-- the mode is auto-detected from stdout: TTY → table; pipe/redirect → JSON.
--
-- The earlier model layered @--output FORMAT@ on top of @--json@ and an
-- agent-mode auto-detect that probed @MONOSCOPE_AGENT_MODE@/@CLAUDE_CODE@/
-- @CI@. That was a footgun — every dev machine exports one of those — and
-- four overlapping toggles for one decision. Now: three flags, one decision.
--
-- @MONOSCOPE_FORCE_COLOR=1@ overrides the non-TTY → JSON path for users who
-- want colored tables through @less -R@ or similar.
detectOutputMode :: (Environment :> es, IOE :> es) => Bool -> Bool -> Bool -> Eff es OutputMode
detectOutputMode jsonFlag yamlFlag tableFlag
  | jsonFlag = pure OutputJSON
  | yamlFlag = pure OutputYAML
  | tableFlag = pure OutputTable
  | otherwise = do
      tty <- isInteractiveTTY
      pure $ if tty then OutputTable else OutputJSON


-- | True iff stdout is a real TTY (or @MONOSCOPE_FORCE_COLOR=1@ overrides).
-- The result is read at @resolveMode@ time and threaded through subsequent
-- rendering decisions; we don't re-query mid-command.
isInteractiveTTY :: (Environment :> es, IOE :> es) => Eff es Bool
isInteractiveTTY = do
  forceColor <- isJust <$> Env.lookupEnv "MONOSCOPE_FORCE_COLOR"
  if forceColor
    then pure True
    else liftIO $ ANSI.hSupportsANSIColor stdout


-- | Deprecated. Old code paths that branched on agent mode now branch on
-- 'isJsonOutput' (set once per command via the mode cache below). Kept as
-- @pure False@ so any straggler import compiles.
isAgentMode :: Eff es Bool
isAgentMode = pure False


-- | Process-wide cache of the resolved 'OutputMode'. Set once by @main@ before
-- any command handler runs; read by 'printError' / 'printDebug' / table
-- renderers that need to know "am I in JSON mode?" without threading the mode
-- through every call site.
--
-- Using a top-level 'IORef' is ugly but bounded: we write once at startup and
-- only read after. The alternative is to pipe an extra parameter through
-- every 'printError' call in the codebase, which is what the original
-- @MONOSCOPE_AGENT_MODE@ env-var probe was working around.
{-# NOINLINE outputModeRef #-}
outputModeRef :: IORef OutputMode
outputModeRef = unsafePerformIO (newIORef OutputTable)


-- | Initialise the process-wide output-mode cache from 'detectOutputMode'.
-- Idempotent; later calls overwrite earlier ones.
setOutputMode :: IOE :> es => OutputMode -> Eff es ()
setOutputMode m = liftIO $ writeIORef outputModeRef m


-- | Read the resolved output mode set by 'setOutputMode'.
getOutputMode :: IOE :> es => Eff es OutputMode
getOutputMode = liftIO (readIORef outputModeRef)


-- | True when stdout is JSON or YAML — i.e. the renderer is not allowed to
-- emit ANSI, table chrome, or human-readable status lines on stdout.
isJsonOutput :: IOE :> es => Eff es Bool
isJsonOutput = (/= OutputTable) <$> getOutputMode


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


-- | Legacy plain-Text renderer. Retained so unmigrated commands still work,
-- but new code should call 'CLI.Table.renderRichTable' for the Unicode
-- box-drawing + per-cell color output. Suppresses ANSI in JSON mode so a
-- non-TTY pipe never sees escape codes.
renderTable :: IOE :> es => [Text] -> [[Text]] -> Eff es ()
renderTable headers rows = do
  jsonMode <- isJsonOutput
  liftIO $ do
    let allRows = headers : rows
        widths = colWidths allRows
    unless jsonMode $ ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
    putTextLn $ formatRow widths headers
    unless jsonMode $ ANSI.setSGR [ANSI.Reset]
    putTextLn $ T.intercalate "  " [T.replicate w "-" | w <- widths]
    mapM_ (putTextLn . formatRow widths) rows


-- | One-line error to stderr.
-- - Table/TTY mode: red @error: …@.
-- - JSON/YAML mode: single-line NDJSON object @{"error": {...}}@ so a pipe
--   consumer can parse stderr as JSON-per-line. Stdout stays clean.
printError :: IOE :> es => Text -> Eff es ()
printError msg = do
  jsonMode <- isJsonOutput
  liftIO
    $ if jsonMode
      then LBS.hPutStr stderr (AE.encode (ErrorPayload "error" msg Nothing Nothing) <> "\n")
      else do
        ANSI.hSetSGR stderr [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
        Data.Text.IO.hPutStrLn stderr $ "error: " <> msg
        ANSI.hSetSGR stderr [ANSI.Reset]


isDebugMode :: Environment :> es => Eff es Bool
isDebugMode = isJust <$> Env.lookupEnv "MONOSCOPE_DEBUG"


-- | Emit a debug line to stderr when MONOSCOPE_DEBUG is set. ANSI suppressed
-- in JSON mode so a pipe consumer parsing stderr doesn't choke on escape
-- codes.
printDebug :: (Environment :> es, IOE :> es) => Text -> Eff es ()
printDebug msg = whenM isDebugMode $ do
  jsonMode <- isJsonOutput
  liftIO
    $ if jsonMode
      then Data.Text.IO.hPutStrLn stderr $ "debug: " <> msg
      else do
        ANSI.hSetSGR stderr [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
        Data.Text.IO.hPutStrLn stderr $ "debug: " <> msg
        ANSI.hSetSGR stderr [ANSI.Reset]


-- | Structured error envelope emitted on stderr under JSON mode.
data ErrorPayload = ErrorPayload
  { code :: Text
  , message :: Text
  , field :: Maybe Text
  -- ^ Set for @unknown_field@ style errors so a consumer can fix the query
  -- programmatically.
  , suggestion :: Maybe Text
  -- ^ Optional one-liner the renderer also prints in TTY mode.
  }
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake], DAE.OmitNothingFields] ErrorPayload


-- Client

data APIError = APIError {statusCode :: Int, message :: Text}
  deriving stock (Show)


-- | Human-readable rendering for end-user error messages.
-- - Connection-level errors (statusCode == 0) print the bare message.
-- - When the server returns a structured @{"error": {...}}@ body (A2),
--   surface just the one-line @message@ + @suggestion@; the SQL/Hasql
--   blob lives in @details@ which is only shown under @--debug@.
-- - Otherwise, falls back to @HTTP N: <body>@.
renderAPIError :: APIError -> Text
renderAPIError e
  | e.statusCode == 0 = e.message
  | otherwise = case decodeStructured e.message of
      Just (code, msg, suggestionM)
        | code == "unknown_field" || code == "invalid_query" || code == "validation" ->
            msg <> maybe "" (\s -> " (" <> s <> ")") suggestionM
      _ -> "HTTP " <> show e.statusCode <> ": " <> e.message
  where
    decodeStructured txt = case AE.eitherDecodeStrict @AE.Value (encodeUtf8 txt) of
      Right (AE.Object o)
        | Just (AE.Object err) <- KM.lookup "error" o
        , Just (AE.String c) <- KM.lookup "code" err
        , Just (AE.String m) <- KM.lookup "message" err ->
            let s = case KM.lookup "suggestion" err of
                  Just (AE.String x) -> Just x
                  _ -> Nothing
             in Just (c, m, s)
      _ -> Nothing


-- | Build common request options (auth + project header + query params).
-- Bumps response timeout to 5 minutes — KQL searches over long ranges
-- can legitimately take a while and the wreq/http-client default (30s)
-- causes spurious 'ResponseTimeout' errors that look like server bugs.
--
-- When @MONOSCOPE_DEBUG@ is set, an @X-Debug: 1@ header lets the server
-- include raw SQL/Hasql exception text in its structured error response
-- (under @error.details.sql@). Without the header, the server returns the
-- friendly one-liner only. Keeps prod stderr clean by default.
reqOpts :: Environment :> es => CLIConfig -> [(Text, Text)] -> Eff es W.Options
reqOpts cfg params = do
  dbg <- isDebugMode
  pure
    $ W.defaults
    -- Override http-client's 30s default; long-running aggregates legitimately exceed it.
    & (Wreq.manager .~ Left tlsManagerSettings{HC.managerResponseTimeout = responseTimeoutMicro (5 * 60 * 1_000_000)})
    & (W.header "Accept" .~ ["application/json"])
    & (if dbg then W.header "X-Debug" .~ ["1"] else id)
    & addAuth cfg.apiKey
    & addProjectId cfg.projectId
    & addParams params


apiGet :: (Environment :> es, HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> Eff es (Either APIError LByteString)
apiGet cfg path params = do
  printDebug $ "GET " <> cfg.apiUrl <> path <> renderParams params
  opts <- reqOpts cfg params
  withRetry 2
    $ catch
      (Right . (^. responseBody) <$> getWith opts (toString $ cfg.apiUrl <> path))
      (pure . Left . httpExToError)


-- | Retry once on transient failures: 502/503/504, gateway timeouts, and
-- @ResponseTimeout@. We don't retry 4xx (user error) or 5xx that aren't in
-- the transient set, because a bad query string would just compound. A
-- small jittered backoff (250–750 ms) keeps the retry cheap and avoids
-- thundering-herd on shared CF edges.
withRetry :: (Environment :> es, IOE :> es) => Int -> Eff es (Either APIError a) -> Eff es (Either APIError a)
withRetry n act = go n
  where
    transient code = code `elem` [502, 503, 504, 522, 524]
    go remaining = do
      r <- act
      case r of
        Right v -> pure (Right v)
        Left e
          | remaining > 0
          , transient e.statusCode || isTransientMessage e.message -> do
              jitter <- liftIO $ Random.randomRIO (250_000, 750_000 :: Int)
              liftIO $ threadDelay jitter
              printDebug $ "retrying after transient error: " <> e.message
              go (remaining - 1)
          | otherwise -> pure (Left e)
    isTransientMessage msg =
      "ResponseTimeout" `T.isInfixOf` msg || "ResponseBodyTooShort" `T.isInfixOf` msg


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
  opts <- reqOpts cfg []
  catch
    (deleteWith opts (toString $ cfg.apiUrl <> path) $> Right ())
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
  baseOpts <- reqOpts cfg params
  let opts = baseOpts & W.header "Content-Type" .~ ["application/json"]
      url = toString $ cfg.apiUrl <> path
  catch
    ( do
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
      -- Lenient decode: the body may be HTML, gzip leftovers, or arbitrary
      -- bytes from a misconfigured upstream — never throw at the boundary.
      decode = decodeUtf8
      statusMsg = decode (resp ^. Wreq.responseStatus . Wreq.statusMessage)
      bodyTxt = T.strip (decode body)
      -- Gateway/edge errors (CF, nginx) send HTML; collapsing it into the
      -- error message dumps a few hundred lines of escaped markup into the
      -- user's terminal. Detect the shape and substitute a one-liner. The
      -- structured server errors are JSON and pass through as-is so
      -- 'renderAPIError' can decode them.
      friendly
        | isHtml bodyTxt = case code of
            504 -> "gateway timed out (504) — try narrowing --since/--from/--to"
            502 -> "bad gateway (502) — upstream not responding"
            503 -> "service unavailable (503) — try again in a moment"
            _ -> "upstream error " <> show code <> " (HTML response, suppressed)"
        | T.null bodyTxt = statusMsg
        | otherwise = bodyTxt
   in APIError code (friendly <> " (" <> reqSummary req <> ")")
httpExToError (HttpExceptionRequest req (ConnectionFailure _)) =
  APIError 0 ("connection failed — is the server running? (" <> reqSummary req <> ")")
httpExToError (HttpExceptionRequest req ResponseTimeout) =
  APIError 504 ("response timeout — try narrowing --since/--from/--to (" <> reqSummary req <> ")")
httpExToError (HttpExceptionRequest req content) =
  APIError 0 (show content <> " (" <> reqSummary req <> ")")
httpExToError (InvalidUrlException url reason) =
  APIError 0 ("invalid URL " <> toText url <> ": " <> toText reason)


-- | Cheap check for non-JSON gateway HTML so we don't echo a CF page back.
isHtml :: Text -> Bool
isHtml t =
  let s = T.toLower (T.take 256 (T.stripStart t))
   in "<!doctype html"
        `T.isPrefixOf` s
        || "<html"
        `T.isPrefixOf` s
        || "<!doctype"
        `T.isPrefixOf` s
        || "cloudflare"
        `T.isInfixOf` s


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
