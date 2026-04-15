module CLI.Core
  ( OutputMode (..)
  , detectOutputMode
  , isInteractiveTTY
  , renderJSON
  , renderYAML
  , renderByMode
  , renderTable
  , printError
  , APIError (..)
  , apiGet
  , apiGetJson
  , apiPost
  , apiPostJson
  , apiPutJson
  , apiPatchJson
  , apiDelete
  , withAPIResult
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
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
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

isAgentMode :: (Environment :> es) => Eff es Bool
isAgentMode = any isJust <$> mapM Env.lookupEnv ["MONO_AGENT_MODE", "CLAUDE_CODE", "CI"]

renderJSON :: (AE.ToJSON a, IOE :> es) => a -> Eff es ()
renderJSON = liftIO . putLBSLn . AE.encodePretty

renderYAML :: (AE.ToJSON a, IOE :> es) => a -> Eff es ()
renderYAML = liftIO . putBSLn . Yaml.encode

-- | Render using the chosen mode; table mode falls back to JSON if no table rows supplied.
renderByMode :: (AE.ToJSON a, IOE :> es) => OutputMode -> Maybe ([Text], [[Text]]) -> a -> Eff es ()
renderByMode mode tableM v = case mode of
  OutputJSON -> renderJSON v
  OutputYAML -> renderYAML v
  OutputTable -> maybe (renderJSON v) (uncurry renderTable) tableM

renderTable :: (IOE :> es) => [Text] -> [[Text]] -> Eff es ()
renderTable headers rows = liftIO $ do
  let allRows = headers : rows
      widths = colWidths allRows
  ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  putTextLn $ formatRow widths headers
  ANSI.setSGR [ANSI.Reset]
  putTextLn $ T.intercalate "  " [T.replicate w "-" | w <- widths]
  mapM_ (putTextLn . formatRow widths) rows

printError :: (IOE :> es) => Text -> Eff es ()
printError msg = liftIO $ do
  ANSI.hSetSGR stderr [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  Data.Text.IO.hPutStrLn stderr $ "error: " <> msg
  ANSI.hSetSGR stderr [ANSI.Reset]

-- Client

data APIError = APIError {statusCode :: Int, message :: Text}
  deriving stock (Show)

-- | Build common request options (auth + project header + query params).
reqOpts :: CLIConfig -> [(Text, Text)] -> W.Options
reqOpts cfg params =
  W.defaults
    & W.header "Accept" .~ ["application/json"]
    & addAuth cfg.apiKey
    & addProjectId cfg.projectId
    & addParams params

apiGet :: (HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> Eff es (Either APIError LByteString)
apiGet cfg path params =
  catch
    (Right . (^. responseBody) <$> getWith (reqOpts cfg params) (toString $ cfg.apiUrl <> path))
    (pure . Left . httpExToError)

-- | GET returning a decoded JSON body.
apiGetJson :: (HTTP :> es, IOE :> es, AE.FromJSON a) => CLIConfig -> Text -> [(Text, Text)] -> Eff es (Either APIError a)
apiGetJson cfg path params = apiGet cfg path params >>= pure . decodeBody

apiPost :: (HTTP :> es, IOE :> es) => Text -> Text -> [(Text, Text)] -> Eff es (Either APIError LByteString)
apiPost baseUrl path params =
  catch
    ( Right . (^. responseBody)
        <$> postWith (W.defaults & W.header "Accept" .~ ["application/json"] & addParams params) (toString $ baseUrl <> path) ("" :: ByteString)
    )
    (pure . Left . httpExToError)

-- | POST with a JSON body, decoding JSON response.
apiPostJson :: (HTTP :> es, IOE :> es, AE.ToJSON a, AE.FromJSON b) => CLIConfig -> Text -> a -> Eff es (Either APIError b)
apiPostJson cfg path body = jsonCall (\o u b -> postWith o u b) cfg path body

apiPutJson :: (HTTP :> es, IOE :> es, AE.ToJSON a, AE.FromJSON b) => CLIConfig -> Text -> a -> Eff es (Either APIError b)
apiPutJson cfg path body = jsonCall (\o u b -> putWith o u b) cfg path body

apiPatchJson :: (HTTP :> es, IOE :> es, AE.ToJSON a, AE.FromJSON b) => CLIConfig -> Text -> a -> Eff es (Either APIError b)
apiPatchJson cfg path body = jsonCall (\o u b -> patchWith o u b) cfg path body

-- | DELETE; discards response body (returns unit on success).
apiDelete :: (HTTP :> es, IOE :> es) => CLIConfig -> Text -> Eff es (Either APIError ())
apiDelete cfg path =
  catch
    (deleteWith (reqOpts cfg []) (toString $ cfg.apiUrl <> path) $> Right ())
    (pure . Left . httpExToError)

jsonCall
  :: (HTTP :> es, IOE :> es, AE.ToJSON a, AE.FromJSON b)
  => (W.Options -> String -> LByteString -> Eff es (Wreq.Response LByteString))
  -> CLIConfig
  -> Text
  -> a
  -> Eff es (Either APIError b)
jsonCall action cfg path body =
  catch
    ( do
        let opts = reqOpts cfg [] & W.header "Content-Type" .~ ["application/json"]
            url = toString $ cfg.apiUrl <> path
        resp <- action opts url (AE.encode body)
        pure $ decodeBody (Right (resp ^. responseBody))
    )
    (pure . Left . httpExToError)

decodeBody :: AE.FromJSON a => Either APIError LByteString -> Either APIError a
decodeBody (Left e) = Left e
decodeBody (Right bs)
  | LBS.null bs = case AE.eitherDecode "null" of
      Left err -> Left $ APIError 0 (toText err)
      Right v -> Right v
  | otherwise = case AE.eitherDecode bs of
      Left err -> Left $ APIError 0 (toText err)
      Right v -> Right v

-- | Fetch JSON from API endpoint and apply a handler, printing errors on failure.
withAPIResult :: (HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> (AE.Value -> Eff es ()) -> Eff es ()
withAPIResult cfg path params onSuccess =
  apiGet cfg path params >>= \case
    Left err -> printError (show err) >> liftIO exitFailure
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
httpExToError (HttpExceptionRequest _ (StatusCodeException resp _)) =
  let code = resp ^. Wreq.responseStatus . Wreq.statusCode
      msg = decodeUtf8 $ resp ^. Wreq.responseStatus . Wreq.statusMessage
   in APIError code msg
httpExToError (HttpExceptionRequest _ (ConnectionFailure _)) =
  APIError 0 "Connection failed — is the server running?"
httpExToError e =
  APIError 0 (toText $ displayException e)
