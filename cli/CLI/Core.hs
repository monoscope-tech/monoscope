module CLI.Core
  ( OutputMode (..)
  , detectOutputMode
  , isInteractiveTTY
  , renderJSON
  , renderTable
  , printError
  , APIError (..)
  , apiGet
  , apiPost
  , withAPIResult
  ) where

import Relude

import CLI.Config (CLIConfig (..))
import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.Aeson.Encode.Pretty qualified as AE
import Data.Effectful.Wreq (HTTP, getWith, postWith, responseBody)
import Data.Effectful.Wreq qualified as W
import Data.Text qualified as T
import Data.Text.IO qualified
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as Env
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.Wreq qualified as Wreq
import Pkg.CLIFormat (colWidths, formatRow)
import System.Console.ANSI qualified as ANSI
import UnliftIO.Exception (catch)

-- Output

data OutputMode = OutputJSON | OutputTable
  deriving stock (Eq, Show)

detectOutputMode :: (Environment :> es, IOE :> es) => Bool -> Eff es OutputMode
detectOutputMode jsonFlag
  | jsonFlag = pure OutputJSON
  | otherwise = do
      tty <- isInteractiveTTY
      pure $ if tty then OutputTable else OutputJSON

-- | True when stdout is a TTY and no agent/CI env vars are set.
isInteractiveTTY :: (Environment :> es, IOE :> es) => Eff es Bool
isInteractiveTTY = do
  tty <- liftIO $ ANSI.hSupportsANSIColor stdout
  agent <- isAgentMode
  pure $ tty && not agent

isAgentMode :: (Environment :> es) => Eff es Bool
isAgentMode = any isJust <$> mapM Env.lookupEnv ["MONO_AGENT_MODE", "CLAUDE_CODE", "CI"]

renderJSON :: (AE.ToJSON a, IOE :> es) => a -> Eff es ()
renderJSON = liftIO . putLBSLn . AE.encodePretty

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

apiGet :: (HTTP :> es, IOE :> es) => CLIConfig -> Text -> [(Text, Text)] -> Eff es (Either APIError LByteString)
apiGet cfg path params = do
  let projectRoutes = ["/log_explorer"]
      needsPrefix = any (`T.isPrefixOf` path) projectRoutes
      (fullPath, params')
        | needsPrefix = (maybe "" (\p -> "/p/" <> p) cfg.projectId <> path, filter ((/= "pid") . fst) params)
        | otherwise = (path, params)
      url = toString (cfg.apiUrl <> fullPath)
      opts =
        W.defaults
          & W.header "Accept" .~ ["application/json"]
          & addAuth cfg.apiKey
          & addParams params'
  catch
    (Right . (^. responseBody) <$> getWith opts url)
    (pure . Left . httpExToError)

apiPost :: (HTTP :> es, IOE :> es) => Text -> Text -> [(Text, Text)] -> Eff es (Either APIError LByteString)
apiPost baseUrl path params = do
  let url = toString (baseUrl <> path)
      opts = W.defaults & W.header "Accept" .~ ["application/json"] & addParams params
  catch
    (Right . (^. responseBody) <$> postWith opts url ("" :: ByteString))
    (pure . Left . httpExToError)

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
