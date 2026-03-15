module Pkg.CLITestUtils (
  runHTTPtoServant,
  testPid,
) where

import Relude hiding (get, put)

import Control.Lens ((^.))
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.Wreq (HTTP (..))
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Effectful
import Effectful.Dispatch.Dynamic
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Network.HTTP.Client (createCookieJar, defaultRequest)
import Network.HTTP.Client.Internal (Response (..), ResponseClose (..))
import Network.HTTP.Types.Status (ok200)
import Network.HTTP.Types.Version (http11)
import Network.Wreq qualified as W
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.Log qualified as Log
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils (TestResources (..), runQueryEffect, testServant)


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


mockResponse :: LBS.ByteString -> Response LBS.ByteString
mockResponse body =
  Response
    { responseStatus = ok200
    , responseVersion = http11
    , responseHeaders = [("Content-Type", "application/json")]
    , responseBody = body
    , responseCookieJar = createCookieJar []
    , responseClose' = ResponseClose pass
    , responseOriginalRequest = defaultRequest
    , responseEarlyHints = []
    }


extractParams :: W.Options -> [(Text, Text)]
extractParams opts = opts ^. W.params


-- | Test HTTP interpreter that routes CLI requests to server handlers.
-- Intercepts GetWith and routes based on URL path to the real handlers.
runHTTPtoServant :: IOE :> es => TestResources -> Eff (HTTP ': es) a -> Eff es a
runHTTPtoServant tr = interpret $ \_ -> \case
  GetWith opts url -> liftIO $ routeRequest tr (extractPath url) (extractParams opts)
  Get url -> liftIO $ routeRequest tr (extractPath url) []
  _ -> error "runHTTPtoServant: only GET is supported for CLI tests"


extractPath :: String -> Text
extractPath url =
  let t = toText url
      -- Strip scheme + host to get path
      afterScheme = fromMaybe t $ T.stripPrefix "https://" t <|> T.stripPrefix "http://" t
      path = T.dropWhile (/= '/') afterScheme
   in if T.null path then "/" else T.takeWhile (/= '?') path


lookupParam :: Text -> [(Text, Text)] -> Maybe Text
lookupParam key = fmap snd . find ((== key) . fst)


routeRequest :: TestResources -> Text -> [(Text, Text)] -> IO (Response LBS.ByteString)
routeRequest tr path params
  | "/log_explorer/schema" `T.isPrefixOf` path =
      pure $ mockResponse $ AE.encode Schema.telemetrySchemaJson
  | "/log_explorer" `T.isPrefixOf` path = do
      let p = lookupParam
          jsonParam = p "json" params
          query = p "query" params
          since = p "since" params
          from = p "from" params
          to = p "to" params
          source = p "source" params
      (_, pg) <-
        testServant tr
          $ Log.apiLogH testPid query Nothing Nothing since from to Nothing source Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing jsonParam Nothing Nothing Nothing Nothing
      pure $ mockResponse $ AE.encode pg
  | "/chart_data" `T.isPrefixOf` path = do
      let p = lookupParam
          query = p "query" params
          since = p "since" params
          from = p "from" params
          to = p "to" params
          source = p "source" params
      result <-
        runQueryEffect tr
          $ Charts.queryMetrics Nothing (Just testPid) query Nothing since from to source []
      pure $ mockResponse $ AE.encode result
  | otherwise = error $ "runHTTPtoServant: unhandled path: " <> path
