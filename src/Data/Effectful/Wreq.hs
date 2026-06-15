module Data.Effectful.Wreq (
  HTTP (..),
  get,
  post,
  put,
  patch,
  delete,
  options,
  head_,
  getWith,
  postWith,
  putWith,
  patchWith,
  deleteWith,
  optionsWith,
  headWith,
  W.header,
  runHTTPWreq,
  runHTTPGolden,
  Options,
  Response,
  W.responseBody,
  W.defaults,
  Postable,
  Putable,
  Patchable,
  withGoldenCache,
) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), createCookieJar, defaultRequest)
import Network.HTTP.Client.Internal (Response (..), ResponseClose (..))
import Network.HTTP.Types.Status (Status (..), statusCode, statusMessage)
import Network.HTTP.Types.Version (http11)
import Network.Wreq qualified as W
import Network.Wreq.Types qualified as W
import Relude hiding (get, put)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Regex.TDFA ((=~))
import UnliftIO.Exception (throwIO, try)


-- Re-export types from Wreq
type Options = W.Options


-- type Response = W.Response
type Postable = W.Postable
type Putable = W.Putable
type Patchable = W.Patchable


type role HTTP phantom nominal
data HTTP :: Effect where
  Get :: String -> HTTP m (Response LBS.ByteString)
  Post :: Postable a => String -> a -> HTTP m (Response LBS.ByteString)
  Put :: Putable a => String -> a -> HTTP m (Response LBS.ByteString)
  Patch :: Patchable a => String -> a -> HTTP m (Response LBS.ByteString)
  Delete :: String -> HTTP m (Response LBS.ByteString)
  Options :: String -> HTTP m (Response LBS.ByteString)
  Head_ :: String -> HTTP m (Response LBS.ByteString)
  GetWith :: Options -> String -> HTTP m (Response LBS.ByteString)
  PostWith :: Postable a => Options -> String -> a -> HTTP m (Response LBS.ByteString)
  PutWith :: Putable a => Options -> String -> a -> HTTP m (Response LBS.ByteString)
  PatchWith :: Patchable a => Options -> String -> a -> HTTP m (Response LBS.ByteString)
  DeleteWith :: Options -> String -> HTTP m (Response LBS.ByteString)
  OptionsWith :: Options -> String -> HTTP m (Response LBS.ByteString)
  HeadWith :: Options -> String -> HTTP m (Response LBS.ByteString)


type instance DispatchOf HTTP = 'Dynamic


makeEffect ''HTTP


-- Interpreters
runHTTPWreq :: IOE :> es => Eff (HTTP ': es) a -> Eff es a
runHTTPWreq = interpret $ \_ -> \case
  Get url -> liftIO $ W.get url
  Post url body -> liftIO $ W.post url body
  Put url body -> liftIO $ W.put url body
  Patch url body -> liftIO $ W.patch url body
  Delete url -> liftIO $ W.delete url
  GetWith opts url -> liftIO $ W.getWith opts url
  PostWith opts url body -> liftIO $ W.postWith opts url body
  PutWith opts url body -> liftIO $ W.putWith opts url body
  PatchWith opts url body -> liftIO $ W.patchWith opts url body
  DeleteWith opts url -> liftIO $ W.deleteWith opts url
  _ -> error "unimplemented"


runHTTPGolden :: IOE :> es => FilePath -> Eff (HTTP ': es) a -> Eff es a
runHTTPGolden goldenDir = interpret $ \_ -> \case
  Get url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_get.json") (W.get url)
  Post url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_post.json") (W.post url body)
  Put url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_put.json") (W.put url body)
  Patch url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_patch.json") (W.patch url body)
  Delete url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_delete.json") (W.delete url)
  GetWith opts url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_get_with.json") (W.getWith opts url)
  PostWith opts url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_post_with.json") (W.postWith opts url body)
  PutWith opts url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_put_with.json") (W.putWith opts url body)
  PatchWith opts url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_patch_with.json") (W.patchWith opts url body)
  DeleteWith opts url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_delete_with.json") (W.deleteWith opts url)
  _ -> error "unimplemented"


-- Helper functions
sanitizeFileName :: String -> String
sanitizeFileName = redactSecrets . map (\c -> if c `elem` (['/', ':', '?', '&'] :: [Char]) then '_' else c)


-- | Redact known secret patterns from golden file names and content to prevent
-- accidentally committing credentials. Replaces Twilio Account SIDs (AC...)
-- and Auth Tokens with fixed placeholders.
redactSecrets :: String -> String
redactSecrets s = case s =~ ("AC[a-f0-9]{32}" :: String) :: (String, String, String) of
  (_, "", _) -> s
  (before, _, after) -> before <> "ACREDACTED00000000000000000000000" <> redactSecrets after


data WreqResponse = WreqResponse
  { statusCode :: Int
  , statusMessage :: Text
  , respBody :: Text
  , responseHeaders :: [(Text, Text)]
  , originalRequest :: Text
  , responseEarlyHints :: [(Text, Text)]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Convert HTTP headers to a more serializable form
convertHeaders :: [(CI.CI ByteString, ByteString)] -> [(Text, Text)]
convertHeaders = map (\(k, v) -> (decodeUtf8 (CI.original k), decodeUtf8 v))


convertHeadersBack :: [(Text, Text)] -> [(CI.CI ByteString, ByteString)]
convertHeadersBack = map (\(k, v) -> (CI.mk (encodeUtf8 k), encodeUtf8 v))


-- Convert from WreqResponse to Response LBS.ByteString
fromWreqResponse :: Response LBS.ByteString -> WreqResponse
fromWreqResponse r =
  WreqResponse
    { statusCode = r.responseStatus.statusCode
    , statusMessage = decodeUtf8 r.responseStatus.statusMessage
    , respBody = redactSecretsT $ decodeUtf8 r.responseBody
    , responseHeaders = convertHeaders r.responseHeaders
    , originalRequest = redactSecretsT $ fromString (show r.responseOriginalRequest)
    , responseEarlyHints = convertHeaders r.responseEarlyHints
    }


redactSecretsT :: Text -> Text
redactSecretsT = fromString . redactSecrets . toString


-- Convert from WreqResponse to Response LBS.ByteString
toWreqResponse :: WreqResponse -> Response LBS.ByteString
toWreqResponse wr =
  Response
    { responseStatus = Status wr.statusCode (encodeUtf8 wr.statusMessage)
    , responseVersion = http11
    , responseHeaders = convertHeadersBack wr.responseHeaders
    , responseBody = encodeUtf8 wr.respBody
    , responseCookieJar = createCookieJar []
    , responseClose' = ResponseClose pass
    , responseOriginalRequest = defaultRequest -- Unsafe.read (toString (wr.originalRequest)) :: Request
    , responseEarlyHints = convertHeadersBack wr.responseEarlyHints
    }


-- | Generic golden-cache read-or-create. When the golden file exists (and we're
-- not refreshing) it's decoded into the cache type @c@ and projected to the
-- result via @decodeResult@; if the file is missing it errors with the standard
-- UPDATE_GOLDEN hint; on UPDATE_GOLDEN it runs @action@ to produce the cache
-- value and the live result, encodes the cache value, and returns the live
-- result (so capture-path behaviour matches each caller, not the round-trip).
withGoldenCache
  :: (AE.FromJSON c, AE.ToJSON c)
  => FilePath
  -- ^ golden dir
  -> String
  -- ^ file name (within dir)
  -> (FilePath -> String)
  -- ^ build the decode-failure message from the file path
  -> (c -> a)
  -- ^ project cached value to result (cached-read path)
  -> IO (c, a)
  -- ^ produce the cache value (to persist) and the live result (to return)
  -> IO a
withGoldenCache goldenDir fileName decodeErr decodeResult action = do
  let filePath = goldenDir </> fileName
  exists <- doesFileExist filePath
  updateGolden <- isJust <$> lookupEnv "UPDATE_GOLDEN"
  if exists && not updateGolden
    then do
      content <- readFileLBS filePath
      case AE.decode content of
        Just cached -> return $ decodeResult cached
        Nothing -> error $ fromString $ decodeErr filePath
    else
      if not exists && not updateGolden
        then
          error
            $ fromString
            $ "Golden file not found: "
            <> filePath
            <> "\nRun tests with UPDATE_GOLDEN=true to create it:\n"
            <> "  UPDATE_GOLDEN=true USE_EXTERNAL_DB=true cabal test integration-tests"
        else do
          createDirectoryIfMissing True goldenDir
          (cached, result) <- action
          writeFileLBS filePath (AE.encode cached)
          return result


getOrCreateGoldenResponse :: FilePath -> String -> IO (W.Response LBS.ByteString) -> IO (W.Response LBS.ByteString)
getOrCreateGoldenResponse goldenDir fileName action =
  withGoldenCache goldenDir fileName (\fp -> "Failed to decode response from file: " <> fp) toWreqResponse $ do
    -- Catch HTTP exceptions and convert them to responses
    result <- try action
    response <- case result of
      Right resp -> return resp
      Left (HttpExceptionRequest _ (StatusCodeException resp body)) ->
        -- Convert 4xx/5xx responses to normal responses for golden files
        return resp{responseBody = fromStrict body}
      Left (ex :: HttpException) -> throwIO ex -- Re-throw other exceptions
    pure (fromWreqResponse response, response)
