{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Effectful.Wreq (
  HTTP (..),
  get,
  post,
  put,
  patch,
  delete,
  W.header,
  W.options,
  W.head_,
  getWith,
  postWith,
  putWith,
  patchWith,
  deleteWith,
  W.optionsWith,
  W.headWith,
  runHTTPWreq,
  runHTTPGolden,
  Options,
  Response,
  W.responseBody,
  W.defaults,
  Postable,
  Putable,
  Patchable,
) where

import Data.Aeson hiding (Options)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Effectful
import Effectful.Dispatch.Dynamic
import Network.HTTP.Client (createCookieJar, defaultRequest)
import Network.HTTP.Client.Internal (Response (..), ResponseClose (..))
import Network.HTTP.Types.Status (Status (..), statusCode, statusMessage)
import Network.HTTP.Types.Version (http11)
import Network.Wreq qualified as W
import Network.Wreq.Types qualified as W
import Relude hiding (get, put)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))


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
  Head :: String -> HTTP m (Response LBS.ByteString)
  GetWith :: Options -> String -> HTTP m (Response LBS.ByteString)
  PostWith :: Postable a => Options -> String -> a -> HTTP m (Response LBS.ByteString)
  PutWith :: Putable a => Options -> String -> a -> HTTP m (Response LBS.ByteString)
  PatchWith :: Patchable a => Options -> String -> a -> HTTP m (Response LBS.ByteString)
  DeleteWith :: Options -> String -> HTTP m (Response LBS.ByteString)
  OptionsWith :: Options -> String -> HTTP m (Response LBS.ByteString)
  HeadWith :: Options -> String -> HTTP m (Response LBS.ByteString)


type instance DispatchOf HTTP = 'Dynamic


-- API functions
get :: HTTP :> es => String -> Eff es (Response LBS.ByteString)
get url = send (Get url)


post :: (HTTP :> es, Postable a) => String -> a -> Eff es (Response LBS.ByteString)
post url body = send (Post url body)


put :: (HTTP :> es, Putable a) => String -> a -> Eff es (Response LBS.ByteString)
put url body = send (Put url body)


patch :: (HTTP :> es, Patchable a) => String -> a -> Eff es (Response LBS.ByteString)
patch url body = send (Patch url body)


delete :: HTTP :> es => String -> Eff es (Response LBS.ByteString)
delete url = send (Delete url)


-- options :: HTTP :> es => String -> Eff es (Response ())
-- options url = send (Options url)

-- head_ :: HTTP :> es => String -> Eff es (Response LBS.ByteString)
-- head_ url = send (Head url)

getWith :: HTTP :> es => Options -> String -> Eff es (Response LBS.ByteString)
getWith opts url = send (GetWith opts url)


postWith :: (HTTP :> es, Postable a) => Options -> String -> a -> Eff es (Response LBS.ByteString)
postWith opts url body = send (PostWith opts url body)


putWith :: (HTTP :> es, Putable a) => Options -> String -> a -> Eff es (Response LBS.ByteString)
putWith opts url body = send (PutWith opts url body)


patchWith :: (HTTP :> es, Patchable a) => Options -> String -> a -> Eff es (Response LBS.ByteString)
patchWith opts url body = send (PatchWith opts url body)


deleteWith :: HTTP :> es => Options -> String -> Eff es (Response LBS.ByteString)
deleteWith opts url = send (DeleteWith opts url)


-- optionsWith :: HTTP :> es => Options -> String -> Eff es (Response LBS.ByteString)
-- optionsWith opts url = send (OptionsWith opts url)

-- headWith :: HTTP :> es => Options -> String -> Eff es (Response LBS.ByteString)
-- headWith opts url = send (HeadWith opts url)

-- Interpreters
runHTTPWreq :: IOE :> es => Eff (HTTP ': es) a -> Eff es a
runHTTPWreq = interpret $ \_ -> \case
  Get url -> liftIO $ W.get url
  Post url body -> liftIO $ W.post url body
  Put url body -> liftIO $ W.put url body
  Patch url body -> liftIO $ W.patch url body
  Delete url -> liftIO $ W.delete url
  -- Options url -> liftIO $ W.options url
  -- Head url -> liftIO $ W.head_ url
  GetWith opts url -> liftIO $ W.getWith opts url
  PostWith opts url body -> liftIO $ W.postWith opts url body
  PutWith opts url body -> liftIO $ W.putWith opts url body
  PatchWith opts url body -> liftIO $ W.patchWith opts url body
  DeleteWith opts url -> liftIO $ W.deleteWith opts url
  _ -> error "unimplemented"


-- OptionsWith opts url -> liftIO $ W.optionsWith opts url
-- HeadWith opts url -> liftIO $ W.headWith opts url

runHTTPGolden :: IOE :> es => FilePath -> Eff (HTTP ': es) a -> Eff es a
runHTTPGolden goldenDir = interpret $ \_ -> \case
  Get url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_get.json") (W.get url)
  Post url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_post.json") (W.post url body)
  Put url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_put.json") (W.put url body)
  Patch url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_patch.json") (W.patch url body)
  Delete url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_delete.json") (W.delete url)
  -- Options url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_options.json") (W.options url)
  -- Head url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_head.json") (W.head_ url)
  GetWith opts url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_get_with.json") (W.getWith opts url)
  PostWith opts url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_post_with.json") (W.postWith opts url body)
  PutWith opts url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_put_with.json") (W.putWith opts url body)
  PatchWith opts url body -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_patch_with.json") (W.patchWith opts url body)
  DeleteWith opts url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_delete_with.json") (W.deleteWith opts url)
  _ -> error "unimplemented"


-- OptionsWith opts url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_options_with.json") (W.optionsWith opts url)
-- HeadWith opts url -> liftIO $ getOrCreateGoldenResponse goldenDir (sanitizeFileName url <> "_head_with.json") (W.headWith opts url)

-- Helper functions
sanitizeFileName :: String -> String
sanitizeFileName = map (\c -> if c `elem` ['/', ':', '?', '&'] then '_' else c)


data WreqResponse = WreqResponse
  { statusCode :: Int
  , statusMessage :: Text
  , respBody :: Text
  , responseHeaders :: [(Text, Text)]
  , originalRequest :: Text
  , responseEarlyHints :: [(Text, Text)]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


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
    , respBody = decodeUtf8 r.responseBody
    , responseHeaders = convertHeaders r.responseHeaders
    , originalRequest = fromString (show r.responseOriginalRequest)
    , responseEarlyHints = convertHeaders r.responseEarlyHints
    }


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


getOrCreateGoldenResponse :: FilePath -> String -> IO (W.Response LBS.ByteString) -> IO (W.Response LBS.ByteString)
getOrCreateGoldenResponse goldenDir fileName action = do
  let filePath = goldenDir </> fileName
  exists <- doesFileExist filePath
  if exists
    then do
      content <- readFileLBS filePath
      case decode content of
        Just response -> return $ toWreqResponse response
        Nothing -> error $ fromString $ "Failed to decode response from file: " <> filePath
    else do
      createDirectoryIfMissing True goldenDir
      response <- action
      writeFileLBS filePath (encode $ fromWreqResponse response)
      return response
