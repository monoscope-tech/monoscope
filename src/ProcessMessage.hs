module ProcessMessage
  ( processMessage,
    processRequestMessage,
  )
where

import Colog.Core (LogAction (..), logStringStdout, (<&))
import qualified Config
import Control.Lens ((^?), _Just)
import qualified Control.Lens as L
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Base64 as B64
import Data.Pool (Pool, withResource)
import qualified Data.Text.Encoding.Base64 as B64T
import Data.Time.LocalTime (getZonedTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection)
import qualified Models.Apis.Endpoints as Endpoints
import qualified Models.Apis.Fields as Fields
import qualified Models.Apis.RequestDumps as RequestDumps
import qualified Network.Google.PubSub as PubSub
import Relude
import Relude.Unsafe (fromJust)
import qualified RequestMessages

processMessage :: LogAction IO String -> Config.EnvConfig -> Pool Connection -> PubSub.ReceivedMessage -> IO (Maybe Text)
processMessage logger envConfig conn msg = do
  let rmMsg = msg ^? PubSub.rmMessage . _Just . PubSub.pmData . _Just
  let decodedMsg = B64.decodeBase64 $ fromJust rmMsg
  let jsonByteStr = fromRight "{}" decodedMsg
  let decodedMsg = eitherDecode (fromStrict jsonByteStr) :: Either String RequestMessages.RequestMessage
  case decodedMsg of
    Left err -> logger <& "error decoding message " <> err
    Right recMsg -> processRequestMessage logger conn recMsg
  pure $ msg L.^. PubSub.rmAckId

-- | processRequestMessage would take a request message and
-- process it into formats which can be persisited into the database.
-- it processes the request message into the request dump,
-- the endpoint struct and then the request fields.
processRequestMessage :: LogAction IO String -> Pool Connection -> RequestMessages.RequestMessage -> IO ()
processRequestMessage logger pool requestMsg = do
  recId <- nextRandom
  timestamp <- getZonedTime
  let dumpAndEndpoint = RequestMessages.requestMsgToDumpAndEndpoint requestMsg timestamp recId
  case dumpAndEndpoint of
    Left err -> logger <& "error with converting request message to dump and endpoint" <> toString err
    Right dAndE -> do
      let (reqDump, endpoint, fields) = dAndE
      withPool pool $ do
        RequestDumps.insertRequestDump reqDump
        enpResp <- Endpoints.upsertEndpoints endpoint
        case enpResp of
          Nothing -> error "error"
          Just (enpID, _, _) -> do
            Fields.upsertFields enpID fields
            pass
