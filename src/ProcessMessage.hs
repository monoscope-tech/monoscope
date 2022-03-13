module ProcessMessage
  ( processMessage,
    processRequestMessage,
  )
where

import Colog.Core (LogAction (..), (<&))
import Config qualified
import Control.Lens ((^?), _Just)
import Control.Lens qualified as L
import Data.Aeson (eitherDecode)
import Data.ByteString.Base64 qualified as B64
import Data.Pool (Pool)
import Data.Time.LocalTime (getZonedTime)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Network.Google.PubSub qualified as PubSub
import Optics.Core ((.~))
import Relude
import Relude.Unsafe (fromJust)
import RequestMessages qualified

processMessage :: LogAction IO String -> Config.EnvConfig -> Pool Connection -> PubSub.ReceivedMessage -> IO (Maybe Text)
processMessage logger _ conn msg = do
  let rmMsg = msg ^? PubSub.rmMessage . _Just . PubSub.pmData . _Just
  let decodedMsg = B64.decodeBase64 $ fromJust rmMsg
  let jsonByteStr = fromRight "{}" decodedMsg
  let decodedMsgE = eitherDecode (fromStrict jsonByteStr) :: Either String RequestMessages.RequestMessage
  case decodedMsgE of
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
      let (reqDump, endpoint, fields, shape) = dAndE
      withPool pool $ do
        enpResp <- Endpoints.upsertEndpoints endpoint
        case enpResp of
          Nothing -> error "error"
          Just enpID -> do
            _ <- Fields.upsertFields enpID fields
            let shape' = shape & #endpointId .~ enpID
            shapeIdM <- Shapes.insertShape shape'
            case shapeIdM of
              Nothing -> error "error"
              Just (Only shapeId) -> do
                let reqDump' = reqDump & #shapeId .~ shapeId
                RequestDumps.insertRequestDump reqDump'
                pass
