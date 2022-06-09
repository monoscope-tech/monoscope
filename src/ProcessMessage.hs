module ProcessMessage
  ( processMessage,
    processRequestMessage,
  )
where

import Colog.Core (LogAction (..), (<&))
import Config qualified
import Control.Lens ((^?), _Just)
import Control.Lens qualified as L
import Control.Monad.Trans.Except
import Control.Monad.Trans.Except.Extra (handleIOExceptT)
import Data.Aeson (eitherDecode)
import Data.ByteString.Base64 qualified as B64
import Data.Map qualified as Map
import Data.Pool (Pool)
import Data.Time.LocalTime (getZonedTime)
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Projects.RedactedFields qualified as RedactedFields
import Network.Google.PubSub qualified as PubSub
import Optics.Core ((.~), (^.))
import Relude hiding (hoistMaybe)
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
  let pid = Projects.ProjectId (requestMsg ^. #projectId)
  resp <- runExceptT $ do
    redactedFieldsMap <- handleIOExceptT (toText @String . show) $ withPool pool $ RedactedFields.redactedFieldsMapByProject pid
    -- For now, we will compare the key accross all endpoints, since we have no easy way to descriminate by endpoint at the moment
    let shouldRedact = \val -> Map.foldr (\redactList acc -> Vector.elem val redactList || acc) False redactedFieldsMap
    (reqDump, endpoint, fields, shape) <- except $ RequestMessages.requestMsgToDumpAndEndpoint shouldRedact requestMsg timestamp recId
    handleIOExceptT (toText @String . show) $
      withPool pool $ do
        liftIO $ logger <& "🔥 logging redactedFieldsMap"
        liftIO $ logger <& show redactedFieldsMap

        (Just enpID) <- Endpoints.upsertEndpoints endpoint
        (formatIds, fieldIds) <- unzip <$> Fields.upsertFields enpID fields
        let shape' = shape & #endpointId .~ enpID
        (Just (Only shapeId)) <- Shapes.insertShape shape'
        let reqDump' =
              reqDump & #shapeId .~ shapeId
                & #formatIds .~ Vector.fromList formatIds
                & #fieldIds .~ Vector.fromList fieldIds

        liftIO $ logger <& "Before insert req dump"
        _ <- RequestDumps.insertRequestDump reqDump'
        pass

  case resp of
    Left err -> logger <& "error with converting request message to dump and endpoint" <> toString err
    Right _ -> pass
