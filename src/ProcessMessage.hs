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
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert), execute, withPool)
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
import Relude.Extra (elems)
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
    -- redactedFieldsMap <- handleIOExceptT (toText @String . show) $ withPool pool $ RedactedFields.redactedFieldsMapByProject pid
    -- For now, we will compare the key accross all endpoints, since we have no easy way to descriminate by endpoint at the moment
    -- Commented out because we want to use the redact fields in the project cache for this purpose.
    -- let shouldRedact = \val -> Map.foldr (\redactList acc -> Vector.elem val redactList || acc) False redactedFieldsMap
    -- let redactFieldsList = toList $ Vector.concat $ elems redactedFieldsMap
    -- (reqDump, endpoint, fields, format, shape) <- except $ RequestMessages.requestMsgToDumpAndEndpoint redactFieldsList shouldRedact requestMsg timestamp recId

    -- FIXME: Project cache should represent the current project and should come from the db or inmemory cache.
    let projectCache = Projects.ProjectCache {hosts = [], endpointHashes = ["abc"], shapeHashes = [], redactFieldslist = []}
    (query, params) <- except $ RequestMessages.requestMsgToDumpAndEndpoint projectCache requestMsg timestamp recId
    handleIOExceptT (toText @String . show) $
      withPool pool $ do
        _ <- execute Insert query params

        -- liftIO $ logger <& "🔥 logging redactedFieldsMap"
        -- liftIO $ logger <& show redactedFieldsMap

        -- TODO: Create inmemory cache with TTL and size limit of the endpoints and projects.
        -- Must include all shapes of a given endpoint, and also a hash of the endpoint.
        -- Let's use this to prevent processing requests that don't fail our model matching logic
        -- Operational Behaviour:
        --
        -- Question: What if we have a large enough bloom filter? with some randomness to fight around the false positives.
        -- Using a bloom filter or any fancy techniques might be difficult to pull off,
        -- because the number of examples in the shape are very important before we can start skipping them.
        -- Maybe we can have a cache that keeps a count of how many times we've seen that shape, and start allowing the shape through, after the first 10 or 20 requests.

        -- FIXME: future steps should not depend on on the enpID. Maybe it should not be returned.
        -- incomplete thought: Could we also have in the projects cache, the list of hosts? (maybe hashes?), so that we can skip updating endpoints host
        -- What if we have a cache that holds projects, and one that holds endpoints, and then hosts of the endpoints and the shapes for that endpoint in memory,
        -- And use that to skip inserting endpoints or all of the other operations.
        -- This inmemory structure will have huge runtime usage, and we need to put in thought into what we're using, to ensure that performance doesn't degrade.
        -- (Just enpID) <- Endpoints.upsertEndpoints endpoint
        -- FIXME: upsertFields doesn't need to return the fieldids and the format ids anymore if we rely on their hashes.
        -- FIXME: reimplement based of fields, and format from above.
        -- (formatIds, fieldIds) <- unzip <$> Fields.upsertFields enpID fields
        -- let shape' = shape & #endpointId .~ enpID
        -- Shapes.insertShape shape
        -- TODO: delete; commented out as unneeded
        -- let reqDump' =
        --       reqDump & #shapeId .~ shapeId
        --         & #formatIds .~ Vector.fromList formatIds
        --         & #fieldIds .~ Vector.fromList fieldIds

        -- liftIO $ logger <& "Before insert req dump"
        -- _ <- RequestDumps.insertRequestDump reqDump
        pass

  case resp of
    Left err -> logger <& "error with converting request message to dump and endpoint" <> toString err
    Right _ -> pass

{--
  Exploring how the inmemory cache could be shaped for performance, and low footprint ability to skip hitting the postgres database when not needed.

 Raw project ID.
 { id :: ProjectId,
   createdAt :: ZonedTime,
   updatedAt :: ZonedTime,
   deletedAt :: Maybe ZonedTime,
   active :: Bool,
   title :: Text,
   description :: Text,
   hosts :: Vector.Vector Text
 }

 HashMap?
  -- All vectors here should be sorted, then we would be able to do conttainment and prefix searches via binary search (Works fast on sorted lists)
  -- It will likely be fine to just insert these projects into the cache with a short (eg 5mins) TTL, and then reload if from the db every 5 mins as needed.
  -- - This might not be most efficient, but we don't need to worry too much about keeping this data in sync but updating values in the cache based on live requests.
  -- - Over inserting into the database within that 5minute timeline should probably be a non-issue in comparison.
  -- Implementation should just be a function that accepts the cache type and a project id, then it checks if the project id exists or not. if it doesn't exist,
  -- it would run the query to get the project from the db in the exact shape we need, and fill up the cache before returning the value
 <projectID> =
   {
      -- Used for the dashboards on every page. The title is displayed on the sidebar.
      -- Title  is also not requeired since in the sidebar, we also show the list of projects,
      -- and it's easier to just deal with that via a db call for list of projects, and use that for all project disply actions.
      Title ""
      -- We need this hosts to mirrow all the hosts in the endpoints table, and could use this for validation purposes to skip inserting endpoints just because of hosts
      -- if endpoint exists but host is not in this list, then we have a query specifically for inserting hosts.
      hosts []
      -- maybe we don't need this? See the next point.
      endpoint_hashes []
      - Since shapes always have the endpoints hash prepended to them, maybe we don't need to store the hash of endpoints, since we can derive that from the shapes.
      shape_hashes []
      --
      -- We could also only hold the hash of formatids.
      -- Since all the hashes are computed and concatenated making it a clear heirarchy/prefix tree?. T
      -- his might be more space efficient to store, at the cost of compute/complexity in searching for them.
      -- Also, maybe instead of shapes being the important unit for checking that we have a shape already, we could just use format_hashes for that. TO make sure every format is not new as well.
      format_hashes []
      --
      -- We check if every request is part of the redact list, so it's better if we don't need to  hit the db for them with each request.
      redact_fields_list []
     }
      -- The only problem is, how do we deal with counts for these format_hashes to make sure we have enough examples for that format_hash?
      -- Should we postfix the count of field examples to it when building this list?
      -- Then only actually skip inserting them if the postfixed number is equal to the maax number of examples per field which we expect.
      -- Next issue with this mode is:
      --    - How do we deal with fields which have a fixed number of examples or value set, which is less than the total number of max examples?
      --      Eg a field has 2 possible answers: yes or no, but then our max is 10, so we keep checking if it has up to 10 fields, but then because it doesn't, we never skip the database operation. 🤔
      --
      -- A solution to this issue could be to have another hash map, but this time, linking:
      <shape_hash> => <ioref counter>
      -- So i can track the count of for that shape in the current session, and allo up to 100 or 200 or more requests per shape but cap it there.

  How to deal with tracking preventing sending field and format updates when we don't need them.
  ==============================
  A workaround to maintaining the count of processed items for a shape, is to intead, deal with endpoints and just maintain number of tracked calls per endpoint.
  It can be a sufficiently large number, to workaround for the probabilities.
  So, we could maintain another inmemory cache that holds endpoints and their counts in memory. It's easy to deterministically  get the hash of an endpoint,
  so we could simply calculate the endpoint hash much earlier, then use it to lookup and incremement the count and return the number. atomically.
  Also, instead of passing this count into the requestMessage function, we can instead pass precalculate everything and just pass in a boolean in there as an argument.
    The advantage of doing this is that we can do other calculations, such as throwing a dice and randomly deciding to still send the request to the db.
    Sending random requests to the DB might still help us catch new fields(formats) that we didnt know about, etc.

    We could also maintain hashes of all the formats in the cache, and check each field format within this list.🤔
 --}
