module ProcessMessage (
  processMessages,
  processMessages',
) where

import Colog.Core (LogAction (..), (<&))
import Control.Exception (try)
import Control.Lens ((^?), _Just)
import Control.Monad.Trans.Except (except, throwE)
import Control.Monad.Trans.Except.Extra (handleExceptT)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import Data.ByteString qualified as B
import Data.Cache qualified as Cache
import Data.Generics.Product (field)
import Data.List (unzip4)
import Data.Pool (Pool)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getZonedTime)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection, Query)
import Database.PostgreSQL.Transact (execute)
import Debug.Pretty.Simple
import Fmt
import Gogol.Data.Base64 (_Base64)
import Gogol.PubSub qualified as PubSub
import Log qualified
import Models.Apis.RequestDumps qualified as RequestDumps
import qualified Data.Text as T
import Data.ByteString.Lazy.Char8 qualified as BL
import Models.Projects.Projects qualified as Projects
import Relude hiding (hoistMaybe)
import RequestMessages qualified
import System.Clock
import System.Config qualified as Config
import System.Types (ATBackgroundCtx)
import Text.Pretty.Simple (pShow)
import Utils (DBField, eitherStrToText)


{--
  Exploring how the inmemory cache could be shaped for performance, and low footprint ability to skip hitting the postgres database when not needed.

  -- All vectors here should be sorted (or we could use sets), then we would be able to do conttainment and prefix searches via binary search (Works fast on sorted lists)
  -- It will likely be fine to just insert these projects into the cache with a short (eg 5mins) TTL, and then reload if from the db every 5 mins as needed.
  -- - This might not be most efficient, but we don't need to worry too much about keeping this data in sync but updating values in the cache based on live requests.
  -- - Over inserting into the database within that 5minute timeline should probably be a non-issue in comparison.
  -- Implementation should just be a function that accepts the cache type and a project id, then it checks if the project id exists or not. if it doesn't exist,
  -- it would run the query to get the project from the db in the exact shape we need, and fill up the cache before returning the value

  -- NOTE: It might be worth it to explore using sets as well for the project cache

  Project Cache structure.
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
processMessages :: Config.EnvConfig -> Pool Connection -> [PubSub.ReceivedMessage] -> Cache.Cache Projects.ProjectId Projects.ProjectCache -> ATBackgroundCtx [Maybe Text]
processMessages env conn' msgs projectCache = do
  let msgs' =
        msgs <&> \msg -> do
          let rmMsg = msg ^? field @"message" . _Just . field @"data'" . _Just . _Base64
          let jsonByteStr = fromMaybe "{}" rmMsg
          let sanitizedJsonStr = replaceNullChars $ decodeUtf8 $ jsonByteStr
          recMsg <- eitherStrToText $ eitherDecode $ BL.fromStrict $ encodeUtf8 sanitizedJsonStr
          Right (msg.ackId, recMsg)

  unless (null $ lefts msgs') do
    let leftMsgs = [(a, b) | (Left a, b) <- zip msgs' msgs]
    forM_ leftMsgs \(a, b) -> Log.logAttention "Error parsing json msgs" (object ["Error" .= a, "OriginalMsg" .= b])

  if null msgs'
    then pure []
    else processMessages' env conn' (rights msgs') projectCache

-- Replace null characters in a Text
replaceNullChars :: Text -> Text
replaceNullChars = T.replace "\\u0000" ""


wrapTxtException :: Text -> SomeException -> Text
wrapTxtException wrap e = " " <> wrap <> " : " <> (toText @String $ show e)


processMessages' :: Config.EnvConfig -> Pool Connection -> [(Maybe Text, RequestMessages.RequestMessage)] -> Cache.Cache Projects.ProjectId Projects.ProjectCache -> ATBackgroundCtx [Maybe Text]
processMessages' _ conn' msgs projectCache' = do
  startTime <- liftIO $ getTime Monotonic
  processed <- mapM (processMessage conn' projectCache') msgs
  let (rmAckIds, queries, params, reqDumps) = unzip4 $ rights processed
  let query' = mconcat queries
  let params' = concat params

  unless (null $ lefts processed) do
    let leftMsgs = [(a, b) | (Left a, b) <- zip processed msgs]
    forM_ leftMsgs \(a, b) -> Log.logAttention "processMessages': Error processing msgs" (object ["Error" .= a, "OriginalMsg" .= b])

  afterProccessing <- liftIO $ getTime Monotonic

  when (null reqDumps) $ Log.logAttention_ "Empty params/query for processMessages for request dumps; "

  resp <- withPool conn' $ runExceptT do
    unless (null params') $
      handleExceptT (wrapTxtException $ toStrict $ "execute query " <> show query') $
        void $
          execute query' params'
    unless (null reqDumps) $
      handleExceptT (wrapTxtException $ toStrict $ "bulkInsertReqDump => " <> show reqDumps <> show msgs) $
        void $
          RequestDumps.bulkInsertRequestDumps reqDumps

  endTime <- liftIO $ getTime Monotonic
  let msg = fmtLn @String $ "Process Message (" +| length msgs |+ ") pipeline microsecs: queryDuration " +| toNanoSecs (diffTimeSpec startTime afterProccessing) `div` 1000 |+ " -> processingDuration " +| toNanoSecs (diffTimeSpec afterProccessing endTime) `div` 1000 |+ " -> TotalDuration " +| toNanoSecs (diffTimeSpec startTime endTime) `div` 1000 |+ ""
  Log.logInfo_ (show msg)

  case resp of
    Left err -> do
      Log.logAttention "error executing RequestMessage derived Insert queries. \n" err
      pure []
    Right _ -> pure rmAckIds
  where
    projectCacheDefault :: Projects.ProjectCache
    projectCacheDefault = Projects.ProjectCache{hosts = [], endpointHashes = [], shapeHashes = [], redactFieldslist = []}

    processMessage
      :: Pool Connection
      -> Cache.Cache Projects.ProjectId Projects.ProjectCache
      -> (Maybe Text, RequestMessages.RequestMessage)
      -> ATBackgroundCtx (Either Text (Maybe Text, Query, [DBField], RequestDumps.RequestDump))
    processMessage conn projectCache (rmAckId, recMsg) = runExceptT do
      timestamp <- liftIO getCurrentTime
      let pid = Projects.ProjectId recMsg.projectId

      -- We retrieve the projectCache object from the inmemory cache and if it doesn't exist,
      -- we set the value in the db into the cache and return that.
      -- This should help with our performance, since this project Cache is the only information we need in order to process
      -- an apitoolkit requestmessage payload. So we're able to process payloads without hitting the database except for the actual db inserts.
      projectCacheValE <-
        liftIO $
          try
            ( Cache.fetchWithCache projectCache pid \pid' -> do
                mpjCache <- withPool conn $ Projects.projectCacheById pid'
                pure $ fromMaybe projectCacheDefault mpjCache
            )
          :: ExceptT Text ATBackgroundCtx (Either SomeException Projects.ProjectCache)

      case projectCacheValE of
        Left e -> throwE $ "An error occurred while fetching project cache: " <> show e
        Right projectCacheVal -> do
          recId <- liftIO nextRandom
          (query, params, reqDump) <- except $ RequestMessages.requestMsgToDumpAndEndpoint projectCacheVal recMsg timestamp recId
          pure (rmAckId, query, params, reqDump)
