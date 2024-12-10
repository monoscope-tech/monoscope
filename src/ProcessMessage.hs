{-# LANGUAGE ScopedTypeVariables #-}

module ProcessMessage (
  processMessages,
  processRequestMessages,
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Types (KeyValue ((.=)), object)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Cache qualified as Cache
import Data.List qualified as L
import Data.Text qualified as T
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as V
import Data.Vector.Algorithms qualified as VAA
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (SomePostgreSqlException)
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Log qualified
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Query qualified as Fields
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import PyF (fmt)
import Relude hiding (ask)
import RequestMessages qualified
import System.Clock (
  Clock (Monotonic),
  diffTimeSpec,
  getTime,
  toNanoSecs,
 )
import System.Config qualified as Config
import UnliftIO.Exception (try)
import Utils (eitherStrToText)


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
processMessages
  :: (Reader.Reader Config.AuthContext :> es, Time.Time :> es, DB :> es, Log :> es, IOE :> es)
  => [(Text, ByteString)]
  -> HashMap Text Text
  -> Eff es [Text]
processMessages [] _ = pure []
processMessages msgs attrs = do
  let msgs' =
        msgs <&> \(ackId, msg) -> do
          let sanitizedJsonStr = replaceNullChars $ decodeUtf8 msg
          recMsg <- eitherStrToText $ AE.eitherDecode $ BL.fromStrict $ encodeUtf8 sanitizedJsonStr
          Right (ackId, recMsg)

  unless (null $ lefts msgs') do
    let leftMsgs = [(a, b) | (Left a, b) <- zip msgs' msgs]
    forM_ leftMsgs \(a, (ackId, msg)) -> Log.logAttention "Error parsing json msgs" (object ["AckId" .= ackId, "Error" .= a, "OriginalMsg" .= decodeUtf8 @Text msg])

  if null msgs'
    then pure []
    else processRequestMessages (rights msgs')


-- Replace null characters in a Text
replaceNullChars :: Text -> Text
replaceNullChars = T.replace "\\u0000" ""


processRequestMessages
  :: (Reader.Reader Config.AuthContext :> es, Time.Time :> es, DB :> es, Log :> es, IOE :> es)
  => [(Text, RequestMessages.RequestMessage)]
  -> Eff es [Text]
processRequestMessages msgs = do
  startTime <- liftIO $ getTime Monotonic
  !processed <- forM msgs \(rmAckId, msg) -> do
    resp <- processRequestMessage msg
    pure $ case resp of
      Left err -> Left (err, rmAckId, msg)
      Right (rd, enp, s, f, fo, err) -> Right (rd, enp, s, f, fo, err, rmAckId)

  let !(failures, successes) = partitionEithers processed
      !(reqDumps, endpoints, shapes, fields, formats, errs, rmAckIds) = L.unzip7 successes
  let !reqDumpsFinal = catMaybes reqDumps
  let !endpointsFinal = VAA.nubBy (comparing (.hash)) $ V.fromList $ catMaybes endpoints
  let !shapesFinal = VAA.nubBy (comparing (.hash)) $ V.fromList $ catMaybes shapes
  let !fieldsFinal = VAA.nubBy (comparing (.hash)) $ V.concat fields
  let !formatsFinal = VAA.nubBy (comparing (.hash)) $ V.concat formats
  let !errsFinal = VAA.nubBy (comparing (.hash)) $ V.concat errs

  forM_ failures $ \(err, rmAckId, msg) ->
    Log.logAttention "Error processing message" (object ["Error" .= err, "AckId" .= rmAckId, "OriginalMsg" .= msg])

  afterProcessing <- liftIO $ getTime Monotonic
  result <- try do
    unless (null reqDumpsFinal) $ RequestDumps.bulkInsertRequestDumps reqDumpsFinal
    unless (null endpointsFinal) $ Endpoints.bulkInsertEndpoints endpointsFinal
    unless (null shapesFinal) $ Shapes.bulkInsertShapes shapesFinal
    unless (null fieldsFinal) $ Fields.bulkInsertFields fieldsFinal
    unless (null formatsFinal) $ Formats.bulkInsertFormat formatsFinal
    unless (null errsFinal) $ Anomalies.bulkInsertErrors errsFinal
  endTime <- liftIO $ getTime Monotonic
  let processingTime = toNanoSecs (diffTimeSpec startTime afterProcessing) `div` 1000
  let queryTime = toNanoSecs (diffTimeSpec afterProcessing endTime) `div` 1000
  let totalTime = toNanoSecs (diffTimeSpec startTime endTime) `div` 1000
  Log.logInfo_ $ show [fmt| Processing {length msgs} msgs. saved {length reqDumpsFinal}. totalTime: {totalTime} -> query: {queryTime} -> processing: {processingTime}|]
  case result of
    Left (e :: SomePostgreSqlException) -> do
      Log.logAttention "Postgres Exception" (show e)
      pure []
    Right _ -> pure rmAckIds


projectCacheDefault :: Projects.ProjectCache
projectCacheDefault =
  Projects.ProjectCache
    { hosts = []
    , endpointHashes = []
    , shapeHashes = []
    , redactFieldslist = []
    , weeklyRequestCount = 0
    , paymentPlan = ""
    }


processRequestMessage
  :: (Reader.Reader Config.AuthContext :> es, Time.Time :> es, IOE :> es)
  => RequestMessages.RequestMessage
  -> Eff es (Either Text (Maybe RequestDumps.RequestDump, Maybe Endpoints.Endpoint, Maybe Shapes.Shape, V.Vector Fields.Field, V.Vector Formats.Format, V.Vector RequestDumps.ATError))
processRequestMessage recMsg = do
  appCtx <- ask @Config.AuthContext
  timestamp <- Time.currentTime
  let pid = Projects.ProjectId recMsg.projectId
  -- We retrieve the projectCache object from the inmemory cache and if it doesn't exist,
  -- we set the value in the db into the cache and return that.
  -- This should help with our performance, since this project Cache is the only information we need in order to process
  -- an apitoolkit requestmessage payload. So we're able to process payloads without hitting the database except for the actual db inserts.
  projectCacheVal <- liftIO $ Cache.fetchWithCache appCtx.projectCache pid \pid' -> do
    mpjCache <- withPool appCtx.jobsPool $ Projects.projectCacheById pid'
    pure $ fromMaybe projectCacheDefault mpjCache
  recId <- liftIO nextRandom
  pure
    $ if False
      then Right (Nothing, Nothing, Nothing, V.empty, V.empty, V.empty)
      else RequestMessages.requestMsgToDumpAndEndpoint projectCacheVal recMsg timestamp recId
