module Models.Tests.TestToDump (testRunToRequestMsg, runTestAndLog) where

import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.Either.Extra (mapLeft)
import Data.Time
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Foreign.C.String (peekCString, withCString)
import Log qualified
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import ProcessMessage qualified
import Relude
import RequestMessages (RequestMessage (..))
import RustInterop (run_testkit)
import System.Config qualified as Config
import Models.Telemetry.Telemetry
  

methodPath :: Testing.CollectionStepData -> Maybe (Text, Text)
methodPath stepData =
  listToMaybe
    $ catMaybes
      [ ("POST",) <$> stepData.post
      , ("GET",) <$> stepData.get
      , ("PUT",) <$> stepData.put
      , ("DELETE",) <$> stepData.delete
      , ("PATCH",) <$> stepData.patch
      , ("UPDATE",) <$> stepData.update
      ]


-- Conversion function
testRunToRequestMsg :: Projects.ProjectId -> UTCTime -> UUID.UUID -> Testing.StepResult -> RequestMessage
testRunToRequestMsg (Projects.ProjectId pid) currentTime parent_id sr = do
  let (method, rawUri) = fromMaybe ("GET", "") $ methodPath sr.request.req
  RequestMessage
    { duration = 1000000
    , host = Just ""
    , method = method
    , pathParams = AE.toJSON (maybeToMonoid sr.request.req.params)
    , projectId = pid
    , protoMajor = 1
    , protoMinor = 1
    , queryParams = AE.toJSON (maybeToMonoid sr.request.req.params) -- Assuming all params are query params
    , rawUrl = rawUri
    , referer = Nothing
    , requestBody = B64.extractBase64 $ B64.encodeBase64 $ encodeUtf8 $ fromMaybe "" sr.request.req.raw
    , requestHeaders = AE.toJSON (maybeToMonoid sr.request.req.headers)
    , responseBody = B64.extractBase64 $ B64.encodeBase64 $ encodeUtf8 sr.request.resp.raw -- TODO: base64 encode
    , responseHeaders = AE.toJSON sr.request.resp.headers
    , sdkType = RequestDumps.TestkitOutgoing
    , statusCode = sr.request.resp.status
    , urlPath = Just rawUri
    , timestamp = utcToZonedTime utc currentTime
    , msgId = Nothing
    , parentId = Just parent_id
    , serviceVersion = Nothing
    , errors = Nothing
    , tags = Nothing
    }


callRunTestkit :: String -> String -> IO String
callRunTestkit hsString hsColid = withCString hsString $ \cstr -> do
  withCString hsColid $ \cstr2 -> do
    resultCString <- run_testkit cstr cstr2
    peekCString resultCString


runCollectionTest :: IOE :> es => V.Vector Testing.CollectionStepData -> Testing.CollectionId -> Eff es (Either Text (V.Vector Testing.StepResult))
runCollectionTest collectionSteps cold_id = do
  tkResp <- liftIO $ callRunTestkit (decodeUtf8 $ AE.encode collectionSteps) (toString cold_id.toText)
  pure $ mapLeft (\e -> fromString e <> toText tkResp) $ AE.eitherDecodeStrictText (toText tkResp)


runTestAndLog
  :: (IOE :> es, Time.Time :> es, Reader.Reader Config.AuthContext :> es, DB :> es, Log :> es)
  => Projects.ProjectId
  -> Testing.CollectionId
  -> V.Vector Testing.CollectionStepData
  -> Eff es (Either Text (V.Vector Testing.StepResult))
runTestAndLog pid colId collectionSteps = do
  stepResultsE <- runCollectionTest collectionSteps colId
  case stepResultsE of
    Left e -> do
      Log.logAttention "unable to run test collection" (AE.object ["error" AE..= e, "steps" AE..= collectionSteps])
      pure $ Left e
    Right stepResults -> do
      currentTime <- Time.currentTime
      let (passed, failed) = Testing.getCollectionRunStatus stepResults

      -- Create a parent request for to act as parent for current test run
      msg_id <- liftIO UUIDV4.nextRandom
      let response = AE.toJSON stepResults
      _ <- dbtToEff $ Testing.updateCollectionLastRun colId (Just response) passed failed
      let isPassed = failed == 0
      let logMsg = if isPassed then "PASSED:  multistep API test succeeded" else "FAILED:  multistep API test failed"
      let log =
            LogRecord
              {  projectId = pid.unProjectId
                , id = Nothing 
                , timestamp = currentTime
                , observedTimestamp = currentTime
                , traceId = ""
                , spanId = Nothing
                , severityText = if isPassed then Just SLInfo else  Just  SLError 
                , severityNumber = if isPassed then 1 else 9
                , body = logMsg
                , attributes = AE.object []
                , resource = AE.object
                     [ 
                       "service.name" AE..= ("system.monitors" :: Text)
                      , "service.namespace" AE..= ("apitoolkit" :: Text)
                        ]
                , instrumentationScope = AE.object ["name" AE..= ("system.monitors" :: Text)]
              }
      let requestMessages = V.toList (stepResults <&> \sR -> ("", testRunToRequestMsg pid currentTime msg_id sR))
      _ <- ProcessMessage.processRequestMessages $ requestMessages
      _ <- bulkInsertLogs [log]
      pure $ Right stepResults

