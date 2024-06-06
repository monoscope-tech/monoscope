module Models.Tests.TestToDump (testRunToRequestMsg, runTestAndLog) where

import Data.Aeson qualified as AE
import Data.ByteString.Base64 qualified as B64
import Data.Either.Extra (fromRight')
import Data.Time
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful (
  Eff,
  IOE,
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Foreign.C.String (peekCString, withCString)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import ProcessMessage qualified
import Relude
import RequestMessages (RequestMessage (..))
import RustInterop (run_testkit)
import System.Config qualified as Config


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
  let (method, _rawUri) = fromMaybe ("GET", "") $ methodPath sr.request.req
  RequestMessage
    { duration = 1000000 -- Placeholder for duration in nanoseconds
    , host = Just ""
    , method = method
    , pathParams = AE.toJSON (fromMaybe mempty (sr.request.req.params))
    , projectId = pid
    , protoMajor = 1
    , protoMinor = 1
    , queryParams = AE.toJSON (fromMaybe mempty (sr.request.req.params)) -- Assuming all params are query params
    , rawUrl = ""
    , referer = Nothing -- Placeholder for the referer
    , requestBody = B64.encodeBase64 $ encodeUtf8 $ fromMaybe "" (sr.request.req.raw)
    , requestHeaders = AE.toJSON (fromMaybe mempty (sr.request.req.headers))
    , responseBody = B64.encodeBase64 $ encodeUtf8 $ sr.request.resp.raw -- TODO: base64 encode
    , responseHeaders = AE.toJSON (sr.request.resp.headers)
    , sdkType = RequestDumps.TestkitOutgoing
    , statusCode = sr.request.resp.status
    , urlPath = Just ""
    , timestamp = utcToZonedTime utc currentTime
    , msgId = Nothing
    , parentId = Just parent_id -- No parentId provided, assuming None
    , serviceVersion = Nothing -- Placeholder for serviceVersion
    , errors = Nothing -- Placeholder for errors
    , tags = Nothing -- Placeholder for tags
    }


callRunTestkit :: String -> IO String
callRunTestkit hsString = withCString hsString $ \cstr -> do
  resultCString <- run_testkit (cstr)
  peekCString (resultCString)


runCollectionTest :: IOE :> es => V.Vector Testing.CollectionStepData -> Eff es (Either Text (V.Vector Testing.StepResult))
runCollectionTest collectionSteps = do
  tkResp <- liftIO $ callRunTestkit $ decodeUtf8 $ AE.encode $ collectionSteps
  -- FIXME: handle fromLeft correctly.
  let stepResults = fromRight' $ AE.eitherDecodeStrictText (toText tkResp) :: V.Vector Testing.StepResult
  pure $ Right stepResults


runTestAndLog
  :: (IOE :> es, Time.Time :> es, Reader.Reader Config.AuthContext :> es, DB :> es, Log :> es)
  => Projects.ProjectId
  -> V.Vector Testing.CollectionStepData
  -> Eff es (Either Text (V.Vector Testing.StepResult))
runTestAndLog pid collectionSteps = do
  stepResultsE <- runCollectionTest collectionSteps
  let stepResults = fromRight' stepResultsE
  currentTime <- Time.currentTime
  -- Create a parent request for to act as parent for current test run
  msg_id <- liftIO $ UUIDV4.nextRandom
  let parent_msg =
        RequestMessage
          { duration = 1000000 -- Placeholder for duration in nanoseconds
          , host = Just ""
          , method = "GET"
          , pathParams = AE.object []
          , projectId = pid.unProjectId
          , protoMajor = 1
          , protoMinor = 1
          , queryParams = AE.object [] -- Assuming all params are query params
          , rawUrl = ""
          , referer = Nothing -- Placeholder for the referer
          , requestBody = B64.encodeBase64 $ encodeUtf8 $ "{\"MESSAGE\": \"CUSTOM PARENT REQUEST CREATED BY APITOOLIT\"}"
          , requestHeaders = AE.object []
          , responseBody = B64.encodeBase64 $ encodeUtf8 $ "" -- TODO: base64 encode
          , responseHeaders = AE.object []
          , sdkType = RequestDumps.TestkitOutgoing
          , statusCode = 200
          , urlPath = Just ""
          , timestamp = utcToZonedTime utc currentTime
          , msgId = Just msg_id
          , parentId = Nothing -- No parentId provided, assuming None
          , serviceVersion = Nothing -- Placeholder for serviceVersion
          , errors = Nothing -- Placeholder for errors
          , tags = Nothing -- Placeholder for tags
          }
  let requestMessages = V.toList (stepResults <&> \sR -> ("", testRunToRequestMsg pid currentTime msg_id sR))
  _ <- ProcessMessage.processRequestMessages $ requestMessages <> [("", parent_msg)]
  pure $ Right stepResults
