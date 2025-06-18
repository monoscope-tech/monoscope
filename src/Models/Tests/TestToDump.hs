module Models.Tests.TestToDump (testRunToRequestMsg, logTest, runCollectionTest) where

import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.Effectful.UUID (UUIDEff)
import Data.Either.Extra (mapLeft)
import Data.Time
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful
import Effectful.Ki qualified as Ki
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


callRunTestkit :: String -> String -> String -> IO String
callRunTestkit hsString hsVars hsColid = withCString hsString $ \cstr -> do
  withCString hsVars $ \cstr1 -> do
    withCString hsColid $ \cstr2 -> do
      resultCString <- run_testkit cstr cstr2 cstr1
      peekCString resultCString


runCollectionTest :: IOE :> es => V.Vector Testing.CollectionStepData -> V.Vector Testing.CollectionVariablesItem -> Testing.CollectionId -> Eff es (Either Text (V.Vector Testing.StepResult))
runCollectionTest collection_steps col_vars cold_id = do
  tkResp <- liftIO $ callRunTestkit (decodeUtf8 $ AE.encode collection_steps) (decodeUtf8 $ AE.encode col_vars) (toString cold_id.toText)
  pure $ mapLeft (\e -> fromString e <> toText tkResp) $ AE.eitherDecodeStrictText (toText tkResp)


logTest
  :: (DB :> es, IOE :> es, Ki.StructuredConcurrency :> es, Log :> es, Reader.Reader Config.AuthContext :> es, Time.Time :> es, UUIDEff :> es)
  => Projects.ProjectId
  -> Testing.CollectionId
  -> V.Vector Testing.CollectionStepData
  -> Either Text (V.Vector Testing.StepResult)
  -> Eff es (Either Text (V.Vector Testing.StepResult))
logTest pid colId collectionSteps stepRes = do
  case stepRes of
    Left e -> do
      Log.logAttention "unable to run test collection" (AE.object ["error" AE..= e, "steps" AE..= collectionSteps])
      pure $ Left e
    Right stepResults -> do
      currentTime <- Time.currentTime
      let (passed, failed) = Testing.getCollectionRunStatus stepResults
      msg_id <- liftIO UUIDV4.nextRandom
      let response = AE.toJSON stepResults
      _ <- dbtToEff $ Testing.updateCollectionLastRun colId (Just response) passed failed
      -- let parent_msg =
      --       RequestMessage
      --         { duration = 1000000 -- Placeholder for duration in nanoseconds
      --         , host = Just "app.apitoolkit.io"
      --         , method = "GET"
      --         , pathParams = AE.object []
      --         , projectId = pid.unProjectId
      --         , protoMajor = 1
      --         , protoMinor = 1
      --         , queryParams = AE.object [] -- Assuming all params are query params
      --         , rawUrl = "/TEST_RUN"
      --         , referer = Nothing -- Placeholder for the referer
      --         , requestBody = B64.extractBase64 $ B64.encodeBase64 $ encodeUtf8 "{\"MESSAGE\": \"CUSTOM PARENT REQUEST CREATED BY APITOOLIT\"}"
      --         , requestHeaders = AE.object []
      --         , responseBody = B64.extractBase64 $ B64.encodeBase64 $ encodeUtf8 "" -- TODO: base64 encode
      --         , responseHeaders = AE.object []
      --         , sdkType = RequestDumps.TestkitOutgoing
      --         , statusCode = 200
      --         , urlPath = Just "/TEST_RUN"
      --         , timestamp = utcToZonedTime utc currentTime
      --         , msgId = Just msg_id
      --         , parentId = Nothing -- No parentId provided, assuming None
      --         , serviceVersion = Nothing -- Placeholder for serviceVersion
      --         , errors = Nothing -- Placeholder for errors
      --         , tags = Nothing -- Placeholder for tags
      --         }
      let requestMessages = V.toList (stepResults <&> \sR -> ("", testRunToRequestMsg pid currentTime msg_id sR))
      _ <- ProcessMessage.processRequestMessages requestMessages
      pure $ Right stepResults
