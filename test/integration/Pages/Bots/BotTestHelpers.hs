module Pages.Bots.BotTestHelpers (
  assertDeferredJobRunnable,
  completeTestRun,
  extractResponseText,
  extractResponseType,
  interactiveInvocation,
  resetLegacyEvidenceScope,
  scheduledInvocation,
  setupLinkedSlackData,
  setupSlackAgent,
  slackEventCallback,
  slackInteraction,
  slackThreadedEvent,
  withSlackMCPResponses,
  withSlackReplyResponses,
) where

import Control.Lens ((.~), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _String)
import Data.Aeson.QQ (aesonQQ)
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as HTTP
import Data.Pool (withResource)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpose, send)
import Effectful.Time (Time)
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client (RequestBody (..), defaultRequest, requestBody)
import Network.HTTP.Types (status202, status204)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types qualified as WreqTypes
import OddJobs.Job qualified as OddJobs
import Pages.AIThreads qualified as AIThreads
import Pages.Bots.Slack qualified as SlackPage
import Pkg.AI qualified as AI
import Pkg.TestUtils
import Servant.API.ResponseHeaders (getResponse)
import System.Types (DB)


-- | Reproduce pre-migration evidence atomically, restoring trigger state on failure.
resetLegacyEvidenceScope :: TestResources -> Projects.ProjectId -> UUIDId "conversation" -> IO ()
resetLegacyEvidenceScope tr project conversation = withResource tr.trPool $ \conn -> PGS.withTransaction conn do
  void $ PGS.execute_ conn "ALTER TABLE apis.ai_conversations DISABLE TRIGGER conversation_evidence_permission"
  void $ PGS.execute_ conn "ALTER TABLE apis.ai_routine_runs DISABLE TRIGGER run_evidence_permission"
  void $ PGS.execute conn [sql|UPDATE apis.ai_conversations SET evidence_permission = NULL WHERE project_id = ? AND conversation_id = ?|] (project, conversation)
  void $ PGS.execute conn [sql|UPDATE apis.ai_routine_runs SET evidence_permission = NULL WHERE project_id = ? AND conversation_id = ?|] (project, conversation)
  void $ PGS.execute_ conn "ALTER TABLE apis.ai_conversations ENABLE TRIGGER conversation_evidence_permission"
  void $ PGS.execute_ conn "ALTER TABLE apis.ai_routine_runs ENABLE TRIGGER run_evidence_permission"


completeTestRun :: (DB es, IOE :> es, Time :> es) => Projects.ProjectId -> Issues.RunClaim -> Issues.RunCompletion -> Eff es ()
completeTestRun project claim completion = AI.requireRunContext project claim >>= \context -> void $ Issues.completeRun context completion


interactiveInvocation :: TestResources -> Projects.ProjectId -> UUIDId "conversation" -> Text -> Issues.ExecutionPolicy -> IO AI.InvocationMode
interactiveInvocation tr project conversation input policy = runTestBg frozenTime tr do
  request <- UUIDId <$> UUID.genUUID
  let user = (getResponse tr.trSessAndHeader).user.id
  Issues.startInteractiveRun project conversation user request input policy Issues.WebConversation >>= \case
    Issues.ConflictingRequest -> liftIO $ fail "Expected a fresh interactive request"
    Issues.AcceptedRequest{} -> fmap AI.DurableRun $ AI.requireRunContext project $ Issues.InteractiveClaim user conversation request


scheduledInvocation :: TestResources -> UUIDId "conversation" -> Issues.ExecutionPolicy -> IO AI.InvocationMode
scheduledInvocation tr conversation policy = do
  let user = (getResponse tr.trSessAndHeader).user.id
  slot <- runTestBg frozenTime tr do
    void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
    scheduled <- Issues.upsertRoutine user testPid conversation $ fromRightShow $ Issues.mkRoutineInterval 15
    Issues.setRoutinePolicy user testPid conversation policy
    pure scheduled
  schedule <- maybe (fail "Expected a scheduled instant") pure slot
  claimed <- runQueryEffect tr (Issues.claimRoutine schedule.id schedule.scheduledAt) >>= maybe (fail "Expected a claimed run") pure
  runTestBg frozenTime tr $ fmap AI.DurableRun $ AI.requireRunContext testPid $ Issues.BackgroundClaim user conversation claimed.runId claimed.claimedAt


setupLinkedSlackData :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupLinkedSlackData tr pid teamId = do
  setupSlackData tr pid teamId
  dbExec tr [sql|UPDATE apis.slack SET scopes = ARRAY['assistant:write', 'chat:write'] WHERE project_id = ?|] (PGS.Only pid)
  dbExec
    tr
    [sql|INSERT INTO apis.slack_identities (team_id, slack_user_id, user_id, project_id)
      VALUES (?, 'U0123ABCDEF', ?, ?)|]
    (teamId, (getResponse tr.trSessAndHeader).user.id, pid)


-- | A linked requester with the Slack investigation template explicitly activated.
setupSlackAgent :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupSlackAgent tr pid teamId = do
  setupLinkedSlackData tr pid teamId
  void $ testServant tr $ AIThreads.routineInstallPostH pid (AIThreads.RoutineTemplateForm "slack-investigation")


-- | Override selected HTTP responses while retaining request recording.
withSlackMCPResponses :: (HTTP.HTTP :> es, IOE :> es) => (HTTP.Options -> AE.Value -> IO AE.Value) -> Eff es a -> Eff es a
withSlackMCPResponses fixture = interpose @HTTP.HTTP \_ -> \case
  HTTP.PostWith options url@"https://mcp.slack.com/mcp" payload -> do
    request <- liftIO $ WreqTypes.postPayload payload defaultRequest
    bytes <- case requestBody request of
      RequestBodyLBS body -> pure body
      RequestBodyBS body -> pure $ fromStrict body
      _ -> liftIO $ fail "Expected a JSON MCP request"
    value <- either (liftIO . fail) pure $ AE.eitherDecode bytes
    response <- send $ HTTP.PostWith options url payload
    if (value ^? key "method" . _String) == Just "notifications/initialized"
      then pure $ response & Wreq.responseStatus .~ status202 & Wreq.responseBody .~ ""
      else do
        reply <- liftIO $ fixture options value
        pure $ response & Wreq.responseBody .~ AE.encode reply & Wreq.responseHeaders .~ [("Content-Type", "application/json"), ("Mcp-Session-Id", "mcp-session")]
  HTTP.DeleteWith options url@"https://mcp.slack.com/mcp" -> do
    response <- send $ HTTP.DeleteWith options url
    pure $ response & Wreq.responseStatus .~ status204 & Wreq.responseBody .~ ""
  request -> send @HTTP.HTTP $ coerce request


-- | Fault only answer posts, allowing asynchronous progress to publish normally.
-- Retain the underlying recorder so assertions inspect actual request payloads.
withSlackReplyResponses :: (HTTP.HTTP :> es, IOE :> es) => IO LByteString -> Eff es a -> Eff es a
withSlackReplyResponses fixture =
  withHTTPResponses (\_ _ -> pure $ Just "{\"ok\":true,\"messages\":[],\"ts\":\"1735689609.000001\",\"members\":[\"U0123ABCDEF\"]}")
    . interpose @HTTP.HTTP
      ( \_ -> \case
          HTTP.PostWith opts "https://slack.com/api/chat.postMessage" value -> do
            request <- liftIO $ WreqTypes.postPayload value defaultRequest
            body <- case requestBody request of
              RequestBodyLBS bytes -> pure bytes
              RequestBodyBS bytes -> pure $ fromStrict bytes
              _ -> liftIO $ fail "Expected a JSON Slack message body"
            response <- send $ HTTP.PostWith opts "https://slack.com/api/chat.postMessage" value
            if (AE.decode @AE.Value body >>= (^? key "metadata" . key "event_type" . _String)) == Just "monoscope_investigation_reply"
              then liftIO fixture <&> \reply -> response & Wreq.responseBody .~ reply
              else pure response
          operation -> send @HTTP.HTTP $ coerce operation
      )


extractResponseText :: AE.Value -> Maybe Text
extractResponseText val = (val ^? key "text" . _String) <|> (val ^? key "content" . _String)


extractResponseType :: AE.Value -> Maybe Text
extractResponseType val = val ^? key "response_type" . _String


slackInteraction :: Text -> Text -> Text -> SlackPage.SlackInteraction
slackInteraction cmd query teamId =
  SlackPage.SlackInteraction
    teamId -- team_id
    cmd -- command
    query -- text
    ("https://hooks.slack.com/commands/" <> teamId <> "/response") -- response_url
    ("T.13579.1234567890.abcdef1234567890abcdef1234567890") -- trigger_id (realistic format)
    "C0123ABCDEF" -- channel_id (realistic Slack channel ID)
    "general" -- channel_name
    "U0123ABCDEF" -- user_id (realistic Slack user ID)


-- | The signed-callback envelope every Slack event arrives in. Only the inner
-- event varies between tests; the wrapper never does.
slackEventCallback :: Text -> Text -> Text -> Text -> Maybe Text -> AE.Value
slackEventCallback teamId channelId userText ts threadTsM =
  [aesonQQ|{
    "type": "event_callback",
    "token": "xoxb-test-fake-token-not-real",
    "team_id": #{teamId},
    "api_app_id": "A0123ABCDEF",
    "event": {
      "type": "message",
      "user": "U0123ABCDEF",
      "text": #{userText},
      "ts": #{ts},
      "channel": #{channelId},
      "event_ts": #{ts},
      "thread_ts": #{maybeToJSON threadTsM}
    },
    "event_id": "Ev0123ABCDEF",
    "event_time": 1700000000
  }|]
  where
    maybeToJSON :: Maybe Text -> AE.Value
    maybeToJSON = maybe AE.Null AE.String


slackThreadedEvent :: Text -> Text -> Text -> Text -> Text -> AE.Value
slackThreadedEvent teamId channelId userText ts threadTs =
  slackEventCallback teamId channelId userText ts (Just threadTs)


assertDeferredJobRunnable :: PGS.Connection -> AE.Value -> IO ()
assertDeferredJobRunnable conn payload = PGS.withTransaction conn do
  void $ PGS.execute_ conn [sql|CREATE TEMP TABLE slack_deferred_poll (LIKE background_jobs INCLUDING ALL) ON COMMIT DROP|]
  copied <- PGS.execute conn [sql|INSERT INTO slack_deferred_poll SELECT * FROM background_jobs WHERE payload = ? ORDER BY run_at DESC LIMIT 1|] (PGS.Only $ Aeson payload)
  copied `shouldBe` 1
  void $ PGS.execute_ conn [sql|UPDATE slack_deferred_poll SET run_at = clock_timestamp() + interval '1 hour'|]
  OddJobs.jobPollingIO conn "slack-regression" "slack_deferred_poll" 60 >>= (`shouldBe` [])
  void $ PGS.execute_ conn [sql|UPDATE slack_deferred_poll SET run_at = clock_timestamp() - interval '1 second'|]
  selected <- OddJobs.jobPollingIO conn "slack-regression" "slack_deferred_poll" 60
  length selected `shouldBe` 1
  OddJobs.jobPollingIO conn "slack-regression" "slack_deferred_poll" 60 >>= (`shouldBe` [])
