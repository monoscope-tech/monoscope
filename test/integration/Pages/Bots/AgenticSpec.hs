{-# LANGUAGE OrPatterns #-}

module Pages.Bots.AgenticSpec (spec) where

import BackgroundJobs qualified
import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), bracket_)
import Control.Lens ((^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, values, _Array, _String)
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM qualified as ELLM
import Data.Effectful.UUID qualified as UUIDEff
import Data.IORef qualified as IORef
import Data.Map qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful qualified as Eff
import Effectful.Concurrent.Async (concurrently, concurrently_)
import Effectful.Dispatch.Dynamic (interpose)
import Effectful.Error.Static (catchError)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Langchain.LLM.Core qualified as LLM
import Models.Apis.Integrations qualified as Integrations
import Models.Apis.Investigations (ToolCallInfo (..), ToolResult (..))
import Models.Apis.Investigations qualified as Investigations
import Models.Apis.IssueEnhancement.Types qualified as Enhancement
import Models.Apis.Issues qualified as Issues
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMemory qualified as Memory
import Models.Projects.Projects qualified as Projects
import OpenAI.V1.Chat.Completions qualified as OpenAI
import Pages.AIThreads qualified as AIThreads
import Pages.Bots.BotFixtures (slackThreadedEvent)
import Pages.Bots.BotTestHelpers (assertJsonGolden, completeTestRun, getOpenAIKey, getOpenAIModel, interactiveInvocation, receiveSlackEvent, resetLegacyEvidenceScope, scheduledInvocation, setupSlackAgent, setupSlackData, withHTTPResponses)
import Pages.Bots.SeedTestData (cleanupTelemetryData, seedTelemetryData)
import Pages.Bots.Utils (processAIQuery)
import Pages.Issues qualified as IssuePage
import Pkg.AI qualified as AI
import Pkg.AgentCatalog qualified as Catalog
import Pkg.AgentOperation qualified as Operation
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant (ServerError (..))
import Servant.API.ResponseHeaders (getResponse)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Logging
import System.Timeout qualified as Timeout
import System.Types (ATAuthCtx, ATBackgroundCtx)
import Test.Hspec (Spec, around, describe, expectationFailure, it, shouldBe, shouldContain, shouldNotContain, shouldSatisfy, shouldThrow)
import UnliftIO.Async qualified as Async
import UnliftIO.Exception (throwIO, tryAny)
import Web.FormUrlEncoded (urlDecodeAsForm)
import Web.Routes qualified as Routes


saveConversationArtifact :: TestResources -> UUIDId "conversation" -> [(Text, AE.Value)] -> IO ToolResult
saveConversationArtifact tr conversation args = do
  mode <- interactiveInvocation tr testPid conversation "Retain this finding" Issues.Observe
  let config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember (getResponse tr.trSessAndHeader).user.id, AI.conversationId = Just conversation, AI.invocationMode = mode}
      provider = interpose @ELLM.LLM \_ -> \case
        ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
          [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "save" "function" $ LLM.ToolFunction "save_artifact" $ Map.fromList args]}
          _ -> LLM.Message LLM.Assistant "Saved." LLM.defaultMessageData
        ELLM.CallLLM{} -> pure $ Left "Unexpected title request"
        ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
  result <- runTestBg frozenTime tr $ provider $ AI.runAgenticChatWithHistory config "Retain this finding" "model" "key"
  case (mode, result) of
    (AI.DurableRun context, Right _) ->
      runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid context.id) >>= \case
        [entry] | Investigations.OperationReturned response <- entry.outcome -> pure response
        _ -> fail "Expected one retained artifact response"
    _ -> fail "Expected a successful durable invocation"


data AbandonedRun = DeletedRequester | DetachedSchedule | DetachedEvent | HistoricalRequester | ReauthorizedRequester
  deriving stock (Eq, Show)


data ReviewScenario = ApproveReview | DeclineReview | ExpireBeforeReview | ExpireAfterReview | RevokeReviewer | AdministratorRequired | PrivateReview | ExpiredProposal | ConcurrentDecisions
  deriving stock (Bounded, Enum, Eq, Show)


spec :: Spec
spec = around withTestResources do
  describe "Agentic Query Processing" do
    describe "Response parsing" do
      it "parses widget response" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = """{"explanation": "Here are the errors", "widgets": [{"type": "timeseries", "query": "level == ERROR", "title": "Error Trend"}], "suggested_query": "level == ERROR"}"""
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            length parsed.widgets `shouldBe` 1
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "handles text-only response" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = """{"explanation": "Your error rate is 5%", "widgets": []}"""
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            null parsed.widgets `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "preserves tool calls from result" \_ -> do
        let toolCall =
              AI.ToolCallInfo
                { AI.name = "run_query"
                , AI.args = mempty
                , AI.resultPreview = "10 rows"
                , AI.rawData = Nothing
                }
            rawResult =
              AI.AgenticChatResult
                { AI.response = "{\"explanation\": \"done\"}"
                , AI.toolCalls = [toolCall]
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> length (fromMaybe [] parsed.toolCalls) `shouldBe` 1
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "handles malformed JSON gracefully" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = "This is not JSON, just a text response"
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            null parsed.widgets `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      -- gpt-5.6-luna rejects function tools + reasoning_effort on /v1/chat/completions (400);
      -- tool-bearing params must downgrade a configured effort to "none"
      it "agenticSetup_toolsWithEffort_sendsEffortNone" \_ -> do
        (_, _, params) <- Eff.runEff $ Time.runTime $ AI.agenticSetup (AI.defaultAgenticConfig testPid) "any query" "gpt-5.6-luna#high"
        let body = AE.toJSON params
        isJust (body ^? key "tools" . _Array) `shouldBe` True
        (body ^? key "reasoning_effort") `shouldBe` Just (AE.String "none")

      it "starts with project tool discovery while keeping actions permission-gated" \tr -> do
        let toolNames config = do
              (_, _, params) <- Eff.runEff $ Time.runTime $ AI.agenticSetup config{AI.projectTools = Just Routes.mcpProjectTools} "any query" "model"
              pure $ AE.toJSON params ^.. key "tools" . values . key "function" . key "name" . _String
        readonly <- toolNames $ AI.defaultAgenticConfig testPid
        mode <- interactiveInvocation tr testPid (UUIDId UUID.nil) "any query" Issues.Autonomous
        actionable <- toolNames (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember (getResponse tr.trSessAndHeader).user.id, AI.conversationId = Just $ UUIDId UUID.nil, AI.invocationMode = mode}
        serviceActions <- toolNames (AI.defaultAgenticConfig testPid){AI.invocationMode = mode}
        observed <- scheduledInvocation tr (UUIDId $ UUID.fromWords 0 0 99 1) Issues.Observe
        autonomous <- scheduledInvocation tr (UUIDId $ UUID.fromWords 0 0 99 2) Issues.Autonomous
        scheduledReadOnly <- toolNames (AI.defaultAgenticConfig testPid){AI.invocationMode = observed}
        scheduledActionable <- toolNames (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember (getResponse tr.trSessAndHeader).user.id, AI.invocationMode = autonomous}
        repositoryAware <- toolNames (AI.defaultAgenticConfig testPid){AI.sourceConfig = Just tr.trATCtx.config}
        readonly `shouldContain` ["list_project_memory", "read_project_memory"]
        readonly `shouldNotContain` ["write_project_memory"]
        actionable `shouldContain` ["write_project_memory"]
        serviceActions `shouldNotContain` ["write_project_memory", "send_to_slack"]
        readonly `shouldNotContain` ["send_to_slack"]
        scheduledReadOnly `shouldNotContain` ["send_to_slack"]
        readonly `shouldSatisfy` (\names -> all (`elem` names) ["search_project_tools", "get_project_tool_schema", "list_metrics"])
        readonly `shouldSatisfy` (\names -> all (`notElem` names) ["list_issues", "get_issue", "list_incidents", "get_incident", "list_monitors", "get_monitor", "list_endpoints", "get_endpoint", "list_log_patterns", "get_log_pattern", "list_dashboards", "get_dashboard", "get_project"])
        repositoryAware `shouldContain` ["get_code_context", "list_runbooks", "read_runbook", "get_linked_repositories", "get_deployments"]
        actionable `shouldContain` ["send_to_slack"]
        scheduledActionable `shouldContain` ["send_to_slack"]

      it "routineAuthority_destinationIsIndependentAndRevokedEditorsCannotChangeSettings" \tr -> do
        let conversation = UUIDId UUID.nil
            user = (getResponse tr.trSessAndHeader).user.id
            destination = AIThreads.routineDestinationPostH testPid conversation . AIThreads.RoutineDestinationForm
        void $ toBaseServantResponse tr $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
        Just routine <- runTestBg frozenTime tr $ Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid conversation $ fromRightShow $ Issues.mkRoutineInterval 15
        execution <- runQueryEffect tr (Issues.claimRoutine routine.id routine.scheduledAt) >>= maybe (fail "Expected a claimed run") pure
        runQueryEffect tr (Issues.routineRunPolicyFor testPid execution.runId user conversation) >>= (`shouldBe` Just Issues.Autonomous)
        void $ testServant tr $ destination Issues.DestinationSlack
        runQueryEffect tr (Issues.routineRunPolicyFor testPid execution.runId user conversation) >>= (`shouldBe` Just Issues.Autonomous)
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
        for_ [Issues.DestinationConversation, Issues.DestinationSlack] \target -> do
          status <- runAuthHandler tr $ (destination target $> 200) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
          status `shouldBe` 403
        runQueryEffect tr (Issues.routineRunPolicyFor testPid execution.runId user conversation) >>= (`shouldBe` Nothing)

    describe "Web conversation persistence" do
      for_ (["create_dashboard", "delete_dashboard"] :: [Text]) \operation ->
        for_ (["checkpoint", "uncertain", "repeat"] :: [Text]) \boundary ->
          it ("agentOperation_dashboardMutationIsNotRepeatedFor" <> toString operation <> toString boundary) \tr -> do
            calls <- IORef.newIORef (0 :: Int)
            interrupt <- IORef.newIORef True
            let request = UUIDId $ UUID.fromWords 0 0 0 198
                user = (getResponse tr.trSessAndHeader).user.id
                dashboard = UUIDId (UUID.fromWords 0 0 47 1) :: UUIDId "dashboard"
                administrative = operation == "delete_dashboard"
                expectedCount = if administrative then 0 else 1
                arguments = if administrative then one ("dashboard_id", AE.toJSON dashboard) else one ("body", AE.object ["title" AE..= ("Durable action regression" :: Text)])
                form = fromRightShow $ urlDecodeAsForm @AIThreads.AIChatForm "query=Apply+a+durable+dashboard+operation&mode=chat&allowActions=true&requestId=00000000-0000-0000-0000-0000000000c6"
                original = Routes.mcpProjectTools
                backend =
                  original
                    { AI.invoke = \principal pid name args -> do
                        liftIO $ IORef.modifyIORef' calls (+ 1)
                        result <- AI.invoke original principal pid name args
                        when (boundary == "uncertain") $ liftIO do
                          wasFirst <- IORef.atomicModifyIORef' interrupt (\old -> (False, old))
                          when wasFirst $ throwIO $ ErrorCall "Worker interrupted after dashboard mutation"
                        pure result
                    }
                provider :: ATAuthCtx a -> ATAuthCtx a
                provider = interpose @ELLM.LLM \_ -> \case
                  ELLM.CallAgenticChat history _ _ -> do
                    let evidence = [message.content | message <- toList history, message.role == LLM.Tool]
                    pure $ Right $ case evidence of
                      [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall identifier "function" (LLM.ToolFunction operation arguments) | identifier <- if boundary == "repeat" then ["create", "duplicate"] else ["create"]]}
                      [_] | boundary == "uncertain" -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "try-again" "function" $ LLM.ToolFunction operation arguments]}
                      _ -> LLM.Message LLM.Assistant "The dashboard operation completed." LLM.defaultMessageData
                  ELLM.CallLLM{} -> pure $ Right "Durable dashboard request"
                  ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding"
                execute = tryAny $ void $ runAuthHandler tr $ provider $ AIThreads.startThreadPostHWithTools (Just backend) testPid form
                ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
                count = runQueryEffect tr $ Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM projects.dashboards WHERE project_id = #{testPid} AND title = 'Durable action regression'|]
            when administrative
              $ runQueryEffect tr
              $ Hasql.interpExecute_
                [HI.sql|INSERT INTO projects.dashboards (id, project_id, created_by, title)
              VALUES (#{dashboard}, #{testPid}, #{user}, 'Durable action regression')|]
            if boundary == "checkpoint"
              then
                bracket_
                  (ddl "ALTER TABLE apis.slack_turn_checkpoints ADD CONSTRAINT reject_tool_completion CHECK (NOT (checkpoint->>'tag' = 'ToolsPending' AND jsonb_array_length(checkpoint->'contents'->2) > 0)) NOT VALID")
                  (ddl "ALTER TABLE apis.slack_turn_checkpoints DROP CONSTRAINT reject_tool_completion")
                  (execute >>= (`shouldSatisfy` isLeft))
              else execute >>= (`shouldSatisfy` if boundary == "repeat" then isRight else isLeft)
            count >>= (`shouldBe` expectedCount)
            result <- execute
            result `shouldSatisfy` isRight
            count >>= (`shouldBe` expectedCount)
            IORef.readIORef calls >>= (`shouldBe` 1)
            saved <- runQueryEffect tr $ Issues.getRoutineRun testPid request
            fmap (.status) saved `shouldBe` Just Issues.RunSucceeded
            (_, page) <- testServant tr $ AIThreads.routineRunGetH testPid request Nothing
            let html = renderT page
            html `shouldSatisfy` T.isInfixOf "Action history"
            html `shouldSatisfy` T.isInfixOf "Role limit: admin"
            html `shouldSatisfy` T.isInfixOf (if boundary == "uncertain" then "Outcome uncertain" else "Response saved")
            html `shouldSatisfy` T.isInfixOf "hx-target=\"#routine-run\""
            html `shouldSatisfy` T.isInfixOf "Proposal and outcome"
            runQueryEffect tr do
              Hasql.interpExecute_ [HI.sql|UPDATE users.users SET is_sudo = FALSE WHERE id = #{user}|]
              Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
            (_, viewerPage) <- testServant tr $ AIThreads.routineRunGetH testPid request Nothing
            renderT viewerPage `shouldSatisfy` T.isInfixOf (if administrative then "Action details require admin permission." else "Action details require edit permission.")
            renderT viewerPage `shouldSatisfy` (not . T.isInfixOf "Proposal and outcome")
            when administrative $ for_ saved \run -> do
              runQueryEffect tr
                $ Hasql.interpExecute_
                  [HI.sql|UPDATE apis.ai_agent_operations
                SET proposal = jsonb_set(proposal, '{policy,capability}', '{"tag":"Administration"}'::jsonb)
                WHERE run_id = #{request}|]
              resetLegacyEvidenceScope tr testPid run.conversationId
              runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid request) >>= (\operations -> length operations `shouldBe` 1)
              runQueryEffect tr (Investigations.conversationEvidencePermission testPid run.conversationId) >>= (`shouldBe` ProjectMembers.PAdmin)

      it "agentOperation_privateAdministratorCreationRetainsItsCredentialWithoutRepeatingTheWrite" \tr -> do
        let conversation = UUIDId $ UUID.fromWords 0 0 47 2
            user = (getResponse tr.trSessAndHeader).user.id
        mode <- interactiveInvocation tr testPid conversation "Create a private API key" Issues.Autonomous
        calls <- IORef.newIORef (0 :: Int)
        let original = Routes.mcpProjectTools
            backend = original{AI.invoke = \actor project name args -> liftIO (IORef.modifyIORef' calls (+ 1)) >> AI.invoke original actor project name args}
            config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember user, AI.conversationId = Just conversation, AI.invocationMode = mode, AI.projectTools = Just backend}
            provider :: ATBackgroundCtx a -> ATBackgroundCtx a
            provider = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
                [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "private-key" "function" $ LLM.ToolFunction "create_api_key" $ one ("body", AE.object ["title" AE..= ("Retained private administrator key" :: Text)])]}
                results -> LLM.Message LLM.Assistant (T.intercalate "\n" results) LLM.defaultMessageData
              ELLM.CallLLM{} -> pure $ Left "Unexpected non-agent call"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding"
            execute = runTestBg frozenTime tr $ provider $ AI.runAgenticChatWithHistory config "Create a private API key" "model" "key"
        answer <- execute >>= either (fail . toString) pure
        case answer.toolCalls of
          [call] -> (call.rawData >>= (^? key "structuredContent" . key "key" . _String)) `shouldSatisfy` maybe False (not . T.null)
          _ -> expectationFailure "Expected one retained private creation result"
        runQueryEffect tr (Investigations.conversationEvidencePermission testPid conversation) >>= (`shouldBe` ProjectMembers.PAdmin)
        replay <- execute >>= either (fail . toString) pure
        replay.response `shouldBe` answer.response
        runQueryEffect tr (Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM projects.project_api_keys WHERE project_id = #{testPid} AND title = 'Retained private administrator key'|]) >>= (`shouldBe` 1)
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
        denied <- tryAny execute
        denied `shouldSatisfy` \case Left err -> isJust (fromException @AI.AgentAccessDenied err); Right _ -> False
        IORef.readIORef calls >>= (`shouldBe` 1)

      for_ [ApproveReview, DeclineReview, ExpireAfterReview] $ \scenario ->
        it ("agentReview_nativeExecutionRetainsItsReviewAndResponseContract: " <> show scenario) \tr -> do
          issueId <- runTestBg frozenTime tr $ Issues.createQueryAlertIssue testPid "review-native" "Original title" "service == checkout" 100 200 Issues.Above >>= Issues.insertIssueReturningId
          void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment")
          Just routine <- runQueryEffect tr $ Issues.getEventRoutine testPid Issues.EventIssueEnrichment
          runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy testPid routine.conversationId Issues.Autonomous
          let decision = if scenario == DeclineReview then Investigations.ReviewRejected else Investigations.ReviewApproved
              conversation = UUIDId issueId.unUUIDId
              native = Operation.EnrichIssue $ Enhancement.IssueEnrichment (Enhancement.IssueEnhancement issueId "Reviewed title" "" "" "") Nothing Nothing frozenTime
              review = Investigations.ReviewRequest (addUTCTime 60 frozenTime) Investigations.ReviewEditors
          void $ runQueryEffect tr $ Issues.getOrCreateConversation testPid conversation Issues.CTAnomaly (AE.object [])
          Just execution <- runTestBg frozenTime tr $ Issues.startEventRun routine (Issues.IssueEnrichmentRequest issueId) Issues.StartRun conversation "Review the issue change"
          context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.EventClaim (Issues.IssueEnrichment issueId) execution.id execution.startedAt
          let turn = Investigations.RunTurn testPid conversation context.requestedBy context.rootId
              lease = Investigations.RunLease turn context.startedAt
              proposal = Investigations.OperationProposal (Operation.NativeCall native) $ AI.OperationPolicy Catalog.ProjectWrite Issues.Autonomous ProjectMembers.PAdmin
          (cursor, _) <- runQueryEffect tr $ Investigations.prepareNativeOperation turn native
          pending <- runQueryEffect tr $ Investigations.beginOperation lease cursor proposal $ Just review
          identifier <- case pending of
            Investigations.AwaitOperationReview identifier _ -> pure identifier
            _ -> fail "Expected the native action to wait"
          runTestBgNoReset tr (Investigations.decideOperationReview testPid context.requestedBy identifier decision) >>= (`shouldSatisfy` \case Investigations.ReviewSaved{} -> True; _ -> False)
          when (scenario == ExpireAfterReview) $ advanceTestTime tr 60
          let reason = if decision == Investigations.ReviewRejected then Investigations.ReviewDeclined else Investigations.ReviewTimedOut
              perform _ = AI.AtomicOperation $ Issues.updateIssueEnhancement frozenTime issueId "Reviewed title" "" "" $> Right AE.Null
          replicateM_ 2 $ runTestBgNoReset tr (AI.executeNativeOperation context native perform) >>= (`shouldBe` if scenario == ApproveReview then Right AE.Null else Left (Investigations.reviewRefusalMessage reason))
          runQueryEffect tr (Issues.selectIssueById testPid issueId) >>= (\issue -> fmap (.title) issue `shouldBe` Just (if scenario == ApproveReview then "Reviewed title" else "Original title"))
          entries <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid context.id
          case entries of
            [entry] -> case entry.outcome of
              Investigations.OperationReturned response -> do
                scenario `shouldBe` ApproveReview
                response.rawData `shouldBe` Just (AE.toJSON (Right AE.Null :: Either Text AE.Value))
              Investigations.OperationRefused refused -> refused `shouldBe` reason
              _ -> expectationFailure "The reviewed native action must retain its result or refusal"
            _ -> expectationFailure "A native retry must reuse its original operation"

      for_ [minBound .. maxBound] $ \scenario ->
        it ("agentReview_retainsTheProposalAndOnlyExecutesAnApprovedUnexpiredAction: " <> show scenario) \tr -> do
          let conversation = UUIDId $ UUID.fromWords 0 0 0 278
              user = (getResponse tr.trSessAndHeader).user.id
              artifactKey = fromRightShow $ Issues.mkArtifactKey "reviewed-finding"
              call = LLM.ToolCall "reviewed-action" "function" $ LLM.ToolFunction "save_artifact" $ fromList [("key", AE.String "reviewed-finding"), ("content", AE.String "Finding"), ("mode", AE.String "append")]
              message = LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [call]}
              parameters = OpenAI._CreateChatCompletion{OpenAI.model = "model", OpenAI.reasoning_effort = Nothing, OpenAI.tools = Nothing, OpenAI.messages = mempty}
              checkpoint = Investigations.ToolsPending (Investigations.Step (one message) parameters 0 (Nothing, Nothing) []) message [] [call]
              proposal = Investigations.OperationProposal (Operation.ModelCall call) $ AI.OperationPolicy Catalog.ConversationWrite Issues.Observe ProjectMembers.PView
              review = Investigations.ReviewRequest (addUTCTime 60 frozenTime) (if scenario == AdministratorRequired then Investigations.ReviewAdministrators else Investigations.ReviewEditors)
          void $ runQueryEffect tr $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
          mode <- interactiveInvocation tr testPid conversation "Retain this finding after review" Issues.Observe
          context <- case mode of
            AI.DurableRun context -> pure context
            AI.InteractiveReadOnly -> fail "Expected a durable invocation"
          let turn = Investigations.RunTurn testPid conversation user context.rootId
              target = Investigations.RunCheckpoint turn
              lease = Investigations.RunLease turn context.startedAt
          saved <- runQueryEffect tr $ Investigations.startCheckpointFor target checkpoint
          let cursor = Investigations.CheckpointCursor target saved.revision
              execute = runQueryEffect tr $ Investigations.atomicOperation lease cursor proposal (Just review) do
                result <- Issues.saveArtifactTx testPid conversation artifactKey Nothing "Finding" Issues.ArtifactAppend
                pure $ Investigations.ToolResult (either show (const "Finding saved.") result) Nothing
              artifacts = runQueryEffect tr $ Issues.listArtifacts testPid conversation $ Just artifactKey
          when (scenario == ExpiredProposal) $ advanceTestTime tr 60
          pending <- execute
          identifier <- case pending of
            Investigations.WaitingResult identifier request -> (request `shouldBe` review) $> identifier
            Investigations.RefusedResult{} | scenario == ExpiredProposal -> do
              entries <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid context.id
              case entries of [entry] -> pure entry.id; _ -> fail "Expected one expired proposal"
            _ -> fail "Expected a retained waiting operation"
          artifacts >>= (`shouldSatisfy` null)
          for_ [Nothing, Just review{Investigations.expiresAt = addUTCTime 3600 frozenTime}, Just review{Investigations.approver = Investigations.ReviewEditors}] $ \requested -> do
            again <- runQueryEffect tr $ Investigations.beginOperation lease cursor proposal requested
            case again of
              Investigations.AwaitOperationReview retainedId request -> (retainedId, request) `shouldBe` (identifier, review)
              Investigations.RefuseOperation{} | scenario == ExpiredProposal -> pass
              _ -> expectationFailure "A retry cannot bypass review or change its original deadline or approver"
          runTestBgNoReset tr (Investigations.decideOperationReview (UUIDId $ UUID.fromWords 0 0 0 99999) user identifier Investigations.ReviewApproved) >>= (`shouldBe` Investigations.ReviewUnavailable)
          let decide = Investigations.decideOperationReview testPid user identifier $ if scenario == DeclineReview then Investigations.ReviewRejected else Investigations.ReviewApproved
          when (scenario == ExpireBeforeReview) $ advanceTestTime tr 60
          when (scenario == RevokeReviewer) $ runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
          when (scenario == AdministratorRequired) $ runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'edit' WHERE project_id = #{testPid} AND user_id = #{user}|]
          when (scenario == PrivateReview) $ runQueryEffect tr (Investigations.retainEvidenceAudience testPid conversation $ Investigations.PersonalAudience $ UUIDId $ UUID.fromWords 0 0 0 99998) >>= (`shouldBe` True)
          decision <-
            if scenario /= ConcurrentDecisions
              then runTestBgNoReset tr decide
              else do
                results <- runTestBgNoReset tr $ concurrently decide decide
                case results of
                  (Investigations.ReviewSaved leftReceipt, Investigations.ReviewAlreadyDecided rightReceipt) -> (leftReceipt `shouldBe` rightReceipt) $> Investigations.ReviewSaved leftReceipt
                  (Investigations.ReviewAlreadyDecided leftReceipt, Investigations.ReviewSaved rightReceipt) -> (leftReceipt `shouldBe` rightReceipt) $> Investigations.ReviewSaved rightReceipt
                  _ -> fail "Concurrent decisions must retain one review receipt"
          case scenario of
            ExpireBeforeReview -> decision `shouldBe` Investigations.ReviewExpired
            (RevokeReviewer; AdministratorRequired; PrivateReview; ExpiredProposal) -> decision `shouldBe` Investigations.ReviewUnavailable
            _ -> case decision of
              Investigations.ReviewSaved receipt -> do
                receipt.decidedBy `shouldBe` user
                runTestBgNoReset tr (Investigations.decideOperationReview testPid user identifier Investigations.ReviewRejected) >>= (`shouldBe` Investigations.ReviewAlreadyDecided receipt)
              _ -> expectationFailure "Expected one immutable review decision"
          when (scenario == ExpireAfterReview) $ advanceTestTime tr 60
          outcomes <- replicateM 2 execute
          for_ outcomes $ \case
            Investigations.RefusedResult retainedId reason -> do
              retainedId `shouldBe` identifier
              reason `shouldBe` (if scenario == DeclineReview then Investigations.ReviewDeclined else Investigations.ReviewTimedOut)
              scenario `shouldSatisfy` (`elem` [DeclineReview, ExpireBeforeReview, ExpireAfterReview, ExpiredProposal])
            Investigations.WaitingResult retainedId request -> (retainedId, request) `shouldBe` (identifier, review)
            Investigations.RecordedResult{} -> scenario `shouldSatisfy` (`elem` [ApproveReview, ConcurrentDecisions])
            Investigations.UncertainResult{} -> expectationFailure "An unstarted action cannot have an uncertain effect"
          retained <- artifacts
          map (.content) retained `shouldBe` [Just "Finding" | scenario `elem` [ApproveReview, ConcurrentDecisions]]
          entries <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid context.id
          length entries `shouldBe` 1

      for_ ["save_artifact", "write_project_memory"] \tool ->
        it ("agentOperation_atomicMutationRollsBackWithItsOutcome: " <> toString tool) \tr -> do
          let wiki = tool == "write_project_memory"
              page = UUIDId UUID.nil :: Memory.PageId
              arguments = if wiki then fromList [("pageId", AE.toJSON page), ("version", AE.toJSON (0 :: Int)), ("document", AE.object ["title" AE..= ("Finding" :: Text), "body" AE..= ("Finding" :: Text)])] else fromList [("key", AE.String "operation-evidence"), ("mode", AE.String "append"), ("content", AE.String "Finding")]
              conversation = UUIDId UUID.nil
              request = UUIDId $ UUID.fromWords 0 0 0 199
              key' = fromRightShow $ Issues.mkArtifactKey "operation-evidence"
              form = fromRightShow $ urlDecodeAsForm @AIThreads.AIChatForm $ "query=Append+this+finding&mode=chat&requestId=00000000-0000-0000-0000-0000000000c7" <> if wiki then "&allowActions=true" else ""
              provider :: ATAuthCtx a -> ATAuthCtx a
              provider = interpose @ELLM.LLM \_ -> \case
                ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
                  [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "append" "function" $ LLM.ToolFunction tool arguments]}
                  _ -> LLM.Message LLM.Assistant "The finding is retained once." LLM.defaultMessageData
                ELLM.CallLLM{} -> pure $ Right "Atomic append"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding"
              execute = tryAny $ void $ runAuthHandler tr $ provider $ AIThreads.threadPostHWithTools (Just Routes.mcpProjectTools) testPid conversation form
              ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
              content = if wiki then map (Memory.memoryText . (.document.body)) <$> runQueryEffect tr (Memory.pageHistory testPid page) else mapMaybe (.content) <$> runQueryEffect tr (Issues.listArtifacts testPid conversation $ Just key')
          runTestBg frozenTime tr do
            void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
            unless wiki $ void $ Issues.saveArtifact testPid conversation key' Nothing "Base" Issues.ArtifactReplace
          bracket_
            (ddl "ALTER TABLE apis.ai_agent_operations ADD CONSTRAINT reject_operation_outcome CHECK (outcome->>'tag' <> 'OperationReturned') NOT VALID")
            (ddl "ALTER TABLE apis.ai_agent_operations DROP CONSTRAINT reject_operation_outcome")
            do
              execute >>= (`shouldSatisfy` isLeft)
              content >>= (`shouldBe` if wiki then [] else ["Base"])
              runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid request) >>= (\operations -> length operations `shouldBe` 0)
          execute >>= (`shouldSatisfy` isRight)
          content >>= (`shouldBe` if wiki then ["Finding"] else ["Base\n\nFinding"])
          operations <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid request
          case operations of
            [entry] -> do
              entry.proposal.policy.execution `shouldBe` if wiki then Issues.Autonomous else Issues.Observe
              entry.outcome `shouldSatisfy` \case Investigations.OperationReturned{} -> True; _ -> False
            _ -> expectationFailure "Expected one committed operation"

      for_ (["checkpoint", "outcome"] :: [Text]) \boundary ->
        it ("agentOperation_slackPublicationIsNotRepeatedAfterLost" <> toString boundary) \tr -> do
          setupSlackData tr testPid "T_OPERATION"
          posts <- IORef.newIORef (0 :: Int)
          let request = UUIDId $ UUID.fromWords 0 0 0 (if boundary == "checkpoint" then 200 else 201)
              form = fromRightShow $ urlDecodeAsForm @AIThreads.AIChatForm $ fromStrict $ encodeUtf8 $ "query=Send+the+result+to+Slack&mode=chat&allowActions=true&requestId=" <> request.toText
              transport = withHTTPResponses \_ url -> do
                url `shouldBe` "https://slack.com/api/chat.postMessage"
                IORef.modifyIORef' posts (+ 1)
                pure $ Just "{\"ok\":true,\"channel\":\"C_NOTIF_CHANNEL\",\"ts\":\"1735689600.000001\"}"
              provider :: ATAuthCtx a -> ATAuthCtx a
              provider = interpose @ELLM.LLM \_ -> \case
                ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
                  [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "publish" "function" $ LLM.ToolFunction "send_to_slack" $ one ("message", AE.String "Retained operation test")]}
                  evidence -> LLM.Message LLM.Assistant (T.intercalate "\n" evidence) LLM.defaultMessageData
                ELLM.CallLLM{} -> pure $ Right "Retained Slack publication"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding"
              execute = tryAny $ void $ runAsBaseRecordingHTTP tr $ atAuthToBase tr.trSessAndHeader $ transport $ provider $ AIThreads.startThreadPostHWithTools (Just Routes.mcpProjectTools) testPid form
              ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
              reject =
                if boundary == "checkpoint"
                  then "ALTER TABLE apis.slack_turn_checkpoints ADD CONSTRAINT reject_publication_checkpoint CHECK (NOT (checkpoint->>'tag' = 'ToolsPending' AND jsonb_array_length(checkpoint->'contents'->2) > 0)) NOT VALID"
                  else "ALTER TABLE apis.ai_agent_operations ADD CONSTRAINT reject_publication_outcome CHECK (outcome->>'tag' <> 'OperationReturned') NOT VALID"
              remove =
                if boundary == "checkpoint"
                  then "ALTER TABLE apis.slack_turn_checkpoints DROP CONSTRAINT reject_publication_checkpoint"
                  else "ALTER TABLE apis.ai_agent_operations DROP CONSTRAINT reject_publication_outcome"
          bracket_ (ddl reject) (ddl remove) $ execute >>= (`shouldSatisfy` isLeft)
          IORef.readIORef posts >>= (`shouldBe` 1)
          execute >>= (`shouldSatisfy` isRight)
          IORef.readIORef posts >>= (`shouldBe` 1)
          runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid request) >>= \case
            [entry] -> case entry.outcome of
              Investigations.OperationReturned result
                | boundary == "checkpoint" ->
                    (result.rawData >>= (^? key "response" . key "ts" . _String)) `shouldBe` Just "1735689600.000001"
              Investigations.OperationUnknown | boundary == "outcome" -> pure ()
              _ -> expectationFailure "Expected the retained Slack acknowledgement or an explicit uncertain outcome"
            _ -> expectationFailure "Expected one publication operation"

      for_ (["retry", "timeout"] :: [Text]) \boundary ->
        it ("agentOperation_concurrentRecoveryRetainsLateOutcomeWithoutRepeatingMutationFor" <> toString boundary) \tr -> do
          entered <- newEmptyMVar
          release <- newEmptyMVar
          calls <- IORef.newIORef (0 :: Int)
          let request = UUIDId $ UUID.fromWords 0 0 0 202
              form = fromRightShow $ urlDecodeAsForm @AIThreads.AIChatForm "query=Create+a+dashboard+once&mode=chat&allowActions=true&requestId=00000000-0000-0000-0000-0000000000ca"
              original = Routes.mcpProjectTools
              backend =
                original
                  { AI.invoke = \principal pid name args -> do
                      liftIO $ IORef.modifyIORef' calls (+ 1)
                      result <- AI.invoke original principal pid name args
                      liftIO $ putMVar entered () >> takeMVar release
                      pure result
                  }
              provider :: ATAuthCtx a -> ATAuthCtx a
              provider = interpose @ELLM.LLM \_ -> \case
                ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
                  [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "create" "function" $ LLM.ToolFunction "create_dashboard" $ one ("body", AE.object ["title" AE..= ("Concurrent durable action" :: Text)])]}
                  evidence -> LLM.Message LLM.Assistant (T.intercalate "\n" evidence) LLM.defaultMessageData
                ELLM.CallLLM{} -> pure $ Right "Concurrent action"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding"
              execute = tryAny $ void $ runAuthHandler tr $ provider $ AIThreads.startThreadPostHWithTools (Just backend) testPid form
          Async.withAsync execute \worker -> do
            Timeout.timeout 30_000_000 (takeMVar entered) >>= (`shouldBe` Just ())
            bracket_ (pure ()) (putMVar release ()) do
              case boundary of
                "retry" -> Timeout.timeout 30_000_000 execute >>= (`shouldSatisfy` maybe False isRight)
                _ -> do
                  Just run <- runQueryEffect tr $ Issues.getRoutineRun testPid request
                  let user = (getResponse tr.trSessAndHeader).user.id
                  runQueryEffect tr (Issues.runContextFor testPid $ Issues.BackgroundClaim user run.conversationId request $ addUTCTime 1 run.startedAt) >>= (`shouldBe` Nothing)
                  runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid request) >>= \case
                    [entry] -> entry.outcome `shouldSatisfy` \case Investigations.OperationRunning -> True; _ -> False
                    _ -> expectationFailure "An invalid completion claim must not change the outstanding operation"
                  context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.InteractiveClaim user run.conversationId request
                  runTestBg frozenTime tr (Issues.completeRun context $ Issues.TimedOutResult $ Issues.RunFailure "Execution timed out" Issues.RunOnly) >>= \case
                    Issues.RunCompleted{} -> pass
                    _ -> expectationFailure "Expected the current run to time out"
              IORef.readIORef calls >>= (`shouldBe` 1)
              runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid request) >>= \case
                [entry] -> entry.outcome `shouldSatisfy` \case Investigations.OperationUnknown -> True; _ -> False
                _ -> expectationFailure "Expected one in-flight operation with an uncertain outcome"
            Timeout.timeout 30_000_000 (Async.wait worker) >>= (`shouldSatisfy` maybe False isLeft)
          runQueryEffect tr (Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM projects.dashboards WHERE project_id = #{testPid} AND title = 'Concurrent durable action'|]) >>= (`shouldBe` 1)
          runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid request) >>= \case
            [entry] -> entry.outcome `shouldSatisfy` \case Investigations.OperationReturned{} -> True; _ -> False
            _ -> expectationFailure "Expected the original worker's late response to improve retained evidence"

      it "agentOperation_catalogExposesTheExecutionRecoveryContract" \tr -> do
        mode <- interactiveInvocation tr testPid (UUIDId UUID.nil) "Inspect tool recovery" Issues.Autonomous
        let conversation = UUIDId UUID.nil
            config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember (getResponse tr.trSessAndHeader).user.id, AI.conversationId = Just conversation, AI.invocationMode = mode, AI.projectTools = Just Routes.mcpProjectTools}
        void $ runQueryEffect tr $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
        for_ ([("get_project", "replay_reads", ProjectMembers.PView, ProjectMembers.PView), ("save_artifact", "atomic_in_run", ProjectMembers.PView, ProjectMembers.PView), ("write_project_memory", "atomic_in_run", ProjectMembers.PEdit, ProjectMembers.PView), ("send_to_slack", "retain_in_run", ProjectMembers.PEdit, ProjectMembers.PView), ("delete_dashboard", "retain_in_run", ProjectMembers.PAdmin, ProjectMembers.PView), ("create_api_key", "retain_in_run", ProjectMembers.PAdmin, ProjectMembers.PAdmin)] :: [(Text, Text, ProjectMembers.Permissions, ProjectMembers.Permissions)]) \(name, expected, role, audience) -> do
          result <- runTestBg frozenTime tr $ AI.executeToolCall config $ LLM.ToolCall "schema" "function" $ LLM.ToolFunction "get_project_tool_schema" $ one ("name", AE.String name)
          (AE.decode @AE.Value (encodeUtf8 result.formatted) >>= (^? key "agent_recovery" . _String)) `shouldBe` Just expected
          for_ ([("minimum_permission", role), ("evidence_permission", audience)] :: [(AE.Key, ProjectMembers.Permissions)]) \(field, permission) ->
            (AE.decode @AE.Value (encodeUtf8 result.formatted) >>= (^? key field)) `shouldBe` Just (AE.toJSON permission)

      it "requires an explicit per-message grant for project changes" \tr -> do
        let absent = urlDecodeAsForm @AIThreads.AIChatForm "query=check+metrics&mode=chat"
            granted = urlDecodeAsForm @AIThreads.AIChatForm "query=acknowledge+issue&mode=chat&allowActions=true"
        fmap (.allowActions) absent `shouldBe` Right Nothing
        fmap (.allowActions) granted `shouldBe` Right (Just True)
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid (UUIDId UUID.nil) Nothing Nothing
        (renderT threadPage) `shouldSatisfy` T.isInfixOf "name=\"allowActions\""
        T.count "name=\"requestId\"" (renderT threadPage) `shouldBe` 2

      for_ (["thread", "new", "routine", "issue", "issue-thread"] :: [Text]) \origin ->
        it ("webRequestCheckpoint_retriesWithoutRepeatingReadsOrConversationMessages: " <> toString origin) \tr -> do
          issueId <- runTestBg frozenTime tr do
            issue <- Issues.createQueryAlertIssue testPid "checkpoint-alert" "Checkout checkpoint context" "service == checkout" 100 200 Issues.Above
            Issues.insertIssueReturningId issue
          let isIssue = "issue" `T.isPrefixOf` origin
              existingConversation = if isIssue then UUIDId issueId.unUUIDId else UUIDId $ UUID.fromWords 0 0 92 1
              issueForm = fromRightShow $ urlDecodeAsForm @IssuePage.AIChatForm "query=Inspect+the+project&requestId=00000000-0000-0000-0000-000000000092"
              form = fromRightShow $ urlDecodeAsForm @AIThreads.AIChatForm $ "query=Inspect+the+project&requestId=00000000-0000-0000-0000-000000000092&" <> if origin == "routine" then "mode=routine&intervalMinutes=15" else "mode=chat"
          when (origin `elem` ["thread", "issue-thread"]) do
            void $ runTestBg frozenTime tr $ Issues.getOrCreateConversation testPid existingConversation (if isIssue then Issues.CTAnomaly else Issues.CTWeb) (AE.object [])
            runQueryEffect tr $ Issues.renameConversation testPid existingConversation "Checkpoint regression"
          calls <- IORef.newIORef ([] :: [Text])
          models <- IORef.newIORef ([] :: [Maybe Text])
          interrupt <- IORef.newIORef True
          let original = Routes.mcpProjectTools
              backend =
                original
                  { AI.invoke = \principal pid name args -> do
                      liftIO $ IORef.modifyIORef' calls (<> [name])
                      when (name == "get_schema") $ liftIO do
                        stop <- IORef.atomicModifyIORef' interrupt (\old -> (False, old))
                        when stop $ throwIO $ ErrorCall "Worker interrupted after a completed web read"
                      AI.invoke original principal pid name args
                  }
              provider :: ATAuthCtx a -> ATAuthCtx a
              provider = interpose @ELLM.LLM \_ -> \case
                ELLM.CallAgenticChat history request _ -> do
                  when isIssue $ liftIO $ unlines [message.content | message <- toList history, message.role == LLM.System] `shouldSatisfy` T.isInfixOf "Checkout checkpoint context"
                  liftIO $ IORef.modifyIORef' models (<> [AE.toJSON request ^? key "model" . _String])
                  let results = [message.content | message <- toList history, message.role == LLM.Tool]
                  pure
                    $ Right
                    $ if null results
                      then LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall name "function" $ LLM.ToolFunction name mempty | name <- ["get_project", "get_schema"]]}
                      else LLM.Message LLM.Assistant "The project evidence is retained." LLM.defaultMessageData
                ELLM.CallLLM{} -> pure $ Left "Unexpected title request"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
              submit changed = case origin of
                "issue" -> IssuePage.aiChatPostHWithTools (Just backend) testPid issueId issueForm{IssuePage.query = changed.query}
                "issue-thread" -> AIThreads.threadPostHWithTools (Just backend) testPid existingConversation changed
                "thread" -> AIThreads.threadPostHWithTools (Just backend) testPid existingConversation changed
                _ -> AIThreads.startThreadPostHWithTools (Just backend) testPid changed
              execute = tryAny $ void $ runAuthHandler tr $ provider $ submit form
          execute >>= (`shouldSatisfy` isLeft)
          let ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
          bracket_
            (ddl "ALTER TABLE apis.ai_routine_runs ADD CONSTRAINT completion_regression CHECK (status <> 'succeeded')")
            (ddl "ALTER TABLE apis.ai_routine_runs DROP CONSTRAINT completion_regression")
            do
              completed <- execute
              IORef.readIORef calls >>= (`shouldBe` ["get_project", "get_schema", "get_schema"])
              completed `shouldSatisfy` isLeft
              pending <- runQueryEffect tr (Issues.getRoutineRun testPid $ UUIDId $ UUID.fromWords 0 0 0 146) >>= maybe (fail "Missing web run") pure
              interruptedHistory <- runQueryEffect tr $ Issues.selectChatHistory testPid pending.conversationId
              [message.content | message <- interruptedHistory, message.role == Issues.ChatAssistant] `shouldBe` []
              pending.status `shouldBe` Issues.RunRunning
          pending <- runQueryEffect tr (Issues.getRoutineRun testPid $ UUIDId $ UUID.fromWords 0 0 0 146) >>= maybe (fail "Missing web run") pure
          let conversation = pending.conversationId
          execute >>= (`shouldSatisfy` isRight)
          IORef.readIORef calls >>= (`shouldBe` ["get_project", "get_schema", "get_schema"])
          when (origin == "routine") $ runQueryEffect tr $ Issues.pauseRoutine testPid conversation
          execute >>= (`shouldSatisfy` isRight)
          when (origin == "routine") do
            saved <- runQueryEffect tr $ Issues.getConversation testPid conversation
            fmap (.routineActive) saved `shouldBe` Just False
          IORef.readIORef calls >>= (`shouldBe` ["get_project", "get_schema", "get_schema"])
          IORef.readIORef models >>= (\requests -> length requests `shouldBe` 2)
          when (origin == "issue") do
            response <- runAuthHandler tr $ provider $ submit form
            let html = renderT $ getResponse response
            T.count "The project evidence is retained." html `shouldBe` 1
            T.count "name=\"requestId\"" html `shouldBe` 1
            html `shouldSatisfy` T.isInfixOf "hx-swap-oob=\"outerHTML\""
            html `shouldSatisfy` (not . T.isInfixOf "name=\"requestId\" value=\"00000000-0000-0000-0000-000000000092\"")
            (_, page) <- testServant tr $ IssuePage.issueDetailGetH testPid issueId Nothing Nothing Nothing Nothing Nothing
            let pageHtml = renderT page
            pageHtml `shouldSatisfy` T.isInfixOf "id=\"issue-ai-form\""
            T.count "name=\"requestId\"" pageHtml `shouldBe` 2
          history <- runQueryEffect tr $ Issues.selectChatHistory testPid conversation
          [message.content | message <- history, message.role == Issues.ChatUser] `shouldBe` ["Inspect the project"]
          [message.content | message <- history, message.role == Issues.ChatAssistant] `shouldBe` ["The project evidence is retained."]
          runs <- runQueryEffect tr $ Issues.listConversationRuns testPid conversation 20
          map (.status) runs `shouldBe` [Issues.RunSucceeded]
          for_ (([form{AIThreads.query = "A different request"}] <> [form{AIThreads.allowActions = Just True} | not isIssue]) :: [AIThreads.AIChatForm]) \changed -> do
            status <- runAuthHandler tr $ provider $ (void (submit changed) $> 200) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
            status `shouldBe` 409
          when (origin `elem` ["new", "routine", "issue"]) do
            runQueryEffect tr $ Issues.deleteConversation testPid conversation
            deleted <- runAuthHandler tr $ provider $ (void (submit form) $> 200) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
            deleted `shouldBe` 409
          let user = (getResponse tr.trSessAndHeader).user.id
          runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|DELETE FROM projects.project_members WHERE project_id = #{testPid} AND user_id = #{user}|]
          revoked <- runAuthHandler tr $ provider $ (void (submit form) $> 200) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
          revoked `shouldBe` 403
          IORef.readIORef calls >>= (`shouldBe` ["get_project", "get_schema", "get_schema"])

      it "assistantPages_defaultToAssistant" \tr -> do
        let convId = UUIDId UUID.nil
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        let html = renderT threadPage
        html `shouldSatisfy` T.isInfixOf "id=\"nav-view-ai\" class=\"sr-only\" checked"

      it "issueConversation_linksBackToItsIssue" \tr -> do
        let issueId = UUIDId UUID.nil :: Issues.IssueId
            convId = UUIDId issueId.unUUIDId
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTAnomaly})|]
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        let html = renderT threadPage
        html `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/issues/" <> issueId.toText <> "\"")

      it "assistantSectionHeadings_linkAndBackdropCoversViewport" \tr -> do
        let convId = UUIDId UUID.nil
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
        (_, conversationPage) <- testServant tr $ AIThreads.threadsGetH testPid
        (_, routinesPage) <- testServant tr $ AIThreads.routinesGetH testPid
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        let render = renderT
            conversationHtml = render conversationPage
        all (`T.isInfixOf` conversationHtml) ["href=\"/p/" <> testPid.toText <> "/ai/routines\"", "Routines", "href=\"/p/" <> testPid.toText <> "/ai\"", "Conversations"] `shouldBe` True
        all (`T.isInfixOf` conversationHtml) ["id=\"ai-composer-modal\"", "backdrop:backdrop-blur-sm"] `shouldBe` True
        for_ [conversationHtml, render routinesPage, render threadPage] (`shouldSatisfy` T.isInfixOf "data-header-actions")

      it "eventRoutineActivation_appearsInstalledWithoutAnInterval" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        conversations <- runTestBg frozenTime tr $ Issues.listConversations testPid
        conversation <- maybe (fail "Missing Slack routine") pure $ find ((== Just "slack-investigation") . (.templateKey)) conversations
        (_, page) <- testServant tr $ AIThreads.routinesGetH testPid
        let html = renderT page
        html `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/ai/" <> conversation.conversationId.toText <> "\"")
        html `shouldSatisfy` T.isInfixOf "1 routine"
        (_, thread) <- testServant tr $ AIThreads.threadGetH testPid conversation.conversationId Nothing Nothing
        (renderT thread) `shouldSatisfy` T.isInfixOf "Routine instructions"

        void $ testServant tr $ AIThreads.routinePostH testPid conversation.conversationId (AIThreads.RoutineForm $ fromRightShow $ Issues.mkRoutineInterval 15)
        unchanged <- runQueryEffect tr $ Issues.getConversation testPid conversation.conversationId
        (unchanged >>= (.routineKind)) `shouldBe` Just Issues.ScheduleSlackMessage

      it "legacySlackAudience_upgradeProtectsStoredHistoryWithoutCurrentIdentityLinks" \tr -> do
        setupSlackAgent tr testPid "T_LEGACY_SCOPE"
        let user = (getResponse tr.trSessAndHeader).user.id
            context channel = AE.object ["team_id" AE..= ("T_LEGACY_SCOPE" :: Text), "channel_id" AE..= (channel :: Text), "thread_ts" AE..= ("1735689600.000001" :: Text)]
            channelAudience = Investigations.SlackChannelAudience $ fromRightShow $ Integrations.slackChannel "T_LEGACY_SCOPE" "G123LEGACY"
            cases = [(40, Issues.CTSlackThread, context "G123LEGACY", channelAudience, 403), (41, Issues.CTSlackThread, context "D123LEGACY", Investigations.PersonalAudience user, 200), (42, Issues.CTSlackThread, AE.object [], Investigations.NoAudience, 403), (43, Issues.CTSlackThread, context "D123UNKNOWN", Investigations.NoAudience, 403), (44, Issues.CTWeb, AE.object [], Investigations.ProjectAudience, 200)] :: [(Word32, Issues.ConversationType, AE.Value, Investigations.EvidenceAudience, Int)]
        for_ cases $ \(suffix, kind, metadata, _, _) -> do
          let conversation = UUIDId $ UUID.fromWords 0 0 44 suffix
              checkpoint = Aeson $ Investigations.AnswerReady $ AI.AgenticChatResult "Legacy Slack history canary" []
          runTestBg frozenTime tr do
            void $ Issues.getOrCreateConversation testPid conversation kind metadata
            Issues.insertChatMessage testPid conversation Issues.ChatAssistant "Legacy Slack history canary" Nothing Nothing
          void $ interactiveInvocation tr testPid conversation "Resume legacy evidence" Issues.Observe
          when (kind == Issues.CTSlackThread)
            $ runQueryEffect tr
            $ Hasql.interpExecute_
              [HI.sql|INSERT INTO apis.slack_turn_checkpoints (project_id, conversation_id, user_id, message_ts, checkpoint)
            VALUES (#{testPid}, #{conversation}, #{user}, '1735689601.000001', #{checkpoint})|]
          when (suffix == 43)
            $ runQueryEffect tr
            $ Hasql.interpExecute_
              [HI.sql|UPDATE apis.ai_routine_runs SET requested_by = NULL, source = #{Issues.TriggeredRun $ Issues.SlackCommand conversation}
            WHERE project_id = #{testPid} AND conversation_id = #{conversation}|]
        let deleted = Issues.slackScopedConversationId testPid "T_LEGACY_SCOPE" "G123LEGACY" "1735689602.000001"
        receipt <- receiveSlackEvent tr $ slackThreadedEvent "T_LEGACY_SCOPE" "G123LEGACY" "Retained incident evidence" "1735689603.000001" "1735689602.000001"
        Just routine <- runQueryEffect tr $ Issues.getEventRoutine testPid Issues.EventSlackMessage
        runTestBg frozenTime tr $ void $ Issues.getOrCreateConversation testPid deleted Issues.CTAnomaly (AE.object [])
        Just retained <- runTestBg frozenTime tr $ Issues.startEventRun routine (Issues.SlackMessageRequest receipt user) Issues.StartRun deleted "Retained incident evidence"
        runTestBg frozenTime tr $ Issues.deleteConversation testPid deleted
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|DELETE FROM apis.slack_identities WHERE team_id = 'T_LEGACY_SCOPE'|]
        migration <- readFileBS "static/migrations/0274_legacy_slack_evidence_audience.sql"
        replicateM_ 2 $ withResource tr.trPool $ \conn -> void $ PGS.execute_ conn $ Query migration
        for_ cases $ \(suffix, _, _, audience, expected) -> do
          let conversation = UUIDId $ UUID.fromWords 0 0 44 suffix
          runQueryEffect tr (Investigations.conversationEvidenceAudience testPid conversation) >>= (`shouldBe` audience)
          status <- runAuthHandler tr $ (AIThreads.threadGetH testPid conversation Nothing Nothing $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
          status `shouldBe` expected
        runQueryEffect tr (Investigations.conversationEvidenceAudience testPid deleted) >>= (`shouldBe` channelAudience)
        retainedStatus <- runAuthHandler tr $ (AIThreads.routineRunGetH testPid retained.id Nothing $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
        retainedStatus `shouldBe` 403
        runTestBg frozenTime tr $ void $ Issues.getOrCreateConversation testPid deleted Issues.CTWeb (AE.object [])
        runQueryEffect tr (Investigations.conversationEvidenceAudience testPid deleted) >>= (`shouldBe` channelAudience)

      it "evidenceAudience_projectAdministratorCannotReadAnotherMembersPrivateConversation" \tr -> do
        let conversation = UUIDId $ UUID.fromWords 0 0 44 20
            otherMember = UUIDId $ UUID.fromWords 0 0 44 21
            secret = "personal-slack-evidence-canary"
            user = (getResponse tr.trSessAndHeader).user.id
            privateAudience = Investigations.PersonalAudience otherMember
        mode <- interactiveInvocation tr testPid conversation "Private Slack investigation" Issues.Autonomous
        runTestBg frozenTime tr $ Issues.insertChatMessage testPid conversation Issues.ChatAssistant secret Nothing Nothing
        runQueryEffect tr (Investigations.retainEvidenceAudience testPid conversation privateAudience) >>= (`shouldBe` True)
        runQueryEffect tr (Investigations.retainEvidenceAudience testPid conversation Investigations.ProjectAudience) >>= (`shouldBe` True)
        runQueryEffect tr (Investigations.retainEvidenceAudience testPid conversation $ Investigations.PersonalAudience user) >>= (`shouldBe` False)
        runQueryEffect tr (Investigations.conversationEvidenceAudience testPid conversation) >>= (`shouldBe` privateAudience)
        for_
          [ AIThreads.threadGetH testPid conversation Nothing Nothing $> ()
          , AIThreads.threadTitlePostH testPid conversation (AIThreads.TitleForm "Leaked title") $> ()
          , AIThreads.threadDeleteH testPid conversation $> ()
          , AIThreads.routinePausePostH testPid conversation $> ()
          ]
          $ \handler -> do
            status <- runAuthHandler tr $ (handler $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
            status `shouldBe` 403
        for_ [AIThreads.threadsGetH testPid, AIThreads.routinesGetH testPid] $ \handler -> do
          (_, page) <- testServant tr handler
          renderT page `shouldSatisfy` (not . T.isInfixOf "Private Slack investigation")
        let config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.WebSession user, AI.conversationId = Just conversation, AI.invocationMode = mode}
        replay <- tryAny $ runTestBg frozenTime tr $ AI.executeToolCall config (LLM.ToolCall "private-document" "function" $ LLM.ToolFunction "read_artifact" mempty)
        replay `shouldSatisfy` \case Left err -> isJust (fromException @AI.AgentAccessDenied err); Right _ -> False
        runTestBg frozenTime tr $ Issues.deleteConversation testPid conversation
        runTestBg frozenTime tr $ void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
        runQueryEffect tr (Investigations.conversationEvidenceAudience testPid conversation) >>= (`shouldBe` privateAudience)

      it "evidenceAudience_publicDemoDoesNotRestorePersonalAccessAfterMembershipRevocation" \tr -> do
        let owner = (getResponse tr.trSessAndHeader).user.id
            personal = UUIDId $ UUID.fromWords 0 0 44 50
            public = UUIDId $ UUID.fromWords 0 0 44 51
        runTestBg frozenTime tr $ for_ [personal, public] $ \conversation -> do
          void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
          Issues.insertChatMessage testPid conversation Issues.ChatAssistant "Retained evidence" Nothing Nothing
        runQueryEffect tr (Investigations.retainEvidenceAudience testPid personal $ Investigations.PersonalAudience owner) >>= (`shouldBe` True)
        runQueryEffect tr do
          Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET active = FALSE WHERE project_id = #{testPid} AND user_id = #{owner}|]
          Hasql.interpExecute_ [HI.sql|UPDATE users.users SET is_sudo = FALSE WHERE id = #{owner}|]
        for_ ([(public, 200), (personal, 403)] :: [(UUIDId "conversation", Int)]) $ \(conversation, expected) -> do
          status <- runAuthHandler tr $ (AIThreads.threadGetH testPid conversation Nothing Nothing $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
          status `shouldBe` expected

      it "evidenceAudience_ownerCanReadButCannotPublishPrivateEvidenceToProjectResources" \tr -> do
        let conversation = UUIDId $ UUID.fromWords 0 0 44 22
            user = (getResponse tr.trSessAndHeader).user.id
            secret = "own-private-slack-evidence"
        mode <- interactiveInvocation tr testPid conversation "My private Slack investigation" Issues.Autonomous
        runTestBg frozenTime tr $ Issues.insertChatMessage testPid conversation Issues.ChatAssistant secret Nothing Nothing
        runQueryEffect tr (Investigations.retainEvidenceAudience testPid conversation $ Investigations.PersonalAudience user) >>= (`shouldBe` True)
        (_, page) <- testServant tr $ AIThreads.threadGetH testPid conversation Nothing Nothing
        renderT page `shouldSatisfy` T.isInfixOf secret
        let config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.WebSession user, AI.conversationId = Just conversation, AI.invocationMode = mode, AI.projectTools = Just Routes.mcpProjectTools}
        denied <- runTestBg frozenTime tr $ AI.executeToolCall config (LLM.ToolCall "shared-dashboard" "function" $ LLM.ToolFunction "create_dashboard" $ one ("body", AE.object ["title" AE..= secret]))
        denied.formatted `shouldSatisfy` T.isInfixOf "restricted evidence"
        runQueryEffect tr (Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM projects.dashboards WHERE project_id = #{testPid} AND title = #{secret}|]) >>= (`shouldBe` 0)

      for_ [False, True] $ \legacy ->
        it ("evidenceDisclosure_adminReadRestrictsItsConversationAfterRoleChange: " <> show legacy) \tr -> do
          let conversation = UUIDId $ UUID.fromWords 0 0 44 1
              user = (getResponse tr.trSessAndHeader).user.id
              secret = "administrator-evidence-canary"
          runQueryEffect tr (ProjectMembers.getSessionPermission testPid user) >>= (`shouldBe` Just ProjectMembers.PAdmin)
          mode <- interactiveInvocation tr testPid conversation "Inspect project credentials" Issues.Autonomous
          let config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.WebSession user, AI.conversationId = Just conversation, AI.invocationMode = mode, AI.projectTools = Just Routes.mcpProjectTools}
              provider :: ATBackgroundCtx a -> ATBackgroundCtx a
              provider = interpose @ELLM.LLM \_ -> \case
                ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
                  [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "credentials" "function" $ LLM.ToolFunction "list_api_keys" mempty]}
                  [_] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "document" "function" $ LLM.ToolFunction "save_artifact" $ Map.fromList [("key", AE.String "admin-evidence"), ("title", AE.String "Administrative findings"), ("content", AE.String secret)]]}
                  [_, _] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "public-dashboard" "function" $ LLM.ToolFunction "create_dashboard" $ one ("body", AE.object ["title" AE..= secret])]}
                  [_, _, _] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "public-team" "function" $ LLM.ToolFunction "create_team" $ one ("body", AE.object ["name" AE..= secret, "handle" AE..= ("private-evidence-canary" :: Text)])]}
                  _ -> LLM.Message LLM.Assistant secret LLM.defaultMessageData
                ELLM.CallLLM{} -> pure $ Right "Credential inventory"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
          runTestBg frozenTime tr $ provider do
            answer <- AI.runAgenticChatWithHistory config "Inspect project credentials" "model" "key" >>= either (throwIO . ErrorCall . toString) pure
            liftIO $ map (.name) answer.toolCalls `shouldBe` ["list_api_keys", "save_artifact", "create_dashboard", "create_team"]
            liftIO $ map (.rawData) (take 2 answer.toolCalls) `shouldSatisfy` all isJust
            liftIO $ map (.resultPreview) (take 2 answer.toolCalls) `shouldSatisfy` all (not . T.isInfixOf "Tool failed:")
            void $ AI.completeAgenticResponse config "Inspect project credentials" "model" "key" answer
          runQueryEffect tr (Investigations.conversationEvidencePermission testPid conversation) >>= (`shouldBe` ProjectMembers.PAdmin)
          runQueryEffect
            tr
            ( Hasql.interpScalar @Int64
                [HI.sql|SELECT
            (SELECT count(*) FROM projects.dashboards WHERE project_id = #{testPid} AND title = #{secret}) +
            (SELECT count(*) FROM projects.teams WHERE project_id = #{testPid} AND name = #{secret})|]
            )
            >>= (`shouldBe` 0)
          (_, administratorView) <- testServant tr $ AIThreads.threadGetH testPid conversation Nothing Nothing
          renderT administratorView `shouldSatisfy` T.isInfixOf secret
          for_ ["send_to_slack", "write_project_memory"] $ \name -> do
            denied <- runTestBg frozenTime tr $ AI.executeToolCall config (LLM.ToolCall "shared-destination" "function" $ LLM.ToolFunction name mempty)
            denied.formatted `shouldSatisfy` T.isInfixOf "restricted evidence"
          when legacy $ resetLegacyEvidenceScope tr testPid conversation
          runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
          (status, readerView) <- runAuthHandler tr $ ((\response -> (200, renderT $ getResponse response)) <$> AIThreads.threadGetH testPid conversation Nothing Nothing) `catchError` \_ (err :: ServerError) -> pure (err.errHTTPCode, "")
          readerView `shouldSatisfy` (not . T.isInfixOf secret)
          status `shouldBe` 403
          for_ [AIThreads.threadsGetH testPid, AIThreads.routinesGetH testPid] $ \handler -> do
            (_, page) <- testServant tr handler
            renderT page `shouldSatisfy` (not . T.isInfixOf "Credential inventory")
          replay <- tryAny $ runTestBg frozenTime tr $ provider $ AI.runAgenticChatWithHistory config "Read the saved private answer" "model" "key"
          replay `shouldSatisfy` \case Left err -> isJust (fromException @AI.AgentAccessDenied err); Right _ -> False
          document <- tryAny $ runTestBg frozenTime tr $ AI.executeToolCall config (LLM.ToolCall "read-private-document" "function" $ LLM.ToolFunction "read_artifact" $ Map.fromList [("key", AE.String "admin-evidence")])
          document `shouldSatisfy` \case Left err -> isJust (fromException @AI.AgentAccessDenied err); Right _ -> False
          case mode of
            AI.DurableRun context -> do
              runStatus <- runAuthHandler tr $ (AIThreads.routineRunGetH testPid context.id Nothing $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
              runStatus `shouldBe` 403
            AI.InteractiveReadOnly -> expectationFailure "Expected a durable private run"
          runQueryEffect tr do
            Hasql.interpExecute_ [HI.sql|UPDATE users.users SET is_sudo = TRUE WHERE id = #{user}|]
            Hasql.interpExecute_ [HI.sql|DELETE FROM projects.project_members WHERE project_id = #{testPid} AND user_id = #{user}|]
          recovered <- runTestBg frozenTime tr $ provider $ AI.runAgenticChatWithHistory config "Read the private result as a sudo session" "model" "key"
          fmap (.response) recovered `shouldBe` Right secret
          memberOnly <- tryAny $ runTestBg frozenTime tr $ AI.requireEvidenceAccess (AI.MemberAccess AI.ProjectMember user) testPid conversation
          memberOnly `shouldSatisfy` \case Left err -> isJust (fromException @AI.AgentAccessDenied err); Right _ -> False

      it "evidenceDisclosure_titleInferenceRechecksAccessBeforeReturningAnAnswer" \tr -> do
        let conversation = UUIDId $ UUID.fromWords 0 0 44 9
            user = (getResponse tr.trSessAndHeader).user.id
        mode <- interactiveInvocation tr testPid conversation "Inspect project credentials" Issues.Observe
        let config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.WebSession user, AI.conversationId = Just conversation, AI.invocationMode = mode, AI.projectTools = Just Routes.mcpProjectTools}
            provider = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
                [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "credentials" "function" $ LLM.ToolFunction "list_api_keys" mempty]}
                _ -> LLM.Message LLM.Assistant "Private credential findings" LLM.defaultMessageData
              ELLM.CallLLM{} -> do
                Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
                pure $ Right "Private title"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
        outcome <- tryAny $ runTestBg frozenTime tr $ provider do
          answer <- AI.runAgenticChatWithHistory config "Inspect project credentials" "model" "key" >>= either (throwIO . ErrorCall . toString) pure
          AI.completeAgenticResponse config "Inspect project credentials" "model" "key" answer
        outcome `shouldSatisfy` \case Left err -> isJust (fromException @AI.AgentAccessDenied err); Right _ -> False

      it "evidenceDisclosure_roleChangeDuringInferenceCannotPublishOrPoisonAnotherConversation" \tr -> do
        let conversation = UUIDId $ UUID.fromWords 0 0 44 3
            user = (getResponse tr.trSessAndHeader).user.id
            secret = "private-inflight-answer"
            configuration mode target = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.WebSession user, AI.conversationId = Just target, AI.invocationMode = mode, AI.projectTools = Just Routes.mcpProjectTools}
        mode <- interactiveInvocation tr testPid conversation "Inspect restricted evidence" Issues.Observe
        let provider = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat history _ _ -> case [message.content | message <- toList history, message.role == LLM.Tool] of
                [] -> pure $ Right $ LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "credentials" "function" $ LLM.ToolFunction "list_api_keys" mempty]}
                _ -> do
                  Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
                  pure $ Right $ LLM.Message LLM.Assistant secret LLM.defaultMessageData
              ELLM.CallLLM{} -> pure $ Left "Unexpected title request"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
        outcome <- tryAny $ runTestBg frozenTime tr $ provider $ AI.runAgenticChatWithHistory (configuration mode conversation) "Inspect restricted evidence" "model" "key"
        outcome `shouldSatisfy` \case Left err -> isJust (fromException @AI.AgentAccessDenied err); Right _ -> False
        case mode of
          AI.DurableRun context -> do
            recorded <- runQueryEffect tr $ Issues.completeRun context $ Issues.CompletedResult Issues.ResultSucceeded Issues.PublishAnswer $ AI.prepareRunAnswer $ AI.AgenticChatResult secret []
            recorded `shouldSatisfy` \case Issues.RunCompleted Issues.CompletedCancelled Nothing -> True; _ -> False
            runQueryEffect tr (Issues.getRoutineRun testPid context.id) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunCancelled)
            runQueryEffect tr (Issues.listRunArtifacts testPid context.id) >>= (`shouldSatisfy` null)
          AI.InteractiveReadOnly -> expectationFailure "Expected a durable request"
        history <- runQueryEffect tr $ Issues.selectChatHistory testPid conversation
        [message.content | message <- history, message.role == Issues.ChatAssistant] `shouldBe` []
        let publicConversation = UUIDId $ UUID.fromWords 0 0 44 4
        public <- interactiveInvocation tr testPid publicConversation "Ordinary reader request" Issues.Observe
        denied <- runTestBg frozenTime tr $ AI.executeToolCall (configuration public publicConversation) (LLM.ToolCall "denied-read" "function" $ LLM.ToolFunction "list_api_keys" mempty)
        denied.formatted `shouldSatisfy` T.isInfixOf "required admin permission"
        runQueryEffect tr (Investigations.conversationEvidencePermission testPid publicConversation) >>= (`shouldBe` ProjectMembers.PView)

      it "evidenceDisclosure_publicDemoRequestsRetainTheirReadOnlySessionAuthority" \tr -> do
        let conversation = UUIDId $ UUID.fromWords 0 0 44 5
        (user, mode) <- runTestBg frozenTime tr do
          actor <- Projects.createUser "Public" "Visitor" "" "public-agent-visitor@example.test"
          Projects.insertUser actor
          request <- UUIDId <$> UUIDEff.genUUID
          void $ Issues.startInteractiveRun testPid conversation actor.id request "Read public demo evidence" Issues.Observe Issues.WebConversation
          invocation <- AI.DurableRun <$> AI.requireRunContext testPid (Issues.InteractiveClaim actor.id conversation request)
          pure (actor.id, invocation)
        let config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.WebSession user, AI.conversationId = Just conversation, AI.invocationMode = mode}
            provider = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat{} -> pure $ Right $ LLM.Message LLM.Assistant "Public demo evidence" LLM.defaultMessageData
              ELLM.CallLLM{} -> pure $ Left "No title required"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
        runTestBg frozenTime tr $ provider do
          answer <- AI.runAgenticChatWithHistory config "Read public demo evidence" "model" "key" >>= either (throwIO . ErrorCall . toString) pure
          void $ AI.completeAgenticResponse config "Read public demo evidence" "model" "key" answer
        history <- runQueryEffect tr $ Issues.selectChatHistory testPid conversation
        [message.content | message <- history, message.role == Issues.ChatAssistant] `shouldBe` ["Public demo evidence"]

      it "evidenceDisclosure_nativeInputsInheritInstallationScopeWithoutDuplicateActivation" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment")
        installed <- runQueryEffect tr (Issues.getEventRoutine testPid Issues.EventIssueEnrichment) >>= maybe (fail "Missing enrichment installation") pure
        runQueryEffect tr do
          Issues.setRoutineInstructions testPid installed.conversationId "Administrative evidence in the routine instructions."
          Investigations.retainEvidencePermission testPid [installed.conversationId] ProjectMembers.PAdmin
        let conversation = UUIDId $ UUID.fromWords 0 0 44 6
            source = Issues.IssueEnrichment $ UUIDId UUID.nil
        runTestBg frozenTime tr do
          void $ Issues.getOrCreateConversation testPid conversation Issues.CTAnomaly (AE.object [])
          void $ Issues.startEventRun installed (Issues.IssueEnrichmentRequest $ UUIDId UUID.nil) Issues.StartRun conversation "Enrich this issue"
        runQueryEffect tr (Investigations.conversationEvidencePermission testPid conversation) >>= (`shouldBe` ProjectMembers.PAdmin)
        execution <- runQueryEffect tr (Issues.getEventRun testPid source) >>= maybe (fail "Missing native run") pure
        (_, page) <- testServant tr $ AIThreads.routineRunGetH testPid execution.id Nothing
        renderT page `shouldSatisfy` T.isInfixOf "Administrative evidence in the routine instructions."
        resetLegacyEvidenceScope tr testPid conversation
        runQueryEffect tr (Investigations.conversationEvidencePermission testPid conversation) >>= (`shouldBe` ProjectMembers.PAdmin)
        let user = (getResponse tr.trSessAndHeader).user.id
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'edit' WHERE project_id = #{testPid} AND user_id = #{user}|]
        status <- runAuthHandler tr $ (AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment") $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
        status `shouldBe` 403
        installations <- runQueryEffect tr $ Issues.listConversations testPid
        length (filter ((== Just "issue-enrichment") . (.templateKey)) installations) `shouldBe` 1
        runTestBg frozenTime tr $ Issues.deleteConversation testPid installed.conversationId
        resetLegacyEvidenceScope tr testPid conversation
        runQueryEffect tr (Investigations.conversationEvidencePermission testPid conversation) >>= (`shouldBe` ProjectMembers.PAdmin)

      it "evidenceDisclosure_unrecognizedLegacyEvidenceRemainsRestricted" \tr -> do
        let cases = [(7, AE.toJSON ([ToolCallInfo "retired-sensitive-operation" mempty "Historical result" Nothing] :: [ToolCallInfo])), (8, AE.object ["unrecognized_schema" AE..= True])] :: [(Word32, AE.Value)]
            user = (getResponse tr.trSessAndHeader).user.id
        for_ cases $ \(suffix, metadata) -> do
          let conversation = UUIDId $ UUID.fromWords 0 0 44 suffix
          runTestBg frozenTime tr do
            void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
            Issues.insertChatMessage testPid conversation Issues.ChatAssistant "Unclassified historical evidence" Nothing (Just metadata)
          resetLegacyEvidenceScope tr testPid conversation
          runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
          status <- runAuthHandler tr $ (AIThreads.threadGetH testPid conversation Nothing Nothing $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
          status `shouldBe` 403
          runQueryEffect tr (Investigations.conversationEvidencePermission testPid conversation) >>= (`shouldBe` ProjectMembers.PAdmin)

      it "evidenceDisclosure_permissionOrderAndRecreatedConversationsRetainTheirScope" \tr -> do
        let conversation = UUIDId $ UUID.fromWords 0 0 44 2
            roles = [minBound .. maxBound] :: [ProjectMembers.Permissions]
        comparisons <-
          runQueryEffect tr
            $ Hasql.interp @[(ProjectMembers.Permissions, ProjectMembers.Permissions, Bool)]
              [HI.sql|SELECT viewer, required, apis.evidence_permission_rank(viewer) >= apis.evidence_permission_rank(required)
            FROM unnest(#{roles}::projects.project_permissions[]) viewer CROSS JOIN unnest(#{roles}::projects.project_permissions[]) required|]
        comparisons `shouldSatisfy` \rows -> length rows == length roles * length roles && all (\(viewer, required, allowed) -> allowed == (viewer >= required)) rows
        void $ interactiveInvocation tr testPid conversation "Retain private evidence" Issues.Observe
        for_ [ProjectMembers.PEdit, ProjectMembers.PAdmin] \required -> do
          runQueryEffect tr do
            Investigations.retainEvidencePermission testPid [conversation] required
            Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_conversations SET evidence_permission = 'view' WHERE project_id = #{testPid} AND conversation_id = #{conversation}|]
            Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routine_runs SET evidence_permission = 'view' WHERE project_id = #{testPid} AND conversation_id = #{conversation}|]
          runQueryEffect tr (Investigations.conversationEvidencePermission testPid conversation) >>= (`shouldBe` required)
        runTestBg frozenTime tr do
          Issues.deleteConversation testPid conversation
          void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
        void $ interactiveInvocation tr testPid conversation "A new request cannot lower the scope" Issues.Observe
        recorded <-
          runQueryEffect tr
            $ Hasql.interp @[HI.OneColumn ProjectMembers.Permissions]
              [HI.sql|SELECT evidence_permission FROM apis.ai_conversations WHERE project_id = #{testPid} AND conversation_id = #{conversation}
            UNION ALL SELECT evidence_permission FROM apis.ai_routine_runs WHERE project_id = #{testPid} AND conversation_id = #{conversation}|]
        map HI.getOneColumn recorded `shouldBe` replicate 3 ProjectMembers.PAdmin
        other <- createTestProject tr "Other evidence project"
        void $ runTestBg frozenTime tr $ Issues.getOrCreateConversation other conversation Issues.CTWeb (AE.object [])
        runQueryEffect tr (Investigations.conversationEvidencePermission other conversation) >>= (`shouldBe` ProjectMembers.PView)

      for_ [("issue-enrichment", Issues.EventIssueEnrichment, Issues.IssueEnrichment $ UUIDId UUID.nil, Issues.IssueEnrichmentRequest $ UUIDId UUID.nil), ("production-impact-review", Issues.EventPullRequest, Issues.PullRequestReview $ UUIDId UUID.nil, Issues.PullRequestReviewRequest $ UUIDId UUID.nil)] $ \(template, kind, source, request) ->
        it ("nativeRequestRecovery_ownerChangeRetainsOriginalActor: " <> template) \tr -> do
          void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm (toText template))
          installed <- runQueryEffect tr (Issues.getEventRoutine testPid kind) >>= maybe (fail "Missing installed routine") pure
          let conversation = installed.conversationId
              start routine continuation input = runTestBg frozenTime tr (Issues.startEventRun routine request continuation conversation input) >>= maybe (fail "Expected native recovery after owner change") pure
          initial <- start installed Issues.StartRun "Original event input"
          replacement <- runTestBg frozenTime tr do
            actor <- Projects.createUser "Replacement" "Owner" "" "native-replacement@example.test"
            Projects.insertUser actor
            Hasql.interpExecute_ [HI.sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (#{testPid}, #{actor.id}, #{ProjectMembers.PEdit})|]
            void $ Issues.resumeRoutine actor.id testPid conversation
            Issues.setRoutineInstructions testPid conversation "Instructions for future events."
            pure actor.id
          current <- runQueryEffect tr (Issues.getEventRoutine testPid kind) >>= maybe (fail "Missing current routine") pure
          current.requestedBy `shouldBe` replacement
          resumed <- start current Issues.ContinueRun "Changed worker input"
          resumed.id `shouldBe` initial.id
          resumed.requestedBy `shouldBe` installed.requestedBy
          resumed.instructions `shouldBe` installed.instructions
          runQueryEffect tr $ completeTestRun testPid (Issues.EventClaim source resumed.id resumed.startedAt) $ Issues.FailedResult $ Issues.RunFailure "Interrupted worker" Issues.RunOnly
          recovered <- start current Issues.ContinueRun "Another worker input"
          recovered.id `shouldSatisfy` (/= initial.id)
          recovered.requestedBy `shouldBe` installed.requestedBy
          retained <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid recovered.id
          (retained >>= (.input)) `shouldBe` Just "Original event input"
          (retained >>= (.instructions)) `shouldBe` Just installed.instructions
          runQueryEffect tr (Hasql.interpScalar @(UUIDId "ai_routine_run") [HI.sql|SELECT root_run_id FROM apis.ai_routine_runs WHERE id = #{recovered.id}|]) >>= (`shouldBe` initial.id)
          runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|DELETE FROM projects.project_members WHERE project_id = #{testPid} AND user_id = #{installed.requestedBy}|]
          context <- runQueryEffect tr $ AI.requireRunContext testPid $ Issues.EventClaim source recovered.id recovered.startedAt
          runQueryEffect tr (Issues.runContextPolicy context) >>= (`shouldBe` Nothing)
          runQueryEffect tr $ void $ Issues.completeRun context $ Issues.CancelledResult Nothing
          restarted <- start installed Issues.RestartRun "Explicitly restarted event"
          restarted.requestedBy `shouldBe` replacement
          restarted.instructions `shouldBe` current.instructions
          fresh <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid restarted.id
          (fresh >>= (.input)) `shouldBe` Just "Explicitly restarted event"
          runQueryEffect tr (Hasql.interpScalar @(UUIDId "ai_routine_run") [HI.sql|SELECT root_run_id FROM apis.ai_routine_runs WHERE id = #{restarted.id}|]) >>= (`shouldBe` restarted.id)

      it "eventRequestRoot_duplicateWorkersShareOneAttemptAndRecoveryRoot" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        routine <- runQueryEffect tr (Issues.getEventRoutine testPid Issues.EventSlackMessage) >>= maybe (fail "Expected a Slack routine") pure
        let user = (getResponse tr.trSessAndHeader).user.id
            start = runTestBg frozenTime tr $ Issues.startEventRun routine (Issues.SlackCommandRequest routine.conversationId user) Issues.ContinueRun routine.conversationId "One concurrent request"
        (Just initial, Just duplicate) <- Async.concurrently start start
        duplicate.id `shouldBe` initial.id
        runQueryEffect tr $ completeTestRun testPid (Issues.EventClaim (Issues.SlackCommand routine.conversationId) initial.id initial.startedAt) $ Issues.FailedResult $ Issues.RunFailure "Interrupted worker" Issues.RunOnly
        (Just recovered, Just replay) <- Async.concurrently start start
        replay.id `shouldBe` recovered.id
        recovered.id `shouldSatisfy` (/= initial.id)
        runQueryEffect tr (Hasql.interpScalar @(UUIDId "ai_routine_run") [HI.sql|SELECT root_run_id FROM apis.ai_routine_runs WHERE id = #{recovered.id}|]) >>= (`shouldBe` initial.id)
        runs <- runQueryEffect tr $ Issues.listConversationRuns testPid routine.conversationId 10
        map (.attempt) runs `shouldBe` [2, 1]

      it "eventRoutineCancellation_stopsActiveRunAndRetainsSnapshotsAcrossRetry" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        (routine, run) <- runTestBg frozenTime tr do
          routine <- Issues.getEventRoutine testPid Issues.EventSlackMessage >>= maybe (throwIO $ ErrorCall "Missing routine") pure
          run <- Issues.startEventRun routine (Issues.SlackMessageRequest (UUIDId UUID.nil) (getResponse tr.trSessAndHeader).user.id) Issues.ContinueRun routine.conversationId "Investigate checkout" >>= maybe (throwIO $ ErrorCall "Missing run") pure
          context <- AI.requireRunContext testPid $ Issues.EventClaim (Issues.SlackMessage $ UUIDId UUID.nil) run.id run.startedAt
          Issues.saveRunResult context "Investigation" "Original evidence"
          pure (routine, run)
        void $ testServant tr $ AIThreads.routineCancelPostH testPid routine.conversationId
        active <- runQueryEffect tr $ Issues.routineRunActive testPid run.id
        active `shouldBe` False
        runQueryEffect tr $ completeTestRun testPid (Issues.EventClaim (Issues.SlackMessage $ UUIDId UUID.nil) run.id run.startedAt) $ Issues.CancelledResult $ Just "Cancelled by member"
        void $ testServant tr $ AIThreads.routineInstructionsPostH testPid routine.conversationId (AIThreads.RoutineInstructionsForm "Check the new deployment first.")
        void $ testServant tr $ AIThreads.routineResumePostH testPid routine.conversationId
        retry <- runTestBg frozenTime tr do
          retry <- Issues.startEventRun routine (Issues.SlackMessageRequest (UUIDId UUID.nil) (getResponse tr.trSessAndHeader).user.id) Issues.RestartRun routine.conversationId "Investigate checkout again" >>= maybe (throwIO $ ErrorCall "Missing retry") pure
          completeTestRun testPid (Issues.EventClaim (Issues.SlackMessage $ UUIDId UUID.nil) retry.id retry.startedAt) $ Issues.CompletedResult Issues.ResultSucceeded Issues.PublishAnswer $ Issues.reportAnswer "Revised evidence" Nothing
          pure retry
        (original, revised, oldInstructions, newInstructions, runs, duplicate) <-
          runTestBg frozenTime tr
            $ (,,,,,)
            <$> Issues.listRunArtifacts testPid run.id
            <*> Issues.listRunArtifacts testPid retry.id
            <*> Issues.getRoutineRunEvidence testPid run.id
            <*> Issues.getRoutineRunEvidence testPid retry.id
            <*> Issues.listConversationRuns testPid routine.conversationId 20
            <*> Issues.startEventRun routine (Issues.SlackMessageRequest (UUIDId UUID.nil) (getResponse tr.trSessAndHeader).user.id) Issues.ContinueRun routine.conversationId "Replay"
        map (.content) original `shouldBe` [Just "Original evidence"]
        map (.content) revised `shouldBe` [Just "Revised evidence"]
        (oldInstructions >>= (.instructions)) `shouldBe` Just routine.instructions
        (newInstructions >>= (.instructions)) `shouldBe` Just "Check the new deployment first."
        map (.attempt) runs `shouldBe` [2, 1]
        runQueryEffect tr (Hasql.interpScalar @(UUIDId "ai_routine_run") [HI.sql|SELECT root_run_id FROM apis.ai_routine_runs WHERE id = #{retry.id}|]) >>= (`shouldBe` retry.id)
        fmap (.id) duplicate `shouldBe` Just retry.id
        (_, page) <- testServant tr $ AIThreads.routineRunGetH testPid run.id Nothing
        let html = renderT page
        filter (not . (`T.isInfixOf` html)) ["Original evidence", "Cancelled", "Investigate checkout", "Instructions used for this run"] `shouldBe` []
        (snd $ T.breakOn "id=\"routine-run\"" html) `shouldSatisfy` (not . T.isInfixOf "Revised evidence")
        failed <- runTestBg frozenTime tr do
          failed <- Issues.startEventRun routine (Issues.SlackMessageRequest (UUIDId UUID.nil) (getResponse tr.trSessAndHeader).user.id) Issues.RestartRun routine.conversationId "Try again" >>= maybe (throwIO $ ErrorCall "Missing failed attempt") pure
          completeTestRun testPid (Issues.EventClaim (Issues.SlackMessage $ UUIDId UUID.nil) failed.id failed.startedAt) $ Issues.FailedResult $ Issues.RunFailure "Model unavailable" Issues.RunOnly
          Issues.listRunArtifacts testPid failed.id
        map (.content) failed `shouldBe` []
        other <- createTestProject tr "Other routine project"
        hidden <- runAuthHandler tr $ (AIThreads.routineRunGetH other run.id Nothing $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
        hidden `shouldBe` 404
        pending <-
          runTestBg frozenTime tr
            $ Issues.startEventRun routine (Issues.SlackMessageRequest (UUIDId UUID.nil) (getResponse tr.trSessAndHeader).user.id) Issues.RestartRun routine.conversationId "Work finishing during deletion"
            >>= maybe (throwIO $ ErrorCall "Missing pending attempt") pure
        runQueryEffect tr do
          completeTestRun testPid (Issues.EventClaim (Issues.SlackMessage $ UUIDId UUID.nil) pending.id pending.startedAt) $ Issues.CompletedResult Issues.ResultSucceeded Issues.PublishAnswer $ Issues.reportAnswer "The published result survives conversation deletion." Nothing
          Issues.deleteConversation testPid routine.conversationId
        retainedArtifacts <- runQueryEffect tr $ Issues.listRunArtifacts testPid pending.id
        map (.content) retainedArtifacts `shouldBe` [Just "The published result survives conversation deletion."]
        (_, retained) <- testServant tr $ AIThreads.routineRunGetH testPid run.id Nothing
        let retainedHtml = renderT retained
        retainedHtml `shouldSatisfy` T.isInfixOf "Original evidence"
        retainedHtml `shouldSatisfy` (not . T.isInfixOf "Open conversation and follow up")
        (_, recent) <- testServant tr $ AIThreads.routinesGetH testPid
        (renderT recent) `shouldSatisfy` T.isInfixOf ("/ai/runs/" <> run.id.toText)

      it "eventRoutinePermissions_readersInspectButCannotActivateOrEdit" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        conversations <- runQueryEffect tr $ Issues.listConversations testPid
        conversation <- maybe (fail "Missing routine") pure $ find ((== Just "slack-investigation") . (.templateKey)) conversations
        let uid = (getResponse tr.trSessAndHeader).user.id
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{uid}|]
        (_, page) <- testServant tr $ AIThreads.routinesGetH testPid
        (_, thread) <- testServant tr $ AIThreads.threadGetH testPid conversation.conversationId Nothing Nothing
        let render = renderT
        render page `shouldSatisfy` T.isInfixOf "1 routine"
        render page `shouldSatisfy` (not . T.isInfixOf "Add routine")
        render thread `shouldSatisfy` T.isInfixOf "Routine instructions"
        render thread `shouldSatisfy` (not . T.isInfixOf "Save instructions")
        for_ [AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "production-impact-review"), AIThreads.routineInstructionsPostH testPid conversation.conversationId (AIThreads.RoutineInstructionsForm "Overwrite"), AIThreads.routineResumePostH testPid conversation.conversationId] $ \handler -> do
          result <- runAuthHandler tr $ (handler $> 200) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
          result `shouldBe` 403
        runQueryEffect tr (Issues.getEventRoutine testPid Issues.EventSlackMessage) >>= (`shouldSatisfy` isNothing)

      it "eventRoutineAuthorization_rechecksAccountProjectAndPlanAccess" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        (routine, run, actor) <- runTestBg frozenTime tr do
          routine <- Issues.getEventRoutine testPid Issues.EventSlackMessage >>= maybe (throwIO $ ErrorCall "Missing routine") pure
          actor <- Projects.createUser "Routine" "Reader" "" "routine-reader@example.test"
          Projects.insertUser actor
          Hasql.interpExecute_ [HI.sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (#{testPid}, #{actor.id}, 'view')|]
          run <- Issues.startEventRun routine (Issues.SlackCommandRequest routine.conversationId actor.id) Issues.StartRun routine.conversationId "Inspect checkout" >>= maybe (throwIO $ ErrorCall "Missing run") pure
          pure (routine, run, actor)
        runQueryEffect tr (Issues.routineRunActive testPid run.id) >>= (`shouldBe` True)
        for_ [([HI.sql|UPDATE users.users SET active = FALSE WHERE id = #{routine.requestedBy}|], [HI.sql|UPDATE users.users SET active = TRUE WHERE id = #{routine.requestedBy}|]), ([HI.sql|UPDATE projects.projects SET active = FALSE WHERE id = #{testPid}|], [HI.sql|UPDATE projects.projects SET active = TRUE WHERE id = #{testPid}|])] $ \(disable, restore) -> do
          runQueryEffect tr $ Hasql.interpExecute_ disable
          runQueryEffect tr (Issues.getEventRoutine testPid Issues.EventSlackMessage) >>= (`shouldSatisfy` isNothing)
          runQueryEffect tr (Issues.routineRunActive testPid run.id) >>= (`shouldBe` False)
          tryAny (runTestBg frozenTime tr $ AI.requireAgentAccess (AI.MemberAccess AI.ProjectMember routine.requestedBy) testPid) >>= (`shouldSatisfy` isLeft)
          runQueryEffect tr $ Hasql.interpExecute_ restore
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.projects SET payment_plan = 'Free' WHERE id = #{testPid}|]
        tryAny (runTestBg frozenTime tr $ AI.requireAgentAccess (AI.MemberAccess AI.ProjectMember actor.id) testPid) >>= (`shouldSatisfy` isLeft)
        runQueryEffect tr (Issues.routineRunActive testPid run.id) >>= (`shouldBe` False)

      it "routineResults_everyRunRendersNotJustTheFirst" \tr -> do
        let convId = UUIDId UUID.nil
        void $ runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          Issues.insertChatMessage testPid convId Issues.ChatUser "Watch checkout latency" Nothing Nothing
          for_ ["First run result", "Second run result"] \r -> Issues.insertChatMessage testPid convId Issues.ChatAssistant r Nothing Nothing
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        let html = renderT threadPage
        filter (not . (`T.isInfixOf` html)) ["First run result", "Second run result"] `shouldBe` []

      it "agentDocuments_uncheckpointedWritesCannotMutateConversationState" \tr -> do
        let conversation = UUIDId UUID.nil
            user = (getResponse tr.trSessAndHeader).user.id
        mode <- interactiveInvocation tr testPid conversation "Retain this finding" Issues.Observe
        let base = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember user, AI.conversationId = Just conversation}
            call = LLM.ToolCall "uncheckpointed" "function" $ LLM.ToolFunction "save_artifact" $ Map.fromList [("key", "must-not-exist"), ("content", "This write has no retained operation.")]
        for_ [(base, False), (base{AI.invocationMode = mode}, True)] $ \(config, writable) -> do
          (system, _, params) <- Eff.runEff $ Time.runTime $ AI.agenticSetup config "Retain this finding" "model"
          let tools = AE.toJSON params ^.. key "tools" . values . key "function" . key "name" . _String
          ("save_artifact" `elem` tools) `shouldBe` writable
          T.isInfixOf "save_artifact" system.content `shouldBe` writable
          result <- runTestBg frozenTime tr $ AI.executeToolCall config call
          result.formatted `shouldBe` "This change requires a durable checkpoint."
          runQueryEffect tr (Issues.listArtifacts testPid conversation Nothing) >>= (`shouldSatisfy` null)

      it "agentDocuments_savedAppendedAndListedBesideTheConversation" \tr -> do
        let convId = UUIDId UUID.nil
        mode <- scheduledInvocation tr convId Issues.Observe
        let web = (AI.defaultAgenticConfig testPid){AI.conversationId = Just convId, AI.conversationType = Just Issues.CTWeb}
            routine = web{AI.access = AI.MemberAccess AI.ProjectMember (getResponse tr.trSessAndHeader).user.id, AI.invocationMode = mode, AI.timezone = Just "America/New_York"}
            call cfg name args = AI.executeToolCall cfg (LLM.ToolCall "call" "function" $ LLM.ToolFunction name $ Map.fromList args)
        saved <- saveConversationArtifact tr convId [("key", "Checkout Review"), ("title", "Checkout review"), ("content", "## Summary\nAll good.")]
        appended <- saveConversationArtifact tr convId [("key", "checkout-review"), ("mode", "append"), ("content", "## Follow-up\nLatency recovered.")]
        (listed, readBack, routinePrompt) <- runTestBg frozenTime tr do
          listed <- call web "read_artifact" []
          readBack <- call web "read_artifact" [("key", "checkout-review")]
          Issues.insertChatMessage testPid convId Issues.ChatAssistant "Saved." Nothing $ Just $ AE.toJSON ([ToolCallInfo "save_artifact" mempty "" saved.rawData] :: [ToolCallInfo])
          (system, _, _) <- AI.agenticSetup routine "run" "model"
          pure (listed, readBack, system.content)
        saved.formatted `shouldSatisfy` T.isPrefixOf "Saved document 'checkout-review'"
        saved.rawData `shouldBe` Just "checkout-review"
        appended.formatted `shouldSatisfy` T.isInfixOf "Checkout review"
        listed.formatted `shouldSatisfy` T.isInfixOf "\"key\":\"checkout-review\""
        readBack.formatted `shouldBe` "## Summary\nAll good.\n\n## Follow-up\nLatency recovered."
        -- frozenTime is 2025-01-01T00:00Z, still New Year's Eve in New York.
        routinePrompt `shouldSatisfy` T.isInfixOf "key \"2024-12-31\""
        (_, chatPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        renderT chatPage `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/ai/" <> convId.toText <> "?doc=checkout-review\"")
        (_, docPage) <- testServant tr $ AIThreads.threadGetH testPid convId (rightToMaybe $ Issues.mkArtifactKey "checkout-review") Nothing
        let html = renderT docPage
        filter (not . (`T.isInfixOf` html)) ["?doc=checkout-review", "<h2 id=\"followup\">", "href=\"#followup\"", "Latency recovered."] `shouldBe` []

      it "agentDocuments_boundedPerDocumentPerConversationAndPerRead" \tr -> do
        let convId = UUIDId UUID.nil
            web = (AI.defaultAgenticConfig testPid){AI.conversationId = Just convId, AI.conversationType = Just Issues.CTWeb}
            save args = (.formatted) <$> saveConversationArtifact tr convId args
            big = AE.String $ T.replicate 90_000 "x"
        badMode <- save [("key", "a"), ("content", "y"), ("mode", "merge")]
        void $ save [("key", "log"), ("content", big)]
        void $ save [("key", "log"), ("mode", "append"), ("content", big)]
        longRead <- runTestBg frozenTime tr $ (.formatted) <$> AI.executeToolCall web (LLM.ToolCall "read" "function" $ LLM.ToolFunction "read_artifact" $ one ("key", "log"))
        overflow <- save [("key", "log"), ("mode", "append"), ("content", big)]
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content) SELECT #{testPid}, #{convId}, 'doc-' || n, 'Doc', '' FROM generate_series(1, #{Issues.maxArtifactsPerConversation - 1}) n|]
        overCap <- save [("key", "one-too-many"), ("content", "y")]
        existing <- save [("key", "log"), ("content", "rewritten")]
        badMode `shouldBe` "mode must be replace or append."
        T.length longRead `shouldSatisfy` (< 41_000)
        longRead `shouldSatisfy` T.isInfixOf "140002 of 180002 characters omitted"
        overflow `shouldSatisfy` T.isPrefixOf "Document 'log' is full."
        overCap `shouldSatisfy` T.isInfixOf "already holds 500 documents"
        existing `shouldSatisfy` T.isPrefixOf "Saved document 'log'"

      it "routineResult_keepsRunEvidenceWhenTheConversationDocumentLimitIsReached" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        execution <- runTestBg frozenTime tr do
          routine <- Issues.getEventRoutine testPid Issues.EventSlackMessage >>= maybe (throwIO $ ErrorCall "Missing active template") pure
          run <- Issues.startEventRun routine (Issues.SlackCommandRequest routine.conversationId routine.requestedBy) Issues.StartRun routine.conversationId "Retain this run result" >>= maybe (throwIO $ ErrorCall "Missing run") pure
          void
            $ Hasql.interpExecute
              [HI.sql|INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content)
            SELECT #{testPid}, #{routine.conversationId}, 'doc-' || n, 'Document', 'Evidence' FROM generate_series(1, #{Issues.maxArtifactsPerConversation}) n|]
          completeTestRun testPid (Issues.EventClaim (Issues.SlackCommand routine.conversationId) run.id run.startedAt) $ Issues.CompletedResult Issues.ResultSucceeded Issues.PublishAnswer $ Issues.reportAnswer "The run keeps its evidence even when the conversation is full." Nothing
          pure run
        count <- runQueryEffect tr $ Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM apis.ai_artifacts WHERE project_id = #{testPid}|]
        count `shouldBe` fromIntegral Issues.maxArtifactsPerConversation
        artifacts <- runQueryEffect tr $ Issues.listRunArtifacts testPid execution.id
        mapMaybe (.content) artifacts `shouldSatisfy` elem "The run keeps its evidence even when the conversation is full."

      for_ (["cancel", "pause", "delete"] :: [Text]) $ \stop ->
        it ("nativeCompletion_explicitStopCannotBecomeARetryableFailure: " <> toString stop) \tr -> do
          void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
          routine <- runQueryEffect tr (Issues.getEventRoutine testPid Issues.EventSlackMessage) >>= maybe (fail "Expected an installed routine") pure
          let source = Issues.SlackCommand routine.conversationId
          Just execution <- runTestBg frozenTime tr $ Issues.startEventRun routine (Issues.SlackCommandRequest routine.conversationId routine.requestedBy) Issues.StartRun routine.conversationId "Retain this result"
          context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.EventClaim source execution.id execution.startedAt
          issueId <- runTestBg frozenTime tr $ Issues.createQueryAlertIssue testPid "cancelled-native" "Original title" "service == checkout" 100 200 Issues.Above >>= Issues.insertIssueReturningId
          runQueryEffect tr $ case stop of
            "cancel" -> Issues.cancelRoutineRun testPid routine.conversationId
            "pause" -> Issues.pauseRoutine testPid routine.conversationId
            _ -> Issues.deleteConversation testPid routine.conversationId
          void $ runTestBg frozenTime tr $ Issues.completeRunWithProjectMutation (Issues.updateIssueEnhancement frozenTime issueId "Must not change" "" "") context $ Issues.CompletedResult Issues.ResultSucceeded Issues.PublishAnswer $ Issues.reportAnswer "Stopped work" Nothing
          void $ runTestBg frozenTime tr $ Issues.completeRun context $ Issues.FailedResult $ Issues.RunFailure "Delivery interrupted" Issues.RunOnly
          runQueryEffect tr (Issues.getRoutineRun testPid execution.id) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunCancelled)
          runQueryEffect tr (Issues.listRunArtifacts testPid execution.id) >>= (`shouldSatisfy` null)
          runQueryEffect tr (Issues.selectIssueById testPid issueId) >>= (\issue -> fmap (.title) issue `shouldBe` Just "Original title")

      for_ ["completion", "receipt"] \boundary ->
        it ("nativeCompletion_enrichmentRollsBackMetadataReportAndOperation: " <> toString boundary) \tr -> do
          calls <- IORef.newIORef (0 :: Int)
          issueId <- runTestBg frozenTime tr do
            issue <- Issues.createQueryAlertIssue testPid "atomic-native" "Original title" "service == checkout" 100 200 Issues.Above
            Issues.insertIssueReturningId issue
          let savedIssue = runQueryEffect tr $ Hasql.interpOneJson @AE.Value [HI.sql|SELECT to_jsonb(issue) FROM apis.issues issue WHERE project_id = #{testPid} AND id = #{issueId}|]
          original <- savedIssue
          void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment")
          Just routine <- runQueryEffect tr $ Issues.getEventRoutine testPid Issues.EventIssueEnrichment
          runQueryEffect tr $ Issues.setRoutinePolicy (getResponse tr.trSessAndHeader).user.id testPid routine.conversationId Issues.Autonomous
          let model :: ATBackgroundCtx a -> ATBackgroundCtx a
              model = interpose @ELLM.LLM \_ -> \case
                ELLM.CallLLM _ prompt _ -> do
                  liftIO $ IORef.modifyIORef' calls (+ 1)
                  pure
                    $ Right
                    $ if
                      | "issue titler" `T.isInfixOf` prompt -> "Enriched title"
                      | "issue describer" `T.isInfixOf` prompt -> "Observed checkout latency.\nCompare the traces.\nlow"
                      | otherwise -> "critical\n0\n0"
                ELLM.CallAgenticChat{} -> pure $ Left "Unexpected agentic request"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
              execute = runTestBg frozenTime tr $ model $ Reader.local (\ctx -> ctx{config = ctx.config{openaiApiKey = "routine-test-key"}}) $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.EnhanceIssuesWithLLM testPid $ V.singleton issueId)
              ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
          bracket_
            (ddl $ if boundary == "completion" then "ALTER TABLE apis.ai_routine_runs ADD CONSTRAINT reject_native_completion CHECK (status <> 'succeeded') NOT VALID" else "ALTER TABLE apis.ai_agent_operations ADD CONSTRAINT reject_native_receipt CHECK (outcome->>'tag' <> 'OperationReturned') NOT VALID")
            (ddl $ if boundary == "completion" then "ALTER TABLE apis.ai_routine_runs DROP CONSTRAINT reject_native_completion" else "ALTER TABLE apis.ai_agent_operations DROP CONSTRAINT reject_native_receipt")
            (tryAny execute >>= (`shouldSatisfy` isLeft))
          Just interrupted <- runQueryEffect tr $ Issues.getEventRun testPid $ Issues.IssueEnrichment issueId
          interrupted.status `shouldBe` Issues.RunRunning
          runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid interrupted.id) >>= (`shouldSatisfy` null)
          runQueryEffect tr (Issues.listRunArtifacts testPid interrupted.id) >>= (`shouldSatisfy` null)
          history <- runQueryEffect tr $ Issues.selectChatHistory testPid interrupted.conversationId
          filter ((== Issues.ChatAssistant) . (.role)) history `shouldSatisfy` null
          savedIssue >>= (`shouldBe` original)
          when (boundary == "completion") do
            other <- runTestBg frozenTime tr $ Issues.createQueryAlertIssue testPid "other-native" "Other title" "service == other" 100 200 Issues.Above >>= Issues.insertIssueReturningId
            context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.EventClaim (Issues.IssueEnrichment issueId) interrupted.id interrupted.startedAt
            let target = Investigations.RunCheckpoint $ Investigations.RunTurn testPid context.conversationId context.requestedBy context.rootId
            Just saved <- runQueryEffect tr $ Investigations.loadCheckpointFor target
            case saved.checkpoint of
              Investigations.NativePending (Operation.EnrichIssue retained) -> do
                let foreignIntent = retained{Enhancement.enhancement = retained.enhancement{Enhancement.issueId = other}}
                runTestBg frozenTime tr (AI.executeNativeOperation context (Operation.EnrichIssue foreignIntent) $ \_ -> AI.EffectfulOperation $ liftIO $ fail "Another issue's enrichment was dispatched")
                  `shouldThrow` (\case Investigations.OperationScopeMismatch -> True; _ -> False)
              _ -> expectationFailure "Expected the retained typed enrichment intent"
          execute
          Just completed <- runQueryEffect tr $ Issues.getEventRun testPid $ Issues.IssueEnrichment issueId
          completed.id `shouldBe` interrupted.id
          completed.status `shouldBe` Issues.RunSucceeded
          runQueryEffect tr (Issues.selectIssueById testPid issueId) >>= (\issue -> fmap (.title) issue `shouldBe` Just "Enriched title")
          runQueryEffect tr (Issues.listRunArtifacts testPid completed.id) >>= (\artifacts -> length artifacts `shouldBe` 1)
          after <- runQueryEffect tr $ Issues.selectChatHistory testPid completed.conversationId
          length (filter ((== Issues.ChatAssistant) . (.role)) after) `shouldBe` 1
          operations <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid completed.id
          [(entry.runId, entry.requestedBy) | entry <- operations] `shouldBe` [(completed.id, (getResponse tr.trSessAndHeader).user.id)]
          [AE.toJSON entry.proposal.call ^? key "contents" . key "tag" . _String | entry <- operations] `shouldBe` [Just "EnrichIssue"]
          [AE.toJSON entry.outcome ^? key "tag" . _String | entry <- operations] `shouldBe` [Just "OperationReturned"]
          IORef.readIORef calls >>= (`shouldBe` 3)

      for_ [Issues.Observe, Issues.Autonomous] $ \policy ->
        it ("nativePolicy_issueEnrichmentRetainsAnalysisAndHonorsMutationPolicy: " <> show policy) \tr -> do
          issueId <- runTestBg frozenTime tr $ Issues.createQueryAlertIssue testPid "native-policy" "Original title" "service == checkout" 100 200 Issues.Above >>= Issues.insertIssueReturningId
          void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment")
          Just routine <- runQueryEffect tr $ Issues.getEventRoutine testPid Issues.EventIssueEnrichment
          runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy testPid routine.conversationId policy
          let model :: ATBackgroundCtx a -> ATBackgroundCtx a
              model = interpose @ELLM.LLM \_ -> \case
                ELLM.CallLLM _ prompt _ ->
                  pure
                    $ Right
                    $ if
                      | "issue titler" `T.isInfixOf` prompt -> "Enriched title"
                      | "issue describer" `T.isInfixOf` prompt -> "Observed latency.\nCompare traces.\nlow"
                      | otherwise -> "critical\n0\n0"
                ELLM.CallAgenticChat{} -> pure $ Left "Unexpected agentic request"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
          runTestBg frozenTime tr $ model $ Reader.local (\ctx -> ctx{config = ctx.config{openaiApiKey = "routine-test-key"}}) $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.EnhanceIssuesWithLLM testPid $ V.singleton issueId)
          runQueryEffect tr (Issues.selectIssueById testPid issueId) >>= (\issue -> fmap (.title) issue `shouldBe` Just (if policy == Issues.Autonomous then "Enriched title" else "Original title"))
          Just execution <- runQueryEffect tr $ Issues.getEventRun testPid $ Issues.IssueEnrichment issueId
          execution.status `shouldBe` Issues.RunSucceeded
          runQueryEffect tr (Investigations.listOperations @AI.OperationPolicy testPid execution.id) >>= (\entries -> length entries `shouldBe` if policy == Issues.Autonomous then 1 else 0)
          runQueryEffect tr (Issues.listRunArtifacts testPid execution.id) >>= (\artifacts -> T.intercalate " " (mapMaybe (.content) artifacts) `shouldSatisfy` T.isInfixOf "Compare traces.")

      for_ (["policy-withdrawn", "original-role-limit", "restricted-evidence", "private-audience"] :: [Text]) $ \restriction ->
        it ("nativePolicy_completionRechecksMutationAuthority: " <> toString restriction) \tr -> do
          issueId <- runTestBg frozenTime tr $ Issues.createQueryAlertIssue testPid "native-policy-race" "Original title" "service == checkout" 100 200 Issues.Above >>= Issues.insertIssueReturningId
          void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment")
          Just routine <- runQueryEffect tr $ Issues.getEventRoutine testPid Issues.EventIssueEnrichment
          let conversation = UUIDId issueId.unUUIDId
              memberRole permission = runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = #{permission} WHERE project_id = #{testPid} AND user_id = #{routine.requestedBy}|]
          void $ toBaseServantResponse tr $ Issues.getOrCreateConversation testPid conversation Issues.CTAnomaly (AE.object [])
          when (restriction == "original-role-limit") $ memberRole ProjectMembers.PView
          Just execution <- runTestBg frozenTime tr $ Issues.startEventRun routine (Issues.IssueEnrichmentRequest issueId) Issues.StartRun conversation "Retain the native request"
          memberRole ProjectMembers.PAdmin
          context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.EventClaim (Issues.IssueEnrichment issueId) execution.id execution.startedAt
          when (restriction == "policy-withdrawn") $ runQueryEffect tr $ Issues.setRoutinePolicy routine.requestedBy testPid routine.conversationId Issues.Observe
          when (restriction == "restricted-evidence") $ runQueryEffect tr $ Investigations.retainEvidencePermission testPid [conversation] ProjectMembers.PAdmin
          when (restriction == "private-audience") $ runQueryEffect tr (Investigations.retainEvidenceAudience testPid conversation $ Investigations.PersonalAudience routine.requestedBy) >>= (`shouldBe` True)
          void $ runTestBg frozenTime tr $ Issues.completeRunWithProjectMutation (Issues.updateIssueEnhancement frozenTime issueId "Must not change" "" "") context $ Issues.CompletedResult Issues.ResultSucceeded Issues.PublishAnswer $ Issues.reportAnswer "Must not publish" Nothing
          runQueryEffect tr (Issues.selectIssueById testPid issueId) >>= (\issue -> fmap (.title) issue `shouldBe` Just "Original title")
          runQueryEffect tr (Issues.getRoutineRun testPid execution.id) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunCancelled)
          runQueryEffect tr (Issues.listRunArtifacts testPid execution.id) >>= (`shouldSatisfy` null)
          runQueryEffect tr (Issues.selectChatHistory testPid conversation) >>= (\messages -> filter ((== Issues.ChatAssistant) . (.role)) messages `shouldSatisfy` null)

      it "issueEnrichment_requiresActivationAndRetainsAnEditableRoutineRun" \tr -> do
        issueId <- runTestBg frozenTime tr do
          issue <- Issues.createQueryAlertIssue testPid "checkout-alert" "Checkout latency" "service == checkout" 100 200 Issues.Above
          Issues.insertIssueReturningId issue
        let configured = Reader.local (\ctx -> ctx{config = ctx.config{openaiApiKey = "routine-test-key"}})
            job = BackgroundJobs.EnhanceIssuesWithLLM testPid (V.singleton issueId)
            execute = configured $ BackgroundJobs.processBackgroundJob tr.trATCtx job
            model :: Bool -> ATBackgroundCtx a -> ATBackgroundCtx a
            model activated = interpose @ELLM.LLM \_ -> \case
              ELLM.CallLLM _ prompt _ -> do
                liftIO $ activated `shouldBe` True
                liftIO $ prompt `shouldSatisfy` T.isInfixOf "Prefer checkout evidence."
                pure
                  $ Right
                  $ if
                    | "issue titler" `T.isInfixOf` prompt -> "Checkout latency exceeds its threshold"
                    | "issue describer" `T.isInfixOf` prompt -> "Checkout latency is twice the threshold.\nCompare the checkout traces.\nlow"
                    | otherwise -> "critical\n0\n0"
              ELLM.CallAgenticChat history params token -> ELLM.callAgenticChat history params token
              ELLM.EmbedDocuments config docs -> ELLM.embedDocuments config docs
        -- Unactivated automation must not spend tokens or change issue metadata.
        runTestBg frozenTime tr $ model False execute
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment")
        conversations <- runQueryEffect tr $ Issues.listConversations testPid
        routine <- maybe (fail "Missing issue enrichment routine") pure $ find ((== Just "issue-enrichment") . (.templateKey)) conversations
        void $ testServant tr $ AIThreads.routineInstructionsPostH testPid routine.conversationId (AIThreads.RoutineInstructionsForm "Prefer checkout evidence.")
        runTestBg frozenTime tr $ model True $ concurrently_ execute execute >> execute
        runs <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
        map (.status) runs `shouldBe` [Issues.RunSucceeded]
        usage <- runQueryEffect tr $ Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM projects.ai_usage WHERE project_id = #{testPid} AND source = 'routine'|]
        usage `shouldBe` 3
        run <- maybe (fail "Missing visible issue enrichment run") pure $ listToMaybe runs
        evidence <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid run.id
        (evidence >>= (.instructions)) `shouldBe` Just "Prefer checkout evidence."
        artifacts <- runQueryEffect tr $ Issues.listRunArtifacts testPid run.id
        T.intercalate " " (mapMaybe (.content) artifacts) `shouldSatisfy` T.isInfixOf "Compare the checkout traces."
        updated <- runQueryEffect tr $ Issues.selectIssueById testPid issueId
        fmap (.title) updated `shouldBe` Just "Checkout latency exceeds its threshold"
        fmap (.recommendedAction) updated `shouldBe` Just "Compare the checkout traces."
        retryIssue <- runTestBg frozenTime tr do
          issue <- Issues.createQueryAlertIssue testPid "retry-alert" "Retry latency" "service == checkout" 100 200 Issues.Above
          Issues.insertIssueReturningId issue
        let retryJob = configured $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.EnhanceIssuesWithLLM testPid $ V.singleton retryIssue)
            failing :: ATBackgroundCtx a -> ATBackgroundCtx a
            failing = interpose @ELLM.LLM \_ -> \case
              ELLM.CallLLM{} -> pure $ Left "Provider unavailable"
              ELLM.CallAgenticChat history params token -> ELLM.callAgenticChat history params token
              ELLM.EmbedDocuments config docs -> ELLM.embedDocuments config docs
        failed <- tryAny $ runTestBg frozenTime tr $ failing retryJob
        failed `shouldSatisfy` isLeft
        attempts <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
        map (.status) attempts `shouldSatisfy` elem Issues.RunFailed
        for_ ([("issue describer", "Truncated description"), ("issue-severity", "ambiguous\n-1\n0")] :: [(Text, Text)]) $ \(stage, invalid) -> do
          let malformed :: ATBackgroundCtx a -> ATBackgroundCtx a
              malformed = interpose @ELLM.LLM \_ -> \case
                ELLM.CallLLM _ prompt _ -> pure $ Right $ if stage `T.isInfixOf` prompt then invalid else if "issue describer" `T.isInfixOf` prompt then "Observed latency.\nCompare traces.\nlow" else "critical\n0\n0"
                ELLM.CallAgenticChat history params token -> ELLM.callAgenticChat history params token
                ELLM.EmbedDocuments config docs -> ELLM.embedDocuments config docs
          invalidResult <- tryAny $ runTestBg frozenTime tr $ malformed retryJob
          invalidResult `shouldSatisfy` isLeft
        runTestBg frozenTime tr $ model True retryJob
        retried <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
        sort (map (.attempt) retried) `shouldBe` [1, 1, 2, 3, 4]
        -- The source lease survives a worker interruption; its next delivery resumes
        -- the existing run instead of starting a concurrent or duplicate attempt.
        interrupted <- runTestBg frozenTime tr do
          installed <- Issues.getEventRoutine testPid Issues.EventIssueEnrichment >>= maybe (throwIO $ ErrorCall "Missing active template") pure
          issue <- Issues.createQueryAlertIssue testPid "interrupted-alert" "Interrupted latency" "service == checkout" 100 200 Issues.Above
          iid <- Issues.insertIssueReturningId issue
          let convId = UUIDId iid.unUUIDId
          void $ Issues.getOrCreateConversation testPid convId Issues.CTAnomaly (AE.object [])
          void $ Issues.startEventRun installed (Issues.IssueEnrichmentRequest iid) Issues.StartRun convId "Interrupted worker"
          pure iid
        runTestBg frozenTime tr $ model True $ configured $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.EnhanceIssuesWithLLM testPid $ V.singleton interrupted)
        recovered <- runQueryEffect tr $ Issues.getEventRun testPid (Issues.IssueEnrichment interrupted)
        fmap (.status) recovered `shouldBe` Just Issues.RunSucceeded
        fmap (.attempt) recovered `shouldBe` Just 1
        -- Saved instructions affect future work, never the existing artifact snapshot.
        void $ testServant tr $ AIThreads.routineInstructionsPostH testPid routine.conversationId (AIThreads.RoutineInstructionsForm "Use the current error budget.")
        stoppedIssue <- runTestBg frozenTime tr do
          issue <- Issues.createQueryAlertIssue testPid "stopped-alert" "Keep original title" "service == checkout" 100 200 Issues.Above
          Issues.insertIssueReturningId issue
        let stoppedJob = configured $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.EnhanceIssuesWithLLM testPid $ V.singleton stoppedIssue)
            stopDuringModel :: ATBackgroundCtx a -> ATBackgroundCtx a
            stopDuringModel = interpose @ELLM.LLM \_ -> \case
              ELLM.CallLLM{} -> Issues.pauseRoutine testPid routine.conversationId $> Right "Unauthorized new title"
              ELLM.CallAgenticChat history params token -> ELLM.callAgenticChat history params token
              ELLM.EmbedDocuments config docs -> ELLM.embedDocuments config docs
        runTestBg frozenTime tr $ stopDuringModel stoppedJob
        cancelled <- runQueryEffect tr $ Issues.getEventRun testPid (Issues.IssueEnrichment stoppedIssue)
        fmap (.status) cancelled `shouldBe` Just Issues.RunCancelled
        original <- runQueryEffect tr $ Issues.selectIssueById testPid stoppedIssue
        fmap (.title) original `shouldBe` Just "Keep original title"
        runTestBg frozenTime tr $ model False stoppedJob
        prior <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid run.id
        (prior >>= (.instructions)) `shouldBe` Just "Prefer checkout evidence."

      it "issueFollowup_readsTheRetainedInvestigationArtifactAsTheSignedInMember" \tr -> do
        let convId = UUIDId UUID.nil
            artifact = fromRightShow $ Issues.mkArtifactKey "issue-evidence"
            config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember (getResponse tr.trSessAndHeader).user.id, AI.conversationId = Just convId, AI.conversationType = Just Issues.CTAnomaly}
        result <- runTestBg frozenTime tr do
          void $ Issues.getOrCreateConversation testPid convId Issues.CTAnomaly (AE.object [])
          void $ Issues.saveArtifact testPid convId artifact (Just "Investigation evidence") "Checkout recovered after the deployment." Issues.ArtifactReplace
          AI.executeToolCall config (LLM.ToolCall "read-evidence" "function" $ LLM.ToolFunction "read_artifact" $ one ("key", AE.String "issue-evidence"))
        result.formatted `shouldBe` "Checkout recovered after the deployment."

      it "lists titled threads and schedules and pauses their routine" \tr -> do
        let convId = UUIDId UUID.nil
        (initialTitle, summary, listed, scheduled, paused, resumed, deleted) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          initialTitle <- Issues.getConversationTitle testPid convId
          Issues.insertChatMessage testPid convId Issues.ChatUser "Send a daily reliability brief to Slack" Nothing Nothing
          Issues.setConversationTitle testPid convId "Daily reliability brief"
          summary <- Issues.getConversation testPid convId
          listed <- Issues.listConversations testPid
          scheduled <- Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId $ fromRightShow $ Issues.mkRoutineInterval 15
          Issues.pauseRoutine testPid convId
          paused <- Issues.getConversation testPid convId
          resumed <- Issues.resumeRoutine (getResponse tr.trSessAndHeader).user.id testPid convId
          Issues.deleteRoutine testPid convId
          deleted <- Issues.getConversation testPid convId
          pure (initialTitle, summary, listed, scheduled, paused, resumed, deleted)
        initialTitle `shouldBe` Nothing
        (.title) <$> summary `shouldBe` Just "Daily reliability brief"
        map (.conversationId) listed `shouldContain` [convId]
        (.scheduledAt) <$> scheduled `shouldBe` Just (addUTCTime 900 frozenTime)
        ((.routineActive) <$> paused, (.routineNextRunAt) =<< paused) `shouldBe` (Just False, Nothing)
        (.scheduledAt) <$> resumed `shouldBe` Just (addUTCTime 900 frozenTime)
        ((.routineInterval) =<< deleted) `shouldBe` Nothing

      it "weeklyRoutine_dstTransitions_keepTheNextRunAtItsLocalTime" \tr -> do
        template <- maybe (fail "Missing weekly template") pure $ find ((== "weekly-reliability-review") . (.key)) Issues.routineTemplates
        let clock day hour minute = UTCTime day (fromIntegral $ hour * 3600 + minute * 60)
            uid = (getResponse tr.trSessAndHeader).user.id
            convId = UUIDId UUID.nil
        for_ ([(clock (fromGregorian 2026 10 26) 13 0, clock (fromGregorian 2026 11 2) 14 0, 1, 9, 0), (clock (fromGregorian 2026 3 1) 7 30, clock (fromGregorian 2026 3 15) 6 30, 7, 2, 30)] :: [(UTCTime, UTCTime, Int, Int, Int)]) $ \(now, expected, weekday, hour, minute) -> do
          scheduled <- runTestBg now tr do
            void $ Issues.getOrCreateConversation testPid convId Issues.CTWeb (AE.object [])
            void $ Issues.installRoutineTemplate uid testPid convId "America/New_York" template
            Hasql.interpExecute_
              [HI.sql|UPDATE apis.ai_routines SET schedule_weekday = #{weekday}, schedule_hour = #{hour}, schedule_minute = #{minute}
              WHERE project_id = #{testPid} AND conversation_id = #{convId}|]
            Issues.resumeRoutine uid testPid convId
          fmap (.scheduledAt) scheduled `shouldBe` Just expected

      it "runContext_recoveredLeaseStopsTheOldWorkerBeforeMutation" \tr -> do
        let conversation = UUIDId UUID.nil
            user = (getResponse tr.trSessAndHeader).user.id
        Just slot <- runTestBg frozenTime tr do
          void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
          Issues.insertChatMessage testPid conversation Issues.ChatUser "Create a dashboard" Nothing Nothing
          Issues.upsertRoutine user testPid conversation $ fromRightShow $ Issues.mkRoutineInterval 15
        Just original <- runQueryEffect tr $ Issues.claimRoutine slot.id slot.scheduledAt
        originalMode <- runTestBg frozenTime tr $ fmap AI.DurableRun $ AI.requireRunContext testPid $ Issues.BackgroundClaim user conversation original.runId original.claimedAt
        let config mode = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember user, AI.conversationId = Just conversation, AI.invocationMode = mode, AI.projectTools = Just Routes.mcpProjectTools}
            provider :: ATBackgroundCtx a -> ATBackgroundCtx a
            provider = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
                [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "create" "function" $ LLM.ToolFunction "create_dashboard" $ one ("body", AE.object ["title" AE..= ("Lease-fenced dashboard" :: Text)])]}
                _ -> LLM.Message LLM.Assistant "Created." LLM.defaultMessageData
              ELLM.CallLLM{} -> pure $ Left "Unexpected title request"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
            execute mode = runTestBg frozenTime tr $ provider $ tryAny $ AI.runAgenticChatWithHistory (config mode) "Create a dashboard" "model" "key"
            count = runQueryEffect tr $ Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM projects.dashboards WHERE project_id = #{testPid} AND title = 'Lease-fenced dashboard'|]
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET running_since = now() - interval '31 minutes' WHERE id = #{slot.id}|]
        Just recovered <- runQueryEffect tr $ Issues.claimRoutine slot.id slot.scheduledAt
        recovered.claimedAt `shouldSatisfy` (/= original.claimedAt)
        for_ [Issues.InteractiveClaim user conversation original.runId, Issues.BackgroundClaim user conversation original.runId original.claimedAt, Issues.EventClaim (Issues.SlackCommand conversation) original.runId original.claimedAt] $ \claim ->
          runQueryEffect tr (Issues.runContextFor testPid claim) >>= (`shouldBe` Nothing)
        stale <- execute originalMode
        count >>= (`shouldBe` 0)
        stale `shouldSatisfy` isLeft
        recoveredMode <- runTestBg frozenTime tr $ fmap AI.DurableRun $ AI.requireRunContext testPid $ Issues.BackgroundClaim user conversation recovered.runId recovered.claimedAt
        direct <- runTestBg frozenTime tr $ AI.executeToolCall (config recoveredMode) $ LLM.ToolCall "uncheckpointed" "function" $ LLM.ToolFunction "create_dashboard" $ one ("body", AE.object ["title" AE..= ("Lease-fenced dashboard" :: Text)])
        direct.formatted `shouldBe` "This change requires a durable checkpoint."
        count >>= (`shouldBe` 0)
        execute recoveredMode >>= (`shouldSatisfy` either (const False) isRight)
        count >>= (`shouldBe` 1)

      it "routineAuthority_newActivationIsAutonomousAndDestinationDoesNotGrantOrRevokeWrites" \tr -> do
        let conversation = UUIDId UUID.nil
            user = (getResponse tr.trSessAndHeader).user.id
        template <- maybe (fail "Expected a built-in routine") pure $ find ((== "daily-operations-briefing") . (.key)) Issues.routineTemplates
        scheduled <- runTestBg frozenTime tr do
          void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
          Issues.installRoutineTemplate user testPid conversation "UTC" template
        slot <- maybe (fail "Expected an installed schedule") pure scheduled
        run <- runQueryEffect tr (Issues.claimRoutine slot.id slot.scheduledAt) >>= maybe (fail "Expected a claimed run") pure
        run.policy `shouldBe` Issues.Autonomous
        mode <- runTestBg frozenTime tr $ fmap AI.DurableRun $ AI.requireRunContext testPid $ Issues.BackgroundClaim user conversation run.runId run.claimedAt
        let config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess AI.ProjectMember user, AI.conversationId = Just conversation, AI.invocationMode = mode, AI.projectTools = Just Routes.mcpProjectTools}
            provider :: ATBackgroundCtx a -> ATBackgroundCtx a
            provider = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat history _ _ -> pure $ Right $ case [message.content | message <- toList history, message.role == LLM.Tool] of
                [] -> LLM.Message LLM.Assistant "" LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall "create" "function" $ LLM.ToolFunction "create_dashboard" $ one ("body", AE.object ["title" AE..= ("Autonomous activation" :: Text)])]}
                _ -> LLM.Message LLM.Assistant "Created the requested dashboard." LLM.defaultMessageData
              ELLM.CallLLM{} -> pure $ Left "Unexpected title request"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
        runTestBg frozenTime tr (provider $ AI.runAgenticChatWithHistory config "Create a dashboard" "model" "key") >>= (`shouldSatisfy` isRight)
        runQueryEffect tr (Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM projects.dashboards WHERE project_id = #{testPid} AND title = 'Autonomous activation'|]) >>= (`shouldBe` 1)
        for_ [Issues.DestinationSlack, Issues.DestinationConversation] \destination -> do
          runQueryEffect tr $ Issues.setRoutineDestination testPid conversation destination
          runQueryEffect tr (Issues.routineRunPolicyFor testPid run.runId user conversation) >>= (`shouldBe` Just Issues.Autonomous)
        for_ [Issues.Observe, Issues.Autonomous] \policy -> do
          void $ testServant tr $ AIThreads.routinePolicyPostH testPid conversation $ AIThreads.RoutinePolicyForm policy
          runQueryEffect tr (Issues.routineRunPolicyFor testPid run.runId user conversation) >>= (`shouldBe` Just Issues.Observe)
        for_ [void $ Issues.upsertRoutine user testPid conversation $ fromRightShow $ Issues.mkRoutineInterval 15, void $ Issues.resumeRoutine user testPid conversation] \reschedule -> do
          runTestBg frozenTime tr reschedule
          runQueryEffect tr (Issues.routineRunPolicyFor testPid run.runId user conversation) >>= (`shouldBe` Just Issues.Observe)
        context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.BackgroundClaim user conversation run.runId run.claimedAt
        runTestBg slot.scheduledAt tr (Issues.completeRun context $ Issues.CompletedResult Issues.ResultNoFindings Issues.RetainAnswer $ AI.prepareRunAnswer $ AI.AgenticChatResult "No findings" []) >>= \case
          Issues.RunCompleted Issues.CompletedNoFindings (Just next) -> do
            future <- runQueryEffect tr (Issues.claimRoutine next.id next.scheduledAt) >>= maybe (fail "Expected a future run") pure
            future.policy `shouldBe` Issues.Autonomous
            runQueryEffect tr (Issues.routineRunPolicyFor testPid future.runId user conversation) >>= (`shouldBe` Just Issues.Autonomous)
          _ -> expectationFailure "Expected a completed run and the next schedule"
        (_, page) <- testServant tr $ AIThreads.threadGetH testPid conversation Nothing Nothing
        renderT page `shouldSatisfy` T.isInfixOf "Execution policy"
        renderT page `shouldSatisfy` T.isInfixOf "Greater authority applies to future runs."
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
        status <- runAuthHandler tr $ (AIThreads.routinePolicyPostH testPid conversation (AIThreads.RoutinePolicyForm Issues.Observe) $> 200) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
        status `shouldBe` 403

      it "installs a built-in routine on project time and applies greater authority only to future runs" \tr -> do
        length Issues.routineTemplates `shouldSatisfy` (>= 15)
        case find ((== "daily-operations-briefing") . (.key)) Issues.routineTemplates of
          Nothing -> expectationFailure "routine catalog must not be empty"
          Just template -> do
            let convId = UUIDId UUID.nil
            (scheduled, installed, canAct, activeRunCanAct, firstRun) <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              scheduled <- Issues.installRoutineTemplate (getResponse tr.trSessAndHeader).user.id testPid convId "Europe/Berlin" template
              Issues.setRoutinePolicy (getResponse tr.trSessAndHeader).user.id testPid convId Issues.Observe
              execution <- traverse (\routine -> Issues.claimRoutine routine.id routine.scheduledAt) scheduled
              let canAct = maybe (pure Nothing) (\run -> Issues.routineRunPolicyFor testPid run.runId (getResponse tr.trSessAndHeader).user.id convId) $ join execution
              initialCanAct <- canAct
              Issues.setRoutineDestination testPid convId Issues.DestinationSlack
              Issues.setRoutinePolicy (getResponse tr.trSessAndHeader).user.id testPid convId Issues.Autonomous
              activeRunCanAct <- canAct
              installed <- Issues.getConversation testPid convId
              pure (scheduled, installed, initialCanAct, activeRunCanAct, join execution)
            (.scheduledAt) <$> scheduled `shouldBe` Just (addUTCTime (8 * 3600) frozenTime)
            ((.templateKey) =<< installed) `shouldBe` Just template.key
            canAct `shouldBe` Just Issues.Observe
            ((.routineDestination) =<< installed) `shouldBe` Just Issues.DestinationSlack
            activeRunCanAct `shouldBe` Just Issues.Observe
            (slot, lease) <- maybe (fail "Expected the initial scheduled run") pure $ (,) <$> scheduled <*> firstRun
            context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.BackgroundClaim (getResponse tr.trSessAndHeader).user.id convId lease.runId lease.claimedAt
            next <-
              runTestBg slot.scheduledAt tr (Issues.completeRun context $ Issues.CompletedResult Issues.ResultNoFindings Issues.RetainAnswer $ AI.prepareRunAnswer $ AI.AgenticChatResult "No findings" []) >>= \case
                Issues.RunCompleted Issues.CompletedNoFindings (Just value) -> pure value
                _ -> fail "Expected the next scheduled instant"
            future <- runQueryEffect tr (Issues.claimRoutine next.id next.scheduledAt) >>= maybe (fail "Expected a future run") pure
            future.policy `shouldBe` Issues.Autonomous
            runQueryEffect tr (Issues.routineRunPolicyFor testPid future.runId (getResponse tr.trSessAndHeader).user.id convId) >>= (`shouldBe` Just Issues.Autonomous)

      it "rescheduling a template as an interval keeps that schedule after resume" \tr -> do
        case find ((== "daily-operations-briefing") . (.key)) Issues.routineTemplates of
          Nothing -> expectationFailure "routine catalog must not be empty"
          Just template -> do
            let convId = UUIDId UUID.nil
                interval = fromRightShow $ Issues.mkRoutineInterval 15
            resumed <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              void $ Issues.installRoutineTemplate (getResponse tr.trSessAndHeader).user.id testPid convId "Europe/Berlin" template
              void $ Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId interval
              Issues.pauseRoutine testPid convId
              Issues.resumeRoutine (getResponse tr.trSessAndHeader).user.id testPid convId
            (.scheduledAt) <$> resumed `shouldBe` Just (addUTCTime 900 frozenTime)

      it "scheduledRoutine_carriesAuthorizingMember_andRunsOnce" \tr -> do
        executions <- IORef.newIORef ([] :: [AI.InvocationMode])
        successors <- IORef.newIORef ([] :: [(UUIDId "ai_routine", UTCTime)])
        let convId = UUIDId UUID.nil
            interval = fromRightShow $ Issues.mkRoutineInterval 15
            execute config = do
              Eff.liftIO $ show config.access `shouldBe` show (AI.MemberAccess AI.ProjectMember (getResponse tr.trSessAndHeader).user.id)
              Eff.liftIO $ IORef.modifyIORef' executions (<> [config.invocationMode])
              pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Routine completed\",\"widgets\":[]}", AI.toolCalls = []}
            enqueueNext routineId at = Eff.liftIO $ IORef.modifyIORef' successors (<> [(routineId, at)])
        (history, routine, runRecord, recentRuns) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          Issues.insertChatMessage testPid convId Issues.ChatUser "Send a daily reliability brief to Slack" Nothing Nothing
          scheduled <- Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
          BackgroundJobs.runAIRoutineWith 1_000_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          -- A duplicate delivery for the completed scheduled instant must be a no-op.
          BackgroundJobs.runAIRoutineWith 1_000_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          (,,,)
            <$> Issues.selectChatHistory testPid convId
            <*> Issues.getConversation testPid convId
            <*> Hasql.interpOne @(Text, Bool, Bool, Maybe Text) [HI.sql|SELECT status, findings IS NOT NULL, actions IS NOT NULL, error FROM apis.ai_routine_runs WHERE routine_id = #{scheduled.id}|]
            <*> Issues.listRoutineRuns testPid 10
        IORef.readIORef executions >>= (`shouldSatisfy` \case [AI.DurableRun context] -> context.policy == Issues.Autonomous; _ -> False)
        IORef.readIORef successors >>= (\items -> length items `shouldBe` 1)
        map (\message -> (message.role, message.content)) history
          `shouldBe` [(Issues.ChatUser, "Send a daily reliability brief to Slack"), (Issues.ChatAssistant, "Routine completed")]
        ((.routineRunningSince) =<< routine) `shouldBe` Nothing
        ((.routineNextRunAt) =<< routine) `shouldSatisfy` isJust
        runRecord `shouldBe` Just ("succeeded", True, True, Nothing)
        map (\run -> (run.conversationId, run.status)) recentRuns `shouldBe` [(convId, Issues.RunSucceeded)]
        for_ recentRuns \run -> do
          evidence <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid run.id
          (evidence >>= (.input)) `shouldBe` Just "Send a daily reliability brief to Slack"
          (evidence >>= (.instructions)) `shouldBe` Just "Send a daily reliability brief to Slack"

      for_ ([(Issues.RunRunning, Nothing)] <> [(status, Just $ addUTCTime 1200 frozenTime) | status <- [Issues.RunSucceeded, Issues.RunNoFindings, Issues.RunFailed, Issues.RunTimedOut, Issues.RunCancelled]] <> [(Issues.RunCancelled, Nothing)]) \(status, finishedAt) ->
        for_ [False, True] \paused ->
          it ("routineRecovery_advancesOnlyTheFinishedSlotLease: " <> show (status, paused, isJust finishedAt)) \tr -> do
            let conversation = UUIDId UUID.nil
                user = (getResponse tr.trSessAndHeader).user.id
                nextAt = addUTCTime 4500 frozenTime
                previousFinish = addUTCTime (-900) frozenTime
            Just slot <- runTestBg frozenTime tr do
              void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
              Issues.insertChatMessage testPid conversation Issues.ChatUser "Keep the cadence" Nothing Nothing
              Issues.upsertRoutine user testPid conversation $ fromRightShow $ Issues.mkRoutineInterval 15
            Just original <- runQueryEffect tr $ Issues.claimRoutine slot.id slot.scheduledAt
            runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routine_runs SET status = #{status}, finished_at = #{finishedAt} WHERE id = #{original.runId}|]
            runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET last_run_at = #{previousFinish} WHERE id = #{slot.id}|]
            let refreshedLease = status == Issues.RunCancelled && not paused && isNothing finishedAt
            when refreshedLease $ runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET running_since = now() - interval '31 minutes' WHERE id = #{slot.id}|]
            when paused $ runQueryEffect tr $ Issues.pauseRoutine testPid conversation
            advanceTestTime tr 3600
            let leaseState = runQueryEffect tr $ Hasql.interpOne @(Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Bool) [HI.sql|SELECT running_since, next_run_at, last_run_at, active FROM apis.ai_routines WHERE id = #{slot.id}|]
                ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
            before <- leaseState
            when refreshedLease do
              runQueryEffect tr (Issues.claimRoutine slot.id slot.scheduledAt) >>= (`shouldSatisfy` isNothing)
              leaseState >>= (`shouldBe` before)
            when (status == Issues.RunSucceeded && not paused)
              $ bracket_
                (ddl "ALTER TABLE apis.ai_routines ADD CONSTRAINT reject_lease_recovery CHECK (running_since IS NOT NULL) NOT VALID")
                (ddl "ALTER TABLE apis.ai_routines DROP CONSTRAINT reject_lease_recovery")
                do
                  tryAny (runQueryEffect tr $ Issues.recoverDueRoutines 100) >>= (`shouldSatisfy` isLeft)
                  leaseState >>= (`shouldBe` before)
            recovered <- runQueryEffect tr $ Issues.recoverDueRoutines 100
            if status == Issues.RunRunning
              then leaseState >>= (`shouldBe` before)
              else do
                map (\next -> (next.id, next.scheduledAt)) recovered `shouldBe` [(slot.id, nextAt) | not paused]
                leaseState >>= (`shouldBe` Just (Nothing, if paused then Nothing else Just nextAt, finishedAt <|> Just previousFinish, not paused))
                runQueryEffect tr (Issues.recoverDueRoutines 100) >>= (`shouldSatisfy` null)
                runQueryEffect tr (Issues.claimRoutine slot.id slot.scheduledAt) >>= (`shouldSatisfy` isNothing)
                unless paused do
                  advanceTestTime tr 900
                  Just next <- runQueryEffect tr $ Issues.claimRoutine slot.id nextAt
                  next.runId `shouldSatisfy` (/= original.runId)
                  next.requestedBy `shouldBe` user
                  next.policy `shouldBe` Issues.Autonomous
            runQueryEffect tr (Issues.getRoutineRun testPid original.runId) >>= (\run -> fmap (.status) run `shouldBe` Just status)

      for_ [DeletedRequester, DetachedSchedule, DetachedEvent, HistoricalRequester, ReauthorizedRequester] \fixture ->
        it ("routineRecovery_retiresAbandonedAttemptsWithoutChangingHealthyGrants: " <> show fixture) \tr -> do
          let conversation = UUIDId $ UUID.fromWords 0 0 113 1
              healthyConversation = UUIDId $ UUID.fromWords 0 0 113 2
              healthyRun = UUIDId $ UUID.fromWords 0 0 113 3
              user = (getResponse tr.trSessAndHeader).user.id
              artifact = fromRightShow $ Issues.mkArtifactKey "abandoned-evidence"
          (actor, abandoned) <- runTestBg frozenTime tr do
            actor <- Projects.createUser "Former" "Requester" "" "abandoned-requester@example.test"
            Projects.insertUser actor
            Hasql.interpExecute_ [HI.sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (#{testPid}, #{actor.id}, #{ProjectMembers.PEdit})|]
            for_ [conversation, healthyConversation] \conv -> void $ Issues.getOrCreateConversation testPid conv Issues.CTWeb (AE.object [])
            void $ Issues.startInteractiveRun testPid healthyConversation user healthyRun "Keep healthy work" Issues.Observe Issues.WebConversation
            run <- case fixture of
              DeletedRequester -> do
                let request = UUIDId $ UUID.fromWords 0 0 113 4
                void $ Issues.startInteractiveRun testPid conversation actor.id request "Retain abandoned work" Issues.Autonomous Issues.WebConversation
                pure request
              DetachedEvent -> do
                template <- maybe (throwIO $ ErrorCall "Missing Slack routine") pure $ find ((== "slack-investigation") . (.key)) Issues.routineTemplates
                void $ Issues.installRoutineTemplate user testPid conversation "UTC" template
                routine <- Issues.getEventRoutine testPid Issues.EventSlackMessage >>= maybe (throwIO $ ErrorCall "Missing event installation") pure
                started <- Issues.startEventRun routine (Issues.SlackCommandRequest conversation actor.id) Issues.StartRun conversation "Retain abandoned event"
                Issues.deleteRoutine testPid conversation
                maybe (throwIO $ ErrorCall "Missing event run") (pure . (.id)) started
              _ -> do
                slot <- Issues.upsertRoutine actor.id testPid conversation (fromRightShow $ Issues.mkRoutineInterval 15) >>= maybe (throwIO $ ErrorCall "Missing schedule") pure
                claimed <- Issues.claimRoutine slot.id slot.scheduledAt >>= maybe (throwIO $ ErrorCall "Missing scheduled run") pure
                case fixture of
                  DetachedSchedule -> Issues.deleteRoutine testPid conversation
                  ReauthorizedRequester -> void $ Issues.resumeRoutine user testPid conversation
                  HistoricalRequester ->
                    Hasql.interpExecute_
                      [HI.sql|UPDATE apis.ai_routines SET requested_by = #{user}, authority_version = authority_version + 1,
                    running_since = NULL, next_run_at = #{slot.scheduledAt} + interval '15 minutes' WHERE id = #{slot.id}|]
                pure claimed.runId
            when (fixture `elem` [DeletedRequester, HistoricalRequester, ReauthorizedRequester]) $ Hasql.interpExecute_ [HI.sql|DELETE FROM users.users WHERE id = #{actor.id}|]
            pure (actor.id, run)
          let proposal = Investigations.OperationProposal (Operation.ModelCall $ LLM.ToolCall "pending" "function" $ LLM.ToolFunction "save_artifact" mempty) $ AI.OperationPolicy Catalog.ConversationWrite Issues.Autonomous ProjectMembers.PEdit
          runQueryEffect tr do
            void $ Issues.saveArtifact testPid conversation artifact (Just "Retained evidence") "Original observation" Issues.ArtifactReplace
            for_ (zip [0, 1 :: Int64] [Investigations.OperationRunning, Investigations.OperationReturned $ Investigations.ToolResult "Known response" Nothing]) \(revision, outcome) ->
              Hasql.interpExecute_
                [HI.sql|INSERT INTO apis.ai_agent_operations (id, project_id, run_id, requested_by, checkpoint_revision, proposal, outcome)
                VALUES (gen_random_uuid(), #{testPid}, #{abandoned}, #{actor}, #{revision}, #{Aeson proposal}, #{Aeson outcome})|]
          before <- runQueryEffect tr $ Hasql.interp @[(UUIDId "ai_routine", Bool, Maybe UTCTime, Maybe UTCTime, Maybe Projects.UserId)] [HI.sql|SELECT id, active, next_run_at, running_since, requested_by FROM apis.ai_routines WHERE project_id = #{testPid}|]
          when (fixture == ReauthorizedRequester) do
            runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET running_since = now() - interval '31 minutes' WHERE project_id = #{testPid} AND conversation_id = #{conversation}|]
            captured <- runQueryEffect tr $ Hasql.interpOne @(UUIDId "ai_routine", UTCTime, UTCTime) [HI.sql|SELECT id, next_run_at, running_since FROM apis.ai_routines WHERE project_id = #{testPid} AND conversation_id = #{conversation}|]
            for_ captured \(rid, scheduled, leased) -> do
              runQueryEffect tr (Issues.claimRoutine rid scheduled) >>= (`shouldSatisfy` isNothing)
              runQueryEffect tr (Hasql.interpScalar @UTCTime [HI.sql|SELECT running_since FROM apis.ai_routines WHERE id = #{rid}|]) >>= (`shouldBe` leased)
          let ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
          bracket_
            (ddl "ALTER TABLE apis.ai_agent_operations ADD CONSTRAINT reject_abandoned_retirement CHECK (outcome->>'tag' <> 'OperationUnknown') NOT VALID")
            (ddl "ALTER TABLE apis.ai_agent_operations DROP CONSTRAINT reject_abandoned_retirement")
            do
              tryAny (runQueryEffect tr $ Issues.recoverDueRoutines 100) >>= (`shouldSatisfy` isLeft)
              runQueryEffect tr (Issues.getRoutineRun testPid abandoned) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunRunning)
              runQueryEffect tr (Issues.listRunArtifacts testPid abandoned) >>= (`shouldSatisfy` null)
          replicateM_ 2 $ void $ runQueryEffect tr $ Issues.recoverDueRoutines 100
          runQueryEffect tr (Issues.getRoutineRun testPid abandoned) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunCancelled)
          runQueryEffect tr (Issues.getRoutineRun testPid healthyRun) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunRunning)
          after <- runQueryEffect tr $ Hasql.interp @[(UUIDId "ai_routine", Bool, Maybe UTCTime, Maybe UTCTime, Maybe Projects.UserId)] [HI.sql|SELECT id, active, next_run_at, running_since, requested_by FROM apis.ai_routines WHERE project_id = #{testPid}|]
          if fixture == ReauthorizedRequester
            then do
              let grant (rid, active, _, _, owner) = (rid, active, owner)
              map grant after `shouldBe` map grant before
              [(next, lease) | (_, _, next, lease, _) <- after] `shouldBe` [(Just $ addUTCTime 1800 frozenTime, Nothing)]
            else after `shouldBe` before
          entries <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid abandoned
          map (.requestedBy) entries `shouldBe` [actor, actor]
          sort [AE.toJSON entry.outcome ^? key "tag" . _String | entry <- entries] `shouldBe` [Just "OperationReturned", Just "OperationUnknown"]
          runQueryEffect tr (Issues.getRoutineRunEvidence testPid abandoned) >>= (\evidence -> (evidence >>= (.requestedBy)) `shouldBe` if fixture `elem` [DeletedRequester, HistoricalRequester, ReauthorizedRequester] then Nothing else Just actor)
          runQueryEffect tr (Issues.listRunArtifacts testPid abandoned) >>= (\artifacts -> map (.content) artifacts `shouldBe` [Just "Original observation"])

      it "routineRecovery_retiresUnownedWorkWithoutInventingAnActorOrDeletingEvidence" \tr -> do
        let conversation = UUIDId UUID.nil
            user = (getResponse tr.trSessAndHeader).user.id
        Just slot <- runTestBg frozenTime tr do
          void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
          Issues.insertChatMessage testPid conversation Issues.ChatUser "Retain this investigation" Nothing Nothing
          Issues.upsertRoutine user testPid conversation $ fromRightShow $ Issues.mkRoutineInterval 15
        Just original <- runQueryEffect tr $ Issues.claimRoutine slot.id slot.scheduledAt
        let call = LLM.ToolCall "interrupted" "function" $ LLM.ToolFunction "save_artifact" mempty
            proposal = Investigations.OperationProposal (Operation.ModelCall call) $ AI.OperationPolicy Catalog.ConversationWrite Issues.Autonomous ProjectMembers.PAdmin
            artifact = fromRightShow $ Issues.mkArtifactKey "orphan-evidence"
        runQueryEffect tr (Issues.saveArtifact testPid conversation artifact (Just "Investigation evidence") "Retained observation" Issues.ArtifactReplace) >>= (`shouldSatisfy` isRight)
        runQueryEffect tr do
          for_ (zip [0, 1 :: Int64] [Investigations.OperationRunning, Investigations.OperationReturned $ Investigations.ToolResult "Saved response" Nothing]) \(revision, outcome) ->
            Hasql.interpExecute_
              [HI.sql|INSERT INTO apis.ai_agent_operations (id, project_id, run_id, requested_by, checkpoint_revision, proposal, outcome)
              VALUES (gen_random_uuid(), #{testPid}, #{original.runId}, #{user}, #{revision}, #{Aeson proposal}, #{Aeson outcome})|]
          Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routine_runs SET requested_by = NULL WHERE id = #{original.runId}|]
          Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET active = FALSE, requested_by = NULL, running_since = now() - interval '31 minutes' WHERE id = #{slot.id}|]
        let ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
        bracket_
          (ddl "ALTER TABLE apis.ai_agent_operations ADD CONSTRAINT reject_orphan_retirement CHECK (outcome->>'tag' <> 'OperationUnknown') NOT VALID")
          (ddl "ALTER TABLE apis.ai_agent_operations DROP CONSTRAINT reject_orphan_retirement")
          do
            tryAny (runQueryEffect tr $ Issues.recoverDueRoutines 100) >>= (`shouldSatisfy` isLeft)
            runQueryEffect tr (Issues.getRoutineRun testPid original.runId) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunRunning)
            runQueryEffect tr (Hasql.interpScalar @Bool [HI.sql|SELECT running_since IS NOT NULL FROM apis.ai_routines WHERE id = #{slot.id}|]) >>= (`shouldBe` True)
            runQueryEffect tr (Issues.listRunArtifacts testPid original.runId) >>= (`shouldSatisfy` null)
        for_ [1, 2 :: Int] \_ -> runQueryEffect tr (Issues.recoverDueRoutines 100) >>= (\due -> map (.id) due `shouldNotContain` [slot.id])
        runQueryEffect tr (Issues.getRoutineRun testPid original.runId) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunCancelled)
        runQueryEffect tr (Issues.getConversation testPid conversation) >>= (\routine -> ((.routineActive) <$> routine, routine >>= (.routineRunningSince), routine >>= (.routineNextRunAt)) `shouldBe` (Just False, Nothing, Nothing))
        entries <- runQueryEffect tr $ Investigations.listOperations @AI.OperationPolicy testPid original.runId
        map (.requestedBy) entries `shouldBe` [user, user]
        runQueryEffect tr (Hasql.interpScalar @Bool [HI.sql|SELECT requested_by IS NULL FROM apis.ai_routine_runs WHERE id = #{original.runId}|]) >>= (`shouldBe` True)
        sort [AE.toJSON entry.outcome ^? key "tag" . _String | entry <- entries] `shouldBe` [Just "OperationReturned", Just "OperationUnknown"]
        history <- runQueryEffect tr $ Issues.selectChatHistory testPid conversation
        map (.content) history `shouldBe` ["Retain this investigation"]
        runQueryEffect tr (Issues.listRunArtifacts testPid original.runId) >>= (\artifacts -> map (.content) artifacts `shouldBe` [Just "Retained observation"])

      for_ (["resume", "interval", "template"] :: [Text]) \activation -> for_ ([False, True] :: [Bool]) \unownedInstallation ->
        it ("routineReauthorization_retiresTheUnownedAttemptAndStartsAFreshGrant: " <> toString activation <> show unownedInstallation) \tr -> do
          let conversation = UUIDId UUID.nil
              user = (getResponse tr.trSessAndHeader).user.id
              interval = fromRightShow $ Issues.mkRoutineInterval 15
              artifact = fromRightShow $ Issues.mkArtifactKey "old-attempt-evidence"
          previousOwner <- runTestBg frozenTime tr do
            owner <- Projects.createUser "Previous" "Owner" "" "previous-routine-owner@example.test"
            Projects.insertUser owner
            Hasql.interpExecute_ [HI.sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (#{testPid}, #{owner.id}, 'admin')|]
            pure owner
          Just slot <- runTestBg frozenTime tr do
            void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
            Issues.insertChatMessage testPid conversation Issues.ChatUser "Original investigation" Nothing Nothing
            scheduled <- Issues.upsertRoutine previousOwner.id testPid conversation interval
            Issues.setRoutinePolicy previousOwner.id testPid conversation Issues.Observe
            pure scheduled
          Just original <- runQueryEffect tr $ Issues.claimRoutine slot.id slot.scheduledAt
          otherProject <- createTestProject tr "Other routine grant"
          Just otherSlot <- runTestBg frozenTime tr do
            void $ Issues.getOrCreateConversation otherProject conversation Issues.CTWeb (AE.object [])
            Issues.upsertRoutine previousOwner.id otherProject conversation interval
          Just otherRun <- runQueryEffect tr $ Issues.claimRoutine otherSlot.id otherSlot.scheduledAt
          runQueryEffect tr do
            void $ Issues.saveArtifact testPid conversation artifact Nothing "Original evidence" Issues.ArtifactReplace
            Issues.pauseRoutine testPid conversation
            Issues.pauseRoutine otherProject conversation
            Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET requested_by = NULL WHERE id IN (#{slot.id}, #{otherSlot.id})|]
            unless unownedInstallation $ Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET requested_by = #{user}, authority_version = authority_version + 1 WHERE id = #{slot.id}|]
            Hasql.interpExecute_ [HI.sql|DELETE FROM users.users WHERE id = #{previousOwner.id}|]
            Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'edit' WHERE project_id = #{testPid} AND user_id = #{user}|]
          template <- maybe (fail "Missing routine template") pure $ find ((== "daily-operations-briefing") . (.key)) Issues.routineTemplates
          let activate = runTestBg slot.scheduledAt tr $ case activation of
                "resume" -> Issues.resumeRoutine user testPid conversation
                "interval" -> Issues.upsertRoutine user testPid conversation interval
                _ -> Issues.installRoutineTemplate user testPid conversation "UTC" template
              ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
          when (activation == "resume" && unownedInstallation)
            $ bracket_
              (ddl "ALTER TABLE apis.ai_routines ADD CONSTRAINT reject_replacement_grant CHECK (NOT active) NOT VALID")
              (ddl "ALTER TABLE apis.ai_routines DROP CONSTRAINT reject_replacement_grant")
              do
                tryAny activate >>= (`shouldSatisfy` isLeft)
                runQueryEffect tr (Issues.getRoutineRun testPid original.runId) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunRunning)
                runQueryEffect tr (Hasql.interpScalar @Bool [HI.sql|SELECT running_since IS NOT NULL FROM apis.ai_routines WHERE id = #{slot.id}|]) >>= (`shouldBe` True)
                runQueryEffect tr (Issues.listRunArtifacts testPid original.runId) >>= (`shouldSatisfy` null)
          next <- activate
          scheduled <- maybe (fail "Unowned attempt stranded the replacement grant") pure next
          invocations <- IORef.newIORef ([] :: [Issues.RunContext])
          let execute config = case config.invocationMode of
                AI.DurableRun context -> Eff.liftIO (IORef.modifyIORef' invocations (<> [context])) $> Right (AI.AgenticChatResult "New investigation" [])
                AI.InteractiveReadOnly -> Eff.liftIO $ fail "Expected a fresh stored run"
          runTestBg scheduled.scheduledAt tr $ BackgroundJobs.runAIRoutineWith 1_000_000 execute (\_ _ -> pass) tr.trATCtx scheduled.id scheduled.scheduledAt
          contexts <- IORef.readIORef invocations
          [(context.requestedBy, context.rootId == context.id, context.id /= original.runId, context.permissionCeiling, context.policy) | context <- contexts]
            `shouldBe` [(user, True, True, ProjectMembers.PEdit, Issues.Observe)]
          runQueryEffect tr (Issues.getRoutineRun testPid original.runId) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunCancelled)
          runQueryEffect tr (Hasql.interpScalar @Bool [HI.sql|SELECT requested_by IS NULL AND policy = #{Issues.Observe} AND permission_ceiling = #{ProjectMembers.PAdmin} FROM apis.ai_routine_runs WHERE id = #{original.runId}|]) >>= (`shouldBe` True)
          runQueryEffect tr (Issues.listRunArtifacts testPid original.runId) >>= (\artifacts -> map (.content) artifacts `shouldBe` [Just "Original evidence"])
          runQueryEffect tr (Issues.getRoutineRun otherProject otherRun.runId) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunRunning)

      for_ (["run-status", "scheduler-lease"] :: [Text]) \boundary ->
        it ("scheduledCompletion_rollsBackPublicationAndLease_thenRetriesTheSavedAnswer: " <> toString boundary) \tr -> do
          let conversation = UUIDId UUID.nil
              user = (getResponse tr.trSessAndHeader).user.id
          scheduled <- runTestBg frozenTime tr do
            void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
            Issues.insertChatMessage testPid conversation Issues.ChatUser "Summarize the project" Nothing Nothing
            scheduled <- Issues.upsertRoutine user testPid conversation (fromRightShow $ Issues.mkRoutineInterval 15) >>= maybe (throwIO $ ErrorCall "Missing routine") pure
            Issues.setRoutineDestination testPid conversation Issues.DestinationConversation
            Issues.setRoutinePolicy user testPid conversation Issues.Observe
            pure scheduled
          grants <- IORef.newIORef ([] :: [Issues.ExecutionPolicy])
          models <- IORef.newIORef (0 :: Int)
          successors <- IORef.newIORef ([] :: [UTCTime])
          let provider :: ATBackgroundCtx a -> ATBackgroundCtx a
              provider = interpose @ELLM.LLM \_ -> \case
                ELLM.CallAgenticChat{} -> do
                  Eff.liftIO $ IORef.modifyIORef' models (+ 1)
                  pure $ Right $ LLM.Message LLM.Assistant "Retained scheduled report" LLM.defaultMessageData
                ELLM.CallLLM{} -> pure $ Left "Unexpected title request"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding request"
              execute config = do
                Eff.liftIO $ show config.access `shouldBe` show (AI.MemberAccess AI.ProjectMember user)
                case config.invocationMode of
                  AI.DurableRun context -> Eff.liftIO $ IORef.modifyIORef' grants (<> [context.policy])
                  _ -> Eff.liftIO $ expectationFailure "Expected a scheduled invocation"
                AI.runAgenticChatWithHistory config "Summarize the project" "model" "key"
              enqueueNext _ at = Eff.liftIO $ IORef.modifyIORef' successors (<> [at])
              run = tryAny $ runTestBg frozenTime tr $ provider $ BackgroundJobs.runAIRoutineWith 5_000_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
              ddl statement = withResource tr.trPool $ \conn -> void $ PGS.execute_ conn statement
          let (reject, restore) =
                if boundary == "run-status"
                  then ("ALTER TABLE apis.ai_routine_runs ADD CONSTRAINT scheduled_completion_regression CHECK (status <> 'succeeded')", "ALTER TABLE apis.ai_routine_runs DROP CONSTRAINT scheduled_completion_regression")
                  else ("ALTER TABLE apis.ai_routines ADD CONSTRAINT scheduled_lease_regression CHECK (running_since IS NOT NULL) NOT VALID", "ALTER TABLE apis.ai_routines DROP CONSTRAINT scheduled_lease_regression")
          bracket_ (ddl reject) (ddl restore) do
            result <- run
            history <- runQueryEffect tr $ Issues.selectChatHistory testPid conversation
            [message.content | message <- history, message.role == Issues.ChatAssistant] `shouldBe` []
            result `shouldSatisfy` isLeft
            pending <- runQueryEffect tr $ Issues.listConversationRuns testPid conversation 10
            map (.status) pending `shouldBe` [Issues.RunRunning]
            saved <- runQueryEffect tr $ Issues.getConversation testPid conversation
            ((.routineRunningSince) =<< saved) `shouldSatisfy` isJust
            ((.routineNextRunAt) =<< saved) `shouldBe` Just scheduled.scheduledAt
            runQueryEffect tr (Issues.listArtifacts testPid conversation Nothing) >>= (`shouldSatisfy` null)
            IORef.readIORef successors >>= (`shouldBe` [])
          runTestBg frozenTime tr do
            replacement <- Projects.createUser "Replacement" "Owner" "" "replacement-owner@example.test"
            Projects.insertUser replacement
            Hasql.interpExecute_ [HI.sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (#{testPid}, #{replacement.id}, 'edit')|]
            Issues.setRoutineDestination testPid conversation Issues.DestinationSlack
            Issues.setRoutinePolicy replacement.id testPid conversation Issues.Autonomous
          retainedRuns <- runQueryEffect tr $ Issues.listConversationRuns testPid conversation 10
          for_ retainedRuns $ \retained -> runQueryEffect tr (Issues.routineRunPolicyFor testPid retained.id user conversation) >>= (`shouldBe` Just Issues.Observe)
          runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET running_since = now() - interval '31 minutes' WHERE id = #{scheduled.id}|]
          run >>= (`shouldSatisfy` isRight)
          run >>= (`shouldSatisfy` isRight)
          IORef.readIORef models >>= (`shouldBe` 1)
          IORef.readIORef grants >>= (`shouldBe` [Issues.Observe, Issues.Observe])
          IORef.readIORef successors >>= (\items -> length items `shouldBe` 1)
          history <- runQueryEffect tr $ Issues.selectChatHistory testPid conversation
          [message.content | message <- history, message.role == Issues.ChatAssistant] `shouldBe` ["Retained scheduled report"]
          runs <- runQueryEffect tr $ Issues.listConversationRuns testPid conversation 10
          map (.status) runs `shouldBe` [Issues.RunSucceeded]
          for_ runs \completed -> do
            artifacts <- runQueryEffect tr $ Issues.listRunArtifacts testPid completed.id
            [(artifact.key, artifact.content) | artifact <- artifacts] `shouldBe` [(fromRightShow $ Issues.mkArtifactKey "latest-result", Just "Retained scheduled report")]

      it "scheduledCompletion_deletedInstallationClosesItsRunWithoutPublishing" \tr -> do
        let conversation = UUIDId UUID.nil
            user = (getResponse tr.trSessAndHeader).user.id
        Just slot <- runTestBg frozenTime tr do
          void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
          Issues.upsertRoutine user testPid conversation $ fromRightShow $ Issues.mkRoutineInterval 15
        Just lease <- runQueryEffect tr $ Issues.claimRoutine slot.id slot.scheduledAt
        context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.BackgroundClaim user conversation lease.runId lease.claimedAt
        runQueryEffect tr $ Issues.deleteConversation testPid conversation
        result <- runTestBg frozenTime tr $ Issues.completeRun context $ Issues.CompletedResult Issues.ResultSucceeded Issues.PublishAnswer $ AI.prepareRunAnswer $ AI.AgenticChatResult "Must not be published" []
        result `shouldSatisfy` (\case Issues.RunCompleted Issues.CompletedCancelled Nothing -> True; _ -> False)
        runQueryEffect tr (Issues.getRoutineRun testPid lease.runId) >>= (\run -> fmap (.status) run `shouldBe` Just Issues.RunCancelled)
        runQueryEffect tr (Issues.listRunArtifacts testPid lease.runId) >>= (`shouldSatisfy` null)
        runQueryEffect tr (Issues.selectChatHistory testPid conversation) >>= (`shouldSatisfy` null)

      it "scheduledCompletion_rejectsAnOlderLeaseBeforeWritingAnyResult" \tr -> do
        let conversation = UUIDId UUID.nil
            user = (getResponse tr.trSessAndHeader).user.id
        scheduled <- runTestBg frozenTime tr do
          void $ Issues.getOrCreateConversation testPid conversation Issues.CTWeb (AE.object [])
          Issues.upsertRoutine user testPid conversation (fromRightShow $ Issues.mkRoutineInterval 15) >>= maybe (throwIO $ ErrorCall "Missing routine") pure
        recovered <- IORef.newIORef Nothing
        successors <- IORef.newIORef ([] :: [UTCTime])
        let execute _ = do
              Hasql.interpExecute_ [HI.sql|UPDATE apis.ai_routines SET running_since = now() - interval '31 minutes' WHERE id = #{scheduled.id}|]
              lease <- Issues.claimRoutine scheduled.id scheduled.scheduledAt
              Eff.liftIO $ IORef.writeIORef recovered lease
              pure $ Right $ AI.AgenticChatResult "Stale report" []
            enqueueNext _ at = Eff.liftIO $ IORef.modifyIORef' successors (<> [at])
        runTestBg frozenTime tr $ BackgroundJobs.runAIRoutineWith 5_000_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
        history <- runQueryEffect tr $ Issues.selectChatHistory testPid conversation
        history `shouldSatisfy` null
        runQueryEffect tr (Issues.listArtifacts testPid conversation Nothing) >>= (`shouldSatisfy` null)
        IORef.readIORef successors >>= (`shouldBe` [])
        lease <- IORef.readIORef recovered >>= maybe (fail "Expected the replacement lease") pure
        context <- runTestBg frozenTime tr $ AI.requireRunContext testPid $ Issues.BackgroundClaim user conversation lease.runId lease.claimedAt
        result <- runTestBg frozenTime tr $ Issues.completeRun context $ Issues.CompletedResult Issues.ResultSucceeded Issues.PublishAnswer $ AI.prepareRunAnswer $ AI.AgenticChatResult "Current report" []
        result `shouldSatisfy` (\case Issues.RunCompleted Issues.CompletedSucceeded (Just _) -> True; _ -> False)
        historyAfter <- runQueryEffect tr $ Issues.selectChatHistory testPid conversation
        [message.content | message <- historyAfter, message.role == Issues.ChatAssistant] `shouldBe` ["Current report"]

      it "suppresses a findings-only routine answer when the agent reports no findings" \tr -> do
        case find ((== "error-regression-radar") . (.key)) Issues.routineTemplates of
          Nothing -> expectationFailure "missing findings-only template"
          Just template -> do
            let convId = UUIDId UUID.nil
                execute _ = pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Nothing material changed\",\"has_findings\":false,\"widgets\":[]}", AI.toolCalls = []}
            (history, runStatus) <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              Issues.insertChatMessage testPid convId Issues.ChatUser template.prompt Nothing Nothing
              scheduled <- Issues.installRoutineTemplate (getResponse tr.trSessAndHeader).user.id testPid convId "UTC" template >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
              BackgroundJobs.runAIRoutineWith 1_000_000 execute (\_ _ -> pass) tr.trATCtx scheduled.id scheduled.scheduledAt
              (,)
                <$> Issues.selectChatHistory testPid convId
                <*> (fmap fst <$> Hasql.interpOne @(Text, Bool) [HI.sql|SELECT status, TRUE FROM apis.ai_routine_runs WHERE routine_id = #{scheduled.id}|])
            map (.role) history `shouldBe` [Issues.ChatUser]
            runStatus `shouldBe` Just "no_findings"

      for_ (["cancel", "pause", "revoke"] :: [Text]) \action ->
        it ("scheduledCompletion_withdrawnAuthorityPreventsPublication: " <> toString action) \tr -> do
          let convId = UUIDId UUID.nil
              interval = fromRightShow $ Issues.mkRoutineInterval 15
              execute _ = do
                case action of
                  "cancel" -> Issues.cancelRoutineRun testPid convId
                  "pause" -> Issues.pauseRoutine testPid convId
                  _ -> do
                    let user = (getResponse tr.trSessAndHeader).user.id
                    Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{user}|]
                pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Do not publish\",\"has_findings\":true}", AI.toolCalls = []}
          (history, runStatus) <- runTestBg frozenTime tr do
            void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
            scheduled <- Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
            BackgroundJobs.runAIRoutineWith 1_000_000 execute (\_ _ -> pass) tr.trATCtx scheduled.id scheduled.scheduledAt
            (,)
              <$> Issues.selectChatHistory testPid convId
              <*> (fmap fst <$> Hasql.interpOne @(Text, Bool) [HI.sql|SELECT status, TRUE FROM apis.ai_routine_runs WHERE routine_id = #{scheduled.id}|])
          history `shouldSatisfy` null
          runStatus `shouldBe` Just "cancelled"

      it "records a routine timeout as an execution event and completes the lease" \tr -> do
        successors <- IORef.newIORef ([] :: [UTCTime])
        let convId = UUIDId UUID.nil
            interval = fromRightShow $ Issues.mkRoutineInterval 15
            execute _ = Eff.liftIO (threadDelay 50_000) $> Right AI.AgenticChatResult{AI.response = "late", AI.toolCalls = []}
            enqueueNext _ at = Eff.liftIO $ IORef.modifyIORef' successors (<> [at])
        (history, routine, runStatus) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          scheduled <- Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
          BackgroundJobs.runAIRoutineWith 1_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          (,,)
            <$> Issues.selectChatHistory testPid convId
            <*> Issues.getConversation testPid convId
            <*> (fmap fst <$> Hasql.interpOne @(Text, Bool) [HI.sql|SELECT status, TRUE FROM apis.ai_routine_runs WHERE routine_id = #{scheduled.id}|])
        map (.role) history `shouldBe` [Issues.ChatExecutionEvent]
        all (isNothing . AI.dbMessageToLLMMessage) history `shouldBe` True
        ((.routineRunningSince) =<< routine) `shouldBe` Nothing
        IORef.readIORef successors >>= (\items -> length items `shouldBe` 1)
        runStatus `shouldBe` Just "timed_out"

    describe "Live API calls (uses golden files)" do
      it "processes error trend query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "plot error trend over time" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON (agenticResp{AI.toolCalls = Nothing} :: AI.LLMResponse)
            assertJsonGolden "agentic/error_trend_response.json" responseJson
            length agenticResp.widgets `shouldSatisfy` (>= 0)

      it "processes service breakdown query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "show warning and error counts grouped by service" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON (agenticResp{AI.toolCalls = Nothing} :: AI.LLMResponse)
            assertJsonGolden "agentic/service_breakdown_response.json" responseJson

      it "processes explanation-only query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "what services have the most errors?" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON (agenticResp{AI.toolCalls = Nothing} :: AI.LLMResponse)
            assertJsonGolden "agentic/explanation_only_response.json" responseJson
            isJust agenticResp.explanation || not (null agenticResp.widgets) `shouldBe` True

      it "handles empty API key gracefully" \tr -> do
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "show errors" Nothing (getOpenAIModel tr) ""
        case result of
          Left err -> T.isInfixOf "unavailable" err || T.isInfixOf "error" (T.toLower err) `shouldBe` True
          Right _ -> pass
        cleanupTelemetryData tr

    describe "Widget URL generation" do
      it "generates signed widget PNG URLs correctly" \tr -> do
        let widget = def{Widget.wType = Widget.WTTimeseries, Widget.title = Just "Test Chart", Widget.query = Just "service == \"api\""}
            secret = tr.trATCtx.env.apiKeyEncryptionSecretKey
            baseUrl = tr.trATCtx.env.hostUrl

        url <- fold <$> liftIO (Eff.runEff $ Logging.runLog "test" tr.trLogger tr.trATCtx.config.logLevel $ Widget.widgetPngUrl secret baseUrl testPid widget Nothing Nothing Nothing)

        url `shouldSatisfy` T.isInfixOf "widgetZ="
        url `shouldSatisfy` T.isInfixOf "&sig="

      it "rejects URLs exceeding 8000 characters" \tr -> do
        -- gzip compresses repeated chars well, so use unique numbered strings that resist compression
        let incompressible = T.concat [show @Text i <> "abcdefg" | i <- [1 .. 3000 :: Int]]
            hugeWidget = def{Widget.wType = Widget.WTTable, Widget.title = Just incompressible, Widget.query = Just incompressible}
            secret = tr.trATCtx.env.apiKeyEncryptionSecretKey
            baseUrl = tr.trATCtx.env.hostUrl

        url <- liftIO $ Eff.runEff $ Logging.runLog "test" tr.trLogger tr.trATCtx.config.logLevel $ Widget.widgetPngUrl secret baseUrl testPid hugeWidget Nothing Nothing Nothing

        url `shouldBe` Nothing
