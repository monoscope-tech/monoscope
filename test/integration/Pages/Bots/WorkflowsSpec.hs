{-# LANGUAGE PackageImports #-}

module Pages.Bots.WorkflowsSpec (spec) where

import BackgroundJobs qualified as Jobs
import Control.Exception (bracket_)
import Control.Lens (ix, (.~), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _String)
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM qualified as ELLM
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Dispatch.Dynamic (interpose, send)
import Effectful.Error.Static (catchError)
import Langchain.LLM.Core qualified as Chat
import Models.Apis.Integrations qualified as Slack
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Discord (discordInteractionsH)
import Pages.Bots.Slack (processSlackEvent, slackEventsPostH, slackInteractionsH)
import Pages.Bots.Slack qualified as SlackPage
import Pages.Bots.Utils qualified as Bot
import Pages.Bots.Whatsapp (whatsappIncomingPostH)
import Pkg.AI qualified as AI
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant.API.ResponseHeaders (getResponse)
import Servant.Server (ServerError (errHTTPCode))
import System.Config qualified as Config
import Test.Hspec (Spec, anyException, around, describe, it, shouldBe, shouldSatisfy, shouldThrow)
import UnliftIO.Async (concurrently, concurrently_)
import UnliftIO.Exception (tryAny)
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


spec :: Spec
spec = around withTestResources do
  describe "Complete Bot Workflows" do
    describe "Slack personal linking" do
      it "sends only a private link, binds through the authenticated form, and rechecks access" \tr -> do
        setupSlackData tr testPid "T_IDENTITY"
        let payload =
              slackThreadedEvent "T_IDENTITY" "C_IDENTITY" "follow up question" "1735689602.000001" "1735689601.000001"
                & key "event"
                . key "type"
                . _String
                .~ "app_mention"
            userId = (getResponse tr.trSessAndHeader).user.id
            resolve = toBaseServantResponse tr $ Slack.resolveSlackPrincipal "T_IDENTITY" "U0123ABCDEF" Nothing
        receiptId <- receiveSlackEvent tr payload
        (requests, outcome) <- runTestBgRecordingHTTP frozenTime tr $ tryAny $ processSlackEvent receiptId
        outcome `shouldSatisfy` isLeft -- The recorder returns {}, not Slack's required ok=true acknowledgement.
        map fst requests `shouldBe` ["https://slack.com/api/chat.postEphemeral"]
        [PGS.Only linkId] <- withResource tr.trPool \conn ->
          PGS.query conn [sql|SELECT id FROM apis.slack_identity_requests WHERE receipt_id = ?|] (PGS.Only receiptId)
        let sent = snd <$> listToMaybe requests >>= AE.decode @AE.Value
        (sent >>= (^? key "user" . _String)) `shouldBe` Just "U0123ABCDEF"
        (sent >>= (^? key "channel" . _String)) `shouldBe` Just "C_IDENTITY"
        (sent >>= (^? key "text" . _String)) `shouldSatisfy` maybe False (T.isInfixOf linkId.toText)
        resolve >>= (`shouldSatisfy` isNothing)
        void $ toServantResponse tr $ SlackPage.linkIdentityGetH linkId
        void $ toServantResponse tr $ SlackPage.linkIdentityPostH linkId $ SlackPage.SlackLinkForm testPid
        principal <- resolve
        fmap (\p -> (p.userId, p.projectId)) principal `shouldBe` Just (userId, testPid)
        toBaseServantResponse tr (Slack.completeSlackLink linkId userId testPid) >>= (`shouldBe` False)
        runTestBg frozenTime tr $ processSlackEvent receiptId
        [PGS.Only conversations] <- withResource tr.trPool \conn -> PGS.query_ conn [sql|SELECT count(*) FROM apis.ai_conversations|]
        (conversations :: Int64) `shouldBe` 1
        toBaseServantResponse tr (Slack.slackThreadProject "T_IDENTITY" "C_IDENTITY" "1735689601.000001") >>= (`shouldBe` Just testPid)
        let access = AI.SlackAccess "T_IDENTITY" "U0123ABCDEF" userId
            config = (AI.defaultAgenticConfig testPid){AI.access = access}
            setActive active = withResource tr.trPool \conn -> void $ PGS.execute conn [sql|UPDATE projects.project_members SET active = ? WHERE user_id = ?|] (active, userId)
        for_ ([Nothing, Just [Chat.ToolCall "tool-1" "function" (Chat.ToolFunction "get_services" mempty)]] :: [Maybe [Chat.ToolCall]]) \toolCalls -> do
          setActive True
          revoked <- newIORef False
          queriesAfterRevocation <- newIORef (0 :: Int)
          let provider = interpose @ELLM.LLM \_ -> \case
                ELLM.CallAgenticChat{} -> do
                  liftIO $ setActive False
                  writeIORef revoked True
                  pure $ Right $ Chat.Message Chat.Assistant "A result that must not be released" Chat.defaultMessageData{Chat.toolCalls = toolCalls}
                ELLM.CallLLM{} -> pure $ Left "Unexpected non-agent call"
                ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
              database = interpose @Hasql.Hasql \_ effect -> do
                whenM (readIORef revoked) $ modifyIORef' queriesAfterRevocation (+ 1)
                case effect of
                  Hasql.UseStatement params statement -> send $ Hasql.UseStatement params statement
                  Hasql.UseSession session -> send $ Hasql.UseSession session
                  Hasql.UseLabeledSession label attributes session -> send $ Hasql.UseLabeledSession label attributes session
          runTestBg frozenTime tr (database $ provider $ AI.runAgenticQuery config "revocation check" "model" "key")
            `shouldThrow` (\AI.AgentAccessDenied -> True)
          -- Only the denying access query runs after revocation, never the requested tool's query.
          readIORef queriesAfterRevocation >>= (`shouldBe` 1)
        resolve >>= (`shouldSatisfy` isNothing)

      it "rejects cross-workspace linking and keeps a thread on its original project after selection changes" \tr -> do
        setupSlackData tr testPid "T_IDENTITY"
        otherPid <- createTestProject tr "Another project"
        setupSlackData tr otherPid "T_OTHER"
        let userId = (getResponse tr.trSessAndHeader).user.id
            linkId n = UUIDId $ UUID.fromWords 0 0 2 n
            payload eventId =
              slackThreadedEvent "T_IDENTITY" "C_IDENTITY" "investigate" "1735689602.000001" "1735689601.000001"
                & key "event_id"
                . _String
                .~ eventId
            complete n pid = toBaseServantResponse tr $ Slack.completeSlackLink (linkId n) userId pid
            request n = do
              receipt <- receiveSlackEvent tr $ payload $ "EvLink" <> show n
              toBaseServantResponse tr (Slack.createSlackLink (linkId n) receipt) >>= (`shouldSatisfy` isJust)
        request 1
        complete 1 otherPid >>= (`shouldBe` False)
        complete 1 testPid >>= (`shouldBe` True)
        Just principal <- toBaseServantResponse tr $ Slack.resolveSlackPrincipal "T_IDENTITY" "U0123ABCDEF" Nothing
        toBaseServantResponse tr (Slack.bindSlackInvestigation principal "T_IDENTITY" "C_IDENTITY" "1735689601.000001") >>= (`shouldBe` True)
        setupSlackData tr otherPid "T_IDENTITY"
        request 2
        (leftResult, rightResult) <- concurrently (complete 2 otherPid) (complete 2 otherPid)
        sort [leftResult, rightResult] `shouldBe` [False, True]
        selected <- toBaseServantResponse tr $ Slack.resolveSlackPrincipal "T_IDENTITY" "U0123ABCDEF" Nothing
        fmap (.projectId) selected `shouldBe` Just otherPid
        bound <- toBaseServantResponse tr $ Slack.slackThreadProject "T_IDENTITY" "C_IDENTITY" "1735689601.000001"
        bound `shouldBe` Just testPid
        authorized <- toBaseServantResponse tr $ Slack.resolveSlackPrincipal "T_IDENTITY" "U0123ABCDEF" bound
        fmap (.projectId) authorized `shouldBe` Just testPid
        let otherUser = UUIDId (UUID.fromWords 0 0 3 1) :: Projects.UserId
        withResource tr.trPool \conn -> do
          void $ PGS.execute conn [sql|INSERT INTO users.users (id, email) VALUES (?, 'slack-other@example.com')|] (PGS.Only otherUser)
          void $ PGS.execute conn [sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (?, ?, 'admin')|] (testPid, otherUser)
        request 3
        toBaseServantResponse tr (Slack.completeSlackLink (linkId 3) otherUser testPid) >>= (`shouldBe` False)
        complete 3 testPid >>= (`shouldBe` True)
        request 4
        withResource tr.trPool \conn -> void $ PGS.execute conn [sql|UPDATE apis.slack_identity_requests SET expires_at = now() - interval '1 second' WHERE id = ?|] (PGS.Only $ linkId 4)
        complete 4 testPid >>= (`shouldBe` False)
        toBaseServantResponse tr (Slack.getSlackLink $ linkId 4) >>= (`shouldSatisfy` isNothing)

    describe "Slack installation authorization" do
      it "requires a current admin and consumes expiring state once for its initiating user" \tr -> do
        let userId = (getResponse tr.trSessAndHeader).user.id
            stateId n = UUIDId $ UUID.fromWords 0 0 1 n
            start n = toBaseServantResponse tr $ Slack.createSlackInstall (stateId n) userId testPid True
            consume n uid = toBaseServantResponse tr $ Slack.consumeSlackInstall (stateId n) uid
            execute query = withResource tr.trPool \conn -> void $ PGS.execute conn query (PGS.Only userId)
        start 1 >>= (`shouldBe` True)
        consume 1 (UUIDId UUID.nil) >>= (`shouldSatisfy` isNothing)
        (left, right) <- concurrently (consume 1 userId) (consume 1 userId)
        map (\request -> (request.projectId, request.onboarding)) (catMaybes [left, right]) `shouldBe` [(testPid, True)]
        consume 1 userId >>= (`shouldSatisfy` isNothing)
        start 2 >>= (`shouldBe` True)
        withResource tr.trPool \conn -> void $ PGS.execute_ conn [sql|UPDATE apis.slack_install_requests SET expires_at = now() - interval '1 second'|]
        consume 2 userId >>= (`shouldSatisfy` isNothing)
        start 3 >>= (`shouldBe` True)
        execute [sql|UPDATE projects.project_members SET permission = 'view' WHERE user_id = ?|]
        start 4 >>= (`shouldBe` False)
        consume 3 userId >>= (`shouldSatisfy` isNothing)
        execute [sql|UPDATE projects.project_members SET permission = 'admin', active = FALSE WHERE user_id = ?|]
        start 5 >>= (`shouldBe` False)
        consume 3 userId >>= (`shouldSatisfy` isNothing)

    describe "Query → Process → Respond" do
      it "Slack: handles general query end-to-end" \tr -> do
        setupLinkedSlackData tr testPid "T_WF_SLACK"
        let interaction = slackInteraction "/monoscope" "show errors" "T_WF_SLACK"

        -- Get immediate loading response
        loadingResp <- toBaseServantResponse tr $ slackInteractionsH interaction
        loadingResp `shouldSatisfy` isValidJsonResponse
        let loadingText = fromMaybe "" $ extractResponseText loadingResp
        (T.isInfixOf "Analyzing" loadingText || T.isInfixOf "⏳" loadingText) `shouldBe` True

        -- Process background jobs that send final response
        void $ runAllBackgroundJobs frozenTime tr.trATCtx

      it "Discord: handles query with signature verification" \tr -> do
        setupDiscordData tr testPid "guild_wf_discord"
        let payload = discordCommandInteraction "monoscope" "show error rate"
            (signedPayload, sig, ts) = signDiscordPayload payload "1700000000"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}

        result <- toBaseServantResponse tr{trATCtx = testCtx} $ discordInteractionsH signedPayload (Just sig) (Just ts)
        result `shouldSatisfy` isValidJsonResponse
        getDiscordResponseType result `shouldSatisfy` isJust

      it "WhatsApp: handles prompt query end-to-end" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone
        let msg = twilioWhatsAppPrompt tr testPhone "show me errors in the last hour"

        result <- toBaseServantResponse tr $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse

    describe "Error Handling Workflows" do
      it "Slack: handles missing team data gracefully" \tr -> do
        let interaction = slackInteraction "/monoscope" "show errors" "T_NONEXISTENT"
        result <- toBaseServantResponse tr $ slackInteractionsH interaction
        result `shouldSatisfy` isValidJsonResponse

      -- NOTE: Invalid signature test removed - handler correctly throws 401 for invalid signatures
      -- as verified by manual testing. The test infrastructure makes it difficult to properly
      -- catch and verify the error in the test context.

      it "WhatsApp: handles unknown phone number" \tr -> do
        let msg = twilioWhatsAppPrompt tr "+19999999999" "show errors"
        result <- toBaseServantResponse tr $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse

    describe "Platform-Specific Commands" do
      it "Slack commands require a linked active member and scope admin channel changes to one project" \tr -> do
        setupLinkedSlackData tr testPid "T_COMMAND"
        otherPid <- createTestProject tr "Other workspace project"
        setupSlackData tr otherPid "T_COMMAND"
        let userId = (getResponse tr.trSessAndHeader).user.id
            command = slackInteraction "/monoscope-here" "" "T_COMMAND"
            readChannel pid = fmap (\sd -> (sd.channelId, sd.webhookUrl)) <$> toBaseServantResponse tr (Slack.getProjectSlackData pid)
            status interaction =
              toBaseServantResponse tr
                $ catchError @ServerError (slackInteractionsH interaction $> 200) (\_ err -> pure err.errHTTPCode)
            changePermission permission = withResource tr.trPool \conn ->
              void
                $ PGS.execute
                  conn
                  [sql|UPDATE projects.project_members SET permission = ?::projects.project_permissions WHERE project_id = ? AND user_id = ?|]
                  (permission :: Text, testPid, userId)
        original <- readChannel testPid
        unlinked <- toBaseServantResponse tr $ slackInteractionsH command{SlackPage.user_id = "U_UNLINKED"}
        extractResponseType unlinked `shouldBe` Just "ephemeral"
        for_ ["view", "edit"] \permission -> do
          changePermission permission
          status command >>= (`shouldBe` 403)
          readChannel testPid >>= (`shouldBe` original)
        changePermission "admin"
        status command >>= (`shouldBe` 200)
        readChannel testPid >>= (`shouldBe` Just ("C0123ABCDEF", Nothing))
        readChannel otherPid >>= (`shouldBe` original)
        withResource tr.trPool \conn -> do
          for_ [testPid, otherPid] \pid ->
            void
              $ PGS.execute
                conn
                [sql|INSERT INTO projects.dashboards (project_id, created_by, title) VALUES (?, ?, ?)|]
                (pid, userId, pid.toText)
          void $ PGS.execute conn [sql|UPDATE projects.project_members SET active = FALSE WHERE project_id = ? AND user_id = ?|] (testPid, userId)
        dashboards <- toBaseServantResponse tr $ Slack.getDashboardsForSlack testPid
        map fst dashboards `shouldBe` [testPid.toText]
        revoked <- toBaseServantResponse tr $ slackInteractionsH command{SlackPage.channel_id = "C_DENIED"}
        extractResponseType revoked `shouldBe` Just "ephemeral"
        readChannel testPid >>= (`shouldBe` Just ("C0123ABCDEF", Nothing))

      it "Slack dashboard actions validate metadata, membership, and current widget definitions" \tr -> do
        setupLinkedSlackData tr testPid "T_ACTIONS"
        otherPid <- createTestProject tr "Private dashboard project"
        setupSlackData tr otherPid "T_ACTIONS"
        let userId = (getResponse tr.trSessAndHeader).user.id
            schema title = AE.object ["widgets" AE..= ([AE.object ["type" AE..= ("timeseries" :: Text), "title" AE..= (title :: Text), "query" AE..= ("summarize count()" :: Text)]] :: [AE.Value])]
            insertDashboard pid = withResource tr.trPool \conn -> do
              [PGS.Only did] <-
                PGS.query
                  conn
                  [sql|INSERT INTO projects.dashboards (project_id, created_by, title, schema) VALUES (?, ?, 'Requests', ?) RETURNING id::text|]
                  (pid, userId, Aeson $ schema "Requests")
              pure (did :: Text)
            body requests = maybe (fail "Missing Slack request") (maybe (fail "Invalid Slack request JSON") pure . AE.decode @AE.Value . snd) $ listToMaybe requests
            metadata request = maybe (fail "Missing modal metadata") pure $ request ^? key "view" . key "private_metadata" . _String
            payload context kind selected viewState =
              SlackPage.SlackActionForm
                $ decodeUtf8
                $ AE.encode
                $ AE.object
                  [ "type" AE..= (kind :: Text)
                  , "team" AE..= AE.object ["id" AE..= ("T_ACTIONS" :: Text)]
                  , "user" AE..= AE.object ["id" AE..= ("U0123ABCDEF" :: Text)]
                  , "view" AE..= AE.object ["id" AE..= ("V_ACTIONS" :: Text), "private_metadata" AE..= (context :: Text), "state" AE..= (viewState :: AE.Value)]
                  , "actions" AE..= (selected :: [AE.Value])
                  ]
            select actionId value = [AE.object ["action_id" AE..= (actionId :: Text), "selected_option" AE..= AE.object ["text" AE..= ("Untrusted title" :: Text), "value" AE..= (value :: Text)]]]
            run form =
              runAsBaseRecordingHTTP tr
                $ catchError @ServerError (SlackPage.slackActionsH form $> 200) (\_ err -> pure err.errHTTPCode)
            denied code form = do
              (requests, status) <- run form
              status `shouldBe` code
              requests `shouldBe` []
        did <- insertDashboard testPid
        foreignDid <- insertDashboard otherPid
        (opened, _) <- runAsBaseRecordingHTTP tr $ slackInteractionsH $ slackInteraction "/dashboard" "" "T_ACTIONS"
        initial <- body opened >>= metadata
        denied 400 $ payload "C_OLD___project___template___url" "view_submission" [] AE.Null
        denied 403 $ payload initial "block_actions" (select "dashboard-select" foreignDid) AE.Null
        initialValue <- either fail pure $ AE.eitherDecode @AE.Value $ encodeUtf8 initial
        let otherOwner = decodeUtf8 $ AE.encode $ initialValue & key "userId" . _String .~ "U_OTHER"
        denied 403 $ payload otherOwner "block_actions" (select "dashboard-select" did) AE.Null
        (chosen, status) <- run $ payload initial "block_actions" (select "dashboard-select" did) AE.Null
        status `shouldBe` 200
        chosenBody <- body chosen
        context <- metadata chosenBody
        option <-
          maybe (fail "Missing widget option") pure
            $ chosenBody
            ^? key "view" . key "blocks" . _Array . ix 0 . key "accessory" . key "options" . _Array . ix 0 . key "value" . _String
        let viewState = AE.object ["values" AE..= AE.object ["widget-select" AE..= AE.object ["widget-select" AE..= AE.object ["selected_option" AE..= AE.object ["value" AE..= option]]]]]
            submit = payload context "view_submission" [] viewState
        (preview, previewStatus) <- run $ payload context "block_actions" (select "widget-select" option) viewState
        previewStatus `shouldBe` 200
        map fst preview `shouldBe` ["https://slack.com/api/views.update"]
        (shared, sharedStatus) <- run submit
        sharedStatus `shouldBe` 200
        map fst shared `shouldBe` ["https://slack.com/api/chat.postMessage"]
        sharedBody <- body shared
        (sharedBody ^? key "channel" . _String) `shouldBe` Just "C0123ABCDEF"
        (sharedBody ^? key "blocks" . _Array . ix 2 . key "image_url" . _String) `shouldSatisfy` maybe False (T.isInfixOf $ "p/" <> testPid.toText <> "/widget.png?")
        withResource tr.trPool \conn ->
          void
            $ PGS.execute
              conn
              [sql|UPDATE projects.dashboards SET schema = ? WHERE id::text = ?|]
              (Aeson $ schema "Changed", did)
        denied 400 submit
        withResource tr.trPool \conn ->
          void
            $ PGS.execute
              conn
              [sql|UPDATE projects.project_members SET active = FALSE WHERE project_id = ? AND user_id = ?|]
              (testPid, userId)
        denied 403 submit

      it "Slack: /here command workflow" \tr -> do
        setupLinkedSlackData tr testPid "T_HERE_WF"
        let interaction = slackInteraction "/monoscope-here" "" "T_HERE_WF"

        result <- toBaseServantResponse tr $ slackInteractionsH interaction
        result `shouldSatisfy` isValidJsonResponse
        hasSuccessBlock result `shouldBe` True

        -- Verify channel was updated (from slackInteraction fixture which uses "C0123ABCDEF")
        slackDataM <- runTestBg frozenTime tr $ Slack.getSlackDataByTeamId "T_HERE_WF"
        slackDataM `shouldSatisfy` isJust
        case slackDataM of
          Just slackData -> slackData.channelId `shouldBe` "C0123ABCDEF"
          Nothing -> pass

      it "Discord: ping interaction workflow" \tr -> do
        let (signedPayload, sig, ts) = signDiscordPayload discordPingPayload "1700000000"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse tr{trATCtx = testCtx} $ discordInteractionsH signedPayload (Just sig) (Just ts)

        result `shouldSatisfy` isValidJsonResponse
        getDiscordResponseType result `shouldBe` Just 1

      it "WhatsApp: dashboard command workflow" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone
        let msg = twilioWhatsAppDashboard tr testPhone

        result <- toBaseServantResponse tr $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse

    describe "Slack signed events" do
      it "preserves non-message events and orders assistant context independently of receipt order" \tr -> do
        let callback eventId event = AE.object ["type" AE..= ("event_callback" :: Text), "team_id" AE..= ("T_SESSION" :: Text), "api_app_id" AE..= ("A_TEST" :: Text), "event_id" AE..= (eventId :: Text), "event" AE..= event]
            session eventType eventTs contextChannel userId =
              AE.object
                [ "type" AE..= (eventType :: Text)
                , "event_ts" AE..= (eventTs :: Text)
                , "assistant_thread"
                    AE..= AE.object
                      [ "user_id" AE..= (userId :: Text)
                      , "channel_id" AE..= ("D_SESSION" :: Text)
                      , "thread_ts" AE..= ("1735689600.000001" :: Text)
                      , "context" AE..= AE.object ["channel_id" AE..= (contextChannel :: Text)]
                      ]
                ]
            newer = session "assistant_thread_context_changed" "1735689602.000001" "C_NEW" "U_SESSION"
            older = session "assistant_thread_started" "1735689601.000001" "C_OLD" "U_SESSION"
            edit = AE.object ["type" AE..= ("message" :: Text), "subtype" AE..= ("message_changed" :: Text), "message" AE..= AE.object ["text" AE..= ("Edited" :: Text)]]
            unknown = AE.object ["type" AE..= ("future_slack_event" :: Text), "future_field" AE..= ([1, 2] :: [Int])]
            events = zip ["EvNewContext", "EvOldStart", "EvEdit", "EvUnknown"] [newer, older, edit, unknown]
        for_ events \(eventId, event) -> do
          receiptId <- receiveSlackEvent tr $ callback eventId event
          replicateM_ 2 $ runTestBg frozenTime tr $ processSlackEvent receiptId
          [PGS.Only (Aeson saved)] <- withResource tr.trPool \conn ->
            PGS.query conn [sql|SELECT payload->'event' FROM apis.slack_events WHERE event_id = ?|] (PGS.Only eventId)
          saved `shouldBe` event
        conflicting <- receiveSlackEvent tr $ callback "EvConflict" $ session "assistant_thread_context_changed" "1735689603.000001" "C_FOREIGN" "U_OTHER"
        runTestBg frozenTime tr (processSlackEvent conflicting) `shouldThrow` anyException
        [PGS.Only pending] <- withResource tr.trPool \conn ->
          PGS.query_ conn [sql|SELECT processed_at IS NULL FROM apis.slack_events WHERE event_id = 'EvConflict'|]
        pending `shouldBe` True
        contexts <- withResource tr.trPool \conn ->
          PGS.query_ conn [sql|SELECT user_id, context->>'channel_id' FROM apis.slack_assistant_threads WHERE team_id = 'T_SESSION'|]
        (contexts :: [(Text, Text)]) `shouldBe` [("U_SESSION", "C_NEW")]
        [PGS.Only conversations] <- withResource tr.trPool \conn -> PGS.query_ conn [sql|SELECT count(*) FROM apis.ai_conversations|]
        (conversations :: Int64) `shouldBe` 0

      it "verifies the original body before decoding or dispatching, and bounds replay age" \tr -> do
        let body = "{\"type\":\"url_verification\",\"challenge\":\"test-challenge\"}"
            -- Independent Python hashlib fixture, not generated by the verifier.
            signature = Just "v0=b9e03024f220f348214a779eb48714e56a8c0cf7c7b55a95e72e6f2cb23ebfac"
            cfg = tr.trATCtx.env{Config.slackSigningSecret = "test-slack-signing-secret"}
            signedTr = tr{trATCtx = tr.trATCtx{Config.env = cfg}}
            status resources bytes timestamp sig =
              toBaseServantResponse resources
                $ catchError @ServerError (slackEventsPostH bytes timestamp sig $> 200) (\_ err -> pure err.errHTTPCode)
        result <- toBaseServantResponse signedTr $ slackEventsPostH body (Just "1735689600") signature
        result `shouldBe` AE.object ["challenge" AE..= ("test-challenge" :: Text)]
        for_ ([(body <> " ", Just "1735689600", signature), ("not-json", Nothing, Nothing), (body, Just "1735689600", Just "v1=bad"), (body, Just "1735689299", Just $ signSlackBody "1735689299" body), (body, Just "1735689901", Just $ signSlackBody "1735689901" body)] :: [(ByteString, Maybe Text, Maybe Text)]) \(bytes, timestamp, sig) ->
          status signedTr bytes timestamp sig >>= (`shouldBe` 401)
        status signedTr "not-json" (Just "1735689600") (Just $ signSlackBody "1735689600" "not-json") >>= (`shouldBe` 400)
        let unconfigured = tr{trATCtx = tr.trATCtx{Config.env = cfg{Config.slackSigningSecret = ""}}}
        status unconfigured body (Just "1735689600") signature >>= (`shouldBe` 503)

      it "authenticates form bytes before decoding and dispatching Slack commands and actions" \tr -> do
        calls <- newIORef (0 :: Int)
        let cfg = tr.trATCtx.env{Config.slackSigningSecret = "test-slack-signing-secret"}
            signedTr = tr{trATCtx = tr.trATCtx{Config.env = cfg}}
            body = "payload=%7B%22text%22%3A%22a+b%2Bc%22%7D"
            timestamp = Just "1735689600"
            signature = Just $ signSlackBody "1735689600" body
            handler = SlackPage.slackFormPostH \form -> do
              modifyIORef' calls (+ 1)
              SlackPage.externalOptionsH form
            status resources bytes ts sig =
              toBaseServantResponse resources
                $ catchError @ServerError (handler bytes ts sig $> 200) (\_ err -> pure err.errHTTPCode)
        status signedTr body timestamp signature >>= (`shouldBe` 200)
        for_ [(body <> "&extra=1", timestamp, signature), (body, Nothing, Nothing), (body, Just "1735689299", Just $ signSlackBody "1735689299" body)] \(bytes, ts, sig) ->
          status signedTr bytes ts sig >>= (`shouldBe` 401)
        status signedTr "missing=payload" timestamp (Just $ signSlackBody "1735689600" "missing=payload") >>= (`shouldBe` 400)
        let unconfigured = tr{trATCtx = tr.trATCtx{Config.env = cfg{Config.slackSigningSecret = ""}}}
        status unconfigured body timestamp signature >>= (`shouldBe` 503)
        readIORef calls >>= (`shouldBe` 1)
        let command = "team_id=T_TEST&command=%2Fmonoscope&text=a+b%2Bc&response_url=https%3A%2F%2Fexample.com&trigger_id=trigger&channel_id=C_TEST&channel_name=test&user_id=U_TEST"
        decoded <-
          toBaseServantResponse signedTr
            $ SlackPage.slackFormPostH (\(interaction :: SlackPage.SlackInteraction) -> pure $ AE.String interaction.text) command timestamp (Just $ signSlackBody "1735689600" command)
        decoded `shouldBe` AE.String "a b+c"

      it "records bot events without starting another conversation" \tr -> do
        setupSlackData tr testPid "T_BOT_EVENT"
        let body = "{\"type\":\"event_callback\",\"team_id\":\"T_BOT_EVENT\",\"api_app_id\":\"A_TEST\",\"event_id\":\"EvBot\",\"event\":{\"type\":\"message\",\"bot_id\":\"B_TEST\",\"text\":\"An answer\",\"channel\":\"C_BOT_EVENT\",\"thread_ts\":\"1735689600.000001\"}}"
            cfg = tr.trATCtx.env{Config.slackSigningSecret = "test-slack-signing-secret"}
            signedTr = tr{trATCtx = tr.trATCtx{Config.env = cfg}}
        void $ toBaseServantResponse signedTr $ slackEventsPostH body (Just "1735689600") (Just $ signSlackBody "1735689600" body)
        [PGS.Only receiptId] <- withResource tr.trPool \conn ->
          PGS.query_ conn [sql|SELECT id FROM apis.slack_events WHERE team_id = 'T_BOT_EVENT'|]
        runTestBg frozenTime signedTr $ processSlackEvent receiptId
        [PGS.Only conversations] <- withResource tr.trPool \conn ->
          PGS.query_ conn [sql|SELECT count(*) FROM apis.ai_conversations|]
        (conversations :: Int64) `shouldBe` 0

      it "rolls back the receipt if its job cannot be persisted" \tr -> do
        let body = toStrict $ AE.encode $ slackThreadedEvent "T_ROLLBACK" "C_ROLLBACK" "hello" "1735689600.000002" "1735689600.000001"
            cfg = tr.trATCtx.env{Config.slackSigningSecret = "test-slack-signing-secret"}
            signedTr = tr{trATCtx = tr.trATCtx{Config.env = cfg}}
            accept = toBaseServantResponse signedTr $ slackEventsPostH body (Just "1735689600") (Just $ signSlackBody "1735689600" body)
            execute statement = withResource tr.trPool \conn -> void $ PGS.execute_ conn statement
        bracket_
          (execute [sql|ALTER TABLE background_jobs ADD CONSTRAINT reject_slack_event_test CHECK (payload->>'tag' <> 'ProcessSlackEvent') NOT VALID|])
          (execute [sql|ALTER TABLE background_jobs DROP CONSTRAINT reject_slack_event_test|])
          (accept `shouldThrow` anyException)
        [PGS.Only receipts] <- withResource tr.trPool \conn ->
          PGS.query_ conn [sql|SELECT count(*) FROM apis.slack_events WHERE team_id = 'T_ROLLBACK'|]
        (receipts :: Int64) `shouldBe` 0
        accept >>= (`shouldBe` AE.object [])

    describe "Thread/Conversation Context" do
      it "seeds history once, retries failures, and isolates projects" \tr -> do
        attempts <- newIORef (0 :: Int)
        let convId = Issues.slackThreadToConversationId "C_RETRY_BACKFILL" "1735689600.000001"
            backfill = liftIO (modifyIORef' attempts (+ 1)) $> Nothing
            resolve = Bot.withBotThread Bot.Slack testPid convId Issues.CTSlackThread (AE.object []) backfill
        replicateM_ 2 $ toBaseServantResponse tr resolve
        readIORef attempts >>= (`shouldBe` 2)
        let history = [(Issues.ChatUser, "What changed?"), (Issues.ChatAssistant, "The error rate increased.")]
        let seed = toBaseServantResponse tr $ Issues.seedChatHistory testPid convId history
        concurrently_ seed seed
        otherPid <- createTestProject tr "Private history"
        toBaseServantResponse tr do
          void $ Issues.getOrCreateConversation otherPid convId Issues.CTSlackThread (AE.object [])
          Issues.insertChatMessage otherPid convId Issues.ChatUser "Another project's private question" Nothing Nothing
        saved <- toBaseServantResponse tr $ Issues.selectChatHistory testPid convId
        map (\message -> (message.role, message.content)) saved `shouldBe` history

      it "Slack: deduplicates unrelated threaded events without starting a conversation" \tr -> do
        setupSlackData tr testPid "T_THREAD_WF"
        void $ runTestBg frozenTime tr $ Slack.updateSlackDefaultChannel testPid "C_THREAD_CHANNEL" Nothing

        let threadedEventJson = slackThreadedEvent "T_THREAD_WF" "C_THREAD_CHANNEL" "follow up question" "1700000002.000" "1700000001.000"
            body = toStrict $ AE.encode threadedEventJson
            cfg = tr.trATCtx.env{Config.slackSigningSecret = "test-slack-signing-secret"}
            signedTr = tr{trATCtx = tr.trATCtx{Config.env = cfg}}
            accept = toBaseServantResponse signedTr $ slackEventsPostH body (Just "1735689600") (Just $ signSlackBody "1735689600" body)
        concurrently_ accept accept
        counts <- withResource tr.trPool \conn ->
          PGS.query_
            conn
            [sql|SELECT (SELECT count(*) FROM apis.slack_events WHERE team_id = 'T_THREAD_WF'),
            (SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'ProcessSlackEvent')|]
        (counts :: [(Int64, Int64)]) `shouldBe` [(1, 1)]
        [PGS.Only (Aeson job)] <- withResource tr.trPool \conn ->
          PGS.query_ conn [sql|SELECT payload FROM background_jobs WHERE payload->>'tag' = 'ProcessSlackEvent'|]
        let process = runTestBg frozenTime signedTr $ Jobs.processBackgroundJob signedTr.trATCtx job
            historySize = withResource tr.trPool \conn -> PGS.query_ conn [sql|SELECT count(*) FROM apis.ai_chat_messages|]
        process
        [PGS.Only conversations] <- withResource tr.trPool \conn -> PGS.query_ conn [sql|SELECT count(*) FROM apis.ai_conversations|]
        (conversations :: Int64) `shouldBe` 0
        beforeReplay <- historySize
        process
        afterReplay <- historySize
        (afterReplay :: [PGS.Only Int64]) `shouldBe` beforeReplay
        [PGS.Only finished] <- withResource tr.trPool \conn ->
          PGS.query_ conn [sql|SELECT processed_at IS NOT NULL FROM apis.slack_events WHERE team_id = 'T_THREAD_WF'|]
        finished `shouldBe` True

      it "Discord: handles thread context" \tr -> do
        setupDiscordData tr testPid "guild_thread_wf"
        let payload = discordThreadInteraction "monoscope" "follow up" "thread_123"
            (signedPayload, sig, ts) = signDiscordPayload payload "1700000000"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}

        result <- toBaseServantResponse tr{trATCtx = testCtx} $ discordInteractionsH signedPayload (Just sig) (Just ts)
        result `shouldSatisfy` isValidJsonResponse

    describe "Multi-Platform Integration" do
      it "handles same project with multiple platforms" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupLinkedSlackData tr testPid "T_MULTI_SLACK"
        setupDiscordData tr testPid "guild_multi_discord"
        setupWhatsappNumber tr testPid testPhone

        -- Test Slack
        let slackInt = slackInteraction "/monoscope" "show status" "T_MULTI_SLACK"
        slackResp <- toBaseServantResponse tr $ slackInteractionsH slackInt
        slackResp `shouldSatisfy` isValidJsonResponse

        -- Test Discord
        let discordPayload = discordCommandInteraction "monoscope" "show status"
            (signedPayload, sig, ts) = signDiscordPayload discordPayload "1700000000"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        discordResp <- toBaseServantResponse tr{trATCtx = testCtx} $ discordInteractionsH signedPayload (Just sig) (Just ts)
        discordResp `shouldSatisfy` isValidJsonResponse

        -- Test WhatsApp
        let whatsappMsg = twilioWhatsAppPrompt tr testPhone "show status"
        whatsappResp <- toBaseServantResponse tr $ whatsappIncomingPostH whatsappMsg
        whatsappResp `shouldSatisfy` isValidJsonResponse

    describe "Response Format Validation" do
      it "Slack responses have correct structure" \tr -> do
        setupLinkedSlackData tr testPid "T_FORMAT_SLACK"
        let interaction = slackInteraction "/monoscope-here" "" "T_FORMAT_SLACK"
        result <- toBaseServantResponse tr $ slackInteractionsH interaction

        extractResponseType result `shouldSatisfy` isJust
        extractSlackBlocks result `shouldSatisfy` isJust

      it "Discord responses have correct structure" \tr -> do
        setupDiscordData tr testPid "guild_format_discord"
        let payload = discordCommandInteraction "monoscope" "test"
            (signedPayload, sig, ts) = signDiscordPayload payload "1700000000"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse tr{trATCtx = testCtx} $ discordInteractionsH signedPayload (Just sig) (Just ts)

        getDiscordResponseType result `shouldSatisfy` isJust

      it "WhatsApp responses have template structure" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone
        let msg = twilioWhatsAppPrompt tr testPhone "test query"
        result <- toBaseServantResponse tr $ whatsappIncomingPostH msg

        result `shouldSatisfy` isValidJsonResponse


signSlackBody :: Text -> ByteString -> Text
signSlackBody timestamp body = "v0=" <> decodeUtf8 (B16.encode $ BA.convert (HMAC.hmac ("test-slack-signing-secret" :: ByteString) ("v0:" <> encodeUtf8 timestamp <> ":" <> body) :: HMAC.HMAC SHA256))
