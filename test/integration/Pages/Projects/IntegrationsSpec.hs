module Pages.Projects.IntegrationsSpec (spec) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Notify (Notification (..))
import Data.Effectful.Notify qualified as Notify
import Data.Text qualified as T
import Data.Vector qualified as V
import Hasql.Interpolate qualified as HI
import Models.Apis.Integrations qualified as ApisInt
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Pages.Bots.BotTestHelpers (setupSlackData)
import Pages.Projects qualified as Pages
import Pages.Settings (TestChannel (..), TestForm (..), TestStatus (..))
import Pages.Settings qualified as Integrations
import Pkg.TestUtils
import Relude
import Servant qualified
import Test.Hspec (Spec, aroundAll, sequential, describe, it, shouldBe, shouldContain, shouldReturn, shouldSatisfy)


spec :: Spec
spec = sequential $ aroundAll withTestResources $ do
  describe "Notification Testing" $ do
    describe "Project-Level Tests" $ do
      it "routes each channel through the settings handler and records sent history" \tr -> do
        let whatsappNum = "+1234567890"
            pagerdutyKey = "a1b2c3d4e5f67890abcdef1234567890"
            cases :: [(TestChannel, Text, IO (), [Notification] -> IO ())]
            cases =
              [ (TCEmail, "runtime_exception", setupEmail tr testPid "alerts@example.com", \ns -> [(d.receiver, "[Test]" `T.isPrefixOf` d.subject) | EmailNotification d <- ns] `shouldBe` [("alerts@example.com", True)])
              , -- Also assert Slack would accept the shape: the test alert is the only
                -- Slack message many projects ever saw succeed, because it is rendered
                -- through the same builder but reached the default channel over the
                -- webhook transport, which never validates Block Kit.
                (TCSlack, "runtime_exception", setupSlackData tr testPid "T_SLACK_TEST", \ns -> [(d.channelId, T.isInfixOf "🧪 TEST" $ payloadText d.payload, slackPayloadViolations d.payload) | SlackNotification d <- ns] `shouldBe` [("C_NOTIF_CHANNEL", True, [])])
              , (TCDiscord, "api_change", setupDiscordDataWithChannel tr testPid "G_DISCORD_TEST" "C_DISCORD_NOTIF", \ns -> [T.isInfixOf "🧪 TEST" $ payloadText d.payload | DiscordNotification d <- ns] `shouldBe` [True])
              , (TCWhatsapp, "runtime_exception", setupWhatsappNumber tr testPid whatsappNum, \ns -> [Notify.to d | WhatsAppNotification d <- ns] `shouldBe` [whatsappNum])
              , (TCPagerduty, "runtime_exception", setupPagerdutyData tr testPid pagerdutyKey, \ns -> [(Notify.integrationKey d, Notify.eventAction d) | PagerdutyNotification d <- ns] `shouldBe` [(pagerdutyKey, Notify.PDTrigger)])
              ]

        for_ cases \(channel, issueType, setup, verify) -> do
          clearTestHistory tr testPid
          setup
          (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid (TestForm issueType channel Nothing)
          verify notifs
          history :: [(TestStatus, Text, TestChannel)] <- runQueryEffect tr $
            Hasql.interp [HI.sql|SELECT status, issue_type, channel FROM apis.notification_test_history WHERE project_id = #{testPid} AND channel = #{channel} ORDER BY created_at DESC LIMIT 1|]
          history `shouldBe` [(TSSent, issueType, channel)]

    describe "Team-Level Tests" $ do
      it "creates a team, routes Slack and PagerDuty to it, then deletes it" \tr -> do
        clearTestHistory tr testPid
        runTestBg frozenTime tr $ Hasql.interpExecute_ [HI.sql|DELETE FROM apis.discord WHERE project_id = #{testPid}|]
        setupSlackData tr testPid "T_TEAM_TEST"
        let currentUser = (Servant.getResponse tr.trSessAndHeader).user
            pagerdutyKey = "svc-key-12345678901234567890abcd"
            form =
              Pages.TeamForm
                { teamName = "Routing Team"
                , teamDescription = "Owns customer alerts"
                , teamHandle = "routing-team"
                , teamMembers = [currentUser.id]
                , notifEmails = V.empty
                , slackChannels = ["#team-alerts"]
                , discordChannels = V.empty
                , phoneNumbers = V.empty
                , pagerdutyServices = [pagerdutyKey]
                , teamId = Nothing
                }
        (_, Pages.ManageTeamsPostError "") <- testServant tr $ Pages.manageTeamPostH testPid form Nothing
        (_, Pages.ManageTeamsGet' (_, _, _, _, teams)) <- testServant tr $ Pages.manageTeamsGetH testPid (Just "")
        team <- maybe (fail "routing team was not listed after creation") pure $ V.find ((== "routing-team") . (.handle)) teams
        map (.memberEmail) (V.toList team.members) `shouldBe` ["test@monoscope.tech"]

        (slackNotifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid (TestForm "runtime_exception" TCSlack $ Just team.id)
        [(d.channelId, T.isInfixOf "🧪 TEST" $ payloadText d.payload) | SlackNotification d <- slackNotifs] `shouldBe` [("#team-alerts", True)]

        clearTestHistory tr testPid
        (pagerdutyNotifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid (TestForm "runtime_exception" TCPagerduty $ Just team.id)
        [Notify.integrationKey d | PagerdutyNotification d <- pagerdutyNotifs] `shouldBe` [pagerdutyKey]

        (_, Pages.ManageTeamsDelete) <- testServant tr $ Pages.manageTeamBulkActionH testPid "delete" (Pages.TBulkActionForm [team.id.unwrap]) Nothing
        (_, Pages.ManageTeamsGet' (_, _, _, _, afterDelete)) <- testServant tr $ Pages.manageTeamsGetH testPid (Just "")
        V.any ((== team.id) . (.id)) afterDelete `shouldBe` False
        (deletedNotifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid (TestForm "runtime_exception" TCSlack $ Just team.id)
        any isSlackNotification deletedNotifs `shouldBe` False

    describe "Test Report Notifications" $ do
      it "sends test report notification to Slack" \tr -> do
        clearTestHistory tr testPid
        setupSlackData tr testPid "T_REPORT_TEST"
        let form = TestForm "report" TCSlack Nothing

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        notifs `shouldSatisfy` any isSlackNotification
        [payloadText sd.payload | SlackNotification sd <- notifs] `shouldSatisfy` any (T.isInfixOf "Weekly report")

    describe "Test History Retrieval" $ do
      it "retrieves test history for a project" \tr -> do
        clearTestHistory tr testPid
        setupSlackData tr testPid "T_HISTORY_TEST"
        void $ testServant tr $ Integrations.notificationsTestPostH testPid (TestForm "runtime_exception" TCSlack Nothing)

        (_, html) <- testServant tr $ Integrations.notificationsTestHistoryGetH testPid
        let htmlText = show html
        htmlText `shouldSatisfy` T.isInfixOf "runtime_exception"

    describe "disabled_channels gating (alert dispatch)" $ do
      it "suppresses a disabled channel, then fans out once re-enabled" \tr -> do
        clearTestHistory tr testPid
        setupSlackData tr testPid "T_DISABLED_GATE"
        setEveryoneDisabled tr testPid ["slack"]
        (disabledNotifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid (TestForm "runtime_exception" TCSlack Nothing)
        any isSlackNotification disabledNotifs `shouldBe` False
        tests :: [(TestStatus, Maybe Text)] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT status, error FROM apis.notification_test_history WHERE project_id = #{testPid} AND channel = 'slack' ORDER BY created_at DESC LIMIT 1|]
        tests `shouldBe` [(TSSkipped, Just "channel_disabled")]

        clearTestHistory tr testPid
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|UPDATE projects.teams SET slack_channels = ARRAY['C_ONE','C_TWO']::text[], disabled_channels = '{}'
                  WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid (TestForm "runtime_exception" TCSlack Nothing)
        let slackTargets = [d.channelId | SlackNotification d <- notifs]
        sort slackTargets `shouldBe` ["C_ONE", "C_TWO"]

    describe "PagerDuty connect/disconnect" $ do
      it "rejects an invalid key, connects a valid service, and disconnects it" \tr -> do
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|UPDATE projects.teams SET pagerduty_services = '{}' WHERE project_id = #{testPid} AND is_everyone = TRUE|]
        let services = do
              [xs] :: [V.Vector Text] <- runQueryEffect tr $
                Hasql.interp [HI.sql|SELECT pagerduty_services FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
              pure $ V.toList xs
            key = "a1b2c3d4e5f67890abcdef1234567890"

        void $ testServant tr $ Pages.pagerdutyConnectH testPid (Pages.PagerdutyConnectForm "short")
        services `shouldReturn` []
        void $ testServant tr $ Pages.pagerdutyConnectH testPid (Pages.PagerdutyConnectForm key)
        services `shouldReturn` [key]
        void $ testServant tr $ Pages.pagerdutyDisconnectH testPid
        services `shouldReturn` []

    describe "Slack connect/disconnect invariant" $ do
      -- The OAuth callback must write credentials to apis.slack AND add the default
      -- channel to @everyone.slack_channels. Test the two-write invariant at the
      -- model layer (the callback's `insertAccessToken` + `addSlackChannelToEveryoneTeam`
      -- pair). A regression dropping either call fails this.
      it "connects credentials and the default channel, then disconnects both" \tr -> do
        -- Clear baseline
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|DELETE FROM apis.slack WHERE project_id = #{testPid}|]
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|UPDATE projects.teams SET slack_channels = '{}' WHERE project_id = #{testPid} AND is_everyone = TRUE|]
        -- Simulate what Slack.linkProjectGetH does
        runTestBg frozenTime tr do
          void $ ApisInt.insertAccessToken testPid "T_OAUTH" "C_OAUTH" "Workspace" "xoxb-oauth" "general" "https://hooks.slack.com/services/oauth"
          void $ PM.addSlackChannelToEveryoneTeam testPid "C_OAUTH"
        slackRows :: [(Text, Maybe Text)] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT channel_id, team_name FROM apis.slack WHERE project_id = #{testPid}|]
        slackRows `shouldBe` [("C_OAUTH", Just "Workspace")]
        [channels] :: [V.Vector Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT slack_channels FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        V.toList channels `shouldContain` ["C_OAUTH"]

        void $ testServant tr $ Pages.slackDisconnectH testPid
        slackAfter :: [Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT team_id FROM apis.slack WHERE project_id = #{testPid}|]
        slackAfter `shouldBe` []
        [channelsAfter] :: [V.Vector Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT slack_channels FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        channelsAfter `shouldBe` V.empty

    describe "disabled_channels inversion (form save)" $ do
      it "stores the complement of enabled channels, including mute-all" \tr -> do
        setupSlackData tr testPid "T_FORM_SAVE"
        let disabled = do
              [xs] :: [V.Vector Text] <- runQueryEffect tr $
                Hasql.interp [HI.sql|SELECT disabled_channels FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
              pure $ sort $ V.toList xs

        void $ testServant tr $ Pages.updateNotificationsChannel testPid (Pages.NotifListForm ["slack", "email"] [] [] [])
        disabled `shouldReturn` ["discord", "pagerduty", "phone"]
        void $ testServant tr $ Pages.updateNotificationsChannel testPid (Pages.NotifListForm [] [] [] [])
        disabled `shouldReturn` ["discord", "email", "pagerduty", "phone", "slack"]


-- Helper functions

isSlackNotification :: Notification -> Bool
isSlackNotification = \case SlackNotification{} -> True; _ -> False

setEveryoneDisabled :: TestResources -> Projects.ProjectId -> [Text] -> IO ()
setEveryoneDisabled tr pid chs = runTestBg frozenTime tr $ Hasql.interpExecute_
  [HI.sql|UPDATE projects.teams SET disabled_channels = #{V.fromList chs}
          WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL|]


setupWhatsappNumber :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupWhatsappNumber tr pid number = runTestBg frozenTime tr $ Hasql.interpExecute_
  [HI.sql|UPDATE projects.teams SET phone_numbers = ARRAY[#{number}]
          WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL|]


setupEmail :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupEmail tr pid email = runTestBg frozenTime tr $ Hasql.interpExecute_
  [HI.sql|UPDATE projects.teams SET notify_emails = ARRAY[#{email}]
          WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL|]


setupPagerdutyData :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupPagerdutyData tr pid integrationKey = runTestBg frozenTime tr $ Hasql.interpExecute_
  [HI.sql|UPDATE projects.teams SET pagerduty_services = ARRAY[#{integrationKey}]::text[]
          WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL|]


setupDiscordDataWithChannel :: TestResources -> Projects.ProjectId -> Text -> Text -> IO ()
setupDiscordDataWithChannel tr pid guildId channelId = runTestBg frozenTime tr do
  Hasql.interpExecute_
    [HI.sql|INSERT INTO apis.discord (project_id, guild_id) VALUES (#{pid}, #{guildId})
         ON CONFLICT (project_id) DO UPDATE SET guild_id = EXCLUDED.guild_id|]
  Hasql.interpExecute_
    [HI.sql|UPDATE projects.teams SET discord_channels = ARRAY[#{channelId}]::text[]
            WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL|]


clearTestHistory :: TestResources -> Projects.ProjectId -> IO ()
clearTestHistory tr pid = runTestBg frozenTime tr $ do
  Hasql.interpExecute_ [HI.sql|DELETE FROM apis.notification_test_history WHERE project_id = #{pid}|]
  Hasql.interpExecute_ [HI.sql|UPDATE projects.teams SET disabled_channels = '{}' WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL|]


payloadText :: AE.Value -> Text
payloadText = decodeUtf8 . toStrict . AE.encode
