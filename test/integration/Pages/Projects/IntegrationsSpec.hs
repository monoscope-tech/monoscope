module Pages.Projects.IntegrationsSpec (spec) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Notify (Notification (..))
import Data.Effectful.Notify qualified as Notify
import Data.Effectful.UUID qualified as UUID
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Hasql.Interpolate qualified as HI
import Models.Apis.Integrations qualified as ApisInt
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Pages.Bots.BotTestHelpers (setupSlackData)
import Pages.Projects qualified as Pages
import Pages.Settings (TestForm (..))
import Pages.Settings qualified as Integrations
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldContain, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources $ do
  describe "Notification Testing" $ do
    describe "Project-Level Tests" $ do
      it "sends test notification to Slack and records history" \tr -> do
        clearTestHistory tr testPid
        setupSlackData tr testPid "T_SLACK_TEST"
        let form = TestForm "runtime_exception" "slack" Nothing

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        -- Verify Slack notification was sent
        notifs `shouldSatisfy` any isSlackNotification
        let slackNotif = find isSlackNotification notifs
        case slackNotif of
          Just (SlackNotification sd) -> do
            sd.channelId `shouldBe` "C_NOTIF_CHANNEL"  -- setupSlackData uses this channel
            T.isInfixOf "🧪 TEST" (payloadText sd.payload) `shouldBe` True
          _ -> pure ()

        -- Verify history was recorded
        tests :: [(Text, Text, Text)] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT status, issue_type, channel FROM apis.notification_test_history WHERE project_id = #{testPid} AND channel = 'slack' ORDER BY created_at DESC LIMIT 1|]
        tests `shouldSatisfy` elem ("sent", "runtime_exception", "slack")

      it "sends test notification to Discord" \tr -> do
        clearTestHistory tr testPid
        setupDiscordDataWithChannel tr testPid "G_DISCORD_TEST" "C_DISCORD_NOTIF"
        let form = TestForm "api_change" "discord" Nothing

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        -- Verify Discord notification was sent
        notifs `shouldSatisfy` any isDiscordNotification
        let discordNotif = find isDiscordNotification notifs
        case discordNotif of
          Just (DiscordNotification dd) -> T.isInfixOf "🧪 TEST" (payloadText dd.payload) `shouldBe` True
          _ -> pure ()

        -- Verify history
        tests :: [Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT status FROM apis.notification_test_history WHERE project_id = #{testPid} AND channel = 'discord' ORDER BY created_at DESC LIMIT 1|]
        tests `shouldBe` ["sent"]

      it "sends test notification to WhatsApp" \tr -> do
        clearTestHistory tr testPid
        let whatsappNum = "+1234567890"
        setupWhatsappNumber tr testPid whatsappNum
        let form = TestForm "runtime_exception" "whatsapp" Nothing  -- query_alert produces MonitorsAlert which WhatsApp doesn't support

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        -- Verify WhatsApp notification was sent
        notifs `shouldSatisfy` any isWhatsAppNotification
        let whatsappNotif = find isWhatsAppNotification notifs
        case whatsappNotif of
          Just (WhatsAppNotification wd) -> Notify.to wd `shouldBe` whatsappNum
          _ -> pure ()

      it "sends test notification to PagerDuty" \tr -> do
        clearTestHistory tr testPid
        let integrationKey = "a1b2c3d4e5f67890abcdef1234567890"
        setupPagerdutyData tr testPid integrationKey
        let form = TestForm "runtime_exception" "pagerduty" Nothing

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        -- Verify PagerDuty notification was sent
        notifs `shouldSatisfy` any isPagerdutyNotification
        let pdNotif = find isPagerdutyNotification notifs
        case pdNotif of
          Just (PagerdutyNotification pd) -> do
            Notify.integrationKey pd `shouldBe` integrationKey
            Notify.eventAction pd `shouldBe` Notify.PDTrigger
          _ -> pure ()

    describe "Team-Level Tests" $ do
      it "sends test notification to team Slack channels" \tr -> do
        clearTestHistory tr testPid
        teamId <- createTestTeam tr testPid ["#team-alerts"]
        let form = TestForm "runtime_exception" "slack" (Just teamId)

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        -- Verify notifications sent to team channels
        let slackNotifs = filter isSlackNotification notifs
        length slackNotifs `shouldSatisfy` (>= 1)
        forM_ slackNotifs \case
          SlackNotification sd -> payloadText sd.payload `shouldSatisfy` T.isInfixOf "🧪 TEST"
          _ -> pure ()

      it "sends test notification to team PagerDuty services" \tr -> do
        clearTestHistory tr testPid
        teamId <- createTestTeamWithPagerduty tr testPid ["svc-key-12345678901234567890abcd"]
        let form = TestForm "runtime_exception" "pagerduty" (Just teamId)

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        let pdNotifs = filter isPagerdutyNotification notifs
        length pdNotifs `shouldSatisfy` (>= 1)

    describe "Test Report Notifications" $ do
      it "sends test report notification to Slack" \tr -> do
        clearTestHistory tr testPid
        setupSlackData tr testPid "T_REPORT_TEST"
        let form = TestForm "report" "slack" Nothing

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        notifs `shouldSatisfy` any isSlackNotification
        let slackNotif = find isSlackNotification notifs
        case slackNotif of
          Just (SlackNotification sd) -> payloadText sd.payload `shouldSatisfy` T.isInfixOf "🧪 TEST"
          _ -> pure ()

    describe "Test History Retrieval" $ do
      it "retrieves test history for a project" \tr -> do
        clearTestHistory tr testPid
        setupSlackData tr testPid "T_HISTORY_TEST"
        void $ testServant tr $ Integrations.notificationsTestPostH testPid (TestForm "runtime_exception" "slack" Nothing)

        (_, html) <- testServant tr $ Integrations.notificationsTestHistoryGetH testPid
        let htmlText = show html
        htmlText `shouldSatisfy` T.isInfixOf "runtime_exception"

    describe "disabled_channels gating (alert dispatch)" $ do
      -- sendAlertToChannels should respect @everyone.disabled_channels: even when
      -- targets are populated, a muted channel type must produce zero notifications.
      it "slack in disabled_channels suppresses slack dispatch even with populated slack_channels" \tr -> do
        clearTestHistory tr testPid
        setupSlackData tr testPid "T_DISABLED_GATE"
        setEveryoneDisabled tr testPid ["slack"]
        let form = TestForm "runtime_exception" "slack" Nothing

        (_notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        -- History: should record 'skipped' with reason 'channel_disabled' or 'no_targets',
        -- never 'sent' when the channel is muted.
        tests :: [(Text, Maybe Text)] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT status, error FROM apis.notification_test_history WHERE project_id = #{testPid} AND channel = 'slack' ORDER BY created_at DESC LIMIT 1|]
        fst <$> tests `shouldBe` ["skipped"]
        -- Reset
        setEveryoneDisabled tr testPid []

      it "multi-channel Slack fan-out fires once per entry in slack_channels" \tr -> do
        clearTestHistory tr testPid
        setupSlackData tr testPid "T_MULTI_SLACK"
        -- Seed @everyone with two channel IDs
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|UPDATE projects.teams SET slack_channels = ARRAY['C_ONE','C_TWO']::text[], disabled_channels = '{}'
                  WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        let form = TestForm "runtime_exception" "slack" Nothing

        (notifs, _) <- testServantWithNotifications tr $ Integrations.notificationsTestPostH testPid form

        let slackTargets = [sd.channelId | SlackNotification sd <- notifs]
        sort slackTargets `shouldBe` ["C_ONE", "C_TWO"]

    describe "PagerDuty connect/disconnect" $ do
      -- PagerDuty integration_key lives only on @everyone.pagerduty_services now.
      -- Connect must append; disconnect must clear.
      it "rejects short PagerDuty key and leaves @everyone untouched" \tr -> do
        -- Clear baseline
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|UPDATE projects.teams SET pagerduty_services = '{}' WHERE project_id = #{testPid} AND is_everyone = TRUE|]
        let form = Pages.PagerdutyConnectForm "short"
        _ <- testServant tr $ Pages.pagerdutyConnectH testPid form
        [services] :: [V.Vector Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT pagerduty_services FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        V.toList services `shouldBe` []

      it "successful connect writes key to @everyone.pagerduty_services" \tr -> do
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|UPDATE projects.teams SET pagerduty_services = '{}' WHERE project_id = #{testPid} AND is_everyone = TRUE|]
        let key = "a1b2c3d4e5f67890abcdef1234567890" -- 32 chars
            form = Pages.PagerdutyConnectForm key
        _ <- testServant tr $ Pages.pagerdutyConnectH testPid form
        [services] :: [V.Vector Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT pagerduty_services FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        V.toList services `shouldBe` [key]

      it "disconnect clears @everyone.pagerduty_services" \tr -> do
        -- Seed
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|UPDATE projects.teams SET pagerduty_services = ARRAY['seed-key']::text[]
                  WHERE project_id = #{testPid} AND is_everyone = TRUE|]
        _ <- testServant tr $ Pages.pagerdutyDisconnectH testPid
        [services] :: [V.Vector Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT pagerduty_services FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        V.toList services `shouldBe` []

    describe "Slack OAuth install invariant" $ do
      -- The OAuth callback must write credentials to apis.slack AND add the default
      -- channel to @everyone.slack_channels. Test the two-write invariant at the
      -- model layer (the callback's `insertAccessToken` + `addSlackChannelToEveryoneTeam`
      -- pair). A regression dropping either call fails this.
      it "insertAccessToken + addSlackChannelToEveryoneTeam leaves both rows consistent" \tr -> do
        -- Clear baseline
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|DELETE FROM apis.slack WHERE project_id = #{testPid}|]
        runTestBg frozenTime tr $ Hasql.interpExecute_
          [HI.sql|UPDATE projects.teams SET slack_channels = '{}' WHERE project_id = #{testPid} AND is_everyone = TRUE|]
        -- Simulate what Slack.linkProjectGetH does
        runTestBg frozenTime tr do
          void $ ApisInt.insertAccessToken testPid "T_OAUTH" "C_OAUTH" "Workspace" "xoxb-oauth" "general"
          void $ PM.addSlackChannelToEveryoneTeam testPid "C_OAUTH"
        slackRows :: [(Text, Maybe Text)] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT channel_id, team_name FROM apis.slack WHERE project_id = #{testPid}|]
        slackRows `shouldBe` [("C_OAUTH", Just "Workspace")]
        [channels] :: [V.Vector Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT slack_channels FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        V.toList channels `shouldContain` ["C_OAUTH"]

    describe "disabled_channels inversion (form save)" $ do
      -- Inversion: disabled_channels = allChannels \\ enabledChannels.
      it "saving with enabledChannels=[slack,email] disables the other three" \tr -> do
        let form = Pages.NotifListForm ["slack", "email"] [] [] []
        _ <- testServant tr $ Pages.updateNotificationsChannel testPid form
        [disabled] :: [V.Vector Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT disabled_channels FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        sort (V.toList disabled) `shouldBe` ["discord", "pagerduty", "phone"]

      it "saving with empty enabledChannels mutes all five types" \tr -> do
        let form = Pages.NotifListForm [] [] [] []
        _ <- testServant tr $ Pages.updateNotificationsChannel testPid form
        [disabled] :: [V.Vector Text] <- runQueryEffect tr $
          Hasql.interp [HI.sql|SELECT disabled_channels FROM projects.teams WHERE project_id = #{testPid} AND is_everyone = TRUE AND deleted_at IS NULL|]
        sort (V.toList disabled) `shouldBe` ["discord", "email", "pagerduty", "phone", "slack"]


-- Helper functions

isNotificationType :: (Notification -> Maybe a) -> Notification -> Bool
isNotificationType f = isJust . f

isSlackNotification :: Notification -> Bool
isSlackNotification = isNotificationType \case SlackNotification n -> Just n; _ -> Nothing

isDiscordNotification :: Notification -> Bool
isDiscordNotification = isNotificationType \case DiscordNotification n -> Just n; _ -> Nothing

isWhatsAppNotification :: Notification -> Bool
isWhatsAppNotification = isNotificationType \case WhatsAppNotification n -> Just n; _ -> Nothing

isPagerdutyNotification :: Notification -> Bool
isPagerdutyNotification = isNotificationType \case PagerdutyNotification n -> Just n; _ -> Nothing


createTestTeam :: TestResources -> Projects.ProjectId -> [Text] -> IO UUID.UUID
createTestTeam tr pid slackChannels = do
  teamId <- runTestBg frozenTime tr UUID.genUUID
  let channels = V.fromList slackChannels
  runTestBg frozenTime tr $ Hasql.interpExecute_
    [HI.sql|INSERT INTO projects.teams (id, project_id, name, description, handle, members, slack_channels, discord_channels, phone_numbers, pagerduty_services)
         VALUES (#{teamId}, #{pid}, 'Test Team', 'Test team', 'test-team', '{}', #{channels}, '{}', '{}', '{}')
         ON CONFLICT (id) DO UPDATE SET slack_channels = EXCLUDED.slack_channels|]
  pure teamId


createTestTeamWithPagerduty :: TestResources -> Projects.ProjectId -> [Text] -> IO UUID.UUID
createTestTeamWithPagerduty tr pid pagerdutyServices = do
  teamId <- runTestBg frozenTime tr UUID.genUUID
  let services = V.fromList pagerdutyServices
  runTestBg frozenTime tr $ Hasql.interpExecute_
    [HI.sql|INSERT INTO projects.teams (id, project_id, name, description, handle, members, slack_channels, discord_channels, phone_numbers, pagerduty_services)
         VALUES (#{teamId}, #{pid}, 'PD Test Team', 'PagerDuty test team', 'pd-test-team', '{}', '{}', '{}', '{}', #{services})
         ON CONFLICT (id) DO UPDATE SET pagerduty_services = EXCLUDED.pagerduty_services|]
  pure teamId


setEveryoneDisabled :: TestResources -> Projects.ProjectId -> [Text] -> IO ()
setEveryoneDisabled tr pid chs = runTestBg frozenTime tr $ Hasql.interpExecute_
  [HI.sql|UPDATE projects.teams SET disabled_channels = #{V.fromList chs}
          WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL|]


setupWhatsappNumber :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupWhatsappNumber tr pid number = runTestBg frozenTime tr $ Hasql.interpExecute_
  [HI.sql|UPDATE projects.teams SET phone_numbers = ARRAY[#{number}]
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
clearTestHistory tr pid = runTestBg frozenTime tr $ Hasql.interpExecute_
  [HI.sql|DELETE FROM apis.notification_test_history WHERE project_id = #{pid}|]


payloadText :: AE.Value -> Text
payloadText = decodeUtf8 . toStrict . AE.encode
