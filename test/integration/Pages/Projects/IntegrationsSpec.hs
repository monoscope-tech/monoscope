module Pages.Projects.IntegrationsSpec (spec) where

import Data.Aeson qualified as AE
import Data.Effectful.Notify (Notification (..))
import Data.Effectful.Notify qualified as Notify
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL qualified as DB
import Models.Projects.Projects qualified as Projects
import Pages.Bots.BotTestHelpers (setupSlackData, testPid)
import Pages.Settings (TestForm (..))
import Pages.Settings qualified as Integrations
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


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
        tests <- runQueryEffect tr $
          DB.query [sql|SELECT status, issue_type, channel FROM apis.notification_test_history WHERE project_id = ? AND channel = 'slack' ORDER BY created_at DESC LIMIT 1|] (Only testPid)
        tests `shouldSatisfy` elem (("sent" :: Text, "runtime_exception" :: Text, "slack" :: Text))

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
        tests <- runQueryEffect tr $
          DB.query [sql|SELECT status FROM apis.notification_test_history WHERE project_id = ? AND channel = 'discord' ORDER BY created_at DESC LIMIT 1|] (Only testPid)
        tests `shouldBe` [Only ("sent" :: Text)]

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
  teamId <- runQueryEffect tr $ DB.query_ [sql|SELECT gen_random_uuid()|] <&> fromMaybe UUID.nil . listToMaybe . fmap fromOnly
  runTestBg tr $ void $ DB.execute
    [sql|INSERT INTO projects.teams (id, project_id, name, description, handle, members, slack_channels, discord_channels, phone_numbers, pagerduty_services)
         VALUES (?, ?, 'Test Team', 'Test team', 'test-team', '{}', ?, '{}', '{}', '{}')
         ON CONFLICT (id) DO UPDATE SET slack_channels = EXCLUDED.slack_channels|]
    (teamId, pid, V.fromList slackChannels)
  pure teamId


createTestTeamWithPagerduty :: TestResources -> Projects.ProjectId -> [Text] -> IO UUID.UUID
createTestTeamWithPagerduty tr pid pagerdutyServices = do
  teamId <- runQueryEffect tr $ DB.query_ [sql|SELECT gen_random_uuid()|] <&> fromMaybe UUID.nil . listToMaybe . fmap fromOnly
  runTestBg tr $ void $ DB.execute
    [sql|INSERT INTO projects.teams (id, project_id, name, description, handle, members, slack_channels, discord_channels, phone_numbers, pagerduty_services)
         VALUES (?, ?, 'PD Test Team', 'PagerDuty test team', 'pd-test-team', '{}', '{}', '{}', '{}', ?)
         ON CONFLICT (id) DO UPDATE SET pagerduty_services = EXCLUDED.pagerduty_services|]
    (teamId, pid, V.fromList pagerdutyServices)
  pure teamId


setupWhatsappNumber :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupWhatsappNumber tr pid number = void $ withResource tr.trPool \conn ->
  PGS.execute conn [sql|UPDATE projects.projects SET whatsapp_numbers = ARRAY[?] WHERE id = ?|] (number, pid)


setupPagerdutyData :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupPagerdutyData tr pid integrationKey = runTestBg tr $ void $ DB.execute
  [sql|INSERT INTO apis.pagerduty (project_id, integration_key) VALUES (?, ?) ON CONFLICT (project_id) DO UPDATE SET integration_key = EXCLUDED.integration_key|]
  (pid, integrationKey)


setupDiscordDataWithChannel :: TestResources -> Projects.ProjectId -> Text -> Text -> IO ()
setupDiscordDataWithChannel tr pid guildId channelId = runTestBg tr $ void $ DB.execute
  [sql|INSERT INTO apis.discord (project_id, guild_id, notifs_channel_id) VALUES (?, ?, ?)
       ON CONFLICT (project_id) DO UPDATE SET guild_id = EXCLUDED.guild_id, notifs_channel_id = EXCLUDED.notifs_channel_id|]
  (pid, guildId, channelId)


clearTestHistory :: TestResources -> Projects.ProjectId -> IO ()
clearTestHistory tr pid = runTestBg tr $ void $ DB.execute
  [sql|DELETE FROM apis.notification_test_history WHERE project_id = ?|] (Only pid)


payloadText :: AE.Value -> Text
payloadText = decodeUtf8 . toStrict . AE.encode
