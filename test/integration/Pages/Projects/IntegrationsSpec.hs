module Pages.Projects.IntegrationsSpec (spec) where

import Control.Lens ((^?), _head, to)
import Data.Aeson qualified as AE
import Data.Effectful.Notify (Notification (..))
import Data.Effectful.Notify qualified as Notify
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pages.Bots.BotTestHelpers (setupSlackData, setupDiscordData, testPid, assertJsonGolden)
import Pages.Integrations (TestForm (..))
import Pages.Integrations qualified as Integrations
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources $ do
  describe "Notification Testing" $ do
    describe "Project-Level Tests" $ do
      it "sends test notification to Slack and records history" \tr -> do
        setupSlackData tr testPid "T_SLACK_TEST"
        let form = TestForm "runtime_exception" "slack" Nothing

        (notifs, _) <- runTestBackgroundWithNotifications tr.trLogger tr.trATCtx $
          void $ testServant tr $ Integrations.notificationsTestPostH testPid form

        -- Verify Slack notification was sent with test prefix
        notifs `shouldSatisfy` any isSlackNotification
        let slackNotif = find isSlackNotification notifs
        whenJust slackNotif \(SlackNotification sd) -> do
          sd.channelId `shouldBe` "C0123ABCDEF"
          T.isInfixOf "🧪 TEST" (show sd.payload) `shouldBe` True

        -- Verify history was recorded
        tests <- runQueryEffect tr $
          DB.query [sql|SELECT status, issue_type, channel FROM apis.notification_test_history WHERE project_id = ? AND channel = 'slack'|] (Only testPid)
        length tests `shouldBe` 1
        tests `shouldSatisfy` elem (("sent" :: Text, "runtime_exception" :: Text, "slack" :: Text))

      it "sends test notification to Discord" \tr -> do
        setupDiscordData tr testPid "G_DISCORD_TEST"
        let form = TestForm "api_change" "discord" Nothing

        (notifs, _) <- runTestBackgroundWithNotifications tr.trLogger tr.trATCtx $
          void $ testServant tr $ Integrations.notificationsTestPostH testPid form

        -- Verify Discord notification was sent
        notifs `shouldSatisfy` any isDiscordNotification
        let discordNotif = find isDiscordNotification notifs
        whenJust discordNotif \(DiscordNotification dd) ->
          T.isInfixOf "🧪 TEST" (show dd.payload) `shouldBe` True

        -- Verify history
        tests <- runQueryEffect tr $
          DB.query [sql|SELECT status FROM apis.notification_test_history WHERE project_id = ? AND channel = 'discord'|] (Only testPid)
        tests `shouldBe` [Only ("sent" :: Text)]

      it "sends test notification to WhatsApp" \tr -> do
        let whatsappNum = "+1234567890"
        setupWhatsappNumber tr testPid whatsappNum
        let form = TestForm "query_alert" "whatsapp" Nothing

        (notifs, _) <- runTestBackgroundWithNotifications tr.trLogger tr.trATCtx $
          void $ testServant tr $ Integrations.notificationsTestPostH testPid form

        -- Verify WhatsApp notification was sent
        notifs `shouldSatisfy` any isWhatsAppNotification
        let whatsappNotif = find isWhatsAppNotification notifs
        whenJust whatsappNotif \(WhatsAppNotification wd) ->
          Notify.to wd `shouldBe` whatsappNum

    describe "Team-Level Tests" $ do
      it "sends test notification to team Slack channels" \tr -> do
        teamId <- createTestTeam tr testPid ["#team-alerts", "#team-incidents"]
        let form = TestForm "runtime_exception" "slack" (Just teamId)

        (notifs, _) <- runTestBackgroundWithNotifications tr.trLogger tr.trATCtx $
          void $ testServant tr $ Integrations.notificationsTestPostH testPid form

        -- Verify notifications sent to team channels
        let slackNotifs = filter isSlackNotification notifs
        length slackNotifs `shouldSatisfy` (>= 1)
        forM_ slackNotifs \(SlackNotification sd) ->
          show sd.payload `shouldSatisfy` T.isInfixOf "🧪 TEST"

      it "sends test notification to team Discord channels" \tr -> do
        teamId <- createTestTeam tr testPid []
        updateTeamDiscordChannels tr teamId ["channel1", "channel2"]
        let form = TestForm "api_change" "discord" (Just teamId)

        (notifs, _) <- runTestBackgroundWithNotifications tr.trLogger tr.trATCtx $
          void $ testServant tr $ Integrations.notificationsTestPostH testPid form

        let discordNotifs = filter isDiscordNotification notifs
        length discordNotifs `shouldSatisfy` (>= 1)

    describe "Test Report Notifications" $ do
      it "sends test report notification to Slack" \tr -> do
        setupSlackData tr testPid "T_REPORT_TEST"
        let form = TestForm "report" "slack" Nothing

        (notifs, _) <- runTestBackgroundWithNotifications tr.trLogger tr.trATCtx $
          void $ testServant tr $ Integrations.notificationsTestPostH testPid form

        notifs `shouldSatisfy` any isSlackNotification
        let slackNotif = find isSlackNotification notifs
        whenJust slackNotif \(SlackNotification sd) ->
          show sd.payload `shouldSatisfy` T.isInfixOf "🧪 TEST"

    describe "Test History Retrieval" $ do
      it "retrieves test history for a project" \tr -> do
        setupSlackData tr testPid "T_HISTORY_TEST"

        -- Send multiple tests
        forM_ [TestForm "runtime_exception" "slack" Nothing, TestForm "api_change" "slack" Nothing] \form ->
          void $ runTestBackgroundWithNotifications tr.trLogger tr.trATCtx $
            void $ testServant tr $ Integrations.notificationsTestPostH testPid form

        -- Retrieve history
        (_, html) <- testServant tr $ Integrations.notificationsTestHistoryGetH testPid

        -- Verify HTML contains test entries
        let htmlText = show html
        htmlText `shouldSatisfy` T.isInfixOf "runtime_exception"
        htmlText `shouldSatisfy` T.isInfixOf "api_change"


-- Helper functions

isSlackNotification :: Notification -> Bool
isSlackNotification (SlackNotification _) = True
isSlackNotification _ = False


isDiscordNotification :: Notification -> Bool
isDiscordNotification (DiscordNotification _) = True
isDiscordNotification _ = False


isWhatsAppNotification :: Notification -> Bool
isWhatsAppNotification (WhatsAppNotification _) = True
isWhatsAppNotification _ = False


createTestTeam :: TestResources -> Projects.ProjectId -> [Text] -> IO UUID.UUID
createTestTeam tr pid slackChannels = do
  let teamId = UUID.nil
  runTestBg tr $ void $ DB.execute
    [sql|INSERT INTO projects.teams (id, project_id, name, description, handle, members, slack_channels, discord_channels, phone_numbers, pagerduty_services)
         VALUES (?, ?, 'Test Team', 'Test team for notification tests', 'test-team', '{}', ?, '{}', '{}', '{}')
         ON CONFLICT (id) DO UPDATE SET slack_channels = EXCLUDED.slack_channels|]
    (teamId, pid, slackChannels)
  pure teamId


updateTeamDiscordChannels :: TestResources -> UUID.UUID -> [Text] -> IO ()
updateTeamDiscordChannels tr teamId discordChannels = runTestBg tr $ void $ DB.execute
  [sql|UPDATE projects.teams SET discord_channels = ? WHERE id = ?|]
  (discordChannels, teamId)


setupWhatsappNumber :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupWhatsappNumber tr pid number = runTestBg tr $ void $ DB.execute
  [sql|UPDATE projects.projects SET whatsapp_numbers = ? WHERE id = ?|]
  (V.singleton number, pid)
