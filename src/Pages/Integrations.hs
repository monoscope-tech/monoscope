module Pages.Integrations (
  TestForm (..),
  notificationsTestPostH,
  notificationsTestHistoryGetH,
  TestHistory (..),
  NotificationTestHistoryGet (..),
) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Error.Static (throwError)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL qualified as DB
import Effectful.Reader.Static (ask)
import Lucid
import Models.Apis.Integrations (DiscordData (..), PagerdutyData (..), SlackData (..), getDiscordDataByProjectId, getPagerdutyByProjectId, getProjectSlackData)
import Models.Apis.Issues (IssueType (..), parseIssueType)
import Models.Projects.ProjectMembers (Team (..), getTeamsById)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Mail (sendDiscordAlert, sendPagerdutyAlertToService, sendPostmarkEmail, sendSlackAlert, sendWhatsAppAlert)
import Pkg.SampleAlerts (sampleAlert, sampleReport)
import Relude hiding (Reader, State, ask)
import Servant (err400, err404, errBody)
import System.Config qualified as Config
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)


data TestForm = TestForm
  { issueType :: Text
  , channel :: Text
  , teamId :: Maybe UUID.UUID
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data TestHistory = TestHistory
  { id :: UUIDId "test_history"
  , projectId :: Projects.ProjectId
  , issueType :: Text
  , channel :: Text
  , target :: Text
  , status :: Text
  , error :: Maybe Text
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)


newtype NotificationTestHistoryGet = NotificationTestHistoryGet {tests :: [TestHistory]}
  deriving stock (Eq, Generic, Show)


instance ToHtml NotificationTestHistoryGet where
  toHtml (NotificationTestHistoryGet tests) = toHtmlRaw $ historyHtml_ tests
  toHtmlRaw (NotificationTestHistoryGet tests) = toHtmlRaw $ historyHtml_ tests


notificationsTestPostH :: Projects.ProjectId -> TestForm -> ATAuthCtx (RespHeaders (Html ()))
notificationsTestPostH pid TestForm{..} = do
  -- Rate limit: check if test was sent in last 60 seconds
  recentTests <- DB.query [sql|SELECT COUNT(*) FROM apis.notification_test_history WHERE project_id = ? AND created_at > now() - interval '60 seconds'|] (Only pid)
  when (maybe 0 fromOnly (listToMaybe recentTests) > (0 :: Int))
    $ throwError err400{errBody = "Rate limit: Please wait 60 seconds between test notifications"}

  project <- Projects.projectById pid >>= maybe (throwError err404) pure
  let alert = bool (sampleAlert (fromMaybe APIChange $ parseIssueType issueType) project.title) (sampleReport project.title) (issueType == "report")
      getTeam tid = listToMaybe <$> getTeamsById pid (V.singleton tid)

  Log.logTrace "Sending test notification" (channel, pid, issueType)
  appCtx <- ask @Config.AuthContext

  let projectUrl = "/p/" <> pid.toText
      sendTestEmail email = case issueType of
        "runtime_exception" -> sendPostmarkEmail email (Just ("runtime-errors", AE.object ["project_name" AE..= project.title, "errors_url" AE..= (appCtx.env.hostUrl <> projectUrl <> "/issues/"), "errors" AE..= AE.Array mempty])) Nothing
        "report" -> sendPostmarkEmail email Nothing (Just ("Test Report", "This is a test report notification for " <> project.title))
        _ -> sendPostmarkEmail email Nothing (Just ("Test Notification", "This is a test notification for " <> project.title))

  case (channel, teamId) of
    ("all", Just tid) ->
      getTeam tid >>= traverse_ \t -> do
        forM_ t.notify_emails sendTestEmail
        forM_ t.slack_channels \c -> sendSlackAlert alert pid project.title (Just c)
        forM_ t.discord_channels \c -> sendDiscordAlert alert pid project.title (Just c)
        when (not $ V.null t.phone_numbers) $ sendWhatsAppAlert alert pid project.title t.phone_numbers
        forM_ t.pagerduty_services \k -> sendPagerdutyAlertToService k alert project.title projectUrl
    ("email", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.notify_emails sendTestEmail
    ("email", Nothing) -> forM_ project.notifyEmails sendTestEmail
    ("slack", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.slack_channels \c -> sendSlackAlert alert pid project.title (Just c)
    ("slack", Nothing) -> getProjectSlackData pid >>= traverse_ \s -> sendSlackAlert alert pid project.title (Just s.channelId)
    ("discord", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.discord_channels \c -> sendDiscordAlert alert pid project.title (Just c)
    ("discord", Nothing) -> getDiscordDataByProjectId pid >>= traverse_ \d -> forM_ d.notifsChannelId \c -> sendDiscordAlert alert pid project.title (Just c)
    ("whatsapp", _) -> sendWhatsAppAlert alert pid project.title project.whatsappNumbers
    ("pagerduty", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.pagerduty_services \k -> sendPagerdutyAlertToService k alert project.title projectUrl
    ("pagerduty", Nothing) -> getPagerdutyByProjectId pid >>= traverse_ \pd -> sendPagerdutyAlertToService pd.integrationKey alert project.title projectUrl
    _ -> throwError err400{errBody = "Unknown notification channel"}

  void
    $ DB.execute
      [sql|INSERT INTO apis.notification_test_history (project_id, issue_type, channel, target, status, error) VALUES (?, ?, ?, ?, ?, ?)|]
      (pid, issueType, channel, "" :: Text, "sent" :: Text, Nothing :: Maybe Text)

  Log.logTrace "Test notification sent" (channel, pid)
  let msg = if channel == "all" then "Test notification sent to all channels!" else "Test " <> channel <> " notification sent!"
  addSuccessToast msg Nothing >> addRespHeaders mempty


notificationsTestHistoryGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders NotificationTestHistoryGet)
notificationsTestHistoryGetH pid = do
  tests <- DB.query [sql|SELECT * FROM apis.notification_test_history WHERE project_id = ? ORDER BY created_at DESC LIMIT 20|] (Only pid)
  addRespHeaders $ NotificationTestHistoryGet tests


historyHtml_ :: [TestHistory] -> Html ()
historyHtml_ tests = if null tests then emptyMsg else renderTable tests
  where
    emptyMsg = div_ [class_ "text-center py-12"] do
      div_ [class_ "text-textWeak mb-2"] "No test notifications sent yet"
      p_ [class_ "text-sm text-textWeaker"] "Test your integrations to see results here"
    renderTable ts = div_ [class_ "bg-bgRaised rounded-lg border border-strokeWeak overflow-hidden"] $ table_ [class_ "table table-sm w-full"] (thead_ [class_ "text-xs text-left text-textStrong font-semibold uppercase bg-fillWeaker border-b border-strokeWeak"] (tr_ (th_ [class_ "p-3"] "Status" <> th_ [class_ "p-3"] "Channel" <> th_ [class_ "p-3"] "Alert Type" <> th_ [class_ "p-3 text-right"] "Time")) <> tbody_ [class_ "text-sm divide-y divide-strokeWeak"] (foldMap' renderRow ts))
    renderRow t = tr_ [class_ "hover-only:hover:bg-fillWeaker transition-colors"] (td_ [class_ "p-3"] (if t.status == "sent" then span_ [class_ "badge badge-success badge-sm gap-1"] (faSprite_ "check" "solid" "h-3 w-3" >> "Sent") else span_ [class_ "badge badge-error badge-sm gap-1"] (faSprite_ "xmark" "solid" "h-3 w-3" >> "Failed")) <> td_ [class_ "p-3 capitalize font-medium"] (toHtml $ if t.channel == "all" then "All channels" else t.channel) <> td_ [class_ "p-3 text-textWeak"] (toHtml $ T.replace "_" " " t.issueType) <> td_ [class_ "p-3 text-right tabular-nums text-textWeak"] (toHtml $ formatTime defaultTimeLocale "%b %d, %H:%M" t.createdAt))
