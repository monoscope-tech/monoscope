module Pages.Integrations (
  TestForm (..),
  notificationsTestPostH,
  notificationsTestHistoryGetH,
  TestHistory (..),
  NotificationTestHistoryGet (..),
) where

import Data.Aeson qualified as AE
import Data.Default (Default (..))
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, try)
import Effectful.PostgreSQL qualified as DB
import Effectful.Reader.Static (Reader, ask)
import Lucid2
import Models.Apis.Issues (IssueType (..), parseIssueType)
import Models.Apis.Slack (getDiscordDataByProjectId, getProjectSlackData)
import Models.Projects.ProjectMembers (getTeam)
import Models.Projects.Projects qualified as Projects
import Models.Projects.Teams (TeamId)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Mail (NotificationAlerts (..), sendDiscordAlert, sendSlackAlert, sendWhatsAppAlert)
import Pkg.SampleAlerts (sampleAlert, sampleReport)
import Relude hiding (Reader, State, ask)
import Servant (ServerError, err400)
import Servant.API (FormUrlEncoded)
import System.Config qualified as Config
import System.Types (DB, RespHeaders)
import Web.FormUrlEncoded (FromForm)
import Web.Pages.Sessions qualified as Sessions
import Web.Utils (addErrorToast, addRespHeaders, addSuccessToast)


data TestForm = TestForm
  { issueType :: Text
  , channel :: Text
  , teamId :: Maybe TeamId
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
  toHtml (NotificationTestHistoryGet tests) = historyHtml_ tests
  toHtmlRaw = toHtml


notificationsTestPostH :: (DB es, Reader Config.AuthContext :> es, Error ServerError :> es) => Projects.ProjectId -> TestForm -> Eff es (RespHeaders (Html ()))
notificationsTestPostH pid TestForm{..} = do
  project <- Sessions.getProject pid
  let alert = bool (sampleAlert (fromMaybe APIChange $ parseIssueType issueType) project.title) (sampleReport project.title) (issueType == "report")
      sendToChannels sendFn getChannels = getChannels >>= traverse_ \cs -> forM_ cs (sendFn alert pid project.title . Just)

  result <- try @SomeException $ case (channel, teamId) of
    ("slack", Just tid) -> getTeam tid >>= sendToChannels sendSlackAlert (pure . (.slack_channels))
    ("slack", Nothing) -> getProjectSlackData pid >>= traverse_ \s -> sendSlackAlert alert pid project.title (Just s.channelId)
    ("discord", Just tid) -> getTeam tid >>= sendToChannels sendDiscordAlert (pure . (.discord_channels))
    ("discord", Nothing) -> getDiscordDataByProjectId pid >>= traverse_ \d -> traverse_ (sendDiscordAlert alert pid project.title . Just) d.notifsChannelId
    ("whatsapp", _) -> sendWhatsAppAlert alert pid project.title project.whatsappNumbers
    _ -> throwError err400

  void $ DB.execute [sql|INSERT INTO apis.notification_test_history (project_id, issue_type, channel, target, status, error) VALUES (?, ?, ?, ?, ?, ?)|]
    (pid, issueType, channel, "" :: Text, bool "failed" "sent" (isRight result), either (Just . show) (const Nothing) result)

  either ((`addErrorToast` Nothing) . show) (const $ addSuccessToast "Test sent!" Nothing) result >> addRespHeaders mempty


notificationsTestHistoryGetH :: DB es => Projects.ProjectId -> Eff es (RespHeaders NotificationTestHistoryGet)
notificationsTestHistoryGetH pid = do
  tests <- DB.query [sql|SELECT * FROM apis.notification_test_history WHERE project_id = ? ORDER BY created_at DESC LIMIT 20|] (Only pid)
  addRespHeaders $ NotificationTestHistoryGet tests


historyHtml_ :: [TestHistory] -> Html ()
historyHtml_ = bool emptyMsg . div_ [class_ "space-y-2"] . foldMap renderTest <*> null
  where
    emptyMsg = p_ [class_ "text-textWeak text-center py-8"] "No tests yet"
    renderTest t = div_ [class_ "flex items-center justify-between p-3 bg-bgRaised rounded"] do
      div_ $ bool (span_ [class_ "badge badge-error badge-sm"] "Failed") (span_ [class_ "badge badge-success badge-sm"] "Sent") (t.status == "sent")
        <> span_ [class_ "ml-2 text-sm"] (toHtml $ t.channel <> " → " <> t.issueType)
        <> foldMap (\e -> p_ [class_ "text-xs text-textError mt-1"] $ toHtml e) t.error
      span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml $ formatTime defaultTimeLocale "%b %d %H:%M" t.createdAt
