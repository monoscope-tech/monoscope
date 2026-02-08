module Pages.Integrations (
  TestForm (..),
  notificationsTestPostH,
  notificationsTestHistoryGetH,
  TestHistory (..),
  NotificationTestHistoryGet (..),
) where

import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL qualified as DB
import Lucid
import Models.Apis.Issues (IssueType (..), parseIssueType)
import Models.Apis.Integrations (DiscordData (..), SlackData (..), getDiscordDataByProjectId, getProjectSlackData)
import Models.Projects.ProjectMembers (Team (..), getTeamsById)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Mail (sendDiscordAlert, sendSlackAlert, sendWhatsAppAlert)
import Pkg.SampleAlerts (sampleAlert, sampleReport)
import Relude hiding (Reader, State, ask)
import Servant (err400, err404)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
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
  project <- Projects.projectById pid >>= maybe (throwError err404) pure
  let alert = bool (sampleAlert (fromMaybe APIChange $ parseIssueType issueType) project.title) (sampleReport project.title) (issueType == "report")
      getTeam tid = listToMaybe <$> getTeamsById pid (V.singleton tid)

  case (channel, teamId) of
    ("slack", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.slack_channels \c -> sendSlackAlert alert pid project.title (Just c)
    ("slack", Nothing) -> getProjectSlackData pid >>= traverse_ \s -> sendSlackAlert alert pid project.title (Just s.channelId)
    ("discord", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.discord_channels \c -> sendDiscordAlert alert pid project.title (Just c)
    ("discord", Nothing) -> getDiscordDataByProjectId pid >>= traverse_ \d -> forM_ d.notifsChannelId \c -> sendDiscordAlert alert pid project.title (Just c)
    ("whatsapp", _) -> sendWhatsAppAlert alert pid project.title project.whatsappNumbers
    _ -> throwError err400

  void $ DB.execute [sql|INSERT INTO apis.notification_test_history (project_id, issue_type, channel, target, status, error) VALUES (?, ?, ?, ?, ?, ?)|]
    (pid, issueType, channel, "" :: Text, "sent" :: Text, Nothing :: Maybe Text)

  addSuccessToast "Test sent!" Nothing >> addRespHeaders mempty


notificationsTestHistoryGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders NotificationTestHistoryGet)
notificationsTestHistoryGetH pid = do
  tests <- DB.query [sql|SELECT * FROM apis.notification_test_history WHERE project_id = ? ORDER BY created_at DESC LIMIT 20|] (Only pid)
  addRespHeaders $ NotificationTestHistoryGet tests


historyHtml_ :: [TestHistory] -> Html ()
historyHtml_ = bool emptyMsg . div_ [class_ "space-y-2"] . foldMap renderTest <*> null
  where
    emptyMsg = p_ [class_ "text-textWeak text-center py-8"] "No tests yet"
    renderTest t = div_ [class_ "flex items-center justify-between p-3 bg-bgRaised rounded"] $ do
      void $ div_ $ bool (span_ [class_ "badge badge-error badge-sm"] "Failed") (span_ [class_ "badge badge-success badge-sm"] "Sent") (t.status == "sent")
        <> span_ [class_ "ml-2 text-sm"] (toHtml $ t.channel <> " → " <> t.issueType)
        <> foldMap (\e -> p_ [class_ "text-xs text-textError mt-1"] $ toHtml e) t.error
      span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml $ formatTime defaultTimeLocale "%b %d %H:%M" t.createdAt
