{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Projects.Integrations (
  CreateProjectForm,
  NotifListForm,
  integrationsSettingsGetH,
  updateNotificationsChannel,
)
where

import Data.Default (Default (..))
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx
import Models.Apis.Slack (SlackData, getDiscordDataByProjectId, getProjectSlackData)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (ask, asks)
import System.Config
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)


data CreateProjectForm = CreateProjectForm
  { title :: Text
  , description :: Text
  , emails :: [Text]
  , permissions :: [ProjectMembers.Permissions]
  , isUpdate :: Bool
  , projectId :: Text
  , paymentPlan :: Text
  , timeZone :: Text
  , orderId :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromForm)


----------------------------------------------------------------------------------------------------------
integrationsSettingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
integrationsSettingsGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let createProj =
        CreateProjectForm
          { title = project.title
          , description = project.description
          , emails = []
          , permissions = []
          , isUpdate = True
          , projectId = pid.toText
          , paymentPlan = project.paymentPlan
          , timeZone = project.timeZone
          , orderId = project.orderId
          }
  slackInfo <- getProjectSlackData pid

  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Integrations", isSettingsPage = True}
  addRespHeaders $ bodyWrapper bwconf $ integrationsBody sess.persistentSession appCtx.config True createProj (Just project.notificationsChannel) slackInfo


data NotifListForm = NotifListForm
  { notificationsChannel :: [Text]
  , discordUrl :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


updateNotificationsChannel :: Projects.ProjectId -> NotifListForm -> ATAuthCtx (RespHeaders (Html ()))
updateNotificationsChannel pid NotifListForm{notificationsChannel, discordUrl} = do
  let discordUrl' = if discordUrl == Just "" then Nothing else discordUrl
  if "discord" `elem` notificationsChannel && isNothing discordUrl'
    then do
      discordData <- getDiscordDataByProjectId pid
      case discordData of
        Just _ -> do
          _ <- dbtToEff do Projects.updateNotificationsChannel pid notificationsChannel discordUrl'
          addSuccessToast "Updated Notification Channels Successfully" Nothing
          addRespHeaders ""
        Nothing -> do
          addErrorToast "You need to connect discord to this project first." Nothing
          addRespHeaders ""
    else do
      if "slack" `elem` notificationsChannel
        then do
          slackData <- getProjectSlackData pid
          case slackData of
            Nothing -> do
              addErrorToast "You need to connect slack to this project first." Nothing
              addRespHeaders ""
            Just _ -> do
              _ <- dbtToEff do Projects.updateNotificationsChannel pid notificationsChannel discordUrl'
              addSuccessToast "Updated Notification Channels Successfully" Nothing
              addRespHeaders ""
        else do
          _ <- dbtToEff do Projects.updateNotificationsChannel pid notificationsChannel discordUrl'
          addSuccessToast "Updated Notification Channels Successfully" Nothing
          addRespHeaders ""


----------------------------------------------------------------------------------------------------------
-- integrationsBody is the core html view
integrationsBody :: Sessions.PersistentSession -> EnvConfig -> Bool -> CreateProjectForm -> Maybe (V.Vector Projects.NotificationChannel) -> Maybe SlackData -> Html ()
integrationsBody sess envCfg isUpdate cp notifChannel slackData = do
  section_ [id_ "main-content", class_ "p-3 py-5 sm:p-6 overflow-y-scroll h-full"] do
    div_ [class_ "mx-auto", style_ "max-width:1000px"] do
      let pid = cp.projectId
      form_ [class_ "mt-10", hxPost_ [text|/p/$pid/notifications-channels|], hxSwap_ "none"] do
        h2_ [class_ "text-slate-700 text-3xl font-medium mb-5"] "Project Notifications"
        div_ [class_ "flex flex-col gap-4"] do
          p_ [] "Select channels to receive updates on this project."
          let notif = fromMaybe [] notifChannel
          let isCheckedM = Projects.NEmail `elem` notif
          div_ [class_ $ "bg-white rounded-lg border border-strokeWeak shadow-xs " <> if isCheckedM then "border-l-4 border-l-primary" else ""] $ do
            div_ [class_ "p-6 pb-3"] $ do
              div_ [class_ "flex items-center justify-between"] $ do
                div_ [class_ "flex items-center gap-3"] $ do
                  div_ [class_ "flex h-10 w-10 items-center justify-center rounded-full bg-fillWeak"]
                    $ faSprite_ "envelope" "solid" "h-6 w-6"
                  div_ [] $ do
                    h3_ [class_ "text-lg font-semibold"] "Email Notifications"
                    p_ [class_ "text-sm text-gray-500"] "Receive project updates via email"
                label_ [class_ "relative inline-flex items-center cursor-pointer"] $ do
                  input_ [type_ "checkbox", name_ "notificationsChannel", value_ "email", if isCheckedM then checked_ else title_ "Enable notification via email", class_ "toggle toggle-primary"]
                  span_ [class_ "slider"] ("" :: Html ())
            div_ [class_ "px-6 pb-6"]
              $ p_ [class_ "text-sm text-gray-500"] "All users on this project will receive updates via email."

          let isCheckedS = Projects.NSlack `elem` notif
          div_ [class_ $ "bg-white rounded-lg border border-strokeWeak shadow-xs " <> if isCheckedS then "border-l-4 border-l-primary" else ""] $ do
            div_ [class_ "p-6 pb-3"] $ do
              div_ [class_ "flex items-center justify-between"] $ do
                div_ [class_ "flex items-center gap-3"] $ do
                  div_ [class_ "flex h-10 w-10 items-center justify-center rounded-full bg-fillWeak"] do
                    faSprite_ "slack" "solid" "h-6 w-6"
                  div_ $ do
                    h3_ [class_ "text-lg font-semibold"] "Slack"
                    p_ [class_ "text-sm text-gray-500"] "Send notifications to Slack channels"
                label_ [class_ "relative inline-flex items-center cursor-pointer"] $ do
                  input_ [type_ "checkbox", id_ "slack-toggle", if isCheckedS then checked_ else title_ "Enable notifications via slack", name_ "notificationsChannel", value_ "slack", class_ "toggle toggle-primary"]
            div_ [class_ "px-6 pb-6"] $ do
              case slackData of
                Just s -> p_ [class_ "text-sm text-gray-500 mb-4 text-green-500"] "Already connected, but you can add again to change workspace or channel."
                Nothing -> pass
              a_ [target_ "_blank", class_ "", href_ $ "https://slack.com/oauth/v2/authorize?client_id=" <> envCfg.slackClientId <> "&scope=chat:write,commands,incoming-webhook,files:write&user_scope=&redirect_uri=" <> envCfg.slackRedirectUri <> pid] do
                img_ [alt_ "Add to slack", height_ "40", width_ "139", src_ "https://platform.slack-edge.com/img/add_to_slack.png", term "srcSet" "https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x"]

          let isCheckedD = Projects.NDiscord `elem` notif
          div_ [class_ $ "bg-white rounded-lg border border-strokeWeak shadow-xs " <> if isCheckedD then "border-l-4 border-l-primary" else ""] $ do
            div_ [class_ "p-6 pb-6"] $ do
              div_ [class_ "flex items-center justify-between "] $ do
                div_ [class_ "flex items-center gap-3"] $ do
                  div_ [class_ "flex h-10 w-10 items-center justify-center rounded-full bg-fillWeak"] do
                    faSprite_ "discord" "solid" "h-6 w-6"
                  div_ $ do
                    h3_ [class_ "text-lg font-semibold"] "Discord"
                    p_ [class_ "text-sm text-gray-500"] "Send notifications to Discord servers"
                div_ [class_ "flex items-center gap-2"] do
                  label_ [class_ "relative inline-flex items-center cursor-pointer"] do
                    input_ [type_ "checkbox", name_ "notificationsChannel", value_ "discord", if isCheckedD then checked_ else title_ "Enable notification via discord", class_ "toggle toggle-primary"]

            div_ [class_ "px-6 pb-6"] do
              let addQueryParams = "&state=" <> pid <> "&redirect_uri=" <> envCfg.discordRedirectUri
              a_ [target_ "_blank", class_ "flex items-center gap-2 border p-2 w-max border-strokeStrong rounded-lg", href_ $ "https://discord.com/oauth2/authorize?response_type=code&client_id=" <> envCfg.discordClientId <> "&permissions=277025392640&integration_type=0&scope=bot+applications.commands" <> addQueryParams] do
                faSprite_ "discord" "solid" "h-6 w-6 text-textBrand"
                span_ [class_ "text-sm text-textStrong font-semibold"] "Add to Discord"

          button_ [class_ "btn btn-primary w-max"] "Save Selections"
