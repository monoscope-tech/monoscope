{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Projects.Integrations (
  CreateProjectForm,
  createProjectGetH,
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
import Models.Apis.Slack (SlackData, getProjectSlackData)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (ask, asks)
import System.Config
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm, Default)


----------------------------------------------------------------------------------------------------------
-- createProjectGetH is the handler for the create projects page
createProjectGetH :: ATAuthCtx (RespHeaders (Html ()))
createProjectGetH = do
  appCtx <- ask @AuthContext
  sess <- Sessions.getSession
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Endpoints"
          }
  addRespHeaders $ bodyWrapper bwconf $ integrationsBody sess.persistentSession appCtx.config False (def @CreateProjectForm) Nothing Nothing


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

  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Integrations"}
  addRespHeaders $ bodyWrapper bwconf $ integrationsBody sess.persistentSession appCtx.config True createProj (Just project.notificationsChannel) slackInfo


data NotifListForm = NotifListForm
  { notificationsChannel :: [Text]
  , discordUrl :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


updateNotificationsChannel :: Projects.ProjectId -> NotifListForm -> ATAuthCtx (RespHeaders (Html ()))
updateNotificationsChannel pid NotifListForm{notificationsChannel, discordUrl} = do
  let discordUrl' = if discordUrl == Just "" then Nothing else discordUrl
  if "discord" `elem` notificationsChannel && isNothing discordUrl'
    then do
      addErrorToast "No discord webhook provided" Nothing
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
      when isUpdate do
        let pid = cp.projectId
        form_ [class_ "mt-10", hxPost_ [text|/p/$pid/notifications-channels|], hxSwap_ "none"] do
          h2_ [class_ "text-slate-700 text-3xl font-medium mb-5"] "Project Notifications"
          div_ [class_ "flex flex-col gap-4 border shadow p-6 rounded-2xl"] do
            p_ [] "Select channels to receive updates on this project."
            let notif = fromMaybe [] notifChannel
            div_ [class_ "shadow-sm border p-6 rounded-lg"] do
              div_ [class_ "flex gap-6 items-center mb-2"] do
                h3_ [class_ "text-2xl font-bold"] "Email"
                let isChecked = Projects.NEmail `elem` notif
                div_
                  [class_ "flex items-center gap-2"]
                  do
                    label_ [class_ "relative inline-flex items-center cursor-pointer"] do
                      input_ [type_ "checkbox", name_ "notificationsChannel", value_ "email", if isChecked then checked_ else title_ "Enable notification via email", class_ "sr-only peer"]
                      div_ [class_ "w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer dark:bg-gray-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-base-100 after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-gray-600 peer-checked:bg-blue-600"] pass
              p_ [class_ " text-brand font-bold"] "All users on this project, will receive updates via email."
            div_ [class_ "shadow-sm border p-6 rounded-lg"] do
              div_ [class_ "flex gap-6 items-center mb-6"] do
                h3_ [class_ "text-2xl font-bold"] "Slack"
                let isChecked = Projects.NSlack `elem` notif
                div_
                  [class_ "flex items-center gap-2"]
                  do
                    label_ [class_ "relative inline-flex items-center cursor-pointer"] do
                      input_ [type_ "checkbox", name_ "notificationsChannel", if isChecked then checked_ else title_ "Enable notifications via slack", value_ "slack", class_ "sr-only peer"]
                      div_ [class_ "w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer dark:bg-gray-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-base-100 after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-gray-600 peer-checked:bg-blue-600"] pass
              case slackData of
                Just s -> span_ [class_ "font-bold  mb-2 text-brand block"] "Already connected, but you can add again to change workspace or channel."
                Nothing -> pass
              a_ [target_ "_blank", class_ "", href_ $ "https://slack.com/oauth/v2/authorize?client_id=6211090672305.6200958370180&scope=chat:write,incoming-webhook&user_scope=&redirect_uri=" <> envCfg.slackRedirectUri <> pid] do
                img_ [alt_ "Add to slack", height_ "40", width_ "139", src_ "https://platform.slack-edge.com/img/add_to_slack.png", term "srcSet" "https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x"]

            div_ [class_ "shadow-sm border p-6 rounded-lg"] do
              div_ [class_ "flex gap-6 items-center mb-2"] do
                h3_ [class_ "text-2xl font-bold"] "Discord"
                let isChecked = Projects.NDiscord `elem` notif
                div_
                  [class_ "flex items-center gap-2"]
                  do
                    label_ [class_ "relative inline-flex items-center cursor-pointer"] do
                      input_ [type_ "checkbox", name_ "notificationsChannel", value_ "discord", if isChecked then checked_ else title_ "Enable notification via discord", class_ "sr-only peer"]
                      div_ [class_ "w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer dark:bg-gray-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-base-100 after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-gray-600 peer-checked:bg-blue-600"] pass
              input_ [type_ "text", name_ "discordUrl", class_ "input input-bordered input-sm w-full mt-2", placeholder_ "Discord Webhook URL"]

            button_ [class_ "btn btn-primary w-max"] "Save Selections"
