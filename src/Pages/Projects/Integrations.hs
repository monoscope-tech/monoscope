{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Projects.Integrations (
  CreateProjectForm,
  NotifListForm,
  integrationSettingGetH,
  updateNotificationsChannel,
)
where

import BackgroundJobs qualified
import Control.Lens ((.~), (^.))
import Data.Aeson (encode)
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Base64 qualified as B64
import Data.CaseInsensitive (original)
import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.List.Extra (cons)
import Data.List.Unique
import Data.Pool (withResource)
import Data.Text (toLower)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Valor (Valor, check1, failIf, validateM)
import Data.Valor qualified as Valor
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Apis.Slack
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMembers qualified as Projects
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Network.Wreq
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.ConvertKit qualified as ConvertKit
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import Servant (
  Headers,
  addHeader,
  noHeader,
 )
import Servant.Htmx
import System.Config
import System.Types
import Utils
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
integrationSettingGetH :: Projects.ProjectId -> ATAuthCtx (Html ())
integrationSettingGetH pid = do
  appCtx <- ask @AuthContext
  sess <- Sessions.getSession
  let pSess = Unsafe.fromJust sess.persistentSession
  projM <- dbtToEff $ Projects.selectProjectForUser (pSess.userId, pid)
  let proj = Unsafe.fromJust projM
  let createProj =
        CreateProjectForm
          { title = proj.title
          , description = proj.description
          , emails = []
          , permissions = []
          , isUpdate = True
          , projectId = pid.toText
          , paymentPlan = proj.paymentPlan
          , timeZone = proj.timeZone
          , orderId = proj.orderId
          }
  slackInfo <- dbtToEff $ getProjectSlackData pid

  let bwconf = (def :: BWConfig){sessM = sess.persistentSession, currProject = projM, pageTitle = "Settings"}
  pure $ bodyWrapper bwconf $ integrationsBody pSess appCtx.config True createProj  (Just proj.notificationsChannel) slackInfo


----------------------------------------------------------------------------------------------------------



data NotifListForm = NotifListForm
  { notificationsChannel :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


updateNotificationsChannel :: Projects.ProjectId -> NotifListForm -> ATAuthCtx (Headers '[HXTrigger] (Html ()))
updateNotificationsChannel pid NotifListForm{notificationsChannel} = do
  if "slack" `elem` notificationsChannel
    then do
      slackData <- dbtToEff $ getProjectSlackData pid
      case slackData of
        Nothing -> do
          let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"errorToast": ["You need to connect slack to this project first."]}|]
          pure $ addHeader hxTriggerData $ span_ ""
        Just _ -> do
          _ <- dbtToEff do Projects.updateNotificationsChannel pid notificationsChannel
          let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Updated Notifications Channels Successfully"]}|]
          pure $ addHeader hxTriggerData $ span_ ""
    else do
      _ <- dbtToEff do Projects.updateNotificationsChannel pid notificationsChannel
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Updated Notifications Channels Successfully"]}|]
      pure $ addHeader hxTriggerData $ span_ ""


----------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------
-- integrationsBody is the core html view
integrationsBody :: Sessions.PersistentSession -> EnvConfig -> Bool -> CreateProjectForm -> Maybe (V.Vector Projects.NotificationChannel) -> Maybe SlackData -> Html ()
integrationsBody sess envCfg isUpdate cp  notifChannel slackData = do
  --let paymentPlan = if cp.paymentPlan == "" then "UsageBased" else cp.paymentPlan
  section_ [id_ "main-content", class_ "p-3 py-5 sm:p-6 overflow-y-scroll h-full"] do
    div_ [class_ "mx-auto", style_ "max-width:800px"] do
      h2_ [class_ "text-slate-700 text-3xl font-medium mb-5"] $ toHtml @String $ if isUpdate then "Integrations" else "Integrations"
      when isUpdate do
        let pid = cp.projectId
        form_ [class_ "mt-10", hxPost_ [text|/p/$pid/notifications-channels|], hxSwap_ "none"] do
          h2_ [class_ "text-slate-700 text-3xl font-medium mb-5"] "Integrations"
          div_ [class_ "flex flex-col gap-4 border p-6 rounded-2xl"] do
            p_ [] "Select channels to receive updates on this project."
            let notif = fromMaybe [] notifChannel
            div_ [class_ "bg-gray-100 p-6 rounded-lg"] do
              div_ [class_ "flex gap-6 items-center mb-6"] do
                let isChecked = Projects.NEmail `elem` notif
                div_
                  [class_ "flex items-center gap-2"]
                  do
                    label_ [class_ "relative inline-flex items-center cursor-pointer"] do
                      input_ [type_ "checkbox", name_ "notificationsChannel", value_ "email", if isChecked then checked_ else title_ "Enable notification via email", class_ "sr-only peer"]
                      div_ [class_ "w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer dark:bg-gray-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-gray-600 peer-checked:bg-blue-600"] pass
                h3_ [class_ "text-2xl font-bold"] "Email"
              input_ [value_ "All users on this project", disabled_ "true", class_ "w-full p-2 my-2 text-sm bg-white text-slate-700 border rounded"]
            div_ [class_ "bg-gray-100 p-6"] do
              div_ [class_ "flex gap-6 items-center mb-6"] do
                let isChecked = Projects.NSlack `elem` notif
                div_
                  [class_ "flex items-center gap-2"]
                  do
                    label_ [class_ "relative inline-flex items-center cursor-pointer"] do
                      input_ [type_ "checkbox", name_ "notificationsChannel", if isChecked then checked_ else title_ "Enable notifications via slack", value_ "slack", class_ "sr-only peer"]
                      div_ [class_ "w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer dark:bg-gray-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-gray-600 peer-checked:bg-blue-600"] pass
                h3_ [class_ "text-2xl font-bold"] "Slack"
              case slackData of
                Just s -> span_ [class_ "font-bold text-sm mb-2 text-blue-500 block"] "Already connected, but you can add again to change workspace or channel."
                Nothing -> pass
              a_ [target_ "_blank", class_ "", href_ $ "https://slack.com/oauth/v2/authorize?client_id=6211090672305.6200958370180&scope=chat:write,incoming-webhook&user_scope=&redirect_uri=" <> envCfg.slackRedirectUri <> pid] do
                img_ [alt_ "Add to slack", height_ "40", width_ "139", src_ "https://platform.slack-edge.com/img/add_to_slack.png", term "srcSet" "https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x"]
            -- span_ [class_ "my-4 text-sm text-gray-500 block"] "OR"
            -- form_ [class_ "flex flex-col rounded-lg", hxPost_ [text|/p/$pid/slack/webhook|], hxSwap_ "none"] do
            --   label_ [] "Slack webhook"
            --   div_ [class_ "flex gap-2 items-center"] do
            --     input_ [type_ "hidden", name_ "projects", value_ pid]
            --     input_ [value_ (maybe "" (\s -> s.webhookUrl) slackData), placeholder_ "https://hooks.slack.com/services/xxxxxxxxx/xxxxxxxx/xxxxxxxxxxx", name_ "webhookUrl", class_ "w-full p-2 my-2 text-sm bg-white text-slate-700 border rounded"]
            --     button_ [class_ "text-white bg-blue-600 rounded-lg px-4 py-1 w-max"] "Save"
            button_ [class_ "btn btn-primary"] "Save Selections"
