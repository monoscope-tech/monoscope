{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Projects.Integrations (
  CreateProjectForm,
  NotifListForm,
  integrationsSettingsGetH,
  updateNotificationsChannel,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default (..))
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
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

  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Integrations", isSettingsPage = True, enableBrowserMonitoring = appCtx.config.enableBrowserMonitoring}
  addRespHeaders $ bodyWrapper bwconf $ integrationsBody sess.persistentSession appCtx.config True createProj (Just project.notificationsChannel) project.whatsappNumbers slackInfo


data NotifListForm = NotifListForm
  { notificationsChannel :: [Text]
  , phones :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, FromForm)


updateNotificationsChannel :: Projects.ProjectId -> NotifListForm -> ATAuthCtx (RespHeaders (Html ()))
updateNotificationsChannel pid NotifListForm{notificationsChannel, phones} = do
  validationResult <- validateNotificationChannels pid notificationsChannel phones
  case validationResult of
    Left errorMessage -> do
      addErrorToast errorMessage Nothing
      addRespHeaders ""
    Right () -> do
      _ <- dbtToEff $ Projects.updateNotificationsChannel pid notificationsChannel phones
      addSuccessToast "Updated Notification Channels Successfully" Nothing
      addRespHeaders ""


validateNotificationChannels :: Projects.ProjectId -> [Text] -> [Text] -> ATAuthCtx (Either Text ())
validateNotificationChannels pid notificationsChannel phones = do
  discordValidation <- validateDiscord pid notificationsChannel
  slackValidation <- validateSlack pid notificationsChannel
  let whatsappValidation = validateWhatsapp notificationsChannel phones
  pure $ discordValidation *> slackValidation *> whatsappValidation


validateDiscord :: Projects.ProjectId -> [Text] -> ATAuthCtx (Either Text ())
validateDiscord pid notificationsChannel =
  if "discord" `elem` notificationsChannel
    then do
      discordData <- getDiscordDataByProjectId pid
      pure $ case discordData of
        Just _ -> Right ()
        Nothing -> Left "You need to connect Discord to this project first."
    else pure $ Right ()


validateSlack :: Projects.ProjectId -> [Text] -> ATAuthCtx (Either Text ())
validateSlack pid notificationsChannel =
  if "slack" `elem` notificationsChannel
    then do
      slackData <- getProjectSlackData pid
      pure $ case slackData of
        Just _ -> Right ()
        Nothing -> Left "You need to connect Slack to this project first."
    else pure $ Right ()


validateWhatsapp :: [Text] -> [Text] -> Either Text ()
validateWhatsapp notificationsChannel numbers = if "phone" `elem` notificationsChannel && null numbers then Left "Provide at least one whatsapp number" else Right ()


----------------------------------------------------------------------------------------------------------
-- integrationsBody is the core html view

integrationsBody :: Sessions.PersistentSession -> EnvConfig -> Bool -> CreateProjectForm -> Maybe (V.Vector Projects.NotificationChannel) -> V.Vector Text -> Maybe SlackData -> Html ()
integrationsBody sess envCfg isUpdate cp notifChannel phones slackData = do
  section_ [id_ "main-content", class_ "p-3 py-5 sm:p-6 overflow-y-scroll h-full"] do
    div_ [class_ "mx-auto", style_ "max-width:1000px"] do
      let pid = cp.projectId
      div_
        [ class_ "mt-10"
        , hxPost_ [text|/p/$pid/notifications-channels|]
        , hxVals_ "js:{notificationsChannel: getChecked(), phones: getTags()}"
        , hxSwap_ "none"
        , hxTrigger_ "submit"
        , id_ "notifsForm"
        ]
        do
          h2_ [class_ "text-textStrong text-3xl font-medium mb-5"] "Project Notifications"
          div_ [class_ "flex flex-col gap-4"] do
            p_ [] "Select channels to receive updates on this project."
            renderNotificationOption "Email Notifications" "Receive project updates via email" "email" Projects.NEmail notifChannel (faSprite_ "envelope" "solid" "h-6 w-6") ""
            renderNotificationOption "Slack" "Send notifications to Slack channels" "slack" Projects.NSlack notifChannel (faSprite_ "slack" "solid" "h-6 w-6") (renderSlackIntegration envCfg pid slackData)
            renderNotificationOption "Discord" "Send notifications to Discord servers" "discord" Projects.NDiscord notifChannel (faSprite_ "discord" "solid" "h-6 w-6") (renderDiscordIntegration envCfg pid)
            renderNotificationOption "WhatsApp" "Send notificataoin via WhatsApp" "phone" Projects.NPhone notifChannel (faSprite_ "whatsapp" "solid" "h-6 w-6") renderWhatsappIntegration
            button_ [class_ "btn btn-primary w-max", [__| on click htmx.trigger("#notifsForm", "submit")|]] "Save Selections"
  let tgs = decodeUtf8 $ AE.encode $ V.toList phones
  script_
    [text|
     document.addEventListener('DOMContentLoaded', function() {
      var inputElem = document.querySelector('#phones_input')
      var tagify = new Tagify(inputElem)
      tagify.addTags($tgs);
      window.tagify = tagify
    })

   function getChecked() {
     const checkedInputs = document.querySelectorAll('input[name="notifChannel"]:checked');
     const vals = Array.from(checkedInputs).map(input => input.value);
     return vals
   }

|]


renderNotificationOption :: Text -> Text -> Text -> Projects.NotificationChannel -> Maybe (V.Vector Projects.NotificationChannel) -> Html () -> Html () -> Html ()
renderNotificationOption title description value channel notifChannel icon extraContent = do
  let isChecked = channel `elem` fromMaybe [] notifChannel
  div_ [class_ $ "bg-bgRaised rounded-lg border border-strokeWeak shadow-xs " <> if isChecked then "border-l-4 border-l-primary" else ""] do
    div_ [class_ "p-6 pb-3"] do
      div_ [class_ "flex items-center justify-between"] do
        div_ [class_ "flex items-center gap-3"] do
          div_ [class_ "flex h-10 w-10 items-center justify-center rounded-full bg-fillWeak"] icon
          div_ $ do
            h3_ [class_ "text-lg font-semibold"] $ toHtml title
            p_ [class_ "text-sm text-textWeak"] $ toHtml description
        label_ [class_ "relative inline-flex items-center cursor-pointer"] do
          input_ [type_ "checkbox", value_ value, name_ "notifChannel", if isChecked then checked_ else title_ $ "Enable notification via " <> toText value, class_ "toggle toggle-primary"]
    div_ [class_ "px-6 pb-6"] do
      extraContent


renderWhatsappIntegration :: Html ()
renderWhatsappIntegration = do
  div_ [class_ "flex flex-col gap-2"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ "text-textStrong lowercase first-letter:uppercase"] "Add phone numbers"
    input_ [class_ "input rounded-lg w-full border border-strokeStrong", term "pattern" "^\\+?[1-9]\\d{6,14}$", type_ "text", id_ "phones_input"]


renderSlackIntegration :: EnvConfig -> Text -> Maybe SlackData -> Html ()
renderSlackIntegration envCfg pid slackData = do
  case slackData of
    Just _ -> p_ [class_ "text-sm text-textWeak mb-4 text-textSuccess"] "Already connected, but you can add again to change workspace or channel."
    Nothing -> pass
  a_ [target_ "_blank", href_ $ "https://slack.com/oauth/v2/authorize?client_id=" <> envCfg.slackClientId <> "&scope=chat:write,commands,incoming-webhook,files:write,app_mentions:read,channels:history,groups:history,im:history,mpim:history&user_scope=&redirect_uri=" <> envCfg.slackRedirectUri <> pid] do
    img_ [alt_ "Add to slack", height_ "40", width_ "139", src_ "https://platform.slack-edge.com/img/add_to_slack.png", term "srcSet" "https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x"]


renderDiscordIntegration :: EnvConfig -> Text -> Html ()
renderDiscordIntegration envCfg pid = do
  let addQueryParams = "&state=" <> pid <> "&redirect_uri=" <> envCfg.discordRedirectUri
  a_ [target_ "_blank", class_ "flex items-center gap-2 border p-2 w-max border-strokeStrong rounded-lg", href_ $ "https://discord.com/oauth2/authorize?response_type=code&client_id=" <> envCfg.discordClientId <> "&permissions=277025392640&integration_type=0&scope=bot+applications.commands" <> addQueryParams] do
    faSprite_ "discord" "solid" "h-6 w-6 text-textBrand"
    span_ [class_ "text-sm text-textStrong font-semibold"] "Add to Discord"
