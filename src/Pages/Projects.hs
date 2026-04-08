module Pages.Projects (
  -- ListProjects
  listProjectsGetH,
  ListProjectsGet (..),
  -- Integrations
  CreateProjectForm (..),
  NotifListForm (..),
  integrationsSettingsGetH,
  updateNotificationsChannel,
  PagerdutyConnectForm (..),
  pagerdutyConnectH,
  pagerdutyDisconnectH,
  slackDisconnectH,
  -- ManageMembers
  manageMembersGetH,
  manageMembersPostH,
  deleteMemberH,
  ManageMembersForm (..),
  manageSubGetH,
  stripeCheckoutInitH,
  StripeCheckoutForm (..),
  ManageMembers (..),
  -- ManageTeams
  manageTeamsGetH,
  -- CreateProject
  createProjectPostH,
  createProjectFormV,
  createProjectFormToModel,
  CreateProjectFormError (..),
  pricingUpdateH,
  PricingUpdateForm (..),
  projectSettingsGetH,
  projectOnboardingH,
  deleteProjectGetH,
  CreateProject (..),
  CreateProjectResp (..),
  manageTeamPostH,
  manageTeamBulkActionH,
  TeamForm (..),
  teamGetH,
  ManageTeams (..),
  TBulkActionForm (..),
)
where

import BackgroundJobs qualified
import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64
import Data.CaseInsensitive (original)
import Data.CaseInsensitive qualified as CI
import Data.Char (isAlphaNum, isDigit, isLower)
import Data.Default (Default (..))
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq
import Data.Effectful.Wreq qualified as W
import Data.List.Unique (uniq)
import Data.Pool (withResource)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time
import Data.UUID qualified as RealUUID
import Data.Valor (Valor, check1, failIf, validateM)
import Data.Valor qualified as Valor
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Fmt
import GHC.Records (HasField (getField))
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Integrations (SlackData, deletePagerdutyData, getDiscordDataByProjectId, getPagerdutyByProjectId, getProjectSlackData, insertPagerdutyData)
import Models.Apis.Integrations qualified as Slack
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers (TeamMemberVM (..), TeamVM (..))
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Network.Wreq (getWith)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), bodyWrapper, settingsContentTarget)
import Pages.Bots.Discord qualified as Discord
import Pages.Bots.Slack qualified as SlackP
import Pages.Bots.Utils qualified as BotUtils
import Pages.Components (BadgeColor (..), FieldCfg (..), FieldSize (..), ModalCfg (..), PanelCfg (..), confirmModal_, dirtyFormSaveAttr_, formActionsModal_, formField_, formSelectField_, iconBadgeXs_, iconBadge_, infoBanner_, modalWith_, panel_, sectionLabel_, settingsH2_, settingsNavLink_, settingsSection_, tagInput_)
import Pages.Settings qualified as Settings
import Pkg.Components.Table (BulkAction (..), Table (..))
import Pkg.Components.Table qualified as Table
import Pkg.Components.Widget (Widget (..), WidgetType (..), widget_)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.EmailTemplates qualified as ET
import Pkg.Mail (addConvertKitUserOrganization, sendRenderedEmail)
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import Servant (addHeader)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers)
import Servant.Server (err302, errHeaders)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addReswap, addSuccessToast, addTriggerEvent, redirectCS)
import UnliftIO.Exception (tryAny)
import Utils (LoadingSize (..), faSprite_, htmxIndicator_, insertIfNotExist, isDemoAndNotSudo, lookupValueText)
import Web.FormUrlEncoded (FromForm)
import "base64" Data.ByteString.Base64 qualified as B64


--------------------------------------------------------------------------------
-- ListProjects
--------------------------------------------------------------------------------

listProjectsGetH :: ATAuthCtx (RespHeaders ListProjectsGet)
listProjectsGetH = do
  (sess, project) <- Projects.sessionAndProject (UUIDId RealUUID.nil)
  appCtx <- ask @AuthContext
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Projects"
          , hideNavbar = True
          , pageActions = Nothing
          , config = appCtx.env
          }

  projects <- V.fromList <$> Projects.selectProjectsForUser sess.persistentSession.userId
  let demoProject =
        (def :: Projects.Project')
          { Projects.title = project.title
          , Projects.description = project.description
          , Projects.createdAt = project.createdAt
          }

  if V.null projects && not appCtx.env.showDemoProject
    then throwError $ err302{errHeaders = [("Location", "/p/new")]}
    else addRespHeaders $ ListProjectsGet $ PageCtx bwconf (projects, demoProject, appCtx.env.showDemoProject)


newtype ListProjectsGet = ListProjectsGet {unwrap :: PageCtx (V.Vector Projects.Project', Projects.Project', Bool)}
  deriving stock (Show)


instance ToHtml ListProjectsGet where
  toHtml (ListProjectsGet (PageCtx bwconf (projects, demoProject, showDemoProject))) = toHtml $ PageCtx bwconf $ listProjectsBody bwconf.sessM projects demoProject showDemoProject
  toHtmlRaw = toHtml


listProjectsBody :: Maybe Projects.Session -> V.Vector Projects.Project' -> Projects.Project' -> Bool -> Html ()
listProjectsBody sessM projects demoProject showDemoProject = do
  nav_ [class_ "fixed top-0 left-0 right-0 bg-bgBase border-b border-strokeWeak z-50"] do
    div_ [class_ "flex items-center justify-between px-4 py-3"] do
      a_ [href_ "/", class_ "flex items-center"] do
        img_ [class_ "h-6 dark:hidden", src_ "/public/assets/svgs/logo_black.svg"]
        img_ [class_ "h-6 hidden dark:block", src_ "/public/assets/svgs/logo_white.svg"]
      div_ [class_ "flex items-center gap-3"] do
        label_ [class_ "swap swap-rotate"] do
          input_
            ( [ type_ "checkbox"
              , class_ "theme-controller"
              , id_ "dark-mode-toggle-navbar"
              , onclick_ "toggleDarkMode()"
              ]
                <> [checked_ | maybe True (\s -> s.theme /= "light") sessM]
            )
          span_ [class_ "swap-off"] $ faSprite_ "sun-bright" "regular" "h-5 w-5"
          span_ [class_ "swap-on"] $ faSprite_ "moon-stars" "regular" "h-5 w-5"
        a_ [class_ "btn btn-ghost btn-sm", href_ "https://monoscope.tech/docs/", target_ "_blank"] "Docs"
        a_ [class_ "btn btn-ghost btn-sm text-textError", href_ "/logout"] "Logout"

  section_ [id_ "main-content", class_ "mx-auto max-md:px-2 px-4 py-6 pb-36 pt-20 overflow-y-auto h-full"] do
    div_ [class_ "flex justify-between items-center mb-8"] do
      h2_ [class_ "text-textStrong text-3xl font-semibold"] "Projects"
      a_ [class_ "btn btn-primary btn-sm", href_ "/p/new"] (faSprite_ "plus" "regular" "h-4 w-4 mr-2" >> "New Project")

    unless (V.null projects) $ div_ [class_ "mb-12"] do
      h3_ [class_ "text-textWeak text-lg font-medium mb-4"] "Your Projects"
      div_ [class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4"] $ mapM_ projectCard_ $ V.toList projects

    when showDemoProject $ div_ [] do
      h3_ [class_ "text-textWeak text-lg font-medium mb-4"] "Demo Project"
      div_ [class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4"] $ projectCard_ demoProject


projectCard_ :: Projects.Project' -> Html ()
projectCard_ project = do
  div_ [class_ "surface-raised hover:shadow-md transition-shadow duration-200 overflow-hidden group"] do
    a_ [href_ ("/p/" <> project.id.toText), class_ "block"] do
      div_ [class_ "p-5 pb-3"] do
        div_ [class_ "flex justify-between items-start mb-3"] do
          div_ [class_ "flex-1 min-w-0"] do
            h4_ [class_ "text-textStrong font-semibold text-lg truncate group-hover:text-textBrand transition-colors"] $ toHtml project.title
            p_ [class_ "text-textWeak text-sm mt-1 line-clamp-2"] $ toHtml project.description
          faSprite_ "arrow-right" "regular" "h-4 w-4 text-iconNeutral opacity-0 group-hover:opacity-100 transition-opacity ml-2 mt-1"

        div_ [class_ "flex items-center justify-between text-sm text-textWeak"] do
          div_ [class_ "flex items-center gap-1"] do
            faSprite_ "calendar" "regular" "h-3.5 w-3.5"
            time_ [datetime_ $ fmt $ dateDashF project.createdAt] $ toHtml @Text $ fmt $ dateDashF project.createdAt

          unless (V.null project.usersDisplayImages)
            $ div_ [class_ "flex -space-x-2"] do
              project.usersDisplayImages & V.toList & take 3 & mapM_ \imgSrc ->
                img_ [class_ "inline-block h-6 w-6 rounded-full ring-2 ring-base-100", src_ imgSrc, alt_ "User avatar", term "loading" "lazy", term "decoding" "async"]
              when (V.length project.usersDisplayImages > 3)
                $ span_ [class_ "flex items-center justify-center h-6 w-6 rounded-full bg-fillWeak text-textWeak text-xs ring-2 ring-base-100"]
                $ toHtml ("+" <> show (V.length project.usersDisplayImages - 3))

      div_ [class_ "border-t border-strokeWeak bg-fillWeaker/30 px-4 pt-8 h-36"] do
        widget_
          $ (def :: Widget)
            { wType = WTTimeseriesLine
            , id = Just project.id.toText
            , title = Nothing
            , subtitle = Nothing
            , hideSubtitle = Just True
            , query = Nothing
            , _projectId = Just project.id
            , naked = Just True
            , hideLegend = Just True
            , standalone = Just True
            }


--------------------------------------------------------------------------------
-- Integrations
--------------------------------------------------------------------------------

data CreateProjectForm = CreateProjectForm
  { title :: Text
  , description :: Text
  , emails :: [Text]
  , permissions :: [ProjectMembers.Permissions]
  , timeZone :: Text
  , errorAlerts :: Maybe Text
  , endpointAlerts :: Maybe Text
  , weeklyNotifs :: Maybe Text
  , dailyNotifs :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromForm)


integrationsSettingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
integrationsSettingsGetH pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let createProj =
        CreateProjectForm
          { title = project.title
          , description = project.description
          , emails = []
          , permissions = []
          , timeZone = project.timeZone
          , errorAlerts = if project.errorAlerts then Just "on" else Nothing
          , endpointAlerts = if project.endpointAlerts then Just "on" else Nothing
          , weeklyNotifs = if project.weeklyNotif then Just "on" else Nothing
          , dailyNotifs = if project.dailyNotif then Just "on" else Nothing
          }
  slackInfo <- getProjectSlackData pid
  pagerdutyInfo <- getPagerdutyByProjectId pid
  channels <- maybe (pure []) (\d -> maybe [] (fromMaybe [] . (.channels)) <$> SlackP.getSlackChannels d.botToken d.teamId) slackInfo
  everyoneTeamM <- ProjectMembers.getEveryoneTeam pid
  let existingSlackChannels = maybe V.empty (.slack_channels) everyoneTeamM

  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Integrations", isSettingsPage = True, config = appCtx.config}
  addRespHeaders
    $ bodyWrapper bwconf
    $ integrationsBody
      IntegrationsConfig
        { session = sess.persistentSession
        , projectId = pid
        , envConfig = appCtx.env
        , isUpdate = True
        , createForm = createProj
        , notifChannel = Just project.notificationsChannel
        , phones = project.whatsappNumbers
        , emails = project.notifyEmails
        , slackData = slackInfo
        , pagerdutyData = pagerdutyInfo
        , slackChannels = channels
        , existingSlackChannels = existingSlackChannels
        , everyoneTeamId = (.id) <$> everyoneTeamM
        }


data NotifListForm = NotifListForm
  { notificationsChannel :: [Text]
  , phones :: [Text]
  , emails :: [Text]
  , slackChannels :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, FromForm)


updateNotificationsChannel :: Projects.ProjectId -> NotifListForm -> ATAuthCtx (RespHeaders (Html ()))
updateNotificationsChannel pid NotifListForm{notificationsChannel, phones, emails, slackChannels} = do
  validationResult <- validateNotificationChannels pid notificationsChannel phones
  case validationResult of
    Left errorMessage -> do
      addErrorToast errorMessage Nothing
      integrationsSettingsGetH pid
    Right () -> do
      _ <- Projects.updateNotificationsChannel pid notificationsChannel phones emails
      projectM <- Projects.projectById pid
      slackInfoM <- getProjectSlackData pid
      everyoneTeamM <- ProjectMembers.getEveryoneTeam pid
      whenJust everyoneTeamM \team -> do
        let oldChannels = S.fromList $ V.toList team.slack_channels
            newChannelsSet = S.fromList slackChannels
            addedChannels = S.difference newChannelsSet oldChannels
            teamDetails = (ProjectMembers.teamToDetails team){ProjectMembers.slackChannels = V.fromList slackChannels} :: ProjectMembers.TeamDetails
        void $ ProjectMembers.updateTeam pid team.id teamDetails
        case ((,) <$> projectM <*> slackInfoM) of
          Just (project, slackInfo) ->
            forM_ addedChannels \channelId -> do
              result <- tryAny $ SlackP.sendSlackWelcomeMessage slackInfo.botToken channelId project.title
              whenLeft_ result (SlackP.logWelcomeMessageFailure channelId)
          Nothing ->
            unless (S.null addedChannels)
              $ addErrorToast "Slack workspace is not linked to this project; welcome messages were not sent" Nothing
      addSuccessToast "Updated Notification Channels Successfully" Nothing
      integrationsSettingsGetH pid


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


newtype PagerdutyConnectForm = PagerdutyConnectForm {integrationKey :: Text}
  deriving stock (Generic)
  deriving anyclass (FromForm)


pagerdutyConnectH :: Projects.ProjectId -> PagerdutyConnectForm -> ATAuthCtx (RespHeaders (Html ()))
pagerdutyConnectH pid form = do
  let key = T.strip form.integrationKey
  if T.length key < 20 || T.null key
    then addErrorToast "PagerDuty integration key is too short" Nothing >> integrationsSettingsGetH pid
    else do
      void $ insertPagerdutyData pid key
      sess <- Projects.getSession
      Projects.logAuditS pid Projects.AEIntegrationConnected sess
        $ Just
        $ AE.object ["integration" AE..= ("pagerduty" :: Text)]
      addSuccessToast "PagerDuty connected" Nothing
      integrationsSettingsGetH pid


pagerdutyDisconnectH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
pagerdutyDisconnectH pid = do
  void $ deletePagerdutyData pid
  sess <- Projects.getSession
  Projects.logAuditS pid Projects.AEIntegrationDisconnected sess
    $ Just
    $ AE.object ["integration" AE..= ("pagerduty" :: Text)]
  addSuccessToast "PagerDuty disconnected" Nothing
  integrationsSettingsGetH pid


slackDisconnectH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
slackDisconnectH pid = do
  deleted <- Slack.deleteSlackData pid
  if deleted > 0
    then do
      void $ ProjectMembers.removeSlackChannelsFromEveryoneTeam pid
      sess <- Projects.getSession
      Projects.logAuditS pid Projects.AEIntegrationDisconnected sess
        $ Just
        $ AE.object ["integration" AE..= ("slack" :: Text)]
      addSuccessToast "Slack disconnected" Nothing
    else addErrorToast "Failed to disconnect Slack" Nothing
  integrationsSettingsGetH pid


data IntegrationsConfig = IntegrationsConfig
  { session :: Projects.PersistentSession
  , projectId :: Projects.ProjectId
  , envConfig :: EnvConfig
  , isUpdate :: Bool
  , createForm :: CreateProjectForm
  , notifChannel :: Maybe (V.Vector Projects.NotificationChannel)
  , phones :: V.Vector Text
  , emails :: V.Vector Text
  , slackData :: Maybe SlackData
  , pagerdutyData :: Maybe Slack.PagerdutyData
  , slackChannels :: [BotUtils.Channel]
  , existingSlackChannels :: V.Vector Text
  , everyoneTeamId :: Maybe UUID.UUID
  }


integrationsBody :: IntegrationsConfig -> Html ()
integrationsBody IntegrationsConfig{..} = do
  settingsSection_ do
    let pid = projectId.toText
    settingsH2_ "Integrations"

    infoBanner_ do
      "Channels configured here are automatically included in the "
      a_ [href_ ("/p/" <> pid <> "/manage_teams/everyone"), class_ "font-medium text-textBrand underline"] "@everyone"
      " team."

    let notifVals = hxVals_ "js:{notificationsChannel: Array.from(document.querySelectorAll('input[name=\"notifChannel\"]:checked')).map(i => i.value), phones: window.getTagValues('#phones_input'), emails: window.getTagValues('#emails_input'), slackChannels: window.getTagValues('#slack-channels-input')}"
    div_ [id_ "integrations-form-section"] do
      div_
        [ hxPost_ [text|/p/$pid/notifications-channels|]
        , notifVals
        , hxSwap_ "none"
        , hxTrigger_ "submit"
        , id_ "notifsForm"
        ]
        do
          let ems = decodeUtf8 $ AE.encode $ V.toList emails
              tgs = decodeUtf8 $ AE.encode $ V.toList phones
              integrations =
                [ ("email", "Email", Projects.NEmail, True, faSprite_ "envelope" "solid" "h-4 w-4", renderEmailIntegration ems)
                , ("slack", "Slack", Projects.NSlack, isJust slackData, faSprite_ "slack" "solid" "h-4 w-4", renderSlackIntegration envConfig pid slackData slackChannels existingSlackChannels)
                , ("discord", "Discord", Projects.NDiscord, True, faSprite_ "discord" "solid" "h-4 w-4", renderDiscordIntegration envConfig pid)
                , ("phone", "WhatsApp", Projects.NPhone, not $ V.null phones, faSprite_ "whatsapp" "solid" "h-4 w-4", renderWhatsappIntegration tgs)
                , ("pagerduty", "PagerDuty", Projects.NPagerduty, isJust pagerdutyData, faSprite_ "pager" "solid" "h-4 w-4", renderPagerdutyIntegration projectId.toText pagerdutyData)
                ]
                  :: [(Text, Text, Projects.NotificationChannel, Bool, Html (), Html ())]

          div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak"] do
            forM_ integrations \(val, title, channel, configured, icon, content) ->
              renderNotificationOption pid everyoneTeamId title val channel notifChannel configured icon content

          div_ [class_ "mt-6"] do
            button_
              [ class_ "btn btn-sm btn-ghost"
              , hxPost_ [text|/p/$pid/notifications-channels|]
              , notifVals
              , hxTarget_ "#integrations-form-section"
              , hxSelect_ "#integrations-form-section"
              , hxSwap_ "outerHTML swap:0.3s"
              , [__| on change from closest <div/> put .btn-primary into my.className then put 'btn btn-sm btn-primary' into my.className end |]
              ]
              "Save"

    -- Developer tools
    div_ [class_ "pt-6 border-t border-strokeWeak space-y-2"] do
      sectionLabel_ "Developer Tools"
      div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak"] do
        settingsNavLink_ ("/p/" <> pid <> "/byob_s3") "bucket" "S3 Bucket" "Connect your own S3-compatible storage"
        settingsNavLink_ ("/p/" <> pid <> "/settings/git-sync") "github" "GitHub Sync" "Sync dashboards to a GitHub repository"

    -- Test History
    div_ [class_ "pt-6 border-t border-strokeWeak space-y-2"] do
      sectionLabel_ "Test History"
      div_ [id_ "test-history", hxGet_ [text|/p/$pid/settings/integrations/history|], hxTrigger_ "load, testSent from:body", hxSwap_ "innerHTML"] do
        p_ [class_ "text-textWeak text-sm py-4"] "Loading..."


renderInlineTestButton :: Text -> Text -> Maybe UUID.UUID -> Html ()
renderInlineTestButton pid channel teamIdM =
  form_ [hxPost_ [text|/p/$pid/settings/integrations/test|], hxSwap_ "none", hxTrigger_ "submit", class_ "inline"] do
    input_ [type_ "hidden", name_ "channel", value_ channel]
    input_ [type_ "hidden", name_ "issueType", value_ "runtime_exception"]
    whenJust teamIdM \tid -> input_ [type_ "hidden", name_ "teamId", value_ $ UUID.toText tid]
    button_ [type_ "submit", class_ "btn btn-xs btn-outline gap-1", [__| on htmx:afterRequest from closest <form/> trigger testSent on body |]] do
      faSprite_ "flask-vial" "regular" "h-3 w-3"
      "Test"


renderNotificationOption :: Text -> Maybe UUID.UUID -> Text -> Text -> Projects.NotificationChannel -> Maybe (V.Vector Projects.NotificationChannel) -> Bool -> Html () -> Html () -> Html ()
renderNotificationOption pid teamIdM title value channel notifChannel isConfigured icon extraContent = do
  let isChecked = channel `elem` fromMaybe [] notifChannel
      isActive = isChecked && isConfigured
  div_ [class_ ""] do
    -- Compact row: icon, name, test, toggle
    div_ [class_ "flex items-center gap-3 p-3"] do
      div_ [class_ "flex items-center justify-center shrink-0 w-7 h-7 rounded-md", class_ $ if isActive then "bg-fillBrand-weak" else "bg-fillWeak"] icon
      span_ [class_ "text-sm font-medium text-textStrong flex-1 min-w-0"] $ toHtml title
      div_ [class_ "flex items-center gap-2 shrink-0", id_ $ value <> "-test-button"] do
        if isActive
          then renderInlineTestButton pid value teamIdM
          else button_ [type_ "button", disabled_ "", class_ "btn btn-xs btn-outline btn-neutral gap-1 cursor-not-allowed opacity-40", Aria.label_ "Enable first"] do
            faSprite_ "flask-vial" "regular" "h-3 w-3"
            "Test"
      label_ [class_ "relative inline-flex items-center cursor-pointer tap-target", Aria.label_ $ if isChecked then "Disable " <> title else "Enable " <> title] do
        input_ [type_ "checkbox", value_ value, name_ "notifChannel", if isChecked then checked_ else title_ $ "Enable " <> title, class_ "toggle toggle-sm toggle-primary"]

    -- Expanded config (when toggled on or has content)
    when (isChecked || not (T.null $ toStrict $ renderText extraContent))
      $ div_ [class_ "px-3 pb-3 pt-0 pl-13"] extraContent


renderEmailIntegration :: Text -> Html ()
renderEmailIntegration ems = do
  p_ [class_ "text-xs text-textWeak mb-2"] "Members are auto-included via @everyone. Add extra emails:"
  formField_ FieldSm def "Additional emails" "emails_input" False $ Just $ tagInput_ "emails_input" "Enter email addresses" [data_ "tagify-initial" ems]


renderWhatsappIntegration :: Text -> Html ()
renderWhatsappIntegration tgs = formField_ FieldSm def "Phone numbers" "phones_input" False $ Just $ tagInput_ "phones_input" "Enter phone numbers" [data_ "tagify-initial" tgs]


renderSlackIntegration :: EnvConfig -> Text -> Maybe SlackData -> [BotUtils.Channel] -> V.Vector Text -> Html ()
renderSlackIntegration envCfg pid slackData channels existingChannels = do
  let stateParam = if T.null pid then "" else "&state=" <> pid
      oauthUrl = "https://slack.com/oauth/v2/authorize?client_id=" <> envCfg.slackClientId <> "&scope=chat:write,commands,incoming-webhook,files:write,app_mentions:read,channels:read,groups:read,channels:history,groups:history,im:history,mpim:history,chat:write.public&user_scope=&redirect_uri=" <> envCfg.slackRedirectUri <> stateParam

  case slackData of
    Just sd -> do
      div_ [class_ "rounded-lg bg-fillBrand-weak p-2.5 text-xs mb-3"] do
        div_ [class_ "flex items-center gap-1.5"] do
          faSprite_ "circle-check" "solid" "w-3.5 h-3.5 text-iconSuccess"
          span_ [class_ "text-textStrong font-medium"] $ toHtml $ maybe ("Connected (Team ID: " <> sd.teamId <> ")") ("Workspace: " <>) sd.teamName
        when (isNothing sd.teamName) $ p_ [class_ "text-textWeak ml-5"] "Reconnect to see workspace name"

      let slackWhitelist = decodeUtf8 $ AE.encode $ map (\c -> AE.object ["value" AE..= BotUtils.channelId c, "name" AE..= ("#" <> BotUtils.channelName c)]) channels
          existingJSON = decodeUtf8 $ AE.encode $ V.toList existingChannels
      div_ [class_ "mb-3"] $ formField_ FieldSm def "Slack channels" "slack-channels-input" False $ Just $ tagInput_ "slack-channels-input" "Add Slack channels" [data_ "tagify-whitelist" slackWhitelist, data_ "tagify-enforce-whitelist" "", data_ "tagify-text-prop" "name", data_ "tagify-initial" existingJSON, data_ "tagify-resolve" ""]

      div_ [class_ "flex items-center gap-2"] do
        a_ [target_ "_blank", class_ "btn btn-xs btn-outline", href_ oauthUrl] "Reconnect"
        form_ [hxDelete_ [text|/p/$pid/settings/integrations/slack|], hxConfirm_ "Are you sure you want to disconnect Slack?", hxSwap_ "none", hxTrigger_ "submit"] do
          button_ [class_ "btn btn-xs btn-ghost text-textError", type_ "submit"] "Disconnect"
    Nothing -> do
      a_ [target_ "_blank", class_ "btn btn-xs btn-outline", href_ oauthUrl] "Connect to Slack"


renderDiscordIntegration :: EnvConfig -> Text -> Html ()
renderDiscordIntegration envCfg pid = do
  let addQueryParams = "&state=" <> pid <> "&redirect_uri=" <> envCfg.discordRedirectUri
  a_ [target_ "_blank", class_ "btn btn-xs btn-outline gap-1.5", href_ $ "https://discord.com/oauth2/authorize?response_type=code&client_id=" <> envCfg.discordClientId <> "&permissions=277025392640&integration_type=0&scope=bot+applications.commands" <> addQueryParams] do
    faSprite_ "discord" "solid" "h-3.5 w-3.5"
    "Add to Discord"


renderPagerdutyIntegration :: Text -> Maybe Slack.PagerdutyData -> Html ()
renderPagerdutyIntegration pid = div_ [id_ "pagerduty-integration"] . maybe disconnectedUI (const connectedUI)
  where
    connectedUI = do
      div_ [class_ "flex items-center gap-2"] do
        span_ [class_ "text-xs text-textSuccess flex items-center gap-1.5"] $ faSprite_ "circle-check" "regular" "h-3.5 w-3.5 text-iconSuccess" >> "Connected"
        button_ [class_ "btn btn-xs btn-ghost text-textError", hxPost_ [text|/p/$pid/settings/integrations/pagerduty/disconnect|], hxTarget_ "#integrations-form-section", hxSelect_ "#integrations-form-section", hxSwap_ "outerHTML swap:0.3s"] "Disconnect"
    disconnectedUI = form_ [class_ "flex flex-col gap-2", hxPost_ [text|/p/$pid/settings/integrations/pagerduty|], hxTarget_ "#integrations-form-section", hxSelect_ "#integrations-form-section", hxSwap_ "outerHTML swap:0.3s"] do
      formField_ FieldSm def{placeholder = "Events API v2 Integration Key"} "Integration Key" "integrationKey" False Nothing
      p_ [class_ "text-xs text-textWeak"] "Get from: PagerDuty → Services → Integrations → Events API v2"
      button_ [class_ "btn btn-sm btn-outline w-max", type_ "submit"] "Connect"


--------------------------------------------------------------------------------
-- ManageMembers
--------------------------------------------------------------------------------

data ManageMembersForm = ManageMembersForm
  { emails :: [Text]
  , permissions :: [ProjectMembers.Permissions]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromForm)


manageMembersPostH :: Projects.ProjectId -> Maybe Text -> ManageMembersForm -> ATAuthCtx (RespHeaders ManageMembers)
manageMembersPostH pid onboardingM form = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let currUserId = sess.persistentSession.userId
  projMembers <- ProjectMembers.selectActiveProjectMembers pid
  let usersAndPermissions = filter (\(x, _) -> not (T.null x)) $ zip (form.emails <&> T.strip) form.permissions & uniq
  let uAndPOldAndChanged =
        mapMaybe
          ( \(email, permission) -> do
              let projMembersM = projMembers & find (\a -> original a.email == email && a.permission /= permission)
              projMembersM >>= (\projMember -> Just (projMember.id, permission))
          )
          usersAndPermissions

  let uAndPNew = filter (\(email, _) -> not $ any (\a -> original a.email == email) projMembers) usersAndPermissions

  let deletedUAndP =
        projMembers
          & filter (\pm -> not $ any (\(email, _) -> original pm.email == email) usersAndPermissions)
          & filter (\a -> a.userId /= currUserId)
          & map (.id)

  newProjectMembers <- forM uAndPNew \(email, permission) -> do
    userId' <- do
      userIdM' <- Projects.userIdByEmail email
      case userIdM' of
        Nothing -> do
          idM' <- Projects.createEmptyUser email
          case idM' of
            Nothing -> error "duplicate email in createEmptyUser"
            Just idX -> pure idX
        Just idX -> pure idX

    when (userId' /= currUserId)
      $ void
      $ liftIO
      $ withResource appCtx.pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.InviteUserToProject currUserId pid email project.title
    pure (email, permission, userId')

  let projectMembers =
        newProjectMembers
          & filter (\(_, _, id') -> id' /= currUserId)
          & map (\(email, permission, id') -> ProjectMembers.CreateProjectMembers pid id' permission)
  _ <- ProjectMembers.insertProjectMembers projectMembers

  unless (null uAndPOldAndChanged)
    $ void
    $ ProjectMembers.updateProjectMembersPermissons uAndPOldAndChanged

  whenJust (nonEmpty deletedUAndP) $ void . ProjectMembers.softDeleteProjectMembers
  when (project.paymentPlan == "Free") $ void $ ProjectMembers.deactivateNonOwnerMembers pid
  unless (null uAndPNew)
    $ Projects.logAuditS pid Projects.AEMemberAdded sess
    $ Just
    $ AE.object ["added" AE..= map fst uAndPNew]
  unless (null uAndPOldAndChanged) $ Projects.logAuditS pid Projects.AEMemberPermissionChanged sess Nothing

  projMembersLatest <- V.fromList <$> ProjectMembers.selectAllProjectMembers pid
  if isJust onboardingM
    then do
      redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=Integration"
      addRespHeaders $ ManageMembersPost (pid, projMembersLatest, project.paymentPlan)
    else do
      if project.paymentPlan == "Free" && not (null uAndPNew)
        then addSuccessToast "Members invited! Upgrade to enable team access." Nothing
        else addSuccessToast "Updated Members List Successfully" Nothing
      addRespHeaders $ ManageMembersPost (pid, projMembersLatest, project.paymentPlan)


data TeamForm = TeamForm
  { teamName :: Text
  , teamDescription :: Text
  , teamHandle :: Text
  , teamMembers :: V.Vector Projects.UserId
  , notifEmails :: V.Vector Text
  , slackChannels :: V.Vector Text
  , discordChannels :: V.Vector Text
  , phoneNumbers :: V.Vector Text
  , pagerdutyServices :: V.Vector Text
  , teamId :: Maybe UUID.UUID
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, FromForm)


validateTeamDetails :: Text -> Text -> V.Vector Text -> Either Text ()
validateTeamDetails name handle notifEmails = validateName name >> validateHandle handle >> forM_ notifEmails validateEmail
  where
    validateName n
      | T.null (T.strip n) = Left "Team name is required"
      | T.length (T.strip n) < 3 = Left "Team name must be at least 3 characters"
      | T.length n > 100 = Left "Team name must be less than 100 characters"
      | not $ T.all (\c -> isAlphaNum c || c `elem` (" -_" :: String)) n = Left "Invalid characters in team name"
      | otherwise = pass
    validateHandle h
      | T.null h = Left "Handle is required"
      | T.length h < 3 = Left "Handle must be at least 3 characters"
      | T.length h > 50 = Left "Handle must be less than 50 characters"
      | not $ all (\c -> isLower c || isDigit c || c == '-') (toString h) = Left "Handle must be lowercase, no spaces, and hyphens only"
      | not (isLower (T.head h)) = Left "Handle must start with a lowercase letter"
      | otherwise = pass
    validateEmail email = case T.splitOn "@" email of
      [localPart, domain] | not (T.null localPart) && not (T.null domain) && T.elem '.' domain -> pass
      _ -> Left $ "Invalid email format: " <> email


manageTeamPostH :: Projects.ProjectId -> TeamForm -> Maybe Text -> ATAuthCtx (RespHeaders ManageTeams)
manageTeamPostH pid form tmView = do
  (sess, _) <- Projects.sessionAndProject pid
  let currUserId = sess.persistentSession.userId
  userPermission <- ProjectMembers.getUserPermission pid currUserId
  projMembers <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  let validMemberIds = V.map (.userId) projMembers
      invalidMembers = V.filter (`V.notElem` validMemberIds) form.teamMembers
      teamDetails = ProjectMembers.TeamDetails form.teamName form.teamDescription form.teamHandle form.teamMembers form.notifEmails form.slackChannels form.discordChannels form.phoneNumbers form.pagerdutyServices
      validationErr msg = addErrorToast msg Nothing >> addRespHeaders (ManageTeamsPostError msg)
  case (userPermission == Just ProjectMembers.PAdmin, V.null invalidMembers, validateTeamDetails form.teamName form.teamHandle form.notifEmails, form.teamId) of
    (False, _, _, _) -> validationErr "Only admins can create or update teams"
    (_, False, _, _) -> validationErr "Some team members are not project members"
    (_, _, Left e, _) -> addErrorToast e Nothing >> addReswap "" >> addRespHeaders (ManageTeamsPostError e)
    (_, _, _, Just tid) -> do
      _ <- ProjectMembers.updateTeam pid tid teamDetails
      addSuccessToast "Team updated successfully" Nothing
      addTriggerEvent "closeModal" ""
      maybe (redirectCS $ "/p/" <> pid.toText <> "/manage_teams") (\_ -> redirectCS $ "/p/" <> pid.toText <> "/team/" <> form.teamHandle) tmView
      addRespHeaders $ ManageTeamsPostError ""
    (_, _, _, Nothing) -> do
      rowsAffected <- ProjectMembers.createTeam pid currUserId teamDetails
      if rowsAffected > 0
        then do
          addSuccessToast "Team saved successfully" Nothing
          addTriggerEvent "closeModal" ""
          redirectCS $ "/p/" <> pid.toText <> "/manage_teams"
          addRespHeaders $ ManageTeamsPostError ""
        else validationErr "Team handle already exists for this project."


newtype TBulkActionForm = TBulkActionForm
  { itemId :: [UUID.UUID]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromForm)


manageTeamBulkActionH :: Projects.ProjectId -> Text -> TBulkActionForm -> Maybe Text -> ATAuthCtx (RespHeaders ManageTeams)
manageTeamBulkActionH pid action TBulkActionForm{itemId} listViewM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  case action of
    "delete" -> do
      teamVm <- ProjectMembers.getTeamsById pid $ V.fromList itemId
      let canDelete = all (\team -> Just sess.user.id == team.created_by) teamVm
      if canDelete
        then do
          _ <- ProjectMembers.deleteTeams pid $ V.fromList itemId
          when (isNothing listViewM)
            $ redirectCS ("/p/" <> pid.toText <> "/manage_teams")
          addRespHeaders ManageTeamsDelete
        else do
          addErrorToast "You may only delete teams you own" Nothing
          addRespHeaders $ ManageTeamsPostError "You may only delete teams you own"
    _ -> do
      addErrorToast "Invalid action" Nothing
      addRespHeaders $ ManageTeamsPostError "Invalid action"


data ManageTeams
  = ManageTeamsGet (PageCtx (Projects.ProjectId, V.Vector ProjectMembers.ProjectMemberVM, [BotUtils.Channel], [BotUtils.Channel], V.Vector ProjectMembers.TeamVM))
  | ManageTeamsGet' (Projects.ProjectId, V.Vector ProjectMembers.ProjectMemberVM, [BotUtils.Channel], [BotUtils.Channel], V.Vector ProjectMembers.TeamVM)
  | ManageTeamsPostError Text
  | ManageTeamsDelete
  | ManageTeamGet (PageCtx (Projects.ProjectId, ProjectMembers.TeamVM, V.Vector ProjectMembers.ProjectMemberVM, [BotUtils.Channel], [BotUtils.Channel]))
  | ManageTeamGet' (Projects.ProjectId, ProjectMembers.TeamVM, V.Vector ProjectMembers.ProjectMemberVM, [BotUtils.Channel], [BotUtils.Channel])
  | ManageTeamGetError (PageCtx (Projects.ProjectId, Text))


instance ToHtml ManageTeams where
  toHtml (ManageTeamsGet (PageCtx bwconf (pid, members, slackChannels, discordChannels, teams))) = toHtml $ PageCtx bwconf $ manageTeamsPage pid members slackChannels discordChannels teams
  toHtml (ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams)) = toHtml $ manageTeamsPage pid members slackChannels discordChannels teams
  toHtml (ManageTeamsPostError msg) = span_ [] ""
  toHtml ManageTeamsDelete = toHtml ""
  toHtml (ManageTeamGet (PageCtx bwconf (pid, team, members, slackChannels, discordChannels))) = toHtml $ PageCtx bwconf $ teamPage pid team members slackChannels discordChannels
  toHtml (ManageTeamGet' (pid, team, members, slackChannels, discordChannels)) = toHtml $ teamPage pid team members slackChannels discordChannels
  toHtml (ManageTeamGetError (PageCtx bwconf (pid, message))) = toHtml $ PageCtx bwconf $ teamPageNF pid message
  toHtmlRaw = toHtml


manageTeamsGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders ManageTeams)
manageTeamsGetH pid layoutM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  projMembers <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  channels <- Slack.getProjectSlackData pid >>= maybe (pure []) \d -> maybe [] (fromMaybe [] . (.channels)) <$> SlackP.getSlackChannels d.botToken d.teamId
  discordChannels <- Slack.getDiscordDataByProjectId pid >>= maybe (pure []) (Discord.getDiscordChannels appCtx.env.discordBotToken . (.guildId))
  teams <- V.fromList <$> ProjectMembers.getTeamsVM pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Team"
          , currProject = Just project
          , isSettingsPage = True
          , config = appCtx.config
          }
  case layoutM of
    Just _ -> do
      addRespHeaders $ ManageTeamsGet' (pid, projMembers, channels, discordChannels, teams)
    _ -> do
      addRespHeaders $ ManageTeamsGet (PageCtx bwconf (pid, projMembers, channels, discordChannels, teams))


manageTeamsPage :: Projects.ProjectId -> V.Vector ProjectMembers.ProjectMemberVM -> [BotUtils.Channel] -> [BotUtils.Channel] -> V.Vector ProjectMembers.TeamVM -> Html ()
encodeChannels :: [BotUtils.Channel] -> Text
encodeChannels = decodeUtf8 . AE.encode . map \x -> AE.object ["name" AE..= ("#" <> x.channelName), "value" AE..= x.channelId]
manageTeamsPage pid projMembers channels discordChannels teams = do
  let whiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= (x.first_name <> " " <> x.last_name), "email" AE..= x.email, "value" AE..= x.userId]) <$> projMembers
      emailWhiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= x.email, "value" AE..= x.email]) <$> projMembers
      (channelWhiteList, discordWhiteList) = (encodeChannels channels, encodeChannels discordChannels)
  section_ [id_ "main-content", class_ "w-full space-y-4"] do
    div_ [class_ "flex items-center justify-between"] do
      h3_ [class_ "text-sm font-medium text-textStrong"] $ toHtml $ "Teams (" <> show (V.length teams) <> ")"
      teamModal pid Nothing whiteList emailWhiteList channelWhiteList discordWhiteList False $ span_ [class_ "btn btn-sm btn-primary gap-1.5"] $ do faSprite_ "plus" "regular" "w-3 h-3"; "New Team"

    if V.null teams
      then div_ [class_ "py-6 text-center text-textWeak text-sm"] "No teams yet. Create one to organize your project."
      else do
        let renderTeamNameCol team = nameCell pid team.name team.description team.handle
            renderModifiedCol team = span_ [class_ "monospace text-textWeak"] $ toHtml $ toText $ formatTime defaultTimeLocale "%b %-e, %-l:%M %P" team.updated_at
            renderMembersCol team = memberCell team.members
            renderNotificationsCol = notifsCell
            tableCols = [Table.col "Name" renderTeamNameCol, Table.col "Modified" renderModifiedCol, Table.col "Members" renderMembersCol, Table.col "Notifications" renderNotificationsCol]
            table =
              Table
                { config = def{Table.elemID = "teams_table", Table.renderAsTable = True, Table.bulkActionsInHeader = Just 0}
                , columns = tableCols
                , rows = teams
                , features =
                    def
                      { Table.rowId = Just \team -> UUID.toText team.id
                      , Table.rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                      , Table.bulkActions = [Table.BulkAction{icon = Just "trash", title = "Delete", uri = "/p/" <> pid.toText <> "/manage_teams/bulk_action/delete"}]
                      , Table.search = Just Table.ClientSide
                      }
                }
        div_ [class_ "w-full"] $ toHtml table


nameCell :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
nameCell pid name description handle = do
  span_ [class_ "flex items-center gap-2"] do
    span_ [class_ "p-1 px-2 bg-fillWeak rounded-md", data_ "tippy-content" "Team"] $ faSprite_ "users" "regular" "w-3 h-3"
    a_ [href_ ("/p/" <> pid.toText <> "/manage_teams/" <> handle), class_ "font-medium text-textStrong hover:text-textBrand hover:underline underline-offset-2"] $ toHtml name
    span_ [class_ "text-textWeak text-sm overflow-ellipsis truncate"] $ toHtml description


memberCell :: V.Vector ProjectMembers.TeamMemberVM -> Html ()
memberCell members = do
  div_ [class_ "inline-block flex -space-x-2"] do
    forM_ members \m ->
      div_ [class_ "inline-block mx-0.5", term "data-tippy-content" m.memberName]
        $ img_ [class_ "inline-block h-6 w-6 rounded-full border border-strokeWeak ", src_ m.memberAvatar, alt_ "User avatar", term "loading" "lazy", term "decoding" "async"]


notifsCell :: ProjectMembers.TeamVM -> Html ()
notifsCell team = div_ [class_ "flex items-center gap-2"] do
  let notifIcon ch icon tip = unless (V.null ch) $ div_ [term "data-tippy-content" tip] $ faSprite_ icon "solid" "h-3.5 w-3.5"
  notifIcon team.slack_channels "slack" "Slack" >> notifIcon team.discord_channels "discord" "Discord" >> notifIcon team.notify_emails "envelope" "Email" >> notifIcon team.pagerduty_services "pager" "PagerDuty"


teamGetH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders ManageTeams)
teamGetH pid handle layoutM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  teamVm <- ProjectMembers.getTeamByHandle pid handle
  projMembers <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  channels <- Slack.getProjectSlackData pid >>= maybe (pure []) \d -> maybe [] (fromMaybe [] . (.channels)) <$> SlackP.getSlackChannels d.botToken d.teamId
  discordChannels <- Slack.getDiscordDataByProjectId pid >>= maybe (pure []) (Discord.getDiscordChannels appCtx.env.discordBotToken . (.guildId))
  case teamVm of
    Just team -> do
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , pageTitle = "Team details"
              , currProject = Just project
              , config = appCtx.config
              , pageActions =
                  if team.is_everyone
                    then Nothing
                    else Just $ label_ [class_ "btn btn-sm btn-outline gap-2", Lucid.for_ $ team.handle <> "-new-team-modal"] do
                      faSprite_ "pen-to-square" "regular" "h-4 w-4"
                      "Edit"
              }
      case layoutM of
        Just _ -> addRespHeaders $ ManageTeamGet' (pid, team, projMembers, channels, discordChannels)
        _ -> addRespHeaders $ ManageTeamGet (PageCtx bwconf (pid, team, projMembers, channels, discordChannels))
    Nothing -> do
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , pageTitle = "Team details"
              , currProject = Just project
              , config = appCtx.config
              }
      addRespHeaders $ ManageTeamGetError (PageCtx bwconf (pid, handle))


teamPage :: Projects.ProjectId -> ProjectMembers.TeamVM -> V.Vector ProjectMembers.ProjectMemberVM -> [BotUtils.Channel] -> [BotUtils.Channel] -> Html ()
teamPage pid team projMembers slackChannels discordChannels = do
  let whiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= (x.first_name <> " " <> x.last_name), "email" AE..= x.email, "value" AE..= x.userId]) <$> projMembers
      emailWhiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= x.email, "value" AE..= x.email]) <$> projMembers
      (channelWhiteList, discordWhiteList) = (encodeChannels slackChannels, encodeChannels discordChannels)
      isEveryone = team.is_everyone
      notifRow_ icon iconType lbl vals inherited = div_ [class_ "flex items-start gap-3 py-2.5"] do
        _ <- span_ [class_ "w-5 h-5 flex items-center justify-center shrink-0"] $ faSprite_ icon iconType "h-4 w-4 text-iconNeutral"
        div_ [class_ "flex-1 min-w-0"] do
          _ <- div_ [class_ "text-sm font-medium text-textStrong"] lbl
          div_ [class_ "text-xs text-textWeak mt-0.5 break-words"] do
            if null vals then span_ [class_ "italic"] "Not configured" else toHtml $ T.intercalate ", " vals
            when inherited $ span_ [class_ "ml-1 text-textBrand"] "· from Integrations"
      resolveChannel chans cid = maybe cid (("#" <>) . (.channelName)) $ find (\x -> x.channelId == cid) chans
      lazySection_ secId icon title searchPh url = div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
        _ <- div_ [class_ "flex items-center justify-between w-full p-4 border-b border-strokeWeak"] do
          _ <- span_ [class_ "flex items-center gap-2 text-sm font-semibold text-textStrong"] (faSprite_ icon "regular" "h-4 w-4" >> toHtml title)
          label_ [class_ "input input-sm w-64 bg-fillWeak border-0"] do
            faSprite_ "magnifying-glass" "regular" "h-3.5 w-3.5 text-iconNeutral"
            input_ [type_ "text", placeholder_ searchPh, class_ "", makeAttribute "_" $ "on input show <tr/> in #" <> secId <> " when its textContent.toLowerCase() contains my value.toLowerCase()"]
        div_ [class_ "w-full max-h-96 overflow-y-auto", id_ secId] do
          unless (T.null url) $ a_ [hxGet_ url, hxTrigger_ "intersect once", hxTarget_ $ "#" <> secId, hxSwap_ "outerHTML"] ""
          div_ [class_ "flex flex-col items-center justify-center py-8 text-center gap-2"] (faSprite_ icon "regular" "h-6 w-6 text-iconNeutral" >> div_ [class_ "text-sm text-textWeak"] (toHtml $ "No " <> T.toLower title <> " linked"))
  section_ [id_ "main-content", class_ "w-full py-8"] $ div_ [class_ "max-md:px-2 px-4 w-full"] do
    div_ [class_ "mb-6 flex items-center gap-3"] do
      a_ [href_ ("/p/" <> pid.toText <> "/manage_teams"), class_ "text-iconNeutral hover:text-iconBrand"] $ faSprite_ "arrow-left" "regular" "h-4 w-4"
      h2_ [class_ "text-textStrong text-3xl font-semibold flex items-center gap-2"] do
        toHtml team.name
        when isEveryone $ span_ [class_ "badge badge-primary"] "Default"
    when isEveryone
      $ div_ [class_ "rounded-lg bg-fillBrand-weak p-4 text-sm text-textStrong mb-6"] do
        faSprite_ "circle-info" "regular" "h-4 w-4 inline mr-2"
        "@everyone automatically includes all project members. "
        "Channels configured on the "
        a_ [href_ ("/p/" <> pid.toText <> "/settings/integrations"), class_ "text-textBrand underline"] "Integrations page"
        " are automatically included in @everyone."
    div_ [class_ "flex gap-4 h-full"] do
      div_ [class_ "w-4/12 space-y-4"] do
        panel_ def{raised = True, icon = Just "circle-info"} "Details" do
          div_ [class_ "space-y-2 text-sm"] do
            div_ [class_ "flex gap-2"] $ span_ [class_ "text-textWeak w-16"] "Handle" >> span_ [class_ "text-textStrong"] (toHtml $ "@" <> team.handle)
            unless (T.null team.description) $ div_ [class_ "flex gap-2"] $ span_ [class_ "text-textWeak w-16"] "About" >> span_ [class_ "text-textStrong"] (toHtml team.description)
        let memberRow_ avatar name email = div_ [class_ "flex items-center gap-3 py-2.5"] $ img_ [src_ avatar, class_ "w-8 h-8 rounded-full border border-strokeWeak", term "loading" "lazy", term "decoding" "async"] >> div_ [] (div_ [class_ "text-sm font-medium text-textStrong"] (toHtml name) >> div_ [class_ "text-xs text-textWeak"] (toHtml email))
            members = if isEveryone then (\m -> ("/api/avatar/" <> m.userId.toText, m.first_name <> " " <> m.last_name, original m.email)) <$> projMembers else (\m -> (m.memberAvatar, m.memberName, m.memberEmail)) <$> team.members
        panel_ def{raised = True, icon = Just "users", subtitle = Just $ " (" <> show (V.length members) <> ")"} "Members" do
          div_ [class_ "divide-y divide-strokeWeak -mx-4"] $ forM_ members \(avatar, name, email) -> div_ [class_ "px-4"] $ memberRow_ avatar name email
        panel_ def{raised = True, icon = Just "bell"} "Notifications" $ div_ [class_ "divide-y divide-strokeWeak -mx-4"] do
          div_ [class_ "px-4"] $ notifRow_ "envelope" "regular" "Email" (V.toList team.notify_emails) isEveryone
          div_ [class_ "px-4"] $ notifRow_ "slack" "solid" "Slack" (V.toList $ V.map (resolveChannel slackChannels) team.slack_channels) isEveryone
          div_ [class_ "px-4"] $ notifRow_ "discord" "solid" "Discord" (V.toList $ V.map (resolveChannel discordChannels) team.discord_channels) isEveryone
          div_ [class_ "px-4"] $ notifRow_ "whatsapp" "solid" "WhatsApp" (V.toList team.phone_numbers) False
          div_ [class_ "px-4"] $ notifRow_ "pager" "solid" "PagerDuty" (V.toList team.pagerduty_services) False
          unless isEveryone do
            let hasAnyChannel = not (V.null team.notify_emails && V.null team.slack_channels && V.null team.discord_channels && V.null team.phone_numbers && V.null team.pagerduty_services)
            div_ [class_ "px-4 pt-3 pb-2"] do
              div_ [class_ "rounded-lg bg-fillBrand-weak/10 border border-strokeBrand-weak p-3"] do
                div_ [class_ "flex items-start justify-between gap-3"] do
                  div_ [class_ "flex items-start gap-2 flex-1"] do
                    faSprite_ "circle-info" "regular" "h-4 w-4 text-iconBrand shrink-0"
                    div_ [class_ "text-xs"] do
                      div_ [class_ "font-medium text-textStrong"] "Test your notification setup"
                      div_ [class_ "text-textWeak mt-0.5"] "Sends a test incident to all configured channels"
                  form_ [hxPost_ [text|/p/${pid.toText}/settings/integrations/test|], hxSwap_ "none", hxTrigger_ "submit", class_ "shrink-0"] do
                    input_ [type_ "hidden", name_ "channel", value_ "all"]
                    input_ [type_ "hidden", name_ "teamId", value_ $ UUID.toText team.id]
                    input_ [type_ "hidden", name_ "issueType", value_ "runtime_exception"]
                    button_ ([type_ "submit", class_ "btn btn-xs btn-primary tap-target", [__| on htmx:afterRequest from closest <form/> trigger testSent on body |]] <> bool [] [disabled_ ""] (not hasAnyChannel)) do
                      faSprite_ "flask-vial" "regular" "h-3.5 w-3.5"
                      " Send Test"
              div_ [id_ $ "team-test-history-" <> UUID.toText team.id, hxGet_ [text|/p/${pid.toText}/settings/integrations/history|], hxTrigger_ "testSent from:body", hxSwap_ "innerHTML", class_ "mt-3"] mempty
      div_ [class_ "flex-1 space-y-4"] do
        lazySection_ "monitors-section" "bell" "Monitors" "Search monitors..." ("/p/" <> pid.toText <> "/monitors/alerts/team/" <> UUID.toText team.id)
        lazySection_ "dashboards-section" "chart-area" "Dashboards" "Search dashboards..." ("/p/" <> pid.toText <> "/dashboards/?teamId=" <> UUID.toText team.id)
        lazySection_ "services-section" "server" "Services" "Search services..." ""
    unless isEveryone $ teamModal pid (Just team) whiteList emailWhiteList channelWhiteList discordWhiteList True mempty


teamPageNF :: Projects.ProjectId -> Text -> Html ()
teamPageNF pid handle = do
  section_ [id_ "main-content", class_ "w-full py-16"] do
    div_ [class_ "p-6 w-[606px] mx-auto"] do
      h2_ [class_ "text-textStrong mb-4 text-xl font-semibold"] $ "Team not found: " <> toHtml handle
      p_ [class_ "text-textWeak text-sm leading-tight"] "We couldn't find the team you're looking for."


manageMembersGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders ManageMembers)
manageMembersGetH pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  projMembers <- V.fromList <$> ProjectMembers.selectAllProjectMembers pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Team"
          , currProject = Just project
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ ManageMembersGet $ PageCtx bwconf (pid, projMembers, project.paymentPlan)


data ManageMembers
  = ManageMembersGet {unwrapGet :: PageCtx (Projects.ProjectId, V.Vector ProjectMembers.ProjectMemberWithStatusVM, Text)}
  | ManageMembersPost {unwrapPost :: (Projects.ProjectId, V.Vector ProjectMembers.ProjectMemberWithStatusVM, Text)}


instance ToHtml ManageMembers where
  toHtml (ManageMembersGet (PageCtx bwconf (pid, members, plan))) = toHtml $ PageCtx bwconf $ manageMembersBody pid members plan
  toHtml (ManageMembersPost (pid, members, plan)) = toHtml $ manageMembersBody pid members plan
  toHtmlRaw = toHtml


manageMembersBody :: Projects.ProjectId -> V.Vector ProjectMembers.ProjectMemberWithStatusVM -> Text -> Html ()
manageMembersBody pid projMembers paymentPlan =
  settingsSection_ do
    settingsH2_ "Team"

    -- Tabs: Members | Teams
    div_ [class_ "flex gap-1 border-b border-strokeWeak", [__| on load if window.location.hash is "#teams" then send click to #teams-tab-btn end |]] do
      button_
        [ class_ "px-4 py-2 text-sm font-medium border-b-2 border-textBrand text-textBrand a-tab-btn"
        , id_ "members-tab-btn"
        , term "data-target" "#members-tab-content"
        , term "_" "on click remove .border-textBrand .text-textBrand from .a-tab-btn then add .border-transparent .text-textWeak to .a-tab-btn then remove .border-transparent .text-textWeak from me then add .border-textBrand .text-textBrand to me then add .hidden to .a-tab-panel then remove .hidden from #members-tab-content"
        ]
        "Members"
      button_
        [ class_ "px-4 py-2 text-sm font-medium border-b-2 border-transparent text-textWeak a-tab-btn"
        , id_ "teams-tab-btn"
        , term "data-target" "#teams-tab-content"
        , term "_" "on click remove .border-textBrand .text-textBrand from .a-tab-btn then add .border-transparent .text-textWeak to .a-tab-btn then remove .border-transparent .text-textWeak from me then add .border-textBrand .text-textBrand to me then add .hidden to .a-tab-panel then remove .hidden from #teams-tab-content then send loadTeams to #teams-tab-content"
        ]
        "Teams"

    -- Members tab
    div_ [id_ "members-tab-content", class_ "a-tab-panel space-y-6"] do
      when (paymentPlan == "Free" && V.length projMembers > 1)
        $ div_ [class_ "bg-fillWarning-weak border border-strokeWarning-weak rounded-xl p-4 flex items-start gap-3"] do
          faSprite_ "triangle-exclamation" "regular" "w-5 h-5 text-iconWarning flex-shrink-0 mt-0.5"
          div_ do
            p_ [class_ "text-sm text-textStrong font-medium"] "Free plan allows only 1 team member"
            p_ [class_ "text-sm text-textWeak mt-1"] "Additional team members are disabled and cannot access the project. Upgrade to enable team access."

      form_ [class_ "space-y-6", hxPost_ "", hxTarget_ settingsContentTarget, hxSwap_ "innerHTML", hxIndicator_ "#submitIndicator"] do
        div_ [class_ "space-y-3"] do
          label_ [class_ "text-sm font-medium text-textStrong block"] "Invite new member"
          div_ [class_ "flex gap-2"] do
            input_ [type_ "email", name_ "emails", class_ "input input-sm input-bordered flex-1", placeholder_ "colleague@company.com", required_ "true"]
            select_ [name_ "permissions", class_ "select select-sm select-bordered w-28"] do
              option_ [value_ "admin"] "Admin"
              option_ [value_ "edit"] "Editor"
              option_ [value_ "view"] "Viewer"
            button_ [class_ "btn btn-sm btn-primary gap-1", hxIndicator_ "#inviteIndicator"] do
              htmxIndicator_ "inviteIndicator" LdXS
              faSprite_ "paper-plane" "regular" "w-3 h-3"
              span_ "Invite"

        div_ [class_ "border-t border-strokeWeak"] ""

        div_ [class_ "space-y-3"] do
          div_ [class_ "flex items-center justify-between"] do
            h3_ [class_ "text-sm font-medium text-textStrong"] $ toHtml $ "Team members (" <> show (V.length projMembers) <> ")"
            when (V.length projMembers > 0) $ button_ [class_ "btn btn-sm btn-ghost text-textWeak", disabled_ "true", id_ "saveMembersBtn", dirtyFormSaveAttr_] "Save changes"
          div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak overflow-hidden"]
            $ if V.null projMembers
              then div_ [class_ "py-6 text-center text-textWeak text-sm"] "No members yet. Invite someone to get started."
              else V.imapM_ (memberRowWithStatus pid) projMembers

    -- Teams tab (lazy loaded)
    div_ [id_ "teams-tab-content", class_ "a-tab-panel hidden", hxGet_ $ "/p/" <> pid.toText <> "/manage_teams?what=partial", hxTrigger_ "loadTeams once", hxSwap_ "innerHTML", hxSelect_ settingsContentTarget] do
      div_ [class_ "flex justify-center py-8"] do
        span_ [class_ "loading loading-spinner loading-md"] ""


memberRowWithStatus :: Projects.ProjectId -> Int -> ProjectMembers.ProjectMemberWithStatusVM -> Html ()
memberRowWithStatus pid idx prM = do
  let email = CI.original prM.email
      memberId = RealUUID.toText prM.id
      isOwner = idx == 0
      isDisabled = not prM.active && not isOwner
  div_ [class_ $ "px-4 py-3 flex items-center gap-3" <> if isDisabled then " opacity-60" else "", id_ $ "member-" <> memberId] do
    div_ [class_ "flex-1 min-w-0"] $ div_ [class_ "flex items-center gap-2"] do
      input_ [type_ "text", name_ "emails", value_ email, readonly_ "true", class_ $ "bg-transparent w-full text-sm truncate focus:outline-none cursor-default" <> if isDisabled then " text-textWeak" else " text-textStrong"]
      when isOwner $ span_ [class_ "badge badge-sm bg-fillBrand-weak text-textBrand border-strokeBrand-weak"] "Owner"
      when isDisabled $ span_ [class_ "badge badge-sm bg-fillWarning-weak text-textWarning border-strokeWarning-weak"] "Disabled"
    div_ [class_ "flex items-center gap-2"] do
      select_ [name_ "permissions", class_ $ "select select-sm select-bordered text-sm" <> if isDisabled then " opacity-50" else ""] do
        option_ ([value_ "admin"] <> [selected_ "" | prM.permission == ProjectMembers.PAdmin]) "Admin"
        option_ ([value_ "edit"] <> [selected_ "" | prM.permission == ProjectMembers.PEdit]) "Editor"
        option_ ([value_ "view"] <> [selected_ "" | prM.permission == ProjectMembers.PView]) "Viewer"
      unless isOwner $ button_ [class_ "btn btn-sm btn-ghost text-iconNeutral hover:text-iconError hover:bg-fillError-weak", Aria.label_ "Remove member", hxDelete_ $ "/p/" <> pid.toText <> "/manage_members/" <> memberId, hxTarget_ $ "#member-" <> memberId, hxSwap_ "outerHTML", hxConfirm_ "Remove this member from the project?"] $ faSprite_ "trash" "regular" "w-4 h-4"


deleteMemberH :: Projects.ProjectId -> RealUUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
deleteMemberH pid memberId = do
  (sess, project) <- Projects.sessionAndProject pid
  let currUserId = sess.persistentSession.userId
  projMembers <- ProjectMembers.selectActiveProjectMembers pid
  let memberM = find (\m -> m.id == memberId) projMembers
  case memberM of
    Nothing -> do
      addErrorToast "Member not found" Nothing
      addRespHeaders ""
    Just member ->
      if member.userId == currUserId
        then do
          addErrorToast "You cannot remove yourself" Nothing
          addRespHeaders ""
        else do
          _ <- ProjectMembers.softDeleteProjectMembers (memberId :| [])
          Projects.logAuditS pid Projects.AEMemberRemoved sess
            $ Just
            $ AE.object ["removed_email" AE..= CI.original member.email]
          addSuccessToast "Member removed" Nothing
          addRespHeaders ""


manageSubGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
manageSubGetH pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  case Projects.billingProvider project.subId of
    Projects.StripeProvider -> do
      -- order_id stores Stripe customer ID for Stripe users
      case project.orderId of
        Just customerId | not (T.null customerId) -> do
          let returnUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/manage_billing"
          portalUrlM <- liftIO $ Settings.createStripePortalSession envCfg.stripeSecretKey customerId returnUrl
          case portalUrlM of
            Just url -> redirectCS url >> addRespHeaders ""
            Nothing -> addErrorToast "Failed to create billing portal" Nothing >> addRespHeaders ""
        _ -> addErrorToast "Customer ID not found" Nothing >> addRespHeaders ""
    Projects.LemonSqueezyProvider -> do
      sub <- liftIO $ getSubscriptionPortalUrl project.subId envCfg.lemonSqueezyApiKey
      case sub of
        Nothing -> addErrorToast "Subscription ID not found" Nothing >> addRespHeaders ""
        Just s -> redirectCS s.dataVal.attributes.urls.customerPortal >> addRespHeaders ""
    Projects.NoBillingProvider -> addErrorToast "No active subscription" Nothing >> addRespHeaders ""


newtype StripeCheckoutForm = StripeCheckoutForm {plan :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


stripeCheckoutInitH :: Projects.ProjectId -> StripeCheckoutForm -> ATAuthCtx (RespHeaders (Html ()))
stripeCheckoutInitH pid form = do
  void $ Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  urlM <-
    liftIO
      $ Settings.createStripeCheckoutSession
        envCfg.stripeSecretKey
        envCfg.hostUrl
        pid
        form.plan
        envCfg.stripePriceIdGraduated
        envCfg.stripePriceIdGraduatedOverage
        envCfg.stripePriceIdByos
  case urlM of
    Just url -> redirectCS url >> addRespHeaders ""
    Nothing -> addErrorToast "Failed to create checkout session" Nothing >> addRespHeaders ""


getSubscriptionPortalUrl :: Maybe Text -> Text -> IO (Maybe SubPortalResponse)
getSubscriptionPortalUrl subId apiKey = do
  case subId of
    Nothing -> return Nothing
    Just sid -> do
      let hds = W.header "Authorization" .~ ["Bearer " <> encodeUtf8 @Text @ByteString apiKey]
      response <- liftIO $ Network.Wreq.getWith (defaults & hds) ("https://api.lemonsqueezy.com/v1/subscriptions/" <> toString sid)
      let responseBdy = response ^. responseBody
      case AE.eitherDecode responseBdy of
        Right res -> return $ Just res
        Left err -> return Nothing


data SubUrls = SubUrls
  { updatePaymentMethod :: Text
  , customerPortal :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SubUrls


newtype SubPortalAttributes = SubPortalAttributes {urls :: SubUrls}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SubPortalAttributes


newtype SubPortalDataVals = SubPortalDataVals {attributes :: SubPortalAttributes}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SubPortalDataVals


newtype SubPortalResponse = SubPortalResponse {dataVal :: SubPortalDataVals}
  deriving stock (Generic, Show)


instance AE.FromJSON SubPortalResponse where
  parseJSON = AE.withObject "SubPortalResponse" $ \obj -> do
    dataVal <- obj AE..: "data"
    return (SubPortalResponse{dataVal = dataVal})


--------------------------------------------------------------------------------
-- CreateProject
--------------------------------------------------------------------------------

data CreateProjectFormError = CreateProjectFormError
  { titleE :: Maybe [String]
  , descriptionE :: Maybe [String]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default)


createProjectFormToModel :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> CreateProjectForm -> Projects.CreateProject
createProjectFormToModel pid subId firstSubId orderId paymentPlan CreateProjectForm{..} =
  Projects.CreateProject
    { id = pid
    , subId = subId
    , firstSubItemId = firstSubId
    , paymentPlan = paymentPlan
    , orderId = orderId
    , weeklyNotif = isJust weeklyNotifs
    , dailyNotif = isJust dailyNotifs
    , endpointAlerts = isJust endpointAlerts
    , errorAlerts = isJust errorAlerts
    , ..
    }


createProjectFormV :: Monad m => Valor CreateProjectForm m CreateProjectFormError
createProjectFormV =
  CreateProjectFormError
    <$> check1 (.title) (failIf ["name can't be empty"] T.null)
    <*> check1 (.description) Valor.pass


projectOnboardingH :: ATAuthCtx (Headers '[Header "Location" Text] (PageCtx (Html ())))
projectOnboardingH = do
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess <- Projects.getSession
  projects <- Projects.selectProjectsForUser sess.persistentSession.userId
  let projectM = find (\pr -> pr.paymentPlan == "ONBOARDING") projects
      bwconf = (def :: BWConfig){sessM = Just sess, currProject = Nothing, pageTitle = "New Project", config = appCtx.config}
  case projectM of
    Just p -> do
      let h = "/p/" <> p.id.toText <> "/onboarding"
      pure $ addHeader h $ PageCtx bwconf ""
    _ -> do
      pid <- UUIDId <$> UUID.genUUID
      let pr = Projects.CreateProject{id = pid, title = "Onboarding Project", description = "", paymentPlan = "ONBOARDING", timeZone = "", subId = Nothing, firstSubItemId = Nothing, orderId = Nothing, weeklyNotif = True, dailyNotif = True, endpointAlerts = True, errorAlerts = True}
      _ <- Projects.insertProject pr
      _ <- ProjectMembers.createEveryoneTeam pid sess.user.id
      projectKeyUUID <- UUID.genUUID
      let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 $ ProjectApiKeys.encryptAPIKey (encodeUtf8 envCfg.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
      pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID "Default API Key" encryptedKeyB64
      _ <- ProjectApiKeys.insertProjectApiKey pApiKey
      let projectMember = ProjectMembers.CreateProjectMembers pid sess.user.id ProjectMembers.PAdmin
      _ <- ProjectMembers.insertProjectMembers [projectMember]
      Projects.logAuditS pid Projects.AEProjectCreated sess Nothing
      let h = "/p/" <> pid.toText <> "/onboarding"
      pure $ addHeader h $ PageCtx bwconf ""


data CreateProjectResp = CreateProjectResp
  { sess :: Projects.PersistentSession
  , pid :: Projects.ProjectId
  , env :: EnvConfig
  , paymentPlan :: Text
  , form :: CreateProjectForm
  , formError :: CreateProjectFormError
  , pro :: Projects.Project
  }
  deriving stock (Generic, Show)


data CreateProject
  = CreateProject (PageCtx (Projects.PersistentSession, Projects.ProjectId, EnvConfig, Text, Bool, CreateProjectForm, CreateProjectFormError, Projects.Project))
  | PostNoContent Text
  | ProjectPost CreateProjectResp
  deriving stock (Generic, Show)


instance HasField "unwrapCreateProjectResp" CreateProject (Maybe CreateProjectResp) where
  getField (CreateProject _) = Nothing
  getField (PostNoContent _) = Nothing
  getField (ProjectPost cpr) = Just cpr


instance ToHtml CreateProject where
  toHtml (CreateProject (PageCtx bwconf (sess, pid, config, paymentPlan, isUpdate, prf, pref, pro))) = toHtml $ PageCtx bwconf $ createProjectBody sess pid config paymentPlan prf pref pro
  toHtml (PostNoContent message) = span_ [class_ ""] $ toHtml message
  toHtml (ProjectPost cpr) = toHtml $ createProjectBody cpr.sess cpr.pid cpr.env cpr.paymentPlan cpr.form cpr.formError cpr.pro
  toHtmlRaw = toHtml


projectSettingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders CreateProject)
projectSettingsGetH pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let createProj =
        CreateProjectForm
          { title = project.title
          , description = project.description
          , emails = []
          , permissions = []
          , timeZone = project.timeZone
          , weeklyNotifs = if project.weeklyNotif then Just "on" else Nothing
          , dailyNotifs = if project.dailyNotif then Just "on" else Nothing
          , errorAlerts = if project.errorAlerts then Just "on" else Nothing
          , endpointAlerts = if project.endpointAlerts then Just "on" else Nothing
          }

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Project"
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ CreateProject $ PageCtx bwconf (sess.persistentSession, pid, appCtx.config, project.paymentPlan, True, createProj, def @CreateProjectFormError, project)


deleteProjectGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders CreateProject)
deleteProjectGetH pid = do
  sess <- Projects.getSession
  appCtx <- ask @AuthContext
  if isDemoAndNotSudo pid sess.user.isSudo
    then do
      addSuccessToast "Can't perform this action on the demon project" Nothing
      addRespHeaders $ PostNoContent ""
    else do
      _ <- Projects.deleteProject pid
      _ <- liftIO $ withResource appCtx.pool \conn ->
        createJob conn "background_jobs" $ BackgroundJobs.DeletedProject pid
      Projects.logAuditS pid Projects.AEProjectDeleted sess Nothing
      addSuccessToast "Deleted Project Successfully" Nothing
      redirectCS "/"
      addRespHeaders $ PostNoContent ""


createProjectPostH :: Projects.ProjectId -> CreateProjectForm -> ATAuthCtx (RespHeaders CreateProject)
createProjectPostH pid createP = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Right cpe -> addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid appCtx.config "" createP cpe project)
    Left cp -> processProjectPostForm cp pid


data SubAttributes = SubAttributes
  { firstSubscriptionItem :: Settings.FirstSubItem
  , productName :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SubAttributes


data SubDataVals = SubDataVals
  { id :: Text
  , attributes :: SubAttributes
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SubDataVals


newtype SubResponse = SubResponse {dataVal :: [SubDataVals]}
  deriving stock (Generic, Show)


instance AE.FromJSON SubResponse where
  parseJSON = AE.withObject "SubResponse" $ \obj -> do
    dataVal <- obj AE..: "data"
    return (SubResponse{dataVal = dataVal})


getSubscriptionId :: HTTP :> es => Maybe Text -> Text -> Eff es (Maybe SubResponse)
getSubscriptionId orderId apiKey = do
  case orderId of
    Nothing -> pure Nothing
    Just ordId -> do
      let hds = W.header "Authorization" .~ ["Bearer " <> encodeUtf8 @Text @ByteString apiKey]
      response <- W.getWith (defaults & hds) ("https://api.lemonsqueezy.com/v1/orders/" <> toString ordId <> "/subscriptions")
      let responseBdy = response ^. responseBody
      case AE.eitherDecode responseBdy of
        Right res -> return $ Just res
        Left err -> return Nothing


data PricingUpdateForm = PricingUpdateForm
  { orderIdM :: Maybe Text
  , plan :: Maybe Text
  , isOnboarding :: Maybe Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromForm)


pricingUpdateH :: Projects.ProjectId -> PricingUpdateForm -> ATAuthCtx (RespHeaders (Html ()))
pricingUpdateH pid PricingUpdateForm{orderIdM, plan, isOnboarding} = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
      apiKey = envCfg.lemonSqueezyApiKey
      steps = project.onboardingStepsCompleted
      newStepsComp = insertIfNotExist "Pricing" steps
      updatePricing name sid fid oid = Projects.updateProjectPricing pid name sid fid oid newStepsComp
      handleOnboarding name = when (project.paymentPlan == "ONBOARDING") $ do
        _ <- liftIO $ withResource appCtx.pool \conn -> do
          let fullName = sess.user.firstName <> " " <> sess.user.lastName
              foundUsFrom = fromMaybe "" $ project.questions >>= (`lookupValueText` "foundUsFrom")
          createJob conn "background_jobs" $ BackgroundJobs.SendDiscordData sess.user.id pid fullName [foundUsFrom] foundUsFrom
        users <- ProjectMembers.selectActiveProjectMembers pid
        unless (T.null envCfg.convertkitApiKey)
          $ forM_ users
          $ \user -> addConvertKitUserOrganization envCfg.convertkitApiKey (CI.original user.email) pid.toText project.title name

  let billingUrl = envCfg.hostUrl <> "p/" <> pid.toText <> "/manage_billing"
      auditPlan name =
        Projects.logAuditS pid Projects.AEPlanChanged sess
          $ Just
          $ AE.object ["plan" AE..= name]
      notifyPlanChange email = sendRenderedEmail (CI.original email)
  case plan of
    Just "Open Source" | envCfg.basicAuthEnabled -> do
      _ <- updatePricing "Open Source" "" "" ""
      auditPlan ("Open Source" :: Text)
      handleOnboarding "Open Source"
      void $ ProjectMembers.activateAllMembers pid
    _ -> case orderIdM of
      Just orderId ->
        getSubscriptionId (Just orderId) apiKey >>= \case
          Just sub | not (null sub.dataVal) -> do
            let target = sub.dataVal Unsafe.!! 0
                subId = show target.attributes.firstSubscriptionItem.subscriptionId
                firstSubId = show target.attributes.firstSubscriptionItem.id
                productName = target.attributes.productName
            _ <- updatePricing productName subId firstSubId orderId
            auditPlan productName
            handleOnboarding productName
            void $ ProjectMembers.activateAllMembers pid
            let (subj, html) = ET.planUpgradedEmail project.title productName billingUrl
            users <- Projects.usersByProjectId pid
            forM_ users \u -> notifyPlanChange u.email subj (ET.renderEmail subj html)
          _ -> addErrorToast "Something went wrong while fetching subscription id" Nothing
      Nothing -> do
        _ <- updatePricing "Free" "" "" ""
        auditPlan ("Free" :: Text)
        handleOnboarding "Free"
        void $ ProjectMembers.deactivateNonOwnerMembers pid
        let (subj, html) = ET.planDowngradedEmail project.title "was cancelled" billingUrl
        users <- Projects.usersByProjectId pid
        forM_ users \u -> notifyPlanChange u.email subj (ET.renderEmail subj html)
  if project.paymentPlan == "ONBOARDING" || isOnboarding == Just True
    then do
      redirectCS $ "/p/" <> pid.toText <> "/"
      addRespHeaders ""
    else do
      addTriggerEvent "closeModal" ""
      addSuccessToast "Pricing updated successfully" Nothing
      addRespHeaders ""


processProjectPostForm :: Valor.Valid CreateProjectForm -> Projects.ProjectId -> ATAuthCtx (RespHeaders CreateProject)
processProjectPostForm cpRaw pid = do
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  (sess, project) <- Projects.sessionAndProject pid

  let cp = Valor.unValid cpRaw
  if isDemoAndNotSudo pid sess.user.isSudo
    then do
      addErrorToast "Can't perform this action on the demo project" Nothing
      addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid envCfg "" cp (def @CreateProjectFormError) project)
    else do
      _ <- Projects.updateProject (createProjectFormToModel pid project.subId project.firstSubItemId project.orderId project.paymentPlan cp)
      Projects.logAuditS pid Projects.AEProjectUpdated sess Nothing
      addSuccessToast "Updated Project Successfully" Nothing
      addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid envCfg "" cp (def @CreateProjectFormError) project)


createProjectBody :: Projects.PersistentSession -> Projects.ProjectId -> EnvConfig -> Text -> CreateProjectForm -> CreateProjectFormError -> Projects.Project -> Html ()
createProjectBody sess pid envCfg paymentPlan cp cpe proj = do
  settingsSection_ do
    settingsH2_ "Project Settings"

    form_
      [ class_ "space-y-5 sm:space-y-8"
      , hxPost_ $ "/p/update/" <> pid.toText
      , hxTarget_ settingsContentTarget
      , hxSwap_ "innerHTML"
      , id_ "createUpdateBodyForm"
      , hxIndicator_ "#createIndicator"
      , [__| on change add .form-dirty to me |]
      ]
      do
        -- Project details
        div_ [class_ "space-y-4"] do
          formField_ FieldSm def{value = cp.title, placeholder = "My Project"} "Project Name" "title" True Nothing
          formSelectField_ FieldSm "Timezone" "timeZone" False do
            option_ [value_ cp.timeZone] $ toHtml cp.timeZone
          formField_ FieldSm def{inputType = "textarea", value = cp.description, placeholder = "What is this project about?", extraAttrs = [rows_ "3"]} "Description" "description" False Nothing

        -- Alert configuration
        div_ [class_ "border-t border-strokeWeak pt-1"]
          $ alertConfiguration (isJust cp.endpointAlerts) (isJust cp.errorAlerts) (isJust cp.weeklyNotifs) (isJust cp.dailyNotifs)

        -- Save button: muted until form is dirty
        div_ [class_ "flex justify-end pt-2"] do
          button_ [id_ "saveBtn", class_ "btn gap-1.5 btn-ghost text-textWeak max-sm:btn-block max-sm:btn-md sm:btn-sm", type_ "submit", disabled_ "true", dirtyFormSaveAttr_] do
            htmxIndicator_ "createIndicator" LdXS
            faSprite_ "floppy-disk" "regular" "w-3 h-3"
            span_ "Save Changes"

    script_ do
      [text|
           (() => {
             const timezoneSelect = document.getElementById("timeZone");
             const timeZones = Intl.supportedValuesOf('timeZone');
             timeZones.forEach((tz) => {
               const option = document.createElement("option");
               option.value = tz;
               option.text = tz;
               timezoneSelect.appendChild(option);
             });
           })();
        |]

    -- Danger zone — compact
    div_ [class_ "border border-strokeError-weak rounded-xl p-4 flex max-sm:flex-col sm:items-center sm:justify-between gap-4"] do
      div_ [class_ "flex items-center gap-3 min-w-0"] do
        iconBadge_ ErrorBadge "triangle-alert"
        div_ [class_ "min-w-0"] do
          h3_ [class_ "text-sm font-medium text-textStrong"] "Delete project"
          p_ [class_ "text-xs text-textWeak"] "Permanently remove this project and all associated data."
      label_
        [ class_ "btn btn-sm bg-fillError-weak text-textError hover:bg-fillError-strong hover:text-white gap-1 shrink-0 max-sm:w-full"
        , Lucid.for_ "delete-project-modal"
        ]
        do
          faSprite_ "trash" "regular" "w-3 h-3"
          span_ "Delete Project"
    confirmModal_
      "delete-project-modal"
      "Delete project?"
      "This permanently removes the project and all associated data. This action cannot be undone."
      [type_ "button", hxDelete_ $ "/p/" <> pid.toText <> "/delete"]
      "Delete project"


alertConfiguration :: Bool -> Bool -> Bool -> Bool -> Html ()
alertConfiguration newEndpointsAlerts errorAlerts weeklyReportsAlerts dailyReportsAlerts =
  div_ do
    div_ [class_ "flex items-center gap-2 mb-3"] do
      iconBadgeXs_ NeutralBadge "bell"
      span_ [class_ "text-sm font-semibold text-textStrong"] "Notifications"
    div_ [class_ "divide-y divide-strokeWeak"] do
      switchRow "New endpoint alerts" "endpointAlerts" "Get notified when new API endpoints are detected" newEndpointsAlerts
      switchRow "Runtime error alerts" "errorAlerts" "Receive immediate notifications for system errors" errorAlerts
      switchRow "Weekly reports" "weeklyNotifs" "Get a summary of your project activity every week" weeklyReportsAlerts
      switchRow "Daily reports" "dailyNotifs" "Receive daily summaries of your project metrics" dailyReportsAlerts
  where
    switchRow lbl forId descr checked =
      div_ [class_ "flex items-center justify-between py-3 first:pt-0 last:pb-0"] do
        div_ [class_ "space-y-0.5"] do
          label_ [Lucid.for_ forId, class_ "text-sm font-medium text-textStrong cursor-pointer"] $ toHtml lbl
          p_ [class_ "text-xs text-textWeak"] $ toHtml descr
        input_ $ [type_ "checkbox", id_ forId, class_ "toggle toggle-sm", name_ forId] ++ [checked_ | checked]


-- Main Modal Component
teamModal :: Projects.ProjectId -> Maybe ProjectMembers.TeamVM -> Text -> Text -> Text -> Text -> Bool -> Html () -> Html ()
teamModal pid team whiteList emailWhiteList channelWhiteList discordWhiteList isInTeamView trigger = do
  let encodeField f = decodeUtf8 $ AE.encode $ maybe [] f team
      name = maybe "" (.name) team
      handle = maybe "" (.handle) team
      description = maybe "" (.description) team
      isEveryoneTeam = maybe False (.is_everyone) team
      prefix = maybe "n" (.handle) team
      modalId = prefix <> "-new-team-modal"
      mkId suffix = prefix <> "-" <> suffix
      membersTags = encodeField \t -> UUID.toText . (.memberId) <$> t.members
      notifEmails = encodeField (.notify_emails)
      slackChannels = encodeField (.slack_channels)
      discordChannels = encodeField (.discord_channels)
      pagerdutyServicesTags = encodeField (.pagerduty_services)
      field_ lbl fid attrs = formField_ FieldSm def lbl (mkId fid) False $ Just $ input_ $ [class_ "input w-full", type_ "text", id_ $ mkId fid, name_ fid] <> attrs
      notifField_ ic lbl fid ph attrs = formField_ FieldSm def{icon = Just ic} lbl (mkId fid) False $ Just $ tagInput_ (mkId fid) ph attrs

  modalWith_ modalId def{boxClass = "p-6 max-w-lg w-full"} (Just trigger) do
    form_ [hxPost_ $ "/p/" <> pid.toText <> "/manage_teams?" <> if isInTeamView then "teamView=true" else "", hxExt_ "json-enc", hxVals_ [text|js:{teamMembers: window.getTagValues('#$prefix-team-members-input'), notifEmails: window.getTagValues('#$prefix-notif-emails-input'), slackChannels: window.getTagValues('#$prefix-slack-channels-input'), discordChannels: window.getTagValues('#$prefix-discord-channels-input'), pagerdutyServices: window.getTagValues('#$prefix-pagerduty-services-input'), phoneNumbers: []}|], hxSwap_ "none", class_ "flex flex-col gap-0 w-full"] do
      whenJust ((.id) <$> team) \tid -> input_ [type_ "hidden", name_ "teamId", value_ $ UUID.toText tid]
      h2_ [class_ "text-lg font-semibold text-textStrong"] $ toHtml (if isJust team then "Edit Team" else "Create Team" :: Text)

      div_ [class_ "flex-1 overflow-y-auto max-h-[60vh] mt-4 divide-y divide-strokeWeak"] do
        -- Details
        div_ [class_ "space-y-3 pb-4"] do
          div_ [class_ "grid grid-cols-2 gap-3"] do
            field_ "Team Name" "teamName" $ [placeholder_ "e.g. Backend Team", value_ name, required_ "true"] <> [disabled_ "" | isEveryoneTeam]
            field_ "Handle" "teamHandle" $ [placeholder_ "e.g. backend-team", value_ handle, required_ "true", pattern_ "^[a-z][a-z0-9\\-]*$"] <> [disabled_ "" | isEveryoneTeam]
          formField_ FieldSm def "Description" (mkId "teamDescription") False $ Just $ textarea_ [class_ "textarea w-full min-h-12 resize-none text-sm leading-relaxed", id_ $ mkId "teamDescription", name_ "teamDescription", placeholder_ "What does this team do?"] $ toHtml description

        -- Members
        unless isEveryoneTeam
          $ div_ [class_ "py-4 space-y-1"]
          $ do
            sectionLabel_ "Members"
            formField_ FieldSm def "" (mkId "team-members-input") False
              $ Just
              $ tagInput_
                (mkId "team-members-input")
                "Start typing to search members..."
                [data_ "tagify-whitelist" whiteList, data_ "tagify-enforce-whitelist" "", data_ "tagify-text-prop" "name", data_ "tagify-initial" membersTags, data_ "tagify-resolve" ""]

        when isEveryoneTeam
          $ div_ [class_ "py-4"]
          $ infoBanner_ do
            "The @everyone team automatically includes all project members. Use the "
            a_ [href_ ("/p/" <> pid.toText <> "/settings/integrations"), class_ "text-textBrand underline"] "Integrations page"
            " for project-wide channels."

        -- Notifications
        div_ [class_ "pt-4 space-y-2"] do
          sectionLabel_ "Notifications"
          div_ [class_ "divide-y divide-strokeWeak [&>*]:py-2.5"] do
            forM_
              ( [ ("envelope", "Email", "notif-emails-input", "Add email addresses...", [data_ "tagify-whitelist" emailWhiteList, data_ "tagify-text-prop" "name", data_ "tagify-initial" notifEmails])
                , ("slack", "Slack", "slack-channels-input", "Add Slack channels...", [data_ "tagify-whitelist" channelWhiteList, data_ "tagify-enforce-whitelist" "", data_ "tagify-text-prop" "name", data_ "tagify-initial" slackChannels, data_ "tagify-resolve" ""])
                , ("discord", "Discord", "discord-channels-input", "Add Discord channels...", [data_ "tagify-whitelist" discordWhiteList, data_ "tagify-enforce-whitelist" "", data_ "tagify-text-prop" "name", data_ "tagify-initial" discordChannels, data_ "tagify-resolve" ""])
                ]
                  :: [(Text, Text, Text, Text, [Attribute])]
              )
              \(ic, lbl, fid, ph, attrs) -> notifField_ ic lbl fid ph attrs
            formField_ FieldSm def{icon = Just "pager"} "PagerDuty" (mkId "pagerduty-services-input") False $ Just do
              tagInput_ (mkId "pagerduty-services-input") "Add integration keys..." [data_ "tagify-text-prop" "name", data_ "tagify-initial" pagerdutyServicesTags]
              details_ [class_ "group mt-1.5"] do
                summary_ [class_ "text-xs text-textWeak hover:text-textStrong cursor-pointer list-none flex items-center gap-1.5"] do
                  faSprite_ "chevron-right" "solid" "w-3 h-3 transition-transform rotate-180 group-open:rotate-270"
                  "How to get your integration key"
                ol_ [class_ "text-xs text-textWeak space-y-1 mt-2 ml-4 list-decimal list-inside"] do
                  li_ "Open PagerDuty → Services"
                  li_ "Select your service → Integrations tab"
                  li_ "Add 'Events API v2' integration"
                  li_ "Copy the Integration Key"

      formActionsModal_ modalId
        $ button_ [class_ "btn btn-sm btn-primary", type_ "submit"]
        $ toHtml (if isJust team then "Save Changes" else "Create Team" :: Text)
