{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pages.Projects (
  -- ListProjects
  listProjectsGetH,
  ListProjectsGet (..),
  -- Integrations
  CreateProjectForm (..),
  NotifListForm (..),
  integrationsSettingsGetH,
  updateNotificationsChannel,
  NotificationsUpdatePost (..),
  -- ManageMembers
  manageMembersGetH,
  manageMembersPostH,
  deleteMemberH,
  ManageMembersForm (..),
  manageSubGetH,
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
  projectDeleteGetH,
  projectSettingsGetH,
  projectOnboardingH,
  deleteProjectGetH,
  CreateProject (..),
  pricingUpdateGetH,
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
import Data.Effectful.UUID (UUID)
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq
import Data.Effectful.Wreq qualified as W
import Data.List.Unique (uniq)
import Data.Pool (withResource)
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
import Lucid (div_)
import Lucid.Htmx
import Lucid.Htmx (hxPost_)
import Lucid.Hyperscript (__)
import Models.Apis.Slack (SlackData, getDiscordDataByProjectId, getProjectSlackData)
import Models.Apis.Slack qualified as Slack
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers (TeamMemberVM (..), TeamVM (..))
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Network.Wreq (getWith)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), bodyWrapper)
import Pages.Bots.Discord qualified as Discord
import Pages.Bots.Slack qualified as SlackP
import Pages.Bots.Utils qualified as BotUtils
import Pages.Components (paymentPlanPicker)
import Pkg.Components.Table (BulkAction (..), Table (..))
import Pkg.Components.Table qualified as Table
import Pkg.Components.Widget (Widget (..), WidgetType (..), widget_)
import Pkg.ConvertKit qualified as ConvertKit
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import Servant (addHeader)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers)
import Servant.Server (err302, errHeaders)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addReswap, addSuccessToast, addTriggerEvent, redirectCS)
import Utils (faSprite_, insertIfNotExist, isDemoAndNotSudo, lookupValueText)
import Web.FormUrlEncoded (FromForm)
import "base64" Data.ByteString.Base64 qualified as B64


--------------------------------------------------------------------------------
-- ListProjects
--------------------------------------------------------------------------------

listProjectsGetH :: ATAuthCtx (RespHeaders ListProjectsGet)
listProjectsGetH = do
  (sess, project) <- Sessions.sessionAndProject (UUIDId RealUUID.nil)
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


listProjectsBody :: Maybe Sessions.Session -> V.Vector Projects.Project' -> Projects.Project' -> Bool -> Html ()
listProjectsBody sessM projects demoProject showDemoProject = do
  nav_ [class_ "fixed top-0 left-0 right-0 bg-bgBase border-b border-strokeWeak z-50"] do
    div_ [class_ "flex items-center justify-between px-6 py-3"] do
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

  section_ [id_ "main-content", class_ "mx-auto p-6 pb-36 pt-20 overflow-y-auto h-full"] do
    div_ [class_ "flex justify-between items-center mb-8"] do
      h2_ [class_ "text-textStrong text-3xl font-semibold"] "Projects"
      a_ [class_ "btn btn-primary btn-sm", href_ "/p/new"] (faSprite_ "plus" "regular" "h-4 w-4 mr-2" >> "New Project")

    unless (V.null projects) $ div_ [class_ "mb-12"] do
      h3_ [class_ "text-textWeak text-lg font-medium mb-4"] "Your Projects"
      div_ [class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"] $ mapM_ projectCard_ $ V.toList projects

    when showDemoProject $ div_ [] do
      h3_ [class_ "text-textWeak text-lg font-medium mb-4"] "Demo Project"
      div_ [class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"] $ projectCard_ demoProject


projectCard_ :: Projects.Project' -> Html ()
projectCard_ project = do
  div_ [class_ "surface-raised hover:shadow-md transition-shadow duration-200 overflow-hidden group"] do
    a_ [href_ ("/p/" <> project.id.toText), class_ "block"] do
      div_ [class_ "p-5 pb-3"] do
        div_ [class_ "flex justify-between items-start mb-3"] do
          div_ [class_ "flex-1 min-w-0"] do
            h4_ [class_ "text-textStrong font-semibold text-lg truncate group-hover:text-textBrand transition-colors"] $ toHtml project.title
            p_ [class_ "text-textWeak text-sm mt-1 line-clamp-2"] $ toHtml project.description
          faSprite_ "arrow-right" "regular" "h-4 w-4 text-textWeak opacity-0 group-hover:opacity-100 transition-opacity ml-2 mt-1"

        div_ [class_ "flex items-center justify-between text-sm text-textWeak"] do
          div_ [class_ "flex items-center gap-1"] do
            faSprite_ "calendar" "regular" "h-3.5 w-3.5"
            time_ [datetime_ $ fmt $ dateDashF project.createdAt] $ toHtml @Text $ fmt $ dateDashF project.createdAt

          unless (V.null project.usersDisplayImages) do
            div_ [class_ "flex -space-x-2"] do
              project.usersDisplayImages & V.toList & take 3 & mapM_ \imgSrc ->
                img_ [class_ "inline-block h-6 w-6 rounded-full ring-2 ring-base-100", src_ imgSrc, alt_ "User avatar"]
              when (V.length project.usersDisplayImages > 3) do
                span_ [class_ "flex items-center justify-center h-6 w-6 rounded-full bg-fillWeak text-textWeak text-xs ring-2 ring-base-100"] do
                  toHtml ("+" <> show (V.length project.usersDisplayImages - 3))

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
  (sess, project) <- Sessions.sessionAndProject pid
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
        , slackData = slackInfo
        }


data NotifListForm = NotifListForm
  { notificationsChannel :: [Text]
  , phones :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, FromForm)


newtype NotificationsUpdatePost = NotificationsUpdatePost ()
  deriving stock (Generic, Show)


instance ToHtml NotificationsUpdatePost where
  toHtml _ = ""
  toHtmlRaw = toHtml


updateNotificationsChannel :: Projects.ProjectId -> NotifListForm -> ATAuthCtx (RespHeaders NotificationsUpdatePost)
updateNotificationsChannel pid NotifListForm{notificationsChannel, phones} = do
  validationResult <- validateNotificationChannels pid notificationsChannel phones
  case validationResult of
    Left errorMessage -> do
      addErrorToast errorMessage Nothing
      addRespHeaders $ NotificationsUpdatePost ()
    Right () -> do
      _ <- Projects.updateNotificationsChannel pid notificationsChannel phones
      addSuccessToast "Updated Notification Channels Successfully" Nothing
      addRespHeaders $ NotificationsUpdatePost ()


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


data IntegrationsConfig = IntegrationsConfig
  { session :: Sessions.PersistentSession
  , projectId :: Projects.ProjectId
  , envConfig :: EnvConfig
  , isUpdate :: Bool
  , createForm :: CreateProjectForm
  , notifChannel :: Maybe (V.Vector Projects.NotificationChannel)
  , phones :: V.Vector Text
  , slackData :: Maybe SlackData
  }


integrationsBody :: IntegrationsConfig -> Html ()
integrationsBody IntegrationsConfig{..} = do
  section_ [id_ "main-content", class_ "p-3 py-5 sm:p-6 overflow-y-scroll h-full"] do
    div_ [class_ "mx-auto", style_ "max-width:1000px"] do
      let pid = projectId.toText
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
            renderNotificationOption "Slack" "Send notifications to Slack channels" "slack" Projects.NSlack notifChannel (faSprite_ "slack" "solid" "h-6 w-6") (renderSlackIntegration envConfig "" slackData)
            renderNotificationOption "Discord" "Send notifications to Discord servers" "discord" Projects.NDiscord notifChannel (faSprite_ "discord" "solid" "h-6 w-6") (renderDiscordIntegration envConfig "")
            renderNotificationOption "WhatsApp" "Send notification via WhatsApp" "phone" Projects.NPhone notifChannel (faSprite_ "whatsapp" "solid" "h-6 w-6") renderWhatsappIntegration
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
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let currUserId = sess.persistentSession.userId
  projMembers <- ProjectMembers.selectActiveProjectMembers pid

  if project.paymentPlan /= "Free"
    then do
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
          userIdM' <- Users.userIdByEmail email
          case userIdM' of
            Nothing -> do
              idM' <- Users.createEmptyUser email
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
          . dbtToEff
        $ ProjectMembers.updateProjectMembersPermissons uAndPOldAndChanged

      whenJust (nonEmpty deletedUAndP)
        $ void
        . ProjectMembers.softDeleteProjectMembers

      projMembersLatest <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
      if isJust onboardingM
        then do
          redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=Integration"
          addRespHeaders $ ManageMembersPost (pid, projMembersLatest)
        else do
          addSuccessToast "Updated Members List Successfully" Nothing
          addRespHeaders $ ManageMembersPost (pid, projMembersLatest)
    else do
      addErrorToast "Only one member allowed on Free plan" Nothing
      addRespHeaders $ ManageMembersPost (pid, V.fromList projMembers)


data TeamForm = TeamForm
  { teamName :: Text
  , teamDescription :: Text
  , teamHandle :: Text
  , teamMembers :: V.Vector Users.UserId
  , notifEmails :: V.Vector Text
  , slackChannels :: V.Vector Text
  , discordChannels :: V.Vector Text
  , phoneNumbers :: V.Vector Text
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
manageTeamPostH pid TeamForm{teamName, teamDescription, teamHandle, teamMembers, notifEmails, slackChannels, discordChannels, phoneNumbers, teamId} tmView = do
  (sess, _) <- Sessions.sessionAndProject pid
  let currUserId = sess.persistentSession.userId
  userPermission <- ProjectMembers.getUserPermission pid currUserId
  projMembers <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  let validMemberIds = V.map (.userId) projMembers
      invalidMembers = V.filter (`V.notElem` validMemberIds) teamMembers
      teamDetails = ProjectMembers.TeamDetails teamName teamDescription teamHandle teamMembers notifEmails slackChannels discordChannels phoneNumbers
      validationErr msg = addErrorToast msg Nothing >> addRespHeaders (ManageTeamsPostError msg)
  case (userPermission == Just ProjectMembers.PAdmin, V.null invalidMembers, validateTeamDetails teamName teamHandle notifEmails, teamId) of
    (False, _, _, _) -> validationErr "Only admins can create or update teams"
    (_, False, _, _) -> validationErr "Some team members are not project members"
    (_, _, Left e, _) -> addErrorToast e Nothing >> addReswap "" >> addRespHeaders (ManageTeamsPostError e)
    (_, _, _, Just tid) -> do
      _ <- ProjectMembers.updateTeam pid tid teamDetails
      addSuccessToast "Team updated successfully" Nothing
      maybe (manageTeamsGetH pid (Just "from_post")) (\_ -> teamGetH pid teamHandle (Just "main-page")) tmView
    (_, _, _, Nothing) -> do
      rowsAffected <- ProjectMembers.createTeam pid currUserId teamDetails
      if rowsAffected > 0
        then addSuccessToast "Team saved successfully" Nothing >> manageTeamsGetH pid (Just "from_post")
        else validationErr "Team handle already exists for this project."


newtype TBulkActionForm = TBulkActionForm
  { itemId :: [UUID.UUID]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromForm)


manageTeamBulkActionH :: Projects.ProjectId -> Text -> TBulkActionForm -> Maybe Text -> ATAuthCtx (RespHeaders ManageTeams)
manageTeamBulkActionH pid action TBulkActionForm{itemId} listViewM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  case action of
    "delete" -> do
      teamVm <- ProjectMembers.getTeamsById pid $ V.fromList itemId
      let canDelete = all (\team -> Just sess.user.id == team.created_by) teamVm
      if canDelete
        then do
          _ <- ProjectMembers.deleteTeams pid $ V.fromList itemId
          when (isNothing listViewM) do
            redirectCS ("/p/" <> pid.toText <> "/manage_teams")
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
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  projMembers <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  slackDataM <- Slack.getProjectSlackData pid
  channels <- case slackDataM of
    Just slackData -> do
      channels' <- SlackP.getSlackChannels appCtx.env.slackBotToken slackData.teamId
      case channels' of
        Just chs -> return chs.channels
        Nothing -> return []
    Nothing -> return []

  discordDataM <- Slack.getDiscordDataByProjectId pid
  discordChannels <- case discordDataM of
    Just discordData -> Discord.getDiscordChannels appCtx.env.discordBotToken discordData.guildId
    Nothing -> return []
  teams <- V.fromList <$> ProjectMembers.getTeamsVM pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Manage teams"
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
manageTeamsPage pid projMembers channels discordChannels teams = do
  let whiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= (x.first_name <> " " <> x.last_name), "email" AE..= x.email, "value" AE..= x.userId]) <$> projMembers
  let channelWhiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= ("#" <> x.channelName), "value" AE..= x.channelId]) <$> channels
  let discordWhiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= ("#" <> x.channelName), "value" AE..= x.channelId]) <$> discordChannels
  section_ [id_ "main-content", class_ "w-full py-8"] do
    div_ [class_ "p-6 w-full"] do
      div_ [class_ "mb-8 w-full flex items-center justify-between"] do
        div_ [class_ "flex flex-col gap-2"] do
          h2_ [class_ "text-textStrong text-3xl font-semibold"] "Teams"
          p_ [class_ "text-textWeak text-sm"] "Manage your project teams and their access"
        label_ [class_ "btn btn-primary btn-sm text-white", Lucid.for_ "n-new-team-modal"] (faSprite_ "plus" "regular" "h-4 w-4 mr-2" >> "New Team")
        input_ [type_ "checkbox", id_ "n-new-team-modal", class_ "modal-toggle"]
        teamModal pid Nothing whiteList channelWhiteList discordWhiteList False
      let renderTeamNameCol team = nameCell pid team.name team.description team.handle
          renderModifiedCol team = span_ [class_ "monospace text-textWeak"] $ toHtml $ toText $ formatTime defaultTimeLocale "%b %-e, %-l:%M %P" team.updated_at
          renderMembersCol team = memberCell team.members
          renderNotificationsCol = notifsCell

      let tableCols =
            [ Table.col "Name" renderTeamNameCol
            , Table.col "Modified" renderModifiedCol
            , Table.col "Members" renderMembersCol
            , Table.col "Notifications" renderNotificationsCol
            ]

      let table =
            Table
              { config = def{Table.elemID = "teams_table", Table.renderAsTable = True, Table.bulkActionsInHeader = Just 0}
              , columns = tableCols
              , rows = teams
              , features =
                  def
                    { Table.rowId = Just \team -> UUID.toText team.id
                    , Table.rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                    , Table.bulkActions =
                        [ Table.BulkAction{icon = Just "trash", title = "Delete", uri = "/p/" <> pid.toText <> "/manage_teams/bulk_action/delete"}
                        ]
                    , Table.search = Just Table.ClientSide
                    }
              }
      div_ [class_ "w-full"] do
        toHtml table


nameCell :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
nameCell pid name description handle = do
  span_ [class_ "flex items-center gap-2"] do
    span_ [class_ "p-1 px-2 bg-fillWeak rounded-md", data_ "tippy-content" "Team"] $ faSprite_ "users" "regular" "w-3 h-3"
    a_ [href_ ("/p/" <> pid.toText <> "/manage_teams/" <> handle), class_ "font-medium text-textStrong hover:text-textBrand hover:underline underline-offset-2"] $ toHtml name
    span_ [class_ "text-textWeak text-sm overflow-ellipsis truncate"] $ toHtml description


memberCell :: V.Vector ProjectMembers.TeamMemberVM -> Html ()
memberCell members = do
  div_ [class_ "inline-block flex -space-x-2"] do
    forM_ members $ \m -> do
      div_ [class_ "inline-block mx-0.5", term "data-tippy-content" m.memberName]
        $ img_ [class_ "inline-block h-6 w-6 rounded-full border border-strokeWeak ", src_ m.memberAvatar, alt_ "User avatar"]


notifsCell :: ProjectMembers.TeamVM -> Html ()
notifsCell team = do
  div_ [class_ "flex items-center gap-2"] do
    unless (V.null team.slack_channels) do
      div_ [term "data-tippy-content" "Slack"] do
        faSprite_ "slack" "solid" "h-3.5 w-3.5"
    unless (V.null team.discord_channels) do
      div_ [term "data-tippy-content" "Discord"] do
        faSprite_ "discord" "solid" "h-3.5 w-3.5"
    unless (V.null team.notify_emails) do
      div_ [term "data-tippy-content" "Email"] do
        faSprite_ "envelope" "solid" "h-3.5 w-3.5"


teamGetH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders ManageTeams)
teamGetH pid handle layoutM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  teamVm <- ProjectMembers.getTeamByHandle pid handle
  projMembers <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  slackDataM <- Slack.getProjectSlackData pid
  channels <- case slackDataM of
    Just slackData -> do
      channels' <- SlackP.getSlackChannels appCtx.env.slackBotToken slackData.teamId
      case channels' of
        Just chs -> return chs.channels
        Nothing -> return []
    Nothing -> return []

  discordDataM <- Slack.getDiscordDataByProjectId pid
  discordChannels <- case discordDataM of
    Just discordData -> Discord.getDiscordChannels appCtx.env.discordBotToken discordData.guildId
    Nothing -> return []
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Team details"
          , currProject = Just project
          , config = appCtx.config
          }
  case teamVm of
    Just team -> case layoutM of
      Just _ -> addRespHeaders $ ManageTeamGet' (pid, team, projMembers, channels, discordChannels)
      _ -> addRespHeaders $ ManageTeamGet (PageCtx bwconf (pid, team, projMembers, channels, discordChannels))
    Nothing -> addRespHeaders $ ManageTeamGetError (PageCtx bwconf (pid, handle))


teamPage :: Projects.ProjectId -> ProjectMembers.TeamVM -> V.Vector ProjectMembers.ProjectMemberVM -> [BotUtils.Channel] -> [BotUtils.Channel] -> Html ()
teamPage pid team projMembers slackChannels discordChannels = do
  let whiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= (x.first_name <> " " <> x.last_name), "email" AE..= x.email, "value" AE..= x.userId]) <$> projMembers
      channelWhiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= ("#" <> x.channelName), "value" AE..= x.channelId]) <$> slackChannels
      discordWhiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= ("#" <> x.channelName), "value" AE..= x.channelId]) <$> discordChannels
      card_ title content = div_ [class_ "surface-raised rounded-2xl p-4"] (span_ [class_ "flex items-center gap-2 text-sm font-semibold text-textStrong"] title >> content)
      notifRow_ icon iconType label vals = div_ [class_ "flex items-start gap-3"] do
        _ <- span_ [class_ "p-1.5 bg-fillWeak rounded-md"] $ faSprite_ icon iconType "h-3.5 w-3.5"
        div_ [class_ "flex-1"] (div_ [class_ "text-sm font-medium"] label >> div_ [class_ "text-xs text-textWeak mt-0.5"] (if null vals then "Not configured" else toHtml $ T.intercalate ", " vals))
      resolveChannel chans cid = maybe cid (("#" <>) . (.channelName)) $ find (\x -> x.channelId == cid) chans
      lazySection_ secId icon title searchPh url = div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
        _ <- div_ [class_ "flex items-center justify-between w-full p-4 border-b border-strokeWeak"] do
          _ <- span_ [class_ "flex items-center gap-2 text-sm font-semibold text-textStrong"] (faSprite_ icon "regular" "h-4 w-4" >> toHtml title)
          label_ [class_ "input input-sm w-64 bg-fillWeak border-0"] do
            faSprite_ "magnifying-glass" "regular" "h-3.5 w-3.5 text-textWeak"
            input_ [type_ "text", placeholder_ searchPh, class_ "", [__|on input show <tr/> in #${secId} when its textContent.toLowerCase() contains my value.toLowerCase()|]]
        div_ [class_ "w-full max-h-96 overflow-y-auto", id_ secId] do
          unless (T.null url) $ a_ [hxGet_ url, hxTrigger_ "intersect once", hxTarget_ $ "#" <> secId, hxSwap_ "outerHTML"] ""
          div_ [class_ "flex flex-col items-center justify-center py-8 text-center gap-2"] (faSprite_ icon "regular" "h-6 w-6 text-textWeak" >> div_ [class_ "text-sm text-textWeak"] (toHtml $ "No " <> T.toLower title <> " linked"))
  section_ [id_ "main-content", class_ "w-full py-8"] $ div_ [class_ "px-6 w-full"] do
    div_ [class_ "mb-6 flex items-center gap-3"] do
      a_ [href_ ("/p/" <> pid.toText <> "/manage_teams"), class_ "text-textWeak hover:text-textStrong"] $ faSprite_ "arrow-left" "regular" "h-4 w-4"
      h2_ [class_ "text-textStrong text-3xl font-semibold"] $ toHtml team.name
    div_ [class_ "flex gap-6 h-full"] do
      div_ [class_ "w-4/12 space-y-4"] do
        card_ (faSprite_ "circle-info" "regular" "h-4 w-4" >> "Details") do
          div_ [class_ "absolute right-4 top-4"] do
            label_ [class_ "btn btn-outline btn-xs", Lucid.for_ $ team.handle <> "-new-team-modal"] $ faSprite_ "pen" "regular" "h-3 w-3 mr-1" >> "Edit"
            input_ [type_ "checkbox", id_ $ team.handle <> "-new-team-modal", class_ "modal-toggle"]
            teamModal pid (Just team) whiteList channelWhiteList discordWhiteList True
          div_ [class_ "mt-3 space-y-2 text-sm"] do
            div_ [class_ "flex gap-2"] $ span_ [class_ "text-textWeak w-16"] "Handle" >> span_ [class_ "text-textStrong"] (toHtml $ "@" <> team.handle)
            unless (T.null team.description) $ div_ [class_ "flex gap-2"] $ span_ [class_ "text-textWeak w-16"] "About" >> span_ [class_ "text-textStrong"] (toHtml team.description)
        card_ (faSprite_ "users" "regular" "h-4 w-4" >> "Members" >> span_ [class_ "text-textWeak font-normal"] ("(" <> show (V.length team.members) <> ")"))
          $ div_ [class_ "mt-3 divide-y divide-strokeWeak"]
          $ forM_ team.members \m ->
            div_ [class_ "flex items-center gap-3 py-2.5"] $ img_ [src_ m.memberAvatar, class_ "w-8 h-8 rounded-full border border-strokeWeak"] >> div_ [] (div_ [class_ "text-sm font-medium text-textStrong"] (toHtml m.memberName) >> div_ [class_ "text-xs text-textWeak"] (toHtml m.memberEmail))
        card_ (faSprite_ "bell" "regular" "h-4 w-4" >> "Notifications") $ div_ [class_ "mt-3 space-y-3"] do
          notifRow_ "envelope" "regular" "Email" $ V.toList team.notify_emails
          notifRow_ "slack" "solid" "Slack" $ V.toList $ V.map (resolveChannel slackChannels) team.slack_channels
          notifRow_ "discord" "solid" "Discord" $ V.toList $ V.map (resolveChannel discordChannels) team.discord_channels
      div_ [class_ "flex-1 space-y-4"] do
        lazySection_ "monitors-section" "bell" "Alerts" "Search alerts..." ("/p/" <> pid.toText <> "/monitors/alerts/team/" <> UUID.toText team.id)
        lazySection_ "dashboards-section" "chart-area" "Dashboards" "Search dashboards..." ("/p/" <> pid.toText <> "/dashboards/?teamId=" <> UUID.toText team.id)
        lazySection_ "services-section" "server" "Services" "Search services..." ""


teamPageNF :: Projects.ProjectId -> Text -> Html ()
teamPageNF pid handle = do
  section_ [id_ "main-content", class_ "w-full py-16"] do
    div_ [class_ "p-6 w-[606px] mx-auto"] do
      h2_ [class_ "text-textStrong mb-4 text-xl font-semibold"] $ "Team not found: " <> toHtml handle
      p_ [class_ "text-textWeak text-sm leading-tight"] "We couldn't find the team you're looking for."


data TeamForm = TeamForm
  { teamName :: Text
  , teamDescription :: Text
  , teamHandle :: Text
  , teamMembers :: V.Vector Users.UserId
  , notifEmails :: V.Vector Text
  , slackChannels :: V.Vector Text
  , discordChannels :: V.Vector Text
  , teamId :: Maybe UUID.UUID
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromForm)
instance AE.FromJSON TeamForm where
  parseJSON = AE.withObject "TeamForm" $ \o -> do
    TeamForm
      <$> o AE..: "teamName"
      <*> o AE..: "teamDescription"
      <*> o AE..: "teamHandle"
      <*> o AE..:? "teamMembers" AE..!= V.empty
      <*> o AE..:? "notifEmails" AE..!= V.empty
      <*> o AE..:? "slackChannels" AE..!= V.empty
      <*> o AE..:? "discordChannels" AE..!= V.empty
      <*> o AE..:? "teamId"


manageTeamPostH :: Projects.ProjectId -> TeamForm -> ATAuthCtx (RespHeaders (Html ()))
manageTeamPostH pid TeamForm{teamName, teamDescription, teamHandle, teamMembers, notifEmails, slackChannels, discordChannels, teamId} = do
  rs <- case teamId of
    Just tid -> do
      _ <- dbtToEff $ ProjectMembers.updateTeam pid tid teamName teamDescription teamHandle teamMembers notifEmails slackChannels discordChannels
      addSuccessToast "Team updated successfully" Nothing
      html <- manageTeamsGetH pid (Just "from_post")
      return html
    Nothing -> do
      _ <- dbtToEff $ ProjectMembers.createTeam pid teamName teamDescription teamHandle teamMembers notifEmails slackChannels discordChannels
      addSuccessToast "Team saved successfully" Nothing
      html <- manageTeamsGetH pid (Just "from_post")
      return html
  return rs


manageTeamsGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
manageTeamsGetH pid layoutM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  projMembers <- dbtToEff $ ProjectMembers.selectActiveProjectMembers pid
  slackDataM <- Slack.getProjectSlackData pid
  channels <- case slackDataM of
    Just slackData -> do
      channels' <- SlackP.getSlackChannels appCtx.env.slackBotToken slackData.teamId
      case channels' of
        Just chs -> return chs.channels
        Nothing -> return []
    Nothing -> return []

  discordDataM <- Slack.getDiscordDataByProjectId pid
  discordChannels <- case discordDataM of
    Just discordData -> do
      channels' <- Discord.getDiscordChannels appCtx.env.discordBotToken discordData.guildId
      return channels'
    Nothing -> return []
  teams <- dbtToEff $ ProjectMembers.getTeams pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Manage teams"
          , currProject = Just project
          , isSettingsPage = True
          , config = appCtx.config
          }
  case layoutM of
    Just _ -> do
      addRespHeaders $ manageTeamsPage pid projMembers channels discordChannels teams
    _ -> do
      addRespHeaders $ bodyWrapper bwconf $ manageTeamsPage pid projMembers channels discordChannels teams


manageTeamsPage :: Projects.ProjectId -> V.Vector ProjectMembers.ProjectMemberVM -> [SlackP.SlackChannel] -> [Discord.DiscordChannel] -> V.Vector ProjectMembers.Team -> Html ()
manageTeamsPage pid projMembers channels discordChannels teams = do
  let whiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= (x.first_name <> " " <> x.last_name), "email" AE..= x.email, "value" AE..= x.id]) <$> projMembers
  let channelWhiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= ("#" <> x.channelName), "value" AE..= x.channelId]) <$> channels
  let discordWhiteList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= ("#" <> x.channelName), "value" AE..= x.channelId]) <$> discordChannels
  section_ [id_ "main-content", class_ "w-full py-8"] do
    div_ [class_ "p-6"] do
      div_ [class_ "mb-8 w-full flex items-center justify-between"] do
        div_ [class_ "flex flex-col gap-2"] do
          h2_ [class_ "text-textStrong text-3xl font-semibold"] "Teams"
          p_ [class_ "text-textWeak text-sm"] "Manage your project teams and their access"
        label_ [class_ "btn btn-primary btn-sm", Lucid.for_ "n-new-team-modal"] (faSprite_ "plus" "regular" "h-4 w-4 mr-2" >> "New Team")
        input_ [type_ "checkbox", id_ "n-new-team-modal", class_ "modal-toggle"]
        teamModal pid Nothing whiteList channelWhiteList discordWhiteList
      div_ [class_ "flex items-center gap-4"] do
        label_ [class_ "input w-96"] do
          faSprite_ "magnifying-glass" "regular" "h-4 w-4 text-textWeak"
          input_ [type_ "text", placeholder_ "Search teams...", class_ ""]
        span_ [class_ "text-textWeak text-sm"] $ (show (V.length teams) <> " teams found")
      div_ [class_ "mt-6 grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"] $ mapM_ (\t -> teamCard pid t whiteList channelWhiteList discordWhiteList) (V.toList teams)


teamCard :: Projects.ProjectId -> ProjectMembers.Team -> Text -> Text -> Text -> Html ()
teamCard pid team whiteList channelWhiteList discordWhiteList = do
  a_ [href_ $ "/p/" <> pid.toText <> "/manage_teams/" <> team.handle, class_ "bg-base-100 border border-strokeWeak rounded-xl shadow-sm hover:shadow-md transition-shadow duration-200 overflow-hidden"] do
    div_ [class_ "p-5 pb-3"] do
      div_ [class_ "flex justify-between items-start mb-3"] do
        div_ [class_ "flex-1 min-w-0"] do
          h4_ [class_ "text-textStrong font-semibold text-lg"] $ toHtml team.name
          p_ [class_ "text-textWeak text-sm mt-1 line-clamp-2"] $ toHtml team.description
        label_ [class_ "btn btn-ghost btn-sm", Lucid.for_ (team.handle <> "-new-team-modal")] "Edit"
        input_ [type_ "checkbox", id_ (team.handle <> "-new-team-modal"), class_ "modal-toggle"]
        teamModal pid (Just team) whiteList channelWhiteList discordWhiteList
      div_ [class_ "flex items-center justify-between text-sm text-textWeak"] do
        div_ [class_ "flex items-center gap-2"] do
          faSprite_ "users" "regular" "h-3.5 w-3.5"
          span_ [] $ toHtml $ (show $ V.length team.members) <> " members"
        div_ [class_ "flex items-center gap-2"] do
          when (not $ V.null team.slack_channels) do
            faSprite_ "slack" "regular" "h-3.5 w-3.5"
          when (not $ V.null team.discord_channels) do
            faSprite_ "discord" "regular" "h-3.5 w-3.5"
          when (not $ V.null team.notify_emails) do
            faSprite_ "envelope" "regular" "h-3.5 w-3.5"


teamGetH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (Html ()))
teamGetH pid handle = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  teamVm <- dbtToEff $ ProjectMembers.getTeamByHandle pid handle
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Manage teams"
          , currProject = Just project
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ bodyWrapper bwconf $ teamPage pid


teamPage :: Projects.ProjectId -> Html ()
teamPage pid = do
  section_ [id_ "main-content", class_ "w-full py-16"] do
    div_ [class_ "p-6 w-[606px] mx-auto"] do
      h2_ [class_ "text-textStrong mb-4 text-xl font-semibold"] "Manage Team"
      p_ [class_ "text-textWeak text-sm leading-tight"] "We'll email them instructions and a link to sign in"


manageMembersGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders ManageMembers)
manageMembersGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  projMembers <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Manage members"
          , currProject = Just project
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ ManageMembersGet $ PageCtx bwconf (pid, projMembers)


data ManageMembers
  = ManageMembersGet {unwrapGet :: PageCtx (Projects.ProjectId, V.Vector ProjectMembers.ProjectMemberVM)}
  | ManageMembersPost {unwrapPost :: (Projects.ProjectId, V.Vector ProjectMembers.ProjectMemberVM)}


instance ToHtml ManageMembers where
  toHtml (ManageMembersGet (PageCtx bwconf (pid, memebers))) = toHtml $ PageCtx bwconf $ manageMembersBody pid memebers
  toHtml (ManageMembersPost (pid, memebers)) = toHtml $ manageMembersBody pid memebers
  toHtmlRaw = toHtml


manageMembersBody :: Projects.ProjectId -> V.Vector ProjectMembers.ProjectMemberVM -> Html ()
manageMembersBody pid projMembers =
  div_ [id_ "main-content", class_ "w-full h-full overflow-y-auto"] do
    section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
      -- Header
      div_ [class_ "mb-2"] do
        h2_ [class_ "text-textStrong text-xl font-semibold"] "Manage Members"
        p_ [class_ "text-textWeak text-sm mt-1"] "Invite team members and manage their access permissions"

      form_
        [ class_ "space-y-6"
        , hxPost_ ""
        , hxTarget_ "#main-content"
        , hxSwap_ "outerHTML"
        , hxIndicator_ "#submitIndicator"
        ]
        do
          -- Invite section
          div_ [class_ "surface-raised rounded-2xl p-4"] do
            label_ [class_ "text-sm font-medium text-textStrong mb-3 block"] "Invite new member"
            div_ [class_ "flex gap-2"] do
              input_ [type_ "email", name_ "emails", class_ "input input-bordered flex-1", placeholder_ "colleague@company.com"]
              select_ [name_ "permissions", class_ "select select-bordered w-32"] do
                option_ [value_ "admin"] "Admin"
                option_ [value_ "edit"] "Editor"
                option_ [value_ "view"] "Viewer"
              button_ [class_ "btn btn-outline gap-1"] do
                faSprite_ "paper-plane" "regular" "w-3 h-3"
                span_ "Invite"

          -- Members list
          div_ [class_ "space-y-3"] do
            div_ [class_ "flex items-center justify-between"] do
              h3_ [class_ "text-sm font-medium text-textStrong"] $ toHtml $ "Team members (" <> show (V.length projMembers) <> ")"
              when (V.length projMembers > 0) $ button_ [class_ "btn btn-sm"] "Save changes"
            div_ [class_ "surface-raised rounded-2xl divide-y divide-strokeWeak overflow-hidden"] do
              if V.null projMembers
                then div_ [class_ "p-8 text-center text-textWeak text-sm"] "No members yet. Invite someone to get started."
                else mapM_ (memberRow pid) projMembers


memberRow :: Projects.ProjectId -> ProjectMembers.ProjectMemberVM -> Html ()
memberRow pid prM = do
  let email = CI.original prM.email
      permission = prM.permission
      memberId = RealUUID.toText prM.id
  div_ [class_ "px-4 py-3 flex items-center gap-3", id_ $ "member-" <> memberId] do
    div_ [class_ "flex-1 min-w-0"] do
      input_ [type_ "text", name_ "emails", value_ email, readonly_ "true", class_ "bg-transparent w-full text-textStrong text-sm truncate focus:outline-none cursor-default"]
    div_ [class_ "flex items-center gap-2"] do
      select_ [name_ "permissions", class_ "select select-sm select-bordered text-sm"] do
        option_ ([value_ "admin"] <> selectedIf ProjectMembers.PAdmin permission) "Admin"
        option_ ([value_ "edit"] <> selectedIf ProjectMembers.PEdit permission) "Editor"
        option_ ([value_ "view"] <> selectedIf ProjectMembers.PView permission) "Viewer"
      button_
        [ class_ "btn btn-sm btn-ghost text-textWeak hover:text-textError hover:bg-fillError-weak"
        , hxDelete_ $ "/p/" <> pid.toText <> "/manage_members/" <> memberId
        , hxTarget_ $ "#member-" <> memberId
        , hxSwap_ "outerHTML"
        , hxConfirm_ "Remove this member from the project?"
        ]
        do
          faSprite_ "trash" "regular" "w-4 h-4"
  where
    selectedIf :: ProjectMembers.Permissions -> ProjectMembers.Permissions -> [Attribute]
    selectedIf a b = [selected_ "" | a == b]


deleteMemberH :: Projects.ProjectId -> RealUUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
deleteMemberH pid memberId = do
  (sess, project) <- Sessions.sessionAndProject pid
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
          addSuccessToast "Member removed" Nothing
          addRespHeaders ""


manageSubGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
manageSubGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sub <- liftIO $ getSubscriptionPortalUrl project.subId envCfg.lemonSqueezyApiKey
  case sub of
    Nothing -> do
      addErrorToast "Subscription ID not found" Nothing
      addRespHeaders ""
    Just s -> do
      redirectCS s.dataVal.attributes.urls.customerPortal
      addRespHeaders ""


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
  sess <- Sessions.getSession
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
      projectKeyUUID <- UUID.genUUID
      let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 $ ProjectApiKeys.encryptAPIKey (encodeUtf8 envCfg.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
      pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID "Default API Key" encryptedKeyB64
      _ <- ProjectApiKeys.insertProjectApiKey pApiKey
      let projectMember = ProjectMembers.CreateProjectMembers pid sess.user.id ProjectMembers.PAdmin
      _ <- ProjectMembers.insertProjectMembers [projectMember]
      let h = "/p/" <> pid.toText <> "/onboarding"
      pure $ addHeader h $ PageCtx bwconf ""


data CreateProjectResp = CreateProjectResp
  { sess :: Sessions.PersistentSession
  , pid :: Projects.ProjectId
  , env :: EnvConfig
  , paymentPlan :: Text
  , form :: CreateProjectForm
  , formError :: CreateProjectFormError
  , pro :: Projects.Project
  }
  deriving stock (Generic, Show)


data CreateProject
  = CreateProject (PageCtx (Sessions.PersistentSession, Projects.ProjectId, EnvConfig, Text, Bool, CreateProjectForm, CreateProjectFormError, Projects.Project))
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
  (sess, project) <- Sessions.sessionAndProject pid
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
          , pageTitle = "Project settings"
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ CreateProject $ PageCtx bwconf (sess.persistentSession, pid, appCtx.config, project.paymentPlan, True, createProj, def @CreateProjectFormError, project)


deleteProjectGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders CreateProject)
deleteProjectGetH pid = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  if isDemoAndNotSudo pid sess.user.isSudo
    then do
      addSuccessToast "Can't perform this action on the demon project" Nothing
      addRespHeaders $ PostNoContent ""
    else do
      _ <- Projects.deleteProject pid
      _ <- liftIO $ withResource appCtx.pool \conn ->
        createJob conn "background_jobs" $ BackgroundJobs.DeletedProject pid
      addSuccessToast "Deleted Project Successfully" Nothing
      redirectCS "/"
      addRespHeaders $ PostNoContent ""


createProjectPostH :: Projects.ProjectId -> CreateProjectForm -> ATAuthCtx (RespHeaders CreateProject)
createProjectPostH pid createP = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Right cpe -> addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid appCtx.config "" createP cpe project)
    Left cp -> processProjectPostForm cp pid


data FirstSubItem = FirstSubItem
  { id :: Int
  , subscriptionId :: Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] FirstSubItem


data Attributes = Attributes
  { firstSubscriptionItem :: FirstSubItem
  , productName :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] Attributes


data DataVals = DataVals
  { id :: Text
  , attributes :: Attributes
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] DataVals


newtype SubResponse = SubResponse {dataVal :: [DataVals]}
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
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromForm)


pricingUpdateH :: Projects.ProjectId -> PricingUpdateForm -> ATAuthCtx (RespHeaders (Html ()))
pricingUpdateH pid PricingUpdateForm{orderIdM, plan} = do
  (sess, project) <- Sessions.sessionAndProject pid
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
          $ \user -> ConvertKit.addUserOrganization envCfg.convertkitApiKey (CI.original user.email) pid.toText project.title name

  case plan of
    Just "Open Source" | envCfg.basicAuthEnabled -> do
      _ <- updatePricing "Open Source" "" "" ""
      handleOnboarding "Open Source"
    _ -> case orderIdM of
      Just orderId -> do
        getSubscriptionId (Just orderId) apiKey >>= \case
          Just sub | not (null sub.dataVal) -> do
            let target = sub.dataVal Unsafe.!! 0
                subId = show target.attributes.firstSubscriptionItem.subscriptionId
                firstSubId = show target.attributes.firstSubscriptionItem.id
                productName = target.attributes.productName
            _ <- updatePricing productName subId firstSubId orderId
            handleOnboarding productName
          _ -> addErrorToast "Something went wrong while fetching subscription id" Nothing
      Nothing -> do
        _ <- updatePricing "Free" "" "" ""
        handleOnboarding "Free"
        users <- ProjectMembers.selectActiveProjectMembers pid
        let usersToDel = map (.id) $ drop 1 users
        whenJust (nonEmpty usersToDel)
          $ void
          . ProjectMembers.softDeleteProjectMembers
  if project.paymentPlan == "ONBOARDING"
    then do
      redirectCS $ "/p/" <> pid.toText <> "/"
      addRespHeaders ""
    else do
      addTriggerEvent "closeModal" ""
      addSuccessToast "Pricing updated successfully" Nothing
      addRespHeaders ""


pricingUpdateGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
pricingUpdateGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , config = appCtx.config
          }
  let envCfg = appCtx.config
      lemon = envCfg.lemonSqueezyUrl <> "&checkout[custom][project_id]=" <> pid.toText
      critical = envCfg.lemonSqueezyCriticalUrl <> "&checkout[custom][project_id]=" <> pid.toText
  addRespHeaders $ PageCtx bwconf $ pricingPage_ pid lemon critical project.paymentPlan appCtx.config.enableFreetier appCtx.config.basicAuthEnabled


pricingPage_ :: Projects.ProjectId -> Text -> Text -> Text -> Bool -> Bool -> Html ()
pricingPage_ pid lemon critical paymentPlan enableFreeTier basicAuthEnabled = do
  section_ [class_ "w-full h-full overflow-y-auto py-12"] do
    div_ [class_ "flex flex-col max-w-4xl mx-auto gap-10 px-4"] do
      h1_ [class_ "font-semibold text-4xl text-textStrong"] "Update pricing"
      paymentPlanPicker pid lemon critical paymentPlan enableFreeTier basicAuthEnabled


processProjectPostForm :: Valor.Valid CreateProjectForm -> Projects.ProjectId -> ATAuthCtx (RespHeaders CreateProject)
processProjectPostForm cpRaw pid = do
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  (sess, project) <- Sessions.sessionAndProject pid

  let cp = Valor.unValid cpRaw
  if isDemoAndNotSudo pid sess.user.isSudo
    then do
      addErrorToast "Can't perform this action on the demo project" Nothing
      addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid envCfg "" cp (def @CreateProjectFormError) project)
    else do
      _ <- Projects.updateProject (createProjectFormToModel pid project.subId project.firstSubItemId project.orderId project.paymentPlan cp)
      addSuccessToast "Updated Project Successfully" Nothing
      addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid envCfg "" cp (def @CreateProjectFormError) project)


createProjectBody :: Sessions.PersistentSession -> Projects.ProjectId -> EnvConfig -> Text -> CreateProjectForm -> CreateProjectFormError -> Projects.Project -> Html ()
createProjectBody sess pid envCfg paymentPlan cp cpe proj = do
  div_ [id_ "main-content", class_ "w-full h-full overflow-y-auto"] do
    section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
      -- Header
      div_ [class_ "mb-2"] do
        h2_ [class_ "text-textStrong text-xl font-semibold"] "Project Settings"
        p_ [class_ "text-textWeak text-sm mt-1"] "Manage your project details and notification preferences"

      form_
        [ class_ "space-y-6"
        , hxPost_ $ "/p/update/" <> pid.toText
        , hxTarget_ "#main-content"
        , hxSwap_ "outerHTML"
        , id_ "createUpdateBodyForm"
        , hxIndicator_ "#createIndicator"
        ]
        do
          -- Project details card
          div_ [class_ "surface-raised rounded-2xl p-4 space-y-4"] do
            div_ [class_ "space-y-4"] do
              div_ [class_ "space-y-1.5"] do
                label_ [class_ "flex items-center gap-1 text-xs font-medium text-textWeak", Lucid.for_ "title"] do
                  "Project Name"
                  span_ [class_ "text-textError"] "*"
                input_ [class_ "input input-bordered input-sm w-full", type_ "text", id_ "title", name_ "title", value_ cp.title, required_ "required", placeholder_ "My Project"]

              div_ [class_ "space-y-1.5"] do
                label_ [class_ "text-xs font-medium text-textWeak", Lucid.for_ "timezone"] "Timezone"
                select_ [name_ "timeZone", id_ "timezone", class_ "select select-bordered select-sm w-full"] do
                  option_ [value_ cp.timeZone] $ toHtml cp.timeZone

              div_ [class_ "space-y-1.5"] do
                label_ [class_ "text-xs font-medium text-textWeak", Lucid.for_ "description"] "Description"
                textarea_ [class_ "textarea textarea-bordered w-full text-sm", rows_ "3", placeholder_ "What is this project about?", id_ "description", name_ "description"] $ toHtml cp.description

          -- Alert configuration card
          alertConfiguration (isJust cp.endpointAlerts) (isJust cp.errorAlerts) (isJust cp.weeklyNotifs) (isJust cp.dailyNotifs)

          -- Save button
          div_ [class_ "flex justify-end"] do
            button_ [class_ "btn btn-sm gap-1", type_ "submit"] do
              span_ [id_ "createIndicator", class_ "htmx-indicator loading loading-dots loading-xs"] ""
              faSprite_ "floppy-disk" "regular" "w-3 h-3"
              span_ "Save Changes"

      script_ do
        [text|
           const timezoneSelect = document.getElementById("timezone");
           const timeZones = Intl.supportedValuesOf('timeZone');
           timeZones.forEach((tz) => {
             const option = document.createElement("option");
             option.value = tz;
             option.text = tz;
             timezoneSelect.appendChild(option);
           });
        |]


projectDeleteGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
projectDeleteGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Delete project"
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ PageCtx bwconf $ deleteProjectBody pid


deleteProjectBody :: Projects.ProjectId -> Html ()
deleteProjectBody pid = do
  let pidText = pid.toText
  div_ [class_ "w-full h-full overflow-y-auto"] do
    section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
      -- Header
      div_ [class_ "mb-2"] do
        h2_ [class_ "text-textStrong text-xl font-semibold"] "Delete Project"
        p_ [class_ "text-textWeak text-sm mt-1"] "Permanently remove this project and all associated data"

      -- Warning card
      div_ [class_ "surface-raised rounded-2xl p-4"] do
        div_ [class_ "flex items-start gap-3"] do
          div_ [class_ "p-2 rounded-full bg-fillError-weak shrink-0"] $ faSprite_ "triangle-alert" "regular" "h-4 w-4 text-textError"
          div_ [class_ "space-y-2"] do
            h3_ [class_ "text-sm font-medium text-textStrong"] "Danger Zone"
            p_ [class_ "text-sm text-textWeak"] "Deleting this project will permanently remove all data including:"
            ul_ [class_ "text-sm text-textWeak list-disc list-inside space-y-1 ml-1"] do
              li_ "API keys and configurations"
              li_ "All collected telemetry data"
              li_ "Dashboards and saved queries"
              li_ "Team member access"
            p_ [class_ "text-sm text-textError font-medium mt-3"] "This action cannot be undone."

      -- Delete button
      div_ [class_ "flex justify-end"] do
        button_
          [ class_ "btn btn-sm bg-fillError-weak text-textError hover:bg-fillError-strong hover:text-white gap-1"
          , hxGet_ [text|/p/$pidText/delete|]
          , hxConfirm_ "Are you sure you want to delete this project? This action cannot be undone."
          ]
          do
            faSprite_ "trash" "regular" "w-3 h-3"
            span_ "Delete Project"


alertConfiguration :: Bool -> Bool -> Bool -> Bool -> Html ()
alertConfiguration newEndpointsAlerts errorAlerts weeklyReportsAlerts dailyReportsAlerts =
  div_ [class_ "surface-raised rounded-2xl p-4"] do
    div_ [class_ "flex items-center gap-2 mb-4"] do
      div_ [class_ "p-1.5 rounded-md bg-fillWeak"] $ faSprite_ "bell" "regular" "h-3.5 w-3.5 text-textWeak"
      label_ [class_ "text-sm font-medium text-textStrong"] "Notifications"
    div_ [class_ "space-y-1 divide-y divide-strokeWeak"] do
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
teamModal :: Projects.ProjectId -> Maybe ProjectMembers.TeamVM -> Text -> Text -> Text -> Bool -> Html ()
teamModal pid team whiteList channelWhiteList discordWhiteList isInTeamView = do
  let name = maybe "" (.name) team
      handle = maybe "" (.handle) team
      description = maybe "" (.description) team
      prefix = maybe "n" (.handle) team
      modalId = prefix <> "-new-team-modal"
      mkId suffix = prefix <> "-" <> suffix
      membersTags = decodeUtf8 $ AE.encode $ maybe [] (\t -> UUID.toText . (.memberId) <$> t.members) team
      notifEmails = decodeUtf8 $ AE.encode $ maybe [] (.notify_emails) team
      slackChannels = decodeUtf8 $ AE.encode $ maybe [] (.slack_channels) team
      discordChannels = decodeUtf8 $ AE.encode $ maybe [] (.discord_channels) team

  let teamSection_ icon title content = div_ [class_ "space-y-4"] do
        _ <- h3_ [class_ "text-sm font-semibold text-textStrong uppercase tracking-wide flex items-center gap-2"] (faSprite_ icon "regular" "w-4 h-4 text-iconNeutral" >> title)
        content

  let field_ inputId lbl input = fieldset_ [class_ "fieldset"] (label_ [class_ "label text-sm font-medium", Lucid.for_ inputId] lbl >> input)
  let fieldIcon_ icon inputId lbl input = fieldset_ [class_ "fieldset"] do
        _ <- label_ [class_ "label text-sm font-medium flex items-center gap-2", Lucid.for_ inputId] (faSprite_ icon "solid" "w-4 h-4 text-iconNeutral" >> lbl)
        input
  let tagInput_ inputId ph = textarea_ [class_ "textarea w-full min-h-12 resize-none", id_ inputId, placeholder_ ph] ""

  div_ [class_ "modal", role_ "dialog", style_ "--color-base-100: var(--color-fillWeaker)"] do
    label_ [class_ "modal-backdrop", Lucid.for_ modalId] ""
    div_ [class_ "modal-box w-full max-w-2xl flex flex-col"] do
      label_ [Lucid.for_ modalId, class_ "btn btn-sm btn-circle btn-ghost absolute right-3 top-3"] "✕"
      form_ [hxPost_ $ "/p/" <> pid.toText <> "/manage_teams?" <> if isInTeamView then "teamView=true" else "", hxExt_ "json-enc", hxVals_ [text|js:{...getTagValues(`$prefix`)}|], hxTarget_ "#main-content", hxSwap_ "outerHTML", class_ "flex flex-col h-full"] do
        whenJust ((.id) <$> team) \tid -> input_ [type_ "hidden", name_ "teamId", value_ $ UUID.toText tid]

        div_ [class_ "pb-4 border-b border-strokeWeak"] do
          h2_ [class_ "text-xl font-semibold text-textStrong flex items-center gap-2"] (span_ [class_ "p-2 bg-fillBrand-weak rounded-lg"] (faSprite_ "users" "solid" "w-5 h-5 text-textBrand") >> toHtml (if isJust team then "Edit Team" else "Create New Team"))
          p_ [class_ "text-sm text-textWeak mt-1 ml-11"] "Set up your team details, add members, and configure notifications"

        div_ [class_ "flex-1 overflow-y-auto max-h-[60vh] py-6 space-y-6"] do
          teamSection_ "circle-info" "Team Details" do
            div_ [class_ "grid grid-cols-2 gap-4"] do
              field_ (mkId "team-name") "Team Name" $ input_ [class_ "input w-full", type_ "text", id_ $ mkId "team-name", name_ "teamName", placeholder_ "e.g. Backend Team", value_ name, required_ "true"]
              field_ (mkId "team-handle") "Handle" $ input_ [class_ "input w-full", type_ "text", id_ $ mkId "team-handle", name_ "teamHandle", placeholder_ "e.g. backend-team", value_ handle, required_ "true", pattern_ "^[a-z][a-z0-9-]*$"]
            field_ (mkId "team-description") "Description" $ textarea_ [class_ "textarea w-full min-h-20 resize-none", id_ $ mkId "team-description", name_ "teamDescription", placeholder_ "What does this team do?"] $ toHtml description

          teamSection_ "user-plus" "Team Members" $ field_ (mkId "team-members-input") "Add members to this team" $ tagInput_ (mkId "team-members-input") "Start typing to search members..."

          teamSection_ "bell" "Notification Channels" do
            p_ [class_ "text-sm text-textWeak -mt-2"] "Configure where this team receives alerts and notifications"
            div_ [class_ "grid gap-4"] do
              fieldIcon_ "envelope" (mkId "notif-emails-input") "Email Addresses" $ tagInput_ (mkId "notif-emails-input") "Add email addresses..."
              fieldIcon_ "slack" (mkId "slack-channels-input") "Slack Channels" $ tagInput_ (mkId "slack-channels-input") "Add Slack channels..."
              fieldIcon_ "discord" (mkId "discord-channels-input") "Discord Channels" $ tagInput_ (mkId "discord-channels-input") "Add Discord channels..."

        div_ [class_ "pt-4 border-t border-strokeWeak flex justify-end gap-3"] do
          label_ [Lucid.for_ modalId, class_ "btn btn-outline"] "Cancel"
          button_ [class_ "btn btn-primary", type_ "submit"] $ faSprite_ "check" "solid" "w-4 h-4 mr-2" >> if isJust team then "Save Changes" else "Create Team"
  script_
    [text|
function getTagValues(prefix) {
  return {
    teamMembers: window[`$${prefix}-membersTagify`]?.value.map(item => item.value) || [],
    notifEmails: window[`$${prefix}-notifEmailsTagify`]?.value.map(item => item.value || item) || [],
    slackChannels: window[`$${prefix}-slackTagify`]?.value.map(item => item.value) || [],
    discordChannels: window[`$${prefix}-discordTagify`]?.value.map(item => item.value) || [],
    phoneNumbers: []
  }
}
window.addEventListener('DOMContentLoaded', () => {
  const whiteList = $whiteList;
  const emailWhiteList = whiteList.map(m => ({ name: m.email, value: m.email }));

  window[`$prefix-membersTagify`] = createTagify('#$prefix-team-members-input', { enforceWhitelist: true, whitelist: whiteList, tagTextProp: 'name' });
  const memberTags = ($membersTags).map(id => whiteList.find(v => v.value == id)).filter(Boolean);
  window[`$prefix-membersTagify`].addTags(memberTags);

  window[`$prefix-notifEmailsTagify`] = createTagify('#$prefix-notif-emails-input', { whitelist: emailWhiteList, tagTextProp: 'name' });
  window[`$prefix-notifEmailsTagify`].addTags($notifEmails);

  window[`$prefix-slackTagify`] = createTagify('#$prefix-slack-channels-input', { enforceWhitelist: true, whitelist: $channelWhiteList, tagTextProp: 'name' });
  const slackTags = ($slackChannels).map(id => ($channelWhiteList).find(v => v.value == id)).filter(Boolean);
  window[`$prefix-slackTagify`].addTags(slackTags);

  window[`$prefix-discordTagify`] = createTagify('#$prefix-discord-channels-input', { enforceWhitelist: true, whitelist: $discordWhiteList, tagTextProp: 'name' });
  const discordTags = ($discordChannels).map(id => ($discordWhiteList).find(v => v.value == id)).filter(Boolean);
  window[`$prefix-discordTagify`].addTags(discordTags);
});
|]
