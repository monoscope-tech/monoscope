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
  ManageMembersForm (..),
  manageSubGetH,
  ManageMembers (..),
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
)
where

import BackgroundJobs qualified
import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64
import Data.CaseInsensitive (original)
import Data.CaseInsensitive qualified as CI
import Data.Default (Default (..))
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq
import Data.Effectful.Wreq qualified as W
import Data.List.Unique (uniq)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.UUID qualified as RealUUID
import Data.Valor (Valor, check1, failIf, validateM)
import Data.Valor qualified as Valor
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Fmt
import GHC.Records (HasField (getField))
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Slack (SlackData, getDiscordDataByProjectId, getProjectSlackData)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Network.Wreq (getWith)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), bodyWrapper)
import Pages.Components (paymentPlanPicker)
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
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent, redirectCS)
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

  projects <- dbtToEff $ Projects.selectProjectsForUser sess.persistentSession.userId
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

    when (not $ V.null projects) $ div_ [class_ "mb-12"] do
      h3_ [class_ "text-textWeak text-lg font-medium mb-4"] "Your Projects"
      div_ [class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"] $ mapM_ projectCard_ $ V.toList projects

    when showDemoProject $ div_ [] do
      h3_ [class_ "text-textWeak text-lg font-medium mb-4"] "Demo Project"
      div_ [class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"] $ projectCard_ demoProject


projectCard_ :: Projects.Project' -> Html ()
projectCard_ project = do
  div_ [class_ "bg-base-100 border border-strokeWeak rounded-xl shadow-sm hover:shadow-md transition-shadow duration-200 overflow-hidden group"] do
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

          when (not $ V.null project.usersDisplayImages) do
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
  addRespHeaders $ bodyWrapper bwconf $ integrationsBody sess.persistentSession appCtx.config True createProj (Just project.notificationsChannel) project.whatsappNumbers slackInfo


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
      _ <- dbtToEff $ Projects.updateNotificationsChannel pid notificationsChannel phones
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


integrationsBody :: Sessions.PersistentSession -> EnvConfig -> Bool -> CreateProjectForm -> Maybe (V.Vector Projects.NotificationChannel) -> V.Vector Text -> Maybe SlackData -> Html ()
integrationsBody sess envCfg isUpdate cp notifChannel phones slackData = do
  section_ [id_ "main-content", class_ "p-3 py-5 sm:p-6 overflow-y-scroll h-full"] do
    div_ [class_ "mx-auto", style_ "max-width:1000px"] do
      let pid = cp.title -- Using title as pid placeholder, will be fixed
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
            renderNotificationOption "Slack" "Send notifications to Slack channels" "slack" Projects.NSlack notifChannel (faSprite_ "slack" "solid" "h-6 w-6") (renderSlackIntegration envCfg "" slackData)
            renderNotificationOption "Discord" "Send notifications to Discord servers" "discord" Projects.NDiscord notifChannel (faSprite_ "discord" "solid" "h-6 w-6") (renderDiscordIntegration envCfg "")
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
  projMembers <- dbtToEff $ ProjectMembers.selectActiveProjectMembers pid

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
            V.toList projMembers
              & filter (\pm -> not $ any (\(email, _) -> original pm.email == email) usersAndPermissions)
              & filter (\a -> a.userId /= currUserId)
              & map (.id)

      newProjectMembers <- forM uAndPNew \(email, permission) -> do
        userId' <- dbtToEff do
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
      _ <- dbtToEff $ ProjectMembers.insertProjectMembers projectMembers

      unless (null uAndPOldAndChanged)
        $ void
          . dbtToEff
        $ ProjectMembers.updateProjectMembersPermissons uAndPOldAndChanged

      unless (null deletedUAndP)
        $ void
          . dbtToEff
        $ ProjectMembers.softDeleteProjectMembers deletedUAndP

      projMembersLatest <- dbtToEff $ ProjectMembers.selectActiveProjectMembers pid
      if isJust onboardingM
        then do
          redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=Integration"
          addRespHeaders $ ManageMembersPost projMembersLatest
        else do
          addSuccessToast "Updated Members List Successfully" Nothing
          addRespHeaders $ ManageMembersPost projMembersLatest
    else do
      addErrorToast "Only one member allowed on Free plan" Nothing
      addRespHeaders $ ManageMembersPost projMembers


manageMembersGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders ManageMembers)
manageMembersGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  projMembers <- dbtToEff $ ProjectMembers.selectActiveProjectMembers pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Manage members"
          , currProject = Just project
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ ManageMembersGet $ PageCtx bwconf projMembers


data ManageMembers
  = ManageMembersGet {unwrapGet :: PageCtx (V.Vector ProjectMembers.ProjectMemberVM)}
  | ManageMembersPost {unwrapPost :: V.Vector ProjectMembers.ProjectMemberVM}


instance ToHtml ManageMembers where
  toHtml (ManageMembersGet (PageCtx bwconf memebers)) = toHtml $ PageCtx bwconf $ manageMembersBody memebers
  toHtml (ManageMembersPost memebers) = toHtml $ manageMembersBody memebers
  toHtmlRaw = toHtml


manageMembersBody :: V.Vector ProjectMembers.ProjectMemberVM -> Html ()
manageMembersBody projMembers =
  div_ [id_ "main-content", class_ "w-full py-16"] do
    section_ [class_ "p-6 w-[606px] mx-auto"] do
      h2_ [class_ "text-textStrong mb-4 text-xl font-semibold"] "Manage Access"
      p_ [class_ "text-textWeak text-sm leading-tight"] "We'll email them instructions and a link to sign in"
      form_
        [ class_ "my-8 flex flex-col gap-8"
        , hxPost_ ""
        , hxTarget_ "#main-content"
        , hxSwap_ "outerHTML"
        , hxIndicator_ "#submitIndicator"
        ]
        do
          div_ [class_ "flex gap-2 w-full"] do
            input_ [type_ "text", name_ "emails", class_ "input w-full", placeholder_ "Add a member by email"]
            select_ [name_ "permissions", class_ "select w-[130px]"] do
              option_ [class_ "text-textWeak", value_ "admin"] "Admin"
              option_ [class_ "text-textWeak", value_ "edit"] "Can Edit"
              option_ [class_ "text-textWeak", value_ "view"] "Can View"
            button_ [class_ "btn btn-secondary"] "Send invite"
          div_ [class_ "flex w-full flex-col gap-4"] do
            h3_ [class_ "text-textWeak font-semibold"] "Members"
            div_ [class_ "flex flex-col gap-2"] do
              mapM_ memberRow projMembers
            button_ [class_ "self-end btn btn-primary mt-2"] "Update settings"


memberRow :: ProjectMembers.ProjectMemberVM -> Html ()
memberRow prM = do
  let email = CI.original prM.email
  div_ [class_ "w-full  px-1.5 py-3 rounded-lg  border border-transparent hover:border-strokeWeak gap-4 hover:bg-fillWeak flex justify-between items-center"] $ do
    div_ [data_ "size" "Small", class_ "w-full grow-1 flex items-center gap-2"] $ do
      div_ [class_ "w-8 h-8 relative rounded-[32px] flex items-center text-xs justify-center outline outline-1 outline-offset-[-1px] outline-strokeWeak text-textWeak uppercase font-medium"] $ toHtml $ T.take 2 email
      div_ [class_ "inline-flex flex-col items-start"] do
        input_ [type_ "text", name_ "emails", value_ email, class_ "focus:border-none focus:outline-0 text-textStrong text-sm font-normal leading-tight"]

    div_ [class_ "flex items-center gap-4"] $ do
      let permission = prM.permission
      select_ [name_ "permissions", class_ "w-max text-textWeak text-sm font-normal leading-tight"] do
        option_ ([class_ "text-textWeak", value_ "admin"] <> selectedIf ProjectMembers.PAdmin permission) "Admin"
        option_ ([class_ "text-textWeak", value_ "edit"] <> selectedIf ProjectMembers.PEdit permission) "Can edit"
        option_ ([class_ "text-textWeak", value_ "view"] <> selectedIf ProjectMembers.PView permission) "View only"
    button_ [[__| on click remove the closest parent <div/> then halt |]] do
      faSprite_ "trash" "regular" "w-4 h-4 text-textWeak"
  where
    selectedIf :: ProjectMembers.Permissions -> ProjectMembers.Permissions -> [Attribute]
    selectedIf a b = [selected_ "" | a == b]


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
      let hds = header "Authorization" .~ ["Bearer " <> encodeUtf8 @Text @ByteString apiKey]
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
  projects <- dbtToEff $ Projects.selectProjectsForUser sess.persistentSession.userId
  let projectM = V.find (\pr -> pr.paymentPlan == "ONBOARDING") projects
      bwconf = (def :: BWConfig){sessM = Just sess, currProject = Nothing, pageTitle = "New Project", config = appCtx.config}
  case projectM of
    Just p -> do
      let h = "/p/" <> p.id.toText <> "/onboarding"
      pure $ addHeader h $ PageCtx bwconf ""
    _ -> do
      pid <- UUIDId <$> UUID.genUUID
      let pr = Projects.CreateProject{id = pid, title = "Onboarding Project", description = "", paymentPlan = "ONBOARDING", timeZone = "", subId = Nothing, firstSubItemId = Nothing, orderId = Nothing, weeklyNotif = True, dailyNotif = True, endpointAlerts = True, errorAlerts = True}
      dbtToEff $ Projects.insertProject pr
      projectKeyUUID <- UUID.genUUID
      let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 $ ProjectApiKeys.encryptAPIKey (encodeUtf8 envCfg.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
      pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID "Default API Key" encryptedKeyB64
      dbtToEff $ ProjectApiKeys.insertProjectApiKey pApiKey
      let projectMember = ProjectMembers.CreateProjectMembers pid sess.user.id ProjectMembers.PAdmin
      _ <- dbtToEff $ ProjectMembers.insertProjectMembers [projectMember]
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
      _ <- dbtToEff $ Projects.deleteProject pid
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
      let hds = header "Authorization" .~ ["Bearer " <> encodeUtf8 @Text @ByteString apiKey]
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
      updatePricing name sid fid oid = dbtToEff $ Projects.updateProjectPricing pid name sid fid oid newStepsComp
      handleOnboarding name = when (project.paymentPlan == "ONBOARDING") $ do
        _ <- liftIO $ withResource appCtx.pool \conn -> do
          let fullName = sess.user.firstName <> " " <> sess.user.lastName
              foundUsFrom = fromMaybe "" $ project.questions >>= (`lookupValueText` "foundUsFrom")
          createJob conn "background_jobs" $ BackgroundJobs.SendDiscordData sess.user.id pid fullName [foundUsFrom] foundUsFrom
        users <- dbtToEff $ ProjectMembers.selectActiveProjectMembers pid
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
        users <- dbtToEff $ ProjectMembers.selectActiveProjectMembers pid
        let usersToDel = (\u -> u.id) <$> V.tail users
        _ <- dbtToEff $ ProjectMembers.softDeleteProjectMembers $ V.toList usersToDel
        pass
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
      _ <- dbtToEff $ Projects.updateProject (createProjectFormToModel pid project.subId project.firstSubItemId project.orderId project.paymentPlan cp)
      addSuccessToast "Updated Project Successfully" Nothing
      addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid envCfg "" cp (def @CreateProjectFormError) project)


createProjectBody :: Sessions.PersistentSession -> Projects.ProjectId -> EnvConfig -> Text -> CreateProjectForm -> CreateProjectFormError -> Projects.Project -> Html ()
createProjectBody sess pid envCfg paymentPlan cp cpe proj = do
  section_ [id_ "main-content", class_ "overflow-y-scroll h-full text-textWeak"] do
    div_ [class_ "mx-auto px-2 pt-12 w-[800px]"] do
      h2_ [class_ "text-textStrong mb-3 text-xl font-semibold"] "Manage Project"
      p_ [class_ "text-textWeak text-sm leading-tight"] "Manage your project details and upgrade your plan"
      form_
        [ class_ "py-8 flex flex-col gap-8 w-full"
        , hxPost_ $ "/p/update/" <> pid.toText
        , hxTarget_ "#main-content"
        , hxSwap_ "outerHTML"
        , id_ "createUpdateBodyForm"
        , hxIndicator_ "#createIndicator"
        ]
        do
          div_ [class_ "flex flex-col gap-1 w-full"] do
            label_ [class_ "text-textStrong leading-normal"] do
              "Project title"
              span_ [class_ ""] " *"
            input_
              [ class_ "input w-full"
              , type_ "text"
              , id_ "title"
              , name_ "title"
              , value_ cp.title
              , required_ "required"
              ]
          div_ [class_ "flex flex-col gap-1 w-full"] do
            label_ [class_ "text-textStrong leading-normal"] do
              "Timezone"
            select_ [name_ "timeZone", id_ "timezone", class_ "select w-full"] do
              option_ [value_ cp.timeZone] $ toHtml cp.timeZone
          div_ [class_ "flex flex-col gap-1 w-full"] do
            label_ [class_ "text-textStrong leading-normal"] "Description"
            textarea_
              [ class_ "textarea w-full"
              , rows_ "4"
              , placeholder_ "Description"
              , id_ "description"
              , name_ "description"
              ]
              $ toHtml cp.description

          alertConfiguration (isJust cp.endpointAlerts) (isJust cp.errorAlerts) (isJust cp.weeklyNotifs) (isJust cp.dailyNotifs)

          div_ [class_ "flex w-full justify-end items-center"] do
            button_
              [class_ "btn btn-primary cursor-pointer", type_ "submit"]
              do
                span_ [id_ "createIndicator", class_ "htmx-indicator loading loading-dots loading-md"] ""
                "Update project"

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
          , pageTitle = "Delete Project"
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ PageCtx bwconf $ deleteProjectBody pid


deleteProjectBody :: Projects.ProjectId -> Html ()
deleteProjectBody pid = do
  let pidText = pid.toText
  section_ [class_ "px-60 w-max"] do
    div_ [class_ " gap-5 w-full my-24"] do
      h1_ [class_ "text-textStrong font-semibold text-2xl"] "Delete project"
      p_ [class_ "py-4 text-sm text-textWeak"] "This is action not reversible, only delete a project you no longer need."
      button_
        [ class_ "btn btn-sm bg-fillError-strong text-white shadow-md hover:bg-fillError-strong cursor-pointer"
        , hxGet_ [text|/p/$pidText/delete|]
        , hxConfirm_ "Are you sure you want to delete this project?"
        ]
        "Delete Project"


alertConfiguration :: Bool -> Bool -> Bool -> Bool -> Html ()
alertConfiguration newEndpointsAlerts errorAlerts weeklyReportsAlerts dailyReportsAlerts =
  div_ [class_ "space-y-6"] $ do
    div_ [class_ "flex items-center gap-3"] $ do
      div_ $ do
        h3_ [class_ "text-textStrong mb-3 text-lg font-semibold"] "Alert Configuration"
        p_ [class_ "text-textWeak text-sm leading-tight"] "Manage your notification preferences"
    div_ [class_ "space-y-4 rounded-lg border border-strokeWeak p-6"] $ do
      let switchRow :: Text -> Text -> Text -> Bool -> Html ()
          switchRow lbl forId descr checked =
            div_ [class_ "flex items-center justify-between"] $ do
              div_ [class_ "space-y-0.5"] $ do
                label_ [Lucid.for_ forId, class_ "text-sm font-medium text-textStrong"] (toHtmlRaw lbl)
                p_ [class_ "text-xs text-textWeak"] (toHtmlRaw descr)
              input_ $ [type_ "checkbox", id_ forId, class_ "switch", name_ forId] ++ [checked_ | checked]
      switchRow "Receive new endpoint alerts" "endpointAlerts" "Get notified when new API endpoints are detected" newEndpointsAlerts
      hr_ [class_ "border-border my-2"]
      switchRow "Receive runtime error alerts" "errorAlerts" "Receive immediate notifications for system errors" errorAlerts
      hr_ [class_ "border-border my-2"]
      switchRow "Receive weekly reports alerts" "weeklyNotifs" "Get a summary of your project activity every week" weeklyReportsAlerts
      hr_ [class_ "border-border my-2"]
      switchRow "Receive daily reports alerts" "dailyNotifs" "Receive daily summaries of your project metrics" dailyReportsAlerts
