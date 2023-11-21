{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Projects.CreateProject (
  CreateProjectForm,
  createProjectGetH,
  createProjectPostH,
  createProjectFormV,
  createProjectFormToModel,
  CreateProjectFormError,
  projectSettingsGetH,
  deleteProjectGetH,
  updateNotificationsChannel,
) where

import BackgroundJobs qualified
import Config
import Data.Aeson (encode)
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
import Database.PostgreSQL.Entity.DBT (withPool)
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
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.ConvertKit qualified as ConvertKit
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (
  Headers,
  addHeader,
  noHeader,
 )
import Servant.Htmx
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
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm, Default)


data CreateProjectFormError = CreateProjectFormError
  { titleE :: Maybe [String]
  , descriptionE :: Maybe [String]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)


createProjectFormToModel :: Projects.ProjectId -> CreateProjectForm -> Projects.CreateProject
createProjectFormToModel pid CreateProjectForm{..} = Projects.CreateProject{id = pid, ..}


createProjectFormV :: Monad m => Valor CreateProjectForm m CreateProjectFormError
createProjectFormV =
  CreateProjectFormError
    <$> check1 title (failIf ["name can't be empty"] T.null)
    <*> check1 description Valor.pass


checkEmail :: Text -> Bool
checkEmail = isJust . T.find (== '@')


----------------------------------------------------------------------------------------------------------
-- createProjectGetH is the handler for the create projects page
createProjectGetH :: Sessions.PersistentSession -> DashboardM (Html ())
createProjectGetH sess = do
  envCfg <- asks env
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Endpoints"
          }
  pure $ bodyWrapper bwconf $ createProjectBody sess envCfg False (def @CreateProjectForm) (def @CreateProjectFormError) Projects.NEmail Nothing


----------------------------------------------------------------------------------------------------------
projectSettingsGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
projectSettingsGetH sess pid = do
  pool <- asks pool
  envCfg <- asks env
  Just proj <- liftIO $ withPool pool $ Projects.selectProjectForUser (sess.userId, pid)
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
          }
  slackInfo <- liftIO $ withPool pool $ getProjectSlackData pid

  -- FIXME: Should be a value from the db

  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just proj, pageTitle = "Settings"}
  pure $ bodyWrapper bwconf $ createProjectBody sess envCfg True createProj (def @CreateProjectFormError) proj.notificationsChannel slackInfo


----------------------------------------------------------------------------------------------------------
deleteProjectGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
deleteProjectGetH sess pid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"errorToast": ["Only project members can perform this action."]}|]
      pure $ addHeader hxTriggerData $ addHeader "/" $ span_ ""
    else do
      _ <- liftIO $ withPool pool $ Projects.deleteProject pid
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Deleted Project Successfully"]}|]
      pure $ addHeader hxTriggerData $ addHeader "/" $ span_ ""


updateNotificationsChannel :: Sessions.PersistentSession -> Projects.ProjectId -> Text -> DashboardM (Headers '[HXTrigger] (Html ()))
updateNotificationsChannel sess pid channel = do
  pool <- asks pool
  _ <- liftIO $ withPool pool $ Projects.updateNotificationsChannel pid channel
  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Updated Notifications Channel Successfully"]}|]
  pure $ addHeader hxTriggerData $ span_ ""


----------------------------------------------------------------------------------------------------------
-- createProjectPostH is the handler for the create projects page form handling.
-- It processes post requests and is expected to return a redirect header and a hyperscript event trigger header.
createProjectPostH :: Sessions.PersistentSession -> CreateProjectForm -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
createProjectPostH sess createP = do
  envCfg <- asks env
  pool <- asks pool
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Right cpe -> pure $ noHeader $ noHeader $ createProjectBody sess envCfg createP.isUpdate createP cpe Projects.NEmail Nothing
    Left cp -> processProjectPostForm sess cp


processProjectPostForm :: Sessions.PersistentSession -> Valor.Valid CreateProjectForm -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
processProjectPostForm sess cpRaw = do
  envCfg <- asks env
  pool <- asks pool
  let cp = Valor.unValid cpRaw
  pid <- liftIO $ maybe (Projects.ProjectId <$> UUIDV4.nextRandom) pure (Projects.projectIdFromText cp.projectId)
  _ <-
    if cp.isUpdate
      then do
        _ <- liftIO $ withPool pool $ Projects.updateProject (createProjectFormToModel pid cp)
        pass
      else do
        let usersAndPermissions = zip cp.emails cp.permissions & uniq
        _ <- liftIO $ withPool pool do
          Projects.insertProject (createProjectFormToModel pid cp)
          projectKeyUUID <- liftIO UUIDV4.nextRandom
          let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 envCfg.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
          let encryptedKeyB64 = B64.encodeBase64 encryptedKey
          let keyPrefix = encryptedKeyB64
          pApiKey <- liftIO $ ProjectApiKeys.newProjectApiKeys pid projectKeyUUID "Default API Key" keyPrefix
          ProjectApiKeys.insertProjectApiKey pApiKey

          liftIO $ ConvertKit.addUserOrganization envCfg.convertkitApiKey (CI.original sess.user.getUser.email) pid.toText cp.title cp.paymentPlan
          newProjectMembers <- forM usersAndPermissions \(email, permission) -> do
            userId' <- runMaybeT $ MaybeT (Users.userIdByEmail email) <|> MaybeT (Users.createEmptyUser email)
            let userId = Unsafe.fromJust userId'
            liftIO $ ConvertKit.addUserOrganization envCfg.convertkitApiKey email pid.toText cp.title cp.paymentPlan
            when (userId' /= Just sess.userId) do
              -- invite the users to the project (Usually as an email)
              _ <- liftIO $ withResource pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.InviteUserToProject userId pid email cp.title
              pass
            pure (email, permission, userId)

          let projectMembers =
                newProjectMembers
                  & filter (\(_, _, id') -> id' /= sess.userId)
                  & map (\(email, permission, id') -> ProjectMembers.CreateProjectMembers pid id' permission)
                  & cons (ProjectMembers.CreateProjectMembers pid sess.userId Projects.PAdmin)
          ProjectMembers.insertProjectMembers projectMembers

        _ <- liftIO $ withResource pool \conn ->
          createJob conn "background_jobs" $ BackgroundJobs.CreatedProjectSuccessfully sess.userId pid (original sess.user.getUser.email) cp.title
        pass

  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Created Project Successfully"]}|]
  let hxTriggerDataUpdate = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Updated Project Successfully"]}|]
  let bdy = createProjectBody sess envCfg cp.isUpdate cp (def @CreateProjectFormError) Projects.NEmail Nothing
  if cp.isUpdate
    then pure $ addHeader hxTriggerDataUpdate $ noHeader bdy
    else pure $ addHeader hxTriggerData $ addHeader ("/p/" <> pid.toText <> "/about_project") bdy


----------------------------------------------------------------------------------------------------------
-- createProjectBody is the core html view
createProjectBody :: Sessions.PersistentSession -> EnvConfig -> Bool -> CreateProjectForm -> CreateProjectFormError -> Projects.NotificationChannel -> Maybe SlackData -> Html ()
createProjectBody sess envCfg isUpdate cp cpe notifChannel slackData = do
  let paymentPlan = if cp.paymentPlan == "" then "Hobby" else cp.paymentPlan
  section_ [id_ "main-content", class_ "p-3 py-5 sm:p-6"] do
    div_ [class_ "mx-auto", style_ "max-width:800px"] do
      h2_ [class_ "text-slate-700 text-3xl font-medium mb-5"] $ toHtml @String $ if isUpdate then "Project Settings" else "Create Project"
      div_ [class_ "grid gap-5"] do
        form_ [class_ "col-span-1 relative px-3 sm:px-10 border border-gray-200 py-10  bg-white rounded-3xl", hxPost_ "/p/new", hxTarget_ "#main-content", hxSwap_ "outerHTML", id_ "createUpdateBodyForm"] do
          input_ [name_ "isUpdate", type_ "hidden", value_ $ if isUpdate then "true" else "false"]
          input_ [name_ "projectId", type_ "hidden", value_ cp.projectId]
          input_ [name_ "paymentPlan", type_ "hidden", value_ paymentPlan, id_ "paymentPlanEl"]
          div_ do
            label_ [class_ "text-slate-700 mx-2 text-sm"] do
              "Title"
              span_ [class_ "text-red-400"] " *"
            input_
              [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl "
              , type_ "text"
              , id_ "title"
              , name_ "title"
              , value_ cp.title
              ]
          div_ [class_ "flex flex-col gap-1 mt-5"] do
            label_ [class_ "text-slate-700 mx-2 text-sm"] do
              "Timezone"
            select_ [name_ "timeZone", id_ "timezone", class_ "px-4 py-2 border rounded-2xl"] do
              option_ [value_ cp.timeZone] $ toHtml cp.timeZone
          div_ [class_ "mt-5 "] do
            label_ [class_ "text-slate-700 mx-2 text-sm"] "Description"
            textarea_
              [ class_ " py-2 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-1 "
              , rows_ "4"
              , placeholder_ "Description"
              , id_ "description"
              , name_ "description"
              ]
              $ toHtml cp.description

          div_ [class_ "mt-5"] do
            p_ [class_ "text-slate-700 mx-2 pb-2 text-sm"] do
              "Please select a plan"
              span_ [class_ "text-red-400"] " *"
            div_ [class_ "grid md:grid-cols-3 gap-4 border-1"] do
              ( [ -- ("Free", "20k", "$0", "1", cp.paymentPlan == "Free", "''")
                  ("Hobby", "250k", "$20", "3", paymentPlan == "Hobby", if envCfg.paddleSandbox then envCfg.paddleSandboxHobby else envCfg.paddleHobby)
                , ("Startup", "1m", "$50", "5", paymentPlan == "Startup", if envCfg.paddleSandbox then envCfg.paddleSandboxStartup else envCfg.paddleStartup)
                , ("Growth", "10m", "$250", "10", paymentPlan == "Growth", if envCfg.paddleSandbox then envCfg.paddleSandboxGrowth else envCfg.paddleGrowth)
                ]
                  :: [(Text, Text, Text, Text, Bool, Text)]
                )
                & mapM_ \(title, included, price, team, isSelected, paddleSubsCode) -> do
                  let isSelectedTxt = toLower $ show isSelected
                  a_
                    [ class_ $ "payment-plans cursor-pointer space-y-1 border border-1  block p-2 rounded-md  shadow-blue-100 " <> if isSelected then " border-blue-200 shadow-lg" else ""
                    , term
                        "_"
                        [text| 
                          init if $isSelectedTxt then set window.paymentPlan to $paddleSubsCode end 
                          on click set window.paymentPlan to $paddleSubsCode
                                           then set #paymentPlanEl.value to "$title"
                                           then remove .border-blue-200 .shadow-lg from .payment-plans
                                           then remove .payment-radio-active from .payment-radio 
                                           then add .payment-radio-active to (.payment-radio in me)
                                           then add .border-blue-200 .shadow-lg to me
                                           |]
                    ]
                    do
                      div_ [class_ "flex items-center justify-between border-b border-b-1 p-2"] do
                        h4_ [class_ "text-xl font-medium text-slate-700"] $ toHtml title
                        div_ [class_ $ "grid place-items-center h-6 w-6 bg-gray-200 border rounded-full payment-radio " <> if isSelected then "payment-radio-active" else ""] do
                          div_ [class_ "bg-white h-3 w-3 hidden rounded-full"] ""
                      div_ [class_ "text-lg py-3"] do
                        span_ [class_ "text-2xl text-blue-700"] $ toHtml price
                        span_ [class_ "text-slate-500"] "/mo"
                      div_ [class_ "flex items-center gap-1"] do
                        faIcon_ "fa-check" "fa-light fa-check" "text-green-500 h-3 w-3"
                        strong_ [] $ toHtml included
                        small_ " Reqs/mo included"
                      div_ [class_ "flex items-center gap-1"] do
                        faIcon_ "fa-check" "fa-light fa-check" "text-green-500 h-3 w-3"
                        small_ "max "
                        span_ $ toHtml team
                        small_ " team members"
                      if paddleSubsCode == ""
                        then do
                          div_ [class_ "flex gap-1 items-center"] do
                            faIcon_ "fa-check" "fa-light fa-check" "text-green-500 h-3 w-3"
                            small_ "7days data retention"
                        else do
                          div_ [class_ "flex gap-1 items-center"] do
                            faIcon_ "fa-check" "fa-light fa-check" "text-green-500 h-3 w-3"
                            small_ "14days data retention"
                          div_ [class_ "flex gap-1 items-center"] do
                            faIcon_ "fa-check" "fa-light fa-check" "text-green-500 h-3 w-3"
                            small_ "API testing pipelines"
                          div_ [class_ "flex gap-1 items-center"] do
                            faIcon_ "fa-check" "fa-light fa-check" "text-green-500 h-3 w-3"
                            small_ "API Swagger/OpenAPI Hosting"
                          div_ [class_ "flex gap-1 items-center"] do
                            faIcon_ "fa-check" "fa-light fa-check" "text-green-500 h-3 w-3"
                            small_ "API Metrics Custom Monitors"
                          div_ [class_ "flex gap-1 items-center"] do
                            faIcon_ "fa-check" "fa-light fa-check" "text-green-500 h-3 w-3"
                            small_ "API Live Traffic AI based validations"

          div_ [class_ $ "mt-10 " <> if isUpdate then "hidden" else ""] do
            p_ [class_ "text-slate-400 mx-2 font-light text-sm"] "Invite a project member"
            section_ [id_ "inviteMemberSection"] do
              template_ [id_ "inviteTmpl"] do
                div_ [class_ "flex flex-row space-x-2"] do
                  input_ [name_ "emails", class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 font-light border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "name@example.com"]
                  select_ [name_ "permissions", class_ "w-1/3 h-10 px-5  my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] do
                    option_ [class_ "text-slate-500", value_ "edit"] "Can Edit"
                    option_ [class_ "text-slate-500", value_ "view"] "Can View"
                  button_
                    [ [__| on click remove the closest parent <div/> then halt |]
                    ]
                    $ img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
            a_
              [ class_ "bg-transparent inline-flex cursor-pointer mt-2"
              , [__| on click append #inviteTmpl.innerHTML to #inviteMemberSection then 
                         _hyperscript.processNode(#inviteMemberSection) then halt |]
              ]
              do
                faIcon_ "fa-plus" "fa-sharp fa-regular fa-plus" "mx-2 w-4 h-4 text-blue-700"
                span_ [class_ "text-blue-700 font-medium text-sm "] "Add member"

          -- START PADDLE payment
          script_ [src_ "https://cdn.paddle.com/paddle/paddle.js"] ("" :: Text)
          let paddleSandbox = if envCfg.paddleSandbox then "Paddle.Environment.set('sandbox');" else ""
          let paddleVendor = if envCfg.paddleSandbox then envCfg.paddleSandboxVendorId else envCfg.paddleVendorId
          let projectId = cp.projectId
          let userId = sess.userId.toText
          let email = CI.original sess.user.getUser.email
          script_
            [type_ "text/javascript"]
            [text|
            // NOTE: Default
           //window.paymentPlan = "Hobby";
           $paddleSandbox 
           Paddle.Setup({ vendor: $paddleVendor });
 
           window.paymentAction = function(){
             const paymentPlan = document.getElementById("paymentPlanEl").value; 
             console.log("PaymentPlan", document.getElementById("paymentPlanEl").value)
             console.log("window.paymentPlan", window.paymentPlan)
             if (document.getElementById("paymentPlanEl").value == "Free"){
                // Free plan simple signup conversion
                gtag('event', 'conversion', {
                    'send_to': 'AW-11285541899/IUBqCKOA-8sYEIvoroUq',
                });

                gtag('event', 'conversion', {
                    'send_to': 'AW-11285541899/rf7NCKzf_9YYEIvoroUq',
                    'value': 1.0,
                    'currency': 'EUR',
                    'transaction_id': '',
                });

               htmx.trigger("#createUpdateBodyForm", "submit")
               return
             }

              gtag('event', 'conversion', {
                  'send_to': 'AW-11285541899/rf7NCKzf_9YYEIvoroUq',
                  'value': 20.0,
                  'currency': 'EUR',
                  'transaction_id': '',
              });
 
             const passthrough = {
               projectId: "$projectId",
               userId: "$userId",
             };
 
             window.onPaddleSuccess = function (x) {
               htmx.trigger("#createUpdateBodyForm", "submit")
             };
             window.onPaddleClose = function () {
             };
 
             Paddle.Checkout.open({
               product: window.paymentPlan,
               email: "$email",
               disableLogout: true,
               passthrough: JSON.stringify(passthrough),
               closeCallback: 'onPaddleClose',
               successCallback: 'onPaddleSuccess',
             });
           };

           const timezoneSelect = document.getElementById("timezone");
           const timeZones = Intl.supportedValuesOf('timeZone');
           timeZones.forEach((tz) => {
             const option = document.createElement("option");
             option.value = tz;
             option.text = tz;
             timezoneSelect.appendChild(option);
           });
         |]
          -- END PADDLE payment
          div_ [class_ "p-5 text-right"] do
            -- if isUpdate then
            --     button_ [class_ "inline-block py-2 px-5 bg-blue-700  text-[white] text-sm rounded-xl cursor-pointer"
            --       , type_ "Submit"
            --       ] "Submit"
            --   else
            a_
              [ class_ "inline-block py-2 px-5 bg-blue-700  text-[white] text-sm rounded-xl cursor-pointer"
              , [__|on click call window.paymentAction() |]
              ]
              "Proceed"

      when isUpdate do
        let pid = cp.projectId
        div_ [class_ "mt-10"] do
          h2_ [class_ "text-slate-700 text-3xl font-medium mb-5"] "Project Notifications"
          div_ [class_ "flex flex-col gap-4 border p-6 rounded-2xl"] do
            p_ [] "Select a notification channel to receive updates on this project"
            case notifChannel of
              Projects.NSlack -> span_ [class_ "text-sm text-blue-500 font-bold"] "Current notifications channel is Slack"
              _ -> span_ [class_ "text-sm text-blue-500 font-bold"] "Current notifications channel is Email"
            div_ [class_ "bg-gray-100 p-6 rounded-lg"] do
              div_ [class_ "flex justify-between items-center mb-6"] do
                h3_ [class_ "text-2xl font-bold"] "Email"
                case notifChannel of
                  Projects.NSlack -> button_ [class_ "btn btn-primary", hxPost_ [text|/p/$pid/notifications-toggle/email|], hxSwap_ "none"] "Use Email"
                  _ -> pass
              input_ [value_ "All users on this project", disabled_ "true", name_ "email", class_ "w-full p-2 my-2 text-sm bg-white text-slate-700 border rounded"]
            div_ [class_ "bg-gray-100 p-6"] do
              div_ [class_ "flex justify-between items-center mb-6"] do
                h3_ [class_ "text-2xl font-bold"] "Slack"
                case notifChannel of
                  Projects.NEmail -> button_ [class_ "btn btn-primary", hxPost_ [text|/p/$pid/notifications-toggle/slack|], hxSwap_ "none"] "Use Slack"
                  _ -> pass
              form_ [class_ "flex flex-col rounded-lg", hxPost_ [text|/p/$pid/slack/webhook|], hxSwap_ "none"] do
                label_ [] "Slack webhook"
                div_ [class_ "flex gap-2 items-center"] do
                  input_ [type_ "hidden", name_ "projects", value_ pid]
                  input_ [value_ (maybe "" (\s -> s.webhookUrl) slackData), placeholder_ "https://hooks.slack.com/services/xxxxxxxxx/xxxxxxxx/xxxxxxxxxxx", name_ "webhookUrl", class_ "w-full p-2 my-2 text-sm bg-white text-slate-700 border rounded"]
                  button_ [class_ "text-white bg-blue-600 rounded-lg px-4 py-1 w-max"] "Save"
              span_ [class_ "my-6 text-sm text-gray-500"] "OR"
              a_ [href_ $ "https://slack.com/oauth/v2/authorize?client_id=6211090672305.6200958370180&scope=chat:write,incoming-webhook&user_scope=&redirect_uri=" <> envCfg.slackRedirectUri <> pid, class_ "mt-4"] do
                img_ [alt_ "Add to Slack", height_ "40", width_ "139", src_ "https://platform.slack-edge.com/img/add_to_slack.png", term "srcSet" "https://platform.slack-edge.com/img/add_to_slack.png 1x, https://platform.slack-edge.com/img/add_to_slack@2x.png 2x"]

        div_ [class_ "col-span-1 h-full justify-center items-center w-full text-center pt-24"] do
          h2_ [class_ "text-red-800 font-medium pb-4"] "Delete project. This is dangerous and unreversable"
          button_
            [ class_ "btn btn-sm bg-red-800 text-white shadow-md hover:bg-red-700 cursor-pointer rounded-md"
            , hxGet_ [text|/p/$pid/delete|]
            , hxConfirm_ "Are you sure you want to delete this project?"
            ]
            "Delete Project"
