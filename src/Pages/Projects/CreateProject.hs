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
  NotifListForm,
  projectSettingsGetH,
  deleteProjectGetH,
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


data CreateProjectFormError = CreateProjectFormError
  { titleE :: Maybe [String]
  , descriptionE :: Maybe [String]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)


createProjectFormToModel :: Projects.ProjectId -> Maybe Text -> Maybe Text -> CreateProjectForm -> Projects.CreateProject
createProjectFormToModel pid subId firstSubId CreateProjectForm{..} =
  Projects.CreateProject
    { id = pid
    , subId = subId
    , firstSubItemId = firstSubId
    , ..
    }


createProjectFormV :: Monad m => Valor CreateProjectForm m CreateProjectFormError
createProjectFormV =
  CreateProjectFormError
    <$> check1 title (failIf ["name can't be empty"] T.null)
    <*> check1 description Valor.pass


----------------------------------------------------------------------------------------------------------
-- createProjectGetH is the handler for the create projects page
createProjectGetH :: ATAuthCtx (Html ())
createProjectGetH = do
  appCtx <- ask @AuthContext
  sess <- Sessions.getSession
  let bwconf =
        (def :: BWConfig)
          { sessM = sess.persistentSession
          , pageTitle = "Endpoints"
          }
  pure $ bodyWrapper bwconf $ createProjectBody (Unsafe.fromJust sess.persistentSession) appCtx.config False (def @CreateProjectForm) (def @CreateProjectFormError) Nothing Nothing


----------------------------------------------------------------------------------------------------------
projectSettingsGetH :: Projects.ProjectId -> ATAuthCtx (Html ())
projectSettingsGetH pid = do
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
  pure $ bodyWrapper bwconf $ createProjectBody pSess appCtx.config True createProj (def @CreateProjectFormError) (Just proj.notificationsChannel) slackInfo


----------------------------------------------------------------------------------------------------------
deleteProjectGetH :: Projects.ProjectId -> ATAuthCtx (Headers '[HXTrigger, HXRedirect] (Html ()))
deleteProjectGetH pid = do
  sess <- Sessions.getSession
  isMember <- dbtToEff $ userIsProjectMember (Unsafe.fromJust sess.persistentSession) pid
  if not isMember
    then do
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"errorToast": ["Only project members can perform this action."]}|]
      pure $ addHeader hxTriggerData $ addHeader "/" $ span_ ""
    else do
      _ <- dbtToEff $ Projects.deleteProject pid
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Deleted Project Successfully"]}|]
      pure $ addHeader hxTriggerData $ addHeader "/" $ span_ ""


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
-- createProjectPostH is the handler for the create projects page form handling.
-- It processes post requests and is expected to return a redirect header and a hyperscript event trigger header.
createProjectPostH :: CreateProjectForm -> ATAuthCtx (Headers '[HXTrigger, HXRedirect] (Html ()))
createProjectPostH createP = do
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Right cpe -> pure $ noHeader $ noHeader $ createProjectBody sess appCtx.config createP.isUpdate createP cpe Nothing Nothing
    Left cp -> processProjectPostForm cp


data FirstSubItem = FirstSubItem
  { id :: Int
  , subscriptionId :: Int
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] FirstSubItem


data Attributes = Attributes
  { firstSubscriptionItem :: FirstSubItem
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] Attributes


data DataVals = DataVals
  { id :: Text
  , attributes :: Attributes
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] DataVals


data SubResponse = SubResponse
  { dataVal :: [DataVals]
  }
  deriving stock (Show, Generic)


instance AE.FromJSON SubResponse where
  parseJSON = AE.withObject "SubResponse" $ \obj -> do
    dataVal <- obj AE..: "data"
    return (SubResponse{dataVal = dataVal})


getSubscriptionId :: Maybe Text -> Text -> IO (Maybe SubResponse)
getSubscriptionId orderId apiKey = do
  case orderId of
    Nothing -> pure Nothing
    Just ordId -> do
      let hds = header "Authorization" .~ ["Bearer " <> encodeUtf8 @Text @ByteString apiKey]
      response <- liftIO $ getWith (defaults & hds) ("https://api.lemonsqueezy.com/v1/orders/" <> toString ordId <> "/subscriptions")
      let responseBdy = response ^. responseBody
      case AE.eitherDecode responseBdy of
        Right res -> do
          return $ Just res
        Left err -> do
          return Nothing


processProjectPostForm :: Valor.Valid CreateProjectForm -> ATAuthCtx (Headers '[HXTrigger, HXRedirect] (Html ()))
processProjectPostForm cpRaw = do
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  let cp = Valor.unValid cpRaw
  pid <- liftIO $ maybe (Projects.ProjectId <$> UUIDV4.nextRandom) pure (Projects.projectIdFromText cp.projectId)
  if cp.isUpdate
    then do
      let hxTriggerDataUpdate = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Updated Project Successfully"]}|]
      let bdy = createProjectBody sess envCfg cp.isUpdate cp (def @CreateProjectFormError) Nothing Nothing
      project <- dbtToEff $ Projects.projectById pid
      case project of
        Just p -> do
          if cp.paymentPlan == "UsageBased" && p.paymentPlan /= "UsageBased"
            then do
              subRes <- liftIO $ getSubscriptionId cp.orderId envCfg.lemonSqueezyApiKey
              let (subId, firstSubItemId) = case subRes of
                    Just sub ->
                      if length sub.dataVal < 1
                        then (Nothing, Nothing)
                        else
                          let target = sub.dataVal Unsafe.!! 0
                              firstSubItemId' = show target.attributes.firstSubscriptionItem.id
                              subId' = show target.attributes.firstSubscriptionItem.subscriptionId
                           in (Just subId', Just firstSubItemId')
                    Nothing -> (Nothing, Nothing)
              if isNothing subId || isNothing firstSubItemId
                then do
                  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"errorToast": ["Couldn't get subscription Id please try again"]}|]
                  let bd = createProjectBody sess envCfg cp.isUpdate cp (def @CreateProjectFormError) Nothing Nothing
                  pure $ addHeader hxTriggerData $ addHeader ("/p/" <> pid.toText <> "/about_project") bd
                else do
                  _ <- dbtToEff $ Projects.updateProject (createProjectFormToModel pid subId firstSubItemId cp)
                  pure $ addHeader hxTriggerDataUpdate $ noHeader bdy
            else do
              _ <- dbtToEff $ Projects.updateProject (createProjectFormToModel pid p.subId p.firstSubItemId cp)
              pure $ addHeader hxTriggerDataUpdate $ noHeader bdy
        Nothing -> do
          let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"errorToast": ["Something went wrong, project not found"]}|]
          pure $ addHeader hxTriggerData $ noHeader $ span_ [] ""
    else do
      let usersAndPermissions = zip cp.emails cp.permissions & uniq
      subRes <- liftIO $ getSubscriptionId cp.orderId envCfg.lemonSqueezyApiKey
      let (subId, firstSubItemId) = case subRes of
            Just sub ->
              if length sub.dataVal < 1
                then (Nothing, Nothing)
                else
                  let target = sub.dataVal Unsafe.!! 0
                      firstSubItemId' = show target.attributes.firstSubscriptionItem.id
                      subId' = show target.attributes.firstSubscriptionItem.subscriptionId
                   in (Just subId', Just firstSubItemId')
            Nothing -> (Nothing, Nothing)
      if (cp.paymentPlan /= "Free" && isNothing firstSubItemId)
        then do
          let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"errorToast": ["Couldn't get subscription Id please try again"]}|]
          let bdy = createProjectBody sess envCfg cp.isUpdate cp (def @CreateProjectFormError) Nothing Nothing
          pure $ addHeader hxTriggerData $ addHeader ("/p/" <> pid.toText <> "/about_project") bdy
        else do
          _ <- dbtToEff do
            Projects.insertProject (createProjectFormToModel pid subId firstSubItemId cp)
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
                _ <- liftIO $ withResource appCtx.pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.InviteUserToProject userId pid email cp.title
                pass
              pure (email, permission, userId)
            let projectMembers =
                  newProjectMembers
                    & filter (\(_, _, id') -> id' /= sess.userId)
                    & map (\(email, permission, id') -> ProjectMembers.CreateProjectMembers pid id' permission)
                    & cons (ProjectMembers.CreateProjectMembers pid sess.userId Projects.PAdmin)
            ProjectMembers.insertProjectMembers projectMembers
          _ <- liftIO $ withResource appCtx.pool \conn ->
            createJob conn "background_jobs" $ BackgroundJobs.CreatedProjectSuccessfully sess.userId pid (original sess.user.getUser.email) cp.title
          let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Created Project Successfully"]}|]
          let bdy = createProjectBody sess envCfg cp.isUpdate cp (def @CreateProjectFormError) Nothing Nothing
          pure $ addHeader hxTriggerData $ addHeader ("/p/" <> pid.toText <> "/about_project") bdy


----------------------------------------------------------------------------------------------------------
-- createProjectBody is the core html view
createProjectBody :: Sessions.PersistentSession -> EnvConfig -> Bool -> CreateProjectForm -> CreateProjectFormError -> Maybe (V.Vector Projects.NotificationChannel) -> Maybe SlackData -> Html ()
createProjectBody sess envCfg isUpdate cp cpe notifChannel slackData = do
  let paymentPlan = if cp.paymentPlan == "" then "UsageBased" else cp.paymentPlan
  section_ [id_ "main-content", class_ "p-3 py-5 sm:p-6 overflow-y-scroll h-full"] do
    div_ [class_ "mx-auto", style_ "max-width:800px"] do
      h2_ [class_ "text-slate-700 text-3xl font-medium mb-5"] $ toHtml @String $ if isUpdate then "Project Settings" else "Create Project"
      div_ [class_ "grid gap-5"] do
        form_
          [ class_ "col-span-1 relative px-3 sm:px-10 border border-gray-200 py-10  bg-white rounded-3xl"
          , hxPost_ "/p/new"
          , hxTarget_ "#main-content"
          , hxSwap_ "outerHTML"
          , id_ "createUpdateBodyForm"
          , hxIndicator_ "#createIndicator"
          ]
          do
            input_ [name_ "isUpdate", type_ "hidden", value_ $ if isUpdate then "true" else "false"]
            input_ [name_ "projectId", type_ "hidden", value_ cp.projectId]
            input_ [name_ "paymentPlan", type_ "hidden", value_ paymentPlan, id_ "paymentPlanEl"]
            div_ do
              label_ [class_ "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"] do
                "Title"
                span_ [class_ "text-red-400"] " *"
              input_
                [ class_ "flex h-9 w-full rounded-lg border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                , type_ "text"
                , id_ "title"
                , name_ "title"
                , value_ cp.title
                , required_ "required"
                ]
            input_ [type_ "hidden", id_ "orderId", name_ "orderId", value_ ""]
            div_ [class_ "flex flex-col gap-1 mt-5"] do
              label_ [class_ "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"] do
                "Timezone"
              select_ [name_ "timeZone", id_ "timezone", class_ "px-4 py-2 border bg-gray-100 rounded-lg"] do
                option_ [value_ cp.timeZone] $ toHtml cp.timeZone
            div_ [class_ "mt-5 "] do
              label_ [class_ "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70"] "Description"
              textarea_
                [ class_ " flex min-h-[60px] w-full rounded-lg border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 "
                , rows_ "4"
                , placeholder_ "Description"
                , id_ "description"
                , name_ "description"
                ]
                $ toHtml cp.description

            div_ [class_ "mt-5"] do
              p_ [class_ "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70 mb-2"] do
                "Please select a plan"
                span_ [class_ "text-red-400"] " *"
              div_ [class_ "grid md:grid-cols-2 gap-4 border-1"] do
                ( [ ("Free", "20k", "$0", "2", cp.paymentPlan == "Free", "Free")
                  , ("Pay as you use", "250k", "$1", "Unlimited", paymentPlan == "UsageBased", "UsageBased")
                  ]
                    :: [(Text, Text, Text, Text, Bool, Text)]
                  )
                  & mapM_ \(title, included, price, team, isSelected, value) -> do
                    let isSelectedTxt = toLower $ show isSelected
                    a_
                      [ class_ $ "payment-plans cursor-pointer space-y-1 border border-1  block p-2  rounded-md " <> if isSelected then " border-2 border-blue-300 shadow-lg" else ""
                      , term
                          "_"
                          [text| 
                          init if $isSelectedTxt then set window.paymentPlan to $value end 
                          on click  set window.paymentPlan to $value
                               then set #paymentPlanEl.value to "$value"
                               then remove .border-2 .border-blue-300 .shadow-lg from .payment-plans
                               then remove .payment-radio-active from .payment-radio 
                               then add .payment-radio-active to (.payment-radio in me)
                               then add .border-2 .border-blue-300 .shadow-lg to me
                               |]
                      ]
                      do
                        div_ [class_ "flex items-center justify-between border-b border-b-1 p-2"] do
                          h4_ [class_ "text-xl font-medium text-slate-700"] $ toHtml title
                          div_ [class_ $ "grid place-items-center h-6 w-6 bg-gray-200 border rounded-full payment-radio " <> if isSelected then "payment-radio-active" else ""] do
                            div_ [class_ "bg-white h-3 w-3 hidden rounded-full"] ""
                        div_ [class_ "text-lg py-3 px-2"] do
                          span_ [class_ "text-2xl text-blue-700"] $ toHtml price
                          if value == "Free"
                            then do span_ [class_ "text-slate-500"] "/month"
                            else do span_ [class_ "text-slate-500"] "/additional 10k requests"
                        div_ [class_ "flex flex-col gap-2 p-3"] do
                          div_ [class_ "flex items-center gap-1"] do
                            checkMark
                            small_ "Max "
                            span_ $ toHtml team
                            small_ " team members"
                          if value == "Free"
                            then do
                              div_ [class_ "flex gap-1 items-center"] do
                                checkMark
                                small_ "20k requests per month"
                              div_ [class_ "flex gap-1 items-center"] do
                                checkMark
                                small_ "7 days data retention"
                            else do
                              div_ [class_ "flex gap-1 items-center"] do
                                checkMark
                                small_ "14 days data retention"
                              div_ [class_ "flex gap-1 items-center"] do
                                checkMark
                                small_ "API testing pipelines"
                              div_ [class_ "flex gap-1 items-center"] do
                                checkMark
                                small_ "API Swagger/OpenAPI Hosting"
                              div_ [class_ "flex gap-1 items-center"] do
                                checkMark
                                small_ "API Metrics Custom Monitors"
                              div_ [class_ "flex gap-1 items-center"] do
                                checkMark
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

            -- LEMON SQUEEZY PAYMENT

            script_ [src_ "https://assets.lemonsqueezy.com/lemon.js"] ("" :: Text)
            let checkoutUrl = envCfg.lemonSqueezyUrl
            script_
              [type_ "text/javascript"]
              [text| 
             window.payLemon = function() {
             const sub = document.getElementById("createIndicator")
             if(sub.classList.contains("htmx-request")) {
              return
              }
             if (document.getElementById("paymentPlanEl").value == "Free"){
                gtag('event', 'conversion', {
                    'send_to': 'AW-11285541899/IUBqCKOA-8sYEIvoroUq',
                });

                gtag('event', 'conversion', {
                    'send_to': 'AW-11285541899/rf7NCKzf_9YYEIvoroUq',
                    'value': 1.0,
                    'currency': 'EUR',
                    'transaction_id': '',
                });
               document.getElementById("orderId").name = "free"
               htmx.trigger("#createUpdateBodyForm", "submit")
               return
             }
             LemonSqueezy.Setup({
               eventHandler: ({event, data}) => {
                 if(event === "Checkout.Success") {
                     document.getElementById("orderId").value = data.order.data.id
                     LemonSqueezy.Url.Close()
                     gtag('event', 'conversion', {
                         'send_to': 'AW-11285541899/rf7NCKzf_9YYEIvoroUq',
                         'value': 20.0,
                         'currency': 'EUR',
                         'transaction_id': '',
                     });
                     htmx.trigger("#createUpdateBodyForm", "submit")
                 }
               }
             })
             LemonSqueezy.Url.Open("$checkoutUrl");
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

            div_ [class_ "p-5 flex w-full justify-end"] do
              -- if isUpdate then
              --     button_ [class_ "inline-block py-2 px-5 bg-blue-700  text-[white] text-sm rounded-xl cursor-pointer"
              --       , type_ "Submit"
              --       ] "Submit"
              --   else
              a_
                [ class_ "lemonsqueezy-button py-2 px-5 w-max bg-blue-700 flex items-center text-[white] text-sm rounded-xl cursor-pointer"
                , [__|on click call window.payLemon() |]
                ]
                do
                  span_ [id_ "createIndicator", class_ "htmx-indicator loading loading-dots loading-md"] ""
                  "Proceed"

      when isUpdate do
        let pid = cp.projectId
        form_ [class_ "mt-10", hxPost_ [text|/p/$pid/notifications-channels|], hxSwap_ "none"] do
          h2_ [class_ "text-slate-700 text-3xl font-medium mb-5"] "Project Notifications"
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

        div_ [class_ "col-span-1 h-full justify-center items-center w-full text-center pt-24"] do
          h2_ [class_ "text-red-800 font-medium pb-4"] "Delete project. This is dangerous and unreversible."
          button_
            [ class_ "btn btn-sm bg-red-800 text-white shadow-md hover:bg-red-700 cursor-pointer rounded-md"
            , hxGet_ [text|/p/$pid/delete|]
            , hxConfirm_ "Are you sure you want to delete this project?"
            ]
            "Delete Project"


checkMark :: Html ()
checkMark =
  div_ [class_ "flex items-center justify-center text-center font-bold text-green-500 rounded-md w-5 h-5 bg-gray-200"] "✓"
