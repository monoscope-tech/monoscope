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
import Data.Default (Default (..))
import Data.List.Extra (cons)
import Data.List.Unique (uniq)
import Data.Pool (withResource)
import Data.Text (toLower)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Valor (Valor, check1, failIf, validateM)
import Data.Valor qualified as Valor
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxConfirm_, hxGet_, hxIndicator_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMembers qualified as Projects
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Network.Wreq (defaults, getWith, header, responseBody)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.ConvertKit qualified as ConvertKit
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import Servant (addHeader, noHeader)
import System.Config
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, redirectCS)
import Utils (faSprite_, isDemoAndNotSudo, lemonSqueezyUrls, lemonSqueezyUrlsAnnual)
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
createProjectGetH :: ATAuthCtx (RespHeaders (Html ()))
createProjectGetH = do
  appCtx <- ask @AuthContext
  sess <- Sessions.getSession
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , pageTitle = "Endpoints"
          }
  addRespHeaders $ bodyWrapper bwconf $ createProjectBody (sess.persistentSession) appCtx.config False (def @CreateProjectForm) (def @CreateProjectFormError)


----------------------------------------------------------------------------------------------------------
projectSettingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
projectSettingsGetH pid = do
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

  let bwconf = (def :: BWConfig){sessM = Just sess.persistentSession, currProject = Just project, pageTitle = "Settings"}
  addRespHeaders $ bodyWrapper bwconf $ createProjectBody (sess.persistentSession) appCtx.config True createProj (def @CreateProjectFormError)


----------------------------------------------------------------------------------------------------------
deleteProjectGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
deleteProjectGetH pid = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  if (isDemoAndNotSudo pid sess.user.isSudo)
    then do
      addSuccessToast "Can't perform this action on the demon project" Nothing
      addRespHeaders ""
    else do
      _ <- dbtToEff $ Projects.deleteProject pid
      _ <- liftIO $ withResource appCtx.pool \conn ->
        createJob conn "background_jobs" $ BackgroundJobs.DeletedProject pid
      addSuccessToast "Deleted Project Successfully" Nothing
      redirectCS "/" >> addRespHeaders ""


----------------------------------------------------------------------------------------------------------
-- createProjectPostH is the handler for the create projects page form handling.
-- It processes post requests and is expected to return a redirect header and a hyperscript event trigger header.
createProjectPostH :: CreateProjectForm -> ATAuthCtx (RespHeaders (Html ()))
createProjectPostH createP = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Right cpe -> addRespHeaders $ createProjectBody (sess.persistentSession) appCtx.config createP.isUpdate createP cpe
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


processProjectPostForm :: Valor.Valid CreateProjectForm -> ATAuthCtx (RespHeaders (Html ()))
processProjectPostForm cpRaw = do
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess <- Sessions.getSession

  let cp = Valor.unValid cpRaw
  pid <- liftIO $ maybe (Projects.ProjectId <$> UUIDV4.nextRandom) pure (Projects.projectIdFromText cp.projectId)
  if cp.isUpdate
    then do
      if isDemoAndNotSudo pid sess.user.isSudo
        then do
          addErrorToast "Can't perform this action on the demo project" Nothing
          addRespHeaders $ createProjectBody sess.persistentSession envCfg cp.isUpdate cp (def @CreateProjectFormError)
        else do
          let hxTriggerDataUpdate = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Updated Project Successfully"]}|]
          let bdy = createProjectBody sess.persistentSession envCfg cp.isUpdate cp (def @CreateProjectFormError)
          project <- dbtToEff $ Projects.projectById pid
          case project of
            Just p -> do
              if (cp.paymentPlan == "UsageBased" && p.paymentPlan /= "UsageBased")
                || (cp.paymentPlan == "GraduatedPricing" && p.paymentPlan /= "GraduatedPricing")
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
                      let bd = createProjectBody sess.persistentSession envCfg cp.isUpdate cp (def @CreateProjectFormError)
                      pure $ addHeader hxTriggerData $ addHeader ("/p/" <> pid.toText <> "/about_project") bd
                    else do
                      _ <- dbtToEff $ Projects.updateProject (createProjectFormToModel pid subId firstSubItemId cp)
                      pure $ addHeader hxTriggerDataUpdate $ noHeader bdy
                else do
                  _ <- dbtToEff $ Projects.updateProject (createProjectFormToModel pid p.subId p.firstSubItemId cp)
                  pure $ addHeader hxTriggerDataUpdate $ noHeader bdy
            Nothing -> do
              let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"errorToast": ["Something went wrong, try again."]}|]
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
          addErrorToast "Couldn't get subscription ID. Please try again" Nothing
          redirectCS ("/p/" <> pid.toText <> "/about_project")
          addRespHeaders $ createProjectBody sess.persistentSession envCfg cp.isUpdate cp (def @CreateProjectFormError)
        else do
          _ <- dbtToEff do
            Projects.insertProject (createProjectFormToModel pid subId firstSubItemId cp)
            projectKeyUUID <- liftIO UUIDV4.nextRandom
            let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 envCfg.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
            let encryptedKeyB64 = B64.encodeBase64 encryptedKey
            let keyPrefix = encryptedKeyB64
            pApiKey <- liftIO $ ProjectApiKeys.newProjectApiKeys pid projectKeyUUID "Default API Key" keyPrefix
            ProjectApiKeys.insertProjectApiKey pApiKey
            liftIO $ ConvertKit.addUserOrganization envCfg.convertkitApiKey (CI.original sess.user.email) pid.toText cp.title cp.paymentPlan
            newProjectMembers <-
              catMaybes <$> forM usersAndPermissions \(email, permission) -> do
                userId' <- runMaybeT $ MaybeT (Users.userIdByEmail email) <|> MaybeT (Users.createEmptyUser email)
                liftIO $ ConvertKit.addUserOrganization envCfg.convertkitApiKey email pid.toText cp.title cp.paymentPlan
                when (userId' /= Just sess.user.id) do
                  case userId' of
                    Just userId -> do
                      -- invite the users to the project (Usually as an email)
                      _ <- liftIO $ withResource appCtx.pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.InviteUserToProject userId pid email cp.title
                      pass
                    Nothing -> pass
                pure $ maybe Nothing (\userId -> Just (email, permission, userId)) userId'
            let projectMembers =
                  newProjectMembers
                    & filter (\(_, _, id') -> id' /= sess.user.id)
                    & map (\(email, permission, id') -> ProjectMembers.CreateProjectMembers pid id' permission)
                    & cons (ProjectMembers.CreateProjectMembers pid sess.user.id Projects.PAdmin)
            ProjectMembers.insertProjectMembers projectMembers
          _ <- liftIO $ withResource appCtx.pool \conn ->
            createJob conn "background_jobs" $ BackgroundJobs.CreatedProjectSuccessfully sess.user.id pid (original sess.user.email) cp.title
          addSuccessToast "Created Project Successfully" Nothing
          redirectCS ("/p/" <> pid.toText <> "/about_project")
          addRespHeaders $ createProjectBody sess.persistentSession envCfg cp.isUpdate cp (def @CreateProjectFormError)


----------------------------------------------------------------------------------------------------------
-- createProjectBody is the core html view
createProjectBody :: Sessions.PersistentSession -> EnvConfig -> Bool -> CreateProjectForm -> CreateProjectFormError -> Html ()
createProjectBody sess envCfg isUpdate cp cpe = do
  let paymentPlan = if cp.paymentPlan == "" then "UsageBased" else cp.paymentPlan
  section_ [id_ "main-content", class_ "p-3 py-5 sm:p-6 overflow-y-scroll h-full"] do
    div_ [class_ "mx-auto", style_ "max-width:1000px"] do
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
              div_ [class_ "grid sm:grid-cols-2 md:grid-cols-2 gap-24 border-1"] do
                ( [ ("Free", "20k", "$0", "2", cp.paymentPlan == "Free", "Free")
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
                        div_ [class_ "text-lg py-3 px-2"] do
                          span_ [class_ "text-2xl text-blue-700"] $ toHtml price
                          case value of
                            "Free" -> do
                              span_ [class_ "text-slate-500"] "/month"
                            _ -> span_ [class_ "text-slate-500"] "/10k requests"
                        checkList value team
                let isSelected = paymentPlan == "GraduatedPricing"
                let isSelectedTxt = toLower $ show $ isSelected
                let value = "GraduatedPricing"
                a_
                  [ class_ $ "payment-plans cursor-pointer space-y-1 border border-1 block p-2 rounded-md " <> if isSelected then " border-2 border-blue-300 shadow-lg" else ""
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
                    div_ [class_ "flex items-center justify-between border-b border-b-1 p-1"] do
                      h4_ [class_ "text-xl font-medium text-slate-700"] $ toHtml "Pay as you use"
                      div_ [role_ "tablist", class_ "tabs tabs-boxed"] $ do
                        input_ [onchange_ "handlePlanToggle(e)", value_ "month", type_ "radio", name_ "plans", role_ "tab", class_ "tab", term "aria-label" "Monthly", checked_]
                        input_ [onchange_ "handlePlanToggle(e)", value_ "annual", type_ "radio", name_ "plans", role_ "tab", class_ "tab", term "aria-label" "Annual"]
                    div_ [class_ "text-lg py-3 px-2"] do
                      span_ [class_ "text-2xl text-blue-700", id_ "price"] $ toHtml "$19"
                      span_ [class_ "text-slate-500", id_ "num_requests"] "/200k"
                      span_ [class_ "text-slate-500 mr-3"] " requests"
                      p_ [class_ "text-blue-500 inline-block mt-0 text-sm text-green-500 font-semibold"] do
                        span_ [] "save "
                        span_ [id_ "save_container"] ""
                      span_ [class_ "text-blue-500 text-sm block mt-2"] "then $1 per 10k requests"
                    div_ [] do
                      input_ [type_ "range", min_ "0", max_ "6", step_ "1", value_ "0", class_ "range range-primary range-sm", id_ "price_range"]

                    checkList "GR" "Unlimited"

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
                      , class_ "cursor-pointer"
                      ]
                      $ faSprite_ "trash" "regular" "w-4 h-4"
              a_
                [ class_ "bg-transparent inline-flex cursor-pointer mt-2"
                , [__| on click put #inviteTmpl.innerHTML at end of #inviteMemberSection then 
                         _hyperscript.processNode(#inviteMemberSection) then halt |]
                ]
                do
                  faSprite_ "plus" "regular" "mx-2 w-4 h-4 text-blue-700"
                  span_ [class_ "text-blue-700 font-medium text-sm "] "Add member"

            -- LEMON SQUEEZY PAYMENT

            script_ [src_ "https://assets.lemonsqueezy.com/lemon.js"] ("" :: Text)
            let checkoutUrl = envCfg.lemonSqueezyUrl
            let graduatedCheckoutOne = V.head lemonSqueezyUrls
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
             if(document.getElementById("paymentPlanEl").value == "GraduatedPricing") {
                  LemonSqueezy.Url.Open(window.graduatedRangeUrl);
              }else {
                 LemonSqueezy.Url.Open("$checkoutUrl");
              }
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
            let lmnUrls = decodeUtf8 $ encode $ lemonSqueezyUrls
            let lmnUrlAnnual = decodeUtf8 $ encode $ lemonSqueezyUrlsAnnual
            script_
              [text|
               const price_indicator = document.querySelector("#price_range");
               window.graduatedRangeUrl = "$graduatedCheckoutOne"
               let plan = "month";
               const prices = [19, 49, 88, 215, 420, 615, 800]
               const saves = [1, 6, 12, 35, 80, 135, 200]
               const reqs = ["200k","550k", "1M", "2.5M", "5M", "7.5M", "10M"]
               const pricesYr = [200, 588, 1056, 2580, 5000, 5000, 5000]
               const savesYr = [28, 72,144,420,960,960,960]
               const reqsYr = ["2.4M", "6.6M", "12M", "30M", "60M", "60M", "60M"]
               const urls = $lmnUrls
               const urlsAnnual = $lmnUrlAnnual
               const priceContainer = document.querySelector("#price")
               const reqsContainer = document.querySelector("#num_requests")
               const saveContainer = document.querySelector("#save_container")
               function priceChange() {
                 const value = price_indicator.value
                 let price = prices[value]
                 let num_reqs = reqs[value]
                 let sav = saves[value] + "/month"
                 window.graduatedRangeUrl = urls[value]
                 if(plan === "annual") {
                    price = pricesYr[value]
                    num_reqs = reqsYr[value]
                    sav = savesYr[value] + "/year"
                    window.graduatedRangeUrl = urlsAnnual[value]
                  }
                 priceContainer.innerText = "$" + price
                 reqsContainer.innerText = "/" + num_reqs
                 saveContainer.innerText = "$" + sav 
                 
               }
               price_indicator.addEventListener('input', priceChange)

               function handlePlanToggle(e) {
                  const radios = document.getElementsByName("plans")
                  for(let radio of radios) {
                    if(radio.checked) {
                        plan = radio.value
                        break
                    }
                  }
                  priceChange()
               }
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


checkList :: Text -> Text -> Html ()
checkList value team =
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
          small_ "API swagger/OpenAPI hosting"
        div_ [class_ "flex gap-1 items-center"] do
          checkMark
          small_ "API metrics custom monitors"
        div_ [class_ "flex gap-1 items-center"] do
          checkMark
          small_ "API live traffic AI-based validations"
