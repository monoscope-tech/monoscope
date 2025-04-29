{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pages.Projects.CreateProject (
  CreateProjectForm (..),
  createProjectPostH,
  createProjectFormV,
  createProjectFormToModel,
  CreateProjectFormError (..),
  pricingUpdateH,
  PricingUpdateForm (..),
  projectSettingsGetH,
  projectOnboarding,
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
import Data.ByteString.Base64 qualified as B64
import Data.CaseInsensitive qualified as CI
import Data.Default (Default (..))
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq
import Data.Effectful.Wreq qualified as W
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Valor (Valor, check1, failIf, validateM)
import Data.Valor qualified as Valor
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import GHC.Records (HasField (getField))
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
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (paymentPlanPicker)
import Pkg.ConvertKit qualified as ConvertKit
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import Servant (addHeader)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers)
import System.Config
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent, redirectCS)
import Utils (faSprite_, insertIfNotExist, isDemoAndNotSudo, lookupValueText)
import Web.FormUrlEncoded (FromForm)


data CreateProjectForm = CreateProjectForm
  { title :: Text
  , description :: Text
  , emails :: [Text]
  , permissions :: [ProjectMembers.Permissions]
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


createProjectFormToModel :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> CreateProjectForm -> Projects.CreateProject
createProjectFormToModel pid subId firstSubId orderId paymentPlan CreateProjectForm{..} =
  Projects.CreateProject
    { id = pid
    , subId = subId
    , firstSubItemId = firstSubId
    , paymentPlan = paymentPlan
    , orderId = orderId
    , ..
    }


createProjectFormV :: Monad m => Valor CreateProjectForm m CreateProjectFormError
createProjectFormV =
  CreateProjectFormError
    <$> check1 title (failIf ["name can't be empty"] T.null)
    <*> check1 description Valor.pass


projectOnboarding :: ATAuthCtx (Headers '[Header "Location" Text] ((PageCtx (Html ()))))
projectOnboarding = do
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess <- Sessions.getSession
  projects <- dbtToEff $ Projects.selectProjectsForUser sess.persistentSession.userId
  let projectM = V.find (\pr -> pr.paymentPlan == "ONBOARDING") projects
      bwconf = (def :: BWConfig){sessM = Just sess, currProject = Nothing, pageTitle = "New Project"}
  case projectM of
    Just p -> do
      let h = "/p/" <> p.id.toText <> "/onboarding"
      pure $ addHeader h $ PageCtx bwconf ""
    _ -> do
      pid <- Projects.ProjectId <$> UUID.genUUID
      let pr = Projects.CreateProject{id = pid, title = "Onboarding Project", description = "", paymentPlan = "ONBOARDING", timeZone = "", subId = Nothing, firstSubItemId = Nothing, orderId = Nothing}
      dbtToEff $ Projects.insertProject pr
      projectKeyUUID <- UUID.genUUID
      let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 $ ProjectApiKeys.encryptAPIKey (encodeUtf8 envCfg.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
      pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID "Default API Key" encryptedKeyB64
      dbtToEff $ ProjectApiKeys.insertProjectApiKey pApiKey
      let projectMember = ProjectMembers.CreateProjectMembers pid sess.user.id Projects.PAdmin
      _ <- dbtToEff $ ProjectMembers.insertProjectMembers [projectMember]
      let h = "/p/" <> pid.toText <> "/onboarding"
      pure $ addHeader h $ PageCtx bwconf ""


----------------------------------------------------------------------------------------------------------
-- createProjectGetH is the handler for the create projects page
-- createProjectGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders CreateProject)
-- createProjectGetH pid = do
--   appCtx <- ask @AuthContext
--   sess <- Sessions.getSession
--   let bwconf =
--         (def :: BWConfig)
--           { sessM = Just sess
--           , pageTitle = "Create Project"
--           }
--   addRespHeaders $ CreateProject $ PageCtx bwconf (sess.persistentSession, pid, appCtx.config, False, def @CreateProjectForm, def @CreateProjectFormError)

data CreateProjectResp = CreateProjectResp
  { sess :: Sessions.PersistentSession
  , pid :: Projects.ProjectId
  , env :: EnvConfig
  , paymentPlan :: Text
  , form :: CreateProjectForm
  , formError :: CreateProjectFormError
  }
  deriving stock (Show, Generic)


data CreateProject
  = CreateProject (PageCtx (Sessions.PersistentSession, Projects.ProjectId, EnvConfig, Text, Bool, CreateProjectForm, CreateProjectFormError))
  | PostNoContent Text
  | ProjectPost CreateProjectResp
  deriving stock (Show, Generic)


instance HasField "unwrapCreateProjectResp" CreateProject (Maybe CreateProjectResp) where
  getField (CreateProject _) = Nothing
  getField (PostNoContent _) = Nothing
  getField (ProjectPost cpr) = Just cpr


instance ToHtml CreateProject where
  toHtml (CreateProject (PageCtx bwconf (sess, pid, config, paymentPlan, isUpdate, prf, pref))) = toHtml $ PageCtx bwconf $ createProjectBody sess pid config paymentPlan prf pref
  toHtml (PostNoContent message) = span_ [class_ ""] $ toHtml message
  toHtml (ProjectPost cpr) = toHtml $ createProjectBody cpr.sess cpr.pid cpr.env cpr.paymentPlan cpr.form cpr.formError
  toHtmlRaw = toHtml


----------------------------------------------------------------------------------------------------------
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
          }

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Project settings"
          , isSettingsPage = True
          }
  addRespHeaders $ CreateProject $ PageCtx bwconf (sess.persistentSession, pid, appCtx.config, project.paymentPlan, True, createProj, def @CreateProjectFormError)


----------------------------------------------------------------------------------------------------------
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


----------------------------------------------------------------------------------------------------------
-- createProjectPostH is the handler for the create projects page form handling.
-- It processes post requests and is expected to return a redirect header and a hyperscript event trigger header.
createProjectPostH :: Projects.ProjectId -> CreateProjectForm -> ATAuthCtx (RespHeaders CreateProject)
createProjectPostH pid createP = do
  sess <- Sessions.getSession
  appCtx <- ask @AuthContext
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Right cpe -> addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid appCtx.config "" createP cpe)
    Left cp -> processProjectPostForm cp pid


data FirstSubItem = FirstSubItem
  { id :: Int
  , subscriptionId :: Int
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] FirstSubItem


data Attributes = Attributes
  { firstSubscriptionItem :: FirstSubItem
  , productName :: Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] Attributes


data DataVals = DataVals
  { id :: Text
  , attributes :: Attributes
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] DataVals


newtype SubResponse = SubResponse
  { dataVal :: [DataVals]
  }
  deriving stock (Show, Generic)


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
        Right res -> do
          return $ Just res
        Left err -> do
          return Nothing
data PricingUpdateForm = PricingUpdateForm
  { orderIdM :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm, Default)


pricingUpdateH :: Projects.ProjectId -> PricingUpdateForm -> ATAuthCtx (RespHeaders (Html ()))
pricingUpdateH pid PricingUpdateForm{orderIdM} = do
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
              foundUsFrom = fromMaybe "" $ project.questions >>= (\x -> lookupValueText x "foundUsFrom")
          createJob conn "background_jobs" $ BackgroundJobs.SendDiscordData sess.user.id pid fullName [foundUsFrom] foundUsFrom
        users <- dbtToEff $ ProjectMembers.selectActiveProjectMembers pid
        forM_ users $ \user -> ConvertKit.addUserOrganization envCfg.convertkitApiKey (CI.original user.email) pid.toText project.title name

  case orderIdM of
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
          }
  let envCfg = appCtx.config
      lemon = envCfg.lemonSqueezyUrl <> "&checkout[custom][project_id]=" <> pid.toText
      critical = envCfg.lemonSqueezyCriticalUrl <> "&checkout[custom][project_id]=" <> pid.toText
  addRespHeaders $ PageCtx bwconf $ pricingPage_ pid lemon critical project.paymentPlan


pricingPage_ :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
pricingPage_ pid lemon critical paymentPlan = do
  section_ [class_ "w-full h-full overflow-y-auto py-12"] do
    div_ [class_ "flex flex-col max-w-4xl mx-auto gap-10 px-4"] do
      h1_ [class_ "font-semibold text-4xl text-textStrong"] "Update pricing"
      paymentPlanPicker pid lemon critical paymentPlan


processProjectPostForm :: Valor.Valid CreateProjectForm -> Projects.ProjectId -> ATAuthCtx (RespHeaders CreateProject)
processProjectPostForm cpRaw pid = do
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess <- Sessions.getSession

  let cp = Valor.unValid cpRaw
  if isDemoAndNotSudo pid sess.user.isSudo
    then do
      addErrorToast "Can't perform this action on the demo project" Nothing
      addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid envCfg "" cp (def @CreateProjectFormError))
    else do
      project <- dbtToEff $ Projects.projectById pid
      case project of
        Just p -> do
          _ <- dbtToEff $ Projects.updateProject (createProjectFormToModel pid p.subId p.firstSubItemId p.orderId p.paymentPlan cp)
          addSuccessToast "Updated Project Successfully" Nothing
          addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid envCfg "" cp (def @CreateProjectFormError))
        Nothing -> do
          addErrorToast "Something went wrong. Please try again." Nothing
          redirectCS ("/p/" <> pid.toText <> "/about_project")
          addRespHeaders $ ProjectPost (CreateProjectResp sess.persistentSession pid envCfg "" cp (def @CreateProjectFormError))


----------------------------------------------------------------------------------------------------------
-- createProjectBody is the core html view
createProjectBody :: Sessions.PersistentSession -> Projects.ProjectId -> EnvConfig -> Text -> CreateProjectForm -> CreateProjectFormError -> Html ()
createProjectBody sess pid envCfg paymentPlan cp cpe = do
  section_ [id_ "main-content", class_ "overflow-y-scroll h-full text-textWeak"] do
    div_ [class_ "mx-auto px-2 pt-12", style_ "max-width:800px"] do
      h2_ [class_ "text-slate-700 text-3xl font-medium mb-2"] "Project Settings"
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

          div_ [class_ "flex w-full justify-end items-center"] do
            -- a_ [href_ $ "/p/" <> pid.toText <> "/update_pricing", class_ "text-textBrand font-medium"] "Update pricing"
            button_
              [ class_
                  "lemonsqueezy-button py-2 px-5 w-max bg-blue-700 flex items-center text-[white]  rounded-xl cursor-pointer"
              , type_ "submit"
              ]
              do
                span_ [id_ "createIndicator", class_ "htmx-indicator loading loading-dots loading-md"] ""
                "Update project"

      div_ [class_ "border-t pt-8"] do
        div_ [class_ "self-stretch justify-start text-textStrong text-base font-semibold font-['Inter']"] "Upgrade plan"
        p_ [class_ "text-textWeak text-sm max-w-[513px] mt-3"] "This is APItoolkit pricing, click on compare feature below to select the option that best suit your project."

      div_ [class_ "border border-strokeWeak rounded-2xl h-32 flex items-center px-6 py-8 overflow-hidden mt-8 relative"] do
        div_ [class_ "w-full h-36 rotate-y-5 rotate-z-15 right-[-40px] top-[-95px] z-0 absolute bg-gradient-to-b from-slate-500/10 to-slate-500/0"] pass
        div_ [class_ "flex items-center justify-between w-full"] do
          div_ [class_ "flex flex-col gap-1"] do
            span_ [class_ "text-textStrong font-semibold"] $ toHtml paymentPlan
            span_ [class_ "rounded-2xl text-textWeak bg-fillWeaker border border-strokeWeak py-1 text-sm px-3"] "current plan"
          div_ [class_ "flex items-center gap-1 mt-4"] do
            div_ [class_ "flex items-end"] do
              span_ [class_ "text-textStrong text-xl"] "$"
              span_ [class_ "text-4xl text-textStrong"] $ if paymentPlan == "Free" then "0" else if paymentPlan == "Critical Systems Plan" then "199" else "34"
            div_ [class_ "flex flex-col text-text-Weak text-sm"] do
              span_ [class_ ""] "Starts at"
              span_ [class_ ""] "/per month"
          label_ [class_ "btn btn-secondary bg-white cursor pointer z-10", Lucid.for_ "pricing-modal", [__|on click set #pricing-modal.check to true|]] "Change plan"

          input_ [type_ "checkbox", id_ "pricing-modal", class_ "modal-toggle"]
          div_ [class_ "modal p-8", role_ "dialog", [__|on closeModal from body set #pricing-modal.checked to false |]] do
            div_ [class_ "modal-box relative flex flex-col gap-5 w-[1250px] py-16 px-32", style_ "max-width:1300px"] $ do
              div_ [class_ "absolute top-8 right-8"] do
                faSprite_ "circle-xmark" "regular" "h-4 w-4"
              div_ [class_ "text-center text-sm text-textWeak w-full mx-auto max-w-96"] do
                span_ [class_ " text-textStrong text-2xl font-semibold"] "What’s Included?"
                p_ [class_ "mt-2 mb-4"] "See and compare what you get in each plan."
                p_ [] "Please adjust the bar below to see difference in price as your events increase"
              let lemonUrl = envCfg.lemonSqueezyUrl <> "&checkout[custom][project_id]=" <> pid.toText
                  critical = envCfg.lemonSqueezyCriticalUrl <> "&checkout[custom][project_id]=" <> pid.toText
              paymentPlanPicker pid lemonUrl critical paymentPlan
            label_ [class_ "modal-backdrop", Lucid.for_ "pricing-modal"] "Close"

      let pidText = pid.toText
      div_ [class_ "border border-red-500 gap-5 w-full p-8 my-24 rounded-lg bg-red-50"] do
        h1_ [class_ "text-red-500 font-medium text-xl"] "Danger zone"
        p_ [class_ "py-4"] "Delete project. This is dangerous and unreversible."
        button_
          [ class_ "btn btn-sm bg-red-500 text-white shadow-md hover:bg-red-700 cursor-pointer"
          , hxGet_ [text|/p/$pidText/delete|]
          , hxConfirm_ "Are you sure you want to delete this project?"
          ]
          "Delete Project"
