module Pages.Projects.ManageMembers (
  manageMembersGetH,
  manageMembersPostH,
  ManageMembersForm (..),
  manageSubGetH,
  ManageMembers (..),
)
where

import BackgroundJobs qualified
import Control.Lens qualified as Lens
import Data.CaseInsensitive qualified as CI

import Data.Aeson qualified as AE
import Data.CaseInsensitive (original)
import Data.Default (def)
import Data.List.Unique (uniq)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Network.Wreq
import OddJobs.Job (createJob)
import Pages.BodyWrapper
import Relude hiding (ask, asks)
import System.Config
import System.Types
import Utils
import Web.FormUrlEncoded (FromForm)


data ManageMembersForm = ManageMembersForm
  { emails :: [Text]
  , permissions :: [ProjectMembers.Permissions]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm)


manageMembersPostH :: Projects.ProjectId -> Maybe Text -> ManageMembersForm -> ATAuthCtx (RespHeaders ManageMembers)
manageMembersPostH pid onboardingM form = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let currUserId = sess.persistentSession.userId
  projMembers <- dbtToEff $ ProjectMembers.selectActiveProjectMembers pid
  -- TODO:
  -- Separate the new emails from the old emails
  -- Insert the new emails and permissions.
  -- Update the permissions only of the existing emails.

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
              & map (.id) -- We should not allow deleting the current user from the project

      -- Create new users and send notifications
      newProjectMembers <- forM uAndPNew \(email, permission) -> do
        userId' <- dbtToEff do
          userIdM' <- Users.userIdByEmail email
          case userIdM' of
            Nothing -> do
              idM' <- Users.createEmptyUser email -- NEXT Trigger email sending
              case idM' of
                Nothing -> error "duplicate email in createEmptyUser"
                Just idX -> pure idX
            Just idX -> pure idX

        when (userId' /= currUserId) $
          void $
            liftIO $
              withResource appCtx.pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.InviteUserToProject currUserId pid email project.title -- invite the users to the project (Usually as an email)
        pure (email, permission, userId')

      let projectMembers =
            newProjectMembers
              & filter (\(_, _, id') -> id' /= currUserId)
              & map (\(email, permission, id') -> ProjectMembers.CreateProjectMembers pid id' permission)
      _ <- dbtToEff $ ProjectMembers.insertProjectMembers projectMembers -- insert new project members

      -- Update existing contacts with updated permissions
      -- TODO: Send a notification via background job, about the users permission having been updated.
      unless (null uAndPOldAndChanged)
        $ void
          . dbtToEff
        $ ProjectMembers.updateProjectMembersPermissons uAndPOldAndChanged

      -- soft delete project members with id
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
      p_ [class_ "text-textWeak text-sm leading-tight"] "We’ll email them instructions and a link to sign in"
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
              option_ [class_ "text-gray-500", value_ "admin"] "Admin"
              option_ [class_ "text-gray-500", value_ "edit"] "Can Edit"
              option_ [class_ "text-gray-500", value_ "view"] "Can View"
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
        option_ ([class_ "text-gray-500", value_ "admin"] <> selectedIf ProjectMembers.PAdmin permission) "Admin"
        option_ ([class_ "text-gray-500", value_ "edit"] <> selectedIf ProjectMembers.PEdit permission) "Can edit"
        option_ ([class_ "text-gray-500", value_ "view"] <> selectedIf ProjectMembers.PView permission) "View only"
    button_ [[__| on click remove the closest parent <div/> then halt |]] do
      faSprite_ "trash" "regular" "w-4 h-4 text-textWeak"
  where
    selectedIf :: ProjectMembers.Permissions -> ProjectMembers.Permissions -> [Attribute]
    selectedIf a b = [selected_ "" | a == b]


-- div_ [class_ "mt-6"] do
--       section_ [id_ "inviteMemberSection"] do
--         mapM_ (projectMemberRow . Just) projMembers
--         template_ [id_ "inviteTmpl"] $ projectMemberRow Nothing
--       a_
--         [ class_ "bg-transparent inline-flex cursor-pointer mt-2"
--         , [__| on click put #inviteTmpl.innerHTML at end of #inviteMemberSection then
--                     _hyperscript.processNode(#inviteMemberSection) then halt |]
--         ]
--         do
--           faSprite_ "plus" "regular" "mt-1 mx-2 w-3 h-3 text-blue-700"
--           span_ [class_ "text-blue-700 font-medium  "] "Add member"
--     button_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 flex items-center bottom-0 right-0 text-[white]  rounded-xl cursor-pointer", type_ "submit"] do
--       "Submit"
--       span_ [id_ "submitIndicator", class_ "loading loading-dots loading-sm htmx-indicator"] ""

projectMemberRow :: Maybe ProjectMembers.ProjectMemberVM -> Html ()
projectMemberRow projMembersM =
  div_ [class_ "flex flex-row space-x-2"] do
    input_
      [ name_ "emails"
      , class_ "w-2/3 h-10 px-5 my-2 w-full  bg-base-100 text-slate-700 font-light border-solid border border-gray-200 rounded-2xl border-0 "
      , placeholder_ "name@example.com"
      , value_ (maybe "" (original . (.email)) projMembersM)
      ]
    let permission = maybe ProjectMembers.PView (.permission) projMembersM
    select_ [name_ "permissions", class_ "w-1/3 h-10 px-5  my-2 w-full  bg-base-100 text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] do
      option_ ([class_ "text-gray-500", value_ "admin"] <> selectedIf ProjectMembers.PAdmin permission) "Admin"
      option_ ([class_ "text-gray-500", value_ "edit"] <> selectedIf ProjectMembers.PEdit permission) "Can Edit"
      option_ ([class_ "text-gray-500", value_ "view"] <> selectedIf ProjectMembers.PView permission) "Can View"
    button_
      [ [__| on click remove the closest parent <div/> then halt |]
      ]
      $ faSprite_ "trash-can" "regular" "w-3 h-3 text-red-700"
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


getSubscriptionPortalUrl :: Maybe Text -> Text -> IO (Maybe SubResponse)
getSubscriptionPortalUrl subId apiKey = do
  case subId of
    Nothing -> do
      return Nothing
    Just sid -> do
      let hds = header "Authorization" Lens..~ ["Bearer " <> encodeUtf8 @Text @ByteString apiKey]
      response <- liftIO $ getWith (defaults & hds) ("https://api.lemonsqueezy.com/v1/subscriptions/" <> toString sid)
      let responseBdy = response Lens.^. responseBody
      case AE.eitherDecode responseBdy of
        Right res -> do
          return $ Just res
        Left err -> do
          return Nothing


data SubUrls = SubUrls
  { updatePaymentMethod :: Text
  , customerPortal :: Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SubUrls


newtype Attributes = Attributes
  { urls :: SubUrls
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] Attributes


newtype DataVals = DataVals
  { attributes :: Attributes
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] DataVals


newtype SubResponse = SubResponse
  { dataVal :: DataVals
  }
  deriving stock (Show, Generic)


instance AE.FromJSON SubResponse where
  parseJSON = AE.withObject "SubResponse" $ \obj -> do
    dataVal <- obj AE..: "data"
    return (SubResponse{dataVal = dataVal})
