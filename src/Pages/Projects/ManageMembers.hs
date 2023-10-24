module Pages.Projects.ManageMembers (
  manageMembersGetH,
  manageMembersPostH,
  ManageMembersForm (..),
) where

import BackgroundJobs qualified
import Config
import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import Data.CaseInsensitive (original)
import Data.Default (def)
import Data.List.Unique (uniq)
import Data.Pool (withResource)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import OddJobs.Job (createJob)
import Optics.Core ((^.))
import Pages.BodyWrapper
import Pages.NonMember
import Relude
import Servant
import Servant.Htmx
import Utils
import Web.FormUrlEncoded (FromForm)


data ManageMembersForm = ManageMembersForm
  { emails :: [Text]
  , permissions :: [ProjectMembers.Permissions]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm)


manageMembersPostH :: Sessions.PersistentSession -> Projects.ProjectId -> ManageMembersForm -> DashboardM (Headers '[HXTrigger] (Html ()))
manageMembersPostH sess pid form = do
  let currUserId = sess.userId
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"errorToast": ["Only project members can update members list"]}|]
      pure $ addHeader hxTriggerData $ h3_ [] "Only members of this project can perform this action"
    else do
      (project, projMembers) <- liftIO $
        withPool
          pool
          do
            project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
            projMembers <- ProjectMembers.selectActiveProjectMembers pid
            pure (project, projMembers)

      -- TODO:
      -- Separate the new emails from the old emails
      -- Insert the new emails and permissions.
      -- Update the permissions only of the existing emails.

      let usersAndPermissions = zip form.emails form.permissions & uniq
      let uAndPOldAndChanged =
            mapMaybe
              ( \(email, permission) -> do
                  let projMembersM = projMembers & find (\a -> original (a ^. #email) == email && a ^. #permission /= permission)
                  projMembersM >>= (\projMember -> Just (projMember ^. #id, permission))
              )
              usersAndPermissions

      let uAndPNew = filter (\(email, _) -> not $ any (\a -> original (a ^. #email) == email) projMembers) usersAndPermissions
      let projectTitle = maybe "" (^. #title) project

      let deletedUAndP =
            Vector.toList projMembers
              & filter (\pm -> not $ any (\(email, _) -> original (pm ^. #email) == email) usersAndPermissions)
              & filter (\a -> a ^. #userId /= currUserId)
              & map (\a -> a ^. #id) -- We should not allow deleting the current user from the project

      -- Create new users and send notifications
      newProjectMembers <- liftIO $
        forM uAndPNew \(email, permission) -> do
          userId' <- withPool pool do
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
              withResource pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.InviteUserToProject userId' pid email projectTitle -- invite the users to the project (Usually as an email)
          pure (email, permission, userId')

      let projectMembers =
            newProjectMembers
              & filter (\(_, _, id') -> id' /= currUserId)
              & map (\(email, permission, id') -> ProjectMembers.CreateProjectMembers pid id' permission)
      _ <- liftIO $ withPool pool $ ProjectMembers.insertProjectMembers projectMembers -- insert new project members

      -- Update existing contacts with updated permissions
      -- TODO: Send a notification via background job, about the users permission having been updated.
      unless (null uAndPOldAndChanged) $
        void $
          liftIO $
            withPool pool $
              ProjectMembers.updateProjectMembersPermissons uAndPOldAndChanged

      -- soft delete project members with id
      unless (null deletedUAndP) $
        void $
          liftIO $
            withPool pool $
              ProjectMembers.softDeleteProjectMembers deletedUAndP

      projMembersLatest <- liftIO $ withPool pool $ ProjectMembers.selectActiveProjectMembers pid
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Updated Members List Successfully"]}|]
      pure $ addHeader hxTriggerData $ manageMembersBody projMembersLatest


manageMembersGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
manageMembersGetH sess pid = do
  pool' <- asks pool
  isMember <- liftIO $ withPool pool' $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, projMembers) <- liftIO $
        withPool
          pool'
          do
            project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
            projMembers <- ProjectMembers.selectActiveProjectMembers pid
            pure (project, projMembers)
      let bwconf = (def :: BWConfig){sessM = Just sess, pageTitle = "Settings", currProject = project}
      pure $ bodyWrapper bwconf $ manageMembersBody projMembers


manageMembersBody :: Vector ProjectMembers.ProjectMemberVM -> Html ()
manageMembersBody projMembers =
  section_ [id_ "main-content", class_ "p-6"] do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5"] "Manage project members"
    form_
      [ class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl"
      , hxPost_ ""
      , hxTarget_ "#main-content"
      , hxSwap_ "outerHTML"
      ]
      do
        div_ [class_ "mt-6"] do
          section_ [id_ "inviteMemberSection"] do
            mapM_ (projectMemberRow . Just) projMembers
            template_ [id_ "inviteTmpl"] $ projectMemberRow Nothing
          a_
            [ class_ "bg-transparent inline-flex cursor-pointer mt-2"
            , [__| on click append #inviteTmpl.innerHTML to #inviteMemberSection then 
                          _hyperscript.processNode(#inviteMemberSection) then halt |]
            ]
            do
              faIcon_ "fa-plus" "fa-sharp fa-regular fa-plus" "mt-1 mx-2 w-3 h-3 text-blue-700"
              span_ [class_ "text-blue-700 font-medium text-sm "] "Add member"
        button_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer", type_ "submit"] "Submit"


projectMemberRow :: Maybe ProjectMembers.ProjectMemberVM -> Html ()
projectMemberRow projMembersM =
  div_ [class_ "flex flex-row space-x-2"] do
    input_
      [ name_ "emails"
      , class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 font-light border-solid border border-gray-200 rounded-2xl border-0 "
      , placeholder_ "name@example.com"
      , value_ (maybe "" (original . (^. #email)) projMembersM)
      ]
    let permission = maybe ProjectMembers.PView (^. #permission) projMembersM
    select_ [name_ "permissions", class_ "w-1/3 h-10 px-5  my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] do
      option_ ([class_ "text-gray-500", value_ "admin"] <> selectedIf ProjectMembers.PAdmin permission) "Admin"
      option_ ([class_ "text-gray-500", value_ "edit"] <> selectedIf ProjectMembers.PEdit permission) "Can Edit"
      option_ ([class_ "text-gray-500", value_ "view"] <> selectedIf ProjectMembers.PView permission) "Can View"
    button_
      [ [__| on click remove the closest parent <div/> then halt |]
      ]
      $ faIcon_ "fa-trash-can-plus" "fa-regular fa-trash-can-plus" "w-3 h-3 text-red-700"
  where
    selectedIf :: ProjectMembers.Permissions -> ProjectMembers.Permissions -> [Attribute]
    selectedIf a b = [selected_ "" | a == b]
