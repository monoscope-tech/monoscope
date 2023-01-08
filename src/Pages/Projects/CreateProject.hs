{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pages.Projects.CreateProject
  ( CreateProjectForm,
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
import Config
import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import Data.CaseInsensitive (original)
import Data.Default
import Data.List.Extra (cons)
import Data.List.Unique
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUIDV4
import Data.Valor (Valor, check1, failIf, validateM)
import Data.Valor qualified as Valor
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMembers qualified as Projects
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude
import Servant
  ( Headers,
    addHeader,
    noHeader,
  )
import Servant.Htmx
import Web.FormUrlEncoded (FromForm)

data CreateProjectForm = CreateProjectForm
  { title :: Text,
    description :: Text,
    emails :: [Text],
    permissions :: [ProjectMembers.Permissions],
    isUpdate :: Bool,
    projectId :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm, Default)

data CreateProjectFormError = CreateProjectFormError
  { titleE :: Maybe [String],
    descriptionE :: Maybe [String]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

createProjectFormToModel :: Projects.ProjectId -> CreateProjectForm -> Projects.CreateProject
createProjectFormToModel pid CreateProjectForm {..} = Projects.CreateProject {id = pid, ..}

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
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            pageTitle = "Endpoints"
          }
  pure $ bodyWrapper bwconf $ createProjectBody False (def @CreateProjectForm) (def @CreateProjectFormError)

----------------------------------------------------------------------------------------------------------
projectSettingsGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
projectSettingsGetH sess pid = do
  pool <- asks pool
  Just proj <- liftIO $ withPool pool $ Projects.selectProjectForUser (sess.userId, pid)
  let createProj =
        CreateProjectForm
          { title = proj.title,
            description = proj.description,
            emails = [],
            permissions = [],
            isUpdate = True,
            projectId = Projects.projectIdText pid
          }
  let bwconf = (def :: BWConfig) {sessM = Just sess, currProject = Just proj, pageTitle = "Settings"}
  pure $ bodyWrapper bwconf $ createProjectBody True createProj (def @CreateProjectFormError)

----------------------------------------------------------------------------------------------------------
deleteProjectGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
deleteProjectGetH sess pid = do
  pool <- asks pool
  _ <- liftIO $ withPool pool $ Projects.deleteProject pid

  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Created Project Successfully"]}|]
  pure $ addHeader hxTriggerData $ addHeader "/" $ span_ ""

----------------------------------------------------------------------------------------------------------
-- createProjectPostH is the handler for the create projects page form handling.
-- It processes post requests and is expected to return a redirect header and a hyperscript event trigger header.
createProjectPostH :: Sessions.PersistentSession -> CreateProjectForm -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
createProjectPostH sess createP = do
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Right cpe -> pure $ noHeader $ noHeader $ createProjectBody createP.isUpdate createP cpe
    Left cp -> processProjectPostForm sess cp

processProjectPostForm :: Sessions.PersistentSession -> Valor.Valid CreateProjectForm -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
processProjectPostForm sess cpRaw = do
  let cp = Valor.unValid cpRaw
  let currUserId = sess.userId
  pool <- asks pool
  pid <- liftIO $ maybe (Projects.ProjectId <$> UUIDV4.nextRandom) pure $ Projects.projectIdFromText cp.projectId
  _ <-
    if cp.isUpdate
      then do
        _ <- liftIO $ withPool pool $ Projects.updateProject (createProjectFormToModel pid cp)
        pass
      else do
        let usersAndPermissions = zip (cp.emails) (cp.permissions) & uniq
        liftIO $ withPool pool $ do
          Projects.insertProject (createProjectFormToModel pid cp)
          newProjectMembers <-
            usersAndPermissions & mapM \(email, permission) -> do
              userId' <- do
                userIdM' <- Users.userIdByEmail email
                case userIdM' of
                  Nothing -> do
                    idM' <- Users.createEmptyUser email -- NEXT Trigger email sending
                    case idM' of
                      Nothing -> error "duplicate email in createEmptyUser"
                      Just idX -> pure idX
                  Just idX -> pure idX
              when (userId' /= currUserId) $ -- invite the users to the project (Usually as an email)
                void $
                  liftIO $
                    withResource pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.InviteUserToProject userId' pid email (cp.title)
              pure (email, permission, userId')

          let projectMembers =
                newProjectMembers
                  & filter (\(_, _, id') -> id' /= currUserId)
                  & map (\(email, permission, id') -> ProjectMembers.CreateProjectMembers pid id' permission)
                  & cons (ProjectMembers.CreateProjectMembers pid currUserId Projects.PAdmin)
          ProjectMembers.insertProjectMembers projectMembers
        liftIO $ withResource pool \conn ->
          createJob conn "background_jobs" $ BackgroundJobs.CreatedProjectSuccessfully currUserId pid (original $ sess.user.getUser.email) (cp.title)
        pass

  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Created Project Successfully"]}|]
  let bdy = createProjectBody cp.isUpdate cp (def @CreateProjectFormError)
  if cp.isUpdate
    then pure $ addHeader hxTriggerData $ noHeader bdy
    else pure $ addHeader hxTriggerData $ addHeader ("/p/" <> Projects.projectIdText pid) bdy

----------------------------------------------------------------------------------------------------------
-- createProjectBody is the core html view
createProjectBody :: Bool -> CreateProjectForm -> CreateProjectFormError -> Html ()
createProjectBody isUpdate cp cpe =
  section_ [id_ "main-content", class_ "p-6"] $ do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5"] $ toHtml @String $ if isUpdate then "Project Settings" else "Create Project"
    div_ [class_ "grid grid-cols-2 gap-5"] do
      form_ [class_ "col-span-1 relative px-10 border border-gray-200 py-10  bg-white rounded-3xl", hxPost_ "/p/new", hxTarget_ "#main-content", hxSwap_ "outerHTML"] $ do
        input_ [name_ "isUpdate", type_ "hidden", value_ $ if isUpdate then "true" else "false"]
        input_ [name_ "projectId", type_ "hidden", value_ $ cp.projectId]
        div_ $ do
          label_ [class_ "text-gray-400 mx-2 font-light text-sm"] "Title"
          input_
            [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl ",
              type_ "text",
              id_ "title",
              name_ "title",
              value_ cp.title
            ]
        div_ [class_ "mt-5 "] $ do
          label_ [class_ "text-gray-400 mx-2  font-light text-sm"] "Description"
          textarea_
            [ class_ " py-2 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-1 ",
              rows_ "4",
              placeholder_ "Description",
              id_ "description",
              name_ "description"
            ]
            $ toHtml cp.description
        div_ [class_ $ "mt-6 " <> if isUpdate then "hidden" else ""] $ do
          p_ [class_ "text-gray-400 mx-2 font-light text-sm"] "Invite a project member"
          section_ [id_ "inviteMemberSection"] $ do
            template_ [id_ "inviteTmpl"] $ do
              div_ [class_ "flex flex-row space-x-2"] $ do
                input_ [name_ "emails", class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 font-light border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "name@example.com"]
                select_ [name_ "permissions", class_ "w-1/3 h-10 px-5  my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
                  option_ [class_ "text-gray-500", value_ "edit"] "Can Edit"
                  option_ [class_ "text-gray-500", value_ "view"] "Can View"
                button_
                  [ [__| on click remove the closest parent <div/> then halt |]
                  ]
                  $ img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
          a_
            [ class_ "bg-transparent inline-flex cursor-pointer mt-2",
              [__| on click append #inviteTmpl.innerHTML to #inviteMemberSection then 
                        _hyperscript.processNode(#inviteMemberSection) then halt |]
            ]
            $ do
              img_ [src_ "/assets/svgs/blue-plus.svg", class_ " mt-1 mx-2 w-3 h-3"]
              span_ [class_ "text-blue-700 font-medium text-sm "] "Add member"
        div_ [class_ "p-5 text-right"] do
          button_ [class_ "inline-block py-2 px-5 bg-blue-700  text-[white] text-sm rounded-xl cursor-pointer", type_ "submit"] "Submit"

      div_ [class_ "col-span-1 h-full justify-center items-center w-full text-center pt-24"] do
        h2_ [class_ "text-red-800 font-medium pb-4"] "Delete project. This is dangerous and unreversable"
        let pid = cp.projectId
        button_
          [ class_ "btn btn-sm bg-red-800 text-white shadow-md hover:bg-red-700 cursor-pointer rounded-md",
            hxGet_ [text|/p/$pid/delete|],
            hxConfirm_ "Are you sure you want to delete this project?"
          ]
          "Delete Project"
