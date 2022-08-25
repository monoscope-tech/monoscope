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
import OddJobs.Job (createJob)
import Optics.Core ((^.))
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
    permissions :: [ProjectMembers.Permissions]
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
  pure $ bodyWrapper bwconf $ createProjectBody (def @CreateProjectForm) (def @CreateProjectFormError)

----------------------------------------------------------------------------------------------------------
-- createProjectPostH is the handler for the create projects page form handling.
-- It processes post requests and is expected to return a redirect header and a hyperscript event trigger header.
createProjectPostH :: Sessions.PersistentSession -> CreateProjectForm -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
createProjectPostH sess createP = do
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Right cpe -> pure $ noHeader $ noHeader $ createProjectBody createP cpe
    Left cp -> processProjectPostForm sess cp

processProjectPostForm :: Sessions.PersistentSession -> Valor.Valid CreateProjectForm -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
processProjectPostForm sess cpRaw = do
  let cp = Valor.unValid cpRaw
  let currUserId = sess.userId
  pid <- Projects.ProjectId <$> liftIO UUIDV4.nextRandom
  let usersAndPermissions = zip (cp.emails) (cp.permissions) & uniq
  pool <- asks pool

  newProjectMembers <- liftIO $
    forM usersAndPermissions \(email, permission) -> do
      userId' <- withPool pool $ do
        userIdM' <- Users.userIdByEmail email
        case userIdM' of
          Nothing -> do
            idM' <- Users.createEmptyUser email -- NEXT Trigger email sending
            case idM' of
              Nothing -> error "duplicate email in createEmptyUser"
              Just idX -> pure idX
          Just idX -> pure idX

      when (userId' /= currUserId) $ -- invite the users to the project (Usually as an email)
        void $ withResource pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.InviteUserToProject userId' pid email (cp.title)

      pure (email, permission, userId')

  let projectMembers =
        newProjectMembers & filter (\(_, _, id') -> id' /= currUserId)
          & map (\(email, permission, id') -> ProjectMembers.CreateProjectMembers pid id' permission)
          & cons (ProjectMembers.CreateProjectMembers pid currUserId Projects.PAdmin)

  _ <- liftIO $ do
    _ <- withPool pool $ do
      Projects.insertProject (createProjectFormToModel pid cp)
      ProjectMembers.insertProjectMembers projectMembers

    withResource pool \conn ->
      createJob conn "background_jobs" $
        BackgroundJobs.CreatedProjectSuccessfully currUserId pid (original $ sess.user.getUser.email) (cp.title)

  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Created Project Successfully"]}|]
  pure $ addHeader hxTriggerData $ addHeader ("/p/" <> Projects.projectIdText pid) $ createProjectBody cp (def @CreateProjectFormError)

----------------------------------------------------------------------------------------------------------
-- createProjectBody is the core html view
createProjectBody :: CreateProjectForm -> CreateProjectFormError -> Html ()
createProjectBody cp cpe =
  section_ [id_ "main-content ", class_ "p-6"] $ do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5"] "Create Project"
    form_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl", hxPost_ "/p/new"] $ do
      div_ $ do
        label_ [class_ "text-gray-400 mx-2 font-light text-sm"] "Title"
        input_
          [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl ",
            type_ "text",
            id_ "title",
            name_ "title"
          ]
      div_ [class_ "mt-5 "] $ do
        label_ [class_ "text-gray-400 mx-2  font-light text-sm"] "Description"
        textarea_
          [ class_ " py-2 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ",
            rows_ "4",
            placeholder_ "Description",
            id_ "description",
            name_ "description"
          ]
          ""
      div_ [class_ "mt-6"] $ do
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
      button_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer", type_ "submit"] "Next step"
