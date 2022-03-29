{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pages.Projects.CreateProject
  ( CreateProjectForm (..),
    createProjectGetH,
    createProjectPostH, 
    createProjectFormV,
    createProjectFormToModel,
    CreateProjectFormError,
  )
where

import Config
import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.Text qualified as T
import Data.Time (ZonedTime, getZonedTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Valor (Valor, check1, failIf, validateM)
import Data.Valor qualified as Valor
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import Lucid.Hyperscript
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Projects.ProjectsEmail qualified as ProjectEmail
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Optics.Core ((^.))
import Pages.BodyWrapper (bodyWrapper)
import Relude
import Servant
  ( addHeader,
    noHeader,
  )
import Web.FormUrlEncoded (FromForm)

data CreateProjectForm = CreateProjectForm
  { title :: Text,
    description :: Text,
    email :: Text,
    permission :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromForm, Default)

data CreateProjectFormError = CreateProjectFormError
  { titleE :: Maybe [String],
    descriptionE :: Maybe [String],
    emailE :: Maybe [String],
    permissionE :: Maybe [String]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

createProjectFormToModel :: Projects.ProjectId -> Text -> Text -> Projects.CreateProject
createProjectFormToModel pid tit desc = Projects.CreateProject {id = pid, title = tit, description = desc}

createProjectFormV :: Monad m => Valor CreateProjectForm m CreateProjectFormError
createProjectFormV =
  CreateProjectFormError
    <$> check1 title (failIf ["name can't be empty"] T.null)
    <*> check1 description Valor.pass
    <*> check1 email (failIf ["must be a valid email address"] checkEmail)
    <*> check1 permission (failIf ["must not be blank"] T.null)

checkEmail :: Text -> Bool
checkEmail = isJust . T.find (== '@')

-- createUserFromInvitation needed as Users.createUser does not accomodate the constraint of invited member and user having the same userid as well as active status should be false until invited member activates the user account
createUserFromInvitation :: Users.UserId -> ZonedTime -> Text -> IO Users.User
createUserFromInvitation uid tNow txt = do
  pure $
    Users.User
      { id = uid,
        createdAt = tNow,
        updatedAt = tNow,
        deletedAt = Nothing,
        active = False,
        firstName = "",
        lastName = "",
        displayImageUrl = "",
        email = CI.mk txt
      }

createProjectPostH :: Sessions.PersistentSession -> CreateProjectForm -> DashboardM (HeadersTriggerRedirect (Html ()))
createProjectPostH sess createP = do
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Left cpRaw -> do
      let cp = Valor.unValid cpRaw
      let titleField = title cp
      let descriptionField = description cp
      let permissionField = permission cp
      let perm = ProjectMembers.parsePermissions permissionField
      let emailField = email cp
      pool <- asks pool
      envKey <- asks env
      let userID = sess ^. #userId
      puid <- liftIO UUIDV4.nextRandom
      inviteID <- liftIO UUIDV4.nextRandom
      invUserID <- liftIO Users.createUserId
      tNow <- liftIO getZonedTime
      let pid = Projects.ProjectId puid
      -- Temporary. They should come from the form
      let adminPermission = ProjectMembers.parsePermissions "admin"
      let projectMembers = [ProjectMembers.CreateProjectMembers pid userID adminPermission]
      invUser <- liftIO $ createUserFromInvitation invUserID tNow emailField
      let invMember = [ProjectMembers.CreateProjectMembers pid invUserID perm]
      invUUID <- liftIO $ ProjectEmail.inviteUUID inviteID invUserID tNow
      let invEmail = ProjectEmail.sendEmail inviteID emailField
      
      _ <- liftIO $
        withPool pool $ do
          _  <- Projects.insertProject (createProjectFormToModel pid titleField descriptionField)
          _  <- ProjectMembers.insertProjectMembers projectMembers
          _  <- Users.insertUser invUser
          _  <- ProjectMembers.insertProjectMembers invMember
          _  <- ProjectEmail.insertInviteID invUUID
          pass

      _ <- liftIO $ ProjectEmail.sendInviteMail envKey invEmail

      pure $ addHeader "HX-Trigger" $ addHeader "/" $ createProjectBody cp (def @CreateProjectFormError)
    
    Right cpe -> pure $ noHeader $ noHeader $ createProjectBody createP cpe    
----------------------------------------------------------------------------------------------------------
-- createProjectGetH is the handler for the create projects page
createProjectGetH :: Sessions.PersistentSession -> DashboardM (Html ())
createProjectGetH sess = do
  pure $ bodyWrapper (Just sess) Nothing "Create Project" $ createProjectBody (def @CreateProjectForm) (def @CreateProjectFormError)

----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
-- createProjectBody is the core html view
createProjectBody :: CreateProjectForm -> CreateProjectFormError -> Html ()
createProjectBody cp cpe = do
  section_ [id_ "main-content ", class_ "p-6"] $ do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5"] "Create Project"
    form_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl", hxPost_ "/p/new"] $ do
      -- , hxTarget_ "#main-content"
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
        section_ [id_ "manage"] $ do
          template_ [id_ "invite"] $ do
            div_ [class_ "flex flex-row space-x-2"] $ do
              input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 font-light border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "anthony@gmail.com",
                type_ "text",
                id_ "email",
                name_ "email"]
              select_ [class_ "w-1/3 h-10 px-5  my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
                option_ [ class_ "text-gray-500",
                  type_ "text",
                  id_ "permission",
                  name_ "permission"
                  ] "Can Edit"
                option_ [ class_ "text-gray-500",
                  type_ "text",
                  id_ "permission",
                  name_ "permission"
                  ] "Can View"
              button_
                [ [__| 
                    remove from my parent             
                  |]
                ]
                $ do img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
        a_
          [ class_ "bg-transparent inline-flex cursor-pointer mt-2",
            [__| 
              on click append #invite.innerHTML to #manage   
              end 
            |]
          ]
          $ do
            img_ [src_ "/assets/svgs/blue-plus.svg", class_ " mt-1 mx-2 w-3 h-3"]
            span_ [class_ "text-blue-700 font-medium text-sm "] "Add member"
      button_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer", type_ "submit"] "Next step"
