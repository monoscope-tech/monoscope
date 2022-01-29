{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pages.Projects.CreateProject
  ( CreateProjectForm,
    createProjectGetH,
    createProjectPostH,
  )
where

import Config
import Control.Concurrent (forkIO)
import Data.Default
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Data.Valor (Valid, Valor, check1, failIf, validateM)
import qualified Data.Valor as Valor
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import qualified Models.Projects.ProjectMembers as ProjectMembers
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Users as Users
import Pages.BodyWrapper (bodyWrapper)
import Relude
import Servant
  ( Handler,
    addHeader,
    noHeader,
  )
import Servant.HTML.Lucid
import Text.RawString.QQ
import Web.FormUrlEncoded (FromForm)

data CreateProjectForm = CreateProjectForm
  { title :: Text,
    description :: Text
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromForm, Default)

data CreateProjectFormError = CreateProjectFormError
  { titleE :: Maybe [String],
    descriptionE :: Maybe [String]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Default)

createProjectFormToModel :: Projects.ProjectId -> CreateProjectForm -> Projects.CreateProject
createProjectFormToModel pid CreateProjectForm {..} = Projects.CreateProject {id = pid, ..}

createProjectFormV :: Monad m => Valor CreateProjectForm m CreateProjectFormError
createProjectFormV =
  CreateProjectFormError
    <$> check1 title (failIf ["name can't be empty"] T.null)
    <*> check1 description Valor.pass

----------------------------------------------------------------------------------------------------------
-- createProjectGetH is the handler for the create projects page
createProjectGetH :: DashboardM (Html ())
createProjectGetH = do
  pure $ bodyWrapper "Create Project" $ createProjectBody (def @CreateProjectForm) (def @CreateProjectFormError)

----------------------------------------------------------------------------------------------------------
-- createProjectPostH is the handler for the create projects page form handling.
-- It processes post requests and is expected to return a redirect header and a hyperscript event trigger header.
createProjectPostH :: CreateProjectForm -> DashboardM (HeadersTriggerRedirect (Html ()))
createProjectPostH createP = do
  validationRes <- validateM createProjectFormV createP
  case validationRes of
    Left cpRaw -> do
      let cp = Valor.unValid cpRaw
      pool <- asks pool
      let userID = Users.UserId UUID.nil
      puid <- liftIO $ UUIDV4.nextRandom
      let pid = Projects.ProjectId puid
      -- Temporary. They should come from the form
      let projectMembers = [(ProjectMembers.CreateProjectMembers pid userID ProjectMembers.PAdmin)]

      _ <- liftIO $
        withPool pool $ do
          Projects.insertProject (createProjectFormToModel pid cp)
          ProjectMembers.insertProjectMembers projectMembers
          pure ()

      pure $ addHeader "HX-Trigger" $ addHeader "/p" $ createProjectBody cp (def @CreateProjectFormError)
    Right cpe -> pure $ noHeader $ noHeader $ createProjectBody createP cpe

----------------------------------------------------------------------------------------------------------
-- createProjectBody is the core html view
createProjectBody :: CreateProjectForm -> CreateProjectFormError -> Html ()
createProjectBody cp cpe = do
  section_ [id_ "main-content"] $ do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5"] "Create Project"
    form_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl", hxPost_ "/p/new", hxTarget_ "#main-content"] $ do
      div_ $ do
        label_ [class_ "text-gray-500 mx-2 font-light text-base"] "Title"
        input_
          [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ",
            type_ "text",
            id_ "title",
            name_ "title"
          ]
      div_ $ do
        label_ [class_ "text-gray-500 mt-5 mx-2 font-light text-base"] "Description"
        textarea_
          [ class_ " py-2 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ",
            rows_ "4",
            placeholder_ "Description",
            id_ "description",
            name_ "description"
          ]
          ""
      div_ $ do
        p_ [class_ "text-gray-500 mt-5 mx-2 font-light text-sm"] "Invite a project member"
        section_ [id_ "manage_project_members"] $ do
          div_ [class_ "flex flex-row space-x-3"] $ do
            input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "anthony@gmail.com"]
            select_ [class_ "w-1/3 h-10 px-5  my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
              option_ [class_ "text-gray-500"] "Can Edit"
              option_ [class_ "text-gray-500"] "Can View"
            button_ [] $ img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
        div_ [class_ "flex flex-row cursor-pointer mt-2"] $ do
          img_ [src_ "/assets/svgs/blue-plus.svg", class_ "mx-2"]
          span_ [class_ "text-blue-700 font-medium text-base "] "Add member"
      button_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer", type_ "submit"] "Next step"
