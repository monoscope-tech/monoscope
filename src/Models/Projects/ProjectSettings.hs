{-# LANGUAGE DuplicateRecordFields #-}

module Models.Projects.ProjectSettings
  (
  )
where

import Config
  ( AuthContext (pool),
    DashboardM,
    HeadersTriggerRedirect,
  )
import Data.Default (Default (def))
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.Valor (Valid, Valor, check1, failIf, validateM)
import qualified Data.Valor as Valor
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity (delete)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne, query_, withPool)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Transact as PgT
import GHC.Generics (Generic)
import Lucid
  ( Html,
    button_,
    class_,
    div_,
    form_,
    h2_,
    id_,
    img_,
    input_,
    label_,
    name_,
    option_,
    p_,
    placeholder_,
    rows_,
    section_,
    select_,
    span_,
    src_,
    textarea_,
    type_,
  )
import Lucid.HTMX (hxPost_, hxTarget_)
import Models.Projects.ProjectMembers ()
import Models.Projects.Projects (Project, ProjectId)
import qualified Models.Projects.Projects as Projects
import Optics.Operators ()
import Optics.TH ()
import Pages.Projects.CreateProject
  ( CreateProjectForm,
    CreateProjectFormError,
    createProjectFormV,
  )
import Relude
import Servant
  ( Handler,
    addHeader,
    noHeader,
  )
import Servant.HTML.Lucid ()
import Web.FormUrlEncoded (FromForm)

editProjectBody :: CreateProjectForm -> CreateProjectFormError -> Html ()
editProjectBody cp cpe = do
  section_ [id_ "main-content"] $ do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5"] "Edit Project"
    form_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl", hxPost_ "/p/edit", hxTarget_ "#main-content"] $ do
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
      button_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer", type_ "submit"] "Next step"

editProjectMembersBody :: CreateProjectForm -> CreateProjectFormError -> Html ()
editProjectMembersBody cp cpe = do
  section_ [id_ "main-content"] $ do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5"] "Edit Project Members"
    form_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl", hxPost_ "/p/edit-members", hxTarget_ "#main-content"] $ do
      div_ $ do
        p_ [class_ "text-gray-500 mt-5 mx-2 font-light text-sm"] "Update Member Permission"
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

editProjectMemberH :: ProjectId -> CreateProjectForm -> DashboardM (HeadersTriggerRedirect (Html ()))
editProjectMemberH pid editP = do
  validationRes <- validateM createProjectFormV editP
  case validationRes of
    Left epRaw -> do
      let ep = Valor.unValid epRaw
      pool <- asks pool
      _ <- liftIO $
        withPool pool $ do
          -- updateProject (createProjectFormToModel pid ep)
          pass

      pure $ addHeader "HX-Trigger" $ addHeader "/p" $ editProjectMembersBody ep (def @CreateProjectFormError)
    Right epe -> pure $ noHeader $ noHeader $ editProjectMembersBody editP epe

editProjectPostH :: ProjectId -> CreateProjectForm -> DashboardM (HeadersTriggerRedirect (Html ()))
editProjectPostH pid editP = do
  validationRes <- validateM createProjectFormV editP
  case validationRes of
    Left epRaw -> do
      let ep = Valor.unValid epRaw
      pool <- asks pool
      _ <- liftIO $
        withPool pool $ do
          -- updateProject (createProjectFormToModel pid ep)
          pass

      pure $ addHeader "HX-Trigger" $ addHeader "/p" $ editProjectBody ep (def @CreateProjectFormError)
    Right epe -> pure $ noHeader $ noHeader $ editProjectBody editP epe

editProjectGetH :: ProjectId -> PgT.DBT IO (Vector.Vector Project)
editProjectGetH pid = query Select q (Only pid)
  where
    q =
      [sql|
        SELECT pp*, ppm* FROM projects.projects AS pp 
            INNER JOIN projects.project_members AS ppm
            ON pp.id = pid 
        WHERE ppm.project_id = pp.id;|]

updateProject :: ProjectId -> PgT.DBT IO Int64
updateProject pid = PgT.execute q (Only pid)
  where
    q =
      [sql|
      UPDATE projects.projects(title, description) VALUES (?, ?)
      WHERE projects.projects.id = pid;|]

deleteProject :: ProjectId -> PgT.DBT IO ()
deleteProject pid = delete @Project (Only pid)

updateMemberPermission :: UUID.UUID -> PgT.DBT IO Int64
updateMemberPermission mid = PgT.execute q (Only mid)
  where
    q =
      [sql|
    UPDATE projects.project_members (permission) VALUES (?)
    WHERE projects.project_members.id = mid;|]

-- deleteMember :: ProjectMembers -> PgT.DBT IO ()
-- deleteMember mid = delete @ProjectMembers (Only mid)
