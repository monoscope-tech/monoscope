{-# LANGUAGE NamedFieldPuns #-}

module Pages.Projects.ProjectSettings
  (
  )
where

import Config
  ( AuthContext (pool),
    DashboardM,
    HeadersTriggerRedirect,
  )
import Data.Default (Default (def))
import Data.Valor (validateM)
import qualified Data.Valor as Valor
import qualified Data.UUID as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX (hxPost_, hxTarget_)
import qualified Models.Projects.ProjectMembers as ProjectMembers
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import Optics.Operators ()
import Optics.TH ()
import Pages.BodyWrapper (bodyWrapper)
import qualified Pages.Projects.CreateProject as CreateProject
import qualified Models.Users.Users as Users
import Relude
import Servant
  ( addHeader,
    noHeader,
  )

editProjectH :: Sessions.PersistentSession -> DashboardM (Html ())
editProjectH sess = do
  pure $ bodyWrapper (Just sess) Nothing "Project Settings" $ editProjectBody (def @CreateProject.CreateProjectForm) (def @CreateProject.CreateProjectFormError)

editProjectBody :: CreateProject.CreateProjectForm -> CreateProject.CreateProjectFormError -> Html ()
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

editProjectMembersBody :: CreateProject.InviteProjectMemberForm -> CreateProject.InviteProjectMemberFormError -> Html ()
editProjectMembersBody mb epe = do
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

editProjectMemberH :: Projects.ProjectId -> Users.UserId -> UUID.UUID -> CreateProject.InviteProjectMemberForm -> DashboardM (HeadersTriggerRedirect (Html ()))
editProjectMemberH pid uid mid CreateProject.InviteProjectMemberForm { permission } = do
  validationRes <- validateM CreateProject.inviteProjectMemberFormV CreateProject.InviteProjectMemberForm { permission }
  case validationRes of
    Left epRaw -> do
      let ep = Valor.unValid epRaw
      pool <- asks pool
      let memberPermission = ProjectMembers.updateMemberPermissionFormToModel pid uid permission
      _ <- liftIO $
        withPool pool $ do
          _ <- ProjectMembers.updateMemberPermission pid mid memberPermission
          pass

      pure $ addHeader "HX-Trigger" $ addHeader "/p" $ editProjectMembersBody ep (def @CreateProject.InviteProjectMemberFormError)

    Right epe -> pure $ noHeader $ noHeader $ editProjectMembersBody CreateProject.InviteProjectMemberForm { permission } epe
   

-- this can be deleted depending on how the mergings are done
editProjectPostH :: Projects.ProjectId -> CreateProject.CreateProjectForm -> DashboardM (HeadersTriggerRedirect (Html ()))
editProjectPostH pid editP = do
  validationRes <- validateM CreateProject.createProjectFormV editP
  case validationRes of
    Left epRaw -> do
      let ep = Valor.unValid epRaw
      pool <- asks pool
      _ <- liftIO $
        withPool pool $ do
          _ <- Projects.updateProject $ CreateProject.createProjectFormToModel pid editP
          pass

      pure $ addHeader "HX-Trigger" $ addHeader "/p" $ editProjectBody ep (def @CreateProject.CreateProjectFormError)
    Right epe -> pure $ noHeader $ noHeader $ editProjectBody editP epe
