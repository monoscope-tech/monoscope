{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.Projects.ProjectSettings ( 
    EditProject
     ) 
where

import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Data.Default
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Entity.Types as PET
import GHC.Generics (Generic)
import qualified Models.Projects.Projects as Projects
import qualified Database.PostgreSQL.Transact as PgT
import Database.PostgreSQL.Entity
import Relude
import Data.Valor (Valid, Valor, check1, failIf, validateM)
import qualified Data.Valor as Valor
import qualified Data.Vector as Vector
import Servant.HTML.Lucid
import qualified Data.Text as T
import Lucid
import Optics.Operators
import Optics.TH
import Lucid.HTMX
import Config
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Web.FormUrlEncoded (FromForm)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool, query)
import Servant
  ( Handler,
    addHeader,
    noHeader,
  )


import Pages.Projects.CreateProject
import Models.Projects.Projects


data EditProject = EditProject
  { id :: ProjectId
  , title :: Text
  , description :: Text
  , hosts :: Text
  , active :: Bool
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "projects", PET.TableName "projects", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] EditProject)

makeFieldLabelsNoPrefix ''EditProject

editProjectBody :: EditProjectForm -> EditProjectFormError -> Html ()
editProjectBody cp cpe = do
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

-- deactivation instead of total delete, hence the active parameter
data EditProjectForm = EditProjectForm
  { edTitle :: Text
  , edDescription :: Text
  , edHosts :: Text
  , edActive :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromForm)

data EditProjectFormError = EditProjectFormError
  { titleE :: Maybe [String]
  , descriptionE :: Maybe [String]
  , hostsE :: Maybe [String]
  , active :: Maybe [Bool]
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (Default)

editProjectFormToModel :: Projects.ProjectId -> EditProjectForm -> EditProject
editProjectFormToModel pid EditProjectForm {..} = EditProject {id = pid, ..} 

editProjectFormV :: Monad m => Valor EditProjectForm m EditProjectFormError
editProjectFormV =
  EditProjectFormError
    <$> check1 edTitle (failIf ["name can't be empty"] T.null)
    <*> check1 edDescription Valor.pass
    <*> check1 edHosts Valor.pass
    <*> check1 edActive Valor.pass
    

editProjectPostH :: UUID.UUID -> EditProjectForm -> DashboardM (HeadersTriggerRedirect (Html ()))
editProjectPostH pid editP = do
    validationRes <- validateM editProjectFormV editP
    case validationRes of 
        Left epRaw -> do
            let ep = Valor.unValid epRaw
            pool <- asks pool
            _ <- liftIO $
                withPool pool $ do
                    -- updateProject (editProjectFormToModel pid ep)
                    pure ()

            pure $ addHeader "HX-Trigger" $ addHeader "/p" $ editProjectBody ep (def @EditProjectFormError)
        Right epe -> pure $ noHeader $ noHeader $ editProjectBody editP epe


-- only admins have access to edit projects
editProjectH :: UUID.UUID -> UUID.UUID -> PgT.DBT IO (Vector.Vector EditProject)
editProjectH pUUID mUUID = query Select q (Only pUUID)
    where q = [sql|
        SELECT pp.title, pp.description pp.hosts, pp.active FROM projects.projects AS pp 
            JOIN projects.project_members AS ppm
            ON ppm.id = mUUID
        WHERE pp.id = pUUID AND ppm.permission = "admin"|]
    

-- updateProject :: EditProjectForm -> PgT.DBT IO ()
-- updateProject edp = insert @Project edp
