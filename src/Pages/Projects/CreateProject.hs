{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.Projects.CreateProject (
  createProject,
  connString,
  createProjectHandler,
  getProjectHandler
  ) where

import Lucid
import Lucid.HTMX
import Relude
import Servant.HTML.Lucid
import Text.RawString.QQ
import Pages.BodyWrapper (bodyWrapper)

import Data.UUID as UUID
import Data.Text (Text)
import qualified Data.Text as T
import Web.Forma
import Servant
import Database.PostgreSQL.Simple (Connection)

import Types (ProjectDummy)

createProject :: Html ()
createProject = bodyWrapper "Create Project" $ do
  p_ [class_ "text-[#3F434A] text-xl font-normal" ] "Create Project"
  div_ [class_ "p-10 bg-[#000000] text-[#ffffff] h-40 w-40 outline outline-offset-2 outline-blue-500"] "Made"
  

-- My next push will reflect the correct library for persisting data
-- I want to use a haskell library called forma to parse and validate the info from create project forms
-- I've not installed or added any of them to the dependencies. Since haskell is still new to me. I'll get your feedbacks before doing so

type CreateProjectFields = '["id", "projectOwner", "projectDescription"]

createProjectForm :: Monad m => FormParser CreateProjectFields Text m ProjectDummy
createProjectForm =
  ProjectDummy
    <$> field #id notEmpty
    <*> field #projectOwner notEmpty
    <*> field #projectDescription notEmpty

    where
      notEmpty :: Monad m => Text -> ExceptT Text m Text
      notEmpty txt = 
        if T.null txt
          then throwError "This field cannot be empty."
          else return txt


-- env is actually nothing. Just did that to represent me getting a library to get info from env file

connString :: Connection
connString = env.DATABASE_URL

createProjectHF :: Connection -> ProjectDummy -> IO UUID
createProjectHF connString createProjectForm = fromSqlKey <$> runAction connString (insert createProjectForm)

createProjectHandler :: Connection -> ProjectDummy -> Handler UUID
createProjectHandler connString  createProjectForm = lift $ createProjectHF connString createProjectForm

runAction :: Connection -> SqlPersist a -> IO a
runAction connection action = runStdoutLoggingT $ withPOstgresqlConn connection $ \backend -> runReaderT action backend

fetchProjectHF :: Connection -> UUID -> IO (Maybe ProjectDummy)
fetchProjectHF connString uuid = runAction connString (get (toSqlKey uuid))

getProjectHandler :: Connection -> UUIID -> Handler ProjectDummy
getProjectHandler connString uuid = do
  maybeProject <- lift $ fetchProjectHF connString uuid
  case maybeProject of
    Just project -> return project
    Nothing -> Handler $ (throwE $ err401 { errBody = "could not find project with uuid: " ++ (show uuid) })




  
