{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Pages.Projects.CreateProject (
  createProject,
  createProjectHandler,
  createProjectForm,
  -- getProjectHandler
  ) where

import Lucid
import Lucid.HTMX
import Relude
import Servant.HTML.Lucid
import Text.RawString.QQ
import Pages.BodyWrapper (bodyWrapper)

import Servant
import Control.Monad
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime, getZonedTime, getCurrentTimeZone)
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Digestive
import Database.PostgreSQL.Entity (Entity (fields, tableName), insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import Database.PostgreSQL.Simple (executeMany, execute_, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Transact as PgT
import qualified Data.UUID as UUID
import Data.UUID (fromText)
import Data.Aeson as AE

import Types


-- I commented this out and worked with a version of it because I inserted bits of code and yet I wanted to preserve a pure copy of the original work

-- createProject :: Html ()
-- createProject = bodyWrapper "Create Project" $ do
--   p_ [class_ "text-slate-700 text-2xl font-medium mb-5" ] "Create Project"
--   div_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl"] $ do
--     p_ [class_ "text-gray-500 mx-2 font-light text-base"] "Title"
--     input_ [class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 " ]
--     p_ [class_ "text-gray-500 mt-5 mx-2 font-light text-base"] "Description"
--     textarea_ [class_ " py-2 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ", rows_ "4", placeholder_ "Description"  ] "Description"  --Once i remove the "Description", it shows a "typecheck(-Wdeferred-type-errors)" error
--     p_ [class_ "text-gray-500 mt-5 mx-2 font-light text-sm"] "Invite a project member"
--     div_ [class_ "flex flex-row space-x-3"] $ do
--       input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "anthony@gmail.com"  ]
--       select_ [class_ "w-1/3 h-10 px-5  my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
--         option_ [class_ "text-gray-500"] "Can Edit"
--         option_ [class_ "text-gray-500"] "Can View"
--       img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
--     div_ [class_ "flex flex-row space-x-3"] $ do
--       input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "smile@yahoo.com"  ]
--       select_ [class_ "w-1/3 h-10 px-5 my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
--         option_ [class_ "text-gray-500"] "Can Edit"
--         option_ [class_ "text-gray-500"] "Can View"
--       img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
--     div_ [class_ "flex flex-row space-x-3"] $ do
--       input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "seniormanVic@gmail.com"  ]
--       select_ [class_ "w-1/3 h-10 px-5 my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
--         option_ [class_ "text-gray-500"] "Can Edit"
--         option_ [class_ "text-gray-500"] "Can View"
--       img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]   
--     div_ [class_ "flex flex-row cursor-pointer mt-2"] $ do
--       img_ [src_ "/assets/svgs/blue-plus.svg", class_ "mx-2"]   
--       span_ [class_ "text-blue-700 font-medium text-base "] "Add member"
--     a_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer" ] "Next step"



createProject :: Html ()
createProject = bodyWrapper "Create Project" $ do
  form_ [action_ "projects/create"] $ do
    p_ [class_ "text-slate-700 text-2xl font-medium mb-5" ] "Create Project"
    div_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl"] $ do
      p_ [class_ "text-gray-500 mx-2 font-light text-base"] "Title"
      input_ [class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ", name_ "cpTitle"]
      p_ [class_ "text-gray-500 mt-5 mx-2 font-light text-base"] "Description"
      textarea_ [class_ " py-2 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ", rows_ "4", placeholder_ "Description",  name_ "cpDescription"] "Description"  --Once i remove the "Description", it shows a "typecheck(-Wdeferred-type-errors)" error
      p_ [class_ "text-gray-500 mt-5 mx-2 font-light text-sm"] "Invite a project member"
      div_ [class_ "flex flex-row space-x-3"] $ do
        input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "anthony@gmail.com",  name_ "mbEmail"]
        select_ [class_ "w-1/3 h-10 px-5  my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
          option_ [class_ "text-gray-500", name_ "mbAccess"] "Can Edit"
          option_ [class_ "text-gray-500", name_ "mbAccess"] "Can View"
        img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
      div_ [class_ "flex flex-row space-x-3"] $ do
        input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "smile@yahoo.com",  name_ "mbEmail"]
        select_ [class_ "w-1/3 h-10 px-5 my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
          option_ [class_ "text-gray-500", name_ "mbAccess"] "Can Edit"
          option_ [class_ "text-gray-500", name_ "mbAccess"] "Can View"
        img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
      div_ [class_ "flex flex-row space-x-3"] $ do
        input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "seniormanVic@gmail.com",  name_ "mbEmail"]
        select_ [class_ "w-1/3 h-10 px-5 my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
          option_ [class_ "text-gray-500", name_ "mbAccess"] "Can Edit"
          option_ [class_ "text-gray-500", name_ "mbAccess"] "Can View"
        img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]   
      div_ [class_ "flex flex-row cursor-pointer mt-2"] $ do
        img_ [src_ "/assets/svgs/blue-plus.svg", class_ "mx-2"]   
        span_ [class_ "text-blue-700 font-medium text-base "] "Add member"
        input_ [type_ "submit", name_ "submit"]
      -- a_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer" ] "Next step"

-- I changed most of the types to Text because the form parser works only with text and the validate functions I designed to convert text to the appropriate types were throwing errors

createProjectForm :: Monad m => Form Text m CreateProject
createProjectForm = CreateProject
    -- <$> "cpProjectID" Text.Digestive..: validate textToUUID (text Nothing) 
    -- <*> "cpTitle" Text.Digestive..: check "cannot be empty" notNull (text Nothing)
    -- <*> "cpDescription" Text.Digestive..: check "cannot be empty" notNull (text Nothing)
    -- <*> "cpCreatedAt" Text.Digestive..: validate textToZonedTime (text Nothing)
    -- <*> "cpUpdatedAt" Text.Digestive..: validate textToZonedTime (text Nothing)
    <$> "cpProjectID" Text.Digestive..: text Nothing
    <*> "cpTitle" Text.Digestive..: check "cannot be empty" notNull (text Nothing)
    <*> "cpDescription" Text.Digestive..: check "cannot be empty" notNull (text Nothing)
    <*> "cpCreatedAt" Text.Digestive..: text Nothing
    <*> "cpUpdatedAt" Text.Digestive..: text Nothing
    -- <*> "mbEmail" Text.Digestive..: validate textToMember (text Nothing)
    <*> "mbEmail" Text.Digestive..: check "not a valid email address" checkEmail (text Nothing)
    -- <*> "mbAccess" Text.Digestive..: choice categories Nothing
    -- <*> "mbCreatedAt" Text.Digestive..: validate textToZonedTime (text Nothing)
    -- <*> "mbUpdatedAt" Text.Digestive..: validate textToZonedTime (text Nothing)


-- textToUUID :: Text -> Result Text UUID.UUID
-- textToUUID txt = maybe (Error "cannot parse UUID") Success . 
--   fromText txt

-- textToZonedTime :: Text -> Result Text ZonedTime
-- textToZonedTime = maybe (Error "cannot parse ZonedTime") Success .
--   return getZonedTime 

-- textToMember :: Text -> Result Text Member
-- textToMember = maybe (Error "cannot parse Member") Success .
--   return cpMember

notNull :: Text -> Bool
notNull txt =
  if T.null txt
    then True
  else False

checkEmail :: Text -> Bool
checkEmail = isJust . T.find (== '@')

categories = [(x, T.pack (show x)) | x <- [minBound .. maxBound]]

-- worked with postgresql-simple as seen in the doc but keeps throwing a type error

createProjectHandler :: CreateProject -> PgT.DBT IO (Maybe (Text, Text, Text)) -> Servant.Handler CreateProject
createProjectHandler project = executeMany q options
  where
    q =
      [sql|  
        INSERT INTO create_project (cpTitle, cpDescription, cpMember)
        VALUES(?, ?, ?) 
        RETURNING id, cpTitle;
      |]
    
    options =
      ( cpTitle project,
        cpDescription project,
        cpMember project
      )

getProjectHandler :: Text -> UUID.UUID -> Servant.Handler CreateProject
getProjectHandler sUUID = do
  let q =
        [sql|  
          SELECT cpTitle, cpDescription, cpMember FROM create_project where cpProjectID = uuid;
        |]
  uuid <- fromText sUUID
  maybeProject <- query q

  case maybeProject of
    Just project -> return project
    Nothing -> Servant.throwError $ err401 { errBody = AE.encode "could not find project with uuid: " ++ (show uuid) }

getAllProjectHandler :: Servant.Handler [CreateProject]
getAllProjectHandler = do
  let q =
        [sql|  
          SELECT cpTitle, cpDescription, cpMember FROM create_project;
        |]
  maybeProjects <- query q
    
  case maybeProjects of
    Just projects -> return projects
    Nothing -> Servant.throwError $ err401 { errBody = AE.encode "no project has been created" }