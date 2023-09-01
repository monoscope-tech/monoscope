{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Pages.Projects.Survey (surveyPutH, SurveyForm) where

import Config
import Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import Data.Aeson.QQ (aesonQQ)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)

import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions

import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute, withPool)
import Relude

import Web.FormUrlEncoded (FromForm)

data SurveyForm = SurveyForm
  { stack :: Text
  , functionality :: [Text]
  , dataLocation :: Text
  , foundUsFrom :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, ToJSON, FromJSON)

surveyPutH :: Sessions.PersistentSession -> Projects.ProjectId -> SurveyForm -> DashboardM (Headers '[HXTrigger] (Html ()))
surveyPutH sess pid survey = do
  pool <- asks pool
  env <- asks env
  let jsonBytes = encode survey
  print survey
  print jsonBytes
  res <- liftIO $ withPool pool $ execute Update [sql| update projects.projects set questions= ? where id=? |] (jsonBytes, pid)
  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "","successToast": ["Thanks for taking the survey"]}|]
  pure $ addHeader hxTriggerData ""
