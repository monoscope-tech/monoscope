{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Models.Apis.Slack (SlackData (..), insertAccessToken, getProjectSlackData) where

import Data.Text
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), queryOne)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Transact (DBT, executeMany)
import Deriving.Aeson qualified as AE
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Relude


data SlackData = SlackData
    { projectId :: Text
    , accessToken :: Text
    }
    deriving stock (Show, Generic, Eq)
    deriving anyclass (FromRow, ToRow)
    deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SlackData


insertAccessToken :: [Text] -> Text -> DBT IO Int64
insertAccessToken projects accessToken = executeMany q params
    where
        q =
            [sql|INSERT INTO apis.slack
               (project_id, access_token)
               VALUES (?,?)
               ON CONFLICT (project_id)
               DO UPDATE SET access_token = EXCLUDED.access_token |]
        params = (\p -> (p, accessToken)) <$> projects


getProjectSlackData :: Projects.ProjectId -> DBT IO (Maybe SlackData)
getProjectSlackData pid = queryOne Select q (Only pid)
    where
        q = [sql|SELECT project_id, access_token FROM apis.slack WHERE project_id =? |]