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
    { projectId :: Projects.ProjectId
    , webhookUrl :: Text
    , channelId :: Text
    }
    deriving stock (Show, Generic, Eq)
    deriving anyclass (FromRow, ToRow)
    deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SlackData


insertAccessToken :: [Text] -> Text -> DBT IO Int64
insertAccessToken projects webhookUrl = executeMany q params
    where
        q =
            [sql|INSERT INTO apis.slack
               (project_id, webhook_url)
               VALUES (?,?)
               ON CONFLICT (project_id)
               DO UPDATE SET webhook_url = EXCLUDED.webhook_url |]
        params = (\p -> (p, webhookUrl)) <$> projects


getProjectSlackData :: Projects.ProjectId -> DBT IO (Maybe SlackData)
getProjectSlackData pid = queryOne Select q (Only pid)
    where
        q = [sql|SELECT project_id, webhook_url, channel_id FROM apis.slack WHERE project_id =? |]