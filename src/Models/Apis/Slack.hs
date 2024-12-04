module Models.Apis.Slack (SlackData (..), insertAccessToken, getProjectSlackData) where

import Database.PostgreSQL.Entity.DBT (QueryNature (Select), queryOne)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT, executeMany)
import Deriving.Aeson qualified as AE
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Projects.Projects qualified as Projects
import Relude


data SlackData = SlackData
  { projectId :: Projects.ProjectId
  , webhookUrl :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, NFData)
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
    params = (,webhookUrl) <$> projects


getProjectSlackData :: DB :> es => Projects.ProjectId -> Eff es (Maybe SlackData)
getProjectSlackData pid = dbtToEff $ queryOne Select q (Only pid)
  where
    q = [sql|SELECT project_id, webhook_url FROM apis.slack WHERE project_id =? |]
