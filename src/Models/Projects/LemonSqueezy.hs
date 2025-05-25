module Models.Projects.LemonSqueezy (LemonSub (..), LemonSubId (..), addSubscription, getTotalUsage, addDailyUsageReport, upgradeToPaid, downgradeToFree) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity (Entity, insert)
import Database.PostgreSQL.Entity.DBT (DBT, execute, query)
import Database.PostgreSQL.Entity.Types (CamelToSnake, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple hiding (execute, query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude
import Servant (FromHttpApiData)


newtype LemonSubId = LemonSubId {lemonSubId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, NFData, Ord, ToField)


instance HasField "toText" LemonSubId Text where
  getField = UUID.toText . lemonSubId


data LemonSub = LemonSub
  { id :: LemonSubId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Text
  , subscriptionId :: Int
  , orderId :: Int
  , firstSubId :: Int
  , productName :: Text
  , userEmail :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "subscriptions", PrimaryKey "id", FieldModifiers '[CamelToSnake]] LemonSub)


addSubscription :: LemonSub -> DBT IO ()
addSubscription = insert @LemonSub


addDailyUsageReport :: Projects.ProjectId -> Int -> DBT IO Int64
addDailyUsageReport pid total_requests = execute q (pid, total_requests)
  where
    q = [sql|INSERT INTO apis.daily_usage (project_id, total_requests) VALUES (?, ?) |]


getTotalUsage :: Projects.ProjectId -> UTCTime -> DBT IO Int64
getTotalUsage pid start = do
  [Only count] <- query q (pid, start)
  return count
  where
    q = [sql|SELECT COALESCE(SUM(total_requests), 0) FROM apis.daily_usage WHERE project_id = ? AND created_at >= ?|]


downgradeToFree :: Int -> Int -> Int -> DBT IO Int64
downgradeToFree orderId subId subItemId = execute q (show orderId, show subId, show subItemId)
  where
    q = [sql|UPDATE projects.projects SET payment_plan = 'Free' WHERE order_id = ? AND sub_id = ? AND first_sub_item_id = ?|]


upgradeToPaid :: Int -> Int -> Int -> DBT IO Int64
upgradeToPaid orderId subId subItemId = execute q (show orderId, show subId, show subItemId)
  where
    q = [sql|UPDATE projects.projects SET payment_plan = 'GraduatedPricing' WHERE order_id = ? AND sub_id = ? AND first_sub_item_id = ?|]
