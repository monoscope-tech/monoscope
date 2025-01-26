module Models.Projects.Dashboards (Dashboard (..), DashboardVM (..), DashboardId (..), readDashboardsFromDirectory) where

import Control.Exception (try)
import Data.Aeson qualified as AE
import Data.ByteString qualified as B
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.List (isSuffixOf)
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Data.Yaml qualified as Yml
import Database.PostgreSQL.Entity qualified as DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import GHC.Records (HasField (getField))
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax qualified as THS
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget (Widget (..))
import Relude
import Servant (FromHttpApiData)
import System.Directory (listDirectory)


data DashboardVM = DashboardVM
  { id :: DashboardId
  , projectId :: Projects.ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , createdBy :: Users.UserId
  , baseTemplate :: Maybe Text
  , schema :: Maybe Dashboard
  , starredSince :: Maybe UTCTime
  , homepageSince :: Maybe UTCTime
  , tags :: V.Vector Text
  , title :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (DBT.Entity)
    via (GenericEntity '[Schema "projects", TableName "dashboards", PrimaryKey "id", FieldModifiers '[CamelToSnake]] DashboardVM)


newtype DashboardId = DashboardId {unDashboardId :: UUID.UUID}
  deriving stock (Generic, Show, Read)
  deriving newtype (Eq, Ord, AE.ToJSON, AE.FromJSON, FromField, ToField, Default, Hashable, NFData, FromHttpApiData)
  deriving anyclass (FromRow, ToRow)


instance HasField "unwrap" DashboardId UUID.UUID where
  getField = coerce


instance HasField "toText" DashboardId Text where
  getField = UUID.toText . unDashboardId


data Dashboard = Dashboard
  { title :: Maybe Text -- Dashboard title
  , file :: Maybe Text
  , tags :: Maybe [Text]
  , refreshInterval :: Maybe Text -- Refresh interval
  , timeRange :: Maybe TimePicker.TimePicker
  , widgets :: [Widget] -- List of widgets
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Dashboard
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson Dashboard


readDashboardsFromDirectory :: FilePath -> Q Exp
readDashboardsFromDirectory dir = do
  runIO $ putStrLn $ "Reading dashboards from: " ++ dir
  files <- runIO $ listDirectory dir
  let files' = filter (".yaml" `isSuffixOf`) files
  dashboards <- runIO $ catMaybes <$> mapM (readDashboardFile dir) files'
  THS.lift dashboards


readDashboardFile :: FilePath -> FilePath -> IO (Maybe Dashboard)
readDashboardFile dir file = do
  let filePath = dir ++ "/" ++ file
  result <- try $ B.readFile filePath :: IO (Either SomeException B.ByteString)
  case result of
    Left err -> do
      putStrLn $ "Error reading file " ++ filePath ++ ": " ++ show err
      pure Nothing
    Right content ->
      case Yml.decodeEither' content of
        Left err -> do
          putStrLn $ "Error decoding JSON in file: " ++ filePath ++ ": " ++ show err
          pure Nothing
        Right dashboard -> pure (Just $ dashboard{file = Just $ fromString file})
