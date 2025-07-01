module Models.Projects.Dashboards (Dashboard (..), DashboardVM (..), DashboardId (..), Variable (..), VariableType (..), getDashboardById, readDashboardsFromDirectory, readDashboardEndpoint, replaceQueryVariables) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as W
import Data.Generics.Labels ()
import Data.List qualified as L (isSuffixOf)
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Data.Yaml qualified as Yml
import Database.PostgreSQL.Entity qualified as DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types (Only (Only), Query (Query))
import Database.PostgreSQL.Transact qualified as DBT
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL.Transact.Effect
import GHC.Records (HasField (getField))
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax qualified as THS
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DashboardUtils qualified as DashboardUtils
import Relude
import Servant (FromHttpApiData, ServerError (..), err401)
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
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (DBT.Entity)
    via (GenericEntity '[Schema "projects", TableName "dashboards", PrimaryKey "id", FieldModifiers '[CamelToSnake]] DashboardVM)


newtype DashboardId = DashboardId {unDashboardId :: UUID.UUID}
  deriving stock (Generic, Read, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, Hashable, NFData, Ord, ToField)
  deriving anyclass (FromRow, ToRow)


instance HasField "unwrap" DashboardId UUID.UUID where
  getField = coerce


instance HasField "toText" DashboardId Text where
  getField = UUID.toText . unDashboardId


data Dashboard = Dashboard
  { title :: Maybe Text -- Dashboard title
  , description :: Maybe Text
  , preview :: Maybe Text
  , icon :: Maybe Text
  , file :: Maybe Text
  , tags :: Maybe [Text]
  , refreshInterval :: Maybe Text -- Refresh interval
  , timeRange :: Maybe TimePicker.TimePicker
  , variables :: Maybe [Variable]
  , widgets :: [Widget.Widget] -- List of widgets
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson Dashboard
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Dashboard


data VariableType = VTQuery | VTValues
  deriving stock (Enum, Eq, Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "VT", DAE.CamelToSnake]] VariableType


data Variable = Variable
  { key :: Text
  , title :: Maybe Text
  , multi :: Maybe Bool
  , required :: Maybe Bool
  , reloadOnChange :: Maybe Bool
  , helpText :: Maybe Text
  , _vType :: VariableType
  , sql :: Maybe Text
  , query :: Maybe Text
  , options :: Maybe [[Text]]
  , value :: Maybe Text
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "_v", DAE.CamelToSnake]] Variable


readDashboardsFromDirectory :: FilePath -> Q Exp
readDashboardsFromDirectory dir = do
  runIO $ putStrLn $ "Reading dashboards from: " ++ dir
  files <- runIO $ listDirectory dir
  let files' = sort $ filter (".yaml" `L.isSuffixOf`) files
  dashboards <- runIO $ catMaybes <$> mapM (readDashboardFile dir) files'
  THS.lift dashboards


readDashboardFile :: FilePath -> FilePath -> IO (Maybe Dashboard)
readDashboardFile dir file = do
  let filePath = dir ++ "/" ++ file
  result <- try $ readFileBS filePath :: IO (Either SomeException BS.ByteString)
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


readDashboardEndpoint :: (Error ServerError :> es, HTTP :> es) => Text -> Eff es Dashboard
readDashboardEndpoint uri = do
  fileResp <- W.get (toString uri)
  Yml.decodeEither' (toStrict $ fileResp ^. W.responseBody)
    & either
      (\e -> throwError $ err401{errBody = "Error decoding dashboard: " <> show e})
      pure


replaceQueryVariables :: Projects.ProjectId -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> Variable -> Variable
replaceQueryVariables pid mf mt allParams variable =
  let mappng = DashboardUtils.variablePresets pid.toText mf mt allParams
   in variable
        & #sql . _Just %~ DashboardUtils.replacePlaceholders mappng
        & #query . _Just %~ DashboardUtils.replacePlaceholders mappng


getDashboardById :: DB :> es => Text -> Eff es (Maybe DashboardVM)
getDashboardById did = dbtToEff $ DBT.queryOne (Query $ encodeUtf8 q) (Only did)
  where
    q = [text|SELECT id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title FROM projects.dashboards WHERE id = ?|]