module Models.Projects.Dashboards (
  Dashboard (..),
  DashboardVM (..),
  DashboardId,
  readDashboardFile,
  Variable (..),
  VariableType (..),
  Tab (..),
  getDashboardById,
  readDashboardsFromDirectory,
  readDashboardEndpoint,
  replaceQueryVariables,
  deleteDashboardsByIds,
  addTeamsToDashboards,
  insert,
  selectDashboardsByTeam,
  selectDashboardsSorted,
) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Text qualified as T
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
import Database.PostgreSQL.Simple.TypeInfo.Static (uuid)
import Database.PostgreSQL.Simple.Types (Only (Only), Query (Query))
import Database.PostgreSQL.Transact qualified as DBT
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL.Transact.Effect
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax qualified as THS
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DashboardUtils qualified as DashboardUtils
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import Servant (ServerError (..), err404)
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
  , teams :: V.Vector UUID.UUID
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (DBT.Entity)
    via (GenericEntity '[Schema "projects", TableName "dashboards", PrimaryKey "id", FieldModifiers '[CamelToSnake]] DashboardVM)


type DashboardId = UUIDId "dashboard"


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
  , tabs :: Maybe [Tab] -- List of tabs
  , widgets :: [Widget.Widget] -- List of widgets (for backward compatibility)
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
  , dependsOn :: Maybe Text -- Variable this one depends on
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "_v", DAE.CamelToSnake]] Variable


data Tab = Tab
  { name :: Text
  , icon :: Maybe Text
  , requires :: Maybe Text -- Required variable name for tab to be active
  , widgets :: [Widget.Widget]
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Tab


insert :: DB :> es => DashboardVM -> Eff es Int64
insert dashboardVM = do
  dbtToEff $ DBT.execute (Query $ encodeUtf8 q) params
  where
    q =
      [text| INSERT INTO projects.dashboards (id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?::uuid[]) |]
    params =
      ( dashboardVM.id
      , dashboardVM.projectId
      , dashboardVM.createdAt
      , dashboardVM.updatedAt
      , dashboardVM.createdBy
      , dashboardVM.baseTemplate
      , dashboardVM.schema
      , dashboardVM.starredSince
      , dashboardVM.homepageSince
      , dashboardVM.tags
      , dashboardVM.title
      , dashboardVM.teams
      )


readDashboardsFromDirectory :: FilePath -> Q Exp
readDashboardsFromDirectory dir = do
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
      (\e -> throwError $ err404{errBody = "Error decoding dashboard: " <> show e})
      pure


replaceQueryVariables :: Projects.ProjectId -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> Variable -> Variable
replaceQueryVariables pid mf mt allParams currentTime variable =
  let mappng = DashboardUtils.variablePresets pid.toText mf mt allParams currentTime
   in variable
        & #sql . _Just %~ DashboardUtils.replacePlaceholders mappng
        & #query . _Just %~ DashboardUtils.replacePlaceholders mappng


getDashboardById :: DB :> es => Text -> Eff es (Maybe DashboardVM)
getDashboardById did = dbtToEff $ DBT.queryOne (Query $ encodeUtf8 q) (Only did)
  where
    q = [text| SELECT id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams FROM projects.dashboards WHERE id = ?|]


deleteDashboardsByIds :: DB :> es => Projects.ProjectId -> V.Vector DashboardId -> Eff es Int64
deleteDashboardsByIds pid dids = dbtToEff $ DBT.execute (Query $ encodeUtf8 q) (pid, dids)
  where
    q = [text|DELETE FROM projects.dashboards WHERE project_id = ? AND id = ANY(?::uuid[])|]


addTeamsToDashboards :: DB :> es => Projects.ProjectId -> V.Vector DashboardId -> V.Vector UUID.UUID -> Eff es Int64
addTeamsToDashboards pid dids teamIds = do
  dbtToEff $ DBT.execute (Query $ encodeUtf8 q) (teamIds, pid, dids)
  where
    q =
      [text|
      UPDATE projects.dashboards
      SET teams = teams || ?::uuid[] 
      WHERE project_id = ? AND id = ANY(?::uuid[])
    |]


selectDashboardsByTeam :: DB :> es => Projects.ProjectId -> UUID.UUID -> Eff es [DashboardVM]
selectDashboardsByTeam pid teamId = do
  dbtToEff $ DBT.query (Query $ encodeUtf8 q) (pid, teamId)
  where
    q = [text| SELECT id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams FROM projects.dashboards WHERE project_id = ? AND teams @> ARRAY[?::uuid] ORDER BY starred_since DESC NULLS LAST, updated_at DESC|]


selectDashboardsSorted :: DB :> es => Projects.ProjectId -> Text -> Eff es (V.Vector DashboardVM)
selectDashboardsSorted pid orderBy = do
  V.fromList <$> dbtToEff (DBT.query (Query $ encodeUtf8 q) (Only pid))
  where
    defaultOrder = "ORDER BY starred_since DESC NULLS LAST, updated_at DESC"
    -- Whitelist allowed sort columns to prevent SQL injection
    allowedColumns = ["id", "created_at", "updated_at", "starred_since", "homepage_since", "title"]
    sanitizeOrderBy ob
      | ob == "" = defaultOrder
      | "ORDER BY " `T.isPrefixOf` ob = let tokens = words $ T.drop 9 ob
                                         in if all isValidPart tokens then ob else defaultOrder
      | otherwise = defaultOrder
    isValidPart p = p `elem` allowedColumns || p `elem` ["ASC", "DESC", "NULLS", "LAST", "FIRST", ","] || any (`T.isPrefixOf` p) allowedColumns
    q = [text| SELECT id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams FROM projects.dashboards WHERE project_id = ? |] <> sanitizeOrderBy orderBy
