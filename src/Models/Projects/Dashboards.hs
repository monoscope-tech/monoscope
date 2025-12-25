{-# LANGUAGE PackageImports #-}

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
  selectDashboardsSortedBy,
  updateSchema,
  updateTitle,
  updateSchemaAndUpdatedAt,
  updateStarredSince,
  deleteDashboard,
  getDashboardByBaseTemplate,
  titleToFilePath,
  computeContentSha,
  updateFileInfo,
) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum)
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as W
import Data.Generics.Labels ()
import Data.List qualified as L (isSuffixOf)
import Data.Text qualified as T
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
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL qualified as PG
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
import System.Types (DB)
import Text.Casing (fromAny, toKebab)
import "base16-bytestring" Data.ByteString.Base16 qualified as B16
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)
import "memory" Data.ByteArray qualified as BA


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
  , filePath :: Maybe Text
  , fileSha :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (DBT.Entity)
    via (GenericEntity '[Schema "projects", TableName "dashboards", PrimaryKey "id", FieldModifiers '[CamelToSnake]] DashboardVM)


type DashboardId = UUIDId "dashboard"


data Dashboard = Dashboard
  { title :: Maybe Text
  , description :: Maybe Text
  , preview :: Maybe Text
  , icon :: Maybe Text
  , file :: Maybe Text
  , tags :: Maybe [Text]
  , teams :: Maybe [Text]
  , refreshInterval :: Maybe Text
  , timeRange :: Maybe TimePicker.TimePicker
  , variables :: Maybe [Variable]
  , tabs :: Maybe [Tab]
  , widgets :: [Widget.Widget]
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


insert :: DB es => DashboardVM -> Eff es Int64
insert dashboardVM = PG.execute (Query $ encodeUtf8 q) params
  where
    q =
      [text| INSERT INTO projects.dashboards (id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams, file_path, file_sha)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?::uuid[], ?, ?) |]
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
      , dashboardVM.filePath
      , dashboardVM.fileSha
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


getDashboardById :: DB es => DashboardId -> Eff es (Maybe DashboardVM)
getDashboardById did = listToMaybe <$> PG.query (Query $ encodeUtf8 q) (Only did)
  where
    q = [text| SELECT id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams, file_path, file_sha FROM projects.dashboards WHERE id = ?|]


deleteDashboardsByIds :: DB es => Projects.ProjectId -> V.Vector DashboardId -> Eff es Int64
deleteDashboardsByIds pid dids = PG.execute (Query $ encodeUtf8 q) (pid, dids)
  where
    q = [text|DELETE FROM projects.dashboards WHERE project_id = ? AND id = ANY(?::uuid[])|]


addTeamsToDashboards :: DB es => Projects.ProjectId -> V.Vector DashboardId -> V.Vector UUID.UUID -> Eff es Int64
addTeamsToDashboards pid dids teamIds = PG.execute (Query $ encodeUtf8 q) (teamIds, pid, dids)
  where
    q =
      [text|
      UPDATE projects.dashboards
      SET teams = teams || ?::uuid[]
      WHERE project_id = ? AND id = ANY(?::uuid[])
    |]


selectDashboardsByTeam :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es [DashboardVM]
selectDashboardsByTeam pid teamId = PG.query (Query $ encodeUtf8 q) (pid, teamId)
  where
    q = [text| SELECT id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams, file_path, file_sha FROM projects.dashboards WHERE project_id = ? AND teams @> ARRAY[?::uuid] ORDER BY starred_since DESC NULLS LAST, updated_at DESC|]


selectDashboardsSortedBy :: DB es => Projects.ProjectId -> Text -> Eff es [DashboardVM]
selectDashboardsSortedBy pid orderBy = PG.query (Query $ encodeUtf8 q) (Only pid)
  where
    defaultOrder = "ORDER BY starred_since DESC NULLS LAST, updated_at DESC"
    -- Reject if contains dangerous SQL chars; only allow alphanumeric, underscore, space, comma, and ORDER BY keywords
    isSafe = T.all (\c -> c `elem` ("_-, " :: String) || isAlphaNum c) . T.filter (/= ' ')
    -- Always put starred items first, then apply the provided sort
    safeOrder
      | orderBy == "" || not (isSafe orderBy) = defaultOrder
      | otherwise = T.replace "ORDER BY " "ORDER BY starred_since DESC NULLS LAST, " orderBy
    q = [text| SELECT id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams, file_path, file_sha FROM projects.dashboards WHERE project_id = ? |] <> safeOrder


updateSchema :: DB es => DashboardId -> Dashboard -> Eff es Int64
updateSchema dashId dashboard = PG.execute (Query "UPDATE projects.dashboards SET schema = ? WHERE id = ?") (dashboard, dashId)


updateTitle :: DB es => DashboardId -> Text -> Eff es Int64
updateTitle dashId title = PG.execute (Query "UPDATE projects.dashboards SET title = ? WHERE id = ?") (title, dashId)


updateSchemaAndUpdatedAt :: DB es => DashboardId -> Dashboard -> UTCTime -> Eff es Int64
updateSchemaAndUpdatedAt dashId dashboard updatedAt = PG.execute (Query "UPDATE projects.dashboards SET schema = ?, updated_at = ? WHERE id = ?") (dashboard, updatedAt, dashId)


updateStarredSince :: DB es => DashboardId -> Maybe UTCTime -> Eff es Int64
updateStarredSince dashId starredSince = PG.execute (Query "UPDATE projects.dashboards SET starred_since = ? WHERE id = ?") (starredSince, dashId)


deleteDashboard :: DB es => DashboardId -> Eff es Int64
deleteDashboard dashId = PG.execute (Query "DELETE FROM projects.dashboards WHERE id = ?") (Only dashId)


getDashboardByBaseTemplate :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe DashboardId)
getDashboardByBaseTemplate pid baseTemplate = coerce @(Maybe (Only DashboardId)) @(Maybe DashboardId) . listToMaybe <$> PG.query (Query "SELECT id FROM projects.dashboards WHERE project_id = ? AND base_template = ?") (pid, baseTemplate)


titleToFilePath :: Text -> Text
titleToFilePath title = toText (toKebab $ fromAny $ toString $ T.strip title) <> ".yaml"


-- | Compute SHA256 hash of content and return as hex string
computeContentSha :: ByteString -> Text
computeContentSha content = decodeUtf8 $ B16.encode $ BA.convert (hash content :: Digest SHA256)


-- | Update file_path and file_sha for a dashboard
updateFileInfo :: DB es => DashboardId -> Text -> Text -> Eff es Int64
updateFileInfo dashId path sha = PG.execute (Query "UPDATE projects.dashboards SET file_path = ?, file_sha = ?, updated_at = now() WHERE id = ?") (path, sha, dashId)
