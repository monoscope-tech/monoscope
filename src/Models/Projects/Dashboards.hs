module Models.Projects.Dashboards (
  Dashboard (..),
  DashboardVM (..),
  DashboardId,
  readDashboardFile,
  readDashboardsFromDisk,
  Variable (..),
  VariableType (..),
  Tab (..),
  Constant (..),
  getDashboardById,
  readDashboardsFromDirectory,
  readDashboardEndpoint,
  replaceQueryVariables,
  replaceConstantVariables,
  deleteDashboardsByIds,
  addTeamsToDashboards,
  insert,
  selectDashboardsByTeam,
  selectDashboardsSortedBy,
  updateSchema,
  updateTitle,
  updateTags,
  updateSchemaAndUpdatedAt,
  updateStarredSince,
  deleteDashboard,
  getDashboardByBaseTemplate,
) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson qualified as AE
import Data.Default
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as W
import Data.Generics.Labels ()
import Data.List qualified as L (isSuffixOf, lookup)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Data.Yaml qualified as Yml
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity (..), PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Effectful
import Effectful.Error.Static (Error, throwError)
import Hasql.Interpolate qualified as HI
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax qualified as THS
import Models.Projects.Projects qualified as Projects
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), selectFrom)
import Pkg.Parser (replacePlaceholders, variablePresets)
import Relude
import Servant (ServerError (..), err404)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Types (DB)


data DashboardVM = DashboardVM
  { id :: DashboardId
  , projectId :: Projects.ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , createdBy :: Projects.UserId
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
  deriving anyclass (FromRow, HI.DecodeRow, NFData, ToRow)
  deriving
    (Entity)
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
  , constants :: Maybe [Constant]
  , variables :: Maybe [Variable]
  , tabs :: Maybe [Tab]
  , widgets :: [Widget.Widget]
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson Dashboard
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Dashboard
  deriving (HI.DecodeValue, HI.EncodeValue) via HI.AsJsonb Dashboard


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


-- | Dashboard constants are query results that can be reused across widgets.
-- Unlike variables which provide UI selection, constants execute a SQL or KQL
-- query once and make the results available as a list that other queries can
-- reference using {{const-<key>}} (e.g., in IN clauses).
data Constant = Constant
  { key :: Text -- The name used to reference this constant, e.g., "top_resources"
  , sql :: Maybe Text -- SQL query to execute
  , query :: Maybe Text -- KQL query to execute (alternative to sql)
  , description :: Maybe Text -- Optional description
  , result :: Maybe [[Text]] -- Populated with query results after execution
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Constant


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
insert DashboardVM{id = did, projectId, createdAt, updatedAt, createdBy, baseTemplate, schema, starredSince, homepageSince, tags, title, teams, filePath, fileSha} =
  Hasql.interpExecute
    [HI.sql| INSERT INTO projects.dashboards (id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams, file_path, file_sha)
           VALUES (#{did}, #{projectId}, #{createdAt}, #{updatedAt}, #{createdBy}, #{baseTemplate}, #{schema}, #{starredSince}, #{homepageSince}, #{tags}, #{title}, #{teams}::uuid[], #{filePath}, #{fileSha}) |]


-- | Read dashboard YAML files from directory at compile time via TH
readDashboardsFromDirectory :: FilePath -> Q Exp
readDashboardsFromDirectory dir = do
  files <- runIO $ listDirectory dir
  let files' = sort $ filter (".yaml" `L.isSuffixOf`) files
  mapM_ (THS.addDependentFile . (dir </>)) files'
  dashboards <- runIO $ catMaybes <$> mapM (readDashboardFile dir) files'
  THS.lift dashboards


-- | Read single dashboard YAML file
readDashboardFile :: FilePath -> FilePath -> IO (Maybe Dashboard)
readDashboardFile dir file = do
  let filePath = dir </> file
  result <- try $ readFileBS filePath :: IO (Either SomeException ByteString)
  case result of
    Left err -> do
      putStrLn $ "Error reading file " ++ filePath ++ ": " ++ show err
      pure Nothing
    Right content ->
      case Yml.decodeEither' content of
        Left err -> do
          putStrLn $ "Error decoding YAML in file: " ++ filePath ++ ": " ++ show err
          pure Nothing
        Right dashboard -> pure (Just $ dashboard{file = Just $ fromString file})


readDashboardsFromDisk :: FilePath -> IO [Dashboard]
readDashboardsFromDisk dir = do
  files <- sort . filter (".yaml" `L.isSuffixOf`) <$> listDirectory dir
  catMaybes <$> mapM (readDashboardFile dir) files


readDashboardEndpoint :: (Error ServerError :> es, HTTP :> es) => Text -> Eff es Dashboard
readDashboardEndpoint uri = do
  fileResp <- W.get (toString uri)
  Yml.decodeEither' (toStrict $ fileResp ^. W.responseBody)
    & either
      (\e -> throwError $ err404{errBody = "Error decoding dashboard: " <> show e})
      pure


replaceQueryVariables :: Projects.ProjectId -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> Variable -> Variable
replaceQueryVariables pid mf mt allParams currentTime v = v & #sql . _Just %~ replace & #query . _Just %~ replace
  where
    replace = replacePlaceholders (variablePresets pid.toText mf mt allParams currentTime)


replaceConstantVariables :: Projects.ProjectId -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> Constant -> Constant
replaceConstantVariables pid mf mt allParams currentTime c = c & #sql . _Just %~ replace & #query . _Just %~ replace
  where
    replace = replacePlaceholders (variablePresets pid.toText mf mt allParams currentTime)


getDashboardById :: DB es => DashboardId -> Eff es (Maybe DashboardVM)
getDashboardById did =
  Hasql.interpOne
    (selectFrom @DashboardVM <> [HI.sql| WHERE id = #{did} |])


deleteDashboardsByIds :: DB es => Projects.ProjectId -> V.Vector DashboardId -> Eff es Int64
deleteDashboardsByIds pid dids =
  Hasql.interpExecute
    [HI.sql| DELETE FROM projects.dashboards WHERE project_id = #{pid} AND id = ANY(#{dids}::uuid[]) |]


addTeamsToDashboards :: DB es => Projects.ProjectId -> V.Vector DashboardId -> V.Vector UUID.UUID -> Eff es Int64
addTeamsToDashboards pid dids teamIds =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.dashboards SET teams = teams || #{teamIds}::uuid[]
           WHERE project_id = #{pid} AND id = ANY(#{dids}::uuid[]) |]


selectDashboardsByTeam :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es [DashboardVM]
selectDashboardsByTeam pid teamId =
  Hasql.interp
    ( selectFrom @DashboardVM
        <> [HI.sql| WHERE project_id = #{pid} AND teams @> ARRAY[#{teamId}::uuid]
           ORDER BY starred_since DESC NULLS LAST, updated_at DESC |]
    )


selectDashboardsSortedBy :: DB es => Projects.ProjectId -> Text -> Eff es [DashboardVM]
selectDashboardsSortedBy pid orderByParam =
  Hasql.interp
    $ selectFrom @DashboardVM
    <> [HI.sql| WHERE project_id = #{pid} ORDER BY starred_since DESC NULLS LAST, |]
    <> orderClause
  where
    sortFields = [("title", "title ASC"), ("created_at", "created_at DESC"), ("updated_at", "updated_at DESC")] :: [(Text, HI.Sql)]
    orderClause = fromMaybe "updated_at DESC" (L.lookup (T.toLower $ T.strip orderByParam) sortFields)


updateSchema :: DB es => DashboardId -> Dashboard -> Eff es Int64
updateSchema dashId dashboard =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.dashboards SET schema = #{dashboard} WHERE id = #{dashId} |]


updateTitle :: DB es => DashboardId -> Text -> Eff es Int64
updateTitle dashId title =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.dashboards SET title = #{title} WHERE id = #{dashId} |]


updateTags :: DB es => DashboardId -> V.Vector Text -> Eff es Int64
updateTags dashId tags =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.dashboards SET tags = #{tags} WHERE id = #{dashId} |]


updateSchemaAndUpdatedAt :: DB es => DashboardId -> Dashboard -> UTCTime -> Eff es Int64
updateSchemaAndUpdatedAt dashId dashboard updatedAt =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.dashboards SET schema = #{dashboard}, updated_at = #{updatedAt} WHERE id = #{dashId} |]


updateStarredSince :: DB es => DashboardId -> Maybe UTCTime -> Eff es Int64
updateStarredSince dashId starredSince =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.dashboards SET starred_since = #{starredSince} WHERE id = #{dashId} |]


deleteDashboard :: DB es => DashboardId -> Eff es Int64
deleteDashboard dashId =
  Hasql.interpExecute
    [HI.sql| DELETE FROM projects.dashboards WHERE id = #{dashId} |]


getDashboardByBaseTemplate :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe DashboardId)
getDashboardByBaseTemplate pid baseTemplate =
  fmap (.id)
    <$> Hasql.interpOne @DashboardVM
      (selectFrom @DashboardVM <> [HI.sql| WHERE project_id = #{pid} AND base_template = #{baseTemplate} |])
