module Models.Projects.Dashboards (
  Dashboard (..),
  DashboardVM (..),
  DashboardView (..),
  variablesForTab,
  setSavedView,
  mkDashboardVM,
  DashboardId,
  readDashboardFile,
  readDashboardsFromDisk,
  Variable (..),
  VariableType (..),
  SqlSource (..),
  SecuredSql (..),
  Tab (..),
  Constant (..),
  getDashboardByIdUnscoped,
  getDashboardByProjectId,
  getDashboardByFilePath,
  readDashboardsFromDirectory,
  readDashboardEndpoint,
  validateDashboard,
  allWidgets,
  replaceDashboardVariables,
  deleteDashboardsByIds,
  addTeamsToDashboards,
  removeTeamsFromDashboards,
  insert,
  getOrInsertByBaseTemplate,
  selectDashboardsByTeam,
  selectDashboardsSortedBy,
  DashboardSort (..),
  SortOrder (..),
  updateSchema,
  updateTitle,
  updateTags,
  updateStarredSince,
  getDashboardByBaseTemplate,
  autoProvisionedTemplates,
  markAutoProvisioned,
) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson qualified as AE
import Data.Default
import Data.Effectful.Hasql (SecuredSql (..), SqlSource (..))
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as W
import Data.Generics.Labels ()
import Data.Generics.Product.Fields (HasField', field')
import Data.OpenApi (ToParamSchema)
import Data.Text qualified as T
import Data.Text.Display (Display, display)
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Data.Yaml qualified as Yml
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity (..), PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.Error.Static (Error, throwError)
import GHC.Records (HasField (..))
import Hasql.Interpolate qualified as HI
import Hasql.Transaction.Sessions qualified as TxS
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax qualified as THS
import Models.Projects.Activation qualified as Activation
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Pkg.Components.Table (SortField (..), SortOrder (..))
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..), selectFrom)
import Pkg.Parser (TemplateLang (..), fillQueryTemplate, variablePresets, variablePresetsKQL)
import Relude
import Servant (FromHttpApiData, ServerError (..), err404)
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, (</>))
import System.Types (DB)
import Text.Slugify (slugify)


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
  , teams :: V.Vector ProjectMembers.TeamId
  , filePath :: Maybe Text
  , fileSha :: Maybe Text
  , gitSyncId :: Maybe (UUIDId "github_sync")
  , savedViews :: V.Vector DashboardView
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
  , views :: Maybe [DashboardView]
  , tabs :: Maybe [Tab]
  , discoveryMetrics :: Maybe (NonEmpty Text) -- Metric-name prefixes (e.g. "postgresql.") that auto-enable this template for projects emitting them
  , widgets :: [Widget.Widget]
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson Dashboard
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Dashboard
  deriving (HI.DecodeValue, HI.EncodeValue) via HI.AsJsonb Dashboard


data DashboardView = DashboardView
  { name :: Text
  , variables :: Map Text Text
  }
  deriving stock (Eq, Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson DashboardView
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake DashboardView
  deriving (HI.DecodeValue, HI.EncodeValue) via HI.AsJsonb DashboardView


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
  , sql :: Maybe SecuredSql
  , facetField :: Maybe Text -- Populate options from the precomputed facet catalog (bare field path, e.g. "db.system.name") instead of a live DISTINCT scan; falls back to sql when absent.
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
  , sql :: Maybe SecuredSql -- SQL query and its owning store
  , query :: Maybe Text -- KQL query to execute (alternative to sql)
  , description :: Maybe Text -- Optional description
  , result :: Maybe [[Text]] -- Populated with query results after execution
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Constant


data Tab = Tab
  { name :: Text
  , icon :: Maybe Text
  , variables :: Maybe [Text]
  , requires :: Maybe Text -- Required variable name for tab to be active
  , widgets :: [Widget.Widget]
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Tab


-- | A tab's URL segment: every tab link, lookup and widget scope goes through this one rule.
instance HasField "slug" Tab Text where
  getField = slugify . (.name)


-- | A blank dashboard row; callers override the fields they actually set.
mkDashboardVM :: DashboardId -> Projects.ProjectId -> UTCTime -> Projects.UserId -> DashboardVM
mkDashboardVM did pid now uid = DashboardVM{id = did, projectId = pid, createdAt = now, updatedAt = now, createdBy = uid, baseTemplate = Nothing, schema = Nothing, starredSince = Nothing, homepageSince = Nothing, tags = V.empty, title = "", teams = V.empty, filePath = Nothing, fileSha = Nothing, gitSyncId = Nothing, savedViews = V.empty}


insert :: DB es => DashboardVM -> Eff es DashboardVM
insert d = do
  HI.OneColumn teams <- Hasql.interpOneOrThrow "Dashboard insert" (insertSql d)
  Activation.recordActivationMilestone d.projectId Activation.DashboardCreated
  pure (d :: DashboardVM){teams}


-- | The project's dashboard for @d@'s base template, inserting @d@ when there is none;
-- 'True' when it was created. The lock serialises concurrent first visits, which each
-- saw no dashboard and each inserted one.
getOrInsertByBaseTemplate :: DB es => DashboardVM -> Eff es (DashboardId, Bool)
getOrInsertByBaseTemplate d = do
  r <- Hasql.transaction TxS.ReadCommitted TxS.Write do
    void $ Hasql.queryTx @[Text] [HI.sql| SELECT pg_advisory_xact_lock(hashtextextended(concat_ws('/', 'dashboard', #{d.projectId}::text, #{d.baseTemplate}), 0))::text |]
    Hasql.queryTx [HI.sql| SELECT id FROM projects.dashboards WHERE project_id = #{d.projectId} AND base_template = #{d.baseTemplate} LIMIT 1 |] >>= \case
      did : _ -> pure (did, False)
      [] -> Hasql.queryTx @[HI.OneColumn (V.Vector ProjectMembers.TeamId)] (insertSql d) $> (d.id, True)
  when (snd r) $ Activation.recordActivationMilestone d.projectId Activation.DashboardCreated
  pure r


insertSql :: DashboardVM -> HI.Sql
insertSql d =
  [HI.sql| INSERT INTO projects.dashboards (id, project_id, created_at, updated_at, created_by, base_template, schema, starred_since, homepage_since, tags, title, teams, file_path, file_sha, git_sync_id, saved_views)
             VALUES (#{d.id}, #{d.projectId}, #{d.createdAt}, #{d.updatedAt}, #{d.createdBy}, #{d.baseTemplate}, #{d.schema}, #{d.starredSince}, #{d.homepageSince}, #{d.tags}, #{d.title},
               CASE WHEN cardinality(#{d.teams}::uuid[]) = 0
                 THEN ARRAY(SELECT id FROM projects.teams WHERE project_id = #{d.projectId} AND is_everyone AND deleted_at IS NULL)
                 ELSE #{d.teams}::uuid[] END,
               #{d.filePath}, #{d.fileSha}, #{d.gitSyncId}, #{d.savedViews}) RETURNING teams |]


yamlFiles :: FilePath -> IO [FilePath]
yamlFiles dir = sort . filter ("yaml" `isExtensionOf`) <$> listDirectory dir


-- | Read dashboard YAML files from directory at compile time via TH
readDashboardsFromDirectory :: FilePath -> Q Exp
readDashboardsFromDirectory dir = do
  files <- runIO $ yamlFiles dir
  mapM_ (THS.addDependentFile . (dir </>)) files
  runIO (mapMaybeM (readDashboardFile dir) files) >>= THS.lift


-- | Read single dashboard YAML file
readDashboardFile :: FilePath -> FilePath -> IO (Maybe Dashboard)
readDashboardFile dir file = do
  raw <- try @SomeException $ readFileBS path
  let parsed =
        first (("read error: " <>) . show) raw
          >>= first (("YAML error: " <>) . show)
          . Yml.decodeEither'
          >>= validateDashboard
          :: Either Text Dashboard
  parsed
    & either
      (\e -> Nothing <$ putTextLn ("Error loading dashboard " <> toText path <> ": " <> e))
      (\d -> pure $ Just d{file = Just $ fromString file})
  where
    path = dir </> file


readDashboardsFromDisk :: FilePath -> IO [Dashboard]
readDashboardsFromDisk dir = yamlFiles dir >>= mapMaybeM (readDashboardFile dir)


readDashboardEndpoint :: (Error ServerError :> es, HTTP :> es) => Text -> Eff es Dashboard
readDashboardEndpoint uri = do
  fileResp <- W.get (toString uri)
  either (\e -> throwError err404{errBody = "Error decoding dashboard: " <> show e}) pure
    $ first show (Yml.decodeEither' $ toStrict $ fileResp ^. W.responseBody)
    >>= validateDashboard


-- | Every widget across root and tabs, group children included.
allWidgets :: Dashboard -> [Widget.Widget]
allWidgets dashboard = foldMap (universeOf (#children . _Just . folded)) $ dashboard.widgets <> foldMap (concatMap (.widgets)) dashboard.tabs


-- | Reject handwritten sortable SQL unless it exposes the trusted sort slot
-- and a fallback order. Without both, the browser can display a sortable header
-- while the database still limits the original order, which is a false control.
validateDashboard :: Dashboard -> Either Text Dashboard
validateDashboard dashboard = case violations of
  [] -> Right dashboard
  xs -> Left $ "sortable SQL widgets require {{table_sort}} and default_sort: " <> T.intercalate ", " xs
  where
    violations =
      [ fromMaybe "<untitled>" widget.title
      | widget <- allWidgets dashboard
      , any ((== Just True) . (.sortable)) $ fold widget.columns
      , Just sql <- [widget.sql]
      , isNothing widget.defaultSort || not ("{{table_sort}}" `T.isInfixOf` sql)
      ]


-- | Substitute the dashboard variable presets into whichever of @sql@ / @query@ a
-- 'Variable' or a 'Constant' carries. One function over both, because the two shapes
-- differ only in the fields they do /not/ share.
replaceDashboardVariables
  :: (HasField' "query" a (Maybe Text), HasField' "sql" a (Maybe SecuredSql))
  => Projects.ProjectId -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> a -> a
replaceDashboardVariables pid mf mt allParams currentTime x =
  x & field' @"sql" . _Just . #statement %~ fill SqlTemplate variablePresets & field' @"query" . _Just %~ fill KqlTemplate variablePresetsKQL
  where
    fill lang presets = fillQueryTemplate lang (presets def pid.toText mf mt allParams currentTime)


-- | Look up a dashboard WITHOUT checking which project owns it.
--
-- Almost never what you want: a dashboard id reaching a handler from a URL, a
-- chat message or an API body is attacker-controlled, and answering it without
-- a project scope serves one tenant another tenant's dashboard. Use
-- 'getDashboardByProjectId'.
--
-- Legitimate only where the caller has no project in hand precisely BECAUSE it
-- is acting on a row it already fetched (post-update sync) or on a job it
-- already scoped. The name is deliberately loud so those sites are a decision
-- rather than a default.
getDashboardByIdUnscoped :: DB es => DashboardId -> Eff es (Maybe DashboardVM)
getDashboardByIdUnscoped did = Hasql.interpOne (selectFrom @DashboardVM <> [HI.sql| WHERE id = #{did} |])


-- | The scoped lookup. A dashboard belonging to another project reads as absent,
-- which is the correct answer to give a caller that should not know it exists.
getDashboardByProjectId :: DB es => Projects.ProjectId -> DashboardId -> Eff es (Maybe DashboardVM)
getDashboardByProjectId pid did = Hasql.interpOne (selectFrom @DashboardVM <> [HI.sql| WHERE project_id = #{pid} AND id = #{did} |])


getDashboardByFilePath :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe DashboardVM)
getDashboardByFilePath pid fp = Hasql.interpOne (selectFrom @DashboardVM <> [HI.sql| WHERE project_id = #{pid} AND file_path = #{fp} AND git_sync_id IS NULL LIMIT 1 |])


deleteDashboardsByIds :: DB es => Projects.ProjectId -> V.Vector DashboardId -> Eff es Int64
deleteDashboardsByIds pid dids = Hasql.interpExecute [HI.sql| DELETE FROM projects.dashboards WHERE project_id = #{pid} AND id = ANY(#{dids}::uuid[]) |]


addTeamsToDashboards :: DB es => Projects.ProjectId -> V.Vector DashboardId -> V.Vector ProjectMembers.TeamId -> Eff es Int64
addTeamsToDashboards pid dids teamIds =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.dashboards SET teams = ARRAY(SELECT DISTINCT unnest(teams || #{teamIds}::uuid[]))
           WHERE project_id = #{pid} AND id = ANY(#{dids}::uuid[]) |]


removeTeamsFromDashboards :: DB es => Projects.ProjectId -> V.Vector DashboardId -> V.Vector ProjectMembers.TeamId -> Eff es Int64
removeTeamsFromDashboards pid dids teamIds =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.dashboards d
             SET teams = ARRAY(SELECT unnest(d.teams) EXCEPT SELECT unnest(#{teamIds}::uuid[]))
             WHERE d.project_id = #{pid} AND d.id = ANY(#{dids}::uuid[]) |]


selectDashboardsByTeam :: DB es => Projects.ProjectId -> ProjectMembers.TeamId -> Eff es [DashboardVM]
selectDashboardsByTeam pid teamId =
  Hasql.interp
    ( selectFrom @DashboardVM
        <> [HI.sql| WHERE project_id = #{pid} AND teams @> ARRAY[#{teamId}::uuid]
           ORDER BY starred_since DESC NULLS LAST, updated_at DESC |]
    )


-- | Sortable dashboard-list columns. The spelling is both the @sort@ URL value and the column:
--
-- >>> import Data.Text.Display (display)
-- >>> map display [minBound .. maxBound :: DashboardSort]
-- ["title","created_at","updated_at"]
data DashboardSort = DSTitle | DSCreatedAt | DSUpdatedAt
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (Display, FromHttpApiData, ToParamSchema) via WrappedEnumSC 'Nothing "DS" DashboardSort


selectDashboardsSortedBy :: DB es => Projects.ProjectId -> SortOrder -> DashboardSort -> Eff es [DashboardVM]
selectDashboardsSortedBy pid order key =
  Hasql.interp
    $ selectFrom @DashboardVM
    <> [HI.sql| WHERE project_id = #{pid} ORDER BY starred_since DESC NULLS LAST, |]
    <> fromString (toString (SortField (display key) order).toSql)


-- | Replace a dashboard's schema, optionally stamping @updated_at@. In-page widget edits pass
-- 'Nothing' so reordering a chart does not reshuffle the "recently updated" list; git sync and
-- the API pass the sync clock.
updateSchema :: DB es => DashboardId -> Dashboard -> Maybe UTCTime -> Eff es Int64
updateSchema dashId dashboard updatedAt =
  Hasql.interpExecute
    $ [HI.sql| UPDATE projects.dashboards SET schema = #{dashboard} |]
    <> foldMap (\t -> [HI.sql| , updated_at = #{t} |]) updatedAt
    <> [HI.sql| WHERE id = #{dashId} |]


-- | Absent tab variable lists retain the dashboard's full filter bar.
variablesForTab :: Dashboard -> Maybe Tab -> [Variable]
variablesForTab dash tab = filter (\v -> all (v.key `elem`) (tab >>= (.variables))) (fold dash.variables)


-- | Replace (@Just@) or delete (@Nothing@) one named view atomically; concurrent saves of other views survive.
setSavedView :: DB es => Projects.ProjectId -> DashboardId -> Text -> Maybe (Map Text Text) -> Eff es (Maybe (V.Vector DashboardView))
setSavedView pid did viewName variables =
  HI.getOneColumn
    <<$>> Hasql.interpOne
      [HI.sql|
  UPDATE projects.dashboards SET saved_views =
    ARRAY(SELECT v FROM unnest(saved_views) v WHERE v->>'name' <> #{viewName}) || #{replacement}
  WHERE project_id = #{pid} AND id = #{did} RETURNING saved_views
|]
  where
    replacement = foldMap (V.singleton . DashboardView viewName) variables


updateTitle :: DB es => Projects.ProjectId -> DashboardId -> Text -> Eff es Int64
updateTitle pid dashId title = Hasql.interpExecute [HI.sql| UPDATE projects.dashboards SET title = #{title} WHERE project_id = #{pid} AND id = #{dashId} |]


updateTags :: DB es => Projects.ProjectId -> DashboardId -> V.Vector Text -> Eff es Int64
updateTags pid dashId tags = Hasql.interpExecute [HI.sql| UPDATE projects.dashboards SET tags = #{tags} WHERE project_id = #{pid} AND id = #{dashId} |]


updateStarredSince :: DB es => Projects.ProjectId -> DashboardId -> Maybe UTCTime -> Eff es Int64
updateStarredSince pid dashId starredSince = Hasql.interpExecute [HI.sql| UPDATE projects.dashboards SET starred_since = #{starredSince} WHERE project_id = #{pid} AND id = #{dashId} |]


getDashboardByBaseTemplate :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe DashboardId)
getDashboardByBaseTemplate pid baseTemplate =
  Hasql.interpOne [HI.sql| SELECT id FROM projects.dashboards WHERE project_id = #{pid} AND base_template = #{baseTemplate} |]


autoProvisionedTemplates :: DB es => Projects.ProjectId -> Eff es [Text]
autoProvisionedTemplates pid = Hasql.interp [HI.sql| SELECT base_template FROM projects.auto_provisioned_dashboards WHERE project_id = #{pid} |]


markAutoProvisioned :: DB es => Projects.ProjectId -> Text -> Eff es Int64
markAutoProvisioned pid baseTemplate =
  Hasql.interpExecute [HI.sql| INSERT INTO projects.auto_provisioned_dashboards (project_id, base_template) VALUES (#{pid}, #{baseTemplate}) ON CONFLICT DO NOTHING |]
