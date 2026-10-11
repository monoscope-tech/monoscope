module Models.Projects.RepositorySync (
  Resource (..),
  ResourceKind (..),
  FolderCandidate (..),
  folderCandidates,
  Definition (..),
  UptimeDefinition (..),
  CronDefinition (..),
  Reconciliation (..),
  reconcile,
  resources,
  adoptDashboards,
  normalizeDefinition,
  localDefinition,
  importDefinition,
  resourceSnapshot,
  decodeDefinition,
  encodeDefinition,
  definitionKind,
  registerResource,
  saveBaseline,
  markConflict,
) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.Char (isAsciiLower, isDigit)
import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Exception (catch, throwIO)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Hasql.Transaction.Sessions qualified as TxS
import Models.Apis.MonitorDefinitions (MonitorInput (..), compileAlertSql, monitorFromInput, monitorToInput)
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ProjectMembers qualified as Members
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude hiding (local)
import System.Types (DB)
import Web.HttpApiData (FromHttpApiData (..))


data ResourceKind = Dashboard | Query | Uptime | Cron
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "" ResourceKind


instance FromHttpApiData ResourceKind where
  parseUrlPiece value = maybeToRight "Choose dashboard, query, uptime, or cron." $ lookup value [("dashboard", Dashboard), ("query", Query), ("uptime", Uptime), ("cron", Cron)]


data FolderCandidate = FolderCandidate {kind :: ResourceKind, localId :: UUID.UUID, title :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


folderCandidates :: DB es => Projects.ProjectId -> Eff es [FolderCandidate]
folderCandidates pid =
  Hasql.interp
    [HI.sql|
  SELECT r.kind, r.id, r.title FROM (
    SELECT 'dashboard'::text AS kind, id, title FROM projects.dashboards WHERE project_id = #{pid} AND schema IS NOT NULL AND git_sync_id IS NULL
    UNION ALL SELECT 'query', id, alert_config->>'title' FROM monitors.query_monitors WHERE project_id = #{pid} AND deleted_at IS NULL
    UNION ALL SELECT 'uptime', id, name FROM apis.prometheus_scrape_configs WHERE project_id = #{pid} AND kind = 'uptime'
    UNION ALL SELECT 'cron', id, name FROM apis.cron_monitors WHERE project_id = #{pid}
  ) r WHERE NOT EXISTS (SELECT 1 FROM projects.repository_resources owned WHERE owned.project_id = #{pid} AND owned.kind = r.kind AND owned.local_id = r.id)
  ORDER BY r.kind, r.title |]


data Resource = Resource
  { id :: UUIDId "repository_resource"
  , projectId :: Projects.ProjectId
  , syncId :: GitSync.GitHubSyncId
  , kind :: ResourceKind
  , localId :: UUID.UUID
  , filePath :: Text
  , mergedDefinition :: Maybe AE.Value
  , remoteDefinition :: Maybe AE.Value
  , conflicted :: Bool
  , localBaseline :: Maybe AE.Value
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data UptimeDefinition = UptimeDefinition
  {name :: Text, url :: Text, intervalSeconds :: Int, expectedStatus :: Int, enabled :: Bool}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake UptimeDefinition


data CronDefinition = CronDefinition
  {slug :: Text, name :: Text, intervalSeconds :: Int, graceSeconds :: Int}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake CronDefinition


data Definition
  = DashboardDefinition Dashboards.Dashboard
  | QueryDefinition MonitorInput
  | UptimeDefinitionConfig UptimeDefinition
  | CronDefinitionConfig CronDefinition
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.Rename "DashboardDefinition" "dashboard", DAE.Rename "QueryDefinition" "query", DAE.Rename "UptimeDefinitionConfig" "uptime", DAE.Rename "CronDefinitionConfig" "cron"], DAE.SumTaggedObject "kind" "spec"] Definition


definitionKind :: Definition -> ResourceKind
definitionKind = \case
  DashboardDefinition _ -> Dashboard
  QueryDefinition _ -> Query
  UptimeDefinitionConfig _ -> Uptime
  CronDefinitionConfig _ -> Cron


data Reconciliation = Unchanged | ImportRemote | KeepLocal | Conflict
  deriving stock (Eq, Show)


-- | Three-way comparison includes absence, so deletions cannot bypass conflict handling.
--
-- >>> reconcile (Just "old", Just "old") (Just "local") (Just "old")
-- KeepLocal
-- >>> reconcile (Just "old", Just "old") (Just "old") Nothing
-- ImportRemote
-- >>> reconcile (Just "old", Just "old") (Just "local") Nothing
-- Conflict
-- >>> reconcile (Just "old", Just "old") (Just "new") (Just "new")
-- Unchanged
-- >>> reconcile (Nothing, Nothing) Nothing (Just "new")
-- ImportRemote
reconcile :: Eq a => (Maybe a, Maybe a) -> Maybe a -> Maybe a -> Reconciliation
reconcile (remoteBase, localBase) local remote
  | local == remote = Unchanged
  | local == localBase = ImportRemote
  | remote == remoteBase = KeepLocal
  | otherwise = Conflict


resources :: DB es => Projects.ProjectId -> GitSync.GitHubSyncId -> Eff es [Resource]
resources pid sid = Hasql.interp [HI.sql| SELECT id, project_id, sync_id, kind, local_id, file_path, merged_definition, remote_definition, conflicted, local_baseline FROM projects.repository_resources WHERE project_id = #{pid} AND sync_id = #{sid} ORDER BY file_path |]


adoptDashboards :: DB es => GitSync.GitHubSync -> Eff es ()
adoptDashboards sync =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO projects.repository_resources (project_id, sync_id, kind, local_id, file_path)
  SELECT project_id, git_sync_id, 'dashboard', id, #{GitSync.getDashboardsPath sync} || file_path FROM projects.dashboards
  WHERE project_id = #{sync.projectId} AND git_sync_id = #{sync.id} AND file_path IS NOT NULL
  ON CONFLICT DO NOTHING |]


normalizeDefinition :: (DB es, Time :> es) => Resource -> Definition -> Eff es Definition
normalizeDefinition resource = \case
  DashboardDefinition schema -> do
    teams <- Members.getTeamsVM resource.projectId
    existing <- Dashboards.getDashboardByProjectId resource.projectId (UUIDId resource.localId)
    let title = fromMaybe (maybe "Untitled" (.title) existing) schema.title
        handles = [team.handle | team <- teams, team.handle `elem` fold schema.teams]
    pure $ DashboardDefinition $ GitSync.buildSchemaWithMeta (Just schema) title (fold schema.tags) handles
  QueryDefinition input -> do
    now <- Time.currentTime
    pure $ QueryDefinition $ monitorToInput $ monitorFromInput resource.projectId now (Monitors.QueryMonitorId resource.localId) Nothing input
  value -> pure value


registerResource :: DB es => Projects.ProjectId -> GitSync.GitHubSyncId -> ResourceKind -> UUID.UUID -> Text -> Eff es ()
registerResource pid sid kind localId path =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO projects.repository_resources (project_id, sync_id, kind, local_id, file_path)
    SELECT #{pid}, id, #{kind}, #{localId}, #{path} FROM projects.git_sync WHERE project_id = #{pid} AND id = #{sid}
    ON CONFLICT (project_id, kind, local_id) DO NOTHING |]


saveBaseline :: DB es => Resource -> Maybe AE.Value -> Maybe AE.Value -> Eff es ()
saveBaseline resource remote local = Hasql.interpExecute_ $ baselineSql resource remote local


baselineSql :: Resource -> Maybe AE.Value -> Maybe AE.Value -> HI.Sql
baselineSql resource remote local = [HI.sql| UPDATE projects.repository_resources SET merged_definition = #{remote}, local_baseline = #{local}, remote_definition = NULL, conflicted = false WHERE id = #{resource.id} AND project_id = #{resource.projectId} |]


markConflict :: DB es => Resource -> Maybe AE.Value -> Eff es ()
markConflict resource value = Hasql.interpExecute_ [HI.sql| UPDATE projects.repository_resources SET remote_definition = #{value}, conflicted = true WHERE id = #{resource.id} AND project_id = #{resource.projectId} |]


decodeDefinition :: Text -> ByteString -> Either Text Definition
decodeDefinition path body
  | "/dashboards/" `T.isInfixOf` ("/" <> path) = DashboardDefinition <$> first (toText . Yaml.prettyPrintParseException) (Yaml.decodeEither' body)
  | otherwise = do
      value <- first (toText . Yaml.prettyPrintParseException) $ Yaml.decodeEither' @AE.Value body
      let tagged = case value of AE.Object obj -> KM.member "kind" obj; _ -> False
      first toText $ if tagged then AET.parseEither AE.parseJSON value else QueryDefinition <$> AET.parseEither AE.parseJSON value


encodeDefinition :: Definition -> ByteString
encodeDefinition = \case
  DashboardDefinition schema -> Yaml.encode schema
  QueryDefinition input -> Yaml.encode input
  value@UptimeDefinitionConfig{} -> Yaml.encode value
  value@CronDefinitionConfig{} -> Yaml.encode value


localDefinition :: DB es => Resource -> Eff es (Maybe Definition)
localDefinition resource = case resource.kind of
  Dashboard ->
    Dashboards.getDashboardByProjectId resource.projectId (UUIDId resource.localId)
      >>= traverse
        ( \dash -> do
            teams <- Members.getTeamsById resource.projectId dash.teams
            pure $ DashboardDefinition $ GitSync.buildSchemaWithMeta dash.schema dash.title (V.toList dash.tags) (map (.handle) teams)
        )
        . mfilter (isJust . (.schema))
  Query -> fmap (QueryDefinition . monitorToInput) . mfilter (isNothing . (.deletedAt)) <$> Monitors.queryMonitorByProjectId resource.projectId (Monitors.QueryMonitorId resource.localId)
  Uptime -> fmap (\config -> UptimeDefinitionConfig $ UptimeDefinition config.name config.url config.scrapeIntervalSeconds (fromMaybe 200 config.expectedStatus) config.enabled) <$> PromCfg.getConfigByProject resource.projectId (UUIDId resource.localId)
  Cron -> fmap (\monitor -> CronDefinitionConfig $ CronDefinition monitor.slug monitor.name monitor.intervalSecs monitor.graceSecs) . find ((== resource.localId) . (.id)) <$> Monitors.cronMonitorsByProject resource.projectId


-- | Row snapshots include runtime fields. If either an editor or evaluator changed the row,
-- retry without writing; the imported definition and its baseline commit together.
resourceSnapshot :: DB es => Resource -> Eff es (Maybe AE.Value)
resourceSnapshot resource = fmap (\(HI.OneColumn value) -> value) <$> Hasql.interpOne (snapshotSql resource)


snapshotSql :: Resource -> HI.Sql
snapshotSql resource = case resource.kind of
  Dashboard -> [HI.sql| SELECT to_jsonb(t) FROM projects.dashboards t WHERE project_id = #{resource.projectId} AND id = #{resource.localId} |]
  Query -> [HI.sql| SELECT to_jsonb(t) FROM monitors.query_monitors t WHERE project_id = #{resource.projectId} AND id = #{resource.localId} |]
  Uptime -> [HI.sql| SELECT to_jsonb(t) FROM apis.prometheus_scrape_configs t WHERE project_id = #{resource.projectId} AND id = #{resource.localId} AND kind = 'uptime' |]
  Cron -> [HI.sql| SELECT to_jsonb(t) FROM apis.cron_monitors t WHERE project_id = #{resource.projectId} AND id = #{resource.localId} |]


importDefinition :: (DB es, Time :> es) => Resource -> Maybe AE.Value -> Maybe Definition -> Eff es (Either Text ())
importDefinition resource observed definition = runExceptT do
  now <- lift Time.currentTime
  (queries, normalized) <- case definition of
    Nothing -> pure ([deleteSql now], Nothing)
    Just incoming -> do
      when (resource.kind /= definitionKind incoming) $ hoistEither $ Left "A resource file cannot change its monitor type. Use a new file."
      (sql, value) <- case incoming of
        DashboardDefinition schema -> do
          teams <- lift $ Members.getTeamsVM resource.projectId
          let title = fromMaybe "Untitled" schema.title
              teamIds = V.fromList [team.id | team <- teams, team.handle `elem` fold schema.teams]
              tags = V.fromList $ fold schema.tags
          sync <- lift $ GitSync.getGitSyncById resource.projectId resource.syncId
          let path = maybe resource.filePath (\connection -> fromMaybe resource.filePath $ T.stripPrefix (GitSync.getDashboardsPath connection) resource.filePath) sync
              canonical = GitSync.buildSchemaWithMeta (Just schema) title (V.toList tags) [team.handle | team <- teams, team.id `V.elem` teamIds]
          pure
            ( [HI.sql| INSERT INTO projects.dashboards (id, project_id, created_at, updated_at, created_by, schema, title, tags, teams, git_sync_id, file_path)
              VALUES (#{resource.localId}, #{resource.projectId}, #{now}, #{now}, #{UUID.nil}, #{schema}, #{title}, #{tags}::text[], #{teamIds}::uuid[], #{resource.syncId}, #{path})
              ON CONFLICT (id) DO UPDATE SET schema = EXCLUDED.schema, title = EXCLUDED.title, tags = EXCLUDED.tags, teams = EXCLUDED.teams, updated_at = EXCLUDED.updated_at
              WHERE projects.dashboards.project_id = EXCLUDED.project_id |]
            , DashboardDefinition canonical
            )
        QueryDefinition input -> do
          when (T.null (T.strip input.title) || input.timeWindowMins <= 0 || input.checkIntervalMins <= 0) $ hoistEither $ Left "Monitor title and positive time window and interval are required."
          teams <- lift $ Members.getTeamsVM resource.projectId
          unless (all (`elem` map (.id) teams) input.teams) $ hoistEither $ Left "Monitor teams must belong to this project."
          existing <- lift $ Monitors.queryMonitorByProjectId resource.projectId (Monitors.QueryMonitorId resource.localId)
          let monitor = (monitorFromInput resource.projectId now (Monitors.QueryMonitorId resource.localId) existing input){Monitors.lastEvaluated = existing >>= (.lastEvaluated)}
          when (T.null $ compileAlertSql resource.projectId input.timeWindowMins input.environment input.service input.query "") $ hoistEither $ Left "The monitor query could not be compiled."
          pure (Monitors.queryMonitorUpsertSql monitor, QueryDefinition $ monitorToInput monitor)
        UptimeDefinitionConfig input -> do
          unless (PromCfg.safeScrapeUrl input.url && not (T.null $ T.strip input.name) && input.intervalSeconds >= 60 && input.expectedStatus >= 100 && input.expectedStatus <= 599) $ hoistEither $ Left "Uptime checks require a name, public HTTP URL, interval of at least 60 seconds, and HTTP status 100–599."
          pure
            ( [HI.sql| INSERT INTO apis.prometheus_scrape_configs (id, project_id, kind, name, url, scrape_interval_seconds, extra_labels, expected_status, enabled)
              VALUES (#{resource.localId}, #{resource.projectId}, 'uptime', #{input.name}, #{input.url}, #{input.intervalSeconds}, '{}', #{input.expectedStatus}, #{input.enabled})
              ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name, url = EXCLUDED.url, scrape_interval_seconds = EXCLUDED.scrape_interval_seconds, expected_status = EXCLUDED.expected_status, enabled = EXCLUDED.enabled, updated_at = #{now}
              WHERE apis.prometheus_scrape_configs.project_id = EXCLUDED.project_id |]
            , incoming
            )
        CronDefinitionConfig input -> do
          unless (not (T.null input.slug || T.null (T.strip input.name)) && T.all (\c -> isAsciiLower c || isDigit c || T.any (== c) "_.-") input.slug && input.intervalSeconds >= 60 && input.graceSeconds >= 60) $ hoistEither $ Left "Cron monitors require a name, a slug using a-z, 0-9, _, . or -, and intervals of at least 60 seconds."
          pure
            ( [HI.sql| INSERT INTO apis.cron_monitors (id, project_id, slug, name, interval_secs, grace_secs, created_at)
              VALUES (#{resource.localId}, #{resource.projectId}, #{input.slug}, #{input.name}, #{input.intervalSeconds}, #{input.graceSeconds}, #{now})
              ON CONFLICT (id) DO UPDATE SET slug = EXCLUDED.slug, name = EXCLUDED.name, interval_secs = EXCLUDED.interval_secs, grace_secs = EXCLUDED.grace_secs
              WHERE apis.cron_monitors.project_id = EXCLUDED.project_id |]
            , incoming
            )
      pure ([sql] <> [[HI.sql| UPDATE monitors.query_monitors SET deleted_at = NULL WHERE project_id = #{resource.projectId} AND id = #{resource.localId} |] | resource.kind == Query], Just $ AE.toJSON value)
  ExceptT $ handleCollision $ Hasql.transaction TxS.ReadCommitted TxS.Write $ runExceptT do
    -- Serialize imports/conflict resolution even when the local row does not exist yet.
    present <- lift $ Hasql.queryTx @(Maybe Resource) [HI.sql| SELECT id, project_id, sync_id, kind, local_id, file_path, merged_definition, remote_definition, conflicted, local_baseline FROM projects.repository_resources WHERE id = #{resource.id} AND project_id = #{resource.projectId} FOR UPDATE |]
    fresh <- hoistEither $ maybeToRight "This resource is no longer in the repository folder." present
    unless ((fresh.mergedDefinition, fresh.remoteDefinition, fresh.conflicted, fresh.localBaseline) == (resource.mergedDefinition, resource.remoteDefinition, resource.conflicted, resource.localBaseline)) $ hoistEither $ Left "Sync state changed during import. Refresh and retry."
    current <- lift $ fmap (\(HI.OneColumn value) -> value) <$> Hasql.queryTx @(Maybe (HI.OneColumn AE.Value)) (snapshotSql resource <> [HI.sql| FOR UPDATE |])
    unless (current == observed) $ hoistEither $ Left "This resource changed during import. Retry to reconcile the latest version."
    lift $ traverse_ Hasql.executeTx $ queries <> [baselineSql resource (AE.toJSON <$> definition) normalized]
  where
    handleCollision action = action `catch` \err -> if Hasql.isUniqueViolation err then pure $ Left "Another resource already uses this name or path. Rename it and retry." else throwIO err

    deleteSql :: UTCTime -> HI.Sql
    deleteSql now = case resource.kind of
      Dashboard -> [HI.sql| DELETE FROM projects.dashboards WHERE project_id = #{resource.projectId} AND id = #{resource.localId} |]
      Query -> [HI.sql| UPDATE monitors.query_monitors SET deleted_at = #{now} WHERE project_id = #{resource.projectId} AND id = #{resource.localId} |]
      Uptime -> [HI.sql| DELETE FROM apis.prometheus_scrape_configs WHERE project_id = #{resource.projectId} AND id = #{resource.localId} AND kind = 'uptime' |]
      Cron -> [HI.sql| DELETE FROM apis.cron_monitors WHERE project_id = #{resource.projectId} AND id = #{resource.localId} |]
