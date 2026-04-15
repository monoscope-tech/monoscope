{-# LANGUAGE OverloadedRecordDot #-}

-- | JSON REST handlers for the public API (Plan A).
--
-- Runs in 'ATBaseCtx' because API-key auth supplies the 'ProjectId' directly
-- (no session). Thin adapters over the existing 'Models.*' domain functions
-- so the HTML handlers (which still own toast/redirect/HTML-rendering concerns)
-- stay untouched.
module Web.ApiHandlers (
  -- Monitors
  apiMonitorsList,
  apiMonitorGet,
  apiMonitorCreate,
  apiMonitorUpdate,
  apiMonitorPatch,
  apiMonitorDelete,
  apiMonitorToggleActive,
  apiMonitorMute,
  apiMonitorUnmute,
  apiMonitorResolve,
  apiMonitorBulk,
  -- Dashboards
  apiDashboardsList,
  apiDashboardGet,
  apiDashboardCreate,
  apiDashboardApply,
  apiDashboardUpdate,
  apiDashboardPatch,
  apiDashboardDelete,
  apiDashboardDuplicate,
  apiDashboardStar,
  apiDashboardUnstar,
  apiDashboardYaml,
  apiDashboardWidgetUpsert,
  apiDashboardWidgetDelete,
  apiDashboardWidgetsReorder,
  apiDashboardBulk,
  -- API keys
  apiKeysList,
  apiKeyGet,
  apiKeyCreate,
  apiKeyActivate,
  apiKeyDeactivate,
  apiKeyDelete,
  -- Events query (body variant)
  apiEventsQuery,
  -- Anomalies bulk
  apiAnomaliesBulk,
  -- Share links
  apiShareLinkCreate,
  ShareLinkCreate (..),
  ShareLinkCreated (..),
) where

import Control.Lens ((%~), _Just)
import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUID
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.OpenApi (ToSchema (..))
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Pages.LogExplorer.Log qualified as Log
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (SnakeSchema (..), UUIDId (..))
import Pkg.Parser qualified as Parser
import Relude hiding (ask, id)
import Servant (NoContent (..), ServerError (..), err400, err404)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATBaseCtx)
import Web.ApiTypes


-- | Return the value or throw a 404 with a given message.
notFoundOr :: Text -> Maybe a -> ATBaseCtx a
notFoundOr msg = maybe (throwError err404{errBody = encodeUtf8 msg}) pure


-- | Dispatch a 'BulkAction' against a table of named operations. Each op returns
-- the count of rows affected; when @0@, every id is reported as "not applied".
-- Ops that cannot report a count can @pure (length ba.ids)@ to mark all as succeeded.
bulkExec :: BulkAction -> [(Text, ATBaseCtx Int64)] -> ATBaseCtx BulkResult
bulkExec ba ops = do
  n <- case List.lookup (T.toLower ba.action) ops of
    Just op -> op
    Nothing -> throwError err400{errBody = encodeUtf8 $ "Unknown bulk action: " <> ba.action}
  pure
    $ if n > 0
      then BulkResult{succeeded = ba.ids, failed = []}
      else BulkResult{succeeded = [], failed = (,"not applied") <$> ba.ids}


apiMonitorsList :: Projects.ProjectId -> ATBaseCtx [Monitors.QueryMonitor]
apiMonitorsList = Monitors.queryMonitorsAll


apiMonitorGet :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx Monitors.QueryMonitor
apiMonitorGet pid mid = do
  m <- Monitors.queryMonitorById mid
  case m of
    Just mon | mon.projectId == pid -> pure mon
    _ -> throwError err404{errBody = "Monitor not found"}


-- | Build a 'QueryMonitor' from a 'MonitorInput'. Shared by create/update.
monitorFromInput :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> Maybe Monitors.QueryMonitor -> MonitorInput -> Monitors.QueryMonitor
monitorFromInput pid now mid existingM inp =
  Monitors.QueryMonitor
    { id = mid
    , projectId = pid
    , createdAt = maybe now (.createdAt) existingM
    , updatedAt = now
    , checkIntervalMins = inp.checkIntervalMins
    , alertThreshold = inp.alertThreshold
    , warningThreshold = inp.warningThreshold
    , logQuery = inp.query
    , logQueryAsSql =
        let cfg = (Parser.defSqlQueryCfg pid Parser.fixedUTCTime Nothing Nothing){Parser.presetRollup = Just "5m"}
         in case Parser.parseQueryToComponents cfg inp.query of
              Right (_, qc) -> fromMaybe (maybe "" (.logQueryAsSql) existingM) qc.finalAlertQuery
              Left _ -> maybe "" (.logQueryAsSql) existingM
    , lastEvaluated = now
    , warningLastTriggered = existingM >>= (.warningLastTriggered)
    , alertLastTriggered = existingM >>= (.alertLastTriggered)
    , triggerLessThan = inp.triggerLessThan
    , thresholdSustainedForMins = fromMaybe 0 inp.thresholdSustainedForMins
    , alertConfig =
        Monitors.MonitorAlertConfig
          { title = inp.title
          , severity = fromMaybe "error" inp.severity
          , subject = fromMaybe inp.title inp.subject
          , message = fromMaybe "" inp.message
          , emails = V.fromList $ CI.mk <$> inp.emails
          , emailAll = fromMaybe False inp.emailAll
          , slackChannels = V.fromList inp.slackChannels
          }
    , deactivatedAt =
        if fromMaybe True inp.active
          then Nothing
          else Just now
    , deletedAt = Nothing
    , visualizationType = fromMaybe "timeseries" inp.visualizationType
    , teams = V.fromList inp.teams
    , widgetId = existingM >>= (.widgetId)
    , dashboardId = existingM >>= (.dashboardId)
    , alertRecoveryThreshold = inp.alertRecoveryThreshold
    , warningRecoveryThreshold = inp.warningRecoveryThreshold
    , currentStatus = maybe Monitors.MSNormal (.currentStatus) existingM
    , currentValue = maybe 0 (.currentValue) existingM
    , mutedUntil = existingM >>= (.mutedUntil)
    , renotifyIntervalMins = inp.notifyAfterMins
    , stopAfterCount = inp.stopAfterCount
    , notificationCount = maybe 0 (.notificationCount) existingM
    , timeWindowMins = inp.timeWindowMins
    }


apiMonitorCreate :: Projects.ProjectId -> MonitorInput -> ATBaseCtx Monitors.QueryMonitor
apiMonitorCreate pid inp = do
  now <- Time.currentTime
  mid <- Monitors.QueryMonitorId <$> UUID.genUUID
  let mon = monitorFromInput pid now mid Nothing inp
  _ <- Monitors.queryMonitorUpsert mon
  pure mon


apiMonitorUpdate :: Projects.ProjectId -> Monitors.QueryMonitorId -> MonitorInput -> ATBaseCtx Monitors.QueryMonitor
apiMonitorUpdate pid mid inp = do
  existing <- apiMonitorGet pid mid
  now <- Time.currentTime
  let mon = monitorFromInput pid now mid (Just existing) inp
  _ <- Monitors.queryMonitorUpsert mon
  pure mon


-- | PATCH — merge fields into existing monitor.
apiMonitorPatch :: Projects.ProjectId -> Monitors.QueryMonitorId -> MonitorPatch -> ATBaseCtx Monitors.QueryMonitor
apiMonitorPatch pid mid patch = do
  existing <- apiMonitorGet pid mid
  now <- Time.currentTime
  let ac = existing.alertConfig
      mergedAc = ac{Monitors.title = fromMaybe ac.title patch.title}
      updateActive m =
        case patch.active of
          Nothing -> m
          Just True -> m{Monitors.deactivatedAt = Nothing}
          Just False -> m{Monitors.deactivatedAt = Just now}
      merged =
        updateActive
          existing
            { Monitors.updatedAt = now
            , Monitors.logQuery = fromMaybe existing.logQuery patch.query
            , Monitors.alertThreshold = fromMaybe existing.alertThreshold patch.alertThreshold
            , Monitors.warningThreshold = patch.warningThreshold <|> existing.warningThreshold
            , Monitors.triggerLessThan = fromMaybe existing.triggerLessThan patch.triggerLessThan
            , Monitors.checkIntervalMins = fromMaybe existing.checkIntervalMins patch.checkIntervalMins
            , Monitors.timeWindowMins = fromMaybe existing.timeWindowMins patch.timeWindowMins
            , Monitors.thresholdSustainedForMins = fromMaybe existing.thresholdSustainedForMins patch.thresholdSustainedForMins
            , Monitors.renotifyIntervalMins = patch.notifyAfterMins <|> existing.renotifyIntervalMins
            , Monitors.stopAfterCount = patch.stopAfterCount <|> existing.stopAfterCount
            , Monitors.alertConfig = mergedAc
            , Monitors.teams = maybe existing.teams V.fromList patch.teams
            , Monitors.visualizationType = fromMaybe existing.visualizationType patch.visualizationType
            , Monitors.alertRecoveryThreshold = patch.alertRecoveryThreshold <|> existing.alertRecoveryThreshold
            , Monitors.warningRecoveryThreshold = patch.warningRecoveryThreshold <|> existing.warningRecoveryThreshold
            }
  _ <- Monitors.queryMonitorUpsert merged
  pure merged


apiMonitorDelete :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx NoContent
apiMonitorDelete pid mid = do
  _ <- apiMonitorGet pid mid -- ensures monitor belongs to project
  _ <- Monitors.monitorSoftDeleteByIds [mid]
  pure NoContent


-- | Verify a resource belongs to the project via @fetch@, run a mutation,
-- then re-fetch the post-mutation value. Used by mute/unmute/resolve/toggle
-- style lifecycle endpoints.
withRefetch :: Monad m => m a -> m b -> m a
withRefetch fetch mutate = fetch *> mutate *> fetch


apiMonitorToggleActive :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx Monitors.QueryMonitor
apiMonitorToggleActive pid mid = withRefetch (apiMonitorGet pid mid) (Monitors.monitorToggleActiveById mid)


apiMonitorMute :: Projects.ProjectId -> Monitors.QueryMonitorId -> Maybe Int -> ATBaseCtx Monitors.QueryMonitor
apiMonitorMute pid mid durationM = withRefetch (apiMonitorGet pid mid) (Monitors.monitorMuteByIds durationM [mid])


apiMonitorUnmute :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx Monitors.QueryMonitor
apiMonitorUnmute pid mid = withRefetch (apiMonitorGet pid mid) (Monitors.monitorUnmuteByIds [mid])


apiMonitorResolve :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATBaseCtx Monitors.QueryMonitor
apiMonitorResolve pid mid = withRefetch (apiMonitorGet pid mid) (Monitors.monitorResolveByIds [mid])


apiMonitorBulk :: Projects.ProjectId -> BulkAction -> ATBaseCtx BulkResult
apiMonitorBulk _pid ba =
  bulkExec
    ba
    [ ("delete", Monitors.monitorSoftDeleteByIds qIds)
    , ("activate", Monitors.monitorReactivateByIds qIds)
    , ("deactivate", Monitors.monitorDeactivateByIds qIds)
    , ("mute", Monitors.monitorMuteByIds ba.durationMinutes qIds)
    , ("unmute", Monitors.monitorUnmuteByIds qIds)
    , ("resolve", Monitors.monitorResolveByIds qIds)
    ]
  where
    qIds = Monitors.QueryMonitorId <$> ba.ids


toSummary :: Dashboards.DashboardVM -> DashboardSummary
toSummary d =
  DashboardSummary
    { id = d.id
    , title = d.title
    , tags = d.tags
    , teams = d.teams
    , starred = isJust d.starredSince
    , createdAt = d.createdAt
    , updatedAt = d.updatedAt
    , filePath = d.filePath
    }


toFull :: Dashboards.DashboardVM -> DashboardFull
toFull d = DashboardFull{summary = toSummary d, fileSha = d.fileSha, schema = d.schema}


apiDashboardsList :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATBaseCtx [DashboardSummary]
apiDashboardsList pid sortM teamIdM = do
  ds <- case teamIdM >>= UUID.fromText of
    Just teamId -> Dashboards.selectDashboardsByTeam pid teamId
    Nothing -> Dashboards.selectDashboardsSortedBy pid (fromMaybe "updated_at" sortM)
  pure $ toSummary <$> ds


apiDashboardGet :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx DashboardFull
apiDashboardGet pid did = do
  d <- notFoundOr "Dashboard not found" =<< Dashboards.getDashboardById did
  when (d.projectId /= pid) $ throwError err404{errBody = "Dashboard not found"}
  pure $ toFull d


-- | Insert a new dashboard row from an input.
insertDashboard :: Projects.ProjectId -> Projects.UserId -> UTCTime -> Dashboards.DashboardId -> Text -> Maybe [Text] -> Maybe [UUID.UUID] -> Maybe Text -> Maybe Dashboards.Dashboard -> ATBaseCtx DashboardFull
insertDashboard pid uid now did title tags teams filePath schema = do
  let d =
        Dashboards.DashboardVM
          { id = did
          , projectId = pid
          , createdAt = now
          , updatedAt = now
          , createdBy = uid
          , baseTemplate = Nothing
          , schema = schema
          , starredSince = Nothing
          , homepageSince = Nothing
          , tags = V.fromList (fromMaybe [] tags)
          , title = title
          , teams = V.fromList (fromMaybe [] teams)
          , filePath = filePath
          , fileSha = Nothing
          }
  _ <- Dashboards.insert d
  pure $ toFull d


apiDashboardCreate :: Projects.ProjectId -> DashboardInput -> ATBaseCtx DashboardFull
apiDashboardCreate pid inp = do
  when (T.null inp.title) $ throwError err400{errBody = "title is required"}
  now <- Time.currentTime
  did <- UUIDId <$> UUID.genUUID
  -- API creation has no session so createdBy is a synthetic nil; git sync is skipped
  insertDashboard pid (Projects.UserId UUID.nil) now did inp.title inp.tags inp.teams inp.filePath inp.schema


-- | Upsert keyed by file_path — the core "Infrastructure-as-Code" entrypoint.
apiDashboardApply :: Projects.ProjectId -> DashboardYAMLDoc -> ATBaseCtx DashboardFull
apiDashboardApply pid doc = do
  now <- Time.currentTime
  existingM <- Dashboards.getDashboardByFilePath pid doc.filePath
  let newTitle = fromMaybe (T.takeWhileEnd (/= '/') doc.filePath) doc.title
  case existingM of
    Just existing -> do
      _ <- Dashboards.updateSchemaAndUpdatedAt existing.id doc.schema now
      _ <- Dashboards.updateTitle existing.id newTitle
      whenJust doc.tags \ts -> void $ Dashboards.updateTags existing.id (V.fromList ts)
      apiDashboardGet pid existing.id
    Nothing -> do
      did <- UUIDId <$> UUID.genUUID
      insertDashboard pid (Projects.UserId UUID.nil) now did newTitle doc.tags doc.teams (Just doc.filePath) (Just doc.schema)


apiDashboardUpdate :: Projects.ProjectId -> Dashboards.DashboardId -> DashboardInput -> ATBaseCtx DashboardFull
apiDashboardUpdate pid did inp = do
  _ <- apiDashboardGet pid did
  now <- Time.currentTime
  _ <- Dashboards.updateTitle did inp.title
  whenJust inp.schema $ \s -> void $ Dashboards.updateSchemaAndUpdatedAt did s now
  whenJust inp.tags $ \ts -> void $ Dashboards.updateTags did (V.fromList ts)
  apiDashboardGet pid did


apiDashboardPatch :: Projects.ProjectId -> Dashboards.DashboardId -> DashboardPatch -> ATBaseCtx DashboardFull
apiDashboardPatch pid did patch = do
  _ <- apiDashboardGet pid did
  now <- Time.currentTime
  whenJust patch.title $ \t -> void $ Dashboards.updateTitle did t
  whenJust patch.schema $ \s -> void $ Dashboards.updateSchemaAndUpdatedAt did s now
  whenJust patch.tags $ \ts -> void $ Dashboards.updateTags did (V.fromList ts)
  apiDashboardGet pid did


apiDashboardDelete :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx NoContent
apiDashboardDelete pid did = do
  _ <- apiDashboardGet pid did
  _ <- Dashboards.deleteDashboard did
  pure NoContent


apiDashboardDuplicate :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx DashboardFull
apiDashboardDuplicate pid did = do
  existing <- apiDashboardGet pid did
  now <- Time.currentTime
  newId <- UUIDId <$> UUID.genUUID
  let copyTitle = existing.summary.title <> " (Copy)"
      copySchema = existing.schema & _Just . #title %~ fmap (<> " (Copy)") . (<|> Just "Untitled")
  insertDashboard pid (Projects.UserId UUID.nil) now newId copyTitle (Just $ V.toList existing.summary.tags) (Just $ V.toList existing.summary.teams) Nothing copySchema


apiDashboardStar :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx DashboardFull
apiDashboardStar pid did =
  withRefetch (apiDashboardGet pid did) (Time.currentTime >>= Dashboards.updateStarredSince did . Just)


apiDashboardUnstar :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx NoContent
apiDashboardUnstar pid did = do
  _ <- apiDashboardGet pid did
  _ <- Dashboards.updateStarredSince did Nothing
  pure NoContent


apiDashboardYaml :: Projects.ProjectId -> Dashboards.DashboardId -> ATBaseCtx DashboardYAMLDoc
apiDashboardYaml pid did = do
  d <- apiDashboardGet pid did
  pure
    DashboardYAMLDoc
      { filePath = fromMaybe "" d.summary.filePath
      , title = Just d.summary.title
      , tags = Just (V.toList d.summary.tags)
      , teams = Just (V.toList d.summary.teams)
      , schema = fromMaybe def d.schema
      }


-- | Widget upsert: replace the widget (by id) within the dashboard schema.
apiDashboardWidgetUpsert :: Projects.ProjectId -> Dashboards.DashboardId -> Widget.Widget -> ATBaseCtx Widget.Widget
apiDashboardWidgetUpsert pid did widget = do
  d <- apiDashboardGet pid did
  now <- Time.currentTime
  let sch = fromMaybe def d.schema
      wId = fromMaybe "" widget.id
      replace ws = case break (\w -> w.id == Just wId) ws of
        (pre, _ : post) -> pre ++ [widget] ++ post
        _ -> ws ++ [widget]
      newSchema = sch & #widgets %~ replace
  _ <- Dashboards.updateSchemaAndUpdatedAt did newSchema now
  pure widget


apiDashboardWidgetDelete :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATBaseCtx NoContent
apiDashboardWidgetDelete pid did wId = do
  d <- apiDashboardGet pid did
  now <- Time.currentTime
  let sch = fromMaybe def d.schema
      newSchema = sch & #widgets %~ filter (\w -> w.id /= Just wId)
  _ <- Dashboards.updateSchemaAndUpdatedAt did newSchema now
  pure NoContent


apiDashboardWidgetsReorder :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Map Text WidgetPosition -> ATBaseCtx NoContent
apiDashboardWidgetsReorder pid did _tabM positions = do
  d <- apiDashboardGet pid did
  now <- Time.currentTime
  let sch = fromMaybe def d.schema
      applyPos w = case w.id >>= (`Map.lookup` positions) of
        Nothing -> w
        Just p ->
          let l = fromMaybe def w.layout
           in w{Widget.layout = Just l{Widget.x = Just p.x, Widget.y = Just p.y, Widget.w = Just p.w, Widget.h = Just p.h}}
      newSchema = sch & #widgets %~ fmap applyPos
  _ <- Dashboards.updateSchemaAndUpdatedAt did newSchema now
  pure NoContent


toApiKeySummary :: ProjectApiKeys.ProjectApiKey -> ApiKeySummary
toApiKeySummary k =
  ApiKeySummary
    { id = k.id
    , title = k.title
    , keyPrefix = T.take 12 k.keyPrefix
    , active = k.active
    , createdAt = k.createdAt
    }


apiKeysList :: Projects.ProjectId -> ATBaseCtx [ApiKeySummary]
apiKeysList pid = fmap toApiKeySummary <$> ProjectApiKeys.projectApiKeysByProjectId pid


apiKeyGet :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATBaseCtx ApiKeySummary
apiKeyGet pid kid = do
  k <- notFoundOr "API key not found" =<< ProjectApiKeys.getProjectApiKey kid
  when (k.projectId /= pid) $ throwError err404{errBody = "API key not found"}
  pure $ toApiKeySummary k


apiKeyCreate :: Projects.ProjectId -> ApiKeyCreate -> ATBaseCtx ApiKeyCreated
apiKeyCreate pid inp = do
  authCtx <- ask @AuthContext
  keyUUID <- UUID.genUUID
  let encryptedKeyB64 = ProjectApiKeys.encodeApiKeyB64 authCtx.config.apiKeyEncryptionSecretKey keyUUID
  k <- ProjectApiKeys.newProjectApiKeys pid keyUUID inp.title encryptedKeyB64
  ProjectApiKeys.insertProjectApiKey k
  pure ApiKeyCreated{summary = toApiKeySummary k, key = encryptedKeyB64}


apiKeyActivate :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATBaseCtx ApiKeySummary
apiKeyActivate pid kid = withRefetch (apiKeyGet pid kid) (ProjectApiKeys.activateApiKey kid)


apiKeyDeactivate :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATBaseCtx ApiKeySummary
apiKeyDeactivate pid kid = do
  s <- apiKeyGet pid kid
  _ <- ProjectApiKeys.revokeApiKey kid
  -- getProjectApiKey filters to active keys, so return pre-fetched summary with active=False
  pure ApiKeySummary{id = s.id, title = s.title, keyPrefix = s.keyPrefix, active = False, createdAt = s.createdAt}


apiKeyDelete :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATBaseCtx NoContent
apiKeyDelete pid kid = do
  _ <- apiKeyGet pid kid
  _ <- ProjectApiKeys.revokeApiKey kid
  pure NoContent


apiEventsQuery :: Projects.ProjectId -> EventsQuery -> ATBaseCtx Log.LogResult
apiEventsQuery pid q = Log.queryEvents pid q.query q.since q.from q.to q.source q.limit


-- | Bulk action: currently supports "delete".
apiDashboardBulk :: Projects.ProjectId -> BulkAction -> ATBaseCtx BulkResult
apiDashboardBulk pid ba =
  bulkExec ba [("delete", Dashboards.deleteDashboardsByIds pid (V.fromList $ UUIDId <$> ba.ids))]


-- | Bulk acknowledge/archive issues/anomalies. Ids are issue ids.
-- Routes through existing model functions so cascade via anomaly_hashes matches the HTML path.
apiAnomaliesBulk :: Projects.ProjectId -> BulkAction -> ATBaseCtx BulkResult
apiAnomaliesBulk _pid ba =
  -- API-key auth has no session user — use nil UUID as the acknowledger.
  let nilUid = Projects.UserId UUID.nil
      vIds = V.fromList $ UUID.toText <$> ba.ids
      nAll = fromIntegral (length ba.ids)
   in bulkExec
        ba
        [ ("acknowledge", nAll <$ (Anomalies.acknowledgeAnomalies nilUid vIds >>= void . Anomalies.acknowlegeCascade nilUid . V.fromList))
        , ("archive", Anomalies.archiveAnomaliesAndIssues vIds)
        ]


-- | Payload for POST /share. event_type defaults to "request" (log/span also accepted).
data ShareLinkCreate = ShareLinkCreate
  { eventId :: UUID.UUID
  , eventCreatedAt :: UTCTime
  , eventType :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ShareLinkCreate
  deriving (ToSchema) via SnakeSchema ShareLinkCreate


-- | Response: `{ id: UUID, url: Text }`. URL lasts 48 hours.
data ShareLinkCreated = ShareLinkCreated
  { id :: UUID.UUID
  , url :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ShareLinkCreated
  deriving (ToSchema) via SnakeSchema ShareLinkCreated


apiShareLinkCreate :: Projects.ProjectId -> ShareLinkCreate -> ATBaseCtx ShareLinkCreated
apiShareLinkCreate pid req = do
  authCtx <- ask @AuthContext
  shareId <- UUID.genUUID
  let eventType = fromMaybe "request" req.eventType
      eventId = req.eventId
      eventCreatedAt = req.eventCreatedAt
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.share_events (id, project_id, event_id, event_type, event_created_at)
             VALUES (#{shareId},#{pid},#{eventId},#{eventType},#{eventCreatedAt}) |]
  let url = authCtx.config.hostUrl <> "/share/r/" <> UUID.toText shareId
  pure ShareLinkCreated{id = shareId, url = url}
