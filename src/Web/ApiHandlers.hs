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
  -- Share links
  apiShareLinkCreate,
  ShareLinkCreate (..),
  ShareLinkCreated (..),
  -- Plan B
  apiMe,
  apiProjectGet,
  apiProjectPatch,
  apiEndpointsList,
  apiEndpointGet,
  apiLogPatternsList,
  apiLogPatternGet,
  apiLogPatternAck,
  apiLogPatternsBulk,
  apiIssuesList,
  apiIssueGet,
  apiIssueAck,
  apiIssueUnack,
  apiIssueArchive,
  apiIssueUnarchive,
  apiIssuesBulk,
  -- Teams (B3)
  apiTeamsList,
  apiTeamGet,
  apiTeamCreate,
  apiTeamUpdate,
  apiTeamPatch,
  apiTeamDelete,
  apiTeamsBulk,
  -- Members (B4)
  apiMembersList,
  apiMemberGet,
  apiMemberAdd,
  apiMemberPatch,
  apiMemberRemove,
  -- Facets
  apiFacets,
  -- Direct event lookup by (id, timestamp)
  apiEventGet,
  -- Internals exposed for testing
  synthStackFromSpans,
) where

import Control.Lens ((%~), _Just)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.UUID qualified as UUID
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.OpenApi (ToSchema (..))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, nominalDay, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Deriving.Aeson qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Fields qualified as Fields
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.ShareEvents qualified as ShareEvents
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.LogExplorer.Log qualified as Log
import Pkg.Components.TimePicker qualified as TP
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
bulkExec :: BulkAction a -> [(Text, ATBaseCtx Int64)] -> ATBaseCtx (BulkResult a)
bulkExec ba ops = do
  n <- case List.lookup (T.toLower ba.action) ops of
    Just op -> op
    Nothing -> throwError err400{errBody = encodeUtf8 $ "Unknown bulk action: " <> ba.action}
  pure
    $ if n > 0
      then BulkResult{succeeded = ba.ids, failed = []}
      else BulkResult{succeeded = [], failed = [BulkFailure{id = i, error = "not applied"} | i <- ba.ids]}


-- | Paginated list helper: normalises page/per_page with caps and constructs
-- the 'Paged' envelope in one place.
resolvePage :: Maybe Int -> Maybe Int -> Int -> (Int, Int, Int)
resolvePage pageM perPageM capPerPage =
  let page = max 0 (fromMaybe 0 pageM)
      perPage = min capPerPage $ max 1 (fromMaybe 50 perPageM)
   in (page, perPage, page * perPage)


mkPaged :: Int -> Int -> Int -> [a] -> Int -> Paged a
mkPaged page perPage offset items total =
  Paged{items, totalCount = total, page, perPage, hasMore = offset + length items < total}


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
      mergedAc =
        ac
          { Monitors.title = fromMaybe ac.title patch.title
          , Monitors.severity = fromMaybe ac.severity patch.severity
          , Monitors.subject = fromMaybe ac.subject patch.subject
          , Monitors.message = fromMaybe ac.message patch.message
          , Monitors.emails = maybe ac.emails (V.fromList . fmap CI.mk) patch.emails
          , Monitors.emailAll = fromMaybe ac.emailAll patch.emailAll
          , Monitors.slackChannels = maybe ac.slackChannels V.fromList patch.slackChannels
          }
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


apiMonitorBulk :: Projects.ProjectId -> BulkAction UUID.UUID -> ATBaseCtx (BulkResult UUID.UUID)
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


-- | Upsert keyed by file_path.
apiDashboardApply :: Projects.ProjectId -> DashboardYAMLDoc -> ATBaseCtx DashboardFull
apiDashboardApply pid doc = do
  now <- Time.currentTime
  existingM <- Dashboards.getDashboardByFilePath pid doc.filePath
  let newTitle = fromMaybe (T.takeWhileEnd (/= '/') doc.filePath) doc.title
  case existingM of
    Just existing -> do
      _ <- Dashboards.updateSchemaAndUpdatedAt existing.id doc.schema now
      _ <- Dashboards.updateTitle existing.id newTitle
      whenJust doc.tags (void . Dashboards.updateTags existing.id . V.fromList)
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
apiDashboardBulk :: Projects.ProjectId -> BulkAction UUID.UUID -> ATBaseCtx (BulkResult UUID.UUID)
apiDashboardBulk pid ba =
  bulkExec ba [("delete", Dashboards.deleteDashboardsByIds pid (V.fromList $ UUIDId <$> ba.ids))]


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
-- Keep in sync with the SQL interval in 'Pages.Share.getShareLink' and
-- the user-facing copy in 'Pages.Share'.
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
  ShareEvents.createShareLink shareId pid req.eventId (fromMaybe "request" req.eventType) req.eventCreatedAt
  let url = authCtx.config.hostUrl <> "/share/r/" <> UUID.toText shareId
  pure ShareLinkCreated{id = shareId, url}


toProjectSummary :: Projects.Project -> ProjectSummary
toProjectSummary p =
  ProjectSummary
    { id = p.id
    , title = p.title
    , description = p.description
    , paymentPlan = p.paymentPlan
    , timeZone = p.timeZone
    , createdAt = p.createdAt
    , updatedAt = p.updatedAt
    }


toProjectFull :: Projects.Project -> [Text] -> ProjectFull
toProjectFull p emails =
  ProjectFull
    { summary = toProjectSummary p
    , dailyNotif = p.dailyNotif
    , weeklyNotif = p.weeklyNotif
    , notifyEmails = emails
    , endpointAlerts = p.endpointAlerts
    , errorAlerts = p.errorAlerts
    }


apiMe :: Projects.ProjectId -> ATBaseCtx MeResponse
apiMe pid = do
  p <- notFoundOr "Project not found" =<< Projects.projectById pid
  pure MeResponse{projectId = pid, project = toProjectSummary p}


apiProjectGet :: Projects.ProjectId -> ATBaseCtx ProjectFull
apiProjectGet pid = do
  p <- notFoundOr "Project not found" =<< Projects.projectById pid
  emails <- maybe [] (V.toList . (.notify_emails)) <$> PM.getEveryoneTeam pid
  pure $ toProjectFull p emails


-- | Partial update: only provided fields change. 0 rows affected ⇒ 404.
apiProjectPatch :: Projects.ProjectId -> ProjectPatch -> ATBaseCtx ProjectFull
apiProjectPatch pid patch = do
  now <- Time.currentTime
  n <- Projects.patchProjectSettings pid patch.title patch.description patch.timeZone patch.dailyNotif patch.weeklyNotif patch.endpointAlerts patch.errorAlerts now
  when (n == 0) $ throwError err404{errBody = "Project not found"}
  apiProjectGet pid


endpointToSummary :: Endpoints.Endpoint -> EndpointSummary
endpointToSummary e =
  EndpointSummary
    { id = e.id
    , projectId = e.projectId
    , urlPath = e.urlPath
    , method = e.method
    , host = e.host
    , hash = e.hash
    , outgoing = e.outgoing
    , description = e.description
    , serviceName = e.serviceName
    , environment = e.environment
    , totalRequests = Nothing
    , lastSeen = Nothing
    }


apiEndpointsList :: Projects.ProjectId -> Maybe Text -> Maybe Bool -> Maybe Int -> Maybe Int -> ATBaseCtx (Paged EndpointSummary)
apiEndpointsList pid searchM outgoingM pageM perPageM = do
  let (page, perPage, offset) = resolvePage pageM perPageM 100
  (rows, total) <- Endpoints.listEndpointsPaged pid (fromMaybe False outgoingM) searchM perPage offset
  pure $ mkPaged page perPage offset (endpointToSummary <$> rows) total


apiEndpointGet :: Projects.ProjectId -> Endpoints.EndpointId -> ATBaseCtx EndpointFull
apiEndpointGet pid eid = do
  e <- notFoundOr "Endpoint not found" =<< Endpoints.getEndpointById pid eid
  pure
    EndpointFull
      { summary = endpointToSummary e
      , urlParams = e.urlParams
      , createdAt = e.createdAt
      , updatedAt = e.updatedAt
      }


logPatternToSummary :: LogPatterns.LogPattern -> LogPatternSummary
logPatternToSummary lp =
  LogPatternSummary
    { id = LogPatterns.unLogPatternId lp.id
    , projectId = lp.projectId
    , patternHash = lp.patternHash
    , sourceField = lp.sourceField
    , serviceName = lp.serviceName
    , logLevel = lp.logLevel
    , state = lp.state
    , occurrenceCount = lp.occurrenceCount
    , firstSeenAt = zonedTimeToUTC lp.firstSeenAt
    , lastSeenAt = zonedTimeToUTC lp.lastSeenAt
    }


apiLogPatternsList :: Projects.ProjectId -> Maybe Int -> Maybe Int -> ATBaseCtx (Paged LogPatternSummary)
apiLogPatternsList pid pageM perPageM = do
  let (page, perPage, offset) = resolvePage pageM perPageM 200
  lps <- LogPatterns.getLogPatterns pid perPage offset
  total <- LogPatterns.countLogPatterns pid
  pure $ mkPaged page perPage offset (logPatternToSummary <$> lps) total


fetchLogPattern :: Projects.ProjectId -> Int64 -> ATBaseCtx LogPatterns.LogPattern
fetchLogPattern pid lpid = notFoundOr "Log pattern not found" =<< LogPatterns.getLogPatternByIdScoped pid lpid


apiLogPatternGet :: Projects.ProjectId -> Int64 -> ATBaseCtx LogPatternFull
apiLogPatternGet pid lpid = do
  lp <- fetchLogPattern pid lpid
  pure
    LogPatternFull
      { summary = logPatternToSummary lp
      , logPattern = lp.logPattern
      , sampleMessage = lp.sampleMessage
      }


apiLogPatternAck :: Projects.ProjectId -> Int64 -> ATBaseCtx LogPatternFull
apiLogPatternAck pid lpid = do
  _ <- fetchLogPattern pid lpid
  _ <- LogPatterns.setLogPatternStatesByIds pid Nothing (V.singleton lpid) LogPatterns.LPSAcknowledged
  apiLogPatternGet pid lpid


apiLogPatternsBulk :: Projects.ProjectId -> BulkAction Int64 -> ATBaseCtx (BulkResult Int64)
apiLogPatternsBulk pid ba = do
  st <- case T.toLower ba.action of
    "acknowledge" -> pure LogPatterns.LPSAcknowledged
    "ignore" -> pure LogPatterns.LPSIgnored
    _ -> throwError err400{errBody = encodeUtf8 $ "Unknown bulk action: " <> ba.action}
  updated <- LogPatterns.setLogPatternStatesByIds pid Nothing (V.fromList ba.ids) st
  let updatedSet = S.fromList updated
  pure
    BulkResult
      { succeeded = updated
      , failed = [BulkFailure{id = i, error = "not applied"} | i <- ba.ids, not (S.member i updatedSet)]
      }


issueToSummary :: Issues.Issue -> IssueApiSummary
issueToSummary i =
  IssueApiSummary
    { id = i.id
    , projectId = i.projectId
    , issueType = i.issueType
    , title = i.title
    , severity = i.severity
    , critical = i.critical
    , service = i.service
    , affectedRequests = i.affectedRequests
    , affectedClients = i.affectedClients
    , acknowledged = isJust i.acknowledgedAt
    , archived = isJust i.archivedAt
    , createdAt = zonedTimeToUTC i.createdAt
    , updatedAt = zonedTimeToUTC i.updatedAt
    }


issueToFull :: Issues.Issue -> IssueApiFull
issueToFull i =
  IssueApiFull
    { summary = issueToSummary i
    , recommendedAction = i.recommendedAction
    , migrationComplexity = i.migrationComplexity
    , issueData = coerce i.issueData
    }


apiIssuesList
  :: Projects.ProjectId
  -> Maybe IssueStatus -- status: open|acknowledged|archived|all
  -> Maybe Text -- issue_type filter
  -> Maybe Text -- service filter
  -> Maybe Int -- page
  -> Maybe Int -- per_page
  -> ATBaseCtx (Paged IssueApiSummary)
apiIssuesList pid statusM typeM svcM pageM perPageM = do
  let (page, perPage, offset) = resolvePage pageM perPageM 200
      status = fromMaybe ISOpen statusM
      -- Map high-level status to (isAck, isArch) filters.
      (ackF, archF) = case status of
        ISOpen -> (Just False, Just False)
        ISAcknowledged -> (Just True, Just False)
        ISArchived -> (Nothing, Just True)
        ISAll -> (Nothing, Nothing)
  (rows, total) <- Issues.selectIssuesByFilters pid ackF archF typeM svcM perPage offset
  pure $ mkPaged page perPage offset (issueToSummary <$> rows) total


fetchIssue :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx Issues.Issue
fetchIssue pid iid = notFoundOr "Issue not found" =<< Issues.selectIssueByIdScoped pid iid


apiIssueGet :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueGet pid iid = issueToFull <$> (enrichIssue pid =<< fetchIssue pid iid)


-- | Fill an empty @stack_trace@ on a runtime-exception issue with a synthesised
-- one derived from the trace's span hierarchy. Most Haskell exceptions arrive
-- with @exception.stacktrace=""@ because GHC doesn't capture a backtrace by
-- default; without this fallback the API/CLI surfaces an empty string and the
-- caller cannot tell whether the data is missing or just unhelpful.
-- TODO(perf): for busy projects this runs two extra queries per issue-detail
-- call (getErrorPatternByHash then getSpanRecordsByTraceId). The right fix is
-- to compute and store the synthetic stack at ingestion/upsert time so reads
-- are zero-cost. Until then the guards keep it O(0) for non-RuntimeException
-- issues and for RuntimeExceptions that already have a stored stack.
enrichIssue :: Projects.ProjectId -> Issues.Issue -> ATBaseCtx Issues.Issue
enrichIssue pid issue
  | issue.issueType /= Issues.RuntimeException = pure issue
  | otherwise = case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (rd :: Issues.RuntimeExceptionData)
        | T.null rd.stackTrace -> do
            epM <- ErrorPatterns.getErrorPatternByHash pid issue.targetHash
            case epM >>= (.recentTraceId) of
              Nothing -> pure issue
              Just trId -> do
                now <- Time.currentTime
                spans <- Telemetry.getSpanRecordsByTraceId pid trId Nothing now
                let synth = synthStackFromSpans trId spans
                pure
                  $ if T.null synth
                    then issue
                    else issue{Issues.issueData = Aeson (AE.toJSON rd{Issues.stackTrace = synth})}
      _ -> pure issue


-- | Build a pseudo-stacktrace from a trace's spans, ordered by start time and
-- annotated with service + span id. Errored span is marked with @!!@.
synthStackFromSpans :: Text -> [Telemetry.OtelLogsAndSpans] -> Text
synthStackFromSpans _ [] = ""
synthStackFromSpans trId spans =
  "(synthesized from trace "
    <> trId
    <> " — GHC backtrace unavailable; spans ordered by start time, !! marks the errored span)\n"
    <> T.intercalate "\n" (map formatOne (sortOn (.start_time) spans))
  where
    formatOne s =
      let svc = Telemetry.spanServiceName s
          spanId = maybe "?" (fromMaybe "?" . (.span_id)) s.context
          errored = s.status_code == Just "ERROR"
          marker = if errored then "!! " else "   "
          svcTxt = foldMap (\v -> " [" <> v <> "]") svc
          label = fromMaybe "<unnamed>" s.name
       in marker <> "at " <> label <> svcTxt <> " (span=" <> spanId <> ")"


issueMutate :: Projects.ProjectId -> Issues.IssueId -> (V.Vector Issues.IssueId -> ATBaseCtx Int64) -> ATBaseCtx IssueApiFull
issueMutate pid iid op = fetchIssue pid iid *> op (V.singleton iid) *> apiIssueGet pid iid


apiIssueAck :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueAck pid iid = do
  now <- Time.currentTime
  issueMutate pid iid $ \ids -> Issues.setAckState pid ids (Just now) Nothing


apiIssueUnack :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueUnack pid iid = issueMutate pid iid $ \ids -> Issues.setAckState pid ids Nothing Nothing


apiIssueArchive :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueArchive pid iid = do
  now <- Time.currentTime
  issueMutate pid iid $ \ids -> Issues.setArchiveState pid ids (Just now)


apiIssueUnarchive :: Projects.ProjectId -> Issues.IssueId -> ATBaseCtx IssueApiFull
apiIssueUnarchive pid iid = issueMutate pid iid $ \ids -> Issues.setArchiveState pid ids Nothing


apiIssuesBulk :: Projects.ProjectId -> BulkAction Issues.IssueId -> ATBaseCtx (BulkResult Issues.IssueId)
apiIssuesBulk pid ba = do
  now <- Time.currentTime
  let vIds = V.fromList ba.ids
      ack = Issues.setAckState pid vIds (Just now) Nothing
      unack = Issues.setAckState pid vIds Nothing Nothing
      archive = Issues.setArchiveState pid vIds (Just now)
      unarchive = Issues.setArchiveState pid vIds Nothing
  bulkExec
    ba
    [ ("acknowledge", ack)
    , ("ack", ack)
    , ("unack", unack)
    , ("unacknowledge", unack)
    , ("archive", archive)
    , ("unarchive", unarchive)
    ]


toTeamSummary :: PM.Team -> TeamSummary
toTeamSummary t =
  TeamSummary
    { id = UUIDId t.id
    , name = t.name
    , handle = t.handle
    , description = t.description
    , isEveryone = t.is_everyone
    , memberCount = V.length t.members
    , createdAt = t.created_at
    , updatedAt = t.updated_at
    }


-- | Project the team's member UUIDs to 'UserRef's via a single users lookup.
resolveTeamMembers :: V.Vector Projects.UserId -> ATBaseCtx [UserRef]
resolveTeamMembers uids
  | V.null uids = pure []
  | otherwise = do
      users <- Projects.usersByIds (V.map Projects.getUserId uids)
      pure (toUserRef <$> users)
  where
    toUserRef u =
      UserRef
        { id = Projects.getUserId u.id
        , email = CI.original u.email
        , name = guarded (not . T.null) (T.strip (u.firstName <> " " <> u.lastName))
        }


toTeamFull :: PM.Team -> ATBaseCtx TeamFull
toTeamFull t = do
  members <- resolveTeamMembers t.members
  pure
    TeamFull
      { summary = toTeamSummary t
      , members = members
      , notifyEmails = V.toList t.notify_emails
      , slackChannels = V.toList t.slack_channels
      , discordChannels = V.toList t.discord_channels
      , phoneNumbers = V.toList t.phone_numbers
      , pagerdutyServices = V.toList t.pagerduty_services
      }


-- | Fetch a single team by id (scoped to project). 404s if missing.
fetchTeam :: Projects.ProjectId -> TeamId -> ATBaseCtx PM.Team
fetchTeam pid tid = notFoundOr "Team not found" =<< PM.getTeamById pid tid.unwrap


apiTeamsList :: Projects.ProjectId -> ATBaseCtx [TeamSummary]
apiTeamsList pid = fmap toTeamSummary <$> PM.getTeams pid


apiTeamGet :: Projects.ProjectId -> TeamId -> ATBaseCtx TeamFull
apiTeamGet pid tid = fetchTeam pid tid >>= toTeamFull


-- | Build 'PM.TeamDetails' from an input, resolving optional collections to empty.
teamDetailsFromInput :: TeamInput -> PM.TeamDetails
teamDetailsFromInput inp =
  PM.TeamDetails
    { name = inp.name
    , description = fromMaybe "" inp.description
    , handle = inp.handle
    , members = vml (fmap Projects.UserId <$> inp.members)
    , notifyEmails = vml inp.notifyEmails
    , slackChannels = vml inp.slackChannels
    , discordChannels = vml inp.discordChannels
    , phoneNumbers = vml inp.phoneNumbers
    , pagerdutyServices = vml inp.pagerdutyServices
    , disabledChannels = V.empty
    }
  where
    vml :: Maybe [a] -> V.Vector a
    vml = V.fromList . fromMaybe []


-- | Rejects the reserved "everyone" handle. Caller owns validation of @name@.
assertHandleAllowed :: Text -> ATBaseCtx ()
assertHandleAllowed h
  | T.toLower h == "everyone" =
      throwError err400{errBody = "Handle \"everyone\" is reserved"}
  | T.null h = throwError err400{errBody = "handle is required"}
  | otherwise = pass


apiTeamCreate :: Projects.ProjectId -> TeamInput -> ATBaseCtx TeamFull
apiTeamCreate pid inp = do
  when (T.null inp.name) $ throwError err400{errBody = "name is required"}
  assertHandleAllowed inp.handle
  createdM <- PM.createTeam pid Nothing (teamDetailsFromInput inp)
  case createdM of
    Nothing -> throwError err400{errBody = encodeUtf8 $ "Handle \"" <> inp.handle <> "\" is already in use"}
    Just tidRaw -> apiTeamGet pid (UUIDId tidRaw)


apiTeamUpdate :: Projects.ProjectId -> TeamId -> TeamInput -> ATBaseCtx TeamFull
apiTeamUpdate pid tid inp = do
  t <- fetchTeam pid tid
  when t.is_everyone $ throwError err400{errBody = "The everyone team cannot be updated via this endpoint"}
  when (T.null inp.name) $ throwError err400{errBody = "name is required"}
  assertHandleAllowed inp.handle
  _ <- PM.updateTeam pid tid.unwrap (teamDetailsFromInput inp)
  apiTeamGet pid tid


apiTeamPatch :: Projects.ProjectId -> TeamId -> TeamPatch -> ATBaseCtx TeamFull
apiTeamPatch pid tid p = do
  t <- fetchTeam pid tid
  when t.is_everyone $ throwError err400{errBody = "The everyone team cannot be updated via this endpoint"}
  whenJust p.handle assertHandleAllowed
  let merged =
        (PM.teamToDetails t)
          { PM.name = fromMaybe t.name p.name
          , PM.description = fromMaybe t.description p.description
          , PM.handle = fromMaybe t.handle p.handle
          , PM.members = maybe t.members (V.fromList . fmap Projects.UserId) p.members
          , PM.notifyEmails = maybe t.notify_emails V.fromList p.notifyEmails
          , PM.slackChannels = maybe t.slack_channels V.fromList p.slackChannels
          , PM.discordChannels = maybe t.discord_channels V.fromList p.discordChannels
          , PM.phoneNumbers = maybe t.phone_numbers V.fromList p.phoneNumbers
          , PM.pagerdutyServices = maybe t.pagerduty_services V.fromList p.pagerdutyServices
          }
  _ <- PM.updateTeam pid tid.unwrap merged
  apiTeamGet pid tid


apiTeamDelete :: Projects.ProjectId -> TeamId -> ATBaseCtx NoContent
apiTeamDelete pid tid = do
  t <- fetchTeam pid tid
  when t.is_everyone $ throwError err400{errBody = "The everyone team cannot be deleted"}
  PM.deleteTeams pid (V.singleton tid.unwrap)
  pure NoContent


apiTeamsBulk :: Projects.ProjectId -> BulkAction TeamId -> ATBaseCtx (BulkResult TeamId)
apiTeamsBulk pid ba = case T.toLower ba.action of
  "delete" -> do
    let ids = (.unwrap) <$> ba.ids
    teams <- PM.getTeamsById pid (V.fromList ids)
    let byId = Map.fromList [(t.id, t) | t <- teams]
        classify uid = case Map.lookup uid byId of
          Nothing -> Left BulkFailure{id = UUIDId uid, error = "not found"}
          Just t | t.is_everyone -> Left BulkFailure{id = UUIDId uid, error = "the everyone team cannot be deleted"}
          Just _ -> Right (UUIDId uid)
        (failed, succeeded) = partitionEithers (classify <$> ids)
    PM.deleteTeams pid (V.fromList [tid.unwrap | tid <- succeeded])
    pure BulkResult{succeeded, failed}
  other -> throwError err400{errBody = encodeUtf8 $ "Unknown bulk action: " <> other}


toMemberSummary :: PM.ProjectMemberVM -> MemberSummary
toMemberSummary m =
  MemberSummary
    { id = m.id
    , userId = Projects.getUserId m.userId
    , email = CI.original m.email
    , firstName = m.first_name
    , lastName = m.last_name
    , permission = m.permission
    }


apiMembersList :: Projects.ProjectId -> ATBaseCtx [MemberSummary]
apiMembersList pid = fmap toMemberSummary <$> PM.selectActiveProjectMembers pid


-- | Locate a single member row via its user_id (project-scoped).
fetchMemberByUserId :: Projects.ProjectId -> UUID.UUID -> ATBaseCtx PM.ProjectMemberVM
fetchMemberByUserId pid uid = notFoundOr "Member not found" =<< PM.getActiveProjectMemberByUserId pid uid


apiMemberGet :: Projects.ProjectId -> UUID.UUID -> ATBaseCtx MemberSummary
apiMemberGet pid uid = toMemberSummary <$> fetchMemberByUserId pid uid


-- | Add a member by email or user_id. Email lookup creates a stub user if no
-- matching row exists (mirrors the HTML manage-members flow).
apiMemberAdd :: Projects.ProjectId -> MemberAdd -> ATBaseCtx MemberSummary
apiMemberAdd pid req = do
  let perm = fromMaybe PM.PView req.permission
  uid <- case (req.userId, req.email) of
    (Just u, _) -> do
      _ <- notFoundOr "User not found" =<< Projects.userById (Projects.UserId u)
      pure (Projects.UserId u)
    (Nothing, Just e) -> do
      existing <- Projects.userIdByEmail e
      case existing of
        Just u -> pure u
        Nothing -> notFoundOr "Could not create user" =<< Projects.createEmptyUser e
    (Nothing, Nothing) ->
      throwError err400{errBody = "Either email or user_id is required"}
  _ <-
    PM.insertProjectMembers
      [PM.CreateProjectMembers{projectId = pid, userId = uid, permission = perm}]
  apiMemberGet pid (Projects.getUserId uid)


apiMemberPatch :: Projects.ProjectId -> UUID.UUID -> MemberPatch -> ATBaseCtx MemberSummary
apiMemberPatch pid uid p = do
  m <- fetchMemberByUserId pid uid
  PM.updateProjectMembersPermissons [(m.id, p.permission)]
  apiMemberGet pid uid


apiMemberRemove :: Projects.ProjectId -> UUID.UUID -> ATBaseCtx NoContent
apiMemberRemove pid uid = do
  m <- fetchMemberByUserId pid uid
  PM.softDeleteProjectMembers (m.id :| [])
  pure NoContent


-- | GET /api/v1/facets — return the precomputed facet summary for a project.
--
-- Facets are top-N values per field, generated by the OTLP facets background
-- job and stored in @apis.facet_summaries@. They tell an agent (or human)
-- what queries are likely to /work/ — "service X has 1.2k events; status
-- code 500 has 47" — without needing to poke at the data first.
--
-- Optional query params:
--
-- * @since@ — relative window (default 24h), feeds 'parseTimeRange'.
-- * @from@/@to@ — absolute ISO timestamps; override @since@.
-- * @field@ — return only the named field's values (e.g. @resource.service.name@).
--
-- The response is always a JSON object keyed by field path, each value a
-- @[{value, count}]@ list sorted by count descending. Missing/expired
-- facets return @{}@ (not 404) — agents can rely on the shape regardless.
apiFacets :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATBaseCtx AE.Value
apiFacets pid sinceM fromM toM fieldM = do
  now <- Time.currentTime
  let (fromT, toT, _) = TP.parseTimeRange now (TP.TimePicker sinceM fromM toM)
      defaultFrom = fromMaybe (addUTCTime (negate nominalDay) now) fromT
      defaultTo = fromMaybe now toT
  summaryM <- Fields.getFacetSummary pid "otel_logs_and_spans" defaultFrom defaultTo
  let Fields.FacetData facetMap = maybe (Fields.FacetData mempty) (.facetJson) summaryM
      asAeson = AE.toJSON facetMap
      filtered = case (fieldM, asAeson) of
        (Just f, AE.Object o) ->
          let k = AEK.fromText f
           in AE.Object (maybe mempty (AEKM.singleton k) (AEKM.lookup k o))
        _ -> asAeson
  pure filtered


-- | GET /api/v1/events/{id}/time/{ts} — O(1) lookup using the timeseries
-- partition key. Both id and timestamp must be supplied; the DB resolves the
-- row via @timestamp BETWEEN ts-1s AND ts+1s AND id = ?@ without a range scan.
-- Returns 404 when the event is not found.
apiEventGet :: Projects.ProjectId -> UUID.UUID -> UTCTime -> ATBaseCtx AE.Value
apiEventGet pid eid ts = do
  mItem <- Telemetry.logRecordByProjectAndId pid ts eid
  case mItem of
    Nothing -> throwError err404{errBody = encodeUtf8 ("event not found" :: Text)}
    Just item -> pure (AE.toJSON item)
