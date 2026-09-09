module Models.Apis.Incidents (
  EpisodeId,
  IncidentEventId,
  SlackRootId,
  SlackDeliveryId,
  IncidentSource (..),
  IncidentChange (..),
  IncidentDelivery (..),
  EpisodePhase (..),
  Episode (..),
  SlackDestination (..),
  SlackPayload (..),
  slackPayload,
  IncidentUpdate (..),
  RecordResult (..),
  recordIncidentEvent,
  recordIncidentEventTx,
  getEpisode,
  InvestigationContext (..),
  slackInvestigationContext,
  latestEpisodeTx,
  ErrorResolution (..),
  resolveErrorIncident,
  SlackTimestamp,
  slackTimestamp,
  slackTimestampText,
  DeliveryOperation (..),
  SlackDelivery,
  SlackDeliveryF (..),
  DeliveryOutcome (..),
  claimSlackDeliveries,
  finishSlackDelivery,
  captureSlackRoot,
  observeSlackRoot,
  IncidentSearch,
  IncidentSearchF (..),
  claimIncidentSearches,
  saveIncidentSearchCursor,
  confirmIncidentSearch,
  correlateSlackDelivery,
) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isDigit)
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Relude
import System.Types (DB)
import UnliftIO.Exception (throwIO)


type EpisodeId = UUIDId "incident_episode"
type IncidentEventId = UUIDId "incident_event"
type SlackRootId = UUIDId "slack_incident_root"
type SlackDeliveryId = UUIDId "slack_incident_delivery"


data IncidentSource = MonitorIncident Monitors.QueryMonitorId | IssueIncident Issues.IssueId | ErrorIncident ErrorPatterns.ErrorPatternId
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson IncidentSource


data IncidentChange = IncidentAlert | IncidentObservation | IncidentReminder | IncidentDataUnavailable | IncidentRecovered | IncidentResolved Projects.UserId
  deriving stock (Eq, Show)


data IncidentDelivery = PublishIncident | RefreshIncident
  deriving stock (Eq, Show)


data EpisodePhase = EpisodeActive | EpisodeRecovered | EpisodeResolved
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.ToJSON, HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "Episode" EpisodePhase


data Episode = Episode
  { id :: EpisodeId
  , projectId :: Projects.ProjectId
  , issueId :: Maybe Issues.IssueId
  , phase :: EpisodePhase
  , startedAt :: UTCTime
  , lastEventAt :: UTCTime
  , closedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow)


data SlackDestination = SlackDestination {teamId :: Text, channelId :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (HI.DecodeRow)


-- | A Slack protocol object. Message builders supply content; the worker adds credentials.
--
-- >>> isNothing (AE.decode @SlackPayload "null") && isNothing (AE.decode @SlackPayload "[]")
-- True
-- >>> fmap (AE.decode . AE.encode) (slackPayload (AE.object [])) == fmap Just (slackPayload (AE.object []))
-- True
newtype SlackPayload = SlackPayload (KM.KeyMap AE.Value)
  deriving stock (Eq, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue, HI.EncodeValue) via Aeson SlackPayload


slackPayload :: AE.Value -> Maybe SlackPayload
slackPayload (AE.Object obj) = Just $ SlackPayload obj
slackPayload _ = Nothing


data IncidentUpdate = IncidentUpdate
  { projectId :: Projects.ProjectId
  , source :: IncidentSource
  , observedAt :: UTCTime
  , change :: IncidentChange
  , delivery :: IncidentDelivery
  , issueId :: Maybe Issues.IssueId
  , rootPayload :: SlackPayload
  , replyPayload :: SlackPayload
  , destinations :: [SlackDestination]
  }
  deriving stock (Show)


data RecordResult
  = Recorded Episode IncidentEventId
  | AlreadyRecorded EpisodeId IncidentEventId
  | OlderThanCurrentEpisode
  | NoOpenEpisode
  | UnknownIncidentSource
  | InactiveIncidentSource
  | IncidentIssueMismatch
  deriving stock (Eq, Show)


sourceFields :: IncidentSource -> (Text, UUID.UUID)
sourceFields (MonitorIncident mid) = ("monitor", mid.unQueryMonitorId)
sourceFields (IssueIncident iid) = ("issue", iid.unUUIDId)
sourceFields (ErrorIncident eid) = ("error", eid.unErrorPatternId)


changeFields :: IncidentChange -> (Text, EpisodePhase, Maybe Projects.UserId)
changeFields IncidentAlert = ("alert", EpisodeActive, Nothing)
changeFields IncidentObservation = ("observation", EpisodeActive, Nothing)
changeFields IncidentReminder = ("reminder", EpisodeActive, Nothing)
changeFields IncidentDataUnavailable = ("data_unavailable", EpisodeActive, Nothing)
changeFields IncidentRecovered = ("recovered", EpisodeRecovered, Nothing)
changeFields (IncidentResolved actor) = ("resolved", EpisodeResolved, Just actor)


queryTx :: HI.DecodeResult a => HI.Sql -> Tx.Transaction a
queryTx = Tx.statement () . HI.interp True


executeTx :: HI.Sql -> Tx.Transaction ()
executeTx sql = void (queryTx sql :: Tx.Transaction HI.RowsAffected)


oneTx :: HI.DecodeRow a => HI.Sql -> Tx.Transaction a
oneTx sql = HI.getOneRow <$> queryTx sql


episodeSelect :: HI.Sql
episodeSelect = [HI.sql|SELECT id, project_id, issue_id, phase, started_at, last_event_at, closed_at FROM apis.incident_episodes |]


getEpisode :: DB es => Projects.ProjectId -> EpisodeId -> Eff es (Maybe Episode)
getEpisode pid eid = Hasql.interpOne (episodeSelect <> [HI.sql|WHERE project_id = #{pid} AND id = #{eid}|])


-- | Stored incident evidence for an authorized Slack thread. Monitor settings
-- are current configuration, not a reconstruction of the configuration at onset.
data InvestigationContext = InvestigationContext
  { episodeId :: EpisodeId
  , issueId :: Maybe Issues.IssueId
  , source :: IncidentSource
  , phase :: EpisodePhase
  , startedAt :: UTCTime
  , lastEventAt :: UTCTime
  , closedAt :: Maybe UTCTime
  , initialNotification :: SlackPayload
  , latestNotification :: SlackPayload
  , currentMonitorQuery :: Maybe Text
  , currentMonitorWindowMinutes :: Maybe Int
  , currentMonitorUnit :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


slackInvestigationContext :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Eff es (Maybe InvestigationContext)
slackInvestigationContext pid teamId channelId threadTs =
  Hasql.interpOne
    [HI.sql|SELECT episode.id, episode.issue_id,
      jsonb_build_object('tag', CASE episode.source_kind
        WHEN 'monitor' THEN 'MonitorIncident' WHEN 'issue' THEN 'IssueIncident' WHEN 'error' THEN 'ErrorIncident' END,
        'contents', episode.source_id),
      episode.phase, episode.started_at, episode.last_event_at, episode.closed_at,
      initial.root_payload, latest.root_payload,
      monitor.log_query, monitor.time_window_mins, monitor.alert_config->>'unit'
    FROM apis.slack_incident_roots root
    JOIN apis.incident_episodes episode ON episode.id = root.episode_id
    JOIN apis.incident_events initial ON initial.id = root.first_event_id AND initial.episode_id = episode.id
    JOIN LATERAL (SELECT event.root_payload FROM apis.incident_events event WHERE event.episode_id = episode.id
      ORDER BY event.observed_at DESC,
        (SELECT max(delivery.sequence) FROM apis.slack_incident_deliveries delivery
          WHERE delivery.event_id = event.id AND delivery.root_id = root.id) DESC NULLS LAST
      LIMIT 1) latest ON TRUE
    LEFT JOIN monitors.query_monitors monitor ON episode.source_kind = 'monitor'
      AND monitor.id = episode.source_id AND monitor.project_id = episode.project_id
    WHERE episode.project_id = #{pid} AND root.team_id = #{teamId}
      AND root.channel_id = #{channelId} AND root.message_ts = #{threadTs}|]


latestEpisodeTx :: Projects.ProjectId -> IncidentSource -> Tx.Transaction (Maybe Episode)
latestEpisodeTx pid source =
  let (sourceKind, sourceId) = sourceFields source
   in listToMaybe
        <$> queryTx @[Episode]
          ( episodeSelect
              <> [HI.sql|WHERE project_id = #{pid} AND source_kind = #{sourceKind} AND source_id = #{sourceId}
          ORDER BY started_at DESC, last_event_at DESC LIMIT 1|]
          )


data ErrorResolution = ErrorResolved | ErrorAlreadyResolved | ErrorResolutionDenied | ErrorResolutionConflict RecordResult
  deriving stock (Eq, Show)


-- | Commit operator attribution, activity, and updates to existing incident threads
-- together. Legacy errors without roots still resolve without creating a channel post.
resolveErrorIncident :: DB es => Projects.ProjectId -> ErrorPatterns.ErrorPatternId -> Projects.UserId -> UTCTime -> (Maybe Issues.IssueId -> SlackPayload) -> Eff es ErrorResolution
resolveErrorIncident pid errorId actor now message = Hasql.transaction TxS.ReadCommitted TxS.Write do
  matched <-
    queryTx @[(Text, ErrorPatterns.ErrorState)]
      [HI.sql|SELECT pattern.hash, pattern.state FROM apis.error_patterns pattern
      JOIN projects.project_members member ON member.project_id = pattern.project_id AND member.user_id = #{actor}
      JOIN users.users account ON account.id = member.user_id
      JOIN projects.projects project ON project.id = member.project_id
      WHERE pattern.id = #{errorId} AND pattern.project_id = #{pid}
        AND member.active AND member.deleted_at IS NULL AND account.active AND account.deleted_at IS NULL
        AND project.active AND project.deleted_at IS NULL
        AND (member.permission IN ('edit', 'admin') OR pattern.assignee_id = #{actor})
      FOR UPDATE OF pattern, member|]
  case listToMaybe matched of
    Nothing -> pure ErrorResolutionDenied
    Just (_, ErrorPatterns.ESResolved) -> pure ErrorAlreadyResolved
    Just (hash, _) -> do
      executeTx
        [HI.sql|UPDATE apis.error_patterns SET state = 'resolved', resolved_at = #{now}, resolved_by = #{actor}, updated_at = #{now}
        WHERE id = #{errorId} AND project_id = #{pid}|]
      sources <-
        queryTx @[(IncidentSource, Maybe Issues.IssueId)]
          [HI.sql|SELECT DISTINCT
            CASE episode.source_kind
              WHEN 'error' THEN jsonb_build_object('tag', 'ErrorIncident', 'contents', episode.source_id)
              WHEN 'issue' THEN jsonb_build_object('tag', 'IssueIncident', 'contents', episode.source_id)
            END,
            CASE WHEN episode.source_kind = 'issue' THEN issue.id ELSE episode.issue_id END
          FROM apis.incident_episodes episode
          LEFT JOIN apis.issues issue ON issue.id = episode.source_id AND episode.source_kind = 'issue'
          WHERE episode.project_id = #{pid} AND episode.phase = 'active' AND
            ((episode.source_kind = 'error' AND episode.source_id = #{errorId}) OR
             (episode.source_kind = 'issue' AND issue.project_id = #{pid} AND issue.target_hash = #{hash} AND issue.issue_type = 'runtime_exception'))
          ORDER BY 1, 2|]
      results <- forM sources \(source, iid) ->
        recordIncidentEventTx
          IncidentUpdate
            { projectId = pid
            , source
            , observedAt = now
            , change = IncidentResolved actor
            , delivery = PublishIncident
            , issueId = iid
            , rootPayload = message iid
            , replyPayload = message iid
            , destinations = []
            }
      case find (\case Recorded{} -> False; AlreadyRecorded{} -> False; NoOpenEpisode -> False; OlderThanCurrentEpisode -> True; UnknownIncidentSource -> True; InactiveIncidentSource -> True; IncidentIssueMismatch -> True) results of
        Nothing -> pure ErrorResolved
        Just conflict -> Tx.condemn $> ErrorResolutionConflict conflict


recordIncidentEvent :: DB es => IncidentUpdate -> Eff es RecordResult
recordIncidentEvent = Hasql.transaction TxS.ReadCommitted TxS.Write . recordIncidentEventTx


-- | Export the transaction as well as its interpreter so the monitor can commit
-- its status, event, and delivery outbox in one database transaction.
-- No network operation occurs while this source lock is held.
recordIncidentEventTx :: IncidentUpdate -> Tx.Transaction RecordResult
recordIncidentEventTx update = do
  let pid = update.projectId
      (sourceKind, sourceId) = sourceFields update.source
      (eventKind, nextPhase, actorId) = changeFields update.change
      at = update.observedAt
      issueId = update.issueId
  -- Match the resolution transaction's lock order before taking the source lock.
  sourceActionable <- case update.source of
    ErrorIncident eid ->
      fromMaybe False
        . listToMaybe
        <$> queryTx @[Bool]
          [HI.sql|SELECT (state <> 'resolved' AND canonical_id IS NULL) OR #{nextPhase}::text <> 'active'
          FROM apis.error_patterns WHERE id = #{eid} AND project_id = #{pid} FOR UPDATE|]
    MonitorIncident _ -> pure True
    IssueIncident _ -> pure True
  -- Hash collisions only serialize unrelated sources. The exact project/source
  -- fields below still determine identity and authorization.
  void
    $ queryTx @[Text]
      [HI.sql|SELECT pg_advisory_xact_lock(hashtextextended(concat_ws('/', #{pid}::text, #{sourceKind}, #{sourceId}::text), 0))::text|]
  validSource <-
    oneTx @Bool
      [HI.sql|SELECT CASE WHEN #{sourceKind} = 'monitor'
    THEN EXISTS (SELECT 1 FROM monitors.query_monitors WHERE id = #{sourceId} AND project_id = #{pid})
    WHEN #{sourceKind} = 'issue' THEN EXISTS (SELECT 1 FROM apis.issues WHERE id = #{sourceId} AND project_id = #{pid})
    WHEN #{sourceKind} = 'error' THEN EXISTS (SELECT 1 FROM apis.error_patterns WHERE id = #{sourceId} AND project_id = #{pid}) ELSE FALSE END|]
  validIssue <-
    oneTx @Bool
      [HI.sql|SELECT CASE WHEN #{sourceKind} = 'error' THEN
        (#{issueId}::uuid IS NULL AND #{nextPhase}::text <> 'active') OR EXISTS
        (SELECT 1 FROM apis.issues issue JOIN apis.error_patterns pattern
          ON pattern.project_id = issue.project_id AND pattern.hash = issue.target_hash
          WHERE pattern.id = #{sourceId} AND issue.id = #{issueId} AND issue.project_id = #{pid} AND issue.issue_type = 'runtime_exception')
      ELSE #{issueId}::uuid IS NULL OR EXISTS
        (SELECT 1 FROM apis.issues WHERE id = #{issueId} AND project_id = #{pid}) END|]
  if not validSource
    then pure UnknownIncidentSource
    else
      if not validIssue
        then pure IncidentIssueMismatch
        else do
          duplicate <-
            queryTx @[(EpisodeId, IncidentEventId)]
              [HI.sql|SELECT episode_id, id FROM apis.incident_events
        WHERE project_id = #{pid} AND source_kind = #{sourceKind} AND source_id = #{sourceId}
          AND observed_at = #{at} AND event_kind = #{eventKind}|]
          case listToMaybe duplicate of
            Just (eid, eventId) -> pure $ AlreadyRecorded eid eventId
            Nothing | not sourceActionable -> pure InactiveIncidentSource
            Nothing -> do
              latest <- latestEpisodeTx pid update.source
              case latest of
                Just episode | at < episode.lastEventAt -> pure OlderThanCurrentEpisode
                _ | (nextPhase /= EpisodeActive || update.change == IncidentDataUnavailable) && maybe True ((/= EpisodeActive) . (.phase)) latest -> pure NoOpenEpisode
                _ -> do
                  episode <- case latest of
                    Just existing | existing.phase == EpisodeActive -> pure existing
                    _ ->
                      oneTx @Episode
                        [HI.sql|INSERT INTO apis.incident_episodes
                (project_id, source_kind, source_id, issue_id, phase, started_at, last_event_at)
                VALUES (#{pid}, #{sourceKind}, #{sourceId}, #{issueId}, 'active', #{at}, #{at})
                RETURNING id, project_id, issue_id, phase, started_at, last_event_at, closed_at|]
                  let eid = episode.id
                      closedAt = if nextPhase == EpisodeActive then Nothing else Just at
                  current <-
                    oneTx @Episode
                      [HI.sql|UPDATE apis.incident_episodes
              SET phase = #{nextPhase}, last_event_at = #{at}, closed_at = #{closedAt},
                  issue_id = COALESCE(issue_id, #{issueId}) WHERE id = #{eid}
              RETURNING id, project_id, issue_id, phase, started_at, last_event_at, closed_at|]
                  eventId <-
                    oneTx @IncidentEventId
                      [HI.sql|INSERT INTO apis.incident_events
              (episode_id, project_id, source_kind, source_id, event_kind, observed_at, actor_id, root_payload, reply_payload)
              VALUES (#{eid}, #{pid}, #{sourceKind}, #{sourceId}, #{eventKind}, #{at}, #{actorId}, #{update.rootPayload}, #{update.replyPayload})
              RETURNING id|]
                  existingDestinations <-
                    queryTx @[SlackDestination]
                      [HI.sql|SELECT team_id, channel_id FROM apis.slack_incident_roots WHERE episode_id = #{eid}|]
                  let destinations = case update.delivery of
                        RefreshIncident -> existingDestinations
                        PublishIncident -> existingDestinations <> [destination | nextPhase == EpisodeActive, destination <- update.destinations]
                  forM_ (ordNub destinations) \destination -> do
                    rootId <-
                      oneTx @SlackRootId
                        [HI.sql|INSERT INTO apis.slack_incident_roots
                        (episode_id, team_id, channel_id, first_event_id)
                        VALUES (#{eid}, #{destination.teamId}, #{destination.channelId}, #{eventId})
                        ON CONFLICT (episode_id, team_id, channel_id) DO UPDATE SET channel_id = EXCLUDED.channel_id
                        RETURNING id|]
                    executeTx
                      [HI.sql|INSERT INTO apis.slack_incident_deliveries
                        (root_id, episode_id, event_id, operation, available_at)
                        SELECT #{rootId}, #{eid}, #{eventId}, 'post_root', #{at}
                        FROM apis.slack_incident_roots WHERE id = #{rootId} AND first_event_id = #{eventId}
                        ON CONFLICT (root_id, event_id, operation) DO NOTHING|]
                    let operations :: [Text]
                        operations = case update.delivery of
                          PublishIncident -> ["post_reply", "update_root"]
                          RefreshIncident -> ["update_root"]
                    forM_ operations \operation ->
                      executeTx
                        [HI.sql|INSERT INTO apis.slack_incident_deliveries
                          (root_id, episode_id, event_id, operation, available_at)
                          SELECT #{rootId}, #{eid}, #{eventId}, #{operation}, #{at}
                          FROM apis.slack_incident_roots WHERE id = #{rootId} AND first_event_id <> #{eventId}
                          ON CONFLICT (root_id, event_id, operation) DO NOTHING|]
                  pure $ Recorded current eventId


newtype SlackTimestamp = SlackTimestamp Text
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON, HI.EncodeValue)


-- | Parse Slack's decimal timestamp without losing precision.
--
-- >>> fmap slackTimestampText (slackTimestamp "1788900000.000001")
-- Just "1788900000.000001"
-- >>> all (isNothing . slackTimestamp) ["", "1", ".1", "1.", "1.2.3", "-1.1"]
-- True
slackTimestamp :: Text -> Maybe SlackTimestamp
slackTimestamp value =
  let (seconds, fraction) = T.breakOn "." value
      digits text = not (T.null text) && T.all isDigit text
   in SlackTimestamp value <$ guard (digits seconds && digits (T.drop 1 fraction))


slackTimestampText :: SlackTimestamp -> Text
slackTimestampText (SlackTimestamp value) = value


data DeliveryOperation timestamp = PostRoot | PostReply timestamp | UpdateRoot timestamp
  deriving stock (Eq, Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson (DeliveryOperation timestamp)


type SlackDelivery = SlackDeliveryF SlackTimestamp


data SlackDeliveryF timestamp = SlackDelivery
  { id :: SlackDeliveryId
  , rootId :: SlackRootId
  , episodeId :: EpisodeId
  , projectId :: Projects.ProjectId
  , teamId :: Text
  , channelId :: Text
  , operation :: DeliveryOperation timestamp
  , payload :: SlackPayload
  , initialPayload :: SlackPayload
  , leaseToken :: UUID.UUID
  , attempts :: Int32
  }
  deriving stock (Eq, Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (HI.DecodeRow)


data DeliveryOutcome
  = DeliveryConfirmed SlackTimestamp
  | WebhookAccepted
  | DeliveryRetry UTCTime Text
  | DeliveryRejected Text
  | DeliveryUncertain Text
  deriving stock (Eq, Show)


-- | An expired send lease is ambiguous, not permission to post another message.
-- Its owner can still confirm it, or a Slack event can reconcile its root.
-- Only the first unsettled delivery for a root is eligible, across all workers.
claimSlackDeliveries :: DB es => UTCTime -> Eff es [SlackDelivery]
claimSlackDeliveries now = do
  result <- Hasql.transaction TxS.ReadCommitted TxS.Write $ claimSlackDeliveriesTx now
  maybe (throwIO InvalidStoredSlackTimestamp) pure result


data IncidentDecodeError = InvalidStoredSlackTimestamp
  deriving stock (Show)
  deriving anyclass (Exception)


claimSlackDeliveriesTx :: UTCTime -> Tx.Transaction (Maybe [SlackDelivery])
claimSlackDeliveriesTx now = do
  executeTx
    [HI.sql|UPDATE apis.slack_incident_deliveries
    SET state = 'uncertain', last_error = 'send_lease_expired'
    WHERE state = 'sending' AND lease_until < #{now}|]
  let leaseUntil = addUTCTime 120 now
  stored <-
    queryTx @[SlackDeliveryF Text]
      [HI.sql|WITH candidates AS (
      SELECT d.id FROM apis.slack_incident_deliveries d
      JOIN apis.slack_incident_roots r ON r.id = d.root_id
      JOIN apis.incident_episodes episode ON episode.id = d.episode_id
      WHERE d.state = 'pending' AND d.available_at <= #{now}
        AND NOT EXISTS (SELECT 1 FROM monitors.query_monitors monitor
          WHERE episode.source_kind = 'monitor' AND monitor.id = episode.source_id AND monitor.muted_until > #{now})
        AND (d.operation = 'post_root' OR r.message_ts IS NOT NULL)
        AND NOT EXISTS (SELECT 1 FROM apis.slack_incident_deliveries earlier
          WHERE earlier.root_id = d.root_id AND earlier.sequence < d.sequence
            AND earlier.state NOT IN ('delivered', 'failed'))
      ORDER BY d.sequence LIMIT 50 FOR UPDATE OF d SKIP LOCKED
    ), claimed AS (
      UPDATE apis.slack_incident_deliveries d
      SET state = 'sending', lease_token = gen_random_uuid(), lease_until = #{leaseUntil}, attempts = attempts + 1
      FROM candidates c WHERE d.id = c.id RETURNING d.*
    ) SELECT d.id, d.root_id, d.episode_id, e.project_id, r.team_id, r.channel_id,
        CASE d.operation
          WHEN 'post_root' THEN jsonb_build_object('tag', 'PostRoot')
          WHEN 'post_reply' THEN jsonb_build_object('tag', 'PostReply', 'contents', r.message_ts)
          WHEN 'update_root' THEN jsonb_build_object('tag', 'UpdateRoot', 'contents', r.message_ts)
        END,
        CASE WHEN d.operation = 'post_reply' THEN e.reply_payload ELSE e.root_payload END,
        initial.root_payload,
        d.lease_token, d.attempts
      FROM claimed d JOIN apis.incident_events e ON e.id = d.event_id
      JOIN apis.slack_incident_roots r ON r.id = d.root_id
      JOIN LATERAL (SELECT root_payload FROM apis.incident_events
        WHERE episode_id = d.episode_id ORDER BY observed_at, id LIMIT 1) initial ON TRUE
      ORDER BY d.sequence|]
  let deliveries = traverse (traverse slackTimestamp) stored
  when (isNothing deliveries) Tx.condemn
  pure deliveries


-- | A completion only belongs to the worker's exact lease. Late results from a
-- previous retry cannot overwrite a newer attempt or replace the thread root.
finishSlackDelivery :: DB es => UTCTime -> SlackDelivery -> DeliveryOutcome -> Eff es Bool
finishSlackDelivery now delivery outcome = Hasql.transaction TxS.ReadCommitted TxS.Write do
  let (deliveryState, availableAt, messageTs, err) = case outcome of
        DeliveryConfirmed ts -> ("delivered", now, Just ts, Nothing)
        WebhookAccepted -> (if delivery.operation == PostRoot then "waiting_root" else "delivered", now, Nothing, Nothing)
        DeliveryRetry retryAt reason -> ("pending", max now retryAt, Nothing, Just reason)
        DeliveryRejected reason -> ("failed", now, Nothing, Just reason)
        DeliveryUncertain reason -> ("uncertain", now, Nothing, Just reason)
  completed <-
    queryTx @[SlackRootId]
      [HI.sql|UPDATE apis.slack_incident_deliveries
    SET state = #{deliveryState :: Text}, available_at = #{availableAt}, message_ts = #{messageTs},
        last_error = #{err}, lease_until = NULL
    WHERE id = #{delivery.id} AND lease_token = #{delivery.leaseToken} AND state IN ('sending', 'uncertain')
    RETURNING root_id|]
  case (listToMaybe completed, delivery.operation, outcome) of
    (Just rid, PostRoot, DeliveryConfirmed ts) -> do
      roots <-
        queryTx @[SlackRootId]
          [HI.sql|UPDATE apis.slack_incident_roots SET message_ts = #{ts}
        WHERE id = #{rid} AND (message_ts IS NULL OR message_ts = #{ts}) RETURNING id|]
      when (null roots)
        $ executeTx
          [HI.sql|UPDATE apis.slack_incident_deliveries
        SET state = 'failed', last_error = 'root_timestamp_conflict' WHERE id = #{delivery.id}|]
      pure $ not $ null roots
    _ -> pure $ not $ null completed


-- | Preserve root correlation while identifying each immutable lifecycle delivery.
correlateSlackDelivery :: SlackDelivery -> SlackPayload -> SlackPayload
correlateSlackDelivery delivery (SlackPayload payload) =
  let eventType = case delivery.operation of
        PostRoot -> "monoscope_incident_root"
        PostReply _ -> "monoscope_incident_delivery"
        UpdateRoot _ -> "monoscope_incident_root"
   in SlackPayload
        $ KM.insert
          "metadata"
          (AE.object ["event_type" AE..= (eventType :: Text), "event_payload" AE..= AE.object ["root_id" AE..= delivery.rootId, "delivery_id" AE..= delivery.id]])
          payload


data RootObservation = RootObservation
  { rootId :: SlackRootId
  , projectId :: Projects.ProjectId
  , teamId :: Text
  , channelId :: Text
  , timestamp :: Text
  }
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


-- | Only persisted signed receipts can supply observations. Verify the message's
-- author app independently of the receiving app, then bind its destination/root.
captureSlackRoot :: DB es => Text -> UUIDId "slack_event" -> Eff es Bool
captureSlackRoot appId receiptId = do
  observation <-
    Hasql.interpOne @RootObservation
      [HI.sql|SELECT r.id, e.project_id, r.team_id, r.channel_id, se.payload #>> '{event,ts}'
      FROM apis.slack_events se
      JOIN apis.slack_incident_roots r ON r.id::text = se.payload #>> '{event,metadata,event_payload,root_id}'
      JOIN apis.incident_episodes e ON e.id = r.episode_id
      JOIN apis.slack s ON s.project_id = e.project_id AND s.team_id = r.team_id
      WHERE se.id = #{receiptId} AND #{appId} <> ''
        AND se.team_id = r.team_id AND se.payload->>'api_app_id' = #{appId}
        AND se.payload #>> '{event,channel}' = r.channel_id
        AND se.payload #>> '{event,type}' = 'message'
        AND COALESCE(se.payload #>> '{event,subtype}', 'bot_message') = 'bot_message'
        AND COALESCE(se.payload #>> '{event,app_id}', se.payload #>> '{event,bot_profile,app_id}') = #{appId}
        AND COALESCE(se.payload #>> '{event,bot_id}', '') <> ''
        AND se.payload #>> '{event,metadata,event_type}' = 'monoscope_incident_root'
        AND se.payload #>> '{event,ts}' IS NOT NULL
        AND COALESCE(se.payload #>> '{event,thread_ts}', se.payload #>> '{event,ts}') = se.payload #>> '{event,ts}'|]
  case observation of
    Just observed
      | Just timestamp <- slackTimestamp observed.timestamp ->
          observeSlackRoot observed.projectId (SlackDestination observed.teamId observed.channelId) observed.rootId timestamp
    _ -> pure False


-- | Each history page has its own lease so a stale search cannot replace a cursor.
type IncidentSearch = IncidentSearchF SlackTimestamp


data IncidentSearchF timestamp = IncidentSearch
  { id :: SlackDeliveryId
  , rootId :: SlackRootId
  , projectId :: Projects.ProjectId
  , teamId :: Text
  , channelId :: Text
  , operation :: DeliveryOperation timestamp
  , cursor :: Maybe Text
  , leaseToken :: UUID.UUID
  }
  deriving stock (Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (HI.DecodeRow)


claimIncidentSearches :: DB es => UTCTime -> Eff es [IncidentSearch]
claimIncidentSearches now = do
  result <- Hasql.transaction TxS.ReadCommitted TxS.Write do
    stored <-
      queryTx @[IncidentSearchF Text]
        [HI.sql|WITH candidates AS (
          SELECT d.id FROM apis.slack_incident_deliveries d
          JOIN apis.slack_incident_roots r ON r.id = d.root_id
          JOIN apis.incident_episodes e ON e.id = r.episode_id
          JOIN projects.projects p ON p.id = e.project_id
          JOIN apis.slack s ON s.project_id = e.project_id AND s.team_id = r.team_id
          WHERE d.attempts > 0 AND d.history_retry_at <= #{now} AND p.active AND p.deleted_at IS NULL
            AND (d.state IN ('uncertain', 'waiting_root') OR (d.state = 'sending' AND d.lease_until < #{now}))
            AND (d.operation = 'post_root' OR r.message_ts IS NOT NULL)
            AND NOT EXISTS (SELECT 1 FROM apis.slack_incident_deliveries earlier
              WHERE earlier.root_id = d.root_id AND earlier.sequence < d.sequence AND earlier.state NOT IN ('delivered', 'failed'))
          ORDER BY d.history_retry_at, d.sequence LIMIT 20 FOR UPDATE OF d SKIP LOCKED
        ), claimed AS (
          UPDATE apis.slack_incident_deliveries d SET history_retry_at = #{addUTCTime 120 now}, history_lease_token = gen_random_uuid()
          FROM candidates c WHERE d.id = c.id RETURNING d.*
        ) SELECT d.id, r.id, e.project_id, r.team_id, r.channel_id,
            CASE d.operation
              WHEN 'post_root' THEN jsonb_build_object('tag', 'PostRoot')
              WHEN 'post_reply' THEN jsonb_build_object('tag', 'PostReply', 'contents', r.message_ts)
              WHEN 'update_root' THEN jsonb_build_object('tag', 'UpdateRoot', 'contents', r.message_ts)
            END, d.history_cursor, d.history_lease_token
          FROM claimed d JOIN apis.slack_incident_roots r ON r.id = d.root_id
          JOIN apis.incident_episodes e ON e.id = r.episode_id|]
    let searches = traverse (traverse slackTimestamp) stored
    when (isNothing searches) Tx.condemn
    pure searches
  maybe (throwIO InvalidStoredSlackTimestamp) pure result


saveIncidentSearchCursor :: DB es => IncidentSearch -> Maybe Text -> UTCTime -> Eff es ()
saveIncidentSearchCursor search next retryAt =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.slack_incident_deliveries SET history_cursor = #{next}, history_retry_at = #{retryAt}, history_lease_token = NULL
    WHERE id = #{search.id} AND state IN ('uncertain', 'waiting_root', 'sending') AND history_lease_token = #{search.leaseToken}|]


confirmIncidentSearch :: DB es => IncidentSearch -> SlackTimestamp -> Eff es Bool
confirmIncidentSearch search timestamp = case search.operation of
  PostRoot -> observeSlackRoot search.projectId (SlackDestination search.teamId search.channelId) search.rootId timestamp
  PostReply root -> confirmAt root
  UpdateRoot root -> confirmAt root
  where
    confirmAt root =
      isJust
        <$> Hasql.interpOne @(HI.OneColumn Bool)
          [HI.sql|UPDATE apis.slack_incident_deliveries d
        SET state = 'delivered', message_ts = #{timestamp}, last_error = NULL, lease_until = NULL, history_lease_token = NULL
        FROM apis.slack_incident_roots r, apis.incident_episodes e
        WHERE d.id = #{search.id} AND d.root_id = #{search.rootId} AND r.id = d.root_id AND e.id = r.episode_id
          AND e.project_id = #{search.projectId} AND r.team_id = #{search.teamId} AND r.channel_id = #{search.channelId}
          AND r.message_ts = #{root} AND d.history_lease_token = #{search.leaseToken} AND d.state IN ('uncertain', 'sending')
          AND ((d.operation = 'update_root' AND #{timestamp} = r.message_ts) OR (d.operation = 'post_reply' AND #{timestamp} <> r.message_ts))
        RETURNING TRUE|]


observeSlackRoot :: DB es => Projects.ProjectId -> SlackDestination -> SlackRootId -> SlackTimestamp -> Eff es Bool
observeSlackRoot pid destination rootId timestamp = Hasql.transaction TxS.ReadCommitted TxS.Write do
  roots <-
    queryTx @[SlackRootId]
      [HI.sql|UPDATE apis.slack_incident_roots r SET message_ts = #{timestamp}
    FROM apis.incident_episodes e WHERE r.id = #{rootId} AND r.episode_id = e.id
      AND e.project_id = #{pid} AND r.team_id = #{destination.teamId} AND r.channel_id = #{destination.channelId}
      AND (r.message_ts IS NULL OR r.message_ts = #{timestamp})
      AND EXISTS (SELECT 1 FROM apis.slack_incident_deliveries d
        WHERE d.root_id = r.id AND d.operation = 'post_root' AND d.attempts > 0)
    RETURNING r.id|]
  unless (null roots)
    $ executeTx
      [HI.sql|UPDATE apis.slack_incident_deliveries
    SET state = 'delivered', message_ts = #{timestamp}, last_error = NULL, lease_until = NULL
    WHERE root_id = #{rootId} AND operation = 'post_root'
      AND state <> 'delivered'|]
  pure $ not $ null roots
