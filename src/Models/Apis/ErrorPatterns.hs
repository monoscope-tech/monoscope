module Models.Apis.ErrorPatterns (
  ErrorPattern (..),
  ErrorPatternId (..),
  ErrorState (..),
  ATError (..),
  CaptureMechanism (..),
  enrichGeo,
  geoFields,
  ErrorPatternL (..),
  -- Queries
  getErrorPatterns,
  getErrorPatternById,
  getErrorPatternByHash,
  updateOccurrenceCountsBatch,
  propagateMergedCountsBatch,
  updateErrorPatternState,
  getErrorPatternLByHash,
  ErrorTraceRefs (..),
  selectErrorTraceRefs,
  bulkCalculateAndUpdateBaselines,
  UpsertOutcome (..),
  batchUpsertErrorPatterns,
  upsertErrorPatternHourlyStats,
  updateErrorPatternSubscription,
  NotifiedStamp (..),
  updateErrorPatternThreadIds,
  setErrorPatternAssignee,
  updateErrorPatternAnalysis,
  -- Error spike detection
  ErrorPatternWithCurrentRate (..),
  upsertErrorPatternUsers,
  userKey,
  errorTags,
  upsertErrorTagCounts,
  selectErrorTagCounts,
  setResolvedInRelease,
  getErrorPatternsWithCurrentRates,
  findCanonicalMatch,
)
where

import Data.Aeson qualified as AE
import Data.Default
import Data.Effectful.Hasql qualified as Hasql
import Data.GeoIP2 qualified as GeoIP2
import Data.HashMap.Strict qualified as HM
import Data.IP (IP)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (BaselineState (..), DB, WrappedEnumSC (..), selectFrom)
import Relude hiding (id)
import Utils (truncateHour)


newtype ErrorPatternId = ErrorPatternId {unErrorPatternId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Eq, FromField, HI.DecodeValue, HI.EncodeValue, NFData, Ord, ToField)
  deriving anyclass (HI.DecodeRow)


-- | How an error pattern is trending. This deliberately overlaps
-- @Incidents.EpisodePhase@ (@active@\/@recovered@\/@resolved@) and the two are
-- bridged by @Incidents.resolveErrorIncident@, which flips both in one
-- transaction.
--
-- They are not merged, and that is a design decision rather than an oversight:
-- this describes the /signal/ (is the error getting worse?) while a phase
-- describes one /notification episode/ (is a Slack thread still open?). One
-- pattern outlives many episodes, so collapsing them would need the escalation
-- states to mean something per-episode. Revisit only with that answer in hand —
-- the bridge is the contract, so any change has to keep the two in step.
data ErrorState
  = ESNew
  | ESEscalating
  | ESOngoing
  | ESResolved
  | ESRegressed
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "ES" ErrorState


data ErrorPattern = ErrorPattern
  { id :: ErrorPatternId
  , projectId :: Projects.ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , errorType :: Text
  , message :: Text
  , stacktrace :: Text
  , hash :: Text
  , environment :: Maybe Text
  , service :: Maybe Text
  , runtime :: Maybe Text
  , errorData :: ATError
  , firstEventId :: Maybe UUID.UUID
  , lastEventId :: Maybe UUID.UUID
  , state :: ErrorState
  , assigneeId :: Maybe Projects.UserId
  , assignedAt :: Maybe ZonedTime
  , resolvedAt :: Maybe ZonedTime
  , regressedAt :: Maybe ZonedTime
  , occurrences_1m :: Int -- snake_case fields match DB column names exactly
  , occurrences_5m :: Int
  , occurrences_1h :: Int
  , occurrences_24h :: Int
  , quietMinutes :: Int
  , resolutionThresholdMinutes :: Int -- Historical setting; silence no longer establishes resolution.
  , baselineState :: BaselineState
  , baselineSamples :: Int
  , baselineErrorRateMean :: Maybe Double
  , baselineErrorRateStddev :: Maybe Double
  , baselineUpdatedAt :: Maybe ZonedTime
  , isIgnored :: Bool
  , ignoredUntil :: Maybe ZonedTime
  , subscribed :: Bool
  , notifyEveryMinutes :: Int
  , lastNotifiedAt :: Maybe ZonedTime
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  , firstTraceId :: Maybe Text
  , recentTraceId :: Maybe Text
  , regressionCount :: Int
  , canonicalId :: Maybe ErrorPatternId
  , embedding :: Maybe (V.Vector Float)
  , embeddingAt :: Maybe ZonedTime
  , mergeOverride :: Bool
  , rootCause :: Maybe Text
  , errorCategory :: Maybe Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , resolvedBy :: Maybe Projects.UserId
  , firstRelease :: Maybe Text
  , lastRelease :: Maybe Text
  , lastReleaseSince :: Maybe ZonedTime
  , resolvedInRelease :: Maybe Text
  , usersCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "error_patterns", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ErrorPattern)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ErrorPattern


-- | Error pattern aggregated with number of occurrences and affected users.
data ErrorPatternL = ErrorPatternL
  { base :: ErrorPattern
  , occurrences :: Int
  , userCount :: Int
  , lastOccurredAt :: Maybe ZonedTime -- hour granularity (bound to MAX(hour_bucket)), not exact event time
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


-- Generic HI.DecodeRow can't derive this: ErrorPattern has DecodeRow but not DecodeValue.
instance HI.DecodeRow ErrorPatternL where
  decodeRow = ErrorPatternL <$> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow <*> HI.decodeRow


-- | How the error reached us. OTel has no "mechanism" attribute, so this is the
-- signal the error was extracted from rather than the SDK's capture hook.
data CaptureMechanism = CMExceptionEvent | CMLogRecord | CMSpanStatus
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display) via WrappedEnumSC 'Nothing "CM" CaptureMechanism


-- | The error's stored snapshot. Rows written before a field existed still decode:
--
-- >>> :set -XOverloadedStrings
-- >>> isRight (AE.eitherDecode @ATError "{\"when\":\"2026-01-01T00:00:00Z\",\"error_type\":\"E\",\"root_error_type\":\"E\",\"message\":\"m\",\"root_error_message\":\"m\",\"stack_trace\":\"\",\"hash\":\"h\",\"is_framework\":false}")
-- True
-- >>> AE.encode CMExceptionEvent
-- "\"exception_event\""
data ATError = ATError
  { projectId :: Maybe Projects.ProjectId
  , when :: UTCTime
  , errorType :: Text
  , rootErrorType :: Text
  , message :: Text
  , rootErrorMessage :: Text
  , stackTrace :: Text
  , hash :: Text
  , parentHash :: Maybe Text
  , shapeHash :: Maybe Text
  , isFramework :: Bool
  , technology :: Maybe LogQueries.SDKTypes
  , requestMethod :: Maybe Text
  , requestPath :: Maybe Text
  , serviceName :: Maybe Text
  , environment :: Maybe Text
  , runtime :: Maybe Text
  , traceId :: Maybe Text
  , spanId :: Maybe Text
  , parentSpanId :: Maybe Text
  , endpointHash :: Maybe Text
  , userId :: Maybe Text
  , userEmail :: Maybe Text
  , userName :: Maybe Text
  , userIp :: Maybe Text
  , sessionId :: Maybe Text
  , tenantName :: Maybe Text
  , -- Everything below is read from OTel semantic-convention attributes, and absent
    -- from rows stored before it existed (the codec defaults a missing field to Nothing).
    release :: Maybe Text
  , handled :: Maybe Bool
  , mechanism :: Maybe CaptureMechanism
  , level :: Maybe Text
  , userAgent :: Maybe Text
  , browser :: Maybe Text
  , os :: Maybe Text
  , device :: Maybe Text
  , geoCountry :: Maybe Text
  , geoRegion :: Maybe Text
  , geoCity :: Maybe Text
  , threadId :: Maybe Text
  , threadName :: Maybe Text
  , urlFull :: Maybe Text
  , urlQuery :: Maybe Text
  , requestHeaders :: Maybe (Map Text Text)
  , attachments :: Maybe [Text]
  -- ^ http(s) URLs from @attachment.url@ / @screenshot.url@ (string or string[]); the SDK hosts the files.
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson ATError
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ATError
  deriving (HI.DecodeValue, HI.EncodeValue) via HI.AsJsonb ATError


-- | Place an error by its @client.address@ when the SDK sent no @geo.*@ attributes.
enrichGeo :: GeoIP2.GeoDB -> ATError -> ATError
enrichGeo db e
  | isJust e.geoCountry = e
  | otherwise = maybe e place (e.userIp >>= readMaybe @IP . toString >>= rightToMaybe . GeoIP2.rawGeoData db)
  where
    place f = let (country, region, city) = geoFields f in e{geoCountry = country, geoRegion = region, geoCity = city}


-- | (country, region, city) from one MaxMind-format record. MaxMind databases nest
-- (@country.iso_code@, @subdivisions[0].iso_code@, @city.names.en@); IPinfo's are flat
-- (@country_code@, @region@, @city@) — reading both lets a city database drop in later.
--
-- >>> let m = GeoIP2.DataMap . Map.fromList . map (\(k, v) -> (GeoIP2.DataString k, v)); s = GeoIP2.DataString
-- >>> geoFields (m [("country", m [("iso_code", s "US")]), ("city", m [("names", m [("en", s "Santa Clara")])]), ("subdivisions", GeoIP2.DataArray [m [("iso_code", s "CA")]])])
-- (Just "US",Just "CA",Just "Santa Clara")
-- >>> geoFields (m [("country_code", s "NG"), ("region", s "Lagos"), ("city", s "Ikeja")])
-- (Just "NG",Just "Lagos",Just "Ikeja")
-- >>> geoFields (m [("asn", s "AS1")])
-- (Nothing,Nothing,Nothing)
geoFields :: GeoIP2.GeoField -> (Maybe Text, Maybe Text, Maybe Text)
geoFields f = (at ["country", "iso_code"] <|> at ["country_code"], subdivision <|> at ["region"], at ["city", "names", "en"] <|> at ["city"])
  where
    at = (`walk` f)
    walk [] (GeoIP2.DataString t) = Just t
    walk (k : ks) (GeoIP2.DataMap m) = walk ks =<< Map.lookup (GeoIP2.DataString k) m
    walk _ _ = Nothing
    subdivision = case f of
      GeoIP2.DataMap m | Just (GeoIP2.DataArray (sub1 : _)) <- Map.lookup (GeoIP2.DataString "subdivisions") m -> walk ["iso_code"] sub1
      _ -> Nothing


-- | Get error patterns for a project with optional state filter (excludes merged patterns)
getErrorPatterns :: DB es => Projects.ProjectId -> Maybe ErrorState -> Int -> Int -> Eff es [ErrorPattern]
getErrorPatterns pid mstate limit offset =
  Hasql.interp (selectFrom @ErrorPattern <> [HI.sql| WHERE project_id = #{pid} AND (#{mstate} IS NULL OR state = #{mstate}) AND canonical_id IS NULL ORDER BY updated_at DESC LIMIT #{limit} OFFSET #{offset} |])


getErrorPatternById :: DB es => ErrorPatternId -> Eff es (Maybe ErrorPattern)
getErrorPatternById eid = Hasql.interpOne (selectFrom @ErrorPattern <> [HI.sql| WHERE id = #{eid} |])


getErrorPatternByHash :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe ErrorPattern)
getErrorPatternByHash pid eHash = Hasql.interpOne (selectFrom @ErrorPattern <> [HI.sql| WHERE project_id = #{pid} AND hash = #{eHash} |])


-- | Read each sampled trace ID and its event timestamp from the same row version.
data ErrorTraceRefs = ErrorTraceRefs
  { firstTraceId :: Maybe Text
  , firstTraceAt :: Maybe UTCTime
  , recentTraceId :: Maybe Text
  , recentTraceAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


selectErrorTraceRefs :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe ErrorTraceRefs)
selectErrorTraceRefs pid h =
  Hasql.interpOne
    [HI.sql| SELECT first_trace_id, first_trace_at, recent_trace_id, recent_trace_at
             FROM apis.error_patterns WHERE project_id = #{pid} AND hash = #{h} LIMIT 1 |]


getErrorPatternLByHash :: DB es => Projects.ProjectId -> Text -> UTCTime -> Eff es (Maybe ErrorPatternL)
getErrorPatternLByHash pid eHash now =
  -- @e@ is the entity's explicit column list, not the table: @e.*@ then expands to exactly
  -- ErrorPattern's fields, so an ADD COLUMN can't shift the aggregates out of their slots.
  Hasql.interpOne
    $ [HI.sql|
    SELECT e.*, COALESCE(ev.occurrences, 0)::BIGINT, COALESCE(ev.user_count, 0)::BIGINT, ev.last_occurred_at
    FROM (|]
    <> selectFrom @ErrorPattern
    <> [HI.sql|) e LEFT JOIN LATERAL (
      SELECT SUM(event_count) AS occurrences, SUM(user_count) AS user_count, MAX(hour_bucket) AS last_occurred_at
      FROM apis.error_hourly_stats WHERE project_id = e.project_id AND error_id = e.id AND hour_bucket >= #{now}::timestamptz - INTERVAL '30 days'
    ) ev ON true WHERE e.project_id = #{pid} AND e.hash = #{eHash} |]


-- | Propagate merged counts for all given projects in a single query.
propagateMergedCountsBatch :: DB es => V.Vector Projects.ProjectId -> Eff es Int64
propagateMergedCountsBatch pids | V.null pids = pure 0
propagateMergedCountsBatch pids =
  Hasql.interpExecute
    [HI.sql|
    WITH snapshot AS (
      SELECT id, canonical_id, occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h
      FROM apis.error_patterns
      WHERE project_id = ANY(#{pids}) AND canonical_id IS NOT NULL
        AND (occurrences_1m > 0 OR occurrences_5m > 0 OR occurrences_1h > 0 OR occurrences_24h > 0)
    ),
    zeroed AS (
      UPDATE apis.error_patterns e SET occurrences_1m = 0, occurrences_5m = 0, occurrences_1h = 0, occurrences_24h = 0
      FROM snapshot s WHERE e.id = s.id
    )
    UPDATE apis.error_patterns c SET
      occurrences_1m = c.occurrences_1m + m.sum_1m,
      occurrences_5m = c.occurrences_5m + m.sum_5m,
      occurrences_1h = c.occurrences_1h + m.sum_1h,
      occurrences_24h = c.occurrences_24h + m.sum_24h
    FROM (SELECT canonical_id, SUM(occurrences_1m) as sum_1m, SUM(occurrences_5m) as sum_5m,
            SUM(occurrences_1h) as sum_1h, SUM(occurrences_24h) as sum_24h
          FROM snapshot GROUP BY canonical_id) m
    WHERE c.id = m.canonical_id AND c.project_id = ANY(#{pids}) |]


-- | Decay occurrence counts without inferring recovery from missing errors.
updateOccurrenceCountsBatch :: DB es => V.Vector Projects.ProjectId -> UTCTime -> Eff es Int64
updateOccurrenceCountsBatch pids _ | V.null pids = pure 0
updateOccurrenceCountsBatch pids now =
  Hasql.interpExecute
    [HI.sql|
      UPDATE apis.error_patterns SET
        occurrences_1m = 0,
        occurrences_5m = GREATEST(0, occurrences_5m - occurrences_1m),
        occurrences_1h = GREATEST(0, occurrences_1h - occurrences_5m),
        occurrences_24h = GREATEST(0, occurrences_24h - occurrences_1h),
        quiet_minutes = CASE WHEN occurrences_1m = 0 THEN quiet_minutes + 1 ELSE 0 END,
        state = CASE
          WHEN state = 'regressed' AND regressed_at IS NOT NULL AND #{now}::timestamptz - regressed_at >= INTERVAL '7 days' THEN 'ongoing'
          ELSE state
        END
      WHERE project_id = ANY(#{pids})
        AND (state != 'resolved' OR occurrences_24h > 0)
        -- Skip rows where every SET clause would be a no-op: counters all zero
        -- AND no pending state transition. Cuts ~95% of per-minute writes on
        -- quiet patterns and stops the TOAST relation from outrunning autovacuum.
        AND (
          occurrences_1m > 0 OR occurrences_5m > 0 OR occurrences_1h > 0 OR occurrences_24h > 0
          OR (state = 'regressed' AND regressed_at IS NOT NULL AND #{now}::timestamptz - regressed_at >= INTERVAL '7 days')
        )
    |]


-- | Move a pattern to a new state. Transitioning to 'ESResolved' also stamps @resolved_at@.
updateErrorPatternState :: DB es => ErrorPatternId -> ErrorState -> UTCTime -> Eff es Int64
updateErrorPatternState eid newState now =
  Hasql.interpExecute
    [HI.sql|
        UPDATE apis.error_patterns SET
          state = #{newState},
          resolved_at = CASE WHEN #{newState}::text = 'resolved' THEN #{now}::timestamptz ELSE resolved_at END,
          resolved_by = NULL,
          updated_at = #{now}
        WHERE id = #{eid} AND state IS DISTINCT FROM #{newState}::text
      |]


setErrorPatternAssignee :: DB es => ErrorPatternId -> Maybe Projects.UserId -> UTCTime -> Eff es Int64
setErrorPatternAssignee eid assigneeIdM now =
  Hasql.interpExecute
    [HI.sql|
        UPDATE apis.error_patterns SET
          assignee_id = #{assigneeIdM},
          assigned_at = CASE WHEN #{assigneeIdM} IS NOT NULL THEN #{now}::timestamptz ELSE NULL END,
          updated_at = #{now}
        WHERE id = #{eid}
      |]


updateErrorPatternAnalysis :: DB es => ErrorPatternId -> Text -> Text -> Eff es Int64
updateErrorPatternAnalysis eid rc eCat =
  Hasql.interpExecute [HI.sql| UPDATE apis.error_patterns SET root_cause = #{rc}, error_category = #{eCat} WHERE id = #{eid} |]


updateErrorPatternSubscription :: DB es => ErrorPatternId -> Bool -> Int -> UTCTime -> Eff es Int64
updateErrorPatternSubscription eid sub notifyMins now =
  Hasql.interpExecute
    [HI.sql|
        UPDATE apis.error_patterns SET
          subscribed = #{sub},
          notify_every_minutes = #{notifyMins},
          last_notified_at = CASE WHEN #{sub} AND NOT subscribed THEN NULL ELSE last_notified_at END,
          updated_at = #{now}
        WHERE id = #{eid}
      |]


-- | Whether a thread-id update also stamps @last_notified_at@. Use
-- 'KeepNotifiedAt' after 'claimDueErrorNotifications' has already stamped it
-- atomically to win the race.
data NotifiedStamp = StampNotifiedAt | KeepNotifiedAt
  deriving stock (Eq, Show)


updateErrorPatternThreadIds :: DB es => NotifiedStamp -> ErrorPatternId -> Maybe Text -> Maybe Text -> UTCTime -> Eff es Int64
updateErrorPatternThreadIds stamp eid slackTs discordMsgId now =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET
        slack_thread_ts = COALESCE(#{slackTs}, slack_thread_ts),
        discord_message_id = COALESCE(#{discordMsgId}, discord_message_id),
        last_notified_at = CASE WHEN #{stamp == StampNotifiedAt} THEN #{now} ELSE last_notified_at END,
        updated_at = #{now} WHERE id = #{eid} |]


-- | Bulk-update baselines for all active error patterns in a project using a single SQL CTE.
-- Reads pre-bucketed data from error_hourly_stats over the last 168 hours.
-- Note: the 'mad' CTE omits a direct JOIN back to error_patterns because it already joins 'stats',
-- which only contains non-resolved errors (filtered via state != 'resolved' in the stats CTE).
bulkCalculateAndUpdateBaselines :: DB es => Projects.ProjectId -> UTCTime -> Eff es Int64
bulkCalculateAndUpdateBaselines pid now =
  Hasql.interpExecute
    [HI.sql|
      WITH stats AS (
        SELECT ehs.error_id,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ehs.event_count) AS median_val,
          COUNT(*) AS total_hours
        FROM apis.error_hourly_stats ehs
        JOIN apis.error_patterns e ON e.id = ehs.error_id
        WHERE e.project_id = #{pid} AND e.state != 'resolved' AND ehs.hour_bucket >= #{now}::timestamptz - INTERVAL '168 hours'
        GROUP BY ehs.error_id
      ),
      mad AS (
        SELECT ehs.error_id,
          PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY ABS(ehs.event_count - s.median_val)) * 1.4826 AS mad_scaled
        FROM apis.error_hourly_stats ehs
        JOIN stats s ON s.error_id = ehs.error_id
        WHERE ehs.hour_bucket >= #{now}::timestamptz - INTERVAL '168 hours' AND ehs.project_id = #{pid}
        GROUP BY ehs.error_id
      )
      UPDATE apis.error_patterns SET
        baseline_error_rate_mean = s.median_val,
        baseline_error_rate_stddev = COALESCE(m.mad_scaled, 0),
        baseline_samples = s.total_hours::BIGINT,
        baseline_state = CASE WHEN s.total_hours >= 24 THEN 'established' ELSE 'learning' END,
        baseline_updated_at = #{now}
      FROM stats s LEFT JOIN mad m ON m.error_id = s.error_id
      WHERE apis.error_patterns.id = s.error_id AND apis.error_patterns.project_id = #{pid}
    |]


-- | An error pattern joined with its current-hour occurrence count (batch spike detection).
data ErrorPatternWithCurrentRate = ErrorPatternWithCurrentRate
  { errorId :: ErrorPatternId
  , projectId :: Projects.ProjectId
  , errorType :: Text
  , message :: Text
  , service :: Maybe Text
  , state :: ErrorState
  , baselineState :: BaselineState
  , baselineMean :: Maybe Double
  , baselineStddev :: Maybe Double
  , currentHourCount :: Int
  , errorData :: ATError
  , stacktrace :: Text
  , hash :: Text
  , parentHash :: Maybe Text
  , isFramework :: Bool
  , slackThreadTs :: Maybe Text
  , discordMessageId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


getErrorPatternsWithCurrentRates :: DB es => Projects.ProjectId -> UTCTime -> Eff es [ErrorPatternWithCurrentRate]
getErrorPatternsWithCurrentRates pid now =
  Hasql.interp
    [HI.sql|
        SELECT
          e.id, e.project_id, e.error_type, LEFT(e.message, 2000), e.service, e.state,
          e.baseline_state, e.baseline_error_rate_mean, e.baseline_error_rate_stddev,
          COALESCE(counts.event_count, 0)::BIGINT AS current_hour_count,
          e.error_data, LEFT(e.stacktrace, 8000), e.hash, e.parent_hash, e.is_framework, e.slack_thread_ts, e.discord_message_id
        FROM apis.error_patterns e
        LEFT JOIN apis.error_hourly_stats counts
          ON counts.error_id = e.id AND counts.project_id = e.project_id
          AND counts.hour_bucket = #{truncateHour now}
        -- Merged patterns are excluded for the same reason as in the notification
        -- sweep: they are no longer their own error, and spike detection mints
        -- issues and alerts of its own. 'propagateMergedCountsBatch' drains their
        -- counters to the canonical each minute anyway, so a merged row's rate is
        -- not a rate anyone should be alerted on.
        WHERE e.project_id = #{pid} AND e.state != 'resolved' AND NOT e.is_ignored
          AND e.canonical_id IS NULL
      |]


-- | Find an existing canonical error pattern with matching (project_id, service, error_type, message).
-- Used for fast pre-merge before issue creation to prevent notification spam from identical errors across different spans.
findCanonicalMatch :: DB es => Projects.ProjectId -> Maybe Text -> Text -> Text -> Eff es (Maybe ErrorPatternId)
findCanonicalMatch pid service eType msg =
  Hasql.interpOne
    [HI.sql| SELECT id FROM apis.error_patterns
        WHERE project_id = #{pid} AND service IS NOT DISTINCT FROM #{service}
          AND error_type = #{eType} AND message = #{msg}
          AND canonical_id IS NULL AND merge_override = FALSE
        ORDER BY created_at ASC LIMIT 1 |]


-- | What an upsert did to a single pattern row.
data UpsertOutcome = UOInserted | UORegressed | UOUnchanged
  deriving stock (Eq, Generic, Read, Show)
  deriving (HI.DecodeValue) via WrappedEnumSC 'Nothing "UO" UpsertOutcome


-- | Batch upsert error patterns using unnest arrays (single round-trip instead of N+1)
-- Groups by hash to avoid "ON CONFLICT DO UPDATE cannot affect row a second time" errors.
-- Returns hashes with their outcome, restricted to newly inserted or regressed patterns.
batchUpsertErrorPatterns :: DB es => Projects.ProjectId -> V.Vector ATError -> UTCTime -> Eff es [(Text, UpsertOutcome)]
batchUpsertErrorPatterns _pid errors _now | V.null errors = pure []
batchUpsertErrorPatterns pid errors now =
  filter ((/= UOUnchanged) . snd)
    <$> Hasql.interp
      [HI.sql| INSERT INTO apis.error_patterns (
            project_id, error_type, message, stacktrace, hash, parent_hash, shape_hash, is_framework,
            environment, service, runtime, error_data, first_release, last_release, last_release_at, last_release_since,
            first_trace_id, first_trace_at, recent_trace_id, recent_trace_at,
            occurrences_1m, occurrences_5m, occurrences_1h, occurrences_24h)
          SELECT #{pid}, u.error_type, u.message, u.stacktrace, u.hash, u.parent_hash, u.shape_hash, u.is_framework,
                 u.environment, u.service, u.runtime, u.error_data, u.release, u.release, CASE WHEN u.release IS NOT NULL THEN u.event_at END, CASE WHEN u.release IS NOT NULL THEN u.event_at END,
                 u.trace_id, CASE WHEN u.trace_id IS NOT NULL THEN u.event_at END,
                 u.trace_id, CASE WHEN u.trace_id IS NOT NULL THEN u.event_at END, u.cnt, u.cnt, u.cnt, u.cnt
          FROM (SELECT unnest(#{errorTypes}::text[]) AS error_type, unnest(#{messages}::text[]) AS message,
                       unnest(#{stacktraces}::text[]) AS stacktrace, unnest(#{hashes}::text[]) AS hash,
                       unnest(#{parentHashes}::text[]) AS parent_hash, unnest(#{shapeHashes}::text[]) AS shape_hash,
                       unnest(#{isFrameworks}::bool[]) AS is_framework,
                       unnest(#{environments}::text[]) AS environment, unnest(#{services}::text[]) AS service,
                       unnest(#{runtimes}::text[]) AS runtime, unnest(#{errorDatas}::jsonb[]) AS error_data, unnest(#{releases}::text[]) AS release,
                       unnest(#{traceIds}::text[]) AS trace_id, unnest(#{eventTimes}::timestamptz[]) AS event_at, unnest(#{counts}::bigint[]) AS cnt) u
          ON CONFLICT (project_id, hash) DO UPDATE SET
            updated_at = #{now},
            first_release = COALESCE(apis.error_patterns.first_release, EXCLUDED.first_release),
            -- Only a newer event moves the last release: batches arrive late and out of order.
            last_release = CASE WHEN ^{newerRelease} THEN EXCLUDED.last_release ELSE apis.error_patterns.last_release END,
            last_release_at = CASE WHEN ^{newerRelease} THEN EXCLUDED.last_release_at ELSE apis.error_patterns.last_release_at END,
            last_release_since = CASE WHEN ^{newerRelease} AND EXCLUDED.last_release IS DISTINCT FROM apis.error_patterns.last_release THEN EXCLUDED.last_release_at ELSE apis.error_patterns.last_release_since END,
            resolved_in_release = CASE WHEN ^{regresses} THEN NULL ELSE apis.error_patterns.resolved_in_release END,
            -- Toastable columns (message, error_data, parent_hash) are content-derived from hash
            -- and refreshed only on regression to avoid pg_toast bloat from per-occurrence rewrites.
            message = CASE WHEN ^{regresses} THEN EXCLUDED.message ELSE apis.error_patterns.message END,
            error_data = CASE WHEN ^{regresses} THEN EXCLUDED.error_data ELSE apis.error_patterns.error_data END,
            parent_hash = CASE WHEN ^{regresses} THEN EXCLUDED.parent_hash ELSE apis.error_patterns.parent_hash END,
            -- Write-once, which is how the 23,372 rows that predate the column acquire
            -- one: a row takes its shape the next time it occurs and never pays a
            -- rewrite after that. Unconditional assignment here would rewrite a column
            -- per occurrence on the hottest path in the product, which is the same
            -- heap/TOAST churn recent_trace_id is throttled to avoid.
            shape_hash = COALESCE(apis.error_patterns.shape_hash, EXCLUDED.shape_hash),
            -- Refresh recent_trace_id at most once every 5 minutes per pattern.
            -- Without this guard a busy pattern rewrites this column on every
            -- occurrence, breaking HOT and bloating the heap/TOAST relation.
            recent_trace_id = CASE
              WHEN EXCLUDED.recent_trace_id IS NOT NULL
                AND apis.error_patterns.updated_at < #{now}::timestamptz - INTERVAL '5 minutes'
                THEN EXCLUDED.recent_trace_id
              ELSE apis.error_patterns.recent_trace_id
            END,
            -- Moves in lockstep with recent_trace_id, under the identical condition.
            -- Readers pin a +/-5min window to this, so a timestamp that advanced while
            -- the id stood still (which is what updated_at does — it is written every
            -- occurrence, the id at most every five minutes) aims the lookup at a window
            -- the trace is not in. Same CASE, or the pair drifts again.
            recent_trace_at = CASE
              WHEN EXCLUDED.recent_trace_id IS NOT NULL
                AND apis.error_patterns.updated_at < #{now}::timestamptz - INTERVAL '5 minutes'
                THEN EXCLUDED.recent_trace_at
              ELSE apis.error_patterns.recent_trace_at
            END,
            first_trace_at = CASE WHEN apis.error_patterns.first_trace_id IS NULL
              THEN EXCLUDED.first_trace_at ELSE apis.error_patterns.first_trace_at END,
            first_trace_id = COALESCE(apis.error_patterns.first_trace_id, EXCLUDED.first_trace_id),
            is_framework = EXCLUDED.is_framework,
            occurrences_1m = apis.error_patterns.occurrences_1m + EXCLUDED.occurrences_1m,
            occurrences_5m = apis.error_patterns.occurrences_5m + EXCLUDED.occurrences_1m,
            occurrences_1h = apis.error_patterns.occurrences_1h + EXCLUDED.occurrences_1m,
            occurrences_24h = apis.error_patterns.occurrences_24h + EXCLUDED.occurrences_1m,
            quiet_minutes = CASE WHEN ^{regresses} THEN 0 ELSE apis.error_patterns.quiet_minutes END,
            state = CASE WHEN ^{regresses} THEN 'regressed' ELSE apis.error_patterns.state END,
            resolved_by = NULL,
            regressed_at = CASE WHEN ^{regresses} THEN #{now} ELSE apis.error_patterns.regressed_at END,
            regression_count = CASE WHEN ^{regresses}
                                    THEN apis.error_patterns.regression_count + 1
                                    ELSE apis.error_patterns.regression_count END
          RETURNING hash, CASE
            WHEN xmax = 0 THEN 'inserted'
            WHEN state = 'regressed' AND regressed_at = #{now} THEN 'regressed'
            ELSE 'unchanged' END::text |]
  where
    -- Group by hash: keep the latest occurrence (by event time, not batch order — a batch
    -- is not time-ordered) + sum count (avoids ON CONFLICT duplicate-row error).
    -- The bang keeps the running count forced: a hot hash otherwise accumulates one thunk per occurrence.
    (errs, counts) =
      V.unzip $ V.fromList $ HM.elems $ HM.fromListWith (\(a, n) (b, !k) -> (bool b a (a.when >= b.when), n + k)) [(e.hash, (e, 1 :: Int)) | e <- V.toList errors]
    errorTypes = V.map (.errorType) errs
    messages = V.map (.message) errs
    stacktraces = V.map (.stackTrace) errs
    hashes = V.map (.hash) errs
    parentHashes = V.map (.parentHash) errs
    shapeHashes = V.map (.shapeHash) errs
    isFrameworks = V.map (.isFramework) errs
    environments = V.map (.environment) errs
    services = V.map (.serviceName) errs
    runtimes = V.map (.runtime) errs
    errorDatas = V.map HI.AsJsonb errs
    -- '' would persist and pass every Maybe check downstream, sending the issue
    -- page off to fetch "trace ''" — every trace-less row in the window.
    traceIds = V.map (mfilter (not . T.null) . (.traceId)) errs
    eventTimes = V.map (.when) errs
    releases = V.map (.release) errs
    -- A resolved pattern regresses on its next occurrence, unless it was resolved "in
    -- next release" and the occurrence still reports that release.
    newerRelease = [HI.sql| EXCLUDED.last_release IS NOT NULL AND (apis.error_patterns.last_release_at IS NULL OR EXCLUDED.last_release_at >= apis.error_patterns.last_release_at) |]
    regresses =
      [HI.sql| apis.error_patterns.state = 'resolved'
               AND (apis.error_patterns.resolved_in_release IS NULL
                    OR EXCLUDED.last_release IS DISTINCT FROM apis.error_patterns.resolved_in_release) |]


-- | Record each error's affected users and bump @users_count@ by the ones not seen
-- before, in one statement. A user is keyed by the most specific OTel identity the
-- event carries: @user.id@, then @user.email@, then @client.address@.
upsertErrorPatternUsers :: DB es => Projects.ProjectId -> V.Vector ATError -> Eff es Int64
upsertErrorPatternUsers pid errs
  | V.null keyed = pure 0
  | otherwise =
      Hasql.interpExecute
        [HI.sql|
          WITH ins AS (
            INSERT INTO apis.error_pattern_users (project_id, error_id, user_key)
            SELECT DISTINCT e.project_id, e.id, u.user_key
            FROM (SELECT unnest(#{hashes}::text[]) AS hash, unnest(#{keys}::text[]) AS user_key) u
            JOIN apis.error_patterns e ON e.project_id = #{pid} AND e.hash = u.hash
            ON CONFLICT DO NOTHING
            RETURNING error_id)
          UPDATE apis.error_patterns p SET users_count = p.users_count + c.n
          FROM (SELECT error_id, count(*) AS n FROM ins GROUP BY error_id) c
          WHERE p.id = c.error_id |]
  where
    keyed = V.mapMaybe (\e -> (e.hash,) <$> userKey e) errs
    (hashes, keys) = V.unzip keyed


-- | The tags an error's distribution is rolled up over: a fixed set, so the rollup's
-- write cost is bounded per event.
--
-- >>> errorTags (def{browser = Just "Chrome", release = Just "1.0", handled = Just False} :: ATError)
-- [("browser","Chrome"),("release","1.0"),("handled","no")]
errorTags :: ATError -> [(Text, Text)]
errorTags e =
  [ (k, v)
  | (k, Just v) <-
      [ ("browser", e.browser)
      , ("os", e.os)
      , ("device", e.device)
      , ("release", e.release)
      , ("environment", e.environment)
      , ("country", e.geoCountry)
      , ("service", e.serviceName)
      , ("handled", bool "no" "yes" <$> e.handled)
      ]
  , not (T.null v)
  ]


-- | Add a batch's tag counts to the per-error rollup.
upsertErrorTagCounts :: DB es => Projects.ProjectId -> V.Vector ATError -> Eff es Int64
upsertErrorTagCounts pid errs
  | null counts = pure 0
  | otherwise =
      Hasql.interpExecute
        [HI.sql| INSERT INTO apis.error_tag_counts (project_id, error_id, tag_key, tag_value, count)
                 SELECT e.project_id, e.id, u.k, u.v, u.n
                 FROM (SELECT unnest(#{hashes}::text[]) AS hash, unnest(#{keys}::text[]) AS k, unnest(#{vals}::text[]) AS v, unnest(#{ns}::bigint[]) AS n) u
                 JOIN apis.error_patterns e ON e.project_id = #{pid} AND e.hash = u.hash
                 ON CONFLICT (project_id, error_id, tag_key, tag_value) DO UPDATE SET count = apis.error_tag_counts.count + EXCLUDED.count |]
  where
    counts = HM.toList $ HM.fromListWith (+) [((e.hash, k, v), 1 :: Int) | e <- V.toList errs, (k, v) <- errorTags e]
    (hashes, keys, vals, ns) = (V.fromList [h | ((h, _, _), _) <- counts], V.fromList [k | ((_, k, _), _) <- counts], V.fromList [v | ((_, _, v), _) <- counts], V.fromList [n | (_, n) <- counts])


-- | An error's tag distribution: per key, values by count, most common first.
selectErrorTagCounts :: DB es => ErrorPatternId -> Eff es [(Text, Text, Int)]
selectErrorTagCounts eid = Hasql.interp [HI.sql| SELECT tag_key, tag_value, count FROM apis.error_tag_counts WHERE error_id = #{eid} ORDER BY tag_key, count DESC |]


-- | The identity 'upsertErrorPatternUsers' counts a user by.
--
-- >>> userKey (def{userEmail = Just "a@b.c", userIp = Just "1.2.3.4"} :: ATError)
-- Just "email:a@b.c"
-- >>> userKey (def :: ATError)
-- Nothing
userKey :: ATError -> Maybe Text
userKey e = asum [("id:" <>) <$> e.userId, ("email:" <>) <$> e.userEmail, ("ip:" <>) <$> e.userIp]


-- | "Resolve in next release" pins the release the pattern is resolved in, so
-- occurrences still reporting it do not regress it; a plain resolve clears it.
setResolvedInRelease :: DB es => ErrorPatternId -> Bool -> Eff es Int64
setResolvedInRelease eid inNextRelease =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET resolved_in_release = CASE WHEN #{inNextRelease} THEN last_release END WHERE id = #{eid} |]


-- | Batch upsert hourly rollup stats. Takes (hash, event_count, user_count) triples and
-- resolves error_id via JOIN on apis.error_patterns(project_id, hash).
upsertErrorPatternHourlyStats :: DB es => Projects.ProjectId -> UTCTime -> V.Vector (Text, Int, Int) -> Eff es Int64
upsertErrorPatternHourlyStats _pid _now stats | V.null stats = pure 0
upsertErrorPatternHourlyStats pid now stats =
  Hasql.interpExecute
    [HI.sql|
          INSERT INTO apis.error_hourly_stats (project_id, error_id, hour_bucket, event_count, user_count)
          SELECT e.project_id, e.id, #{truncateHour now}, u.event_count, u.user_count
          FROM (SELECT unnest(#{hashes}::text[]) AS hash, unnest(#{eventCounts}::bigint[]) AS event_count, unnest(#{userCounts}::bigint[]) AS user_count) u
          JOIN apis.error_patterns e ON e.project_id = #{pid} AND e.hash = u.hash
          ON CONFLICT (project_id, error_id, hour_bucket)
          DO UPDATE SET event_count = apis.error_hourly_stats.event_count + EXCLUDED.event_count,
                        user_count = apis.error_hourly_stats.user_count + EXCLUDED.user_count |]
  where
    (hashes, eventCounts, userCounts) = V.unzip3 stats
