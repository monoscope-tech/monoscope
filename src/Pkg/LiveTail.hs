-- | Live Tail: matching telemetry on the ingest pod and pushing it to a browser.
--
-- The Events tab answers "what happened?" by querying TimeFusion. Live Tail answers "what is
-- happening?", and a query cannot: the row a user is waiting for has not been written yet. So
-- the match happens on the ingest pod, on the in-memory record, before the durable write — and
-- the matched row is pushed sideways to whichever web pod holds that user's SSE connection.
--
-- == The shape of the problem
--
-- Ingest pods (@CONSUMER_ONLY@) and web pods are different processes. The browser talks to a
-- web pod; the telemetry arrives on an ingest pod. Three things bridge them:
--
-- 1. __A leased subscription row in Postgres.__ The web pod writes what the browser wants;
--    ingest pods poll unexpired rows into a 'SubCache'. This is the only thing ingest learns
--    from outside, and it is a lease so a lost DELETE cannot strand a matcher forever.
-- 2. __A Kafka side-topic.__ Ingest publishes only rows that already matched, keyed by
--    subscription id. Post-filter traffic is small by construction — a project nobody is
--    tailing produces nothing at all.
-- 3. __A local hub on each web pod.__ Every web pod consumes the whole side-topic under its
--    own consumer group (any pod may hold any connection) and routes by subscription id into
--    the bounded queue feeding that SSE response.
--
-- == What must never happen
--
-- Live Tail is a convenience feature living inside the ingestion path, which is not one. Every
-- failure here — cache stale, filter broken, Kafka down, browser slow — must degrade Live Tail
-- and leave the telemetry write untouched. So 'publishMatches' catches everything, never
-- blocks on a full queue, and treats an undecidable filter as a __non-match__: a filter that
-- failed open would show a user rows they never asked for, which is worse than showing none.
module Pkg.LiveTail (
  -- * Subscriptions
  Subscription (..),
  SubscriptionId,
  Scope (..),
  scopeFor,
  scopeToText,
  scopeFromRow,
  NewSubscription (..),
  RegisterError (..),
  registerErrorMessage,
  maxQueryLength,
  kafkaTopicName,
  cacheRefreshSecs,
  leaseSecs,
  queueCapacity,
  maxPerUser,
  maxPerProject,
  maxCached,
  compileQuery,
  effectiveFilter,

  -- * Storage
  insertSubscription,
  activeSubscriptions,
  activeSubscriptionFor,
  countActiveForUser,
  countActiveForProject,
  renewLease,
  deleteSubscription,
  deleteSubscriptionIO,
  reapExpiredSubscriptions,

  -- * Ingest side
  SubCache (..),
  CompiledSub (..),
  newSubCache,
  refreshSubCache,
  compileSubs,
  matchesFor,

  -- * Wire format
  LiveEnvelope (..),
  EnvelopeResult (..),
  decodeEnvelope,
  envelopeFromValue,
  LiveRow (..),
  LogRowFields (..),
  envelopeVersion,
  maxRowFieldChars,
  toLiveRow,

  -- * Web-pod side
  Hub,
  Conn (..),
  newHub,
  newConn,
  attachConn,
  deliver,
  takeBatch,
  takeBatchWithin,

  -- * Transport
  Transport (..),
  transportFor,
  relayPublish,
  relayDrain,
  relayWatermark,
  relayReap,

  -- * Runtime
  Runtime (..),
  RelayBuffer (..),
  newRelayBuffer,
  bufferForRelay,
  takeRelayBuffer,
  hubIsEmpty,
  relayBufferDepth,
  relayPollMs,
  relayRetentionSecs,
  PublishStats (..),
  publishMatches,
) where

import Control.Concurrent.STM (TBQueue, check, flushTBQueue, isFullTBQueue, newTBQueueIO, orElse, readTBQueue, registerDelay, swapTVar, writeTBQueue)
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import GHC.Generics (Generically (..))
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Pkg.DeriveUtils (AesonText (..), DB, UUIDId (..))
import Pkg.Parser.Eval (EvalError, Resolver, evalExpr, filterExpr, resolveIn)
import Pkg.Parser.Expr (Expr, FieldKey (..), Subject (..))
import Pkg.Parser.Stats (parseQueryToAST, validateFields)
import Relude
import System.IO.Unsafe (unsafePerformIO)


-- $setup
-- >>> :set -XOverloadedStrings


-- | Opaque handle for one browser's stream. Random, and __not__ an authorisation token: every
-- route re-checks project membership and ownership. It exists so the Kafka envelope and the
-- hub can address a stream without carrying user identity around.
type SubscriptionId = UUIDId "live_tail_subscription"


-- | Which signals a subscription watches, and — structurally — whether it is gated by a
-- service.
--
-- The two live surfaces bound their volume differently, and encoding that as a sum rather than
-- a @Bool@ plus a @Maybe Text@ is what stops the wrong combination existing at all. Live Tail
-- /cannot/ be constructed without a service; the Events tab has no service to give, and no
-- amount of handler code can accidentally drop the gate from the one that needs it.
data Scope
  = -- | Live Tail: one service, logs only. The service is the bound.
    LogsOnly Text
  | -- | The Events tab's live toggle: every signal, no service gate. Bounded instead by the
    -- per-connection queue — a tail matching more than a browser can take drops the oldest
    -- rows and reports the count, rather than being refused up front.
    AllSignals
  deriving stock (Eq, Generic, Show)


-- | Storage form. Kept narrow deliberately: the column is a discriminator, not a place to
-- smuggle more state.
scopeToText :: Scope -> (Text, Maybe Text)
scopeToText = \case
  LogsOnly svc -> ("logs_only", Just svc)
  AllSignals -> ("all_signals", Nothing)


-- | Rebuild a 'Scope' from its stored parts. A @logs_only@ row with no service is corrupt —
-- the schema cannot express "gated by nothing" — so it fails rather than silently widening
-- into a tail across every service.
--
-- >>> scopeFromRow "logs_only" (Just "checkout")
-- Just (LogsOnly "checkout")
-- >>> scopeFromRow "logs_only" Nothing
-- Nothing
-- >>> scopeFromRow "all_signals" Nothing
-- Just AllSignals
scopeFromRow :: Text -> Maybe Text -> Maybe Scope
scopeFromRow "logs_only" (Just svc) = Just (LogsOnly svc)
scopeFromRow "all_signals" _ = Just AllSignals
scopeFromRow _ _ = Nothing


-- | A lease, as stored. @expiresAt@ is authoritative — see the migration for why.
data Subscription = Subscription
  { id :: SubscriptionId
  , projectId :: Projects.ProjectId
  , userId :: Projects.UserId
  , scope :: Scope
  , environment :: Maybe Text
  , query :: Text
  , columns :: [Text]
  -- ^ The column names the browser is rendering, sent at registration.
  --
  -- The server cannot derive these: a query's @finalColumns@ may hold SQL expressions that
  -- only the database can evaluate. But the browser already knows its own column order, so it
  -- says so, and each name is resolved against the in-memory record by mapping @___@ back to
  -- @.@ — which covers @context___trace_id@, @resource___service___name@,
  -- @attributes___http___request___method@ and the plain columns alike. Anything that only
  -- SQL could have computed resolves to null in the live row and is filled in when the
  -- durable read replaces it.
  , expiresAt :: UTCTime
  }
  deriving stock (Generic, Show)


-- | What a browser asked for, after the handler authenticated it. Project and user are never
-- read from the request body, so they are not fields here.
--
-- @service@ is optional at the wire level and required by 'LogsOnly' at the type level; the
-- handler is what turns one into the other, and refuses when it cannot.
data NewSubscription = NewSubscription
  { service :: Maybe Text
  , environment :: Maybe Text
  , query :: Maybe Text
  , allSignals :: Maybe Bool
  -- ^ Set by the Events tab. Absent or false means Live Tail's logs-only, service-gated form.
  , columns :: Maybe [Text]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake NewSubscription


-- | Resolve the browser's requested scope, enforcing the gate that 'LogsOnly' requires.
--
-- >>> scopeFor (NewSubscription (Just "checkout") Nothing Nothing Nothing Nothing)
-- Right (LogsOnly "checkout")
-- >>> scopeFor (NewSubscription Nothing Nothing Nothing Nothing Nothing)
-- Left ServiceRequired
-- >>> scopeFor (NewSubscription Nothing Nothing Nothing (Just True) Nothing)
-- Right AllSignals
scopeFor :: NewSubscription -> Either RegisterError Scope
scopeFor ns
  | ns.allSignals == Just True = Right AllSignals
  | otherwise = maybe (Left ServiceRequired) (Right . LogsOnly) (mfilter (not . T.null) (T.strip <$> ns.service))


-- | Why a registration was refused. Typed rather than stringly so the handler maps each case
-- to a status code and the UI can react to the limit cases specifically.
data RegisterError
  = -- | The service gate. Live Tail's volume is bounded by this and nothing else.
    ServiceRequired
  | -- | @summarize@, @sort@, @take@, @project@, @extend@ or @source=@ — all need a result set,
    -- and a stream has none.
    NotAFilter
  | -- | Parser rejected the text, or it names a field that does not exist.
    BadQuery Text
  | QueryTooLong Int
  | TooManySubscriptionsForUser Int
  | TooManySubscriptionsForProject Int
  deriving stock (Eq, Show)


-- | User-facing text. Deliberately says nothing about parser internals or limits belonging to
-- other tenants.
registerErrorMessage :: RegisterError -> Text
registerErrorMessage = \case
  ServiceRequired -> "Select a service before starting the tail."
  NotAFilter -> "Live Tail streams individual logs, so it only accepts filters — remove summarize, sort, take, project and extend."
  BadQuery m -> m
  QueryTooLong n -> "Query is " <> show n <> " characters; the limit is " <> show maxQueryLength <> "."
  TooManySubscriptionsForUser n -> "You already have " <> show n <> " live tails open. Close one to start another."
  TooManySubscriptionsForProject n -> "This project already has " <> show n <> " live tails open."


maxQueryLength :: Int
maxQueryLength = 4000


-- ---------------------------------------------------------------------------------------
-- Tuning
-- ---------------------------------------------------------------------------------------

-- These were environment variables. They are constants now, because none of them is a
-- deployment decision: they are the shape of the feature, and an operator asked to choose a
-- lease length or a queue depth is being handed a question they have no way to answer. What a
-- deployment does vary — whether Kafka exists — is read from the broker list it already had.

-- | Kafka side-topic name. Create it with short retention; nothing here is replayable.
kafkaTopicName :: Text
kafkaTopicName = "live_tail"


-- | How often an ingest pod reloads unexpired subscriptions, and so also the worst-case delay
-- before a newly started tail begins matching.
cacheRefreshSecs :: Int
cacheRefreshSecs = 2


-- | Lease length. An open tab renews at a third of this; anything that stops renewing stops
-- matching within one period.
leaseSecs :: Int
leaseSecs = 45


-- | Rows buffered per browser connection before the oldest is dropped.
queueCapacity :: Natural
queueCapacity = 500


-- | Active tails per user, and per project. Courtesy bounds on open tabs — see
-- 'insertSubscription' for why they are approximate under concurrency.
maxPerUser, maxPerProject :: Int
maxPerUser = 3
maxPerProject = 20


-- | Hard cap on subscriptions one ingest pod will hold, independent of the per-user and
-- per-project bounds: those limit new registrations, this limits the pod even when the table
-- already holds more rows than a later, tighter limit would allow.
maxCached :: Int
maxCached = 500


-- | How often a web pod drains the relay table. Only runs while that pod holds at least one
-- SSE connection, so a pod nobody is tailing through costs nothing.
relayPollMs :: Int
relayPollMs = 250


-- | How long a relayed row survives. Long enough to cover a poll plus a slow pod, short enough
-- that the table never becomes storage.
relayRetentionSecs :: Int
relayRetentionSecs = 30


-- | Cap on the column list a browser may register.
--
-- The list is client-supplied and drives a per-row traversal on the ingest path, so it is
-- untrusted input on the hottest code in the system. A real Events table shows tens of
-- columns; anything beyond this is a client bug or an attempt to make ingest do unbounded
-- work per record.
maxColumns :: Int
maxColumns = 128


-- | Bound the registered column list. Truncates rather than refuses: an over-long list is
-- almost certainly a client bug, and dropping the tail still renders a usable table while
-- keeping the per-row cost bounded.
clampColumns :: [Text] -> [Text]
clampColumns = take maxColumns . filter (not . T.null)


-- | Parse and validate a user's filter text, rejecting anything Live Tail cannot stream.
--
-- Returns the AST rather than a matcher because ingest pods rebuild the compiled form on every
-- cache refresh, while this runs once on the web pod at registration. Both paths go through
-- here, so a query that registers is a query that will match.
--
-- >>> compileQuery "level == \"error\"" & isRight
-- True
--
-- Every pipeline command needs a result /set/, which a stream has none of. All of these parse
-- and validate cleanly — the only thing separating them from a filter is that their answer
-- cannot exist for a single row:
--
-- >>> compileQuery "level == \"error\" | summarize count() by kind"
-- Left NotAFilter
-- >>> compileQuery "level == \"error\" | take 10"
-- Left NotAFilter
-- >>> compileQuery "level == \"error\" | sort by timestamp"
-- Left NotAFilter
-- >>> compileQuery "level == \"error\" | project name = name"
-- Left NotAFilter
-- >>> compileQuery "level == \"error\" | extend slow = duration"
-- Left NotAFilter
--
-- A piped @where@ narrows rather than replaces — replacing would quietly widen the tail:
--
-- >>> compileQuery "level == \"error\" | where duration > 10" & isRight
-- True
--
-- >>> compileQuery "nonexistent_field == 1" & isLeft
-- True
-- >>> compileQuery "   "
-- Right Nothing
-- >>> compileQuery (mconcat (replicate 500 "level == \"error\" and ")) & isLeft
-- True
compileQuery :: Text -> Either RegisterError (Maybe Expr)
compileQuery raw
  | T.length raw > maxQueryLength = Left (QueryTooLong (T.length raw))
  | T.null (T.strip raw) = Right Nothing
  | otherwise = do
      sections <- first BadQuery (parseQueryToAST raw)
      first BadQuery (validateFields sections)
      maybe (Left NotAFilter) (Right . Just) (filterExpr sections)


-- | A subscription with its filter already parsed, as ingest pods hold it.
data CompiledSub = CompiledSub
  { sub :: Subscription
  , userFilter :: Maybe Expr
  }
  deriving stock (Generic)


-- | The predicate ingest actually evaluates: the server-imposed selectors AND the user filter.
--
-- Composed here, once, and never from client-supplied text. The service and environment are
-- compared as exact values against the resolved row rather than spliced into the query string,
-- so no amount of quoting in a user's filter can widen or replace them — a client that could
-- reach those predicates could tail another team's traffic.
effectiveFilter :: CompiledSub -> AE.Value -> Either EvalError Bool
effectiveFilter cs row
  | not serviceOk = Right False
  | not (maybe True (matches envSubject) cs.sub.environment) = Right False
  | otherwise = maybe (Right True) (evalExpr resolver) cs.userFilter
  where
    -- 'AllSignals' has no service predicate to apply — that is the whole difference between
    -- the two surfaces, and it lives here rather than in a nullable field the caller might
    -- forget to check.
    serviceOk = case cs.sub.scope of
      LogsOnly svc -> matches serviceSubject svc
      AllSignals -> True
    resolver :: Resolver
    resolver = resolveIn row
    matches subj expected = expected `elem` map asText (resolver subj)
    asText = \case AE.String s -> s; v -> decodeUtf8 (AE.encode v)


-- | @resource.service.name@ and @resource.deployment.environment.name@ as subjects, built
-- directly rather than parsed: these are ours, not the user's, and re-parsing a constant on
-- every batch would be work for nothing.
serviceSubject, envSubject :: Subject
serviceSubject = Subject "resource.service.name" "resource" [FieldKey "service", FieldKey "name"]
envSubject = Subject "resource.deployment.environment.name" "resource" [FieldKey "deployment", FieldKey "environment", FieldKey "name"]


-- | Every subscription this row matches.
--
-- Logs only: a batch may carry spans and metrics, and Live Tail is a log view. Filtering here
-- rather than at the call site keeps the "what is a log?" decision in one place.
--
-- An 'EvalError' resolves to a non-match, and is returned alongside so the caller can count
-- it. Failing closed matters: a broken filter that matched everything would stream a user rows
-- from services they never selected.
matchesFor :: [CompiledSub] -> Telemetry.OtelLogsAndSpans -> ([CompiledSub], [EvalError])
matchesFor subs rec = partitionEithers (mapMaybe decide subs)
  where
    row = AE.toJSON rec
    isLog = rec.kind == Just "log"
    -- The logs-only restriction is per subscription, not per batch: one project can have a
    -- Live Tail and an Events tail open at once, and the same span must reach the second while
    -- never reaching the first. Matched exhaustively rather than by guard + `otherwise`, so a
    -- third Scope inherits neither answer silently — it fails to compile until decided.
    decide cs = case cs.sub.scope of
      LogsOnly _ -> if isLog then verdict cs else Nothing
      AllSignals -> verdict cs

    verdict cs = case effectiveFilter cs row of
      Right True -> Just (Left cs)
      Right False -> Nothing
      Left e -> Just (Right e)


-- ---------------------------------------------------------------------------------------
-- Storage
-- ---------------------------------------------------------------------------------------

-- | Insert a lease. The caller has already authenticated the user and enforced the limits.
-- | Insert a lease, but only if both limits still hold.
--
-- One statement rather than count-check-insert across three round trips, which is what the
-- plan asked for and what the first version did not do: between a separate @count@ and its
-- @INSERT@, any number of other registrations can land, so the caps were advisory at best.
--
-- 'Nothing' means a limit was hit and nothing was written. The caller re-reads the counts only
-- then, purely to say /which/ limit — a slightly stale number in an error message costs
-- nothing, and keeps the success path to a single query.
--
-- __Honest bound:__ under @READ COMMITTED@ two registrations racing each other still cannot
-- see each other's uncommitted row, so the cap is "about N", not exactly N. That is deliberate
-- rather than overlooked: these limits are a courtesy bound on one user's tabs, and paying for
-- exactness (advisory locks, or @SERIALIZABLE@ with retries) on a registration path is not
-- worth it. The bound that actually protects the fleet is 'liveTailMaxCached', which is a
-- @LIMIT@ on the ingest side and cannot be raced at all.
insertSubscription
  :: DB es
  => Projects.ProjectId
  -> Projects.UserId
  -> NewSubscription
  -> Scope
  -> Int
  -- ^ max active per user
  -> Int
  -- ^ max active per project
  -> UTCTime
  -> Eff es (Maybe SubscriptionId)
insertSubscription pid uid ns scope perUser perProject expiresAt =
  Hasql.interpOne
    [HI.sql|
      INSERT INTO projects.live_tail_subscriptions (project_id, user_id, scope, service, environment, query, columns, expires_at)
      SELECT #{pid}, #{uid}, #{scopeText}, #{svc}, #{ns.environment}, #{fromMaybe "" ns.query}, #{cols}, #{expiresAt}
      WHERE (SELECT count(*) FROM projects.live_tail_subscriptions
             WHERE project_id = #{pid} AND user_id = #{uid} AND expires_at > now()) < #{perUser}
        AND (SELECT count(*) FROM projects.live_tail_subscriptions
             WHERE project_id = #{pid} AND expires_at > now()) < #{perProject}
      RETURNING id
    |]
  where
    (scopeText, svc) = scopeToText scope
    cols = V.fromList (clampColumns (fromMaybe [] ns.columns))


-- | Every unexpired subscription, for the ingest cache refresh.
--
-- Bounded by @limit@ so a runaway or a stale limit change cannot make an ingest pod hold
-- unbounded state; the caller logs when the bound bites.
activeSubscriptions :: DB es => Int -> Eff es [Subscription]
activeSubscriptions limit =
  mapMaybe fromRow
    <$> Hasql.interp
      [HI.sql|
        SELECT id, project_id, user_id, scope, service, environment, query, columns, expires_at
        FROM projects.live_tail_subscriptions
        WHERE expires_at > now()
        ORDER BY created_at
        LIMIT #{limit}
      |]


-- | The stored row, before 'Scope' is reassembled from its discriminator and service.
type SubRow = (SubscriptionId, Projects.ProjectId, Projects.UserId, Text, Maybe Text, Maybe Text, Text, V.Vector Text, UTCTime)


-- | Rebuild a 'Subscription', dropping any row whose scope cannot be reconstructed.
--
-- Dropping rather than defaulting: the only way this fails is a @logs_only@ row with no
-- service, and the safe reading of a missing gate is "do not match", never "match everything".
fromRow :: SubRow -> Maybe Subscription
fromRow (sid, pid, uid, scopeText, svc, environment, query, columns, expiresAt) = do
  scope <- scopeFromRow scopeText svc
  pure Subscription{id = sid, projectId = pid, userId = uid, scope, environment, query, columns = V.toList columns, expiresAt}


-- | One unexpired subscription, scoped to its owner. Every stream and delete route goes
-- through this, so a subscription id from another project or another user simply is not found.
activeSubscriptionFor :: DB es => Projects.ProjectId -> Projects.UserId -> SubscriptionId -> Eff es (Maybe Subscription)
activeSubscriptionFor pid uid sid =
  listToMaybe
    . mapMaybe fromRow
    <$> Hasql.interp
      [HI.sql|
        SELECT id, project_id, user_id, scope, service, environment, query, columns, expires_at
        FROM projects.live_tail_subscriptions
        WHERE id = #{sid} AND project_id = #{pid} AND user_id = #{uid} AND expires_at > now()
      |]


countActiveForUser :: DB es => Projects.ProjectId -> Projects.UserId -> Eff es Int
countActiveForUser pid uid =
  fromMaybe 0
    . listToMaybe
    <$> Hasql.interp
      [HI.sql| SELECT count(*)::int FROM projects.live_tail_subscriptions WHERE project_id = #{pid} AND user_id = #{uid} AND expires_at > now() |]


countActiveForProject :: DB es => Projects.ProjectId -> Eff es Int
countActiveForProject pid =
  fromMaybe 0
    . listToMaybe
    <$> Hasql.interp
      [HI.sql| SELECT count(*)::int FROM projects.live_tail_subscriptions WHERE project_id = #{pid} AND expires_at > now() |]


-- | Push the lease out. Returns the new expiry, or 'Nothing' if the lease had already lapsed —
-- a stream whose renewal finds nothing must close rather than resurrect itself, otherwise a
-- subscription the ingest cache has already forgotten would linger as a silent connection.
renewLease :: DB es => SubscriptionId -> UTCTime -> Eff es (Maybe UTCTime)
renewLease sid newExpiry =
  listToMaybe
    <$> Hasql.interp
      [HI.sql|
        UPDATE projects.live_tail_subscriptions SET expires_at = #{newExpiry}
        WHERE id = #{sid} AND expires_at > now()
        RETURNING expires_at
      |]


-- | Idempotent: deleting an already-reaped subscription is success, so disconnect cleanup and
-- the reaper never race into a spurious error.
deleteSubscription :: DB es => Projects.ProjectId -> Projects.UserId -> SubscriptionId -> Eff es ()
deleteSubscription pid uid sid =
  Hasql.interp
    [HI.sql| DELETE FROM projects.live_tail_subscriptions WHERE id = #{sid} AND project_id = #{pid} AND user_id = #{uid} |]


-- ---------------------------------------------------------------------------------------
-- Relay transport
-- ---------------------------------------------------------------------------------------

-- | Hand matched rows to the relay table. Batched: a burst is one insert, not one per row.
relayPublish :: DB es => [LiveEnvelope] -> Eff es ()
relayPublish [] = pass
relayPublish envs =
  Hasql.interp
    [HI.sql|
      INSERT INTO projects.live_tail_events (subscription_id, payload)
      SELECT * FROM unnest(#{V.fromList (map (.subscriptionId) envs)}, #{V.fromList (map (AesonText . AE.toJSON) envs)})
    |]


-- | Everything written after @lastSeen@, with the new watermark.
--
-- Ordered by the sequence rather than by time so a clock skew between writers cannot make the
-- consumer skip rows, and bounded so one slow poll cannot pull an unbounded batch into memory.
relayDrain :: DB es => Int64 -> Eff es ([EnvelopeResult], Int64)
relayDrain lastSeen = do
  rows :: [(Int64, AesonText AE.Value)] <-
    Hasql.interp
      [HI.sql|
        SELECT id, payload FROM projects.live_tail_events
        WHERE id > #{lastSeen} ORDER BY id LIMIT 500
      |]
  -- Every row goes through the same version check the Kafka path uses, so a rolling-deploy
  -- skew is reported as a skew on both transports rather than looking like corruption on one
  -- of them. The watermark advances past undecodable rows so they are not retried forever.
  let decoded = map (\(_, AesonText v) -> envelopeFromValue v) rows
      watermark = maybe lastSeen fst (viaNonEmpty Relude.last rows)
  pure (decoded, watermark)


-- | Where a pod starts reading: the current end of the table.
--
-- Latest, not earliest — the same choice the Kafka consumer makes and for the same reason. A
-- row written before this pod had a connection open is a row already missed; replaying it
-- would show a "live" tail rows from minutes ago.
relayWatermark :: DB es => Eff es Int64
relayWatermark = fromMaybe 0 . listToMaybe <$> Hasql.interp [HI.sql| SELECT coalesce(max(id), 0) FROM projects.live_tail_events |]


-- | Drop relayed rows past their usefulness. Unlike the subscription reaper this one is
-- load-bearing: without it the table grows for as long as anyone is tailing.
relayReap :: DB es => Eff es ()
relayReap =
  Hasql.interp
    [HI.sql| DELETE FROM projects.live_tail_events WHERE created_at < now() - make_interval(secs => #{relayRetentionSecs}) |]


-- | Delete a subscription from raw 'IO', for the one caller that has no effect stack: the SSE
-- streaming body.
--
-- The server observes a dropped connection the instant the response body fails, which is
-- strictly better information than waiting out the lease. Without this, a browser that crashed
-- (or slept, or lost the network) keeps ingest matching and publishing for its full lease —
-- rows nobody will ever read, produced to Kafka or inserted into the relay.
--
-- Safe against reconnects: the client registers a /new/ subscription when it reconnects (see
-- `LiveStream.start`, which stops before it starts), so the id torn down here is always one
-- nothing will come back to.
deleteSubscriptionIO :: OHasql.TracedPool -> Projects.ProjectId -> Projects.UserId -> SubscriptionId -> IO ()
deleteSubscriptionIO hpool pid uid sid =
  Safe.handleAny (const pass)
    $ runEff
    $ Hasql.runHasqlPool hpool
    $ deleteSubscription pid uid sid


-- | Reclaim space only. Expiry — not this — is what makes a subscription inactive, so running
-- it late, or not at all, changes nothing a user can observe.
reapExpiredSubscriptions :: DB es => Eff es ()
reapExpiredSubscriptions =
  Hasql.interp [HI.sql| DELETE FROM projects.live_tail_subscriptions WHERE expires_at < now() - interval '1 hour' |]


-- ---------------------------------------------------------------------------------------
-- Ingest-side cache
-- ---------------------------------------------------------------------------------------

-- | Ingest's view of what browsers are watching, grouped by project so a project nobody is
-- tailing costs exactly one lookup on the hot path.
--
-- @refreshedAt@ is kept so the ingest side can report cache age: a cache that has silently
-- stopped refreshing looks identical to "nobody is tailing", and those must not be confused.
data SubCache = SubCache
  { subs :: IORef (HM.HashMap Projects.ProjectId [CompiledSub])
  , refreshedAt :: IORef (Maybe UTCTime)
  }
  deriving stock (Generic)


newSubCache :: MonadIO m => m SubCache
newSubCache = SubCache <$> newIORef HM.empty <*> newIORef Nothing


-- | Depth of the relay write buffer. One tick's worth of a busy tail; past this the oldest
-- rows are dropped, which is the same bargain the browser queue makes.
relayBufferDepth :: Natural
relayBufferDepth = 4096


-- | Parse stored filters and group by project, dropping rows that no longer compile.
--
-- A stored query can stop parsing across a deploy that tightens the parser. Dropping that one
-- subscription (and reporting it) is the only safe answer: keeping it unparsed would mean
-- guessing, and failing the whole refresh would take every other tail down with it.
compileSubs :: [Subscription] -> (HM.HashMap Projects.ProjectId [CompiledSub], [(SubscriptionId, RegisterError)])
compileSubs rows = (HM.fromListWith (<>) [(cs.sub.projectId, [cs]) | cs <- oks], bad)
  where
    (bad, oks) = partitionEithers (map compile1 rows)
    compile1 s = bimap (s.id,) (CompiledSub s) (compileQuery s.query)


-- | Replace the cache contents. Returns the subscriptions that failed to compile so the caller
-- can count them.
refreshSubCache :: MonadIO m => SubCache -> UTCTime -> [Subscription] -> m [(SubscriptionId, RegisterError)]
refreshSubCache cache now rows = do
  let (grouped, bad) = compileSubs rows
  writeIORef cache.subs grouped
  writeIORef cache.refreshedAt (Just now)
  pure bad


-- ---------------------------------------------------------------------------------------
-- Wire format
-- ---------------------------------------------------------------------------------------

-- | Bumped whenever 'LiveRow' changes shape. Ingest and web pods roll separately, so a
-- consumer will briefly see envelopes from the other version and must be able to say so
-- rather than mis-decode them.
--
-- 2: 'LiveRow' became a tagged sum ('LogRow' \/ 'TableRow') when the Events tab joined the
-- push path. A v1 payload is a bare log record with no @shape@ discriminator, so it cannot be
-- read as a v2 row at all — which is exactly why 'decodeEnvelope' reads this field before it
-- looks at the row.
envelopeVersion :: Int
envelopeVersion = 2


-- | Per-field character cap. A single log carrying a megabyte stack trace must not be able to
-- exceed the broker's message limit or stall a browser; the row says when it truncated.
maxRowFieldChars :: Int
maxRowFieldChars = 8000


-- | One log, reduced to what the Live Tail list renders. Deliberately not the whole
-- 'Telemetry.OtelLogsAndSpans': the browser opens the full record from the durable store when
-- the user expands a row, by which time the write has long landed.
data LogRowFields = LogRowFields
  { id :: Text
  , timestamp :: UTCTime
  , level :: Maybe Text
  , service :: Maybe Text
  , traceId :: Maybe Text
  , spanId :: Maybe Text
  , name :: Maybe Text
  , body :: Text
  , truncated :: Bool
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake LogRowFields


-- | One matched record, shaped for whichever surface asked for it.
--
-- Two shapes rather than one because the two surfaces render genuinely different things, and
-- collapsing them would mean the Live Tail list carrying columns it never shows while the
-- Events table hunts for fields inside a fixed projection. The 'Scope' that selected the
-- subscription is what selects the shape, so they cannot be mismatched.
data LiveRow
  = -- | Live Tail's fixed, compact projection.
    LogRow LogRowFields
  | -- | The Events table: column name → value, for exactly the columns the browser said it
    -- was rendering. A name the in-memory record cannot answer is absent rather than guessed,
    -- and the client leaves that cell empty until the durable read fills it.
    TableRow (Map Text AE.Value)
  deriving stock (Generic, Show)


-- Tagged so a consumer can tell the two apart without inferring it from the payload's shape.
instance AE.ToJSON LiveRow where
  toJSON = \case
    LogRow f -> AE.object ["shape" AE..= ("log" :: Text), "log" AE..= f]
    TableRow cols -> AE.object ["shape" AE..= ("table" :: Text), "cols" AE..= cols]


instance AE.FromJSON LiveRow where
  parseJSON = AE.withObject "LiveRow" \o ->
    o AE..: "shape" >>= \case
      ("log" :: Text) -> LogRow <$> o AE..: "log"
      "table" -> TableRow <$> o AE..: "cols"
      other -> fail ("unknown live row shape: " <> toString other)


-- | What crosses Kafka: one matched row addressed to one subscription.
data LiveEnvelope = LiveEnvelope
  { v :: Int
  , subscriptionId :: SubscriptionId
  , row :: LiveRow
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake LiveEnvelope


-- | What reading a message off the side-topic produced.
--
-- Three outcomes rather than @Maybe@, because they call for three different responses and
-- collapsing them is what made the previous version dangerous: a version skew during a rolling
-- deploy and a genuinely corrupt payload both arrived as "failed to decode" and were both
-- dropped in silence.
data EnvelopeResult
  = -- | Current version, decoded.
    Delivered LiveEnvelope
  | -- | A pod on the other side of a rolling deploy wrote this. Expected, transient, and
    -- someone else's to deliver — but counted, because a version that never stops appearing
    -- is a deploy that never finished.
    VersionMismatch Int
  | -- | Neither ours nor a version we recognise. Always worth a human's attention.
    Undecodable Text
  deriving stock (Show)


-- | Read a side-topic message, checking the version /before/ the row.
--
-- The order is the whole point. 'LiveRow' is a tagged sum, so a payload from an older
-- 'envelopeVersion' fails inside the row parser — if the envelope were decoded in one step,
-- that failure would surface as "corrupt" and the version field written precisely to explain
-- it would never be read.
--
-- >>> decodeEnvelope "{\"v\":1,\"subscription_id\":\"x\",\"row\":{}}"
-- VersionMismatch 1
-- >>> decodeEnvelope "not json"
-- Undecodable "not a live-tail envelope"
decodeEnvelope :: ByteString -> EnvelopeResult
decodeEnvelope = maybe (Undecodable "not a live-tail envelope") envelopeFromValue . AE.decodeStrict'


-- | The version-before-row rule, on an already-parsed value.
--
-- Shared with the relay so both transports answer a skew the same way. When this existed only
-- inside 'decodeEnvelope', the relay quietly grew its own @parseMaybe@ and lost the check
-- entirely — the version field was written on every row and read on none of them.
envelopeFromValue :: AE.Value -> EnvelopeResult
envelopeFromValue value = case AET.parseMaybe (AE.withObject "LiveEnvelope" (AE..: "v")) value of
  Nothing -> Undecodable "not a live-tail envelope"
  Just v
    | v /= envelopeVersion -> VersionMismatch v
    | otherwise -> either (Undecodable . toText) Delivered (AET.parseEither AE.parseJSON value)


-- | Project a record into the shape this subscription's surface renders.
toLiveRow :: Scope -> [Text] -> Telemetry.OtelLogsAndSpans -> LiveRow
toLiveRow scope cols r = case scope of
  LogsOnly _ -> LogRow (toLogRowFields r)
  AllSignals -> TableRow (toTableCols cols r)


-- | Resolve each column the browser is rendering against the in-memory record.
--
-- Column names arrive in the query layer's spelling, where a nested path is flattened with
-- @___@ (@resource___service___name@). Mapping that back to @.@ and reusing the filter
-- evaluator's resolver means one traversal rule serves both matching and projection, so a
-- field that filters correctly also renders correctly.
--
-- Columns SQL would have computed (aggregates, expressions) resolve to nothing and are simply
-- omitted; the row carries what it can prove and the durable read supplies the rest.
toTableCols :: [Text] -> Telemetry.OtelLogsAndSpans -> Map Text AE.Value
toTableCols cols r = fromList [(c, v) | c <- cols, Just v <- [resolveCol c]]
  where
    json = AE.toJSON r
    resolveCol c = case resolveIn json (dottedSubject (T.replace "___" "." c)) of
      [] -> Nothing
      v : _ -> Just (truncateValue v)
    truncateValue = \case
      AE.String t | T.length t > maxRowFieldChars -> AE.String (T.take maxRowFieldChars t)
      v -> v


-- | A 'Subject' for a dotted path, built directly rather than parsed — these come from a
-- column list the server already validated, not from user text.
dottedSubject :: Text -> Subject
dottedSubject path = case T.splitOn "." path of
  root : rest -> Subject path root (map FieldKey rest)
  [] -> Subject path path []


toLogRowFields :: Telemetry.OtelLogsAndSpans -> LogRowFields
toLogRowFields r =
  LogRowFields
    { id = r.id
    , timestamp = r.timestamp
    , level = r.level
    , service = lookupText serviceSubject
    , traceId = r.context >>= (.trace_id) >>= nonEmptyText
    , spanId = r.context >>= (.span_id) >>= nonEmptyText
    , name = r.name
    , body = bodyText
    , truncated = T.length rawBody > maxRowFieldChars
    }
  where
    json = AE.toJSON r
    lookupText subj = viaNonEmpty Relude.head (mapMaybe asText (resolveIn json subj))
    asText = \case AE.String s -> Just s; AE.Null -> Nothing; v -> Just (decodeUtf8 (AE.encode v))
    nonEmptyText t = t <$ guard (not (T.null t))
    rawBody = maybe "" (\(AesonText b) -> case b of AE.String s -> s; v -> decodeUtf8 (AE.encode v)) r.body
    bodyText = T.take maxRowFieldChars rawBody


-- ---------------------------------------------------------------------------------------
-- Web-pod hub
-- ---------------------------------------------------------------------------------------

-- | One browser's bounded mailbox.
--
-- @dropped@ is cumulative and monotonic: the UI shows a total, not a rate, because "you have
-- missed 4,812 logs, narrow your filter" is actionable during an incident and "dropping now"
-- is not.
data Conn = Conn
  { queue :: TBQueue LiveRow
  , dropped :: TVar Int
  }
  deriving stock (Generic)


-- | Subscription id → the connections on /this/ pod watching it.
--
-- A list, not a single 'Conn': a user can have the same tail open in two tabs, and a reconnect
-- can briefly overlap with the connection it replaces.
type Hub = TVar (HM.HashMap SubscriptionId [(Unique, Conn)])


-- | Identity for a connection within a subscription, so detaching removes the right one when
-- two tabs share a subscription id.
newtype Unique = Unique Int
  deriving newtype (Eq, Show)


newHub :: MonadIO m => m Hub
newHub = newTVarIO HM.empty


newConn :: MonadIO m => Natural -> m Conn
newConn cap = Conn <$> liftIO (newTBQueueIO cap) <*> newTVarIO 0


-- | Register a connection and hand back its detach action.
--
-- The caller is expected to pair these with @bracket@; returning the exact detach rather than
-- letting the caller reconstruct it is what stops a mismatched key from leaking a queue for
-- the lifetime of the pod.
attachConn :: MonadIO m => Hub -> SubscriptionId -> Conn -> m (IO ())
attachConn hub sid conn = atomically do
  n <- readTVar counter
  writeTVar counter (n + 1)
  let key = Unique n
  modifyTVar' hub (HM.insertWith (<>) sid [(key, conn)])
  pure (detachConn hub sid key)


detachConn :: MonadIO m => Hub -> SubscriptionId -> Unique -> m ()
detachConn hub sid key =
  atomically $ modifyTVar' hub $ HM.update (nonEmptyList . filter ((/= key) . fst)) sid
  where
    nonEmptyList xs = xs <$ guard (not (null xs))


-- | Monotonic source for 'Unique'. A global is right here: the value means nothing outside the
-- process and only has to be distinct within it.
counter :: TVar Int
counter = unsafePerformIO (newTVarIO 0)
{-# NOINLINE counter #-}


-- | Queue a matched row for the relay writer. Never blocks: on a full buffer the oldest row
-- is discarded, because a slow database must cost a tail its rows and never cost ingestion its
-- latency.
-- | Matched rows waiting to be written to the relay table, and what overflow has cost.
--
-- The counter is not decoration. Every other drop in this feature is counted — the per-browser
-- queue reports its own, publish failures land in 'PublishStats' — and a relay buffer
-- overflowing under load was the one place rows disappeared with no signal anywhere.
data RelayBuffer = RelayBuffer
  { queue :: TBQueue LiveEnvelope
  , dropped :: TVar Int
  }
  deriving stock (Generic)


newRelayBuffer :: MonadIO m => m RelayBuffer
newRelayBuffer = RelayBuffer <$> liftIO (newTBQueueIO relayBufferDepth) <*> newTVarIO 0


-- | Queue a matched row for the relay writer. Never blocks: on a full buffer the oldest row is
-- discarded and counted, because a slow database must cost a tail its rows and never cost
-- ingestion its latency.
bufferForRelay :: RelayBuffer -> LiveEnvelope -> IO ()
bufferForRelay rb e = atomically do
  full <- isFullTBQueue rb.queue
  when full do
    void (readTBQueue rb.queue)
    modifyTVar' rb.dropped (+ 1)
  writeTBQueue rb.queue e


-- | Everything buffered since the last call, with the number of rows overflow discarded in
-- that window. Both are reset by the read, so the caller reports a window rather than a total.
takeRelayBuffer :: MonadIO m => RelayBuffer -> m ([LiveEnvelope], Int)
takeRelayBuffer rb = atomically do
  pending <- flushTBQueue rb.queue
  lost <- swapTVar rb.dropped 0
  pure (pending, lost)


-- | Whether this pod has any live connection at all. The relay poller checks it first so a pod
-- nobody is tailing through never queries.
hubIsEmpty :: MonadIO m => Hub -> m Bool
hubIsEmpty = fmap HM.null . readTVarIO


-- | Hand a row to every local connection watching this subscription, dropping the oldest row
-- from any queue that is full.
--
-- Never blocks and never retries. A browser that cannot keep up is the browser's problem to
-- report; it must not become the ingest path's problem, and a bounded queue with a visible
-- loss counter is the only version of that which stays honest.
deliver :: MonadIO m => Hub -> LiveEnvelope -> m ()
deliver hub env = atomically do
  conns <- HM.lookupDefault [] env.subscriptionId <$> readTVar hub
  forM_ conns \(_, c) -> do
    full <- isFullTBQueue c.queue
    when full do
      void (readTBQueue c.queue)
      modifyTVar' c.dropped (+ 1)
    writeTBQueue c.queue env.row


-- | Block for the next row, then take whatever else has arrived, with the running drop count.
--
-- Batching matters: a burst that arrives as 200 rows should become a handful of SSE writes
-- rather than 200 syscalls, and the browser renders one frame either way.
takeBatch :: MonadIO m => Conn -> m ([LiveRow], Int)
takeBatch conn = atomically (batchSTM conn)


-- | 'takeBatch', but giving up after @micros@ with 'Nothing'.
--
-- The timeout is what makes heartbeats possible: a tail on a quiet service produces no rows
-- for minutes at a time, and a proxy that has seen no bytes will close the connection. Without
-- this the stream would look identical whether it was idle or dead.
takeBatchWithin :: MonadIO m => Int -> Conn -> m (Maybe ([LiveRow], Int))
takeBatchWithin micros conn = do
  timer <- liftIO (registerDelay micros)
  atomically $ (Just <$> batchSTM conn) `orElse` (Nothing <$ (readTVar timer >>= check))


batchSTM :: Conn -> STM ([LiveRow], Int)
batchSTM conn = do
  hd <- readTBQueue conn.queue
  rest <- flushTBQueue conn.queue
  n <- readTVar conn.dropped
  pure (hd : rest, n)


-- ---------------------------------------------------------------------------------------
-- Transport
-- ---------------------------------------------------------------------------------------

-- | How a matched row reaches the web pod holding the browser's connection.
--
-- There is no "unavailable" case and no in-process option, and both absences are deliberate.
--
-- Kafka is the queue this system is built around, but it is optional infrastructure — dev,
-- docker-compose and self-hosted installs routinely run without it. Postgres is not optional,
-- so it is the floor: Live Tail works with no new dependency anywhere, and Kafka is an
-- optimisation for fleets that already run it.
--
-- The in-process hub is gone because choosing it requires knowing whether ingest and HTTP are
-- the same process, and no process can tell — a web pod cannot see whether separate
-- @CONSUMER_ONLY@ pods exist. Picking it on the available evidence (no brokers) was silently
-- wrong for every split deployment without Kafka. Guessing was the bug.
data Transport
  = -- | The side-topic, when brokers are configured. Cheaper than the relay under load and the
    -- natural fit for a fleet already running Kafka for ingest.
    KafkaTopic Text
  | -- | The @live_tail_events@ relay table. Works everywhere, including single-process.
    PostgresRelay
  deriving stock (Eq, Show)


-- | Pick the transport. The only input is whether Kafka is reachable.
--
-- >>> transportFor ["broker:9092"]
-- KafkaTopic "live_tail"
-- >>> transportFor []
-- PostgresRelay
-- >>> transportFor [""]
-- PostgresRelay
transportFor :: [Text] -> Transport
transportFor brokers
  | all T.null brokers = PostgresRelay
  | otherwise = KafkaTopic kafkaTopicName


-- ---------------------------------------------------------------------------------------
-- Runtime
-- ---------------------------------------------------------------------------------------

-- | Everything Live Tail needs at runtime, assembled once at startup and carried on
-- 'System.Config.AuthContext'.
--
-- @emit@ is a plain callback rather than a 'Transport' this module interprets, which is what
-- keeps Kafka out of here: startup installs either the local 'deliver' or a producer call, and
-- 'publishMatches' — the function on the ingest hot path — never learns which.
data Runtime = Runtime
  { transport :: Transport
  , cache :: SubCache
  , hub :: Hub
  , relayBuffer :: RelayBuffer
  , emit :: LiveEnvelope -> IO ()
  }
  deriving stock (Generic)


-- | What one batch did, for metrics. Counted rather than logged: these are rates, and a
-- per-row log on the ingestion path is a cost, not a diagnostic.
-- Fields are 'Sum' so the monoid derives: summing counters is exactly what 'Generically'
-- gives for a product of monoids, and a hand-written instance is four more places to forget a
-- field when one is added.
data PublishStats = PublishStats
  { evaluated :: !(Sum Int)
  , matched :: !(Sum Int)
  , failed :: !(Sum Int)
  -- ^ Rows whose filter could not be decided. Treated as non-matches.
  , publishFailed :: !(Sum Int)
  -- ^ Matched rows the transport refused. Counted rather than thrown, because a broker that
  -- is down must not fail a telemetry write — but a silently swallowed exception would make
  -- "nobody is tailing" and "delivery is broken" indistinguishable.
  }
  deriving stock (Generic, Show)
  deriving (Monoid, Semigroup) via Generically PublishStats


-- | Match one project's freshly-decoded records against its active tails and emit what hits.
--
-- Called from the ingest path with the durable write still ahead of it, so the contract is
-- narrow: return quickly, never throw, never block. The cheap exit — a project nobody is
-- tailing — is one 'HM.lookup' returning an empty list, which is the overwhelmingly common
-- case and the only one whose cost matters.
--
-- Exceptions from @emit@ are swallowed on purpose. A broker that is down must not turn into a
-- failed telemetry write; the publish-failure counter is how that surfaces instead.
publishMatches :: MonadIO m => Runtime -> Projects.ProjectId -> V.Vector Telemetry.OtelLogsAndSpans -> m PublishStats
publishMatches rt pid records = do
  subs <- HM.lookupDefault [] pid <$> readIORef rt.cache.subs
  if null subs
    then pure mempty
    else liftIO $ foldlM (step subs) mempty records
  where
    step subs acc rec = do
      let (hits, errs) = matchesFor subs rec
      sent <- foldMapM emitOne hits
      pure (acc <> PublishStats 1 (Sum (length hits)) (Sum (length errs)) (Sum (length hits) - sent))
      where
        emitOne cs =
          Safe.handleAny (const (pure (Sum 0)))
            $ Sum 1
            <$ rt.emit (LiveEnvelope envelopeVersion cs.sub.id (toLiveRow cs.sub.scope cs.sub.columns rec))
