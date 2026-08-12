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
  NewSubscription (..),
  RegisterError (..),
  registerErrorMessage,
  maxQueryLength,
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
  LiveRow (..),
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

  -- * Runtime
  Runtime (..),
  PublishStats (..),
  publishMatches,
) where

import Control.Concurrent.STM (TBQueue, check, flushTBQueue, isFullTBQueue, newTBQueueIO, orElse, readTBQueue, registerDelay, writeTBQueue)
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
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


-- | A lease, as stored. @expiresAt@ is authoritative — see the migration for why.
data Subscription = Subscription
  { id :: SubscriptionId
  , projectId :: Projects.ProjectId
  , userId :: Projects.UserId
  , service :: Text
  , environment :: Maybe Text
  , query :: Text
  , expiresAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | What a browser asked for, after the handler authenticated it. Project and user are never
-- read from the request body, so they are not fields here.
data NewSubscription = NewSubscription
  { service :: Text
  , environment :: Maybe Text
  , query :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake NewSubscription


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


-- | Parse and validate a user's filter text, rejecting anything Live Tail cannot stream.
--
-- Returns the AST rather than a matcher because ingest pods rebuild the compiled form on every
-- cache refresh, while this runs once on the web pod at registration. Both paths go through
-- here, so a query that registers is a query that will match.
--
-- >>> compileQuery "level == \"error\"" & isRight
-- True
-- >>> compileQuery "level == \"error\" | summarize count() by kind"
-- Left NotAFilter
-- >>> compileQuery "nonexistent_field == 1" & isLeft
-- True
-- >>> compileQuery "   "
-- Right Nothing
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
  | not (matches serviceSubject cs.sub.service) = Right False
  | not (maybe True (matches envSubject) cs.sub.environment) = Right False
  | otherwise = maybe (Right True) (evalExpr resolver) cs.userFilter
  where
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
matchesFor subs rec
  | rec.kind /= Just "log" = ([], [])
  | otherwise = partitionEithers (mapMaybe decide subs)
  where
    row = AE.toJSON rec
    decide cs = case effectiveFilter cs row of
      Right True -> Just (Left cs)
      Right False -> Nothing
      Left e -> Just (Right e)


-- ---------------------------------------------------------------------------------------
-- Storage
-- ---------------------------------------------------------------------------------------

-- | Insert a lease. The caller has already authenticated the user and enforced the limits.
insertSubscription :: DB es => Projects.ProjectId -> Projects.UserId -> NewSubscription -> UTCTime -> Eff es SubscriptionId
insertSubscription pid uid ns expiresAt =
  Hasql.interp
    [HI.sql|
      INSERT INTO projects.live_tail_subscriptions (project_id, user_id, service, environment, query, expires_at)
      VALUES (#{pid}, #{uid}, #{ns.service}, #{ns.environment}, #{fromMaybe "" ns.query}, #{expiresAt})
      RETURNING id
    |]
    <&> maybe (error "insertSubscription: RETURNING produced no row") Relude.id
    . listToMaybe


-- | Every unexpired subscription, for the ingest cache refresh.
--
-- Bounded by @limit@ so a runaway or a stale limit change cannot make an ingest pod hold
-- unbounded state; the caller logs when the bound bites.
activeSubscriptions :: DB es => Int -> Eff es [Subscription]
activeSubscriptions limit =
  Hasql.interp
    [HI.sql|
      SELECT id, project_id, user_id, service, environment, query, expires_at
      FROM projects.live_tail_subscriptions
      WHERE expires_at > now()
      ORDER BY created_at
      LIMIT #{limit}
    |]


-- | One unexpired subscription, scoped to its owner. Every stream and delete route goes
-- through this, so a subscription id from another project or another user simply is not found.
activeSubscriptionFor :: DB es => Projects.ProjectId -> Projects.UserId -> SubscriptionId -> Eff es (Maybe Subscription)
activeSubscriptionFor pid uid sid =
  listToMaybe
    <$> Hasql.interp
      [HI.sql|
        SELECT id, project_id, user_id, service, environment, query, expires_at
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
envelopeVersion :: Int
envelopeVersion = 1


-- | Per-field character cap. A single log carrying a megabyte stack trace must not be able to
-- exceed the broker's message limit or stall a browser; the row says when it truncated.
maxRowFieldChars :: Int
maxRowFieldChars = 8000


-- | One log, reduced to what the Live Tail list renders. Deliberately not the whole
-- 'Telemetry.OtelLogsAndSpans': the browser opens the full record from the durable store when
-- the user expands a row, by which time the write has long landed.
data LiveRow = LiveRow
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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake LiveRow


-- | What crosses Kafka: one matched row addressed to one subscription.
data LiveEnvelope = LiveEnvelope
  { v :: Int
  , subscriptionId :: SubscriptionId
  , row :: LiveRow
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake LiveEnvelope


-- | Project a record down to the streamed row, truncating anything unbounded.
toLiveRow :: Telemetry.OtelLogsAndSpans -> LiveRow
toLiveRow r =
  LiveRow
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
-- Exactly one applies to a deployment, chosen by 'transportFor'. There is no "both": a
-- single process publishing to Kafka /and/ its own hub would deliver every row twice.
data Transport
  = -- | Ingest and HTTP in one process. Correct for dev, docker-compose and single-container
    -- self-hosting; silently wrong for anything else, which is why 'transportFor' will not
    -- pick it for a split deployment.
    LocalHub
  | -- | The named side-topic. The only transport that works when ingest and web are separate
    -- pods.
    KafkaTopic Text
  | -- | Nothing can deliver, so nothing should be accepted. Registration is refused and the
    -- UI says the feature is unavailable rather than opening a stream that stays empty
    -- forever — an accepted subscription that can never produce a row is the one outcome
    -- that looks like a product bug from every angle.
    Unavailable Text
  deriving stock (Eq, Show)


-- | Pick the transport from configuration.
--
-- The deciding signal is __whether Kafka brokers are configured at all__, deliberately not
-- @enableKafkaService@. That flag is per-process: in a split deployment the web pods run with
-- it off and the ingest pods with it on, so reading it would have each side pick a different
-- transport — web pods serving from a local hub nobody publishes to while ingest publishes to
-- a topic nobody consumes. Subscriptions would register, rows would flow, and the tail would
-- stay empty forever. Broker configuration is the one input both roles share.
--
-- >>> transportFor True True "live_tail"
-- KafkaTopic "live_tail"
--
-- No brokers means nothing to publish through, so everything is in one process and the local
-- hub is both available and correct — the configured topic is simply irrelevant there.
--
-- >>> transportFor True False "live_tail"
-- LocalHub
-- >>> transportFor True False ""
-- LocalHub
--
-- Brokers but no topic is the case worth naming: ingest and web may well be separate, and the
-- local hub would accept subscriptions on a web pod that ingest can never feed.
--
-- >>> transportFor True True ""
-- Unavailable "Live Tail needs LIVE_TAIL_TOPIC set when Kafka is configured."
-- >>> transportFor False True "live_tail"
-- Unavailable "Live Tail is turned off for this deployment."
--
-- A single process /with/ brokers configured round-trips its own rows through Kafka rather
-- than short-cutting to the hub. That is a little wasteful and entirely correct: it publishes
-- and consumes under its own group, and it keeps this decision independent of process role.
transportFor
  :: Bool
  -- ^ 'System.Config.enableLiveTail'
  -> Bool
  -- ^ are any 'System.Config.kafkaBrokers' configured?
  -> Text
  -- ^ 'System.Config.liveTailTopic'
  -> Transport
transportFor enabled hasBrokers topic
  | not enabled = Unavailable "Live Tail is turned off for this deployment."
  | not hasBrokers = LocalHub
  | not (T.null topic) = KafkaTopic topic
  | otherwise = Unavailable "Live Tail needs LIVE_TAIL_TOPIC set when Kafka is configured."


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
  , emit :: LiveEnvelope -> IO ()
  }
  deriving stock (Generic)


-- | What one batch did, for metrics. Counted rather than logged: these are rates, and a
-- per-row log on the ingestion path is a cost, not a diagnostic.
data PublishStats = PublishStats
  { evaluated :: !Int
  , matched :: !Int
  , failed :: !Int
  -- ^ Rows whose filter could not be decided. Treated as non-matches.
  , publishFailed :: !Int
  -- ^ Matched rows the transport refused. Counted rather than thrown, because a broker that
  -- is down must not fail a telemetry write — but a silently swallowed exception would make
  -- "nobody is tailing" and "delivery is broken" indistinguishable.
  }
  deriving stock (Generic, Show)


instance Semigroup PublishStats where
  a <> b =
    PublishStats
      (a.evaluated + b.evaluated)
      (a.matched + b.matched)
      (a.failed + b.failed)
      (a.publishFailed + b.publishFailed)


instance Monoid PublishStats where
  mempty = PublishStats 0 0 0 0


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
      sent <-
        if null hits
          then pure 0
          else do
            let liveRow = toLiveRow rec
            getSum . fold <$> forM hits \cs ->
              Safe.handleAny (const (pure (Sum 0))) (Sum 1 <$ rt.emit (LiveEnvelope envelopeVersion cs.sub.id liveRow))
      pure (acc <> PublishStats 1 (length hits) (length errs) (length hits - sent))
