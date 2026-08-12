-- | Live Tail end to end, at the layers where it can actually be wrong.
--
--   * __Registration__ — the gates deciding whether a subscription exists at all: the service
--     requirement and the filter-only restriction. These are what keep Live Tail's volume
--     bounded and its semantics streamable, so each gets its own assertion rather than a
--     shared happy path.
--   * __Matching and delivery__ — subscriptions in the ingest cache, a batch pushed through
--     'LT.publishMatches', and an assertion about exactly which rows reached the hub. This is
--     the layer where a filter bug leaks another team's logs to a user, so the negative cases
--     carry more weight here than the positive one.
--   * __Backpressure__ — that the per-connection queue holds its bound and loses the oldest.
--
-- These drive 'LT.publishMatches' directly rather than going through OTLP: the hook in
-- @dualWriteWithPoisonMapping@ is one call, while everything worth asserting about — which
-- subscriptions match, what crosses the wire, what is dropped — lives on this side of it.
module LiveTailSpec (spec) where

import Control.Concurrent.STM (isEmptyTBQueue)
import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (AesonText (..), UUIDId (..))
import Pkg.LiveTail qualified as LT
import Pkg.TestUtils (TestResources, frozenTime, runHasqlEffect, withTestResources)
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldContain, shouldReturn, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


-- | A second project and two users, for the cross-tenant checks.
otherPid :: Projects.ProjectId
otherPid = UUIDId (UUID.fromWords 0 0 0 99)


ownerId, intruderId :: Projects.UserId
ownerId = Projects.UserId UUID.nil
intruderId = Projects.UserId (UUID.fromWords 0 0 0 98)


newSub :: Text -> LT.NewSubscription
newSub service = LT.NewSubscription (Just service) Nothing Nothing Nothing Nothing


spec :: Spec
spec = do
  -- Single-expression assertions about the pure helpers (compileQuery, transportFor, scopeFor,
  -- scopeFromRow) live as doctests on those functions; only what needs more than one
  -- expression, or needs state, is here.
  describe "LiveTail transport selection" do
    it "falls back to Postgres when Kafka is absent, so the feature has no optional dependency" do
      -- Kafka is the queue this system is built around but it is optional infrastructure; the
      -- relay table is not. There is no third answer and no "unavailable" — a deployment that
      -- cannot reach Kafka still gets a working tail rather than a dead toggle.
      LT.transportFor [] `shouldBe` LT.PostgresRelay
      LT.transportFor [""] `shouldBe` LT.PostgresRelay
      LT.transportFor ["broker:9092"] `shouldBe` LT.KafkaTopic LT.kafkaTopicName

  describe "LiveTail matching" do
    it "delivers a matching log and withholds everything else" do
      let sub = mkSub 1 "checkout" Nothing "level == \"error\""
      (conn, rt) <- fixture [sub]
      _ <-
        LT.publishMatches rt pid $
          V.fromList
            [ logRecord "checkout" "prod" "error" "boom" -- matches
            , logRecord "checkout" "prod" "info" "fine" -- wrong level
            , logRecord "billing" "prod" "error" "not yours" -- wrong service: must never appear
            , spanRecord "checkout" "prod" "error" -- not a log; Live Tail is logs only
            ]
      (rows, dropped) <- LT.takeBatch conn
      map logBody rows `shouldBe` ["boom"]
      dropped `shouldBe` 0

    it "matches every log for the selected service when no filter is given" do
      let sub = mkSub 2 "checkout" Nothing ""
      (conn, rt) <- fixture [sub]
      _ <- LT.publishMatches rt pid $ V.fromList [logRecord "checkout" "prod" "info" "a", logRecord "billing" "prod" "info" "b"]
      (rows, _) <- LT.takeBatch conn
      map logBody rows `shouldBe` ["a"]

    it "keeps the server's selectors out of the client query's reach" do
      -- The selectors are applied as resolved values, never spliced into the query text, so a
      -- filter that is true for every row still cannot escape its service or environment.
      let sub = mkSub 3 "checkout" (Just "prod") "level == \"error\" or level != \"error\""
      (conn, rt) <- fixture [sub]
      _ <-
        LT.publishMatches rt pid $
          V.fromList
            [ logRecord "checkout" "prod" "error" "prod row"
            , logRecord "checkout" "staging" "error" "staging row"
            , logRecord "billing" "prod" "error" "other service"
            ]
      (rows, _) <- LT.takeBatch conn
      map logBody rows `shouldBe` ["prod row"]

    it "fans one row out to every subscription that matches it" do
      -- Two people watching the same service with different filters both get the row.
      let a = mkSub 4 "checkout" Nothing ""
          b = mkSub 5 "checkout" Nothing "level == \"error\""
      hub <- LT.newHub
      connA <- LT.newConn 100
      connB <- LT.newConn 100
      _ <- LT.attachConn hub a.id connA
      _ <- LT.attachConn hub b.id connB
      rt <- runtimeWith hub [a, b]
      _ <- LT.publishMatches rt pid (V.fromList [logRecord "checkout" "prod" "error" "boom"])
      (rowsA, _) <- LT.takeBatch connA
      (rowsB, _) <- LT.takeBatch connB
      map logBody rowsA `shouldBe` ["boom"]
      map logBody rowsB `shouldBe` ["boom"]

    it "costs nothing for a project nobody is tailing" do
      hub <- LT.newHub
      rt <- runtimeWith hub []
      stats <- LT.publishMatches rt pid (V.fromList [logRecord "checkout" "prod" "error" "boom"])
      -- Not merely "no rows delivered" — the batch is never walked at all, which is the
      -- property that makes this safe to call from the ingestion hot path.
      getSum stats.evaluated `shouldBe` 0

    it "truncates an unbounded body and says that it did" do
      let sub = mkSub 6 "checkout" Nothing ""
      (conn, rt) <- fixture [sub]
      _ <- LT.publishMatches rt pid (V.fromList [logRecord "checkout" "prod" "info" (toText (replicate (LT.maxRowFieldChars + 500) 'x'))])
      (rows, _) <- LT.takeBatch conn
      map (\case LT.LogRow f -> f.truncated; _ -> False) rows `shouldBe` [True]
      map (T.length . logBody) rows `shouldBe` [LT.maxRowFieldChars]

  describe "Events live mode (AllSignals scope)" do
    it "delivers spans, which a logs-only subscription must never see" do
      -- The whole reason Events needs its own scope. One project can have both open at once,
      -- and the same span has to reach one and not the other.
      let events = mkSubScoped 20 LT.AllSignals ["kind", "name"] Nothing ""
          liveTail = mkSub 21 "checkout" Nothing ""
      hub <- LT.newHub
      eventsConn <- LT.newConn 100
      logsConn <- LT.newConn 100
      _ <- LT.attachConn hub events.id eventsConn
      _ <- LT.attachConn hub liveTail.id logsConn
      rt <- runtimeWith hub [events, liveTail]
      _ <- LT.publishMatches rt pid (V.fromList [spanRecord "checkout" "prod" "error"])
      (eventsRows, _) <- LT.takeBatch eventsConn
      length eventsRows `shouldBe` 1
      -- The logs-only queue never received it, so it is still empty.
      atomically (isEmptyTBQueue logsConn.queue) `shouldReturn` True

    it "matches across services, since Events has no service gate" do
      let sub = mkSubScoped 22 LT.AllSignals ["service"] Nothing ""
      (conn, rt) <- fixture [sub]
      _ <-
        LT.publishMatches rt pid
          $ V.fromList [logRecord "checkout" "prod" "info" "a", logRecord "billing" "prod" "info" "b"]
      (rows, _) <- LT.takeBatch conn
      length rows `shouldBe` 2

    it "projects the browser's columns, flattening ___ back to a path" do
      -- The column list arrives in the query layer's spelling; resolving it has to agree with
      -- how the same field is matched, or a row could filter in and render blank.
      let cols = ["timestamp", "kind", "resource___service___name", "attributes___http___request___method"]
          sub = mkSubScoped 23 LT.AllSignals cols Nothing ""
      (conn, rt) <- fixture [sub]
      _ <- LT.publishMatches rt pid (V.fromList [httpLogRecord "checkout" "prod" "GET"])
      (rows, _) <- LT.takeBatch conn
      case rows of
        [LT.TableRow m] -> do
          Map.lookup "resource___service___name" m `shouldBe` Just (AE.String "checkout")
          Map.lookup "attributes___http___request___method" m `shouldBe` Just (AE.String "GET")
          Map.lookup "kind" m `shouldBe` Just (AE.String "log")
        _ -> fail "expected exactly one TableRow"

    it "omits a column the in-memory record cannot answer rather than guessing" do
      -- Aggregates and SQL expressions have no in-memory equivalent. Leaving the key out lets
      -- the client render an empty cell until the durable read supplies the real value.
      let sub = mkSubScoped 24 LT.AllSignals ["kind", "count_"] Nothing ""
      (conn, rt) <- fixture [sub]
      _ <- LT.publishMatches rt pid (V.fromList [logRecord "checkout" "prod" "info" "a"])
      (rows, _) <- LT.takeBatch conn
      case rows of
        [LT.TableRow m] -> Map.member "count_" m `shouldBe` False
        _ -> fail "expected exactly one TableRow"

  describe "LiveTail Kafka envelope" do
    it "survives the round trip that crosses the side-topic, for both row shapes" do
      -- What actually goes over Kafka is this encoding, so a shape that cannot decode is a
      -- tail that silently delivers nothing. Both surfaces share the envelope, so both are
      -- pinned here rather than only the one that happened to be written first.
      let logEnv = LT.LiveEnvelope LT.envelopeVersion (mkSub 30 "checkout" Nothing "").id (LT.toLiveRow (LT.LogsOnly "checkout") [] (logRecord "checkout" "prod" "error" "boom"))
          tableEnv = LT.LiveEnvelope LT.envelopeVersion (mkSub 31 "checkout" Nothing "").id (LT.toLiveRow LT.AllSignals ["kind"] (logRecord "checkout" "prod" "error" "boom"))
      forM_ [logEnv, tableEnv] \env ->
        AE.eitherDecode (AE.encode env) `shouldSatisfy` \case
          Right (decoded :: LT.LiveEnvelope) -> decoded.v == env.v && decoded.subscriptionId == env.subscriptionId
          Left _ -> False

    it "routes a decoded envelope to the subscription it names, and to no other" do
      -- This is the web-pod half of the Kafka path: every pod consumes every message, so
      -- delivering to the wrong local queue would leak one user's rows into another's tail.
      hub <- LT.newHub
      mine <- LT.newConn 10
      theirs <- LT.newConn 10
      let a = mkSub 32 "checkout" Nothing ""
          b = mkSub 33 "billing" Nothing ""
      _ <- LT.attachConn hub a.id mine
      _ <- LT.attachConn hub b.id theirs
      let env = LT.LiveEnvelope LT.envelopeVersion a.id (LT.toLiveRow (LT.LogsOnly "checkout") [] (logRecord "checkout" "prod" "error" "boom"))
      -- Decode first, exactly as the consumer does, so the test exercises the wire form.
      case AE.eitherDecode (AE.encode env) of
        Left e -> fail ("envelope did not decode: " <> e)
        Right (decoded :: LT.LiveEnvelope) -> LT.deliver hub decoded
      (rows, _) <- LT.takeBatch mine
      map logBody rows `shouldBe` ["boom"]
      atomically (isEmptyTBQueue theirs.queue) `shouldReturn` True

  -- These need the database: the scoping and the caps are enforced in SQL, and asserting them
  -- anywhere else would be asserting a copy of the logic rather than the logic.
  -- These use wall-clock time, not 'frozenTime'. The lease predicate is `expires_at > now()`
  -- evaluated by *Postgres*, while the Time effect in tests runs on a TestClock pinned to 2025
  -- — so a frozenTime-relative expiry is already in the past to the database, every row reads
  -- as expired, and both the lookup and the cap silently degrade to no-ops.
  around withTestResources $ describe "LiveTail subscription storage" do
    it "scopes a lookup to its owner, so another project or user simply cannot find it" \tr -> do
      -- Every route that touches an existing subscription — stream, renew, delete — resolves it
      -- through 'activeSubscriptionFor'. Testing the three routes separately would be testing
      -- the same WHERE clause three times; testing it here tests all three.
      future <- addUTCTime 300 <$> getCurrentTime
      sid <-
        runHasqlEffect tr
          $ LT.insertSubscription pid ownerId (newSub "checkout") (LT.LogsOnly "checkout") 9 9 future
      theId <- maybe (fail "insert produced no id") pure sid
      found <- runHasqlEffect tr $ LT.activeSubscriptionFor pid ownerId theId
      fmap (.id) found `shouldBe` Just theId
      -- Wrong user, right project.
      otherUser <- runHasqlEffect tr $ LT.activeSubscriptionFor pid intruderId theId
      otherUser `shouldSatisfy` isNothing
      -- Right user, wrong project.
      otherProject <- runHasqlEffect tr $ LT.activeSubscriptionFor otherPid ownerId theId
      otherProject `shouldSatisfy` isNothing

    -- The cap is a predicate on the INSERT rather than a count before it, so there is no window
    -- between deciding and writing. (Two racing registrations can still each miss the other's
    -- uncommitted row — see the note on 'insertSubscription'; the cap is "about N".)
    it "refuses the insert once a cap is reached, in the same statement that writes" \tr -> do
      future <- addUTCTime 300 <$> getCurrentTime
      let register = runHasqlEffect tr $ LT.insertSubscription pid ownerId (newSub "checkout") (LT.LogsOnly "checkout") 2 9 future
      results <- replicateM 3 register
      -- Third one refused, and refused by the write itself rather than by a prior count.
      map isJust results `shouldBe` [True, True, False]

    it "carries a row from ingest to a browser without Kafka" \tr -> do
      -- The relay is the transport for every deployment that does not run Kafka — dev,
      -- docker-compose, self-hosted — so it is the common path, not the fallback. Nothing else
      -- exercises publish → drain → deliver end to end.
      let sub = mkSub 40 "checkout" Nothing ""
          env = LT.LiveEnvelope LT.envelopeVersion sub.id (LT.toLiveRow (LT.LogsOnly "checkout") [] (logRecord "checkout" "prod" "error" "relayed"))
      before <- runHasqlEffect tr LT.relayWatermark
      runHasqlEffect tr (LT.relayPublish [env])
      (results, next) <- runHasqlEffect tr (LT.relayDrain before)
      next `shouldSatisfy` (> before)
      map logBody [r | LT.Delivered e <- results, let r = e.row] `shouldContain` ["relayed"]

    it "reports a version skew on the relay as a skew, not as corruption" \tr -> do
      -- The Kafka path learned this the hard way: a payload from another envelope version fails
      -- inside the row parser, so unless the version is read *first* the skew is indistinguish-
      -- able from a corrupt row. Both transports must answer it the same way.
      let sub = mkSub 41 "checkout" Nothing ""
          stale = AE.object ["v" AE..= (LT.envelopeVersion - 1), "subscription_id" AE..= sub.id.toText, "row" AE..= AE.object []]
      LT.envelopeFromValue stale `shouldSatisfy` \case LT.VersionMismatch _ -> True; _ -> False
      LT.envelopeFromValue (AE.object ["nope" AE..= True]) `shouldSatisfy` \case LT.Undecodable _ -> True; _ -> False

    it "counts rows the relay buffer drops rather than losing them quietly" \_ -> do
      -- Overflow is expected under load; invisible overflow is not. Every other drop in this
      -- feature is counted, and this was the one that was not.
      buf <- LT.newRelayBuffer
      let env n = LT.LiveEnvelope LT.envelopeVersion (mkSub n "checkout" Nothing "").id (LT.toLiveRow (LT.LogsOnly "checkout") [] (logRecord "checkout" "prod" "info" (show n)))
      traverse_ (LT.bufferForRelay buf . env) [1 .. fromIntegral LT.relayBufferDepth + 5]
      (pending, lost) <- LT.takeRelayBuffer buf
      length pending `shouldBe` fromIntegral LT.relayBufferDepth
      lost `shouldBe` 5

    it "stops matching once the lease lapses, with no explicit delete" \tr -> do
      -- Expiry, not DELETE, is what deactivates a subscription: every failure that matters
      -- (crashed tab, killed pod, sleeping laptop) loses the DELETE.
      past <- addUTCTime (-60) <$> getCurrentTime
      expired <-
        runHasqlEffect tr
          $ LT.insertSubscription pid ownerId (newSub "checkout") (LT.LogsOnly "checkout") 9 9 past
      expired `shouldSatisfy` isJust
      active <- runHasqlEffect tr $ LT.activeSubscriptions 100
      map (.id) active `shouldSatisfy` notElem (fromMaybe (UUIDId UUID.nil) expired)

  describe "LiveTail backpressure" do
    it "holds the queue bound and drops the oldest, counting the loss" do
      let sub = mkSub 7 "checkout" Nothing ""
      hub <- LT.newHub
      conn <- LT.newConn 2 -- deliberately tiny
      _ <- LT.attachConn hub sub.id conn
      rt <- runtimeWith hub [sub]
      _ <- LT.publishMatches rt pid $ V.fromList [logRecord "checkout" "prod" "info" (show n) | n <- [1 :: Int .. 5]]
      (rows, dropped) <- LT.takeBatch conn
      -- What survives is the *newest*: a live view showing the oldest rows of a burst while
      -- discarding what is happening now would be worse than showing fewer rows.
      length rows `shouldBe` 2
      map logBody rows `shouldBe` ["4", "5"]
      dropped `shouldBe` 3

  describe "LiveTail cache" do
    it "drops a subscription whose stored filter no longer compiles, keeping the rest" do
      -- A deploy that tightens the parser must not take every other tail down with it.
      let good = mkSub 8 "checkout" Nothing "level == \"error\""
          bad = mkSub 9 "checkout" Nothing "level == \"error\" | summarize count()"
          (grouped, rejected) = LT.compileSubs [good, bad]
      map fst rejected `shouldBe` [bad.id]
      map (.sub.id) (fold (HM.elems grouped)) `shouldBe` [good.id]


-- | The body of a Live Tail row. Events rows are a different shape by construction, so a
-- mismatch here means a subscription produced the wrong projection entirely.
logBody :: LT.LiveRow -> Text
logBody = \case
  LT.LogRow f -> f.body
  LT.TableRow _ -> error "expected a LogRow from a logs-only subscription"


-- ---------------------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------------------

-- | One subscription, one attached connection, and the runtime an ingest pod would hold one
-- cache refresh after the browser registered.
fixture :: [LT.Subscription] -> IO (LT.Conn, LT.Runtime)
fixture subs = do
  hub <- LT.newHub
  conn <- LT.newConn 100
  forM_ subs \s -> void (LT.attachConn hub s.id conn)
  rt <- runtimeWith hub subs
  pure (conn, rt)


runtimeWith :: LT.Hub -> [LT.Subscription] -> IO LT.Runtime
runtimeWith hub subs = do
  cache <- LT.newSubCache
  _ <- LT.refreshSubCache cache frozenTime subs
  buf <- LT.newRelayBuffer
  pure LT.Runtime{transport = LT.PostgresRelay, cache, hub, relayBuffer = buf, emit = LT.deliver hub}


mkSub :: Word32 -> Text -> Maybe Text -> Text -> LT.Subscription
mkSub n service = mkSubScoped n (LT.LogsOnly service) []


mkSubScoped :: Word32 -> LT.Scope -> [Text] -> Maybe Text -> Text -> LT.Subscription
mkSubScoped n scope columns environment query =
  LT.Subscription
    { id = UUIDId (UUID.fromWords 0 0 0 n)
    , projectId = pid
    , userId = Projects.UserId UUID.nil
    , scope
    , environment
    , query
    , columns
    , expiresAt = addUTCTime 300 frozenTime
    }


logRecord :: Text -> Text -> Text -> Text -> Telemetry.OtelLogsAndSpans
logRecord service env level body =
  (baseRecord service env){Telemetry.kind = Just "log", Telemetry.level = Just level, Telemetry.body = Just (AesonText (AE.String body))}


-- | A log carrying nested http attributes, for the column-projection test.
httpLogRecord :: Text -> Text -> Text -> Telemetry.OtelLogsAndSpans
httpLogRecord service env method =
  (logRecord service env "info" "req")
    { Telemetry.attributes =
        Just (AesonText (Map.fromList [("http", AE.object ["request" AE..= AE.object ["method" AE..= method]])]))
    }


spanRecord :: Text -> Text -> Text -> Telemetry.OtelLogsAndSpans
spanRecord service env level = (baseRecord service env){Telemetry.kind = Just "server", Telemetry.level = Just level}


-- | Resource shaped the way the ingest path writes it: dot-notation attribute keys expanded
-- into nested objects, which is what the subject resolver has to walk.
baseRecord :: Text -> Text -> Telemetry.OtelLogsAndSpans
baseRecord service env =
  Telemetry.OtelLogsAndSpans
    { id = UUID.toText UUID.nil
    , project_id = pid.toText
    , timestamp = frozenTime
    , parent_id = Nothing
    , observed_timestamp = Nothing
    , hashes = Just V.empty
    , name = Just "handler"
    , kind = Just "log"
    , status_code = Nothing
    , status_message = Nothing
    , level = Nothing
    , severity = Nothing
    , body = Nothing
    , duration = Just 1
    , start_time = frozenTime
    , end_time = Nothing
    , context = Nothing
    , events = Nothing
    , links = Nothing
    , attributes = Nothing
    , resource =
        Just
          ( AesonText
              ( Map.fromList
                  [ ("service", AE.object ["name" AE..= service])
                  , ("deployment", AE.object ["environment" AE..= AE.object ["name" AE..= env]])
                  ]
              )
          )
    , summary = V.empty
    , date = frozenTime
    , errors = Nothing
    , message_size_bytes = 0
    }
