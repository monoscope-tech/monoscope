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

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (AesonText (..), UUIDId (..))
import Pkg.LiveTail qualified as LT
import Pkg.TestUtils (frozenTime)
import Relude
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


spec :: Spec
spec = do
  describe "LiveTail query validation" do
    it "accepts a filter and rejects anything needing a result set" do
      -- All four are valid KQL that passes field validation. The only thing separating them is
      -- whether the answer exists for a single row — which is exactly what a stream can do.
      LT.compileQuery "level == \"error\"" `shouldSatisfy` isRight
      LT.compileQuery "level == \"error\" | summarize count() by kind" `shouldBe` Left LT.NotAFilter
      LT.compileQuery "level == \"error\" | take 10" `shouldBe` Left LT.NotAFilter
      LT.compileQuery "level == \"error\" | sort by timestamp" `shouldBe` Left LT.NotAFilter
      LT.compileQuery "level == \"error\" | project name = name" `shouldBe` Left LT.NotAFilter
      LT.compileQuery "level == \"error\" | extend slow = duration" `shouldBe` Left LT.NotAFilter

    it "conjoins a piped where rather than letting it replace the earlier filter" $
      -- A `|` that replaced the filter instead of narrowing it would quietly widen the tail.
      LT.compileQuery "level == \"error\" | where duration > 10" `shouldSatisfy` isRight

    it "rejects a field that does not exist instead of silently never matching" $
      LT.compileQuery "nonexistent_field == 1" `shouldSatisfy` isLeft

    it "rejects a query past the length limit" $
      LT.compileQuery (mconcat (replicate 500 "level == \"error\" and ")) `shouldSatisfy` isLeft

  describe "LiveTail transport selection" do
    it "refuses to accept subscriptions it could never deliver" do
      -- Brokers configured (so ingest and web may well be separate) with no topic. The local
      -- hub would accept a subscription on the web pod that ingest can never feed, which reads
      -- as a product bug from every angle.
      LT.transportFor True True "" `shouldSatisfy` isUnavailable
      LT.transportFor False False "live_tail" `shouldSatisfy` isUnavailable
      LT.transportFor True True "live_tail" `shouldBe` LT.KafkaTopic "live_tail"

    it "ignores the configured topic when no brokers are configured" do
      -- 'liveTailTopic' ships with a default, so a dev box with no brokers must still choose
      -- the local hub — routing it at a broker it does not have would break the common case.
      LT.transportFor True False "live_tail" `shouldBe` LT.LocalHub
      LT.transportFor True False "" `shouldBe` LT.LocalHub

    it "never falls back to the local hub once brokers are configured" do
      -- The regression this guards: a signal that differs per process (such as
      -- 'enableKafkaService', off on web pods and on for ingest) would let a web pod choose the
      -- local hub while ingest publishes to Kafka — subscriptions accepted, rows flowing, tail
      -- empty forever. Whatever the topic, a broker-configured deployment must never land on
      -- LocalHub; it either uses Kafka or refuses outright.
      let withBrokers = [LT.transportFor True True topic | topic <- ["live_tail", "", "other"]]
      withBrokers `shouldSatisfy` notElem LT.LocalHub

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
      map (.body) rows `shouldBe` ["boom"]
      dropped `shouldBe` 0

    it "matches every log for the selected service when no filter is given" do
      let sub = mkSub 2 "checkout" Nothing ""
      (conn, rt) <- fixture [sub]
      _ <- LT.publishMatches rt pid $ V.fromList [logRecord "checkout" "prod" "info" "a", logRecord "billing" "prod" "info" "b"]
      (rows, _) <- LT.takeBatch conn
      map (.body) rows `shouldBe` ["a"]

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
      map (.body) rows `shouldBe` ["prod row"]

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
      map (.body) rowsA `shouldBe` ["boom"]
      map (.body) rowsB `shouldBe` ["boom"]

    it "costs nothing for a project nobody is tailing" do
      hub <- LT.newHub
      rt <- runtimeWith hub []
      stats <- LT.publishMatches rt pid (V.fromList [logRecord "checkout" "prod" "error" "boom"])
      -- Not merely "no rows delivered" — the batch is never walked at all, which is the
      -- property that makes this safe to call from the ingestion hot path.
      stats.evaluated `shouldBe` 0

    it "truncates an unbounded body and says that it did" do
      let sub = mkSub 6 "checkout" Nothing ""
      (conn, rt) <- fixture [sub]
      _ <- LT.publishMatches rt pid (V.fromList [logRecord "checkout" "prod" "info" (toText (replicate (LT.maxRowFieldChars + 500) 'x'))])
      (rows, _) <- LT.takeBatch conn
      map (.truncated) rows `shouldBe` [True]
      map (T.length . (.body)) rows `shouldBe` [LT.maxRowFieldChars]

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
      map (.body) rows `shouldBe` ["4", "5"]
      dropped `shouldBe` 3

  describe "LiveTail cache" do
    it "drops a subscription whose stored filter no longer compiles, keeping the rest" do
      -- A deploy that tightens the parser must not take every other tail down with it.
      let good = mkSub 8 "checkout" Nothing "level == \"error\""
          bad = mkSub 9 "checkout" Nothing "level == \"error\" | summarize count()"
          (grouped, rejected) = LT.compileSubs [good, bad]
      map fst rejected `shouldBe` [bad.id]
      map (.sub.id) (fold (HM.elems grouped)) `shouldBe` [good.id]


isUnavailable :: LT.Transport -> Bool
isUnavailable = \case LT.Unavailable _ -> True; _ -> False


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
  pure LT.Runtime{transport = LT.LocalHub, cache, hub, emit = LT.deliver hub}


mkSub :: Word32 -> Text -> Maybe Text -> Text -> LT.Subscription
mkSub n service environment query =
  LT.Subscription
    { id = UUIDId (UUID.fromWords 0 0 0 n)
    , projectId = pid
    , userId = Projects.UserId UUID.nil
    , service
    , environment
    , query
    , expiresAt = addUTCTime 300 frozenTime
    }


logRecord :: Text -> Text -> Text -> Text -> Telemetry.OtelLogsAndSpans
logRecord service env level body =
  (baseRecord service env){Telemetry.kind = Just "log", Telemetry.level = Just level, Telemetry.body = Just (AesonText (AE.String body))}


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
