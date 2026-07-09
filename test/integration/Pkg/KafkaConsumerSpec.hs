module Pkg.KafkaConsumerSpec (spec) where

import Control.Concurrent.STM (check, stateTVar)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (runErrorNoCallStack)
import Kafka.Consumer qualified as K
import Kafka.Effectful.Consumer.Effect (KafkaConsumer (..))
import Kafka.Effectful.Producer.Effect (KafkaProducer (..))
import Kafka.Producer qualified as KP
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.Queue qualified as Queue
import Pkg.TestUtils
import Relude
import System.Timeout (timeout)
import System.Types (ATBackgroundCtx, runBackground)
import Test.Hspec
import UnliftIO.Async (race_)
import UnliftIO.Concurrent (forkIO, threadDelay)


-- | An in-memory Kafka shared by the producer and consumer interpreters: each
-- topic is an append-only log (offset = index). The mock producer enqueues to a
-- @pending@ buffer (modelling librdkafka's async send queue) and only
-- 'FlushProducer' makes those records durable on their topic log — so the
-- consumer's flush-before-commit gate is exercised end-to-end: a DLQ publish is
-- visible on the dead-letter topic only once the committer has flushed. One
-- TVar, so producer→consumer round-trips behave like a real broker.
data Broker = Broker
  { logs :: Map Text [K.ConsumerRecord (Maybe ByteString) (Maybe ByteString)]
  , pending :: [(Text, Maybe ByteString, K.Headers)] -- produced, awaiting FlushProducer
  , cursor :: Map (Text, Int) Int -- next index the consumer reads per (topic, partition)
  , committed :: Map (Text, Int) Int64
  }


emptyBroker :: Broker
emptyBroker = Broker mempty mempty mempty mempty


-- Append a record to a topic's log, assigning the next sequential offset.
appendRecord :: Text -> Maybe ByteString -> K.Headers -> Broker -> Broker
appendRecord topic val hdrs b =
  let existing = Map.findWithDefault [] topic b.logs
      off = fromIntegral (length existing)
      cr = K.ConsumerRecord (K.TopicName topic) (K.PartitionId 0) (K.Offset off) K.NoTimestamp hdrs Nothing val
   in b{logs = Map.insert topic (existing <> [cr]) b.logs}


runMockProducer :: IOE :> es => TVar Broker -> Eff (KafkaProducer ': es) a -> Eff es a
runMockProducer bVar = interpret \_ -> \case
  ProduceMessage KP.ProducerRecord{KP.prTopic = topic, KP.prValue = val, KP.prHeaders = hdrs} ->
    atomically $ modifyTVar' bVar \b -> b{pending = b.pending <> [(topic.unTopicName, val, hdrs)]}
  -- Flush makes every buffered record durable on its topic log (offset order).
  FlushProducer ->
    atomically $ modifyTVar' bVar \b -> (foldl' (\acc (t, v, h) -> appendRecord t v h acc) b b.pending){pending = []}
  _ -> error "runMockProducer: unexpected producer op"


-- The consumer reads from the subscribed @topics@, advancing each topic's cursor
-- (offset order), and commit records the high-water offset per (topic, 0).
runMockConsumer :: IOE :> es => [Text] -> TVar Broker -> Eff (KafkaConsumer ': es) a -> Eff es a
runMockConsumer topics bVar = interpret \_ -> \case
  PollMessageBatch _ (K.BatchSize n) -> do
    batch <- atomically do
      b <- readTVar bVar
      let pull (acc, st) topic
            | length acc >= n = (acc, st)
            | otherwise =
                let c = Map.findWithDefault 0 (topic, 0) st -- records are all partition 0 (appendRecord)
                    taken = take (n - length acc) (drop c (Map.findWithDefault [] topic b.logs))
                 in (acc <> taken, Map.insert (topic, 0) (c + length taken) st)
          (recs, cursor') = foldl' pull ([], b.cursor) topics
      writeTVar bVar b{cursor = cursor'}
      pure recs
    when (null batch) (threadDelay 20_000) -- don't busy-spin once drained
    pure (map Right batch)
  CommitPartitionsOffsets _ tps -> atomically $ modifyTVar' bVar \b -> b{committed = foldl' recordTp b.committed tps}
  -- Seek rewinds the per-topic read cursor so the offsets from the target are
  -- re-delivered on the next poll — models librdkafka redelivering uncommitted
  -- offsets only after a seek (never mid-session otherwise).
  SeekPartitions tps _ -> atomically $ modifyTVar' bVar \b -> b{cursor = foldl' seekCursor b.cursor tps}
  PausePartitions _ -> pass
  ResumePartitions _ -> pass
  Assignment -> pure $ Map.fromList [(K.TopicName t, [K.PartitionId 0]) | t <- topics]
  _ -> error "runMockConsumer: unmocked consumer op"
  where
    recordTp m (K.TopicPartition (K.TopicName t) (K.PartitionId p) (K.PartitionOffset o)) = Map.insertWith max (t, p) o m
    recordTp m _ = m
    seekCursor c (K.TopicPartition (K.TopicName t) (K.PartitionId p) (K.PartitionOffset o)) = Map.insert (t, fromIntegral p) (fromIntegral o) c
    seekCursor c _ = c


-- Run decoupledLoop against the in-memory broker until `done` fires, with a hard
-- timeout backstop. Returns the final broker state.
drive
  :: TestResources
  -> TVar Broker
  -> [Text]
  -> Queue.KafkaRole
  -> ([(Text, ByteString)] -> HashMap Text Text -> ATBackgroundCtx (Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg])))
  -> STM ()
  -> IO Broker
drive tr bVar topics role fn done = do
  let loop =
        runBackground tr.trLogger tr.trATCtx tr.trTracerProvider
          $ void
          $ runErrorNoCallStack @K.KafkaError
          $ runMockProducer bVar
          $ runMockConsumer topics bVar
          $ Queue.decoupledLoop tr.trLogger tr.trATCtx tr.trTracerProvider role 1000 "test-client" fn
  void $ timeout 15_000_000 $ race_ loop (atomically done)
  readTVarIO bVar


seedTopic :: TVar Broker -> Text -> [Int64] -> IO ()
seedTopic bVar topic offs =
  atomically $ modifyTVar' bVar \b -> foldl' (\acc o -> appendRecord topic (Just (encodeUtf8 (show o))) (K.headersFromList []) acc) b offs


-- | Seed records carrying a @monoscope-next-due-at@ header (epoch seconds),
-- exactly as every retry-tier publish stamps them ('publishToDeadLetterQueue'
-- writes @nowEpoch + tier delay@). These are the records 'subChunksFor'
-- (KafkaDlqReplay) skips while now < due.
seedTopicDue :: TVar Broker -> Text -> Int -> [Int64] -> IO ()
seedTopicDue bVar topic dueEpoch offs =
  atomically $ modifyTVar' bVar \b ->
    foldl' (\acc o -> appendRecord topic (Just (encodeUtf8 (show o))) (K.headersFromList [("monoscope-next-due-at", encodeUtf8 (show dueEpoch))]) acc) b offs


spec :: Spec
spec = around withTestResources $ describe "Kafka decoupledLoop (in-memory broker)" do
  it "drains a poll batch through processing and commits the contiguous offset watermark" \tr -> do
    bVar <- newTVarIO emptyBroker
    seedTopic bVar "otlp_logs" [0 .. 4]
    receivedVar <- newTVarIO ([] :: [ByteString])
    let testFn tuples _ = do
          atomically $ modifyTVar' receivedVar (<> map snd tuples)
          pure (Right (map fst tuples, [])) -- ack all as durable
    b <-
      drive tr bVar ["otlp_logs"] Queue.KafkaPrimary testFn $ do
        s <- readTVar bVar
        check (Map.lookup ("otlp_logs", 0) s.committed == Just 5)
    received <- readTVarIO receivedVar
    sort received `shouldBe` sort (map (encodeUtf8 . show) [0 .. 4 :: Int64]) -- each record processed once
    Map.lookup ("otlp_logs", 0) b.committed `shouldBe` Just 5 -- watermark = max offset + 1
  it "flushes DLQ publishes durable before committing the source offset (no commit-before-send loss window)" \tr -> do
    bVar <- newTVarIO emptyBroker
    seedTopic bVar "otlp_logs" [0 .. 4]
    -- Every record is poison → routeBatchOutcome publishes it to the DLQ topic.
    let testFn tuples _ = pure (Right ([], [(t, raw, "forced poison") | (t, raw) <- tuples]))
    b <-
      -- Wait for the commit. With the flush-before-commit gate the DLQ records
      -- are only durable (moved out of `pending` onto the log) once the committer
      -- has flushed; the commit fires immediately after. Drop the gate and the
      -- DLQ records stay stuck in `pending` (FlushProducer never runs), so the
      -- assertions below fail — that is the regression this test guards.
      drive tr bVar ["otlp_logs"] Queue.KafkaPrimary testFn $ do
        s <- readTVar bVar
        check (Map.lookup ("otlp_logs", 0) s.committed == Just 5)
    let dlq = Map.findWithDefault [] "otlp_deadletter" b.logs
    length dlq `shouldBe` 5 -- all five poisoned records landed on the dead-letter topic
    null b.pending `shouldBe` True -- flushed before commit: nothing left buffered in the producer
    -- and they carry the bumped attempt-count header the replay consumer keys on
    let hdrs = foldMap (K.headersToList . (.crHeaders)) (take 1 dlq)
    (snd <$> find ((== "monoscope-attempt-count") . fst) hdrs) `shouldBe` Just "1"
    Map.lookup ("otlp_logs", 0) b.committed `shouldBe` Just 5 -- DLQ accepted → offsets advanced

  -- Regression guard for the 2026-06-25 DLQ commit-starvation wedge: a chunk that
  -- returns no acks (transient write failure → routeBatchOutcome []) pins the
  -- commit base while the poll cursor advances past it. librdkafka never
  -- redelivers uncommitted offsets in-session, so without a self-heal the base
  -- pins forever (the whole 7.6M-lag stall). The fix seeks the partition back to
  -- the un-acked base so it is re-fetched and retried once the failure clears.
  it "reseeks and recommits a chunk whose first processing returned no acks (commit-starvation self-heal)" \tr -> do
    bVar <- newTVarIO emptyBroker
    seedTopic bVar "otlp_logs" [0 .. 4]
    attemptVar <- newTVarIO (0 :: Int)
    -- First processing of the chunk no-acks (models a transient TF outage);
    -- once healed, the redelivered chunk acks and the base advances.
    let testFn tuples _ = do
          n <- atomically $ stateTVar attemptVar \k -> (k, k + 1)
          pure $ Right $ if n == 0 then ([], []) else (map fst tuples, [])
    b <-
      drive tr bVar ["otlp_logs"] Queue.KafkaPrimary testFn $ do
        s <- readTVar bVar
        check (Map.lookup ("otlp_logs", 0) s.committed == Just 5)
    Map.lookup ("otlp_logs", 0) b.committed `shouldBe` Just 5 -- reseek redelivered the chunk → watermark recovered
    -- Prove the retry actually fired (≥2 calls): without it the watermark would be
    -- unreachable. Guards against the no-ack branch being silently removed (which
    -- would ack on the first call and pass the assertion above trivially).
    readTVarIO attemptVar >>= (`shouldSatisfy` (>= 2))

  -- Regression guard for the 2026-07-09 outage loss: during a 7h TF downtime the
  -- DLQ replay consumer polled retry-tier records *before* their next-due-at
  -- (the consumer keeps up with inflow, so a fresh publish due at +5s/+60s is
  -- nearly always polled early). subChunksFor skips not-due records — the poll
  -- cursor moves past them and no tracker entry is seeded — then a later poll's
  -- due chunk seeds the commit base PAST the skipped offsets and the committer
  -- makes the skip durable. ~1.9M messages were committed-over this way and
  -- never written to TF nor escalated to the next tier.
  it "dlq_replay_never_commits_past_a_skipped_not_yet_due_record" \tr -> do
    now <- floor . toRational <$> getPOSIXTime
    bVar <- newTVarIO emptyBroker
    -- fresh retry-tier publishes: polled immediately, due 2s from now
    seedTopicDue bVar "otlp_deadletter" (now + 2) [0 .. 4]
    receivedVar <- newTVarIO ([] :: [ByteString])
    let testFn tuples _ = do
          atomically $ modifyTVar' receivedVar (<> map snd tuples)
          pure (Right (map fst tuples, [])) -- the write leg is healthy: ack everything we're given
    -- 1s in (after the first poll skipped 0..4 as not-yet-due), already-due
    -- messages land behind them on the log
    _ <- forkIO do
      threadDelay 1_000_000
      seedTopicDue bVar "otlp_deadletter" (now - 1) [5 .. 9]
    b <-
      drive tr bVar ["otlp_deadletter"] Queue.KafkaDlqReplay testFn $ do
        s <- readTVar bVar
        check (maybe False (>= 10) (Map.lookup ("otlp_deadletter", 0) s.committed))
    -- Committing offset 10 declares 0..9 durable, so all ten records must have
    -- been processed by then (0..4 were due 2s in — the loop must redeliver
    -- them, not commit over them). Anything missing here is silent loss: the
    -- committed offset survives restarts, so no rebalance ever brings them back.
    Map.lookup ("otlp_deadletter", 0) b.committed `shouldBe` Just 10
    received <- readTVarIO receivedVar
    sort received `shouldBe` sort (map (encodeUtf8 . show) [0 .. 9 :: Int64])

  -- Companion liveness guard: a not-yet-due record must be processed once its
  -- due-time passes *within the session*. Today the poll cursor moves past it
  -- and nothing seeks back (the 5s DLQ backoff sleeps but re-polls from the new
  -- position), so it is stranded until the next rebalance/restart — during the
  -- outage that turned 60s-tier retries into never-retries.
  it "dlq_replay_processes_a_not_yet_due_record_once_due" \tr -> do
    now <- floor . toRational <$> getPOSIXTime
    bVar <- newTVarIO emptyBroker
    seedTopicDue bVar "otlp_deadletter" (now + 2) [0 .. 4]
    let testFn tuples _ = pure (Right (map fst tuples, []))
    b <-
      drive tr bVar ["otlp_deadletter"] Queue.KafkaDlqReplay testFn $ do
        s <- readTVar bVar
        check (maybe False (>= 5) (Map.lookup ("otlp_deadletter", 0) s.committed))
    -- Due 2s in, drive allows 15s: plenty for a correct loop to redeliver and
    -- commit. A timeout here (committed = Nothing) is the stranding bug.
    Map.lookup ("otlp_deadletter", 0) b.committed `shouldBe` Just 5
