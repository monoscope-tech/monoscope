module Pkg.KafkaConsumerSpec (spec) where

import Control.Concurrent.STM (check)
import Data.Map.Strict qualified as Map
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
import UnliftIO.Concurrent (threadDelay)

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
  , cursor :: Map Text Int -- next index the consumer reads per topic
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
   in b {logs = Map.insert topic (existing <> [cr]) b.logs}

runMockProducer :: (IOE :> es) => TVar Broker -> Eff (KafkaProducer ': es) a -> Eff es a
runMockProducer bVar = interpret \_ -> \case
  ProduceMessage KP.ProducerRecord {KP.prTopic = topic, KP.prValue = val, KP.prHeaders = hdrs} ->
    atomically $ modifyTVar' bVar \b -> b {pending = b.pending <> [(topic.unTopicName, val, hdrs)]}
  -- Flush makes every buffered record durable on its topic log (offset order).
  FlushProducer ->
    atomically $ modifyTVar' bVar \b -> (foldl' (\acc (t, v, h) -> appendRecord t v h acc) b b.pending) {pending = []}
  _ -> error "runMockProducer: unexpected producer op"

-- The consumer reads from the subscribed @topics@, advancing each topic's cursor
-- (offset order), and commit records the high-water offset per (topic, 0).
runMockConsumer :: (IOE :> es) => [Text] -> TVar Broker -> Eff (KafkaConsumer ': es) a -> Eff es a
runMockConsumer topics bVar = interpret \_ -> \case
  PollMessageBatch _ (K.BatchSize n) -> do
    batch <- atomically do
      b <- readTVar bVar
      let pull (acc, st) topic
            | length acc >= n = (acc, st)
            | otherwise =
                let c = Map.findWithDefault 0 topic st
                    taken = take (n - length acc) (drop c (Map.findWithDefault [] topic b.logs))
                 in (acc <> taken, Map.insert topic (c + length taken) st)
          (recs, cursor') = foldl' pull ([], b.cursor) topics
      writeTVar bVar b {cursor = cursor'}
      pure recs
    when (null batch) (threadDelay 20_000) -- don't busy-spin once drained
    pure (map Right batch)
  CommitPartitionsOffsets _ tps -> atomically $ modifyTVar' bVar \b -> b {committed = foldl' recordTp b.committed tps}
  PausePartitions _ -> pass
  ResumePartitions _ -> pass
  Assignment -> pure $ Map.fromList [(K.TopicName t, [K.PartitionId 0]) | t <- topics]
  _ -> error "runMockConsumer: unmocked consumer op"
  where
    recordTp m (K.TopicPartition (K.TopicName t) (K.PartitionId p) (K.PartitionOffset o)) = Map.insertWith max (t, p) o m
    recordTp m _ = m

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

spec :: Spec
spec = aroundAll withTestResources $ describe "Kafka decoupledLoop (in-memory broker)" do
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
