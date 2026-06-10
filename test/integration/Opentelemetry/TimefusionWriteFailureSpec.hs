-- | Regression for the silent data-loss bug where `processList` swallowed
-- every exception from the write path and returned ackIds of a failed batch.
-- The Kafka/PubSub consumers in `Pkg.Queue` commit offsets for whatever
-- ackIds processList returns, so a successful return on a failed write =
-- lost data (observed 40-min outage 2026-06-10).
--
-- Fix: remove the swallowing `onException` in `processList`. Pkg.Queue's
-- batch-failure handler (Queue.hs:163-172) already does the right thing on
-- exception: Hasql-transient → no ack (broker redelivers), anything else →
-- publishToDeadLetterQueue + ack only if DLQ accepted.
module Opentelemetry.TimefusionWriteFailureSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.ProtoLens (encodeMessage)
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pkg.DeriveUtils (UUIDId (..), mkHasqlPool)
import Pkg.TestUtils
import Relude
import UnliftIO.Exception (try)
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec (Spec, SpecWith, aroundAll, beforeWith, describe, expectationFailure, it, shouldBe)


-- | A closed loopback port. Pool acquire fails with ECONNREFUSED so we
-- exercise the TF-write-failure path without standing up a second DB.
badTfConnStr :: ByteString
badTfConnStr = "postgresql://postgres:postgres@127.0.0.1:1/postgres"


-- | Route attrs that pin the batch to the OTLP-logs decoder.
logsAttrs :: HM.HashMap Text Text
logsAttrs = HM.fromList [("ce-type", "org.opentelemetry.otlp.logs.v1")]


-- | One valid logs proto bound to a real (DB-resident) API key so processBatchPipeline
-- actually emits rows to write. Without a real key, `projectIdsByProjectApiKeys`
-- returns empty, the convert step yields zero rows, and dbInsert is never called —
-- making the TF-down case look like a successful no-op.
validLogMsg :: Text -> Text -> (Text, ByteString)
validLogMsg apiKey ackId =
  (ackId, encodeMessage $ createOtelLogAtTime apiKey ["valid record from " <> ackId] frozenTime)


-- | A bytestring that's NOT a valid OTLP logs proto — exercise the decode-fail
-- path. processBatchPipeline buckets these as "no-work" and acks them so they
-- don't poison the queue forever.
corruptMsg :: Text -> (Text, ByteString)
corruptMsg ackId = (ackId, "this is not a valid protobuf payload")


demoProjectId :: Projects.ProjectId
demoProjectId = UUIDId UUID.nil


-- | Wrap a test in a TestResources whose labeled "timefusion" pool points at a
-- closed port — every TF write attempt fails fast.
withBrokenTf :: TestResources -> (TestResources -> IO ()) -> IO ()
withBrokenTf tr k = do
  badTfPool <- mkHasqlPool 1 badTfConnStr
  let origCtx = tr.trATCtx
      ctxBadTf =
        origCtx
          { hasqlTimefusionPool = badTfPool
          , env = origCtx.env {enableTimefusionWrites = True}
          , config = origCtx.config {enableTimefusionWrites = True}
          }
  k tr {trATCtx = ctxBadTf}


-- | Bind a fresh API key to the demo project before each spec so the
-- decode/convert path actually produces rows to write. Returns @(TestResources, apiKey)@.
mkResWithKey :: TestResources -> IO (TestResources, Text)
mkResWithKey tr = do
  key <- createTestAPIKey tr demoProjectId "tf-write-failure-spec"
  pure (tr, key)


-- | Run processList, expecting it to THROW. Returns Right on exception
-- (correct behaviour: Pkg.Queue will see the throw and DLQ-or-redeliver) and
-- Left on a non-throwing return (bug: ackIds get committed, batch dropped).
expectProcessListThrows :: TestResources -> [(Text, ByteString)] -> HM.HashMap Text Text -> IO ()
expectProcessListThrows tr msgs attrs = do
  res <- try @_ @SomeException $ runTestBg frozenTime tr $ OtlpServer.processList msgs attrs
  case res of
    Left _ -> pass
    Right ackIds ->
      expectationFailure
        $ "processList returned successfully when an exception was expected; "
        <> "Pkg.Queue would commit these ackIds and drop the batch: "
        <> show ackIds


spec :: SpecWith ()
spec = aroundAll withTestResources $ beforeWith mkResWithKey do
  describe "processList exception propagation (Pkg.Queue handles DLQ-or-redeliver)" do
    it "writes succeed → all ackIds returned, nothing thrown" \(tr, key) -> do
      let msgs = [validLogMsg key "ack-good-1", validLogMsg key "ack-good-2"]
      ackIds <- runTestBg frozenTime tr $ OtlpServer.processList msgs logsAttrs
      L.sort ackIds `shouldBe` ["ack-good-1", "ack-good-2"]

    it "all good msgs + TF down → exception PROPAGATES (Queue.hs will DLQ or no-ack)" \(tr, key) ->
      withBrokenTf tr \tr' -> do
        let msgs = [validLogMsg key "ack-good-1", validLogMsg key "ack-good-2"]
        expectProcessListThrows tr' msgs logsAttrs

    it "single good msg + TF down → exception propagates (original 40-min-data-loss bug repro)" \(tr, key) ->
      withBrokenTf tr \tr' -> do
        tr'.trATCtx.env.enableTimefusionWrites `shouldBe` True
        expectProcessListThrows tr' [validLogMsg key "kafka-ack-1"] logsAttrs

    it "mixed batch (good + corrupt) + TF down → exception propagates (whole batch DLQ'd by Queue.hs)" \(tr, key) ->
      withBrokenTf tr \tr' -> do
        -- All-or-nothing: Pkg.Queue receives the throw and routes the entire
        -- batch (good + corrupt) to the DLQ. Manual investigation can split
        -- them apart later. No partial-ack at this layer — bulk write is atomic.
        let msgs = [validLogMsg key "ack-good", corruptMsg "ack-poison"]
        expectProcessListThrows tr' msgs logsAttrs

    -- TODO(dlq-decode-failures): currently decode-failed messages are
    -- log-and-acked rather than DLQ'd. This is pre-existing behaviour, not a
    -- regression — but the user's stated policy is "manual investigation for
    -- anything unprocessable", which means decode failures should route through
    -- the DLQ too. Tracked separately; see memory note `processBatchPipeline_dlq_decode_failures`.
    it "corrupt-only batch + TF healthy → corrupt acked + logged (KNOWN GAP: should be DLQ'd)" \(tr, _) -> do
      ackIds <- runTestBg frozenTime tr $ OtlpServer.processList [corruptMsg "ack-poison"] logsAttrs
      ackIds `shouldBe` ["ack-poison"]

    it "mixed batch (good + corrupt) + TF healthy → both acked, corrupt logged" \(tr, key) -> do
      let msgs = [validLogMsg key "ack-good", corruptMsg "ack-poison"]
      ackIds <- runTestBg frozenTime tr $ OtlpServer.processList msgs logsAttrs
      L.sort ackIds `shouldBe` ["ack-good", "ack-poison"]
