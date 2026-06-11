-- | processList must surface dual-write outcomes via 'Either WriteFailure',
-- never as a swallowed exception or a silent ack. Pkg.Queue.kafkaService /
-- pubsubService pattern-match on the result and route 'Left' to the DLQ with
-- side-tracking headers (which side(s) succeeded / failed) so the replay tool
-- can rewrite only the missing side.
module Opentelemetry.TimefusionWriteFailureSpec (spec) where

import Control.Exception (ErrorCall (..), evaluate)
import UnliftIO.Exception (try)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.ProtoLens (encodeMessage)
import Data.These (These (..))
import Data.These.Combinators (isThat)
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pkg.DeriveUtils (UUIDId (..), mkHasqlPool)
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec (SpecWith, aroundAll, beforeWith, describe, expectationFailure, it, shouldBe)


-- | A closed loopback port. Pool acquire fails with ECONNREFUSED so we
-- exercise the write-failure path without standing up a second DB.
badConnStr :: ByteString
badConnStr = "postgresql://postgres:postgres@127.0.0.1:1/postgres"


logsAttrs :: HM.HashMap Text Text
logsAttrs = HM.fromList [("ce-type", "org.opentelemetry.otlp.logs.v1")]


-- | A valid logs proto bound to a real (DB-resident) API key. Without one,
-- projectIdsByProjectApiKeys returns empty, the convert step yields zero rows,
-- and dbInsert is never called — making "TF down" look like a healthy no-op.
validLogMsg :: Text -> Text -> (Text, ByteString)
validLogMsg apiKey ackId =
  (ackId, encodeMessage $ createOtelLogAtTime apiKey ["valid record from " <> ackId] frozenTime)


corruptMsg :: Text -> (Text, ByteString)
corruptMsg ackId = (ackId, "this is not a valid protobuf payload")


demoProjectId :: Projects.ProjectId
demoProjectId = UUIDId UUID.nil


withBrokenTf :: TestResources -> (TestResources -> IO ()) -> IO ()
withBrokenTf tr k = do
  badPool <- mkHasqlPool 1 badConnStr
  let origCtx = tr.trATCtx
      ctx =
        origCtx
          { hasqlTimefusionPool = badPool
          , env = origCtx.env {enableTimefusionWrites = True}
          , config = origCtx.config {enableTimefusionWrites = True}
          }
  k tr {trATCtx = ctx}


-- | Override the main Hasql pool with one that can't connect. Pins down "PG
-- is mandatory" — the previous log-and-continue branch silently lost the PG
-- side, leaving dashboards short rows.
withBrokenPg :: TestResources -> (TestResources -> IO ()) -> IO ()
withBrokenPg tr k = do
  badPool <- mkHasqlPool 1 badConnStr
  let origCtx = tr.trATCtx
      ctx =
        origCtx
          { hasqlPool = badPool
          , env = origCtx.env {enableTimefusionWrites = True}
          , config = origCtx.config {enableTimefusionWrites = True}
          }
  k tr {trATCtx = ctx}


mkResWithKey :: TestResources -> IO (TestResources, Text)
mkResWithKey tr = do
  key <- createTestAPIKey tr demoProjectId "tf-write-failure-spec"
  pure (tr, key)


expectLeftMatching
  :: HasCallStack
  => Text
  -> (Telemetry.WriteFailure -> Bool)
  -> Either Telemetry.WriteFailure ([Text], [Telemetry.PoisonMsg])
  -> IO ()
expectLeftMatching what predicate = \case
  Right (acks, poison) ->
    expectationFailure
      $ toString
      $ "expected " <> what <> " (Left WriteFailure); got Right with acks="
      <> show acks
      <> " poison="
      <> show (map (\(a, _, _) -> a) poison)
  Left wf
    | predicate wf -> pass
    | otherwise ->
        expectationFailure
          $ toString
          $ "expected " <> what <> "; got Left with summary: " <> Telemetry.writeFailureSummary wf


spec :: SpecWith ()
spec = aroundAll withTestResources $ beforeWith mkResWithKey do
  describe "processList Either contract (dual-write outcome surfaces to Pkg.Queue)" do
    it "writes succeed → Right (acks, no poison)" \(tr, key) -> do
      let msgs = [validLogMsg key "ack-good-1", validLogMsg key "ack-good-2"]
      res <- runTestBg frozenTime tr $ OtlpServer.processList msgs logsAttrs
      case res of
        Right (acks, poison) -> do
          L.sort acks `shouldBe` ["ack-good-1", "ack-good-2"]
          poison `shouldBe` []
        Left wf ->
          expectationFailure
            $ "expected Right; got Left " <> toString (Telemetry.writeFailureSummary wf)

    it "TF down → Left (That tfErr) — TF-only failure" \(tr, key) ->
      withBrokenTf tr \tr' -> do
        tr'.trATCtx.env.enableTimefusionWrites `shouldBe` True
        res <- runTestBg frozenTime tr' $ OtlpServer.processList [validLogMsg key "ack-1"] logsAttrs
        expectLeftMatching "TF-only failure" isThat res

    -- PG-down trips the project-key lookup before any write attempt. With
    -- queryProjectIdByKey now propagating Hasql errors (silent-swallow fix),
    -- processList THROWS HasqlException rather than returning Left WriteFailure.
    -- Pkg.Queue's outer tryAny catches and routes via DLQ-or-redeliver.
    it "PG down → processList throws (Pkg.Queue routes to DLQ)" \(tr, key) ->
      withBrokenPg tr \tr' -> do
        res <- try @_ @SomeException $ runTestBg frozenTime tr' $ OtlpServer.processList [validLogMsg key "ack-1"] logsAttrs
        case res of
          Left _ -> pass
          Right v -> expectationFailure $ "expected exception; got " <> show v

    it "both down → processList throws (lookup fails first, Pkg.Queue DLQs)" \(tr, key) -> do
      badPool1 <- mkHasqlPool 1 badConnStr
      badPool2 <- mkHasqlPool 1 badConnStr
      let origCtx = tr.trATCtx
          ctx =
            origCtx
              { hasqlPool = badPool1
              , hasqlTimefusionPool = badPool2
              , env = origCtx.env {enableTimefusionWrites = True}
              , config = origCtx.config {enableTimefusionWrites = True}
              }
          tr' = tr {trATCtx = ctx}
      res <- try @_ @SomeException $ runTestBg frozenTime tr' $ OtlpServer.processList [validLogMsg key "ack-1"] logsAttrs
      case res of
        Left _ -> pass
        Right v -> expectationFailure $ "expected exception; got " <> show v

    it "mixed batch (good + corrupt) + TF down → Left wf (whole batch DLQ'd)" \(tr, key) ->
      withBrokenTf tr \tr' -> do
        let msgs = [validLogMsg key "ack-good", corruptMsg "ack-poison"]
        res <- runTestBg frozenTime tr' $ OtlpServer.processList msgs logsAttrs
        expectLeftMatching "TF-only failure on mixed batch" isThat res

    -- Regression: this used to return Right ([], []) — no acks, no poison —
    -- which never commits/acks and stalls the partition on these records
    -- forever (hot redelivery loop, hit by DLQ replay of e.g. rrweb messages).
    it "unsupported ce-type → whole batch poisoned (→ DLQ), never an uncommittable no-op" \(tr, key) -> do
      let msgs = [validLogMsg key "ack-unsup-1", corruptMsg "ack-unsup-2"]
      res <- runTestBg frozenTime tr $ OtlpServer.processList msgs (HM.fromList [("ce-type", "org.rrweb.events.v1")])
      case res of
        Right (acks, poison) -> do
          acks `shouldBe` []
          map (\(a, _, r) -> (a, r)) poison `shouldBe` [("ack-unsup-1", "unsupported ce-type"), ("ack-unsup-2", "unsupported ce-type")]
        Left wf -> expectationFailure $ "expected Right; got Left " <> toString (Telemetry.writeFailureSummary wf)

    it "corrupt-only batch + TF healthy → Right ([], [poison]); Pkg.Queue DLQs it" \(tr, _) -> do
      res <- runTestBg frozenTime tr $ OtlpServer.processList [corruptMsg "ack-poison"] logsAttrs
      case res of
        Right (acks, poison) -> do
          acks `shouldBe` []
          map (\(a, _, _) -> a) poison `shouldBe` ["ack-poison"]
        Left wf -> expectationFailure $ "expected Right; got Left " <> toString (Telemetry.writeFailureSummary wf)

    it "mixed batch (good + corrupt) + TF healthy → Right (good acked, corrupt in poison list)" \(tr, key) -> do
      let msgs = [validLogMsg key "ack-good", corruptMsg "ack-poison"]
      res <- runTestBg frozenTime tr $ OtlpServer.processList msgs logsAttrs
      case res of
        Right (acks, poison) -> do
          acks `shouldBe` ["ack-good"]
          map (\(a, _, _) -> a) poison `shouldBe` ["ack-poison"]
        Left wf ->
          expectationFailure
            $ "expected Right; got Left " <> toString (Telemetry.writeFailureSummary wf)

  describe "DLQ side-tracking headers (writeFailureDlqHeaders)" do
    it "PG-only failure → pg-succeeded=false, tf-succeeded=true" \(_, _) -> do
      pgErr <- toException <$> evaluate (ErrorCall "synthetic pg")
      let hdrs = Telemetry.writeFailureDlqHeaders (This pgErr)
      HM.lookup "monoscope-write-failure" hdrs `shouldBe` Just "pg-failed"
      HM.lookup "monoscope-pg-succeeded" hdrs `shouldBe` Just "false"
      HM.lookup "monoscope-tf-succeeded" hdrs `shouldBe` Just "true"

    it "TF-only failure → pg-succeeded=true, tf-succeeded=false" \(_, _) -> do
      tfErr <- toException <$> evaluate (ErrorCall "synthetic tf")
      let hdrs = Telemetry.writeFailureDlqHeaders (That tfErr)
      HM.lookup "monoscope-write-failure" hdrs `shouldBe` Just "tf-failed"
      HM.lookup "monoscope-pg-succeeded" hdrs `shouldBe` Just "true"
      HM.lookup "monoscope-tf-succeeded" hdrs `shouldBe` Just "false"

    it "both failed → both sides false" \(_, _) -> do
      pgErr <- toException <$> evaluate (ErrorCall "synthetic pg")
      tfErr <- toException <$> evaluate (ErrorCall "synthetic tf")
      let hdrs = Telemetry.writeFailureDlqHeaders (These pgErr tfErr)
      HM.lookup "monoscope-write-failure" hdrs `shouldBe` Just "both-failed"
      HM.lookup "monoscope-pg-succeeded" hdrs `shouldBe` Just "false"
      HM.lookup "monoscope-tf-succeeded" hdrs `shouldBe` Just "false"
