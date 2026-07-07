-- | processList must surface dual-write outcomes via 'Either WriteFailure',
-- never as a swallowed exception or a silent ack. Pkg.Queue.kafkaService /
-- pubsubService pattern-match on the result and route 'Left' to the DLQ with
-- side-tracking headers (which side(s) succeeded / failed) so the replay tool
-- can rewrite only the missing side.
module Opentelemetry.TimefusionWriteFailureSpec (spec) where

import Control.Exception (ErrorCall (..))
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Pool (withResource)
import Data.ProtoLens (encodeMessage)
import Data.These.Combinators (isThat)
import Data.UUID qualified as UUID
import Data.Effectful.Hasql qualified as EHasql
import Database.PostgreSQL.Simple (Only (..), Query, execute_, query)
import Hasql.Errors qualified as HE
import Hasql.Interpolate qualified as HI
import Hasql.Pool qualified as HP
import UnliftIO.Exception (finally, throwIO, try)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pkg.DeriveUtils (UUIDId (..), mkHasqlPool, rawSql)
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec (SpecWith, around, beforeWith, describe, expectationFailure, it, shouldBe)


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


-- | Rows the demo project currently holds in otel_logs_and_spans. Used as a
-- before/after delta so leg-targeting can be asserted without a DB reset.
-- Count spans in the store the write leg actually targets. When TF writes are
-- enabled we must read the hasql TF pool, NOT trPool: the postgres-simple
-- timefusionPgPool always points at the main test DB even when
-- TIMEFUSION_PG_TEST_URL wires in a real TF, so only the hasql TF pool sees
-- real-TF rows. When TF is disabled the spans live in the main DB, so route
-- there (withHasqlTimefusion False = main pool) — mirroring the app rather than
-- unconditionally hitting TF. project_id is embedded as a literal so the query
-- works whether TF is real (text column) or the Postgres-as-TF fallback (uuid).
countDemoSpans :: TestResources -> IO Int
countDemoSpans tr =
  fromIntegral @Int64
    <$> runTestBg frozenTime tr (EHasql.withHasqlTimefusion tr.trATCtx.config.enableTimefusionWrites (HI.getOneColumn . HI.getOneRow <$> EHasql.interp (rawSql sql)))
  where
    sql = "SELECT count(*)::int8 FROM otel_logs_and_spans WHERE project_id = '" <> demoProjectId.toText <> "'"


-- | Force the dual-write on (both pools point at the shared test DB unless
-- TIMEFUSION_PG_TEST_URL is set), so a WriteBoth visibly writes twice.
withTfEnabled :: TestResources -> TestResources
withTfEnabled tr =
  let ctx = tr.trATCtx
   in tr {trATCtx = ctx {env = ctx.env {enableTimefusionWrites = True}, config = ctx.config {enableTimefusionWrites = True}}}


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


-- | A TimeFusion pool that is fully reachable and accepts inserts WITHOUT
-- error, yet silently persists nothing: a BEFORE INSERT row trigger, keyed on a
-- per-connection GUC carried only by this pool's connection string, discards
-- every row (RETURN NULL → 0 rows affected, no exception). The PG pool has no
-- GUC so its writes land normally.
--
-- This is the faithful repro of the 2026-06-12 incident: PG held the data, TF
-- did not, and nothing errored. Before the 'unaccountedRows' cross-check the
-- dual-write reported this as a clean success and the batch was acked (silent
-- loss); after it, the missing-rows count surfaces as Left (That tfErr) → DLQ.
silentTfDDL :: [Query]
silentTfDDL =
  [ "CREATE OR REPLACE FUNCTION test_silent_tf_discard() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF current_setting('test.silent_tf', true) = 'on' THEN RETURN NULL; END IF; RETURN NEW; END $$"
  , "DROP TRIGGER IF EXISTS test_silent_tf ON otel_logs_and_spans"
  , "CREATE TRIGGER test_silent_tf BEFORE INSERT ON otel_logs_and_spans FOR EACH ROW EXECUTE FUNCTION test_silent_tf_discard()"
  ]


withSilentTf :: TestResources -> (TestResources -> IO ()) -> IO ()
withSilentTf tr k = do
  withResource tr.trPool \c -> mapM_ (execute_ c) silentTfDDL
  -- Keyword-form conninfo (host=… dbname=…); carry the per-connection GUC via
  -- the `options` keyword so ONLY this pool's backend discards inserts.
  silentPool <- mkHasqlPool 1 (tr.trConnStr <> " options='-c test.silent_tf=on'")
  let origCtx = tr.trATCtx
      ctx =
        origCtx
          { hasqlTimefusionPool = silentPool
          , env = origCtx.env {enableTimefusionWrites = True}
          , config = origCtx.config {enableTimefusionWrites = True}
          }
  k tr {trATCtx = ctx}
    `finally` withResource tr.trPool \c -> void (execute_ c "DROP TRIGGER IF EXISTS test_silent_tf ON otel_logs_and_spans")


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


-- | The exact 2026-06-21 prod failure shape: a server-sent statement error with
-- an EMPTY SQLSTATE (pgdog / datafusion-postgres reset a backend mid-statement).
-- hasql classes every StatementSessionError as non-transient, so pre-fix one
-- blip dead-lettered the whole batch before the write was even attempted.
emptyResetError :: HP.UsageError
emptyResetError =
  HP.SessionUsageError (HE.StatementSessionError 1 0 "select 1" [] True (HE.ServerStatementError (HE.ServerError "" "Server error" Nothing Nothing Nothing)))


-- | A genuine poison error (real SQLSTATE) must stay non-transient: fail fast → DLQ.
poisonError :: HP.UsageError
poisonError =
  HP.SessionUsageError (HE.StatementSessionError 1 0 "select 1" [] True (HE.ServerStatementError (HE.ServerError "23505" "duplicate key" Nothing Nothing Nothing)))


spec :: SpecWith ()
spec = around withTestResources $ beforeWith mkResWithKey do
  -- B1 of the 2026-06-21 fix: the per-batch project-id / cache lookups are
  -- wrapped in 'retryTransientEff', so a transient connection reset retries
  -- instead of dead-lettering a whole batch that the write path would accept.
  describe "read-gate transient retry (retryTransientEff)" do
    it "retries an empty-SQLSTATE reset, then succeeds (a blip no longer DLQs the batch)" \(tr, _) -> do
      counter <- newIORef (0 :: Int)
      let flakyRead = do
            n <- atomicModifyIORef' counter \c -> (c + 1, c + 1)
            if n < 3 then throwIO (EHasql.HasqlException emptyResetError) else pure n
      res <- runTestBg frozenTime tr $ Telemetry.retryTransientEff 5 "read-gate" flakyRead
      res `shouldBe` 3
      readIORef counter >>= (`shouldBe` 3)

    it "does NOT retry a real-SQLSTATE poison error (fails fast → DLQ)" \(tr, _) -> do
      counter <- newIORef (0 :: Int)
      let poisonRead = do
            void $ atomicModifyIORef' counter \c -> (c + 1, ())
            throwIO (EHasql.HasqlException poisonError)
      res <- try @_ @SomeException (runTestBg frozenTime tr (Telemetry.retryTransientEff 5 "read-gate" poisonRead) :: IO Int)
      isLeft res `shouldBe` True
      readIORef counter >>= (`shouldBe` 1)

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

    -- THE TuplesOk behavior, at the TimeFusion write layer. TimeFusion's PGWire
    -- emulation answers an INSERT with a `TuplesOk` status tag instead of
    -- `CommandComplete`; hasql's rowsAffected decoder throws on that, and
    -- `retryHasqlWrite` *swallows it into a 0-row `Right mempty`* on the
    -- assumption the rows landed — WITHOUT reading anything back. This test pins
    -- that swallow: a TuplesOk failure becomes a reported success with
    -- rowsInserted == 0. That fabricated "success" is exactly what made the
    -- 2026-06-12 loss silent (PG had the rows, TF didn't, nothing errored).
    it "TuplesOk wire-mismatch is swallowed into a 0-row success (no persistence check)" \(tr, _) -> do
      res <-
        runTestBg frozenTime tr
          $ Telemetry.retryHasqlWrite 3 "tf" (throwIO (ErrorCall "TimeFusion PGWire: Unexpected result status: TuplesOk"))
      case res :: Either SomeException Telemetry.BulkInsertResult of
        Right bir -> bir.rowsInserted `shouldBe` 0 -- reported success, yet zero rows accounted for
        Left e -> expectationFailure $ "expected the TuplesOk swallow (Right mempty); got Left " <> show e

    -- The consequence + the fix. The 0-row "success" the TuplesOk swallow above
    -- produces is indistinguishable from a store that accepted the write and
    -- dropped it. Here a reachable TF pool persists nothing (rows submitted but
    -- 0 land, no error) — pre-fix the dual-write returned Right (acks, no poison)
    -- and acked the batch (silent loss past the DLQ); the 'unaccountedRows'
    -- cross-check now turns "reported success but rows missing" into Left (That).
    it "TF reports success but persists nothing → Left (That tfErr), never a silent ack" \(tr, key) ->
      withSilentTf tr \tr' -> do
        res <- runTestBg frozenTime tr' $ OtlpServer.processList [validLogMsg key "ack-silent-tf"] logsAttrs
        expectLeftMatching "TF silent under-persist (rows-persisted cross-check)" isThat res

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

  -- DLQ replay must rewrite ONLY the leg that originally failed: the durable leg
  -- already holds the row and PG inserts aren't idempotent, so a dual-write
  -- replay duplicates it. (The header → target mapping itself is covered by the
  -- 'writeTargetFor' / 'writeFailureDlqHeaders' doctests; these pin the
  -- row-level effect end-to-end.)
  describe "DLQ replay leg-targeting (single-leg rewrite)" do
    -- The core regression. Both pools hit the shared test DB, so a (pre-fix)
    -- WriteBoth replay writes the record twice; the fix writes it once.
    it "tf-failed replay rewrites only TF — adds exactly one row, never a duplicate" \(tr, key) -> do
      let tr' = withTfEnabled tr
          replayAttrs = HM.insert "monoscope-write-failure" "tf-failed" logsAttrs
      before <- countDemoSpans tr'
      res <- runTestBg frozenTime tr' $ OtlpServer.processList [validLogMsg key "ack-tf-replay"] replayAttrs
      after <- countDemoSpans tr'
      case res of
        Right (acks, poison) -> do
          acks `shouldBe` ["ack-tf-replay"]
          poison `shouldBe` []
          (after - before) `shouldBe` 1
        Left wf -> expectationFailure $ "expected Right; got Left " <> toString (Telemetry.writeFailureSummary wf)

    -- A pg-failed replay rewrites only PG, so a DOWN TimeFusion leg is skipped
    -- and the replay succeeds — whereas a normal message with TF down DLQs
    -- (Left That, the "TF down" case above). Proves the leg is skipped, not just
    -- tolerated.
    it "pg-failed replay skips a down TimeFusion leg → Right (a normal msg would DLQ)" \(tr, key) ->
      withBrokenTf tr \tr' -> do
        let replayAttrs = HM.insert "monoscope-write-failure" "pg-failed" logsAttrs
        res <- runTestBg frozenTime tr' $ OtlpServer.processList [validLogMsg key "ack-pg-replay"] replayAttrs
        case res of
          Right (acks, _) -> acks `shouldBe` ["ack-pg-replay"]
          Left wf -> expectationFailure $ "expected Right (TF leg skipped); got Left " <> toString (Telemetry.writeFailureSummary wf)
