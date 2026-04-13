module Opentelemetry.TraceSessionPropagationSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Pkg.TraceSessionCache qualified as TSC
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


querySessionColumns :: TestResources -> Text -> IO (V.Vector (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text))
querySessionColumns tr trId =
  withPool tr.trPool $
    DBT.query
      [sql| SELECT name, attributes___session___id, attributes___user___id,
                   attributes___user___email, attributes___user___name, attributes___user___full_name
            FROM otel_logs_and_spans
            WHERE project_id = ? AND context___trace_id = ?
            ORDER BY name |]
      (pid, trId)


findSpan :: Text -> V.Vector (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text) -> Maybe (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
findSpan name = find (\(n, _, _, _, _, _) -> n == name) . V.toList


spec :: Spec
spec = aroundAll withTestResources do
  describe "Trace Session Propagation" do

    it "propagates all session/user attrs across batches via cache" $ \tr -> do
      key <- createTestAPIKey tr pid "sess-prop-1"
      let trId = "aabbccdd11223344aabbccdd11223344"
          resource = mkResource key []
          reqWith = mkSpanRequest trId "span1aabbccdd" Nothing "frontend-span"
            [] Nothing [ mkAttr "session.id" "sess-100", mkAttr "user.id" "usr-200"
                       , mkAttr "user.email" "u@test.com", mkAttr "user.name" "uname"
                       , mkAttr "user.full_name" "User Full" ] resource frozenTime
          reqWithout = mkSpanRequest trId "span2aabbccdd" Nothing "backend-span"
            [] Nothing [] resource frozenTime
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqWith)
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqWithout)
      rows <- querySessionColumns tr trId
      V.length rows `shouldBe` 2
      forM_ (V.toList rows) $ \(_, sessId, userId, email, uname, fullName) -> do
        sessId `shouldBe` Just "sess-100"
        userId `shouldBe` Just "usr-200"
        email `shouldBe` Just "u@test.com"
        uname `shouldBe` Just "uname"
        fullName `shouldBe` Just "User Full"

    it "does not overwrite existing session attributes" $ \tr -> do
      key <- createTestAPIKey tr pid "sess-prop-2"
      let trId = "11223344aabbccdd11223344aabbccdd"
          resource = mkResource key []
          reqA = mkSpanRequest trId "spanaaaa1111" Nothing "span-A"
            [] Nothing [mkAttr "session.id" "sess-A"] resource frozenTime
          reqB = mkSpanRequest trId "spanbbbb2222" Nothing "span-B"
            [] Nothing [mkAttr "session.id" "sess-B"] resource frozenTime
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqA)
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqB)
      rows <- querySessionColumns tr trId
      fmap (\(_, s, _, _, _, _) -> s) (findSpan "span-A" rows) `shouldBe` Just (Just "sess-A")
      fmap (\(_, s, _, _, _, _) -> s) (findSpan "span-B" rows) `shouldBe` Just (Just "sess-B")

    it "merges partial session info across spans via Semigroup" $ \tr -> do
      key <- createTestAPIKey tr pid "sess-prop-3"
      let trId = "deadbeef11223344deadbeef11223344"
          resource = mkResource key []
          reqSess = mkSpanRequest trId "span11112222" Nothing "has-session"
            [] Nothing [mkAttr "session.id" "sess-merge"] resource frozenTime
          reqUser = mkSpanRequest trId "span33334444" Nothing "has-user"
            [] Nothing [mkAttr "user.id" "usr-merge"] resource frozenTime
          reqEmpty = mkSpanRequest trId "span55556666" Nothing "has-neither"
            [] Nothing [] resource frozenTime
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqSess)
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqUser)
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqEmpty)
      rows <- querySessionColumns tr trId
      fmap (\(_, s, _, _, _, _) -> s) (findSpan "has-neither" rows) `shouldBe` Just (Just "sess-merge")
      fmap (\(_, _, u, _, _, _) -> u) (findSpan "has-neither" rows) `shouldBe` Just (Just "usr-merge")

    it "backfill propagates session attrs to spans inserted before cache was populated" $ \tr -> do
      key <- createTestAPIKey tr pid "sess-prop-4"
      now <- getCurrentTime
      let trId = "cafebabe11223344cafebabe11223344"
          resource = mkResource key []
          reqEmpty = mkSpanRequest trId "spaneeee1111" Nothing "early-span"
            [] Nothing [] resource now
          reqWith = mkSpanRequest trId "spanffff2222" Nothing "late-span"
            [] Nothing [mkAttr "session.id" "sess-backfill", mkAttr "user.id" "usr-backfill"] resource now
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqEmpty)
      void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto reqWith)
      rowsBefore <- querySessionColumns tr trId
      fmap (\(_, s, _, _, _, _) -> s) (findSpan "early-span" rowsBefore) `shouldBe` Just Nothing
      -- Run shared backfill
      n <- runQueryEffect tr TSC.backfillSessionAttributes
      n `shouldSatisfy` (> 0)
      rowsAfter <- querySessionColumns tr trId
      fmap (\(_, s, _, _, _, _) -> s) (findSpan "early-span" rowsAfter) `shouldBe` Just (Just "sess-backfill")
      fmap (\(_, _, u, _, _, _) -> u) (findSpan "early-span" rowsAfter) `shouldBe` Just (Just "usr-backfill")

    it "evicts stale and over-cap entries" $ \tr -> do
      cache <- TSC.newTraceSessionCache
      let mkInfo t = TSC.TraceSessionInfo (Just "s") Nothing Nothing Nothing Nothing t
          t0 = frozenTime
          t1 = addUTCTime 100 t0
          t2 = addUTCTime 200 t0
          now = addUTCTime 400 t0 -- 400s after t0
      -- Insert 3 entries: t0 is stale (>300s), t1 fresh, t2 fresh
      atomicModifyIORef' cache $ \_ ->
        (HM.fromList [(("p","tr0"), mkInfo t0), (("p","tr1"), mkInfo t1), (("p","tr2"), mkInfo t2)], ())
      -- Evict with 300s idle, max 10 entries: t0 should be evicted
      evicted1 <- TSC.evictStaleEntries cache 300 10 now
      evicted1 `shouldBe` 1
      remaining <- readIORef cache
      HM.size remaining `shouldBe` 2
      HM.member ("p","tr0") remaining `shouldBe` False
      -- Evict with LRU cap of 1: oldest of the 2 fresh entries should be dropped
      evicted2 <- TSC.evictStaleEntries cache 300 1 now
      evicted2 `shouldBe` 1
      remaining2 <- readIORef cache
      HM.size remaining2 `shouldBe` 1
      HM.member ("p","tr2") remaining2 `shouldBe` True
