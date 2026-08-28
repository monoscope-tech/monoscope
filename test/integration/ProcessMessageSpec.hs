module ProcessMessageSpec (spec) where

import Data.Aeson qualified as AE
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.HashMap.Strict qualified as HashMap
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import ProcessMessage (emptyPathClassifier, processMessages, processSpanToEntities)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec (Spec, around, describe, expectationFailure, it, shouldBe, shouldContain, shouldNotBe)
import Utils (toXXHash)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


spec :: Spec
spec = around withTestResources do
  describe "processSpanToEntities" do
    -- Regression: an unparseable project_id used to be `Unsafe.fromJust . UUID.fromText`,
    -- crashing the whole ingestion batch. The owning ProjectId is now threaded in typed,
    -- so the span's own project_id text is never parsed and a garbage value can't crash.
    it "does not crash on an unparseable project_id and stamps the threaded ProjectId" $ \_ -> do
      now <- getCurrentTime
      let badSpan =
            Telemetry.OtelLogsAndSpans
              { project_id = "not-a-uuid"
              , id = ""
              , timestamp = now
              , observed_timestamp = Nothing
              , context = Nothing
              , level = Nothing
              , severity = Nothing
              , body = Nothing
              , attributes = Nothing
              , resource = Nothing
              , hashes = Nothing
              , kind = Nothing
              , status_code = Nothing
              , status_message = Nothing
              , start_time = now
              , end_time = Nothing
              , events = Nothing
              , links = Nothing
              , duration = Nothing
              , name = Nothing
              , parent_id = Nothing
              , summary = V.empty
              , date = now
              , errors = Nothing
              , message_size_bytes = 0
              }
          (mkEndpoint, _hashes, _) = processSpanToEntities emptyPathClassifier Projects.defaultProjectCache pid badSpan
      fmap (.projectId) (mkEndpoint UUID.nil) `shouldBe` Just pid

  describe "process request to db" do
    it "test processing raw request message string" $ \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let jsonMsg = "{\"duration\":737639,\"host\":\"3.228.92.161\",\"method\":\"POST\",\"path_params\":{\"0\":\"/service/extension/backup/mboximport\"},\"project_id\":\"00000000-0000-0000-0000-000000000000\",\"proto_minor\":1,\"proto_major\":1,\"query_params\":{\"account-name\":[\"admin\"],\"account-status\":[\"1\"],\"ow\":[\"cmd\"]},\"raw_url\":\"/service/extension/backup/mboximport?account-name=admin&account-status=1&ow=cmd\",\"referer\":\"\",\"request_body\":\"e30=\",\"request_headers\":{\"connection\":[\"upgrade\"],\"host\":[\"3.228.92.161\"],\"x-real-ip\":[\"172.31.5.55\"],\"x-forwarded-for\":[\"138.199.34.206, 172.31.5.55\"],\"content-length\":[\"716\"],\"x-forwarded-proto\":[\"http\"],\"x-forwarded-port\":[\"80\"],\"x-amzn-trace-id\":[\"Root=1-65e01ff0-22108e1659ac458930d6e9b0\"],\"user-agent\":[\"Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:92.0) Gecko/20100101 Firefox/92.0\"],\"accept-encoding\":[\"gzip, deflate\"],\"content-type\":[\"application/x-www-form-urlencoded\"]},\"response_body\":\"Tm90IEZvdW5k\",\"response_headers\":{\"x-powered-by\":[\"Express\"],\"vary\":[\"Origin\"],\"access-control-allow-credentials\":[\"true\"],\"content-type\":[\"text/html; charset=utf-8\"],\"content-length\":[\"9\"],\"etag\":[\"W/\\\"9-0gXL1ngzMqISxa6S1zx3F4wtLyg\\\"\"]},\"sdk_type\":\"JsExpress\",\"status_code\":404,\"timestamp\":\"" <> toString nowTxt <> "\",\"url_path\":\"/service/extension/backup/mboximport\",\"errors\":[],\"tags\":[],\"msg_id\":\"fdadc75d-5710-49cd-adfd-2d7547c9ab17\"}"
      let msgs = [("m1", encodeUtf8 jsonMsg)]
      resp <- runTestBg frozenTime tr $ processMessages msgs HashMap.empty
      case resp of
        Right (ids, _poison) -> ids `shouldBe` ["m1"]
        Left _ -> expectationFailure "processMessages returned Left WriteFailure"

    it "should save the request" $ \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg2)
            ]
      resp <- runTestBg frozenTime tr $ processMessages msgs HashMap.empty
      case resp of
        Right (ids, _poison) -> ids `shouldBe` ["m1", "m2"]
        Left _ -> expectationFailure "processMessages returned Left WriteFailure"

    -- Ingest silently declines to store anything for a free-tier project that is over its
    -- daily cap, while still acking every message. That is deliberate policy, but it is
    -- indistinguishable from data loss unless something pins it, so: the batch is acked in
    -- full and no span reaches the store. Seeded through the project cache because the real
    -- count is a live COUNT(*) and 10,000 rows is not a unit of test setup.
    it "freeTierOverDailyCap_acksTheBatchAndStoresNothing" \tr -> do
      let overCap = def{Projects.paymentPlan = "Free", Projects.dailyEventCount = 10001} :: Projects.ProjectCache
      liftIO $ Cache.insert tr.trProjectCache pid overCap
      nowTxt <- toText . formatTime defaultTimeLocale "%FT%T%QZ" <$> getCurrentTime
      let reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
          msgs = [("over-cap", toStrict $ AE.encode reqMsg)]
      let spanCount = withPool tr.trPool (DBT.query [sql| SELECT count(*) FROM otel_logs_and_spans WHERE project_id = ? |] (Only pid)) :: IO (V.Vector (Only Int64))
      before' <- spanCount
      runTestBg frozenTime tr (processMessages msgs HashMap.empty) >>= \case
        Right (ids, poison) -> (ids, poison) `shouldBe` (["over-cap"], [])
        Left _ -> expectationFailure "processMessages returned Left WriteFailure"
      spanCount >>= (`shouldBe` before')
      liftIO $ Cache.delete tr.trProjectCache pid

    -- Regression: hashes used to arrive only via the extraction worker's hash-merge
    -- UPDATE, which minted a duplicate row version per span in TimeFusion's
    -- merge-on-read store (2x scan cost on every dashboard query). Ingest now stamps
    -- them before insert ('stampHashesAtIngest'), so rows must carry the endpoint
    -- hash before any background processing runs.
    it "stampHashesAtIngest_rowsCarryEndpointHashBeforeBackgroundProcessing" $ \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
          reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      resp <- runTestBg frozenTime tr $ processMessages [("m1", toStrict $ AE.encode reqMsg1)] HashMap.empty
      whenLeft_ resp \_ -> expectationFailure "processMessages returned Left WriteFailure"
      -- No drainExtractionWorker / runAllBackgroundJobs: the hash must be there already.
      hashes <- withPool tr.trPool $ DBT.query [sql| SELECT unnest(hashes) FROM otel_logs_and_spans WHERE project_id = ? |] (Only pid) :: IO (V.Vector (Only Text))
      let expected = toXXHash $ pid.toText <> "172.31.29.11" <> "GET" <> "/"
      V.map fromOnly hashes `shouldBe` V.singleton expected

    it "We should expect 2 endpoints, albeit unacknowleged." $ \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg2)
            ]
      f <- runTestBg frozenTime tr $ processMessages msgs HashMap.empty
      drainExtractionWorker tr
      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs

      _ <- runAllBackgroundJobs frozenTime tr.trATCtx
      endpoints <- runTestBg frozenTime tr $ Endpoints.endpointRequestStatsByProject Endpoints.WithStats False pid False Nothing Nothing Nothing 0 200 "Incoming" "24h"
      V.length endpoints `shouldBe` 2
      forM_ endpoints \enp ->
        ["/", "/api/v1/user/login", "/service/extension/backup/mboximport"] `shouldContain` [enp.urlPath]
