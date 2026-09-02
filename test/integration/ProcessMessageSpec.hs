module ProcessMessageSpec (spec) where

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HashMap
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (AesonText (..), UUIDId (..))
import Pkg.TestUtils
import ProcessMessage (PathClassifier (..), emptyPathClassifier, processMessages, processSpanToEntities)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec (Spec, around, describe, expectationFailure, it, shouldBe, shouldContain)
import Utils (toXXHash)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


-- | A span with every optional field empty, for tests that care about one or two
-- attributes and nothing else.
emptySpan :: UTCTime -> Telemetry.OtelLogsAndSpans
emptySpan now =
  Telemetry.OtelLogsAndSpans
    { project_id = pid.toText
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


spec :: Spec
spec = around withTestResources do
  describe "processSpanToEntities" do
    -- Regression: an unparseable project_id used to be `Unsafe.fromJust . UUID.fromText`,
    -- crashing the whole ingestion batch. The owning ProjectId is now threaded in typed,
    -- so the span's own project_id text is never parsed and a garbage value can't crash.
    it "does not crash on an unparseable project_id and stamps the threaded ProjectId" $ \_ -> do
      now <- getCurrentTime
      let badSpan = (emptySpan now){Telemetry.project_id = "not-a-uuid"}
          (mkEndpoint, _hashes, _, _) = processSpanToEntities emptyPathClassifier Projects.defaultProjectCache pid badSpan
      fmap (.projectId) (mkEndpoint UUID.nil) `shouldBe` Just pid

  -- http.route is the one place a template arrives as fact rather than
  -- inference. It was being preferred over url.path and then handed, in the
  -- framework's own syntax, to code that expects a URL.
  describe "http.route is taken as the route the framework matched" do
    let route :: Text -> Text -> UTCTime -> Telemetry.OtelLogsAndSpans
        route r p now =
          (emptySpan now)
            { Telemetry.kind = Just "server"
            , Telemetry.attributes =
                Just
                  $ AesonText
                  $ fromList
                    [ ("http", AE.object ["request" AE..= AE.object ["method" AE..= ("GET" :: Text)], "route" AE..= r])
                    , ("url", AE.object ["path" AE..= p])
                    ]
            }
        pathOf classifier cache sp = (\(_, _, np, _) -> np) $ processSpanToEntities classifier cache pid sp
        fwHashOf classifier cache sp = (\(_, _, _, fw) -> fw) $ processSpanToEntities classifier cache pid sp

    -- Django writes a Python named-group regex. normalizeUrlPath then truncated
    -- it at the '?' of "(?P<", which is how "api/v1/wallets/(" became 162 real
    -- endpoints across four projects.
    it "httpRoute_djangoNamedGroupRegex_becomesATemplateNotATruncatedRegex" \_ -> do
      now <- getCurrentTime
      let sp = route "api/v1/wallets/(?P<pk>[^/.]+)/initiate_bank_transfer/$" "/api/v1/wallets/75261113-6245-431c-a1d0-bb45bcb9f7e4/initiate_bank_transfer/" now
      pathOf emptyPathClassifier Projects.defaultProjectCache sp
        `shouldBe` Just "/api/v1/wallets/{pk}/initiate_bank_transfer/"
      -- New, and the framework's own word for it, so it becomes its own
      -- canonical template and discovery may never merge it away.
      fwHashOf emptyPathClassifier Projects.defaultProjectCache sp `shouldBe` Just (toXXHash $ pid.toText <> "" <> "GET" <> "/api/v1/wallets/{pk}/initiate_bank_transfer/")

    -- Adopting framework parameter names renames endpoints, and a rename is a
    -- new hash. A project that already has the flattened row keeps it rather
    -- than re-minting its whole table and re-notifying every route.
    it "httpRoute_projectAlreadyHasFlattenedRow_doesNotReMintUnderTheFrameworkName" \_ -> do
      now <- getCurrentTime
      let existing = toXXHash $ pid.toText <> "" <> "GET" <> "/v1/rates/{param}/history"
          classifier = emptyPathClassifier{knownHashes = fromList [existing]}
          cache = Projects.defaultProjectCache{Projects.endpointHashes = V.singleton existing}
          sp = route "/v1/rates/:id/history" "/v1/rates/8821/history" now
      pathOf classifier cache sp `shouldBe` Just "/v1/rates/{param}/history"
      fwHashOf classifier cache sp `shouldBe` Nothing

    -- The legacy SDK bridge synthesises http.route from the raw URL. It carries
    -- no parameter, so it is not authority and the id heuristics still run.
    it "httpRoute_synthesisedFromConcreteUrl_isNotTreatedAsAuthority" \_ -> do
      now <- getCurrentTime
      let sp = route "/users/550e8400-e29b-41d4-a716-446655440000" "/users/550e8400-e29b-41d4-a716-446655440000" now
      pathOf emptyPathClassifier Projects.defaultProjectCache sp `shouldBe` Just "/users/{uuid}"
      fwHashOf emptyPathClassifier Projects.defaultProjectCache sp `shouldBe` Nothing

    -- A catch-all has no literal segment to anchor it. Promoting one to a
    -- canonical template would let it swallow every path of its length.
    it "httpRoute_catchAllWithNoLiteralSegment_isNotPromotedToATemplate" \_ -> do
      now <- getCurrentTime
      let sp = route "/{path}" "/anything" now
      fwHashOf emptyPathClassifier Projects.defaultProjectCache sp `shouldBe` Nothing

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

    -- http.route is promoted out of the `attributes` blob into its own column,
    -- on both dual-write legs. One insert statement serves both, so a column
    -- present on one store and absent on the other fails every batch — this
    -- asserts the Postgres half exists and is populated (migration 0141).
    it "httpRoute_isWrittenToItsPromotedColumn_notOnlyTheAttributesBlob" $ \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
          reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      resp <- runTestBg frozenTime tr $ processMessages [("m1", toStrict $ AE.encode reqMsg1)] HashMap.empty
      whenLeft_ resp \_ -> expectationFailure "processMessages returned Left WriteFailure"
      routes <-
        withPool tr.trPool
          $ DBT.query [sql| SELECT attributes___http___route FROM otel_logs_and_spans WHERE project_id = ? |] (Only pid)
          :: IO (V.Vector (Only (Maybe Text)))
      V.mapMaybe fromOnly routes `shouldBe` V.singleton "/"

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
      endpoints <- runTestBg frozenTime tr $ Endpoints.endpointRequestStatsByProject Endpoints.WithStats False pid Endpoints.EndpointQuery{direction = Endpoints.Incoming, archived = False, host = Nothing, search = Nothing, sort = Endpoints.SortEvents, page = 0, perPage = 200, period = Endpoints.Window24h}
      V.length endpoints `shouldBe` 2
      forM_ endpoints \enp ->
        ["/", "/api/v1/user/login", "/service/extension/backup/mboximport"] `shouldContain` [enp.urlPath]
