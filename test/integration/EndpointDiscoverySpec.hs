module EndpointDiscoverySpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import ProcessMessage (tokenizeUrlPath)
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)
import Utils (toXXHash)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


-- | Insert test endpoints directly into DB, bypassing message ingestion.
insertTestEndpoints :: TestResources -> [(Text, Text, Text)] -> IO ()
insertTestEndpoints tr endpoints = forM_ endpoints \(method, host, path) ->
  void $ withPool tr.trPool $ DBT.execute
    [sql| INSERT INTO apis.endpoints (project_id, url_path, url_params, method, host, hash, outgoing)
          VALUES (?, ?, ?, ?, ?, ?, ?)
          ON CONFLICT (hash) DO NOTHING |]
    (pid, path, AE.object [], method, host, toXXHash (pid.toText <> host <> method <> path), False :: Bool)


-- | An open api_change issue pinned to one endpoint hash. Four of these across
-- four endpoints that later merge is what used to abort the whole cleanup on
-- @idx_issues_project_target_type_open@.
insertOpenIssue :: TestResources -> Text -> IO ()
insertOpenIssue tr endpointHash =
  void $ withPool tr.trPool $ DBT.execute
    [sql| INSERT INTO apis.issues (project_id, issue_type, endpoint_hash, target_hash, title)
          VALUES (?, 'api_change', ?, ?, 'test issue') |]
    (pid, endpointHash, endpointHash)


-- | Which open issues have been notified, and which are still deferred.
queryNotifiedSplit :: TestResources -> IO (Int, Int)
queryNotifiedSplit tr = do
  r <- withPool tr.trPool $ DBT.query
    [sql| SELECT count(*) FILTER (WHERE last_notified_at IS NOT NULL)::int,
                 count(*) FILTER (WHERE last_notified_at IS NULL)::int
          FROM apis.issues WHERE project_id = ? AND archived_at IS NULL |]
    (Only pid) :: IO (V.Vector (Int, Int))
  pure $ fromMaybe (0, 0) (r V.!? 0)


-- | target_hash of every open issue for this project.
queryOpenIssueTargets :: TestResources -> IO (V.Vector Text)
queryOpenIssueTargets tr =
  V.map (\(Only t) -> t) <$> withPool tr.trPool (DBT.query
    [sql| SELECT target_hash FROM apis.issues
          WHERE project_id = ? AND acknowledged_at IS NULL AND archived_at IS NULL |]
    (Only pid))


-- | Clear test endpoints for this project before each test group.
clearTestEndpoints :: TestResources -> IO ()
clearTestEndpoints tr =
  void $ withPool tr.trPool $ DBT.execute
    [sql| DELETE FROM apis.endpoints WHERE project_id = ? |]
    (Only pid)
    >> void (withPool tr.trPool $ DBT.execute [sql| DELETE FROM apis.issues WHERE project_id = ? |] (Only pid))


-- | After endpointTemplateDiscovery, merged endpoints are deleted and only the
-- canonical template endpoint remains. This queries for those templates.
queryCanonicalTemplates :: TestResources -> IO (V.Vector (Text, Text, Text))
queryCanonicalTemplates tr = withPool tr.trPool $ DBT.query
  [sql| SELECT url_path, method, host FROM apis.endpoints
        WHERE project_id = ? AND canonical_hash = hash
        ORDER BY method, url_path |]
  (Only pid)


-- | Query all remaining endpoints for this project.
queryAllEndpoints :: TestResources -> IO (V.Vector (Text, Text))
queryAllEndpoints tr = withPool tr.trPool $ DBT.query
  [sql| SELECT url_path, method FROM apis.endpoints
        WHERE project_id = ? ORDER BY method, url_path |]
  (Only pid)


spec :: Spec
spec = around withTestResources do
  describe "Endpoint Template Discovery" do
    describe "End-to-end: group, merge, clean up" do
      it "collapses ID-like paths onto one template and leaves everything else alone" \tr -> do
        clearTestEndpoints tr

        -- 1) ID-like endpoints  -> one {param} template
        -- 2) static sibling     -> survives untouched
        -- 3) shorter path       -> never merges into a longer template
        insertTestEndpoints tr
          [ ("GET", "api.example.com", "/api/v1/users/auth0|abc123def456")
          , ("GET", "api.example.com", "/api/v1/users/auth0|xyz789ghi012")
          , ("GET", "api.example.com", "/api/v1/users/google-oauth2|111222333")
          , ("GET", "api.example.com", "/api/v1/users/list-all")
          , ("GET", "api.example.com", "/api/v1/health")
          , ("GET", "api.example.com", "/api/v1/users")
          ]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid

        templatePaths <- V.toList . V.map (\(p, _, _) -> p) <$> queryCanonicalTemplates tr
        templatePaths `shouldSatisfy` elem "/api/v1/users/{param}"

        paths <- V.toList . V.map fst <$> queryAllEndpoints tr
        sort paths
          `shouldBe` ["/api/v1/health", "/api/v1/users", "/api/v1/users/list-all", "/api/v1/users/{param}"]

      it "running discovery twice is idempotent" \tr -> do
        clearTestEndpoints tr
        insertTestEndpoints tr
          [ ("GET", "api.example.com", "/api/v1/items/auth0|item001")
          , ("GET", "api.example.com", "/api/v1/items/auth0|item002")
          , ("GET", "api.example.com", "/api/v1/dashboard/overview")
          ]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid
        endpoints1 <- queryAllEndpoints tr

        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid
        queryAllEndpoints tr >>= (`shouldBe` endpoints1)

    describe "Edge cases" do
      it "requires at least 2 endpoints to form a tokenization template" \tr -> do
        clearTestEndpoints tr
        insertTestEndpoints tr
          [ ("GET", "api.example.com", "/api/v1/orders/provider|order001")
          ]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid

        templates <- queryCanonicalTemplates tr
        V.length templates `shouldBe` 0

      it "groups by method and host independently" \tr -> do
        clearTestEndpoints tr
        insertTestEndpoints tr
          [ ("GET", "api.example.com", "/api/v1/items/provider|item001")
          , ("GET", "api.example.com", "/api/v1/items/provider|item002")
          , ("POST", "api.example.com", "/api/v1/items/provider|item001")
          , ("POST", "api.example.com", "/api/v1/items/provider|item002")
          ]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid

        templates <- queryCanonicalTemplates tr
        V.length templates `shouldBe` 2
        forM_ templates \(urlPath, _, _) ->
          urlPath `shouldBe` "/api/v1/items/{param}"

    describe "tokenizeUrlPath" do
      it "normalizes HTTP status codes to format strings" \_ ->
        V.toList (tokenizeUrlPath "/api/v1/users/200") `shouldBe` ["", "api", "v1", "users", "{http_status}"]

      it "normalizes UUIDs to format string" \_ -> do
        let result = V.toList $ tokenizeUrlPath "/api/v1/users/550e8400-e29b-41d4-a716-446655440000"
        viaNonEmpty last result `shouldBe` Just "{uuid}"

      it "normalizes compound IDs (auth0|xxx) to <*>" \_ ->
        V.toList (tokenizeUrlPath "/api/v2/users/auth0|69a7015edae92e991cd72d91")
          `shouldBe` ["", "api", "v2", "users", "<*>"]

      it "keeps static segments unchanged" \_ ->
        V.toList (tokenizeUrlPath "/api/v1/health") `shouldBe` ["", "api", "v1", "health"]

    -- The failure this whole change exists to prevent: a customer whose primary
    -- key is "SB-<hex>" got 18,213 endpoint rows for ~1,900 routes, because no
    -- rule recognised a prefixed id. Prefixed/encoded/short-hex ids must collapse
    -- and route words must not, and the two have to hold simultaneously.
    describe "Prefixed and encoded ids collapse; route words survive" do
      it "collapses SB-<hex>, WALLET_<hex> and percent-encoded ids into one template each" \tr -> do
        clearTestEndpoints tr
        insertTestEndpoints tr
          $ [("GET", "api.example.com", "/v1/shipments/tracking/SB-" <> h) | h <- ["91673E0634FB", "5A3469F964B1", "F76B055211BA"]]
          <> [("GET", "api.example.com", "/v1/wallet/complete-fund/WALLET_" <> h) | h <- ["3D16EFF38182", "A1B2C3D4E5F6"]]
          <> [("GET", "api.example.com", "/api/v1/customers/auth0%7C" <> h) | h <- ["6a5ebc849d77a9a4717b0980", "66a12f77bc56dad584b79c8b"]]
          <> [("GET", "api.example.com", "/v1/shipbubble/products/" <> h <> "/stock_status") | h <- ["27ad70cdce4a", "40e1beed4b9e"]]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid

        paths <- V.toList . V.map fst <$> queryAllEndpoints tr
        sort paths
          `shouldBe` [ "/api/v1/customers/{param}"
                     , "/v1/shipbubble/products/{param}/stock_status"
                     , "/v1/shipments/tracking/{param}"
                     , "/v1/wallet/complete-fund/{param}"
                     ]

      it "never folds a flat family of route words into a parameter" \tr -> do
        clearTestEndpoints tr
        let verbs = ["verify_phone", "verify_email", "deactivate_user", "reset-password", "send_otp", "biometric_login", "update_email", "random_username", "transactions", "notifications"]
        insertTestEndpoints tr [("POST", "api.example.com", "/v1/account/" <> v) | v <- verbs]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid

        paths <- V.toList . V.map fst <$> queryAllEndpoints tr
        sort paths `shouldBe` sort [("/v1/account/" <>) v | v <- verbs]

      it "keeps a literal route alongside the wildcard one it sits next to" \tr -> do
        clearTestEndpoints tr
        insertTestEndpoints tr
          $ ("GET", "api.example.com", "/v1/orders/search")
          : ("GET", "api.example.com", "/v1/orders/summary")
          : [("GET", "api.example.com", "/v1/orders/SB-" <> h) | h <- ["91673E0634FB", "5A3469F964B1", "F76B055211BA"]]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid

        paths <- V.toList . V.map fst <$> queryAllEndpoints tr
        sort paths `shouldBe` ["/v1/orders/search", "/v1/orders/summary", "/v1/orders/{param}"]

      it "assigns a later straggler to an existing template without re-deriving it" \tr -> do
        clearTestEndpoints tr
        insertTestEndpoints tr [("GET", "api.example.com", "/v1/track/SB-" <> h) | h <- ["91673E0634FB", "5A3469F964B1"]]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid

        -- One new concrete path, alone: too little evidence to mint a template,
        -- but the template already exists and vouches for it.
        insertTestEndpoints tr [("GET", "api.example.com", "/v1/track/SB-000000000001")]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid

        paths <- V.toList . V.map fst <$> queryAllEndpoints tr
        paths `shouldBe` ["/v1/track/{param}"]

    -- The evidence bar exists because a "param" verdict deletes issues if it is
    -- wrong and nothing downstream can check it. These pin the two conditions
    -- that carry the weight: agreement across passes, and a population that is
    -- still growing.
    describe "Evidence required before an LLM verdict may merge" do
      it "refuses a single verdict, a closed set, and a group containing a route word" \_ -> do
        -- one pass is not agreement, however large
        BackgroundJobs.mergeEvidenceMet 1 8 500 ["a1b2c3d4"] `shouldBe` False
        -- confirmed repeatedly, but the family never grew: a closed set of verbs
        BackgroundJobs.mergeEvidenceMet 9 20 20 ["a1b2c3d4"] `shouldBe` False
        -- grew and was confirmed, but one member reads as a route
        BackgroundJobs.mergeEvidenceMet 3 8 40 ["a1b2c3d4", "deactivate_user"] `shouldBe` False
        -- too small to be a population
        BackgroundJobs.mergeEvidenceMet 3 2 5 ["a1b2c3d4"] `shouldBe` False
        -- confirmed twice, grew, big enough, nothing word-like
        BackgroundJobs.mergeEvidenceMet 2 8 40 ["a1b2c3d4", "SHO3KOOWWN", "00Zj"] `shouldBe` True

      it "will not veto ids that merely look like words to a naive test" \_ ->
        map BackgroundJobs.looksLikeRouteWord ["cus_QpeOrF3HMRjazD", "SHO3KOOWWN", "a-df0u-05mwux", "00Zj"]
          `shouldBe` [False, False, False, False]

    describe "Merge cleanup consolidates rather than colliding" do
      it "folds many endpoints' open issues onto one canonical issue per type" \tr -> do
        clearTestEndpoints tr
        let hashes = ["SB-" <> h | h <- ["91673E0634FB", "5A3469F964B1", "F76B055211BA", "D5AC39C066E5"]]
        insertTestEndpoints tr [("GET", "api.example.com", "/v1/ship/" <> h) | h <- hashes]
        -- One open api_change issue per endpoint: 4 rows that all want the same
        -- (project_id, target_hash, issue_type) once their endpoints merge.
        forM_ hashes \h -> insertOpenIssue tr $ toXXHash (pid.toText <> "api.example.com" <> "GET" <> "/v1/ship/" <> h)
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid

        paths <- V.toList . V.map fst <$> queryAllEndpoints tr
        paths `shouldBe` ["/v1/ship/{param}"]
        -- Survives as exactly one issue, pointed at the canonical endpoint.
        issues <- queryOpenIssueTargets tr
        V.length issues `shouldBe` 1
        issues V.!? 0 `shouldBe` Just (toXXHash (pid.toText <> "api.example.com" <> "GET" <> "/v1/ship/{param}"))

      -- The ordering bug this guards: a concrete path is announced by mail the
      -- moment it is ingested, and merged away hours later. Discovery is what
      -- decides whether it was ever a distinct route, so it is what announces it.
      it "announces only the endpoints that survive discovery, not the ones it merges away" \tr -> do
        clearTestEndpoints tr
        let ids = ["SB-" <> h | h <- ["91673E0634FB", "5A3469F964B1", "F76B055211BA"]]
        insertTestEndpoints tr $ ("GET", "api.example.com", "/v1/ship/status") : [("GET", "api.example.com", "/v1/ship/" <> h) | h <- ids]
        -- Every endpoint arrives with an un-notified issue, exactly as ingest leaves them.
        forM_ ("status" : ids) \h -> insertOpenIssue tr $ toXXHash (pid.toText <> "api.example.com" <> "GET" <> "/v1/ship/" <> h)
        (notifiedBefore, deferredBefore) <- queryNotifiedSplit tr
        notifiedBefore `shouldBe` 0
        deferredBefore `shouldBe` 4

        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid

        -- The three ids collapsed into one template and their issues were
        -- consolidated; only the literal route is left to announce.
        paths <- V.toList . V.map fst <$> queryAllEndpoints tr
        sort paths `shouldBe` ["/v1/ship/status", "/v1/ship/{param}"]
        (notifiedAfter, _) <- queryNotifiedSplit tr
        notifiedAfter `shouldSatisfy` (< 4)

      it "is re-runnable: a second cleanup pass changes nothing" \tr -> do
        clearTestEndpoints tr
        insertTestEndpoints tr [("GET", "api.example.com", "/v1/x/SB-" <> h) | h <- ["91673E0634FB", "5A3469F964B1"]]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid
        before <- queryAllEndpoints tr
        runTestBg frozenTime tr $ BackgroundJobs.endpointMergeCleanup pid
        queryAllEndpoints tr >>= (`shouldBe` before)
