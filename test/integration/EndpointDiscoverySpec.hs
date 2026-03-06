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
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)
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


-- | Clear test endpoints for this project before each test group.
clearTestEndpoints :: TestResources -> IO ()
clearTestEndpoints tr =
  void $ withPool tr.trPool $ DBT.execute
    [sql| DELETE FROM apis.endpoints WHERE project_id = ? |]
    (Only pid)


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


-- | Count endpoints with embeddings stored.
queryEmbeddedCount :: TestResources -> IO Int
queryEmbeddedCount tr = do
  result <- withPool tr.trPool $ DBT.query
    [sql| SELECT COUNT(*)::int FROM apis.endpoints
          WHERE project_id = ? AND embedding IS NOT NULL |]
    (Only pid) :: IO (V.Vector (Only Int))
  pure $ maybe 0 (\(Only n) -> n) $ result V.!? 0


spec :: Spec
spec = aroundAll withTestResources do
  describe "Endpoint Template Discovery" do
    describe "End-to-end: tokenization + embedding + merge + cleanup" do
      it "full pipeline: tokenizes ID-like paths, embeds static paths, merges similar, cleans up" \tr -> do
        clearTestEndpoints tr

        -- Mix of endpoint types in a single batch:
        -- 1) ID-like endpoints → should be tokenized to {param} template
        -- 2) Static endpoint → should be embedded, become centroid
        -- 3) Unrelated endpoint → should survive untouched
        -- 4) Short path → should NOT merge with longer paths (segment count guard)
        insertTestEndpoints tr
          [ ("GET", "api.example.com", "/api/v1/users/auth0|abc123def456")
          , ("GET", "api.example.com", "/api/v1/users/auth0|xyz789ghi012")
          , ("GET", "api.example.com", "/api/v1/users/google-oauth2|111222333")
          , ("GET", "api.example.com", "/api/v1/users/list-all")
          , ("GET", "api.example.com", "/api/v1/health")
          , ("GET", "api.example.com", "/api/v1/users")
          ]

        -- Pass 1: tokenization merges auth0 endpoints, embeds static ones as centroids
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid

        -- auth0 endpoints should be merged into {param} template
        templates1 <- queryCanonicalTemplates tr
        let templatePaths = V.toList $ V.map (\(p, _, _) -> p) templates1
        templatePaths `shouldSatisfy` elem "/api/v1/users/{param}"

        -- /api/v1/users (short path) should survive independently — not merged with 4-segment paths
        usersEndpoint <- withPool tr.trPool $ DBT.query
          [sql| SELECT url_path FROM apis.endpoints
                WHERE project_id = ? AND url_path = '/api/v1/users' |]
          (Only pid) :: IO (V.Vector (Only Text))
        V.length usersEndpoint `shouldBe` 1

        -- Static endpoints should have embeddings now
        embCount1 <- queryEmbeddedCount tr
        embCount1 `shouldSatisfy` (>= 2)

        -- Pass 2: add a semantically similar endpoint to trigger embedding comparison
        -- "get-all" vs "list-all" cosine ~0.90 → ambiguous zone → goes to LLM judge
        insertTestEndpoints tr
          [ ("GET", "api.example.com", "/api/v1/users/get-all")
          ]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid

        -- get-all should now have an embedding (it went through the embedding pipeline)
        embCount2 <- queryEmbeddedCount tr
        embCount2 `shouldSatisfy` (> embCount1)

        -- Both endpoints should still exist (the LLM judge processes the ambiguous pair
        -- and its golden response determines the merge outcome)
        allPaths <- withPool tr.trPool $ DBT.query
          [sql| SELECT url_path FROM apis.endpoints
                WHERE project_id = ? ORDER BY url_path |]
          (Only pid) :: IO (V.Vector (Only Text))
        let paths = V.toList $ V.map (\(Only p) -> p) allPaths
        -- /api/v1/health should still exist independently (not similar to users)
        paths `shouldSatisfy` elem "/api/v1/health"
        -- The {param} template from tokenization should survive
        paths `shouldSatisfy` elem "/api/v1/users/{param}"
        -- /api/v1/users (short path) should survive (segment count guard)
        paths `shouldSatisfy` elem "/api/v1/users"

      it "running discovery twice is idempotent" \tr -> do
        clearTestEndpoints tr
        insertTestEndpoints tr
          [ ("GET", "api.example.com", "/api/v1/items/auth0|item001")
          , ("GET", "api.example.com", "/api/v1/items/auth0|item002")
          , ("GET", "api.example.com", "/api/v1/dashboard/overview")
          ]
        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        endpoints1 <- queryAllEndpoints tr

        runTestBg frozenTime tr $ BackgroundJobs.endpointTemplateDiscovery pid
        endpoints2 <- queryAllEndpoints tr
        endpoints1 `shouldBe` endpoints2

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
