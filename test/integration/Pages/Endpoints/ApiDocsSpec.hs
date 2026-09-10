-- | The learned-OpenAPI surface of the API catalog: what /api_catalog/docs
-- renders and what the openapi.json / openapi.yaml routes serve.
--
-- Seeding goes through the real schema-learning path ('Hot.observeSpans' +
-- 'Worker.flushDirty') rather than hand-written catalog rows, so the spec
-- breaks if the shape the learner writes ever drifts from the shape the
-- generator reads.
module Pages.Endpoints.ApiDocsSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _Object, _String)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Lens ((^?))
import Lucid (renderText, toHtml)
import Models.Projects.Projects qualified as Projects
import Pages.Endpoints qualified as ApiCatalog
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.SchemaLearning.Hot qualified as Hot
import Pkg.SchemaLearning.Worker qualified as Worker
import Pkg.TestUtils (TestResources (..), frozenTime, runHasqlEffect, testServant, withTestResources)
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, sequential, shouldBe, shouldSatisfy)
import Utils (toXXHash)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


host :: Text
host = "api.example.com"


-- | One learned endpoint with every category the generator reads: a templated
-- path, a query param, a request header, a nested request body and an array
-- response body.
seededPath :: Text
seededPath = "/v1/stores/:store_id/orders"


seededHash :: Text
seededHash = toXXHash (pid.toText <> host <> "POST" <> seededPath)


observation :: UTCTime -> Hot.ObservationInput
observation ts =
  Hot.ObservationInput
    { keyKind = Catalog.HttpEndpoint
    , keyHash = seededHash
    , scope =
        Catalog.Scope
          { Catalog.service = Just "orders-api"
          , Catalog.spanName = Just "POST /v1/stores/:store_id/orders"
          , Catalog.kind = Just "server"
          , Catalog.host = Just host
          , Catalog.method = Just "POST"
          , Catalog.urlPath = Just seededPath
          , Catalog.statusCodes = V.fromList [201]
          }
    , walk = \_ ->
        [ (p, V.singleton (v, fmt), c)
        | (p, v, fmt, c) <-
            [ ("store_id", AE.String "st_123", Just "{uuid}", Catalog.FCPathParam)
            , ("page[*]", AE.String "2", Just "{integer}", Catalog.FCQueryParam)
            , ("x-api-key", AE.String "k", Nothing, Catalog.FCRequestHeader)
            , ("content-type", AE.String "application/json", Nothing, Catalog.FCResponseHeader)
            , ("customer.email", AE.String "a@b.co", Just "{email}", Catalog.FCRequestBody)
            , ("items[*].sku", AE.String "TK-1", Nothing, Catalog.FCRequestBody)
            , ("items[*].qty", AE.Number 2, Nothing, Catalog.FCRequestBody)
            , ("[*].order_id", AE.String "SB-1", Nothing, Catalog.FCResponseBody)
            ]
        ]
    , timestamp = ts
    }


-- | Learn the endpoint. The field walk is sampled, so a single observation is
-- not enough to guarantee the body fields land — the same threshold
-- 'SchemaLearningSpec' uses.
seed :: TestResources -> IO ()
seed tr = do
  void $ withPool tr.trPool $ DBT.execute [sql| DELETE FROM apis.schema_catalog WHERE project_id = ? |] (Only pid)
  ref <- newIORef Hot.emptySchemaShardState
  Hot.observeSpans ref Hot.defaultPolicy pid (V.replicate 201 (observation frozenTime))
  void $ runHasqlEffect tr (Worker.flushDirty ref)


fetchSpec :: TestResources -> Maybe Text -> IO AE.Value
fetchSpec tr endpointM = snd <$> testServant tr (ApiCatalog.apiSpecJsonH pid (Just host) (Just "Incoming") endpointM)


spec :: Spec
spec = sequential $ aroundAll withTestResources $ describe "API catalog – learned OpenAPI" do
  it "documents a learned endpoint as a valid-shaped OpenAPI 3.1 operation" \tr -> do
    seed tr
    doc <- fetchSpec tr Nothing
    doc ^? key "openapi" . _String `shouldBe` Just "3.1.0"
    -- ':store_id' is a framework route spelling; OpenAPI needs '{store_id}'.
    let op = fromMaybe AE.Null $ doc ^? key "paths" . key "/v1/stores/{store_id}/orders" . key "post"
    op ^? key "operationId" . _String `shouldBe` Just seededHash
    op ^? key "tags" . _Array `shouldBe` Just (V.singleton (AE.String "orders-api"))

    -- Every location the catalog can learn a parameter in. The path variable is
    -- required; learned traffic can't prove the others are.
    let located l = [p | p <- maybe [] V.toList (op ^? key "parameters" . _Array), p ^? key "in" . _String == Just l]
    map (^? key "name" . _String) (located "path") `shouldBe` [Just "store_id"]
    map (^? key "required") (located "path") `shouldBe` [Just (AE.Bool True)]
    -- 'page[*]' is how the value walk spells a repeated query param.
    map (^? key "name" . _String) (located "query") `shouldBe` [Just "page"]
    map (^? key "name" . _String) (located "header") `shouldBe` [Just "x-api-key"]

    -- Flattened body paths are rebuilt into nested JSON Schema, arrays included.
    let reqSchema = fromMaybe AE.Null $ op ^? key "requestBody" . key "content" . key "application/json" . key "schema"
    reqSchema ^? key "properties" . key "customer" . key "properties" . key "email" . key "format" . _String `shouldBe` Just "email"
    reqSchema ^? key "properties" . key "items" . key "type" . _String `shouldBe` Just "array"
    reqSchema ^? key "properties" . key "items" . key "items" . key "properties" . key "qty" . key "type" . _String `shouldBe` Just "number"

    -- The observed status carries the response, and the merged response schema
    -- lives in components so several statuses can share one definition.
    let resp = fromMaybe AE.Null $ op ^? key "responses" . key "201"
    resp ^? key "headers" . key "content-type" `shouldSatisfy` isJust
    resp ^? key "content" . key "application/json" . key "schema" . key "$ref" . _String
      `shouldBe` Just ("#/components/schemas/" <> seededHash <> "Response")
    doc ^? key "components" . key "schemas" . key (fromString $ toString (seededHash <> "Response")) . key "type" . _String
      `shouldBe` Just "array"

  it "scopes the spec to one endpoint and serves the same document as YAML" \tr -> do
    seed tr
    scoped <- fetchSpec tr (Just seededHash)
    fmap length (scoped ^? key "paths" . _Object) `shouldBe` Just 1
    -- A hash no endpoint carries documents nothing, rather than falling back to
    -- the whole host.
    unknown <- fetchSpec tr (Just "deadbeef")
    fmap length (unknown ^? key "paths" . _Object) `shouldBe` Just 0

    (_, yamlText) <- testServant tr (ApiCatalog.apiSpecYamlH pid (Just host) (Just "Incoming") (Just seededHash))
    either (const AE.Null) id (Yaml.decodeEither' @AE.Value (encodeUtf8 yamlText)) `shouldBe` scoped

  it "renders the docs page with a Swagger UI mount pointed at the spec route" \tr -> do
    seed tr
    (_, page) <- testServant tr (ApiCatalog.apiDocsH pid (Just host) (Just "Incoming") (Just seededHash))
    let html = toStrict $ renderText $ toHtml page
    forM_
      [ "id=\"swagger-ui\""
      , "swagger-ui/swagger-ui-bundle."
      , "api_catalog/openapi.json?host="
      , "openapi.yaml"
      , "1 learned operation"
      ]
      \needle -> html `shouldSatisfy` T.isInfixOf needle
