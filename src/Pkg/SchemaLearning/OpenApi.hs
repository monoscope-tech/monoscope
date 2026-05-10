{-# LANGUAGE OverloadedRecordDot #-}

-- | On-demand OpenAPI emitter from the schema-learning catalog. Pure: takes
-- a vector of catalog entries (the HTTP-keyed subset) and returns a minimal
-- 'OpenApi' document. Not wired to a route in this PR — the entry point is
-- 'fromCatalog' which the per-host endpoint can call once a UI is added.
module Pkg.SchemaLearning.OpenApi (
  fromCatalog,
)
where

import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict.InsOrd qualified as IOH
import Data.OpenApi (OpenApi)
import Data.OpenApi qualified as OA
import Data.Text qualified as T
import Data.Vector qualified as V
import Pkg.SchemaLearning.Catalog (CatalogEntry, KeyKind (..))
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Relude


-- | Emit a minimal OpenAPI 3.0 document describing the HTTP endpoints in a
-- vector of 'CatalogEntry's. Non-HTTP keys are skipped; the document
-- includes only paths + methods for now (request/response schemas can be
-- layered on by walking 'FieldStruct's in a follow-up).
--
-- The intent is one document per @(project, host)@ — callers filter the
-- catalog vector before invoking.
fromCatalog :: V.Vector CatalogEntry -> OpenApi
fromCatalog entries =
  mempty
    { OA._openApiInfo =
        mempty
          { OA._infoTitle = "Discovered API"
          , OA._infoVersion = "1.0.0"
          , OA._infoDescription = Just "Generated from monoscope schema-learning catalog."
          }
    , OA._openApiPaths = IOH.fromList $ HM.toList byPath
    }
  where
    httpEntries = V.filter (\e -> e.template.keyKind == HttpEndpoint) entries
    byPath :: HashMap FilePath OA.PathItem
    byPath = V.foldl' addEntry HM.empty httpEntries

    addEntry :: HashMap FilePath OA.PathItem -> CatalogEntry -> HashMap FilePath OA.PathItem
    addEntry acc e =
      let p = toString (fromMaybe "/" e.scope.urlPath)
          item = HM.lookupDefault mempty p acc
          item' = applyMethod (fromMaybe "GET" e.scope.method) (mkOperation e) item
       in HM.insert p item' acc

    mkOperation :: CatalogEntry -> OA.Operation
    mkOperation e =
      mempty
        { OA._operationDescription =
            Just $ "Auto-discovered. Sample count: " <> T.pack (show e.sampleCount)
        }

    applyMethod :: Text -> OA.Operation -> OA.PathItem -> OA.PathItem
    applyMethod m op item = case T.toUpper m of
      "GET" -> item{OA._pathItemGet = Just op}
      "POST" -> item{OA._pathItemPost = Just op}
      "PUT" -> item{OA._pathItemPut = Just op}
      "DELETE" -> item{OA._pathItemDelete = Just op}
      "PATCH" -> item{OA._pathItemPatch = Just op}
      "HEAD" -> item{OA._pathItemHead = Just op}
      "OPTIONS" -> item{OA._pathItemOptions = Just op}
      _ -> item
