{-# LANGUAGE OverloadedRecordDot #-}

-- | Render learned schema-catalog entries as an OpenAPI 3.1 document.
--
-- Everything here is pure: the DB layer hands us @(endpointHash, CatalogEntry)@
-- pairs (see "Models.Apis.SchemaCatalog".@endpointCatalog@) and we fold them
-- into one spec. Nothing is persisted — the old @projects.swagger_jsons@ table
-- was dropped in migration @0059@ and the spec is cheap enough to rebuild per
-- request.
--
-- The interesting part is 'pathTokens': the catalog stores flattened field
-- paths (@menu.items[*].name@) and OpenAPI wants nested JSON Schema, so we
-- rebuild the tree.
module Pkg.OpenApi (buildSpec, specYaml, pathTemplate, pathTokens, jsonSchema) where

import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Yaml.Pretty qualified as YamlP
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Relude


-- $setup
-- >>> :set -XOverloadedStrings -XOverloadedRecordDot


-- ---------------------------------------------------------------------------
-- Field paths → JSON Schema

-- | Split a learned field path into its root array depth and its
-- @(name, array depth)@ segments. @[*]@ marks "the thing to its left is a
-- list"; a bare @[*]@ segment attaches to the previous name (or to the
-- document root when there is none).
--
-- >>> pathTokens "user.address.city"
-- (0,[("user",0),("address",0),("city",0)])
--
-- >>> pathTokens "[*].id"
-- (1,[("id",0)])
--
-- >>> pathTokens "menu.items[*].name"
-- (0,[("menu",0),("items",1),("name",0)])
--
-- >>> pathTokens ".menu.[*].a"
-- (0,[("menu",1),("a",0)])
--
-- >>> pathTokens "grid[*][*]"
-- (0,[("grid",2)])
--
-- >>> pathTokens ""
-- (0,[])
pathTokens :: Text -> (Int, [(Text, Int)])
pathTokens = go 0 [] . filter (not . T.null) . T.splitOn "."
  where
    go !rootD acc [] = (rootD, reverse acc)
    go rootD acc (s : ss) = case (stars s, acc) of
      (("", d), (n, d0) : rest) -> go rootD ((n, d0 + d) : rest) ss
      (("", d), []) -> go (rootD + d) [] ss
      (seg, _) -> go rootD (seg : acc) ss
    stars t = maybe (t, 0) (second (+ 1) . stars) (T.stripSuffix "[*]" t)


-- | One field's evidence: its structure, sampled values, and value counts.
type Evidence = (Catalog.FieldStruct, Maybe Catalog.Examples, Maybe Catalog.TopK)


data SNode = SNode {leaf :: Maybe Evidence, kids :: Map Text (Int, SNode)}


emptyNode :: SNode
emptyNode = SNode Nothing mempty


-- | Build one JSON Schema object from a set of flattened field paths.
-- An empty set yields the empty schema (@{}@), which OpenAPI reads as
-- "anything" — correct for an endpoint we have never seen a body on.
--
-- >>> import Data.Aeson (encode)
-- >>> let s ts = (Catalog.FieldStruct (HS.fromList ts) mempty Catalog.FCResponseBody False, Nothing, Nothing)
-- >>> encode $ jsonSchema [("items[*].id", s [Catalog.FTNumber])]
-- "{\"properties\":{\"items\":{\"items\":{\"properties\":{\"id\":{\"type\":\"number\"}},\"type\":\"object\"},\"type\":\"array\"}},\"type\":\"object\"}"
--
-- The bare root path is the document itself, not a property named @""@:
--
-- >>> encode $ jsonSchema [("", s [Catalog.FTString])]
-- "{\"type\":\"string\"}"
jsonSchema :: [(Text, Evidence)] -> AE.Value
jsonSchema fields = wrapArray rootDepth (render tree)
  where
    (rootDepth, tree) = foldl' step (0, emptyNode) fields
    step (!d, !t) (path, ev) = let (d', segs) = pathTokens path in (max d d', insertAt segs ev t)

    insertAt [] ev n = n{leaf = Just ev}
    insertAt ((k, d) : rest) ev n =
      n{kids = M.alter (\m -> let (d0, sub) = fromMaybe (0, emptyNode) m in Just (max d0 d, insertAt rest ev sub)) k n.kids}

    render n
      | M.null n.kids = maybe (AE.object []) leafSchema n.leaf
      | otherwise =
          AE.object
            [ "type" .= ("object" :: Text)
            , "properties" .= AE.object [AEK.fromText k .= wrapArray d (render sub) | (k, (d, sub)) <- M.toList n.kids]
            ]


wrapArray :: Int -> AE.Value -> AE.Value
wrapArray n v
  | n <= 0 = v
  | otherwise = AE.object ["type" .= ("array" :: Text), "items" .= wrapArray (n - 1) v]


leafSchema :: Evidence -> AE.Value
leafSchema (fs, exs, tk) =
  AE.object
    $ typeKV
    <> maybe [] (\f -> ["format" .= f]) (listToMaybe $ mapMaybe openApiFormat $ sort $ HS.toList fs.formats)
    <> [("enum" :: AEK.Key) .= sort (HM.keys top) | fs.isEnum, top <- [maybe mempty (.top) tk], not (HM.null top)]
    <> maybe [] (\e -> ["example" .= e]) (exs >>= (\(Catalog.Examples vs) -> viaNonEmpty head (toList vs)))
  where
    typeKV = case L.nub $ sort $ mapMaybe jsonType $ HS.toList fs.types of
      [] -> []
      [t] -> ["type" .= t]
      ts -> ["type" .= ts]


-- | OTel/catalog value kind → JSON Schema type. 'Catalog.FTUnknown' has no
-- honest mapping, so it contributes nothing rather than lying with @string@.
jsonType :: Catalog.FieldTypes -> Maybe Text
jsonType = \case
  Catalog.FTString -> Just "string"
  Catalog.FTNumber -> Just "number"
  Catalog.FTBool -> Just "boolean"
  Catalog.FTObject -> Just "object"
  Catalog.FTList -> Just "array"
  Catalog.FTNull -> Just "null"
  Catalog.FTUnknown -> Nothing


-- | Catalog format hints are brace-wrapped pattern names (@{uuid}@). Translate
-- the ones OpenAPI standardises, pass the rest through unbraced (the spec
-- allows arbitrary @format@ strings), and drop the three that are really type
-- restatements.
--
-- >>> map openApiFormat ["{uuid}", "{YYYY-MM-DD}", "{md5}", "text"]
-- [Just "uuid",Just "date",Just "md5",Nothing]
openApiFormat :: Text -> Maybe Text
openApiFormat f = case T.dropAround (`elem` ("{}" :: String)) f of
  "text" -> Nothing
  "bool" -> Nothing
  "null" -> Nothing
  "integer" -> Just "int64"
  "float" -> Just "double"
  "url" -> Just "uri"
  "base64" -> Just "byte"
  "YYYY-MM-DD" -> Just "date"
  "YYYYMMDD" -> Just "date"
  "HH:MM:SS" -> Just "time"
  "YYYY-MM-DDThh:mm:ss.sTZD" -> Just "date-time"
  "YYYY-MM-DD HH:MM:SS" -> Just "date-time"
  other -> Just other


-- ---------------------------------------------------------------------------
-- URL paths

-- | Normalise a learned URL path to an OpenAPI path template and return the
-- template variables in order. Learned paths use two spellings —
-- @:name@ (framework routes) and the anonymous @{param}@ the path classifier
-- emits — and OpenAPI requires every variable to be named and unique.
-- Anonymous slots borrow the catalog's path-param names when the counts line
-- up, and fall back to positional @param1@, @param2@ … otherwise.
--
-- >>> pathTemplate [] "/v1/queues/:queueName/:jobId/clean"
-- ("/v1/queues/{queueName}/{jobId}/clean",["queueName","jobId"])
--
-- >>> pathTemplate ["platform"] "/v1/stores/integrations/{param}"
-- ("/v1/stores/integrations/{platform}",["platform"])
--
-- >>> pathTemplate [] "/a/{param}/b/{param}"
-- ("/a/{param1}/b/{param2}",["param1","param2"])
--
-- >>> pathTemplate [] "/health"
-- ("/health",[])
pathTemplate :: [Text] -> Text -> (Text, [Text])
pathTemplate known raw = (T.intercalate "/" named, catMaybes vars)
  where
    segs = T.splitOn "/" raw
    anonCount = length [() | s <- segs, isSlot s, not (":" `T.isPrefixOf` s)]
    -- Borrow catalog names for the anonymous slots only when there is exactly
    -- one plausible assignment; anything else would be a guess at ordering.
    anonNames = if length known == anonCount then known else ["param" <> show i | i <- [1 .. anonCount] :: [Int]]
    (vars, named) = unzip $ evalState (traverse step segs) anonNames
    step s
      | Just n <- T.stripPrefix ":" s, not (T.null n) = pure (Just n, "{" <> n <> "}")
      | isSlot s = state \case
          (n : rest) -> ((Just n, "{" <> n <> "}"), rest)
          [] -> ((Just s, s), [])
      | otherwise = pure (Nothing, s)
    isSlot s = (":" `T.isPrefixOf` s && T.length s > 1) || ("{" `T.isPrefixOf` s && "}" `T.isSuffixOf` s)


-- ---------------------------------------------------------------------------
-- Document assembly

-- | Fold catalog entries into one OpenAPI 3.1 document. @entries@ are
-- @(endpointHash, entry)@ pairs; the hash becomes the @operationId@ so links
-- from the endpoints table can deep-link an operation.
buildSpec :: Text -> Text -> [(Text, Catalog.CatalogEntry)] -> AE.Value
buildSpec title server entries =
  AE.object
    [ "openapi" .= ("3.1.0" :: Text)
    , "info"
        .= AE.object
          [ "title" .= title
          , "version" .= ("learned" :: Text)
          , "description" .= ("Generated by Monoscope from observed traffic. Schemas reflect what was actually sent and received, not a hand-written contract." :: Text)
          ]
    , "servers" .= ([AE.object ["url" .= server]] :: [AE.Value])
    , "paths" .= AE.object [AEK.fromText p .= AE.object o | (p, o) <- M.toList paths]
    , "components" .= AE.object ["schemas" .= AE.object schemas]
    ]
  where
    built = mapMaybe operation entries
    paths = M.fromListWith (<>) [(p, [m .= o]) | (p, m, o, _) <- built]
    schemas = concat [ss | (_, _, _, ss) <- built]


-- | One catalog entry → @(path, method, operation, component schemas)@.
-- Non-HTTP entries (span identities) carry no method/path and are skipped.
operation :: (Text, Catalog.CatalogEntry) -> Maybe (Text, AEK.Key, AE.Value, [(AEK.Key, AE.Value)])
operation (hash, e) = do
  method <- T.toLower <$> e.scope.method
  rawPath <- e.scope.urlPath
  let byCat c = [(k, (v, HM.lookup k e.valuesDelta, HM.lookup k e.counts)) | (k, v) <- HM.toList e.template.fields, v.category == c]
      -- Learned paths are occasionally relative or empty (a span with no route
      -- attribute); OpenAPI requires every path to start with "/".
      (path, pathVars) = first (("/" <>) . T.dropWhile (== '/')) $ pathTemplate (map fst (byCat Catalog.FCPathParam)) rawPath
      reqFields = byCat Catalog.FCRequestBody
      respFields = byCat Catalog.FCResponseBody
      respHeaderFields = byCat Catalog.FCResponseHeader
      respKey = AEK.fromText (hash <> "Response")
      statuses = if null e.scope.statusCodes then ["default"] else map show (sort $ toList e.scope.statusCodes)
      jsonContent schema = AE.object ["application/json" .= AE.object ["schema" .= schema]]
      body = AE.object ["required" .= False, "content" .= jsonContent (jsonSchema reqFields)]
      -- No learned body means we never saw one, which is not the same as "the
      -- body is an untyped JSON blob" — so the response declares no content at
      -- all rather than an empty schema that reads as "anything goes".
      response =
        AE.object
          $ ["description" .= ("Observed response" :: Text)]
          <> [("content" :: AEK.Key) .= jsonContent (AE.object ["$ref" .= ("#/components/schemas/" <> AEK.toText respKey)]) | not (null respFields)]
          <> [ ("headers" :: AEK.Key) .= AE.object [AEK.fromText (paramName k) .= AE.object ["schema" .= leafSchema ev] | (k, ev) <- respHeaderFields]
             | not (null respHeaderFields)
             ]
      op =
        AE.object
          $ [ "operationId" .= hash
            , -- Swagger UI prints the summary beside the path, so restating the
              -- path there is wasted space. The evidence behind the operation is
              -- what a reader can't get anywhere else.
              "summary" .= ("Observed " <> show e.sampleCount <> " times" :: Text)
            , "tags" .= maybeToList e.scope.service
            , "responses" .= AE.object [AEK.fromText s .= response | s <- statuses]
            , "x-monoscope"
                .= AE.object
                  [ "endpointHash" .= hash
                  , "host" .= e.scope.host
                  , "sampleCount" .= e.sampleCount
                  , "firstSeen" .= e.firstSeen
                  , "lastSeen" .= e.lastSeen
                  ]
            ]
          <> [("parameters" :: AEK.Key) .= ps | ps <- [pathParams pathVars (byCat Catalog.FCPathParam) <> params "query" (byCat Catalog.FCQueryParam) <> params "header" (byCat Catalog.FCRequestHeader)], not (null ps)]
          <> [("requestBody" :: AEK.Key) .= body | not (null reqFields)]
  pure (path, AEK.fromText method, op, [(respKey, jsonSchema respFields) | not (null respFields)])


-- | Query/header names arrive with the array marker the value walk added
-- (@page[*]@) — OpenAPI wants the bare name.
--
-- >>> map paramName ["page[*]", "filter.status[*]", "x-api-key"]
-- ["page","filter.status","x-api-key"]
paramName :: Text -> Text
paramName = T.replace "[*]" ""


-- | Parameters for one @in@ location, one per distinct name. Learned traffic
-- can't prove a parameter is required, so everything is optional.
params :: Text -> [(Text, Evidence)] -> [AE.Value]
params loc fields =
  [ AE.object ["in" .= loc, "name" .= n, "required" .= False, "schema" .= leafSchema ev]
  | (n, ev) <- M.toList $ M.fromList [(paramName k, v) | (k, v) <- fields]
  ]


-- | Path parameters follow the template's variable order, borrowing the
-- catalog's evidence when a name matches. A template variable with no learned
-- field still has to be declared — OpenAPI rejects a path template whose
-- variables aren't all present.
pathParams :: [Text] -> [(Text, Evidence)] -> [AE.Value]
pathParams vars learned =
  [ AE.object ["in" .= ("path" :: Text), "name" .= v, "required" .= True, "schema" .= maybe (AE.object ["type" .= ("string" :: Text)]) leafSchema (L.lookup v learned)]
  | v <- L.nub vars
  ]


-- | YAML rendering for the @openapi.yaml@ route. Plain 'Yaml.encode' sorts keys
-- alphabetically, which opens every spec on @components: {}@; the reading order
-- of an OpenAPI document is conventional, so pin the top-level keys to it.
specYaml :: AE.Value -> Text
specYaml = decodeUtf8 . YamlP.encodePretty (YamlP.setConfCompare (comparing rank <> compare) YamlP.defConfig)
  where
    rank k = fromMaybe (maxBound :: Int) $ L.elemIndex k ["openapi", "info", "servers", "paths", "components"]
