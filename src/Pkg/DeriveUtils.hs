-- | Database-bound half of the shared deriving helpers. The DB-free wrappers
-- ('WrappedEnumSC', 'SnakeSchema', 'escapeRegex', …) live in 'Pkg.Deriving' and
-- are re-exported here, so importing this module still gets you everything.
module Pkg.DeriveUtils (
  module Pkg.Deriving,
  SnakeSchema (..),
  CamelSchema (..),
  JsonValueSchema (..),
  AesonText (..),
  BaselineState (..),
  DB,
  PGTextArray (..),
  UUIDId (..),
  WrappedEnum (..),
  WrappedEnumShow (..),
  addKeepaliveParams,
  appendConnParams,
  connectPostgreSQL,
  idToText,
  idFromText,
  unAesonText,
  unAesonTextMaybe,
  assetUrl,
  staticAssetHashes,
  hashedAssetPath,
  stripAssetHash,
  assetHash,
  viteAssetFile,
  showPGFloatArray,
  textArrayEnc,
  mkHasqlPool,
  rawSql,
  selectFrom,
  -- Text-backed enum helpers, for types whose stored spelling is not the snake-cased
  -- constructor (see 'Pkg.Git.GitHost') and so cannot use 'WrappedEnumSC'.
  enumFromField,
  refineText,
) where

import Control.Exception (throwIO)
import Control.Lens ((?~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI (mk, original)
import Data.Char (isHexDigit)
import Data.Default (Default (..))
import Data.Digest.XXHash (xxHash)
import Data.Effectful.Hasql (Hasql)
import Data.HashMap.Strict qualified as HM
import Data.IntMap qualified as IntMap
import Data.OpenApi (NamedSchema (..), ToParamSchema (..), ToSchema (..), enum_, genericDeclareNamedSchema, type_)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Internal.Schema (GToSchema)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_select)
import Database.PostgreSQL.Entity.Types (Entity)
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple (Connection, FromRow, ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (Conversion (..), Field, FromField (..), returnError)
import Database.PostgreSQL.Simple.Internal qualified as PGI
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful (IOE, type (:>))
import GHC.Generics (Rep)
import GHC.Records (HasField (getField))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Hasql.Connection.Settings qualified as HCS
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Interpolate qualified as HI
import Hasql.Pool.Config qualified as HPC
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Numeric (showHex)
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Pkg.AssetManifestFingerprint (assetManifestFingerprint)
import Pkg.Deriving
import Relude
import Servant (FromHttpApiData (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.IO.Unsafe (unsafePerformIO)
import Text.Casing (quietSnake)


type DB es = (Hasql :> es, IOE :> es)


-- | Newtype wrapper for JSON fields that can handle JSONB, ByteString, and varchar/text columns
-- This is useful when a database column might be any of: JSONB, ByteString containing JSON, or TEXT containing JSON
-- It works with any type that has FromJSON/ToJSON instances, including Map Text Value
newtype AesonText a = AesonText a
  deriving (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, NFData)


instance (AE.FromJSON a, Typeable a) => FromField (AesonText a) where
  -- JSONB (via the Aeson newtype) first, then a raw ByteString column, then text/varchar.
  fromField f mdata = tryJsonb <|> tryDecode (fromField f mdata) "ByteString" <|> tryDecode (encodeUtf8 <$> (fromField f mdata :: Conversion Text)) "text"
    where
      tryJsonb = (\(Aeson v) -> AesonText v) <$> fromField f mdata
      tryDecode bs src =
        bs >>= either (\err -> returnError ConversionFailed f ("Failed to parse JSON from " <> src <> ": " <> err)) (pure . AesonText) . AE.eitherDecodeStrict


instance AE.ToJSON a => ToField (AesonText a) where
  toField (AesonText v) = toField (Aeson v)


instance AE.FromJSON a => HI.DecodeValue (AesonText a) where
  decodeValue = coerce (HI.decodeValue @(HI.AsJsonb a))


instance AE.ToJSON a => HI.EncodeValue (AesonText a) where
  encodeValue = coerce (HI.encodeValue @(HI.AsJsonb a))


-- | Unwrap an AesonText value
unAesonText :: AesonText a -> a
unAesonText (AesonText a) = a


-- | Unwrap a Maybe AesonText value
unAesonTextMaybe :: Maybe (AesonText a) -> Maybe a
unAesonTextMaybe = fmap unAesonText


-- | Newtype wrapper for PostgreSQL text arrays (TEXT[])
newtype PGTextArray = PGTextArray (V.Vector Text)
  deriving (Eq, Generic, Show)
  deriving newtype (FromField, NFData, ToField)


-- | Generic UUID-based ID type with phantom type parameter for type safety
-- Usage: type ProjectId = UUIDId "project"
newtype UUIDId (name :: Symbol) = UUIDId {unUUIDId :: UUID.UUID}
  deriving stock (Generic, Read, Show, TH.Lift)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, Hashable, NFData, Ord, ToField)
  deriving anyclass (FromRow, HI.DecodeRow, ToRow)


instance KnownSymbol name => ToSchema (UUIDId name) where declareNamedSchema _ = declareNamedSchema (Proxy @UUID.UUID)
instance ToParamSchema (UUIDId name) where toParamSchema _ = toParamSchema (Proxy @UUID.UUID)


-- Case-insensitive text is documented as plain text; lives here so every model
-- carrying a 'CI Text' field gets the schema without redeclaring the orphan.
instance ToSchema (CI Text) where declareNamedSchema _ = declareNamedSchema (Proxy @Text)


instance HasField "toText" (UUIDId name) Text where
  getField = UUID.toText . unUUIDId


instance HasField "unwrap" (UUIDId name) UUID.UUID where
  getField = coerce


-- | Convert any UUID-based ID to Text
idToText :: UUIDId name -> Text
idToText uid = uid.toText


-- | Parse Text to a UUID-based ID
idFromText :: Text -> Maybe (UUIDId name)
idFromText = fmap UUIDId . UUID.fromText


-- | DerivingVia wrapper for enum-like sum types stored in TEXT columns.
-- Decoders use @D.text@; for a real PG ENUM column use 'WrappedEnumSC' with @'Just "schema.type"@ instead.
newtype WrappedEnum (prefix :: Symbol) a = WrappedEnum a
  deriving (Generic)


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnum prefix a) where
  toField (WrappedEnum a) = toField . T.toUpper . fromString . drop (length $ symbolVal (Proxy @prefix)) . show $ a


-- | Shared postgresql-simple 'fromField' for text-backed enum wrappers: NULL fails,
-- otherwise the decoded bytes are transformed then parsed; a parse failure quotes the
-- transformed string.
enumFromField :: Typeable c => (b -> c) -> (Text -> String) -> (String -> Maybe b) -> Field -> Maybe ByteString -> Conversion c
enumFromField ctor xform parse f = \case
  Nothing -> returnError UnexpectedNull f ""
  Just bss -> let str = xform (decodeUtf8 bss) in maybe (returnError ConversionFailed f $ "Cannot parse: " <> str) (pure . ctor) (parse str)


instance (KnownSymbol prefix, Read a, Typeable a) => FromField (WrappedEnum prefix a) where
  fromField = enumFromField WrappedEnum (\t -> symbolVal (Proxy @prefix) <> toString (T.toTitle t)) readMaybe


instance (KnownSymbol prefix, Show a) => HI.EncodeValue (WrappedEnum prefix a) where
  encodeValue = contramap (\(WrappedEnum a) -> T.toUpper $ toText $ drop (length $ symbolVal (Proxy @prefix)) $ show a) E.text


-- | Shared helper for hasql 'D.DecodeValue' instances that parse text and refine into a Haskell value.
refineText :: Text -> (Text -> Maybe a) -> D.Value a
refineText ctx f = D.refine (\t -> maybeToRight (ctx <> ": cannot parse " <> t) (f t)) D.text


instance (KnownSymbol prefix, Read a) => HI.DecodeValue (WrappedEnum prefix a) where
  decodeValue = refineText "WrappedEnum" \t ->
    WrappedEnum <$> readMaybe (symbolVal (Proxy @prefix) <> toString (T.toTitle t))


instance (KnownSymbol prefix, Read a) => HI.DecodeRow (WrappedEnum prefix a) where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnumSC qualType prefix a) where
  toField (WrappedEnumSC a) = toField $ encodeEnumSC @prefix a


instance (KnownSymbol prefix, Read a, Typeable a, Typeable qualType) => FromField (WrappedEnumSC qualType prefix a) where
  fromField = enumFromField WrappedEnumSC toString (decodeEnumSC @prefix)


instance (KnownMaybeSymbol qualType, KnownSymbol prefix, Show a) => HI.EncodeValue (WrappedEnumSC qualType prefix a) where
  encodeValue = contramap (\(WrappedEnumSC a) -> toText (encodeEnumSC @prefix a)) case maybeSymbolVal (Proxy @qualType) of
    Nothing -> E.text
    Just qn -> uncurry E.enum (splitQualType qn) id


instance (KnownMaybeSymbol qualType, KnownSymbol prefix, Read a) => HI.DecodeValue (WrappedEnumSC qualType prefix a) where
  decodeValue = case maybeSymbolVal (Proxy @qualType) of
    Nothing -> refineText "WrappedEnumSC" (fmap WrappedEnumSC . decodeEnumSC @prefix . toString)
    Just qn -> WrappedEnumSC <$> uncurry D.enum (splitQualType qn) (decodeEnumSC @prefix . toString)


instance (KnownMaybeSymbol qualType, KnownSymbol prefix, Read a) => HI.DecodeRow (WrappedEnumSC qualType prefix a) where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeValue ZonedTime where
  decodeValue = utcToZonedTime utc <$> D.timestamptz


instance HI.EncodeValue ZonedTime where
  encodeValue = contramap zonedTimeToUTC E.timestamptz


-- | Like 'WrappedEnum' but uses the unmodified @Show@/@Read@ representation. TEXT columns only.
newtype WrappedEnumShow a = WrappedEnumShow a
  deriving (Generic)


instance Show a => ToField (WrappedEnumShow a) where
  toField (WrappedEnumShow a) = toField (show a)


instance (Read a, Typeable a) => FromField (WrappedEnumShow a) where
  fromField = enumFromField WrappedEnumShow toString readMaybe


instance Show a => HI.EncodeValue (WrappedEnumShow a) where
  encodeValue = contramap (\(WrappedEnumShow a) -> toText $ show a) E.text


instance Read a => HI.DecodeValue (WrappedEnumShow a) where
  decodeValue = refineText "WrappedEnumShow" (fmap WrappedEnumShow . readMaybe . toString)


-- | OpenApi half of the shared deriving wrappers. Kept out of 'Pkg.Deriving' so
-- the CLI does not link openapi3; everything here is server-side only.
--
-- The instances for 'WrappedEnumSC' are orphans for the same reason.
instance {-# OVERLAPPABLE #-} (Bounded a, Enum a, KnownSymbol prefix, Show a, Typeable a, Typeable qualType) => ToSchema (WrappedEnumSC qualType prefix a) where
  declareNamedSchema (_ :: proxy (WrappedEnumSC qualType prefix a)) = pure $ NamedSchema Nothing $ enumSCSchema @prefix @a


instance (Bounded a, Enum a, KnownSymbol prefix, Show a) => ToParamSchema (WrappedEnumSC qualType prefix a) where
  toParamSchema (_ :: proxy (WrappedEnumSC qualType prefix a)) = enumSCSchema @prefix @a


-- | Shared string-enum OpenApi schema for a 'WrappedEnumSC'.
enumSCSchema :: forall prefix a. (Bounded a, Enum a, KnownSymbol prefix, Show a) => OpenApi.Schema
enumSCSchema =
  mempty
    & type_
    ?~ OpenApi.OpenApiString
      & enum_
    ?~ [AE.String (toText $ encodeEnumSC @prefix v) | v <- [minBound @a .. maxBound @a]]


-- | DerivingVia wrapper: produces ToSchema with snake_case field names matching DAE.Snake's ToJSON output.
newtype SnakeSchema a = SnakeSchema a


instance (GToSchema (Rep a), Generic a, Typeable a) => ToSchema (SnakeSchema a) where
  declareNamedSchema _ =
    genericDeclareNamedSchema
      OpenApi.defaultSchemaOptions{OpenApi.fieldLabelModifier = quietSnake . fromString}
      (Proxy @a)


-- | DerivingVia wrapper: produces ToSchema with unmodified (camelCase) field names.
newtype CamelSchema a = CamelSchema a


instance (GToSchema (Rep a), Generic a, Typeable a) => ToSchema (CamelSchema a) where
  declareNamedSchema _ = genericDeclareNamedSchema OpenApi.defaultSchemaOptions (Proxy @a)


-- | DerivingVia wrapper: emit an unconstrained JSON value schema for types whose
-- subtrees don't have ToSchema (escape hatch for deeply nested domain types).
newtype JsonValueSchema a = JsonValueSchema a


instance Typeable a => ToSchema (JsonValueSchema a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @AET.Value)


instance ToSchema AET.Value where
  declareNamedSchema _ = pure $ NamedSchema (Just "JSONValue") mempty


data BaselineState = BSLearning | BSEstablished
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "BS" BaselineState


-- | Append libpq connection parameters, handling both URI
-- (@postgres://…?k=v&…@) and keyword/value (@host=… sslmode=…@) forms.
appendConnParams :: [(ByteString, ByteString)] -> ByteString -> ByteString
appendConnParams kvs connStr
  | null kvs = connStr
  | "postgres://" `BS.isPrefixOf` connStr || "postgresql://" `BS.isPrefixOf` connStr =
      connStr <> (if "?" `BS.isInfixOf` connStr then "&" else "?") <> BS.intercalate "&" params
  | otherwise = connStr <> " " <> BS.intercalate " " params
  where
    params = [k <> "=" <> v | (k, v) <- kvs]


-- | Socket-liveness params for every PG/TF pool. Keepalives detect a dead
-- idle peer in ~45s, but two hangs escape them (2026-07-04 wedged-consumer
-- incident: a worker sat in one uninterruptible libpq call for 40+ min):
-- @connect_timeout@ bounds connection establishment (otherwise unbounded —
-- the pool's acquisitionTimeout only bounds slot-wait), and
-- @tcp_user_timeout@ bounds writes with unacked data in the send queue,
-- where keepalive probes don't run (otherwise the ~15–25min kernel
-- retransmit ceiling; ignored on non-Linux). The kernel then surfaces the
-- dead socket as a connection error, which the transient-retry/DLQ path
-- already handles.
--
-- >>> addKeepaliveParams "postgres://u:p@host:5432/db"
-- "postgres://u:p@host:5432/db?keepalives=1&keepalives_idle=15&keepalives_interval=10&keepalives_count=3&connect_timeout=5&tcp_user_timeout=30000"
--
-- >>> addKeepaliveParams "host=localhost dbname=db"
-- "host=localhost dbname=db keepalives=1 keepalives_idle=15 keepalives_interval=10 keepalives_count=3 connect_timeout=5 tcp_user_timeout=30000"
addKeepaliveParams :: ByteString -> ByteString
addKeepaliveParams =
  appendConnParams
    [ ("keepalives", "1")
    , ("keepalives_idle", "15")
    , ("keepalives_interval", "10")
    , ("keepalives_count", "3")
    , ("connect_timeout", "5")
    , ("tcp_user_timeout", "30000")
    ]


connectPostgreSQL :: ByteString -> IO Connection
connectPostgreSQL connstr = do
  conn <- PGI.connectdb connstr
  stat <- PQ.status conn
  case stat of
    PQ.ConnectionOk -> do
      connectionHandle <- newMVar conn
      connectionObjects <- newMVar IntMap.empty
      connectionTempNameCounter <- newIORef 0
      pure PGI.Connection{..}
    _ -> do
      msg <- fromMaybe "connectPostgreSQL error" <$> PQ.errorMessage conn
      throwIO $ PGI.fatalError msg


-- | The 8-digit content hash an asset's URL embeds. Zero-padded so 'stripAssetHash'
-- can recognise it unambiguously.
--
-- >>> assetHash "hello"
-- "fb0077f9"
-- >>> assetHash "2"  -- padded, so the segment is always 8 wide
-- "0c436334"
assetHash :: LByteString -> Text
assetHash = T.justifyRight 8 '0' . toText . flip showHex "" . xxHash


-- | Content hash of every file under @static@, keyed by URL path
-- (@"public/assets/js/main.js" -> "2c58501e"@). Read once, on first use: the image is
-- immutable in prod, and in dev ghcid restarts the server when an asset changes.
--
-- This is deliberately the /only/ source of asset hashes. 'assetUrl' renders from it and
-- 'System.Server.hashedAssetMiddleware' verifies against it, so the URL a page emits and
-- the URL the server accepts cannot disagree. They used to: the URL carried a hash baked
-- in by Template Haskell at compile time while the check read disk at boot, so any asset
-- rebuilt after the last compile — a CSS rebuild, @make fa-add@, a Vite build — failed
-- the check, and the check fails closed. Every page loaded without its stylesheet.
--
-- A CAF rather than an effect because the readers are pure Lucid renderers ('faSprite_'
-- is @Monad m => HtmlT m ()@); this is the same process-lifetime constant TH used to
-- inline, read at boot instead of at compile time.
staticAssetHashes :: HashMap Text Text
staticAssetHashes = unsafePerformIO $ HM.fromList <$> walk ""
  where
    onDisk rel = "static/" <> toString rel
    walk rel =
      listDirectory (onDisk rel) >>= foldMapM \name -> do
        let child = rel <> toText name
        doesDirectoryExist (onDisk child) >>= \case
          True -> walk $ child <> "/"
          False -> one . (child,) . assetHash <$> readFileLBS (onDisk child)
{-# NOINLINE staticAssetHashes #-}


-- | URL of a static asset with its content hash embedded in the __path__.
--
-- The hash must live in the path, not in a @?v=@ query: CDNs key on the path, so
-- during a rolling deploy a request for the new URL can be answered by an old
-- replica — which ignores the query and streams the stale file off disk — poisoning
-- the cache for the whole (year-long) max-age. Distinct paths can't collide that way.
-- 'System.Server.hashedAssetMiddleware' maps the name back to the file on disk.
--
-- An asset that isn't under @static@ keeps its plain path: the middleware passes unhashed
-- URLs through, so a missing file costs cache-busting rather than the whole response.
assetUrl :: Text -> Text
assetUrl path = maybe path (hashedAssetPath path) $ HM.lookup (T.dropWhile (== '/') path) staticAssetHashes


-- | Insert a content hash before the final extension.
--
-- >>> hashedAssetPath "/public/assets/js/main.js" "2c58501e"
-- "/public/assets/js/main.2c58501e.js"
-- >>> hashedAssetPath "/public/assets/no-extension" "2c58501e"
-- "/public/assets/no-extension"
--
-- >>> stripAssetHash (hashedAssetPath "public/assets/js/main.js" "2c58501e")
-- Just ("public/assets/js/main.js","2c58501e")
hashedAssetPath :: Text -> Text -> Text
hashedAssetPath path h = case T.breakOnEnd "." path of
  (stem, ext) | not (T.null stem), not (T.any (== '/') ext) -> stem <> h <> "." <> ext
  _ -> path


-- | Inverse of 'hashedAssetPath': split a hashed URL back into the path on disk and
-- the hash it claims that file has.
--
-- >>> stripAssetHash "public/assets/js/main.2c58501e.js"
-- Just ("public/assets/js/main.js","2c58501e")
-- >>> stripAssetHash "public/assets/js/main.js"
-- Nothing
-- >>> stripAssetHash "public/assets/deps/htmx/htmx-4.0.0-beta6.min.js"
-- Nothing
stripAssetHash :: Text -> Maybe (Text, Text)
stripAssetHash p = do
  let (stemDot, ext) = T.breakOnEnd "." p
  (prefix, h) <- T.breakOnEnd "." <$> T.stripSuffix "." stemDot
  guard $ T.length h == 8 && T.all isHexDigit h && not (T.null prefix)
  pure (prefix <> ext, h)


-- | URL of a Vite build output, looked up by manifest key (e.g. @"index.html"@ for the
-- entry). The emitted filename already carries a content hash, so this must NOT get a
-- @?v=@ query on top: chunks import the entry back as a bare @./index.<hash>.js@, and a
-- queried URL is a second module identity — the browser would evaluate the whole graph
-- twice and every @customElements.define@ in it would throw on the second pass.
viteAssetFile :: FilePath -> TH.Q TH.Exp
viteAssetFile key =
  assetManifestFingerprint `seq` do
    let dir = "/public/assets/web-components/dist/"
        manifest = "static" <> dir <> "manifest.json"
    TH.qAddDependentFile manifest
    chunks <- TH.runIO $ AE.eitherDecodeFileStrict' @AE.Object manifest
    either (fail . (("viteAssetFile " <> manifest <> ": ") <>)) (TH.lift . (dir <>) . toString @Text)
      $ chunks
      >>= AET.parseEither (\o -> o AE..: fromString key >>= (AE..: "file"))


-- | Format a list of Floats as a PostgreSQL array literal, e.g. "{1.0,2.0,3.0}"
--
-- >>> showPGFloatArray [1.0, 2.5, 3.0]
-- "{1.0,2.5,3.0}"
--
-- >>> showPGFloatArray []
-- "{}"
showPGFloatArray :: [Float] -> Text
showPGFloatArray xs = "{" <> T.intercalate "," (map show xs) <> "}"


-- Types without Generic need explicit DecodeRow via OneColumn.
instance HI.DecodeRow UUID.UUID where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeValue a => HI.DecodeRow (V.Vector a) where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeRow Text where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeRow Int64 where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeRow Int where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeRow Bool where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeRow UTCTime where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeRow Double where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeValue a => HI.DecodeRow (Maybe a) where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


-- Int has no hasql instances (uses Int32/Int64). Map to int8 to match postgresql-simple.
instance HI.DecodeValue Int where
  decodeValue = fromIntegral <$> D.int8


instance HI.EncodeValue Int where
  encodeValue = contramap (fromIntegral @Int @Int64) E.int8


instance HI.DecodeValue Integer where
  decodeValue = fromIntegral <$> D.int8


instance HI.EncodeValue Integer where
  encodeValue = contramap (fromIntegral @Integer @Int64) E.int8


instance HI.DecodeValue AET.Value where
  decodeValue = D.jsonb


instance HI.DecodeRow AET.Value where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.EncodeValue AET.Value where
  encodeValue = E.jsonb


instance HI.DecodeValue (Map Text AET.Value) where
  decodeValue = D.refine toMap D.jsonb
    where
      toMap (AET.Object o) = Right $ KEM.toMapText o
      toMap other = Left $ "Expected JSON object, got: " <> show other


instance HI.EncodeValue (Map Text AET.Value) where
  encodeValue = contramap (AET.Object . KEM.fromMapText) E.jsonb


instance AET.FromJSON a => HI.DecodeValue (Aeson a) where
  decodeValue = D.refine (bimap toText Aeson . AET.parseEither AET.parseJSON) D.jsonb


instance AET.ToJSON a => HI.EncodeValue (Aeson a) where
  encodeValue = contramap (\(Aeson a) -> AET.toJSON a) E.jsonb


-- | Decode @CITEXT@ (and the @email@ domain over it, which Postgres reports as
-- citext to libpq). DB columns are routinely @citext@/@email@ rather than plain
-- @text@; using 'D.text' here fails with @UnexpectedColumnTypeStatementError@.
instance HI.DecodeValue (CI Text) where
  decodeValue = CI.mk <$> D.citext


instance HI.EncodeValue (CI Text) where
  encodeValue = contramap CI.original E.text


textArrayEnc :: E.Value (V.Vector Text)
textArrayEnc = E.array (E.dimension foldl' (E.element (E.nonNullable E.text)))
{-# INLINE textArrayEnc #-}


-- | Build a hasql pool. Timeouts are `DiffTime` (seconds): 30s acquisition,
-- 30min aging/idleness — matches the postgresql-simple pool's lifetime envelope.
-- Returns an OTel-instrumented pool: every session opens a Client span with
-- the OTel db semantic-convention attributes (db.system, db.namespace, server.address,
-- server.port, db.user) parsed from the connection string.
mkHasqlPool :: Int -> ByteString -> IO OHasql.TracedPool
mkHasqlPool sz cstr =
  OHasql.acquireFromConnString
    ( HPC.settings
        [ HPC.size sz
        , HPC.acquisitionTimeout 30
        , HPC.agingTimeout 1800
        , HPC.idlenessTimeout 1800
        , HPC.staticConnectionSettings (HCS.connectionString (decodeUtf8 cstr))
        ]
    )
    cstr


-- | Embed a raw @Text@ value as a literal SQL fragment (no escaping/parameterization).
-- Use only for column/table names and other trusted fragments, never user input.
rawSql :: Text -> HI.Sql
rawSql = fromString . toString


-- | Convert pg-entity's @_select \@Entity@ to an @HI.Sql@ fragment.
-- Produces @SELECT col1, col2, ... FROM schema.table@, ready to append
-- a WHERE clause via @<> [HI.sql| WHERE ... |]@.
selectFrom :: forall e. Entity e => HI.Sql
selectFrom = rawSql $ decodeUtf8 $ fromQuery (_select @e)
