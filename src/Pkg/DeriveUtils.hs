-- | Database-bound half of the shared deriving helpers. The DB-free wrappers
-- ('WrappedEnumSC', 'SnakeSchema', 'escapeRegex', …) live in 'Pkg.Deriving' and
-- are re-exported here, so importing this module still gets you everything.
module Pkg.DeriveUtils (
  module Pkg.Deriving,
  AesonText (..),
  SnakeJSON (..),
  BaselineState (..),
  DB,
  WrappedEnum (..),
  WrappedEnumShow (..),
  addKeepaliveParams,
  appendConnParams,
  connectPostgreSQL,
  idFromText,
  unAesonText,
  unAesonTextMaybe,
  assetUrl,
  staticAssetHashes,
  hashedAssetPath,
  stripAssetHash,
  assetHash,
  viteAssetFile,
  embedIconEntries,
  bulkActionSlug,
  showPGFloatArray,
  mkHasqlPool,
  mkHasqlPoolWithIdleTimeout,
  rawSql,
  SqlEnum (..),
  selectFrom,
  -- Text-backed enum helpers, for types whose stored spelling is not the snake-cased
  -- constructor (see 'Pkg.Git.GitHost') and so cannot use 'WrappedEnumSC'.
  enumFromField,
  refineText,
) where

import Control.Exception (throwIO)
import Data.Aeson qualified as AE
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
import Data.List (lookup)
import Data.OpenApi (ToSchema (..))
import Data.Text qualified as T
import Data.Text.Display (Display)
import Data.Time (DiffTime, UTCTime, ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
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
import Deriving.Aeson.Stock qualified as DAE
import Effectful (type (:>))
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


type DB es = Hasql :> es


-- | Newtype wrapper for JSON fields that can handle JSONB, ByteString, and varchar/text columns
-- This is useful when a database column might be any of: JSONB, ByteString containing JSON, or TEXT containing JSON
-- It works with any type that has FromJSON/ToJSON instances, including Map Text Value
newtype AesonText a = AesonText a
  deriving (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, NFData)
  deriving (ToField) via Aeson a
  deriving (HI.DecodeValue, HI.EncodeValue) via HI.AsJsonb a


instance (AE.FromJSON a, Typeable a) => FromField (AesonText a) where
  -- JSONB (via the Aeson newtype) first, then a raw ByteString column, then text/varchar.
  fromField f mdata = tryJsonb <|> tryDecode (fromField f mdata) "ByteString" <|> tryDecode (encodeUtf8 <$> (fromField f mdata :: Conversion Text)) "text"
    where
      tryJsonb = (\(Aeson v) -> AesonText v) <$> fromField f mdata
      tryDecode bs src =
        bs >>= either (\err -> returnError ConversionFailed f ("Failed to parse JSON from " <> src <> ": " <> err)) (pure . AesonText) . AE.eitherDecodeStrict


-- | Unwrap an AesonText value
unAesonText :: AesonText a -> a
unAesonText (AesonText a) = a


-- | Unwrap a Maybe AesonText value
unAesonTextMaybe :: Maybe (AesonText a) -> Maybe a
unAesonTextMaybe = fmap unAesonText


deriving newtype instance HI.DecodeValue a => HI.DecodeValue (Sum a)


deriving newtype instance FromField (UUIDId name)
deriving newtype instance ToField (UUIDId name)
deriving newtype instance HI.DecodeValue (UUIDId name)
deriving newtype instance HI.EncodeValue (UUIDId name)
deriving anyclass instance FromRow (UUIDId name)
deriving anyclass instance HI.DecodeRow (UUIDId name)
deriving anyclass instance ToRow (UUIDId name)


-- | DerivingVia wrapper: snake_case JSON ('DAE.Snake') plus the OpenAPI schema of
-- @schema a@ — @via SnakeJSON SnakeSchema T@, or @via SnakeJSON JsonValueSchema T@
-- for types whose subtrees have no 'ToSchema'. DB-free: belongs in 'Pkg.Deriving' once the
-- watchers next rebuild the shared package, so shared types can use it too.
newtype SnakeJSON (schema :: Type -> Type) a = SnakeJSON a


deriving via DAE.Snake a instance AE.ToJSON (DAE.Snake a) => AE.ToJSON (SnakeJSON schema a)
deriving via DAE.Snake a instance AE.FromJSON (DAE.Snake a) => AE.FromJSON (SnakeJSON schema a)


instance (ToSchema (schema a), Typeable a, Typeable schema) => ToSchema (SnakeJSON schema a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @(schema a))


-- Case-insensitive text is documented as plain text; lives here so every model
-- carrying a 'CI Text' field gets the schema without redeclaring the orphan.
instance ToSchema (CI Text) where declareNamedSchema _ = declareNamedSchema (Proxy @Text)


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


-- | Wire slug for a bulk-action enum — the @BA@-prefixed sum types behind the
-- @bulk_action\/:action@ routes. Both sides of each route go through this: the URL is
-- built with it and the capture is parsed by the matching 'WrappedEnumSC' instance, so
-- a link and its handler cannot drift apart.
bulkActionSlug :: Show a => a -> Text
bulkActionSlug = toText . encodeEnumSC @"BA"


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


instance Read a => FromHttpApiData (WrappedEnumShow a) where
  parseUrlPiece t = maybeToRight ("Invalid value: " <> t) $ WrappedEnumShow <$> readMaybe (toString t)


data BaselineState = BSLearning | BSEstablished
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, SqlEnum, ToField) via WrappedEnumSC 'Nothing "BS" BaselineState


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
  PQ.status conn >>= \case
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
-- >>> stripAssetHash "public/assets/deps/htmx/htmx-4.0.0.min.js"
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


instance HI.DecodeValue a => HI.DecodeRow (V.Vector a) where
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


deriving via HI.Jsonb instance HI.DecodeValue AET.Value
deriving via HI.Jsonb instance HI.EncodeValue AET.Value
deriving via HI.AsJsonb (Map Text AET.Value) instance HI.DecodeValue (Map Text AET.Value)
deriving via HI.AsJsonb (Map Text AET.Value) instance HI.EncodeValue (Map Text AET.Value)
deriving via HI.AsJsonb a instance AET.FromJSON a => HI.DecodeValue (Aeson a)
deriving via HI.AsJsonb a instance AET.ToJSON a => HI.EncodeValue (Aeson a)


-- | Decode @CITEXT@ (and the @email@ domain over it, which Postgres reports as
-- citext to libpq). DB columns are routinely @citext@/@email@ rather than plain
-- @text@; using 'D.text' here fails with @UnexpectedColumnTypeStatementError@.
instance HI.DecodeValue (CI Text) where
  decodeValue = CI.mk <$> D.citext


instance HI.EncodeValue (CI Text) where
  encodeValue = contramap CI.original E.text


-- | Build a hasql pool. Timeouts are `DiffTime` (seconds): 30s acquisition,
-- 30min aging/idleness — matches the postgresql-simple pool's lifetime envelope.
-- Returns an OTel-instrumented pool: every session opens a Client span with
-- the OTel db semantic-convention attributes (db.system, db.namespace, server.address,
-- server.port, db.user) parsed from the connection string.
mkHasqlPool :: Int -> ByteString -> IO OHasql.TracedPool
mkHasqlPool = mkHasqlPoolWithIdleTimeout 1800


mkHasqlPoolWithIdleTimeout :: DiffTime -> Int -> ByteString -> IO OHasql.TracedPool
mkHasqlPoolWithIdleTimeout idleTimeout sz cstr =
  OHasql.acquireFromConnString
    ( HPC.settings
        [ HPC.size sz
        , HPC.acquisitionTimeout 30
        , HPC.agingTimeout 1800
        , HPC.idlenessTimeout idleTimeout
        , HPC.staticConnectionSettings (HCS.connectionString (decodeUtf8 cstr))
        ]
    )
    cstr


-- | Embed a raw @Text@ value as a literal SQL fragment (no escaping/parameterization).
-- Use only for column/table names and other trusted fragments, never user input.
rawSql :: Text -> HI.Sql
rawSql = fromString . toString


-- | An enum's stored spelling as an inline SQL string literal, so the query text keeps
-- the literal a partial index predicate matches (a bound parameter would not). Only
-- 'WrappedEnumSC' provides it, so free text can never be spliced unescaped.
class SqlEnum a where
  enumLit :: a -> HI.Sql


instance (KnownSymbol prefix, Show a) => SqlEnum (WrappedEnumSC qualType prefix a) where
  enumLit (WrappedEnumSC a) = rawSql $ "'" <> toText (encodeEnumSC @prefix a) <> "'"


-- | Convert pg-entity's @_select \@Entity@ to an @HI.Sql@ fragment.
-- Produces @SELECT col1, col2, ... FROM schema.table@, ready to append
-- a WHERE clause via @<> [HI.sql| WHERE ... |]@.
selectFrom :: forall e. Entity e => HI.Sql
selectFrom = rawSql $ decodeUtf8 $ fromQuery (_select @e)


-- | Read Font Awesome sprite sheets at compile time and embed their symbols as
-- @(kind, name, viewBox, presentation attributes, body)@ tuples.
embedIconEntries :: [(String, FilePath)] -> TH.Q TH.Exp
embedIconEntries sprites = do
  entries <-
    concat <$> forM sprites \(kind, path) -> do
      TH.qAddDependentFile path
      contents <- TH.runIO $ decodeUtf8 <$> readFileBS path
      either (fail . ((path <> ": ") <>)) pure $ parseSprite kind contents
  -- SVG fragment lookup resolves the first matching id. Preserve that behavior
  -- for the few legacy duplicate symbols already present in the sheets.
  TH.lift $ deduplicate entries


type EmbeddedIcon = (String, String, String, [(String, String)], String)


parseSprite :: String -> Text -> Either String [EmbeddedIcon]
parseSprite kind contents = stripComments contents >>= go []
  where
    go acc source = case T.breakOn "<symbol" source of
      (_, rest)
        | T.null rest ->
            if null acc
              then Left "sprite contains no <symbol> elements"
              else Right $ reverse acc
      (_, rest) -> do
        let (opening, afterOpening) = T.breakOn ">" rest
        when (T.null afterOpening) $ Left "unterminated <symbol> opening tag"
        attrs <- parseAttributes $ T.drop (T.length "<symbol") opening
        name <- requiredAttribute "id" attrs
        viewBox <- requiredAttribute "viewBox" attrs
        let (body, afterBody) = T.breakOn "</symbol>" $ T.drop 1 afterOpening
        when (T.null afterBody) $ Left $ "unterminated <symbol> for icon " <> toString name
        let presentationAttrs = filter (\(key, _) -> key /= "id" && key /= "viewBox" && key /= "width" && key /= "height") attrs
            entry =
              ( kind
              , toString name
              , toString viewBox
              , map (bimap toString toString) presentationAttrs
              , toString $ minifyMarkup $ prefixInternalIds (toText kind <> "-" <> name) body
              )
        go (entry : acc) $ T.drop (T.length "</symbol>") afterBody


deduplicate :: [EmbeddedIcon] -> [EmbeddedIcon]
deduplicate = go []
  where
    go _ [] = []
    go seen (entry@(kind, name, _, _, _) : rest)
      | (kind, name) `elem` seen = go seen rest
      | otherwise = entry : go ((kind, name) : seen) rest


stripComments :: Text -> Either String Text
stripComments source = case T.breakOn "<!--" source of
  (before, rest)
    | T.null rest -> Right before
    | otherwise -> do
        let (_, afterComment) = T.breakOn "-->" $ T.drop (T.length "<!--") rest
        when (T.null afterComment) $ Left "unterminated XML comment"
        (before <>) <$> stripComments (T.drop (T.length "-->") afterComment)


requiredAttribute :: Text -> [(Text, Text)] -> Either String Text
requiredAttribute key attrs =
  maybeToRight ("<symbol> is missing required " <> toString key <> " attribute") $ lookup key attrs


parseAttributes :: Text -> Either String [(Text, Text)]
parseAttributes = go [] . T.strip
  where
    go acc input
      | T.null input = Right $ reverse acc
      | otherwise = do
          let (key, afterKey) = T.breakOn "=" input
              trimmedKey = T.strip key
          when (T.null trimmedKey || T.null afterKey) $ Left $ "invalid <symbol> attributes: " <> toString input
          let valueStart = T.stripStart $ T.drop 1 afterKey
          unless (T.isPrefixOf "\"" valueStart) $ Left $ "attribute value must use double quotes: " <> toString input
          let (value, afterValue) = T.breakOn "\"" $ T.drop 1 valueStart
          when (T.null afterValue) $ Left $ "unterminated attribute value: " <> toString input
          go ((trimmedKey, value) : acc) $ T.stripStart $ T.drop 1 afterValue


-- The sprite formatting is useful to humans but wasteful when repeated in page
-- HTML. XML whitespace between/inside tags is insignificant for these paths.
minifyMarkup :: Text -> Text
minifyMarkup = unwords . words


-- Inline SVGs share the document id namespace. Prefix definitions and their
-- local URL references so gradients/clip paths from different icons cannot
-- accidentally resolve to one another.
prefixInternalIds :: Text -> Text -> Text
prefixInternalIds prefix markup = foldl' rewrite markup $ internalIds markup
  where
    rewrite body internalId =
      T.replace ("url(#" <> internalId <> ")") ("url(#" <> prefixed internalId <> ")")
        $ T.replace ("id=\"" <> internalId <> "\"") ("id=\"" <> prefixed internalId <> "\"") body
    prefixed internalId = prefix <> "-" <> internalId


internalIds :: Text -> [Text]
internalIds = go
  where
    go source = case T.breakOn "id=\"" source of
      (_, rest) | T.null rest -> []
      (_, rest) ->
        let valueStart = T.drop (T.length "id=\"") rest
            (value, afterValue) = T.breakOn "\"" valueStart
         in value : go (T.drop 1 afterValue)


-- Types without Generic decode a one-column row via OneColumn.
concat <$> forM [[t|UUID.UUID|], [t|Text|], [t|Int64|], [t|Int|], [t|Bool|], [t|UTCTime|], [t|Double|], [t|AET.Value|]] \t ->
  [d|instance HI.DecodeRow $t where decodeRow = HI.getOneColumn <$> HI.decodeRow|]
