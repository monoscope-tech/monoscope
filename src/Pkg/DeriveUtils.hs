{-# LANGUAGE CPP #-}

module Pkg.DeriveUtils (
  AesonText (..),
  BaselineState (..),
  DB,
  PGTextArray (..),
  SnakeSchema (..),
  UUIDId (..),
  WrappedEnum (..),
  WrappedEnumSC (..),
  encodeEnumSC,
  WrappedEnumShow (..),
  addKeepaliveParams,
  connectPostgreSQL,
  idToText,
  idFromText,
  unAesonText,
  unAesonTextMaybe,
  hashAssetFile,
  hashFile,
  showPGFloatArray,
  textArrayEnc,
  mkHasqlPool,
  rawSql,
  selectFrom,
) where

import Control.Exception (throwIO)
import Control.Lens ((?~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.Types qualified as AET
import Data.CaseInsensitive (CI, FoldCase)
import Data.CaseInsensitive qualified as CI (mk, original)
import Data.Default (Default (..))
import Data.Digest.XXHash (xxHash)
import Data.Effectful.Hasql (Hasql)
import Data.IntMap qualified as IntMap
import Data.OpenApi (NamedSchema (..), ToParamSchema (..), ToSchema (..), enum_, genericDeclareNamedSchema, type_)
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Internal.Schema (GToSchema)
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime, ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_select)
import Database.PostgreSQL.Entity.Types (Entity)
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple (Connection, FromRow, ResultError (..), ToRow)
import Database.PostgreSQL.Simple.Types (PGArray (..), Query (..))
import Database.PostgreSQL.Simple.FromField (Conversion (..), FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.Internal qualified as PGI
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Effectful (IOE, type (:>))
import GHC.Generics (Rep)
import GHC.Records (HasField (getField))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Hasql.Connection.Setting qualified as HCS
import Hasql.Connection.Setting.Connection qualified as HCSC
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Interpolate qualified as HI
import Hasql.Pool qualified as HPool
import Hasql.Pool.Config qualified as HPC
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH.Syntax qualified as THS
import Numeric (showHex)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (FromHttpApiData (..))
import Text.Casing (fromSnake, quietSnake, toPascal)


type DB es = (Hasql :> es, IOE :> es)



-- | Newtype wrapper for JSON fields that can handle JSONB, ByteString, and varchar/text columns
-- This is useful when a database column might be any of: JSONB, ByteString containing JSON, or TEXT containing JSON
-- It works with any type that has FromJSON/ToJSON instances, including Map Text Value
newtype AesonText a = AesonText a
  deriving (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, NFData)


instance (AE.FromJSON a, Typeable a) => FromField (AesonText a) where
  fromField f mdata = do
    -- Try to parse as JSONB first (via Aeson newtype)
    let tryJsonb = do
          Aeson v <- fromField f mdata :: Conversion (Aeson a)
          return (AesonText v)
    -- Try parsing as ByteString
    let tryByteString = do
          bs <- fromField f mdata :: Conversion ByteString
          case AE.eitherDecodeStrict bs of
            Right v -> return (AesonText v)
            Left err -> returnError ConversionFailed f ("Failed to parse JSON from ByteString: " ++ err)
    -- If that fails, try parsing as text/varchar
    let tryText = do
          txt <- fromField f mdata :: Conversion Text
          case AE.eitherDecodeStrict (encodeUtf8 txt) of
            Right v -> return (AesonText v)
            Left err -> returnError ConversionFailed f ("Failed to parse JSON from text: " ++ err)
    tryJsonb <|> tryByteString <|> tryText


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
  deriving stock (Generic, Read, Show, THS.Lift)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, Hashable, NFData, Ord, ToField)
  deriving anyclass (FromRow, HI.DecodeRow, ToRow)


instance KnownSymbol name => ToSchema (UUIDId name) where declareNamedSchema _ = declareNamedSchema (Proxy @UUID.UUID)
instance ToParamSchema (UUIDId name) where toParamSchema _ = toParamSchema (Proxy @UUID.UUID)


instance HasField "toText" (UUIDId name) Text where
  getField = UUID.toText . unUUIDId


instance HasField "unwrap" (UUIDId name) UUID.UUID where
  getField = coerce


-- | Convert any UUID-based ID to Text
idToText :: UUIDId name -> Text
idToText = UUID.toText . unUUIDId


-- | Parse Text to a UUID-based ID
idFromText :: Text -> Maybe (UUIDId name)
idFromText = fmap UUIDId . UUID.fromText


newtype WrappedEnum (prefix :: Symbol) a = WrappedEnum a
  deriving (Generic)


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnum prefix a) where
  toField (WrappedEnum a) = toField . T.toUpper . fromString . drop (length $ symbolVal (Proxy @prefix)) . show $ a


instance (KnownSymbol prefix, Read a, Typeable a) => FromField (WrappedEnum prefix a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss ->
      let str = symbolVal (Proxy @prefix) <> toString (T.toTitle (decodeUtf8 bss))
       in case readMaybe str of
            Just a -> pure $ WrappedEnum a
            Nothing -> returnError ConversionFailed f $ "Cannot parse: " <> str


instance (KnownSymbol prefix, Show a) => HI.EncodeValue (WrappedEnum prefix a) where
  encodeValue = E.enum (\(WrappedEnum a) -> T.toUpper $ toText $ drop (length $ symbolVal (Proxy @prefix)) $ show a)


instance (KnownSymbol prefix, Read a) => HI.DecodeValue (WrappedEnum prefix a) where
  decodeValue = D.enum \t -> WrappedEnum <$> readMaybe (symbolVal (Proxy @prefix) <> toString (T.toTitle t))

instance (KnownSymbol prefix, Read a) => HI.DecodeRow (WrappedEnum prefix a) where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


newtype WrappedEnumSC (prefix :: Symbol) a = WrappedEnumSC a
  deriving (Generic)


encodeEnumSC :: forall prefix a. (KnownSymbol prefix, Show a) => a -> String
encodeEnumSC = quietSnake . fromString . drop (length $ symbolVal (Proxy @prefix)) . show


decodeEnumSC :: forall prefix a. (KnownSymbol prefix, Read a) => String -> Maybe a
decodeEnumSC s = readMaybe $ symbolVal (Proxy @prefix) <> toPascal (fromSnake s)


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnumSC prefix a) where
  toField (WrappedEnumSC a) = toField $ encodeEnumSC @prefix a


instance (KnownSymbol prefix, Read a, Typeable a) => FromField (WrappedEnumSC prefix a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss -> maybe (returnError ConversionFailed f $ "Cannot parse: " <> str) (pure . WrappedEnumSC) $ decodeEnumSC @prefix str
      where
        str = toString @Text (decodeUtf8 bss)


instance (KnownSymbol prefix, Show a) => HI.EncodeValue (WrappedEnumSC prefix a) where
  encodeValue = E.enum (\(WrappedEnumSC a) -> toText $ encodeEnumSC @prefix a)


instance (KnownSymbol prefix, Read a) => HI.DecodeValue (WrappedEnumSC prefix a) where
  decodeValue = D.enum (fmap WrappedEnumSC . decodeEnumSC @prefix . toString)

instance (KnownSymbol prefix, Read a) => HI.DecodeRow (WrappedEnumSC prefix a) where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance HI.DecodeValue ZonedTime where
  decodeValue = utcToZonedTime utc <$> D.timestamptz

instance (HI.DecodeValue a) => HI.DecodeValue (PGArray a) where
  decodeValue = PGArray . V.toList <$> HI.decodeValue

instance (HI.EncodeValue a) => HI.EncodeValue (PGArray a) where
  encodeValue = contramap (\(PGArray xs) -> V.fromList xs) HI.encodeValue


instance HI.EncodeValue ZonedTime where
  encodeValue = contramap zonedTimeToUTC E.timestamptz


instance (KnownSymbol prefix, Show a) => Display (WrappedEnumSC prefix a) where
  displayBuilder (WrappedEnumSC a) = fromString $ encodeEnumSC @prefix a


instance (KnownSymbol prefix, Show a) => AE.ToJSON (WrappedEnumSC prefix a) where
  toJSON (WrappedEnumSC a) = AE.String . toText $ encodeEnumSC @prefix a


instance (KnownSymbol prefix, Read a, Show a) => AE.FromJSON (WrappedEnumSC prefix a) where
  parseJSON = AE.withText "WrappedEnumSC" \t ->
    maybe (fail $ "Invalid value: " <> toString t) (pure . WrappedEnumSC) $ decodeEnumSC @prefix (toString t)


instance (KnownSymbol prefix, Read a, Show a) => FromHttpApiData (WrappedEnumSC prefix a) where
  parseUrlPiece t = maybe (Left $ "Invalid " <> fromString (symbolVal (Proxy @prefix)) <> " value: " <> t) (Right . WrappedEnumSC) $ decodeEnumSC @prefix (toString @Text t)


instance {-# OVERLAPPABLE #-} (Bounded a, Enum a, KnownSymbol prefix, Show a, Typeable a) => ToSchema (WrappedEnumSC prefix a) where
  declareNamedSchema (_ :: proxy (WrappedEnumSC prefix a)) =
    pure
      $ NamedSchema Nothing
      $ mempty
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


newtype WrappedEnumShow a = WrappedEnumShow a
  deriving (Generic)


instance Show a => ToField (WrappedEnumShow a) where
  toField (WrappedEnumShow a) = toField (show a)


instance (Read a, Typeable a) => FromField (WrappedEnumShow a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss ->
      let str = toString @Text (decodeUtf8 bss)
       in case readMaybe str of
            Just a -> pure $ WrappedEnumShow a
            Nothing -> returnError ConversionFailed f $ "Cannot parse: " <> str


instance Show a => HI.EncodeValue (WrappedEnumShow a) where
  encodeValue = E.enum (\(WrappedEnumShow a) -> toText $ show a)


instance Read a => HI.DecodeValue (WrappedEnumShow a) where
  decodeValue = D.enum (fmap WrappedEnumShow . readMaybe . toString)


data BaselineState = BSLearning | BSEstablished
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "BS" BaselineState


addKeepaliveParams :: ByteString -> ByteString
addKeepaliveParams connStr =
  let params = "keepalives=1&keepalives_idle=15&keepalives_interval=10&keepalives_count=3"
      sep = if T.isInfixOf "?" (decodeUtf8 connStr) then "&" else "?"
   in connStr <> sep <> params



connectPostgreSQL :: ByteString -> IO Connection
connectPostgreSQL connstr = do
  conn <- PGI.connectdb connstr
  stat <- PQ.status conn
  case stat of
    PQ.ConnectionOk -> do
      connectionHandle <- newMVar conn
      connectionObjects <- newMVar IntMap.empty
      connectionTempNameCounter <- newIORef 0
      let wconn = PGI.Connection{..}
      pure wconn
    _ -> do
      msg <- fromMaybe "connectPostgreSQL error" <$> PQ.errorMessage conn
      throwIO $ PGI.fatalError msg


-- adds a version hash to file paths, to force cache invalidation when a new version appears
hashAssetFile :: FilePath -> TH.Q TH.Exp
hashAssetFile path = do
  content <- TH.runIO $ readFileLBS ("static" <> path)
  let hash = fromString $ showHex (xxHash content) ""
  [|$(TH.lift path) <> "?v=" <> $(TH.lift (toString hash))|]


hashFile :: FilePath -> TH.Q TH.Exp
hashFile path = do
  content <- TH.runIO $ readFileLBS ("static" <> path)
  let hash = fromString $ showHex (xxHash content) ""
  [|$(TH.lift (toString hash))|]


-- | Format a list of Floats as a PostgreSQL array literal, e.g. "{1.0,2.0,3.0}"
--
-- >>> showPGFloatArray [1.0, 2.5, 3.0]
-- "{1.0,2.5,3.0}"
--
-- >>> showPGFloatArray []
-- "{}"
showPGFloatArray :: [Float] -> Text
showPGFloatArray xs = "{" <> T.intercalate "," (map show xs) <> "}"

-- Default instances (orphans)

#if __GLASGOW_HASKELL__ < 910
instance Default Bool where
  def = False
  {-# INLINE def #-}
#endif


instance Default ZonedTime where
  def = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
  {-# INLINE def #-}


instance Default UTCTime where
  def = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
  {-# INLINE def #-}


instance Default UUID.UUID where
  def = UUID.nil
  {-# INLINE def #-}


instance Default AET.Value where
  def = AET.emptyObject
  {-# INLINE def #-}


instance ToSchema AET.Value where
  declareNamedSchema _ = pure $ NamedSchema (Just "JSONValue") mempty


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


instance (AET.FromJSON a) => HI.DecodeValue (Aeson a) where
  decodeValue = D.refine (\v -> bimap toText Aeson $ AET.parseEither AET.parseJSON v) D.jsonb

instance (AET.ToJSON a) => HI.EncodeValue (Aeson a) where
  encodeValue = contramap (\(Aeson a) -> AET.toJSON a) E.jsonb


instance HI.DecodeValue (CI Text) where
  decodeValue = CI.mk <$> D.text

instance HI.EncodeValue (CI Text) where
  encodeValue = contramap CI.original E.text


instance (Default s, FoldCase s) => Default (CI s) where
  def = CI.mk def
  {-# INLINE def #-}


instance Default T.Text where
  def = T.empty
  {-# INLINE def #-}


instance Default TL.Text where
  def = TL.empty
  {-# INLINE def #-}


instance Default (V.Vector a) where
  def = V.empty
  {-# INLINE def #-}


textArrayEnc :: E.Value (V.Vector Text)
textArrayEnc = E.array (E.dimension foldl' (E.element (E.nonNullable E.text)))
{-# INLINE textArrayEnc #-}


-- | Build a hasql pool. Timeouts are `DiffTime` (seconds): 30s acquisition,
-- 30min aging/idleness — matches the postgresql-simple pool's lifetime envelope.
mkHasqlPool :: Int -> ByteString -> IO HPool.Pool
mkHasqlPool sz cstr =
  HPool.acquire
    $ HPC.settings
      [ HPC.size sz
      , HPC.acquisitionTimeout 30
      , HPC.agingTimeout 1800
      , HPC.idlenessTimeout 1800
      , HPC.staticConnectionSettings [HCS.connection (HCSC.string (decodeUtf8 cstr))]
      ]


-- | Embed a raw @Text@ value as a literal SQL fragment (no escaping/parameterization).
-- Use only for column/table names and other trusted fragments, never user input.
rawSql :: Text -> HI.Sql
rawSql = fromString . toString


-- | Convert pg-entity's @_select \@Entity@ to an @HI.Sql@ fragment.
-- Produces @SELECT col1, col2, ... FROM schema.table@, ready to append
-- a WHERE clause via @<> [HI.sql| WHERE ... |]@.
selectFrom :: forall e. Entity e => HI.Sql
selectFrom = rawSql $ decodeUtf8 $ fromQuery (_select @e)
