module Pkg.DeriveUtils (
  AesonText (..),
  BaselineState (..),
  PGTextArray (..),
  UUIDId (..),
  WrappedEnum (..),
  WrappedEnumSC (..),
  WrappedEnumShow (..),
  connectPostgreSQL,
  idToText,
  idFromText,
  unAesonText,
  unAesonTextMaybe,
  hashAssetFile,
  hashFile,
) where

import Control.Exception (throwIO)
import Data.Aeson qualified as AE
import Data.Default (Default (..))
import Data.Default.Instances ()
import Data.Digest.XXHash (xxHash)
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple (Connection, FromRow, ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (Conversion (..), FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.Internal qualified as PGI
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Deriving.Aeson qualified as DAE
import GHC.Records (HasField (getField))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Language.Haskell.TH.Syntax qualified as THS
import Numeric (showHex)
import Relude
import Servant (FromHttpApiData (..))
import Text.Casing (fromSnake, quietSnake, toPascal)
import Web.HttpApiData (FromHttpApiData)


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
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, Hashable, NFData, Ord, ToField)
  deriving anyclass (FromRow, ToRow)


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


newtype WrappedEnumSC (prefix :: Symbol) a = WrappedEnumSC a
  deriving (Generic)


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnumSC prefix a) where
  toField (WrappedEnumSC a) = toField . quietSnake . fromString . drop (length $ symbolVal (Proxy @prefix)) . show $ a


instance (KnownSymbol prefix, Read a, Typeable a) => FromField (WrappedEnumSC prefix a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss ->
      let str = symbolVal (Proxy @prefix) <> toPascal (fromSnake $ toString @Text (decodeUtf8 bss))
       in case readMaybe str of
            Just a -> pure $ WrappedEnumSC a
            Nothing -> returnError ConversionFailed f $ "Cannot parse: " <> str


instance (KnownSymbol prefix, Show a) => Display (WrappedEnumSC prefix a) where
  displayBuilder (WrappedEnumSC a) = fromString . quietSnake . drop (length $ symbolVal (Proxy @prefix)) . show $ a


instance (KnownSymbol prefix, Read a, Show a) => FromHttpApiData (WrappedEnumSC prefix a) where
  parseUrlPiece t =
    case readMaybe (symbolVal (Proxy @prefix) <> toPascal (fromSnake $ toString @Text t)) of
      Just a -> Right $ WrappedEnumSC a
      Nothing -> Left $ "Invalid " <> fromString (symbolVal (Proxy @prefix)) <> " value: " <> t


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


-- | Baseline state for anomaly detection.
data BaselineState = BSLearning | BSEstablished
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "BS", DAE.CamelToSnake]] BaselineState
  deriving (FromField, ToField) via WrappedEnumSC "BS" BaselineState


instance Default BaselineState where
  def = BSLearning


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
