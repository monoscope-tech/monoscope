module Pkg.DeriveUtils (
  AesonText (..),
  PGTextArray (..),
  UUIDId (..),
  idToText,
  idFromText,
  unAesonText,
  unAesonTextMaybe,
) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Default.Instances ()
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (FromRow, ResultError (ConversionFailed), ToRow)
import Database.PostgreSQL.Simple.FromField (Conversion (..), FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)
import Language.Haskell.TH.Syntax qualified as THS
import Relude
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
  deriving newtype (NFData)


instance FromField PGTextArray where
  fromField f mdata = do
    -- PostgreSQL arrays can come in different formats
    arr <- fromField f mdata :: Conversion (V.Vector Text)
    return $ PGTextArray arr


instance ToField PGTextArray where
  toField (PGTextArray v) = toField v


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
