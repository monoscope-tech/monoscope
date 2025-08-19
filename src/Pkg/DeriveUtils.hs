module Pkg.DeriveUtils (
  AesonText (..),
  PGTextArray (..),
) where

import Data.Aeson qualified as AE
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.FromField (Conversion (..), FromField (..), returnError, fromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple (ResultError (ConversionFailed))
import Relude


-- | Newtype wrapper for JSON fields that can handle both JSONB and varchar/text columns
-- This is useful when a database column might be either JSONB or TEXT containing JSON
-- It works with any type that has FromJSON/ToJSON instances, including Map Text Value
newtype AesonText a = AesonText a
  deriving (Generic, Show, Eq)
  deriving newtype (AE.FromJSON, AE.ToJSON, NFData)

instance (AE.FromJSON a, Typeable a) => FromField (AesonText a) where
  fromField f mdata = do
    -- Try to parse as JSONB first (via Aeson newtype)
    let tryJsonb = do
          Aeson v <- fromField f mdata :: Conversion (Aeson a)
          return (AesonText v)
    -- If that fails, try parsing as text/varchar
    let tryText = do
          txt <- fromField f mdata :: Conversion Text
          case AE.eitherDecodeStrict (encodeUtf8 txt) of
            Right v -> return (AesonText v)
            Left err -> returnError ConversionFailed f ("Failed to parse JSON from text: " ++ err)
    tryJsonb <|> tryText

instance (AE.ToJSON a) => ToField (AesonText a) where
  toField (AesonText v) = toField (Aeson v)


-- | Newtype wrapper for PostgreSQL text arrays (TEXT[])
newtype PGTextArray = PGTextArray (V.Vector Text)
  deriving (Generic, Show, Eq)
  deriving newtype (NFData)

instance FromField PGTextArray where
  fromField f mdata = do
    -- PostgreSQL arrays can come in different formats
    arr <- fromField f mdata :: Conversion (V.Vector Text)
    return $ PGTextArray arr

instance ToField PGTextArray where
  toField (PGTextArray v) = toField v
