module Pkg.DBUtils (WrappedEnum (..), WrappedEnumSC(..)) where

import Data.Text qualified as T
import Database.PostgreSQL.Simple (ResultError (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import GHC.TypeLits
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Casing


newtype WrappedEnum (prefix :: Symbol) a = WrappedEnum a
  deriving (Generic)


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnum prefix a) where
  toField (WrappedEnum a) = toField . T.toUpper . fromString . drop (length $ symbolVal (Proxy @prefix)) . show $ a


instance (KnownSymbol prefix, Typeable a, Read a) => FromField (WrappedEnum prefix a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss -> pure $ WrappedEnum (Unsafe.read $ symbolVal (Proxy @prefix) <> toString (T.toTitle (decodeUtf8 bss)))


-- Snakecase 
newtype WrappedEnumSC (prefix :: Symbol) a = WrappedEnumSC a
  deriving (Generic)


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnumSC prefix a) where
  toField (WrappedEnumSC a) = toField . quietSnake . fromString . drop (length $ symbolVal (Proxy @prefix)) . show $ a


instance (KnownSymbol prefix, Typeable a, Read a) => FromField (WrappedEnumSC prefix a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss -> pure $ WrappedEnumSC (Unsafe.read $ symbolVal (Proxy @prefix) <> toString (T.toTitle (decodeUtf8 bss)))
