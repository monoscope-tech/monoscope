module Pkg.DBUtils (WrappedEnum (..), WrappedEnumSC (..), replacePlaceholders) where

import Data.Map qualified as M
import Data.Text qualified as T
import Database.PostgreSQL.Simple (ResultError (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import GHC.TypeLits
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Casing
import Text.Regex.TDFA ((=~))


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


-- | Replace all occurrences of {key} in the input text using the provided mapping.
replacePlaceholders :: M.Map T.Text T.Text -> T.Text -> T.Text
replacePlaceholders mappng input =
  let regex = "\\{([^}]+)\\}" :: T.Text
      go txt =
        case T.unpack txt =~ T.unpack regex :: (String, String, String, [String]) of
          (before, match, after, [key])
            | not (null match) ->
                let replacement = M.findWithDefault (T.pack match) (T.pack key) mappng
                 in T.pack before <> replacement <> go (T.pack after)
          _ -> txt
   in go input
