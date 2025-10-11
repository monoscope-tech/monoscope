module Pkg.DBUtils (WrappedEnum (..), WrappedEnumSC (..), connectPostgreSQL) where

import Control.Exception
import Data.IntMap qualified as IntMap
import Data.Text qualified as T
import Data.Text.Display (Display (..))
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple (Connection, ResultError (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.Internal qualified as PGI
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import GHC.TypeLits

-- import OpenTelemetry.Instrumentation.PostgresqlSimple (postgreSQLSimpleInstrumentationConfig, wrapConnection)
import Relude
import Relude.Unsafe qualified as Unsafe
import Text.Casing
import Type.Reflection (typeRep)
import Web.FormUrlEncoded (FromForm (..), lookupUnique)
import Web.HttpApiData (FromHttpApiData (..))


newtype WrappedEnum (prefix :: Symbol) a = WrappedEnum a
  deriving (Generic)


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnum prefix a) where
  toField (WrappedEnum a) = toField . T.toUpper . fromString . drop (length $ symbolVal (Proxy @prefix)) . show $ a


instance (KnownSymbol prefix, Read a, Typeable a) => FromField (WrappedEnum prefix a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss -> pure $ WrappedEnum (Unsafe.read $ symbolVal (Proxy @prefix) <> toString (T.toTitle (decodeUtf8 bss)))


-- Snakecase
newtype WrappedEnumSC (prefix :: Symbol) a = WrappedEnumSC a
  deriving (Generic)


instance (KnownSymbol prefix, Show a) => ToField (WrappedEnumSC prefix a) where
  toField (WrappedEnumSC a) = toField . quietSnake . fromString . drop (length $ symbolVal (Proxy @prefix)) . show $ a


instance (KnownSymbol prefix, Read a, Typeable a) => FromField (WrappedEnumSC prefix a) where
  fromField f = \case
    Nothing -> returnError UnexpectedNull f ""
    Just bss -> pure $ WrappedEnumSC (Unsafe.read $ symbolVal (Proxy @prefix) <> toString (T.toTitle (decodeUtf8 bss)))


-- Display instance that shows the value in snake_case without prefix
instance (KnownSymbol prefix, Show a) => Display (WrappedEnumSC prefix a) where
  displayBuilder (WrappedEnumSC a) = fromString . quietSnake . drop (length $ symbolVal (Proxy @prefix)) . show $ a


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
      -- Wrap the connection with OpenTelemetry instrumentation
      -- wrapConnection postgreSQLSimpleInstrumentationConfig wconn
      pure wconn
    _ -> do
      msg <- fromMaybe "connectPostgreSQL error" <$> PQ.errorMessage conn
      throwIO $ PGI.fatalError msg
