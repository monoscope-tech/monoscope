module Pkg.DBUtils (WrappedEnum (..), WrappedEnumSC (..), connectPostgreSQL) where

import Control.Exception
import Data.IntMap qualified as IntMap
import Data.Pool (Pool, withResource)
import Data.Text qualified as T
import Database.PostgreSQL.LibPQ qualified as PQ
import Database.PostgreSQL.Simple (Connection, ResultError (..))
import Database.PostgreSQL.Simple.FromField (FromField (..), fromField, returnError)
import Database.PostgreSQL.Simple.Internal qualified as PGI
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Transact (DBT (DBT))
import Effectful (Dispatch (Static), DispatchOf, Eff, (:>))
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static (StaticRep)
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


connectPostgreSQL :: ByteString -> IO Connection
connectPostgreSQL connstr = do
  traceShowM "conn A"
  conn <- PGI.connectdb connstr
  stat <- PQ.status conn
  case stat of
    PQ.ConnectionOk -> do
      traceShowM "conn D"
      connectionHandle <- newMVar conn
      connectionObjects <- newMVar (IntMap.empty)
      connectionTempNameCounter <- newIORef 0
      let wconn = PGI.Connection{..}
      -- version <- PQ.serverVersion conn
      -- _ <- PGI.execute_ wconn ""
      return wconn
    _ -> do
      traceShowM "conn J"
      msg <- maybe "connectPostgreSQL error" id <$> PQ.errorMessage conn
      traceShowM "conn K"
      throwIO $ PGI.fatalError msg
