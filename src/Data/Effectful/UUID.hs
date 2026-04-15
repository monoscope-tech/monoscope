module Data.Effectful.UUID (
  UUID.UUID,
  UUID.toText,
  UUIDEff,
  genUUID,
  runUUID,
  runStaticUUID,
  runStaticUUIDRef,
) where

import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Relude


data UUIDEff :: Effect where
  GenUUID :: UUIDEff m UUID.UUID


type instance DispatchOf UUIDEff = 'Dynamic


makeEffect ''UUIDEff


-- TODO: swap to UUIDv7 for time-sortable ids (hypertable BRIN locality) once a
-- maintained Hackage library ships one. Today the `uuid` package tops out at V4
-- and there is no `uuid-v7` package available — keeping V4 until then.
runUUID :: IOE :> es => Eff (UUIDEff ': es) a -> Eff es a
runUUID = interpret $ \_ -> \case
  GenUUID -> liftIO UUID.nextRandom


-- | `runStaticUUID` is useful during tests and returns uuids by cycling
-- through a list of uuids fed into this intepreter.
runStaticUUID :: IOE :> es => [UUID.UUID] -> Eff (UUIDEff ': es) a -> Eff es a
runStaticUUID uuids eff = do
  ref <- liftIO $ newIORef (cycle uuids)
  runStaticUUIDRef ref eff


-- | Variant that reuses an externally-managed IORef so multiple effect runs
-- in the same test draw from a shared, non-resetting counter (avoids id
-- collisions across handler invocations).
runStaticUUIDRef :: IOE :> es => IORef [UUID.UUID] -> Eff (UUIDEff ': es) a -> Eff es a
runStaticUUIDRef ref =
  interpret \_ -> \case
    GenUUID -> liftIO $ atomicModifyIORef' ref \case
      (u : rs) -> (rs, u)
      [] -> error "runStaticUUIDRef: empty UUID pool"
