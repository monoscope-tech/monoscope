{-# LANGUAGE GADTs #-}

module Data.Effectful.UUID (
  UUIDEff,
  genUUID,
  runUUID,
  runStaticUUID,
) where

import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Effectful
import Effectful.Dispatch.Dynamic
import Relude


data UUIDEff :: Effect where
  GenUUID :: UUIDEff m UUID.UUID


type instance DispatchOf UUIDEff = 'Dynamic


genUUID :: UUIDEff :> es => Eff es UUID.UUID
genUUID = send GenUUID


runUUID :: IOE :> es => Eff (UUIDEff ': es) a -> Eff es a
runUUID = interpret $ \_ -> \case
  GenUUID -> liftIO UUID.nextRandom


-- | `runStaticUUID` is useful during tests and returns uuids by cycling
-- through a list of uuids fed into this intepreter.
runStaticUUID :: IOE :> es => [UUID.UUID] -> Eff (UUIDEff ': es) a -> Eff es a
runStaticUUID uuids eff = do
  ref <- liftIO $ newIORef (cycle uuids)
  interpret
    ( \_ -> \case
        GenUUID -> do
          liftIO $ do
            (uuid : rest) <- readIORef ref
            writeIORef ref rest
            return uuid
    )
    eff
