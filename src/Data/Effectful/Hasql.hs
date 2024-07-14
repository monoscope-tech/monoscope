{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Effectful.Hasql (
  -- * Effect
  Hasql (..),
  runSession,

  -- * Handlers
  runHasqlIO,
  runWithPool,
)
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Hasql.Pool (Pool, UsageError, use)
import Hasql.Session (Session)
import Relude


-- | Provides the ability to execute actions against a postgresql database
type role Hasql nominal nominal


data Hasql :: Effect where
  RunSession :: forall es a. Error UsageError :> es => Session a -> Hasql (Eff es) a


type instance DispatchOf Hasql = 'Dynamic


runSession :: (HasCallStack, Error UsageError :> es, Hasql :> es) => Session a -> Eff es a
runSession = send . RunSession


runHasqlIO
  :: forall es a
   . IOE :> es
  => Pool
  -> Eff (Hasql ': es) a
  -> Eff es a
runHasqlIO pool = interpret $ \env -> \case
  RunSession session -> do
    r <- liftIO $ use pool session
    localSeqUnlift env $ \unlift -> unlift $ do
      either throwError pure r


runWithPool :: Pool -> Eff '[Hasql, Error UsageError, IOE] a -> IO (Either (CallStack, UsageError) a)
runWithPool pool = runEff . runError @UsageError . runHasqlIO pool
