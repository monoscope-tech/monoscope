module Pkg.IngestBudget (IngestBudget, newIngestBudget, ingestByteLimit, withIngestBytes) where

import Control.Concurrent.QSemN qualified as Q
import Relude
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket_)


-- | Shared by all Kafka consumers in an application context. Queue backpressure
-- bounds raw records; this bounds the raw bytes being expanded into decoded
-- telemetry and database parameters at the same time.
newtype IngestBudget = IngestBudget Q.QSemN


ingestByteLimit :: Int
ingestByteLimit = 64 * 1024 * 1024


newIngestBudget :: IO IngestBudget
newIngestBudget = IngestBudget <$> Q.newQSemN ingestByteLimit


-- | QSemN serves waiters in FIFO order. A record larger than the budget reserves
-- the whole budget and runs alone: rejecting or waiting for impossible capacity
-- would strand a valid Kafka record. Cancellation releases acquired capacity.
withIngestBytes :: MonadUnliftIO m => IngestBudget -> Natural -> m a -> m a
withIngestBytes (IngestBudget sem) bytes =
  let reservation = fromIntegral $ min (fromIntegral ingestByteLimit) (max 1 bytes)
   in bracket_ (liftIO $ Q.waitQSemN sem reservation) (liftIO $ Q.signalQSemN sem reservation)
