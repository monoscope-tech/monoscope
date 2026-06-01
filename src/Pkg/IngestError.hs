-- | Classifies ingestion-pipeline exceptions into transient infra failures
-- (retry: don't commit Kafka offset) versus poison (DLQ + commit).
--
-- Conservative default for unknown exceptions: 'PoisonBatch'. The cost is "rare
-- transient that we mistakenly DLQ" — easy to spot in DLQ depth and to act on.
-- The alternative default ('TransientInfra') hides bugs as permanent partition
-- stalls. Visible-and-actionable wins over silent-and-stuck.
module Pkg.IngestError (IngestError (..), classify, isTransient, isPoison) where

import Data.Effectful.Hasql qualified as Hasql
import Relude


data IngestError = TransientInfra | PoisonBatch
  deriving stock (Eq, Show)


classify :: SomeException -> IngestError
classify e
  | Just he <- fromException @Hasql.HasqlException e
  , Hasql.isTransientHasqlError he =
      TransientInfra
  | otherwise = PoisonBatch


isTransient, isPoison :: IngestError -> Bool
isTransient = (== TransientInfra)
isPoison = (== PoisonBatch)
