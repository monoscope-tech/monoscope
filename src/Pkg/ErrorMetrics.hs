-- | Process-wide counter for cheap, payload-free drop/skip metrics. Shared
-- between the OTLP wire-error path (Opentelemetry.OtlpServer) and other
-- producers (e.g. Pages.Replay). The periodic logger in OtlpServer flushes
-- this same IORef once a minute; callers from any module can `bumpErrorCounter`
-- without taking a dependency on OtlpServer's transitive imports.
module Pkg.ErrorMetrics (
  wireTypeErrorsRef,
  bumpErrorCounter,
) where

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Relude
import System.IO.Unsafe (unsafePerformIO)


wireTypeErrorsRef :: IORef (HM.HashMap Text (Int, AE.Value), Int)
{-# NOINLINE wireTypeErrorsRef #-}
wireTypeErrorsRef = unsafePerformIO $ newIORef (HM.empty, 0)


-- | Increment a counter without recording an example payload — for cases
-- where holding the bytes would defeat the purpose (oversized message
-- dropped to free heap, attacker-influenced parse failure, etc.).
bumpErrorCounter :: MonadIO m => Text -> m ()
bumpErrorCounter !key = liftIO $ atomicModifyIORef' wireTypeErrorsRef $ \(m, dropped) ->
  if HM.size m >= 100 && not (HM.member key m)
    then ((m, dropped + 1), ())
    else
      let bump Nothing = Just (1, AE.Null)
          bump (Just (c, ex)) = let !c' = c + 1 in Just (c', ex)
       in ((HM.alter bump key m, dropped), ())
