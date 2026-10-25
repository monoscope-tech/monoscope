-- | Free-tier sampling: spread a capped project's daily event budget across the UTC day
-- instead of keeping everything until the cap and nothing after it.
--
-- Pacing is ad-budget pacing: each pool keeps with probability (budget left) / (traffic
-- still expected today). Decisions hash a stable key (trace id, record id, metric series),
-- so every record sharing a key gets the same answer and a kept trace stays whole.
module Pkg.Sampling (Tally (..), errorReserve, ceiling, pace, keeps) where

import Data.Digest.XXHash (xxHash)
import Data.Fixed (mod')
import Data.Time (UTCTime (..))
import Relude hiding (ceiling)


-- | One pool's counters for a capped project, read from @projects.ingest_sampling@.
-- @seen@ counts before sampling, so yesterday's hours are a usable forecast of today's.
data Tally = Tally
  { keptToday :: Int
  , seenYesterdayAhead :: Int
  -- ^ yesterday's seen events from this hour of day onward
  , seenLastHour :: Int
  , seenThisHour :: Int
  }
  deriving stock (Eq, Show)


-- | Share of the daily budget held back for errors, so a noisy success path can't crowd them out.
errorReserve :: Double
errorReserve = 0.2


-- | Hard stop: past this many kept events in a day nothing more is kept, whatever the forecast.
--
-- >>> ceiling 10000
-- 12000
ceiling :: Int -> Int
ceiling budget = budget + budget `div` 5


-- | Keep probability for one pool: budget left over traffic still expected before 00:00 UTC.
-- The forecast is yesterday's traffic from this hour on, falling back to the recent hourly
-- rate; with no history at all, everything is kept until counters exist.
--
-- >>> import Data.Time (fromGregorian)
-- >>> let t = UTCTime (fromGregorian 2026 10 25) 43200 -- noon
-- >>> pace t 8000 (Tally 0 0 0 0)
-- 1.0
-- >>> pace t 8000 (Tally 4000 40000 0 0) -- half the budget left, 10x that still coming
-- 0.1
-- >>> pace t 8000 (Tally 4000 0 3600 0) -- no yesterday: 3600/h for the 12h left
-- 9.259259259259259e-2
-- >>> pace t 8000 (Tally 8000 40000 0 0) -- spent
-- 0.0
pace :: UTCTime -> Int -> Tally -> Double
pace now budget t
  | left <= 0 = 0
  | forecast <= 0 = 1
  | otherwise = min 1 (fromIntegral left / forecast)
  where
    left = budget - t.keptToday
    secondsLeft = realToFrac (86400 - utctDayTime now) :: Double
    perSecond
      | t.seenLastHour > 0 = fromIntegral t.seenLastHour / 3600
      | otherwise = fromIntegral t.seenThisHour / max 60 ((realToFrac (utctDayTime now) :: Double) `mod'` 3600)
    forecast = if t.seenYesterdayAhead > 0 then fromIntegral t.seenYesterdayAhead else perSecond * secondsLeft


-- | Consistent keep decision: the key's hash, scaled to [0,1), against the probability.
-- Raising @p@ only adds keys, so a trace kept at a lower rate is kept at every higher one.
--
-- >>> keeps 1 "trace-a" && not (keeps 0 "trace-a")
-- True
-- >>> let n = length (filter (keeps 0.25 . show) [1 .. 10000 :: Int]) in n > 2300 && n < 2700
-- True
keeps :: Double -> Text -> Bool
keeps p key = fromIntegral (xxHash (encodeUtf8 key)) < p * 4294967296
