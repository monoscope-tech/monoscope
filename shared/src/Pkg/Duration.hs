-- | Shared duration/timespan parsing and rendering across KQL @bin()@,
-- TimePicker @since@, CLI flags, and monitor schedules.
module Pkg.Duration (
  DurationUnit (..),
  unitSuffix,
  sinceUnits,
  durationUnits,
  kqlUnits,
  parseDuration,
  parsePostgresInterval,
  durationToSeconds,
  durationToMs,
  durationToPostgresInterval,
) where

import Data.Char (isDigit)
import Data.List (lookup)
import Data.Set qualified as S
import Data.Text qualified as T
import Relude


data DurationUnit = Ns | Us | Ms | Sec | Min | Hour | Day | Week
  deriving stock (Bounded, Enum, Eq, Ord, Show)


-- | Allowed units for server-side @since@ parameters (@s|m|h|d@).
sinceUnits :: Set DurationUnit
sinceUnits = S.fromList [Sec, Min, Hour, Day]


-- | Allowed units for CLI duration flags (@ms|s|m|h|d@).
durationUnits :: Set DurationUnit
durationUnits = S.fromList [Ms, Sec, Min, Hour, Day]


-- | Allowed units for KQL @bin()@ intervals (@ns|us|µs|ms|s|m|h|d|w@).
kqlUnits :: Set DurationUnit
kqlUnits = S.fromList [minBound .. maxBound]


unitSuffix :: DurationUnit -> Text
unitSuffix = \case
  Ns -> "ns"
  Us -> "us"
  Ms -> "ms"
  Sec -> "s"
  Min -> "m"
  Hour -> "h"
  Day -> "d"
  Week -> "w"


unitPostgresName :: DurationUnit -> Text
unitPostgresName = \case
  Ns -> "nanoseconds"
  Us -> "microseconds"
  Ms -> "milliseconds"
  Sec -> "seconds"
  Min -> "minutes"
  Hour -> "hours"
  Day -> "days"
  Week -> "weeks"


unitSeconds :: DurationUnit -> Rational
unitSeconds = \case
  Ns -> 1e-9
  Us -> 1e-6
  Ms -> 1e-3
  Sec -> 1
  Min -> 60
  Hour -> 3600
  Day -> 86400
  Week -> 604800


-- | Parse a positive integer duration with a suffix in the allowed unit set.
-- When @caseInsensitive@ is 'True', suffix matching ignores case (e.g. @"1H"@).
--
-- >>> parseDuration True sinceUnits "24H"
-- Just (24,Hour)
-- >>> parseDuration True durationUnits "500ms"
-- Just (500,Ms)
-- >>> parseDuration False kqlUnits "1M"
-- Nothing
-- >>> parseDuration True sinceUnits "0s"
-- Nothing
-- >>> parseDuration True sinceUnits "500ms"
-- Nothing
-- >>> parseDuration False kqlUnits "10µs"
-- Just (10,Us)
parseDuration :: Bool -> Set DurationUnit -> Text -> Maybe (Int, DurationUnit)
parseDuration caseInsensitive allowed raw = do
  let trimmed = T.strip raw
      (digits, rest) = T.span isDigit trimmed
      sfx = (if caseInsensitive then T.toLower else id) $ T.strip rest
  n <- readMaybe @Int (toString digits)
  guard (n > 0)
  u <- lookup sfx suffixTable
  guard (u `S.member` allowed)
  pure (n, u)
  where
    suffixTable :: [(Text, DurationUnit)]
    suffixTable = ("µs", Us) : [(unitSuffix u, u) | u <- [minBound .. maxBound]]


-- | Parse a positive PostgreSQL interval string (e.g. @"5 minutes"@, @"1 hour"@).
--
-- >>> map parsePostgresInterval ["10 seconds", "1 hours", "500 milliseconds", "2 days", "0 seconds", "nope"]
-- [Just (10,Sec),Just (1,Hour),Just (500,Ms),Just (2,Day),Nothing,Nothing]
parsePostgresInterval :: Text -> Maybe (Int, DurationUnit)
parsePostgresInterval t = case words t of
  [num, unit] -> do
    n <- readMaybe @Int (toString num)
    guard (n > 0)
    u <- lookup (T.dropWhileEnd (== 's') unit) pgUnits
    pure (n, u)
  _ -> Nothing
  where
    pgUnits :: [(Text, DurationUnit)]
    pgUnits = [(T.dropWhileEnd (== 's') (unitPostgresName u), u) | u <- [minBound .. maxBound]]


-- | Exact duration in seconds.
--
-- >>> map (fromRational @Double . durationToSeconds) [(500, Ms), (2, Hour), (1, Week)]
-- [0.5,7200.0,604800.0]
durationToSeconds :: (Int, DurationUnit) -> Rational
durationToSeconds (n, u) = fromIntegral n * unitSeconds u


-- | Duration in milliseconds (rounded).
durationToMs :: (Int, DurationUnit) -> Int
durationToMs d = round (durationToSeconds d * 1000)


-- | Canonical PostgreSQL interval string (never passes raw user input through).
--
-- >>> parsePostgresInterval (durationToPostgresInterval (15, Min)) == Just (15, Min)
-- True
durationToPostgresInterval :: (Int, DurationUnit) -> Text
durationToPostgresInterval (n, u) = show n <> " " <> unitPostgresName u
