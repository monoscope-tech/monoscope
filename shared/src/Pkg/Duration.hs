-- | Shared duration/timespan parsing and rendering across KQL @bin()@,
-- TimePicker @since@, CLI flags, and monitor schedules.
module Pkg.Duration (
  DurationUnit (..),
  CaseSensitivity (..),
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
import Data.Text qualified as T
import Relude


data DurationUnit = Ns | Us | Ms | Sec | Min | Hour | Day | Week
  deriving stock (Bounded, Enum, Eq, Ord, Show)


data CaseSensitivity = CaseSensitive | CaseInsensitive
  deriving stock (Eq, Show)


-- | Canonical (compact suffix, PostgreSQL plural unit, seconds) for each unit.
unitInfo :: DurationUnit -> (Text, Text, Rational)
unitInfo = \case
  Ns -> ("ns", "nanoseconds", 1e-9)
  Us -> ("us", "microseconds", 1e-6)
  Ms -> ("ms", "milliseconds", 1e-3)
  Sec -> ("s", "seconds", 1)
  Min -> ("m", "minutes", 60)
  Hour -> ("h", "hours", 3600)
  Day -> ("d", "days", 86400)
  Week -> ("w", "weeks", 604800)


unitSuffix :: DurationUnit -> Text
unitSuffix u = let (s, _, _) = unitInfo u in s


-- | Allowed units for server-side @since@ parameters (@s|m|h|d@).
sinceUnits :: [DurationUnit]
sinceUnits = [Sec, Min, Hour, Day]


-- | Allowed units for CLI duration flags (@s|m|h|d|ms@).
durationUnits :: [DurationUnit]
durationUnits = [Sec, Min, Hour, Day, Ms]


-- | Allowed units for KQL @bin()@ intervals (@ns|us|µs|ms|s|m|h|d|w@).
kqlUnits :: [DurationUnit]
kqlUnits = [minBound .. maxBound]


-- | Parse a positive integer duration with a suffix in the allowed unit list.
--
-- >>> parseDuration CaseInsensitive sinceUnits "24H"
-- Just (24,Hour)
-- >>> parseDuration CaseInsensitive durationUnits "500ms"
-- Just (500,Ms)
-- >>> parseDuration CaseSensitive kqlUnits "1M"
-- Nothing
-- >>> parseDuration CaseInsensitive sinceUnits "0s"
-- Nothing
-- >>> parseDuration CaseInsensitive sinceUnits "500ms"
-- Nothing
-- >>> parseDuration CaseSensitive kqlUnits "10µs"
-- Just (10,Us)
parseDuration :: CaseSensitivity -> [DurationUnit] -> Text -> Maybe (Int, DurationUnit)
parseDuration caseSens allowed raw = do
  let trimmed = T.strip raw
      (digits, rest) = T.span isDigit trimmed
      sfx = (case caseSens of CaseInsensitive -> T.toLower; CaseSensitive -> id) $ T.strip rest
  n <- readMaybe @Int (toString digits)
  guard (n > 0)
  u <- lookup sfx suffixTable
  guard (u `elem` allowed)
  pure (n, u)
  where
    suffixTable :: [(Text, DurationUnit)]
    suffixTable = ("µs", Us) : [(unitSuffix u, u) | u <- [minBound .. maxBound]]


-- | Parse a positive PostgreSQL interval string (e.g. @"5 minutes"@, @"1 hour"@).
--
-- >>> map parsePostgresInterval ["10 seconds", "1 hour", "500 milliseconds", "2 days", "0 seconds", "10 secondss", "nope"]
-- [Just (10,Sec),Just (1,Hour),Just (500,Ms),Just (2,Day),Nothing,Nothing,Nothing]
parsePostgresInterval :: Text -> Maybe (Int, DurationUnit)
parsePostgresInterval t = case words t of
  [num, unit] -> do
    n <- readMaybe @Int (toString num)
    guard (n > 0)
    let singular = fromMaybe unit (T.stripSuffix "s" unit)
    u <- lookup singular pgUnits
    pure (n, u)
  _ -> Nothing
  where
    pgUnits :: [(Text, DurationUnit)]
    pgUnits = [(fromMaybe nm (T.stripSuffix "s" nm), u) | u <- [minBound .. maxBound], let (_, nm, _) = unitInfo u]


-- | Exact duration in seconds.
--
-- >>> map (fromRational @Double . durationToSeconds) [(500, Ms), (2, Hour), (1, Week)]
-- [0.5,7200.0,604800.0]
durationToSeconds :: (Int, DurationUnit) -> Rational
durationToSeconds (n, u) = let (_, _, secs) = unitInfo u in fromIntegral n * secs


-- | Duration in milliseconds (rounded).
durationToMs :: (Int, DurationUnit) -> Int
durationToMs d = round (durationToSeconds d * 1000)


-- | Canonical PostgreSQL interval string (never passes raw user input through).
--
-- >>> parsePostgresInterval (durationToPostgresInterval (15, Min)) == Just (15, Min)
-- True
durationToPostgresInterval :: (Int, DurationUnit) -> Text
durationToPostgresInterval (n, u) = let (_, nm, _) = unitInfo u in show n <> " " <> nm
