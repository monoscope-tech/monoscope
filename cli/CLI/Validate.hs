-- | Pure client-side validators used by the CLI before hitting the API.
-- Catching malformed input here gives agents a clear, actionable error
-- message instead of an opaque server-side HTTP 400.
module CLI.Validate (
  EventKind (..),
  validateDurationFor,
  validateSinceFor,
  validateKind,
  validateOrDie,
  validateDurationOrDie,
  validateSinceOrDie,
  validateKindOrDie,
  validateQueryOrDie,
) where

import Relude

import CLI.Core (dieWith)
import Data.Text qualified as T
import Effectful
import Pkg.Duration qualified as Duration
import Pkg.Parser.Stats (parseQueryToAST)


-- | Validate a relative-duration string against the allowed unit suffixes.
-- Suffix matching is case-insensitive — the platform's TimePicker emits
-- uppercase forms ("1H", "24H") and the server accepts either. Original casing
-- is preserved on success.
validateUnitsFor :: [Duration.DurationUnit] -> Text -> Text -> Either Text Text
validateUnitsFor allowed flag t
  | T.null t = Left $ flag <> " must not be empty"
  | Just _ <- Duration.parseDuration Duration.CaseInsensitive allowed t = Right (T.strip t)
  | otherwise = Left $ flag <> " must match " <> T.intercalate "|" (map (("N" <>) . Duration.unitSuffix) allowed) <> " (e.g. 30m, 2h, 7d); got '" <> t <> "'"


-- | Validate a client-side duration (@s|m|h|d|ms@) for flags parsed locally
-- by @parseDurationMs@ (@--interval@, @--window@, @--watch@).
--
-- >>> validateDurationFor "--interval" "500ms"
-- Right "500ms"
-- >>> validateDurationFor "--window" "1H"
-- Right "1H"
-- >>> validateDurationFor "--window" "1xyz"
-- Left "--window must match Ns|Nm|Nh|Nd|Nms (e.g. 30m, 2h, 7d); got '1xyz'"
-- >>> validateDurationFor "--window" ""
-- Left "--window must not be empty"
validateDurationFor :: Text -> Text -> Either Text Text
validateDurationFor = validateUnitsFor Duration.durationUnits


-- | Validate a server-side @--since@ duration (@s|m|h|d@).
--
-- >>> validateSinceFor "--since" "10s"
-- Right "10s"
-- >>> validateSinceFor "--since" "1H"
-- Right "1H"
-- >>> validateSinceFor "--since" "500ms"
-- Left "--since must match Ns|Nm|Nh|Nd (e.g. 30m, 2h, 7d); got '500ms'"
-- >>> validateSinceFor "--since" ""
-- Left "--since must not be empty"
validateSinceFor :: Text -> Text -> Either Text Text
validateSinceFor = validateUnitsFor Duration.sinceUnits


-- | Validated @--kind@ flag: @log@ → 'EKLog', @span@/@trace@ → 'EKSpan'.
data EventKind = EKLog | EKSpan
  deriving stock (Eq, Show)


-- | Parse and canonicalize @--kind@ values accepted at the CLI surface
-- (@trace@ aliases @span@). Folded into the KQL query as @kind=="log"@ or
-- @kind!="log"@ (server @source@ selects the table, not kind).
--
-- >>> validateKind "log"
-- Right EKLog
-- >>> validateKind "trace"
-- Right EKSpan
-- >>> validateKind "span"
-- Right EKSpan
-- >>> validateKind "banana"
-- Left "--kind must be one of: log, trace, span; got 'banana'"
validateKind :: Text -> Either Text EventKind
validateKind = \case
  "log" -> Right EKLog
  "span" -> Right EKSpan
  "trace" -> Right EKSpan
  t -> Left $ "--kind must be one of: log, trace, span; got '" <> t <> "'"


-- | Print the error and exit non-zero on Left; return the value on Right.
-- The standard "validate then continue" combinator at the CLI boundary.
validateOrDie :: IOE :> es => Either Text a -> Eff es a
validateOrDie = either dieWith pure


-- | Validate an optional client-side duration flag (@s|m|h|d|ms@), exiting on failure.
validateDurationOrDie :: IOE :> es => Text -> Maybe Text -> Eff es ()
validateDurationOrDie flag = mapM_ (validateOrDie . validateDurationFor flag)


-- | Validate an optional server-side @--since@ duration flag (@s|m|h|d@), exiting on failure.
validateSinceOrDie :: IOE :> es => Maybe Text -> Eff es ()
validateSinceOrDie = mapM_ (validateOrDie . validateSinceFor "--since")


-- | Validate an optional @--kind@ flag into an 'EventKind', exiting on failure.
validateKindOrDie :: IOE :> es => Maybe Text -> Eff es (Maybe EventKind)
validateKindOrDie = traverse (validateOrDie . validateKind)


-- | Parse the KQL query client-side; on failure print Megaparsec's pretty
-- error and exit. Empty queries pass through — the server accepts them.
validateQueryOrDie :: IOE :> es => Text -> Eff es ()
validateQueryOrDie q
  | T.null (T.strip q) = pass
  | otherwise = case parseQueryToAST q of
      Right _ -> pass
      Left err -> dieWith ("invalid query: " <> err)
