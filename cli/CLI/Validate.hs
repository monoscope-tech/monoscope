-- | Pure client-side validators used by the CLI before hitting the API.
-- Catching malformed input here gives agents a clear, actionable error
-- message instead of an opaque server-side HTTP 400.
module CLI.Validate
  ( validateDuration
  , validateUuid
  , validateKind
  , normalizeKind
  , validateOrDie
  , validateDurationOrDie
  , validateAndNormalizeKind
  ) where

import Relude

import CLI.Core (printError)
import Data.Char (isDigit, isHexDigit)
import Data.Text qualified as T
import Effectful


-- | Validate a relative-duration string of the form @\\d+(ms|s|m|h|d)@.
-- Returns the (already trimmed) input on success or an error message.
--
-- >>> validateDuration "10s"
-- Right "10s"
-- >>> validateDuration "30m"
-- Right "30m"
-- >>> validateDuration "2h"
-- Right "2h"
-- >>> validateDuration "7d"
-- Right "7d"
-- >>> validateDuration "500ms"
-- Right "500ms"
-- >>> validateDuration "1xyz"
-- Left "--since must match Ns|Nm|Nh|Nd|Nms (e.g. 30m, 2h, 7d); got '1xyz'"
-- >>> validateDuration ""
-- Left "--since must not be empty"
validateDuration :: Text -> Either Text Text
validateDuration t
  | T.null t = Left "--since must not be empty"
  | otherwise =
      let trimmed = T.strip t
          (digits, suffix) = T.span isDigit trimmed
       in if T.null digits || suffix `notElem` ["ms", "s", "m", "h", "d"]
            then Left $ "--since must match Ns|Nm|Nh|Nd|Nms (e.g. 30m, 2h, 7d); got '" <> t <> "'"
            else Right trimmed


-- | Loose UUID syntax check (8-4-4-4-12 hex). Lets us fail fast on @--project foo@
-- etc. rather than waiting for the server to return a 4xx.
--
-- >>> validateUuid "00000000-0000-0000-0000-000000000000"
-- Right "00000000-0000-0000-0000-000000000000"
-- >>> validateUuid "not-a-uuid"
-- Left "expected UUID (8-4-4-4-12 hex); got 'not-a-uuid'"
-- >>> validateUuid ""
-- Left "expected UUID; got empty string"
validateUuid :: Text -> Either Text Text
validateUuid t
  | T.null t = Left "expected UUID; got empty string"
  | otherwise =
      let parts = T.splitOn "-" t
       in if map T.length parts == [8, 4, 4, 4, 12] && T.all isHexDigit (T.concat parts)
            then Right t
            else Left $ "expected UUID (8-4-4-4-12 hex); got '" <> t <> "'"


-- | Whitelist for @--kind@ values accepted at the CLI surface.
-- Server-side, the @source@ query param accepts only @log@/@span@; we expose
-- @log@/@trace@ in user-facing flags and translate via 'normalizeKind'.
--
-- >>> validateKind "log"
-- Right "log"
-- >>> validateKind "trace"
-- Right "trace"
-- >>> validateKind "span"
-- Right "span"
-- >>> validateKind "banana"
-- Left "--kind must be one of: log, trace, span; got 'banana'"
validateKind :: Text -> Either Text Text
validateKind t
  | t `elem` ["log", "trace", "span"] = Right t
  | otherwise = Left $ "--kind must be one of: log, trace, span; got '" <> t <> "'"


-- | Map the user-facing @--kind@ value onto the wire-level @source@ value.
-- The server stores spans, so @trace@ rewrites to @span@; @log@ and @span@
-- pass through unchanged.
--
-- >>> normalizeKind "trace"
-- "span"
-- >>> normalizeKind "log"
-- "log"
-- >>> normalizeKind "span"
-- "span"
normalizeKind :: Text -> Text
normalizeKind "trace" = "span"
normalizeKind k = k


-- | Print the error and exit non-zero on Left; return the value on Right.
-- The standard "validate then continue" combinator at the CLI boundary.
validateOrDie :: IOE :> es => Either Text a -> Eff es a
validateOrDie = \case
  Left err -> printError err >> liftIO exitFailure
  Right a -> pure a


-- | Validate an optional duration flag, exiting on failure.
validateDurationOrDie :: IOE :> es => Maybe Text -> Eff es ()
validateDurationOrDie = mapM_ (validateOrDie . validateDuration)


-- | Validate an optional @--kind@ flag and apply 'normalizeKind' so callers
-- always get the wire-level @log@/@span@ value. Exits on failure.
validateAndNormalizeKind :: IOE :> es => Maybe Text -> Eff es (Maybe Text)
validateAndNormalizeKind = mapM (\k -> normalizeKind k <$ validateOrDie (validateKind k))
