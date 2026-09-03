-- The key errors are grouped for LLM review under: error type plus normalised
-- message, without service, span name or stack.
--
-- Deliberately a new column rather than a reuse of `parent_hash`. That column holds
-- broad-hash values on all 23,372 existing rows and is refreshed only on resolve, so
-- reading it as a shape would match the wrong rows — silently, and worst during the
-- re-keying wave this exists to survive.
--
-- No backfill here: the shape is a normalisation of the message, computed in Haskell
-- (`Pkg.ErrorFingerprint.computeErrorHashes`), and there is no SQL equivalent. The
-- upsert fills it with COALESCE, so a row acquires its shape the next time it occurs
-- and never pays a rewrite after that. Rows that never occur again do not need one:
-- there is nothing left for them to suppress. Review-time clustering computes the
-- shape in Haskell and does not depend on this column being populated.
ALTER TABLE apis.error_patterns ADD COLUMN IF NOT EXISTS shape_hash TEXT;

-- Partial: the lookups are all "rows of this shape that are not themselves merged",
-- and the merged rows are the ones we never want back.
--
-- Not CONCURRENTLY: the runner executes each migration inside a transaction, where
-- that is an error. The table is ~23k rows, so the exclusive lock is brief.
CREATE INDEX IF NOT EXISTS error_patterns_shape_hash_idx
  ON apis.error_patterns (project_id, shape_hash)
  WHERE shape_hash IS NOT NULL AND canonical_id IS NULL;
