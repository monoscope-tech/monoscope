-- Normalize apis.issues.severity to the closed set IssueSeverity encodes:
-- 'critical' | 'warning' | 'info' | 'low'. Legacy rows carried ''/NULL (handled
-- until now by a COALESCE at read time). Typing the column in Haskell requires
-- the stored value to always be valid, so backfill and enforce it here.
--
-- The backfill mirrors the previous read-time fallback
--   COALESCE(NULLIF(severity, ''), CASE WHEN critical THEN 'critical' ELSE 'info' END)
-- so no issue changes its effective displayed severity.
UPDATE apis.issues
SET severity = CASE WHEN critical THEN 'critical' ELSE 'info' END
WHERE severity IS NULL
   OR severity = ''
   OR severity NOT IN ('critical', 'warning', 'info', 'low');

ALTER TABLE apis.issues ALTER COLUMN severity SET DEFAULT 'info';
ALTER TABLE apis.issues ALTER COLUMN severity SET NOT NULL;
