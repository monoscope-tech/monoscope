-- A NULL in any column the QueryMonitor decoder reads as non-nullable fails the whole row
-- set, not one row, so every monitor stops being checked. last_evaluated is genuinely
-- nullable (a monitor that has never run) and is now Maybe in Haskell; these two are not —
-- NULL carries no meaning for either, so close them at the source instead.
UPDATE monitors.query_monitors SET trigger_less_than = FALSE WHERE trigger_less_than IS NULL;
UPDATE monitors.query_monitors SET teams = '{}'::uuid[] WHERE teams IS NULL;

ALTER TABLE monitors.query_monitors
  ALTER COLUMN trigger_less_than SET DEFAULT FALSE,
  ALTER COLUMN trigger_less_than SET NOT NULL,
  ALTER COLUMN teams SET DEFAULT '{}'::uuid[],
  ALTER COLUMN teams SET NOT NULL;
