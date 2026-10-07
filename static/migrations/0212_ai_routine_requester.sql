ALTER TABLE apis.ai_routines
  ADD COLUMN requested_by UUID REFERENCES users.users(id);

-- Legacy routines have no recorded authorization. A member must resume them.
UPDATE apis.ai_routines SET active = FALSE, cancelled_at = clock_timestamp()
WHERE requested_by IS NULL;

ALTER TABLE apis.ai_routines
  ADD CONSTRAINT ai_routines_active_requester CHECK (NOT active OR requested_by IS NOT NULL);
