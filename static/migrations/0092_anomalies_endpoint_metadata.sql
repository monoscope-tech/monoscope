-- Carry endpoint metadata directly on each anomaly row so the
-- new-endpoint notification path doesn't depend on a successful join back to
-- apis.endpoints. Prior bug: schema-learning hashed the raw path while the
-- endpoint writer hashed the canonical path, so the join missed and the Slack
-- template fell back to literal "UNKNOWN /". Even after fixing the hash skew,
-- a hash mismatch should degrade gracefully instead of silently shipping
-- garbage labels to customers.
ALTER TABLE apis.anomalies
  ADD COLUMN IF NOT EXISTS method   text,
  ADD COLUMN IF NOT EXISTS host     text,
  ADD COLUMN IF NOT EXISTS url_path text;
