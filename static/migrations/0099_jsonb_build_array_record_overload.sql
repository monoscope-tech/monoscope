-- PG-side counterpart to TimeFusion's analyzer rule for `jsonb_build_array(sub.*)`.
-- PG does not expand `sub.*` inside a function call (it passes the row as a
-- single composite). This `record`-arg overload is more specific than the
-- variadic-any builtin, so PG dispatches `jsonb_build_array(sub.*)` here and
-- returns an ordered jsonb array of the record's column values — matching TF.
-- STABLE, not IMMUTABLE: row_to_json's per-column output functions can be
-- STABLE (e.g. timestamptz honoring the session TimeZone GUC).
CREATE OR REPLACE FUNCTION jsonb_build_array(rec record) RETURNS jsonb AS $$
BEGIN
  RETURN (
    SELECT jsonb_agg(value ORDER BY ord)
    FROM json_each(row_to_json(rec)) WITH ORDINALITY t(key, value, ord)
  );
END;
$$ LANGUAGE plpgsql STABLE PARALLEL SAFE;
