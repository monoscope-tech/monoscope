-- The LLM pages read TimeFusion's llm_1h rollup tier; the Postgres per-hour cache is gone.
DROP TABLE IF EXISTS apis.llm_unit_hours;
