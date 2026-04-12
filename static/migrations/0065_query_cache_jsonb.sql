ALTER TABLE query_cache ALTER COLUMN cached_data TYPE JSONB USING cached_data::JSONB;
