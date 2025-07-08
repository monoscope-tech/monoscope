-- Add support for grouping multiple anomalies under a single issue
ALTER TABLE apis.issues 
ADD COLUMN IF NOT EXISTS anomaly_hashes TEXT[] DEFAULT '{}',
ADD COLUMN IF NOT EXISTS endpoint_hash TEXT DEFAULT '';

-- Add index for finding open issues by endpoint
CREATE INDEX IF NOT EXISTS idx_issues_endpoint_hash_open 
ON apis.issues (project_id, endpoint_hash) 
WHERE acknowleged_at IS NULL AND archived_at IS NULL;

-- Add index for anomaly hashes array
CREATE INDEX IF NOT EXISTS idx_issues_anomaly_hashes 
ON apis.issues USING GIN (anomaly_hashes);

-- Function to merge JSONB arrays (for payload changes)
CREATE OR REPLACE FUNCTION merge_payload_arrays(existing JSONB, new_data JSONB) 
RETURNS JSONB AS $$
BEGIN
    -- If existing is empty, return new data
    IF existing = '[]'::jsonb OR existing IS NULL THEN
        RETURN new_data;
    END IF;
    
    -- If new data is empty, return existing
    IF new_data = '[]'::jsonb OR new_data IS NULL THEN
        RETURN existing;
    END IF;
    
    -- Combine arrays and remove duplicates based on a unique key
    -- For now, just concatenate - can be improved later with deduplication
    RETURN existing || new_data;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Update the updated_at timestamp when these columns change
-- (Assuming manage_updated_at trigger already exists on the table)