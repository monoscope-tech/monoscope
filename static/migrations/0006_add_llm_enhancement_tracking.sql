-- Add fields to track LLM enhancement status for issues
BEGIN;

ALTER TABLE apis.issues 
ADD COLUMN IF NOT EXISTS llm_enhanced_at TIMESTAMP WITH TIME ZONE,
ADD COLUMN IF NOT EXISTS llm_enhancement_version INTEGER DEFAULT 1;

-- Create an index on llm_enhanced_at to efficiently find unenhanced issues
CREATE INDEX IF NOT EXISTS idx_apis_issues_llm_enhanced_at 
ON apis.issues(llm_enhanced_at) 
WHERE llm_enhanced_at IS NULL;

-- Create an index to find recently created issues that need enhancement
CREATE INDEX IF NOT EXISTS idx_apis_issues_created_at_unenhanced 
ON apis.issues(created_at) 
WHERE llm_enhanced_at IS NULL 
  AND acknowledged_at IS NULL 
  AND archived_at IS NULL;

COMMIT;