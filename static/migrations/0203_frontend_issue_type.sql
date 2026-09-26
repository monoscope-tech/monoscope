-- Frontend issues (rage clicks, dead clicks) detected over browser interaction spans.
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'frontend';
