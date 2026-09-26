-- Performance issues (N+1 and slow database queries) detected over completed traces.
ALTER TYPE apis.issue_type ADD VALUE IF NOT EXISTS 'performance';
