-- One read model over every activity ledger, so the audit page, its CSV and the API agree:
-- people's changes (audit_log), issue lifecycle events (issue_activity_log, minus views),
-- and the operations AI agents ran on someone's behalf (ai_agent_operations).
CREATE OR REPLACE VIEW projects.activity AS
SELECT a.project_id, a.created_at AS at, 'audit'::text AS source, a.actor_kind, a.actor_email AS actor,
       a.event, a.target_kind, a.target_title, a.target_id
FROM projects.audit_log a
UNION ALL
SELECT i.project_id, l.created_at, 'issue', CASE WHEN l.created_by IS NULL THEN 'system' ELSE 'user' END, u.email::text,
       l.event, 'issue', i.title, i.id::text
FROM apis.issue_activity_log l
JOIN apis.issues i ON i.id = l.issue_id
LEFT JOIN users.users u ON u.id = l.created_by
WHERE l.event <> 'viewed'
UNION ALL
SELECT o.project_id, o.created_at, 'agent', 'agent', u.email::text,
       COALESCE(o.proposal #>> '{call,function,name}', o.proposal #>> '{call,contents,tag}', 'operation'),
       'conversation', c.title, r.conversation_id::text
FROM apis.ai_agent_operations o
LEFT JOIN apis.ai_routine_runs r ON r.id = o.run_id
LEFT JOIN apis.ai_conversations c ON c.project_id = o.project_id AND c.conversation_id = r.conversation_id
LEFT JOIN users.users u ON u.id = o.requested_by;
