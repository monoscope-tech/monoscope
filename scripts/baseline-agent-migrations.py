#!/usr/bin/env python3
"""Print transactional SQL to adopt consolidated migrations on the unreleased schema.

This does not connect to a database. Run the output with psql --single-transaction
only after inspecting production history and taking a backup. Old records stay intact.
"""

import base64
import hashlib
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OLD_HEAD = "588beb587947201168615a36d09ac320dd71d136"
PREVIOUS = {
    "0212_ai_routine_requester.sql": "z8tN6jCUtPHvfalfNFFCsQ==",
    "0229_pr_review_usage.sql": "MToQldtfKRkkKDk8slLI7Q==",
    "0230_pr_review_checks.sql": "MkM2QvF1KzQW9MNcpgh1Nw==",
    "0231_pr_review_knowledge.sql": "z2eLGJUJhT6+vk8IqJJQsQ==",
    "0232_issue_fix_runs.sql": "ONCPV2Nfe63j7gOedwVa2w==",
    "0233_issue_fix_execution_identity.sql": "vxtgMhTL+EuDGpPwO11vhw==",
    "0234_pr_review_attempts.sql": "2YCOBkgCfX/jnqnPQE8KiA==",
    "0235_pr_review_attempt_receipts.sql": "uGF23hlN2JBAUc18p47HnA==",
    "0236_issue_fix_usage.sql": "KkVj8fTWI0H7vQ8u5WCnQg==",
    "0237_issue_fix_jobs.sql": "dtxSewJejgRtL059+UG34Q==",
    "0238_issue_fix_account_access.sql": "5xez2yyQurtaXQDiAhJYwQ==",
    "0239_issue_fix_explanations.sql": "xoazDDWiUB4kbfQeymHSuw==",
    "0240_issue_fix_verified_checkpoint.sql": "8O48s3tmopalYdRqaNbWmA==",
    "0241_issue_fix_checkpoint_context.sql": "GMO8dLhXMciThzUGG72liQ==",
    "0242_issue_fix_media_retention.sql": "FUfhmWaqHboUTzTw4HQlhw==",
    "0243_agent_usage_activity.sql": "gYxOJOdfvZL2nlYPmCdbqg==",
    "0244_agent_workspace_allocations.sql": "rTt3jzFQ2OYe0wUEQW7Wiw==",
    "0245_issue_fix_independent_review.sql": "ICOWZqNNWJqU4x4rTYrVRQ==",
    "0246_agent_model_calls.sql": "BdUBOajZqhtTAHkxknsWzA==",
    "0247_agent_model_call_attempt.sql": "1SVCct+lQo8rI99FguHZIA==",
    "0254_project_memory_conversations.sql": "G9T11H6MCmIxqsL/MdMc2g==",
    "0255_routine_memory_grant.sql": "g3Bs4A4DVapu5+Ie5l5oAQ==",
    "0257_event_routines_and_run_artifacts.sql": "dChdYghSf6gOwl362kvWFg==",
    "0258_issue_enrichment_routine.sql": "G1IIuKMd7zVuO0luf7JFzA==",
    "0259_agent_run_checkpoints.sql": "X+BZlhIBnnbNd0Jw6/Lt/A==",
    "0260_interactive_agent_runs.sql": "bc85DSa6ekRxEOi8ckt6NQ==",
    "0261_agent_operations.sql": "5hRaQ/n6KWsir7lBfmFYxw==",
    "0262_unified_routine_authority.sql": "piDRQddX79mdhHiqlcb6tQ==",
    "0263_agent_request_roots.sql": "xKvAUu4PtYXrYLhfQ7Cu5Q==",
    "0264_agent_evidence_scope.sql": "oa0dRqfrD91AJ/wB2fRj3w==",
    "0265_agent_evidence_lookup.sql": "lvaI2N49yDqBRAQrGK/AMQ==",
    "0266_agent_role_ceiling.sql": "PUtQ7xyeC9J5KUOP4jyDAA==",
    "0267_slack_message_identity.sql": "tifHjHq1tGJ8uPYrqBnnmw==",
    "0268_wiki_run_author.sql": "AtIjdmt7NKcpeKHW15f6QQ==",
    "0269_channel_incident_routines.sql": "BMnH6/AdInWjmwIS3LYecA==",
    "0270_incident_agent_delivery.sql": "rRioPrGlGKmQelUeXp2WsA==",
    "0271_slack_user_authorization.sql": "dz6HuYqILzXr7NixxyRGXQ==",
    "0272_slack_token_refresh.sql": "R2pWi6xe1EfwJnfUVH4EXw==",
    "0273_agent_evidence_audience.sql": "XMUdINXiQeqkkXqdHeKaXw==",
    "0274_legacy_slack_evidence_audience.sql": "ov/4eQQM+ZEs+XmY1vE/Hg==",
    "0275_agent_operation_review.sql": "tE5VUFWY8mCdz1fqrJU9Dg==",
    "0276_agent_review_policy.sql": "Ono9EgG7eiaqQou6nNpY7Q==",
    "0277_member_conversation_runs.sql": "p3XuAtP/UjlpvGypn1SWrQ==",
    "0278_run_reply_publications.sql": "vLxijNvEdJVa36MQJ88dOA==",
    "0279_retained_command_origin.sql": "z/2DAI95x+2X8Rm3O9AuXw==",
    "0280_native_fix_runs.sql": "ynoYUxJ8Sv/WChhO6lzTKQ==",
    "0281_slack_progress_stream.sql": "/Qi9Fo5Lr1/j/fkEIh5VFA==",
    "0282_issue_fix_project_eligibility.sql": "1UKh8m1yf8z3rl7gilqFTw=="
}
CONSOLIDATED = (
    "0259_native_agent_records.sql",
    "0260_shared_agent_runs.sql",
    "0261_slack_agent_delivery.sql",
    "0262_legacy_slack_evidence.sql",
    "0263_native_agent_runs.sql",
)


def checksum(content):
    return base64.b64encode(hashlib.md5(content).digest()).decode("ascii")


def adoption_sql():
    previous = PREVIOUS
    current = {n: checksum((ROOT / "static/migrations" / n).read_bytes()) for n in CONSOLIDATED}
    by_number = {int(name.split("_", 1)[0]): name for name in previous}
    group_numbers = (
        set(range(229, 248)),
        set(by_number) - set(range(229, 248)) - {259, 267, 270, 271, 272, 278, 279, 280, 281, 282, 274},
        {259, 267, 270, 271, 272, 278, 279, 281},
        {274},
        {280, 282},
    )
    groups = {name: [by_number[n] for n in sorted(numbers)]
              for name, numbers in zip(CONSOLIDATED, group_numbers, strict=True)}
    memory = sorted([by_number[254], by_number[255]])
    return f"""-- Generated from original source {OLD_HEAD}; no schema or data deletion.
-- Run in one transaction: psql -X -v ON_ERROR_STOP=1 --single-transaction -f FILE
LOCK TABLE public.schema_migrations IN EXCLUSIVE MODE;
DO $baseline$
DECLARE
  previous jsonb := '{json.dumps(previous, sort_keys=True)}'::jsonb;
  consolidated jsonb := '{json.dumps(current, sort_keys=True)}'::jsonb;
  groups jsonb := '{json.dumps(groups, sort_keys=True)}'::jsonb;
  group_record record;
  applied text[];
BEGIN
  IF NOT EXISTS (SELECT 1 FROM public.schema_migrations migration
    JOIN jsonb_each_text(previous) expected ON migration.filename = expected.key) THEN
    RAISE EXCEPTION 'No original feature history; use normal startup migrations';
  END IF;
  IF EXISTS (
    SELECT migration.filename FROM public.schema_migrations migration
    JOIN jsonb_each_text(previous || consolidated) expected ON migration.filename = expected.key
    GROUP BY migration.filename, expected.value
    HAVING count(*) <> 1 OR bool_or(migration.checksum IS DISTINCT FROM expected.value)
  ) THEN
    RAISE EXCEPTION 'Original or consolidated migration checksums conflict, or history contains duplicates';
  END IF;
  FOR group_record IN SELECT * FROM jsonb_each(groups) LOOP
    SELECT COALESCE(array_agg(migration.filename ORDER BY migration.filename), ARRAY[]::text[])
      INTO applied FROM public.schema_migrations migration
      WHERE group_record.value ? migration.filename;
    IF cardinality(applied) = jsonb_array_length(group_record.value) THEN
      INSERT INTO public.schema_migrations (filename, checksum)
        SELECT group_record.key, consolidated->>group_record.key
        WHERE NOT EXISTS (SELECT 1 FROM public.schema_migrations migration
          WHERE migration.filename = group_record.key);
    ELSIF cardinality(applied) = 0 OR
      (group_record.key = '0260_shared_agent_runs.sql'
        AND to_jsonb(applied) = '{json.dumps(memory)}'::jsonb) THEN
      -- Pending DDL handles the verified older memory shape explicitly.
      NULL;
    ELSE
      RAISE EXCEPTION 'Partial original history for %; stop and inspect before adoption', group_record.key;
    END IF;
  END LOOP;
END
$baseline$;
"""


if __name__ == "__main__":
    print(adoption_sql(), end="")
