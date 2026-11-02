#!/usr/bin/env python3
"""Exercise baseline adoption against an isolated PostgreSQL database (libpq env)."""
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import unittest
import uuid

spec = importlib.util.spec_from_file_location("baseline", Path(__file__).with_name("baseline-agent-migrations.py"))
baseline = importlib.util.module_from_spec(spec)
spec.loader.exec_module(baseline)


def psql(database, sql, check=True):
    return subprocess.run(["psql", "-X", "-At", "-v", "ON_ERROR_STOP=1", "--single-transaction", "-d", database],
                          input=sql, text=True, capture_output=True, check=check)


class BaselineAdoption(unittest.TestCase):
    def test_preserves_data_and_history_and_refuses_unsafe_adoption(self):
        database = "agent_baseline_test_" + uuid.uuid4().hex
        admin = os.environ.get("PGDATABASE", "postgres")
        subprocess.run(["createdb", "--maintenance-db", admin, database], check=True, capture_output=True)
        try:
            original = baseline.PREVIOUS
            self.assertEqual(len(original), 48)
            fixture = """CREATE TABLE schema_migrations (filename varchar(512), checksum varchar(32), executed_at timestamp DEFAULT now());
CREATE TABLE retained_evidence (id int PRIMARY KEY, payload jsonb);
INSERT INTO retained_evidence VALUES (1, '{"run":"original","policy":"autonomous","review":"resolved"}');
""" + "INSERT INTO schema_migrations (filename, checksum) SELECT * FROM jsonb_each_text('" + json.dumps(original) + "'::jsonb);"
            fixture += " INSERT INTO schema_migrations (filename, checksum) VALUES ('unrelated.sql', 'preserved');"
            psql(database, fixture)
            adoption = baseline.adoption_sql()
            retained = "SELECT row_to_json(evidence) FROM retained_evidence evidence;"
            before = psql(database, retained).stdout
            old_rows = "SELECT filename,checksum,executed_at FROM schema_migrations WHERE filename NOT LIKE '0259_native_agent_records.sql' AND filename NOT IN ('0260_shared_agent_runs.sql','0261_slack_agent_delivery.sql','0262_legacy_slack_evidence.sql','0263_native_agent_runs.sql') ORDER BY filename;"
            history = psql(database, old_rows).stdout
            psql(database, adoption)
            psql(database, adoption)
            self.assertEqual(psql(database, "SELECT count(*) FROM schema_migrations;").stdout.strip(), "54")
            self.assertEqual(psql(database, retained).stdout, before)
            self.assertEqual(psql(database, old_rows).stdout, history)
            production = {n: c for n, c in original.items()
                          if 229 <= int(n.split("_", 1)[0]) <= 247 or n.startswith(("0254_", "0255_"))}
            production_sql = "SAVEPOINT production_shape; DELETE FROM schema_migrations WHERE filename NOT IN (SELECT key FROM jsonb_each_text('" + json.dumps(production) + "'::jsonb));"
            result = psql(database, production_sql + adoption + "SELECT count(*) FROM schema_migrations; ROLLBACK TO SAVEPOINT production_shape;")
            self.assertIn("22", result.stdout.splitlines())
            self.assertEqual(psql(database, retained).stdout, before)
            self.assertEqual(psql(database, old_rows).stdout, history)
            name = next(iter(original))
            corruptions = (
                f"DELETE FROM schema_migrations WHERE filename = '{name}';",
                f"UPDATE schema_migrations SET checksum = 'changed' WHERE filename = '{name}';",
                f"INSERT INTO schema_migrations SELECT * FROM schema_migrations WHERE filename = '{name}';",
                "UPDATE schema_migrations SET checksum = 'changed' WHERE filename = '0260_shared_agent_runs.sql';",
                "UPDATE schema_migrations SET checksum = NULL WHERE filename = '0260_shared_agent_runs.sql';",
                "INSERT INTO schema_migrations SELECT * FROM schema_migrations WHERE filename = '0260_shared_agent_runs.sql';",
                "DELETE FROM schema_migrations;",
            )
            for corruption in corruptions:
                with self.subTest(corruption=corruption):
                    result = psql(database, corruption + adoption, check=False)
                    self.assertNotEqual(result.returncode, 0)
                    self.assertIn("ERROR:", result.stderr)
                    self.assertEqual(psql(database, retained).stdout, before)
                    self.assertEqual(psql(database, old_rows).stdout, history)
                    self.assertEqual(psql(database, "SELECT count(*) FROM schema_migrations;").stdout.strip(), "54")
        finally:
            subprocess.run(["dropdb", "--maintenance-db", admin, database], check=True, capture_output=True)


if __name__ == "__main__":
    unittest.main()
