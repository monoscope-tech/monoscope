# Integration Testing Guide

## Running Tests

### Standard CI Mode (uses golden files only)
```bash
USE_EXTERNAL_DB=true cabal test integration-tests
```

Tests will use cached golden files and fail if any are missing.

### Updating Golden Files (makes real API calls)
```bash
UPDATE_GOLDEN=true USE_EXTERNAL_DB=true cabal test integration-tests
```

This will:
- Make real HTTP calls to Twilio, Discord, Slack, etc.
- Make real LLM calls to OpenAI API
- Cache all responses to golden files
- Requires credentials in `.env` file

### Running Specific Test Suites
```bash
USE_EXTERNAL_DB=true cabal test integration-tests \
  --test-option="--match=/Discord Bot/"
```

## Golden File System

Golden files cache real API responses to make tests:
- **Fast**: No network calls in CI
- **Deterministic**: Same response every time
- **Runnable without credentials**: Golden files checked into repo

### When to Update Golden Files

1. **After changing bot response format** - Regenerate bot golden files
2. **After changing LLM prompts** - Regenerate LLM golden files
3. **After external API changes** - Regenerate HTTP golden files
4. **When adding new tests** - Generate golden files for new test cases

### Golden File Locations

- HTTP responses: `tests/golden/*.json` (URL-based filenames)
- Bot responses: `tests/golden/bots/{discord,slack,whatsapp}/*.json`
- LLM responses: `tests/golden/agentic_*.json` (hash-based filenames)

### Missing Golden Files

If you see errors like:
```
Golden file not found: tests/golden/https___api.twilio.com_...json
Run tests with UPDATE_GOLDEN=true to create it
```

This means a new test was added without golden files. To fix:

```bash
UPDATE_GOLDEN=true USE_EXTERNAL_DB=true cabal test integration-tests
```

**Important**: Commit the new golden files to the repository!

## External Database Mode

Integration tests use `USE_EXTERNAL_DB=true` for:
- Better isolation (each test run gets fresh DB from template)
- Faster test runs (template cached, no migration reruns)
- Reliable cleanup (force-terminate connections)

### Requirements

- PostgreSQL running on `localhost:5432`
- User: `postgres`, Password: `postgres` (or set `EXTERNAL_DB_URL`)
- TimescaleDB extension installed

### Database Lifecycle

1. Template database created: `monoscope_test_template`
2. Test database cloned: `monoscope_test_{uuid}`
3. Tests run with isolated database
4. Connections terminated and database dropped

## Test Credentials

Tests need real credentials in `.env` for `UPDATE_GOLDEN=true` mode:

```env
TWILIO_ACCOUNT_SID=AC...
TWILIO_AUTH_TOKEN=...
DISCORD_BOT_TOKEN=...
DISCORD_PUBLIC_KEY=...
SLACK_BOT_TOKEN=xoxb-...
OPENAI_API_KEY=sk-...
```

Copy configs from `.env.prod` (with production credentials).

**Note**: Golden files are designed as golden tests - they capture real provider responses once, then replay them for fast CI runs without credentials.

## Troubleshooting

### "database is being accessed by other users"
- Connection cleanup improved - should not occur anymore
- If still occurring, check for long-running background jobs

### "relation telemetry.otel_logs_and_spans does not exist"
- Table is in public schema, not telemetry schema - should be fixed

### Discord signature verification failures
- Verify `DISCORD_PUBLIC_KEY` in `.env` matches test fixtures
- Check timestamp handling in test requests

### Golden file response mismatches
- Run `UPDATE_GOLDEN=true` to regenerate from live APIs
- Compare diffs to understand what changed
- Update test expectations if API behavior changed legitimately
