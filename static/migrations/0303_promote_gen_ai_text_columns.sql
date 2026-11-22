-- The Postgres half of TimeFusion's 2026-11-22 widening: the GenAI text fields the LLM pages aggregate
-- (Pages.LLM.selectUnits), so a cold hour skips the attributes blob. Nullable, so no rewrite.
ALTER TABLE otel_logs_and_spans
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___input___preview TEXT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___tool___call___arguments TEXT,
  ADD COLUMN IF NOT EXISTS attributes___gen_ai___response___finish_reasons TEXT;
