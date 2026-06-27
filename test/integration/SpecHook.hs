module SpecHook (hook) where

import Test.Hspec (Spec, sequential)

-- hspec-discover applies this hook to the whole discovered spec. Each spec file uses
-- `around withTestResources` (a fresh DB + test clock per example) for per-test
-- isolation; state-sharing files use `aroundAll`.
--
-- We force the whole suite SEQUENTIAL. Global `parallel` was attempted, but once the
-- template-DB setup race was fixed (so example setups stop failing fast) the suite
-- began aborting mid-run — the known hspec `aroundAll`-under-`parallel` interaction —
-- and it couldn't be diagnosed without a working local test run. Sequential keeps the
-- valuable per-test isolation and a clean, deterministic run; re-enabling parallelism
-- is a follow-up that needs local repro to pin the aroundAll/parallel crash.
hook :: Spec -> Spec
hook = sequential
