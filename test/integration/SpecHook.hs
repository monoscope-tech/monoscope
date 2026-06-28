module SpecHook (hook) where

import Test.Hspec (Spec, sequential)

-- hspec-discover applies this hook to the whole discovered spec. Each spec file uses
-- `around withTestResources` (a fresh DB + test clock per example) for per-test
-- isolation; state-sharing files use `aroundAll` and wrap their whole `spec` in
-- `sequential $ …`.
--
-- We force the whole suite SEQUENTIAL. `parallel` is the goal and the per-test
-- specs are isolated for it, but flipping this hook to `parallel` reproducibly
-- DEADLOCKS mid-run: several examples each set up a test DB then sit idle (0
-- active queries, 0 lock waits, ~0% CPU) and the suite never finishes — the known
-- hspec `aroundAll`-under-`parallel` interaction. Pinning that needs dedicated
-- root-cause work (likely making the aroundAll specs not share state so they can
-- use per-test `around` like the rest, or isolating the shared resource that
-- blocks across parallel workers), not a one-line hook flip.
--
-- DO NOT strip the `sequential $` wrappers in the spec files (GitSyncSpec,
-- MonitoringSpec, DashboardsSpec, ErrorPatternsSpec, …): they're no-ops under this
-- `sequential` hook but encode which specs genuinely share state across examples,
-- and they do real work the moment this hook is switched back to `parallel`.
hook :: Spec -> Spec
hook = sequential
