module SpecHook (hook) where

import Test.Hspec (Spec, sequential)

-- hspec-discover applies this hook to the whole discovered spec. Each spec file uses
-- `around withTestResources` (a fresh DB + test clock per example) for per-test
-- isolation; state-sharing files use `aroundAll` and wrap their whole `spec` in
-- `sequential $ …`.
--
-- The suite stays SEQUENTIAL in-process. Flipping this hook to `parallel` reproducibly
-- DEADLOCKS mid-run (examples set up a test DB then sit idle — 0 active queries, 0 lock
-- waits, ~0% CPU — and it never finishes). Root-caused via a green-thread dump to the
-- per-example resource-pool lifecycle: `withTestResources` builds a `Data.Pool` whose
-- background collector MVar gets orphaned when hspec tears down `around` resources
-- concurrently, after which every `withResource` blocks on it forever. Fixing in-process
-- `parallel` needs that lifecycle de-shared (e.g. one process-wide pool, no per-example
-- collector), not a one-line hook flip.
--
-- Parallelism is instead done at the PROCESS level (see test/integration/Main.hs +
-- `make test-shards` / the CI workflows): N processes, each its own RTS running a
-- disjoint shard SEQUENTIALLY, so there's no shared pool lifecycle to race.
--
-- DO NOT strip the `sequential $` wrappers in the spec files (GitSyncSpec,
-- MonitoringSpec, DashboardsSpec, ErrorPatternsSpec, …): they encode which specs share
-- state across examples, so process shards keep each such spec whole on one shard.
hook :: Spec -> Spec
hook = sequential
