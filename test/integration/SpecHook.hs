module SpecHook (hook) where

import Test.Hspec (Spec, parallel)

-- hspec-discover applies this hook to the whole discovered spec. Each spec file now
-- uses `around withTestResources` (a fresh DB + test clock per example), so examples
-- are hermetic and safe to run concurrently. `parallel` then overlaps them across the
-- whole suite. Files that genuinely share state across examples (e.g. a `beforeAll`)
-- either convert to `around` for per-test isolation, or keep `aroundAll` and opt out
-- of parallelism locally with `sequential` (see GitSyncSpec, MonitoringSpec, …).
hook :: Spec -> Spec
hook = parallel
