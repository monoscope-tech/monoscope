module SpecHook (hook) where

import Test.Hspec (Spec, parallel)

-- hspec-discover applies this hook to the whole discovered spec. Each spec file now
-- uses `around withTestResources` (a fresh DB + test clock per example), so examples
-- are hermetic and safe to run concurrently. `parallel` then overlaps them across the
-- whole suite. Files that genuinely share state across examples (e.g. a `beforeAll`)
-- opt out locally with `sequential`.
hook :: Spec -> Spec
hook = parallel
