# Review interpreter contract

Date: 1 November 2026.
This record covers the review interpreter and admission rationale boundary.
It does not establish whole-branch signoff.

## Defects and changes

The fake repository-review interpreter ignored its caller's result validator.
A rejected draft therefore returned success in tests.
The fake admission interpreter also accepted an empty rationale that production rejected.
Both regressions failed before the fix.
The saved log is `scripts/local/review-interpreter-validation-red.log`.

Repository reviews now decode the provider response and call the original validator.
Repository, source, and patch reviews share one local helper for response parsing and validation.
Provider failures return before the validator runs.
Malformed structured responses also return before the validator runs.
Successful responses preserve the original provider text.

Admission decisions now carry the existing opaque `MemoryText 1200` type.
Its constructor rejects blank, oversized, and NUL-containing text.
The production interpreter no longer repeats the same rationale checks.
Application callers unwrap the bounded value after admission.
The JSON shape remains a tagged decision with a plain string in `contents`.
Provider fixtures retain explicit JSON objects, independent of the new encoder.

## Review loop

The scoped `hs-distill` pass replaced repeated response handling with the shared local helper.
The second pass found no further material reduction in these changes.
The extended regression covers repository reviews, patch reviews, and both source-review modes in one table.
Pure admission boundary examples remain beside the type as Haddock doctests.

The scoped `hs-evasion-review` pass checked all admission consumers and the bounded type implementation.
The bounded type hides its constructor and uses a nominal limit parameter.
The changes add no warning suppression, runtime IO seam, dependency, or weaker application effect row.
The helper uses IO only inside the existing effect interpreter and preserves the original callback through its unlift.

The scoped `hs-lob-review` pass found no client behavior in this change.
The new helper remains local to its only interpreter caller.
The existing top-level `mapSession` serves four effect branches through the shared production interpreter.
Moving that helper adds no implementation reduction, so this pass retains it.

## Verification

The repository-pinned formatter accepts all four changed Haskell files.
HLint reports no hints in the three changed runtime modules.
HLint reports seven suggestions on unchanged test lines.
Those suggestions remain outside this fix.

All six selected review and recovery cases passed in the shared watcher.
They cover validator callbacks, real admission, retained child sessions, and both application-job lifecycles.
The combined run also included three concurrent Slack workflow cases.
One Slack workflow case failed, so the combined result is **9 examples, 1 failure**.
This record does not describe that combined run as green.
Its saved log is `scripts/local/review-interpreter-lifecycle-mixed.log`.
The four source hashes are in `scripts/local/review-interpreter-source.json`.

The supported `make test-doctests` run passes with 2,076 examples tried and no errors or failures.
The previous checkpoint contained 2,068 examples.
The saved log is `scripts/local/review-interpreter-doctests.log`.
The four source hashes remain unchanged after this run.
The full native review group remains in progress.

## Remaining boundaries

The latest fetched `origin/master`, `3c34f3c83`, is an ancestor of this branch.
Whole-branch reviews, remaining live acceptance, final local signoff, and the PR update remain open.
No production application image build or deployment started during this fix.
Production image generation must use the local OrbStack builder on this Mac.
