# Native guest command timeout bounds

Date: 1 November 2026.
This scoped review covers the SDK bash tool and native guest command decoding.
Whole-branch review and release signoff remain open.

## Defect and change

The custom SDK bash adapter accepted negative timeout values.
It bypassed the default SDK process executor, where the SDK performs its own timeout validation.
The native adapter then converted a Double to Word32 at execution time.
Invalid or oversized values could wrap or fail during conversion.
An absent per-command timeout remains permitted under the existing session deadline.

The failing test drives the SDK's real runToolCall path through hostTools.
A timeout of -1 reached the guest operation instead of returning a tool error.
The saved failure is `scripts/local/guest-timeout-sdk-red.log`.

The SDK tool schema now requires a positive timeout within the native Word32 range.
Rejected arguments produce a normal SDK tool error without forwarding a guest operation.
The next valid call remains usable.
The schema extends the existing definition instead of replacing the SDK execution machinery.

Native decoding now produces the opaque CommandTimeout type.
Its decoder compares the exact JSON number against the range before conversion.
It rounds valid fractional seconds upward, preserving the supervisor's existing behavior.
The constructor is hidden, and the type has no Generic, Read, or numeric instance.
Execution consumes the checked Word32 directly.

## Repeated reviews

The hs-distill pass reuses the SDK schema validator and derives the numeric JSON encoder.
It removes the runtime floating-point conversion from command execution.
No compatible bounded-seconds type exists in the shared package.
The second pass finds no further material reduction in this scope.

The hs-evasion-review pass follows the SDK override, actual tool-call validation,
native JSON decoder, Exec field, supervisor arguments, and CLI session entry point.
The checked type prevents invalid Haskell values outside its module.
The client and native decoder each parse their own transport boundary once.
Neither changes command authority, the unprivileged guest account, or the outer session deadline.
The change adds no dependency, effect escape, warning suppression, or constructor bypass.
The second pass finds no remaining material issue in these changed functions.

The hs-lob-review pass finds no browser behavior in this change.
The JavaScript is the existing native process host, not a client interaction layer.
The pure type examples stay in Haddock; the SDK regression exercises tool dispatch and recovery.
A second pass finds no locality issue introduced here.

## Verification

The focused SDK test fails before the fix and passes afterward.
It rejects negative, zero, overflowing, NaN and infinite values without a guest operation.
It then accepts absent, fractional, ordinary and maximum timeout values.
The full worker suite passes **20 tests, 0 failures** in 1.9086 seconds.
Logs are `scripts/local/guest-timeout-sdk-green.log` and `guest-timeout-sdk-full.log`.
Installed Pi packages are version 1.4.0, and TypeBox is 1.3.27.
All four versions match package-lock.json.

The supported make cli-build succeeds with the changed shared package.
The required shared-package watcher reload happens once per root watcher.
The existing native SDK exploration case then passes in **34.7481 seconds**.
It covers real guest execution, timeout signalling, retained workspace edits,
child cleanup, output bounds, skills, memory, and cancellation.
The saved log is `scripts/local/guest-timeout-native-exploration-green.log`.
This remains a local SDK and VM checkpoint, not Linux/KVM or arbitrary host-loss acceptance.

The supported make test-doctests passes **2,095 tried, 0 errors, 0 failures**.
The prior count was 2,089, so all six new examples ran.
They cover range endpoints, numeric JSON round-trip, fractional rounding,
zero, negative, overflow, a huge exponent, and a nonnumeric input.
The log is `scripts/local/guest-timeout-doctests.log`.
The pinned formatter accepts the shared module; HLint reports no hints there.
Node syntax and whitespace checks also pass.
All three recorded timeout input hashes remain unchanged through these checks.

The freshly built Mac CLI also passes package-cli.sh.
This includes portable-linkage checking, signing, signature verification and runtime version.
The packaged binary also exits successfully for help.
The hashes and result are `scripts/local/guest-timeout-cli-package.json`.
This replaces the earlier cached-binary-only packaging evidence.
It does not establish a Linux release or generate an application image.
