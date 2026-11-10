### ⚠️ Reuses the idempotency key on every retry

The second retry sent a new key, so the provider saw two charges.

![Recording](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.gif?sig=s)

[Watch full video](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.mp4?sig=s)

*Merge-base · exit 1*

![Merge-base](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-base.gif?sig=s)

**Also checked** · 1 match · 1 not verified
✅ Retries a timed-out capture once · [Watch recording](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.mp4?sig=s)
👀 Shows the retry count in the receipt

#### 🟡 Merge risk: medium

Adds retries to payment capture. A second timeout can charge a customer twice, and the give-up path is silent in production.

| Check | Verdict | What raises the risk |
| :-- | :-- | :-- |
| **Alignment** | ✅ good | — |
| **Impact** | ⚠️ large | Changes how payment captures are retried; checkout-api serves every order |
| **Verification** | 👀 decent | No test covers a capture that times out twice |
| **Rollback cost** | ✅ low | — |
| **Error detection** | ⚠️ poor | The new retry path logs nothing when it gives up |

#### Worth checking in production (2)

> **[`src/payments/capture.ts:88`](https://github.com/acme/checkout/pull/42/files#diff-5badcd3e5097afa5539d90a33dfe2c3d239f41513fa07ece41696a1a74ab438bR88)**
> A retried capture reuses the same idempotency key only on the first retry; a second timeout sends a fresh key, so a slow provider can charge twice.
>
> **Next check:** Count captures with two or more retries in the last week, then add a test for a double timeout.
>
> [checkout-api in production](https://app.monoscope.tech/p/x/log_explorer): 120.3K events and 312 errors, Jan 1 to Jan 8, 2025

> **[`src/payments/capture.ts:120`](https://github.com/acme/checkout/pull/42/files#diff-5badcd3e5097afa5539d90a33dfe2c3d239f41513fa07ece41696a1a74ab438bR120)**
> The give-up branch swallows the provider error, so a failed capture leaves no log or span error.
>
> **Next check:** Log the provider error and mark the span as an error before returning.
>
> [checkout-api in production](https://app.monoscope.tech/p/x/log_explorer): 120.3K events and 312 errors, Jan 1 to Jan 8, 2025

**What this review could not verify**

- No load test covers the retry backoff.
- Provider-side duplicates are not visible in this project's telemetry.


Reviewed [`aaaaaaa`](https://github.com/acme/checkout/commit/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) with production telemetry from Jan 1 to Jan 8, 2025 · [Review settings](https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/settings/code-mappings)