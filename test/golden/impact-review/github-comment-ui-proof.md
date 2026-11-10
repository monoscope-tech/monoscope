### ✅ Shows the retry count in the receipt

The receipt shows Retried once under the charge; before, it showed only the amount.

<a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.mp4?sig=s"><img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.gif?sig=s" alt="Recording" width="100%"></a>

[Watch full video](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.mp4?sig=s) · [0:00 Open the receipt](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.mp4?sig=s#t=0) · [0:06 Retry the charge](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.mp4?sig=s#t=6.4) · [0:14 Read the retry count](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.mp4?sig=s#t=14.2)

<sub>Merge-base</sub>

<img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-base-1.png?sig=s" alt="Receipt without retries" width="100%">

<sub>This PR <code>aaaaaaaa</code></sub>

<img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head-1.png?sig=s" alt="Receipt with retry count" width="100%">

<img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head-2.png?sig=s" alt="Retry history" width="100%">

**Also checked** · 1 match<br>✅ Retries a timed-out capture once · [Watch recording](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.mp4?sig=s)

#### 🟡 Merge risk: medium

Adds retries to payment capture. A second timeout can charge a customer twice, and the give-up path is silent in production.

| Check | Verdict | What raises the risk |
| :-- | :-- | :-- |
| **Alignment** | ✅ good | — |
| **Impact** | ⚠️ large | Changes how payment captures are retried<br>checkout-api serves every order |
| **Verification** | 👀 decent | No test covers a capture that times out twice |
| **Rollback cost** | ✅ low | — |
| **Error detection** | ⚠️ poor | The new retry path logs nothing when it gives up |

#### Worth checking in production (2)

> [!WARNING]
> **[`src/payments/capture.ts:88`](https://github.com/acme/checkout/pull/42/files#diff-5badcd3e5097afa5539d90a33dfe2c3d239f41513fa07ece41696a1a74ab438bR88)**
> A retried capture reuses the same idempotency key only on the first retry; a second timeout sends a fresh key, so a slow provider can charge twice.
>
> **Next check:** Count captures with two or more retries in the last week, then add a test for a double timeout.
>
> [checkout-api in production](https://app.monoscope.tech/p/x/log_explorer): 120.3K events and 312 errors, Jan 1 to Jan 8, 2025

> [!WARNING]
> **[`src/payments/capture.ts:120`](https://github.com/acme/checkout/pull/42/files#diff-5badcd3e5097afa5539d90a33dfe2c3d239f41513fa07ece41696a1a74ab438bR120)**
> The give-up branch swallows the provider error, so a failed capture leaves no log or span error.
>
> **Next check:** Log the provider error and mark the span as an error before returning.
>
> [checkout-api in production](https://app.monoscope.tech/p/x/log_explorer): 120.3K events and 312 errors, Jan 1 to Jan 8, 2025

![Observed service events](https://app.monoscope.tech/p/x/widget.png?widgetZ=fixture)

<details><summary>What this review could not verify (2)</summary>

- No load test covers the retry backoff.
- Provider-side duplicates are not visible in this project's telemetry.

</details>

<sub>Reviewed [`aaaaaaa`](https://github.com/acme/checkout/commit/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) with production telemetry from Jan 1 to Jan 8, 2025 · [Open the full review](https://app.monoscope.tech/p/x/ai/runs/y) · [Review settings](https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/settings/code-mappings) · Comment `@monoscope prove it` to prove again</sub>