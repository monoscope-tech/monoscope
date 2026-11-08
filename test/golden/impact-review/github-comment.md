### 🟡 Merge risk: medium

Adds retries to payment capture. A second timeout can charge a customer twice, and the give-up path is silent in production.

| Check | Verdict | What raises the risk |
| :-- | :-- | :-- |
| **Alignment** | ✅ good | — |
| **Impact** | ⚠️ large | Changes how payment captures are retried<br>checkout-api serves every order |
| **Verification** | 👀 decent | No test covers a capture that times out twice |
| **Rollback cost** | ✅ low | — |
| **Error detection** | ⚠️ poor | The new retry path logs nothing when it gives up |

#### Proof · 1 holds · 1 differs · 1 not verified

**✅ Retries a timed-out capture once**

The capture command now retries after the first timeout and succeeds.

<table><tr><td><sub>Before · ddddddd · exit 1</sub><br><a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-base.mp4?sig=s"><img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-base.gif?sig=s" alt="Before · ddddddd · exit 1: Retries a timed-out capture once" width="440"></a></td><td><sub>This PR · aaaaaaa · exit 0</sub><br><a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.mp4?sig=s"><img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.gif?sig=s" alt="This PR · aaaaaaa · exit 0: Retries a timed-out capture once" width="440"></a></td></tr></table>
<sub>Select a recording to watch the video. Asciinema casts: <a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-base.cast?sig=s">Before</a> · <a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.cast?sig=s">This PR</a></sub>

> [!WARNING]
> **Doesn't do what the PR says: Reuses the idempotency key on every retry**
>
> The second retry sent a new key, so the provider saw two charges.

<table><tr><td><sub>Before · ddddddd · exit 1</sub><br><a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-base.mp4?sig=s"><img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-base.gif?sig=s" alt="Before · ddddddd · exit 1: Reuses the idempotency key on every retry" width="440"></a></td><td><sub>This PR · aaaaaaa · exit 0</sub><br><a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.mp4?sig=s"><img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.gif?sig=s" alt="This PR · aaaaaaa · exit 0: Reuses the idempotency key on every retry" width="440"></a></td></tr></table>
<sub>Select a recording to watch the video. Asciinema casts: <a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-base.cast?sig=s">Before</a> · <a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.cast?sig=s">This PR</a></sub>

**👀 Not verified: Shows the retry count in the receipt**

The receipt page needs a browser, which this review cannot record.

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

<sub>Reviewed <a href="https://github.com/acme/checkout/commit/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"><code>aaaaaaa</code></a> against production telemetry from Jan 1 to Jan 8, 2025 (UTC) · <a href="https://app.monoscope.tech/p/x/ai/runs/y">Open the full review</a> · <a href="https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/settings/code-mappings">Review settings</a><br>Comment <code>@monoscope review</code> to review again, or <code>@monoscope prove it</code> to record proof.</sub>