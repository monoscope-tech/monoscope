### ⚠️ Reuses the idempotency key on every retry

The second retry sent a new key, so the provider saw two charges.

<a href="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.mp4?sig=s"><img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.gif?sig=s" alt="Recording" width="100%"></a>

[Watch full video](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-head.mp4?sig=s)

<sub>Merge-base · exit 1</sub>

<img src="https://app.monoscope.tech/p/x/reviews/r/proofs/proof-2-base.gif?sig=s" alt="Merge-base" width="100%">

**Also checked** · 1 match · 1 not verified<br>✅ Retries a timed-out capture once · [Watch recording](https://app.monoscope.tech/p/x/reviews/r/proofs/proof-1-head.mp4?sig=s)<br>👀 Shows the retry count in the receipt

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
> The reasoning and production evidence are in the [full review](https://app.monoscope.tech/p/x/ai/runs/y).

> [!WARNING]
> **[`src/payments/capture.ts:120`](https://github.com/acme/checkout/pull/42/files#diff-5badcd3e5097afa5539d90a33dfe2c3d239f41513fa07ece41696a1a74ab438bR120)**
> The reasoning and production evidence are in the [full review](https://app.monoscope.tech/p/x/ai/runs/y).

<sub>Reviewed [`aaaaaaa`](https://github.com/acme/checkout/commit/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) with production telemetry from Jan 1 to Jan 8, 2025 · [Open the full review](https://app.monoscope.tech/p/x/ai/runs/y) · [Review settings](https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/settings/code-mappings) · Comment `@monoscope prove it` to prove again</sub>