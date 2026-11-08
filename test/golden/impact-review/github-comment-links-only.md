### 🟡 Merge risk: medium

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

<sub>Reviewed <a href="https://github.com/acme/checkout/commit/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"><code>aaaaaaa</code></a> against production telemetry from Jan 1 to Jan 8, 2025 (UTC) · <a href="https://app.monoscope.tech/p/x/ai/runs/y">Open the full review</a> · <a href="https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/settings/code-mappings">Review settings</a><br>Comment <code>@monoscope review</code> to review again, or <code>@monoscope prove it</code> to record proof.</sub>