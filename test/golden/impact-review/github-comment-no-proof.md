### 🟡 Merge risk: medium

Adds a Railway deploy button to the README and drops the obsolete Compose version field.

**No proof needed** · Only the README and docker-compose.yml change; the diff shows both.

| Check | Verdict | What raises the risk |
| :-- | :-- | :-- |
| **Alignment** | ✅ good | — |
| **Impact** | ⚠️ large | Changes how payment captures are retried<br>checkout-api serves every order |
| **Verification** | 👀 decent | No test covers a capture that times out twice |
| **Rollback cost** | ✅ low | — |
| **Error detection** | ⚠️ poor | The new retry path logs nothing when it gives up |

<details><summary>What this review could not verify (2)</summary>

- No load test covers the retry backoff.
- Provider-side duplicates are not visible in this project's telemetry.

</details>

<sub>Reviewed [`aaaaaaa`](https://github.com/acme/checkout/commit/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) with production telemetry from Jan 1 to Jan 8, 2025 · [Open the full review](https://app.monoscope.tech/p/x/ai/runs/y) · [Review settings](https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/settings/code-mappings) · Comment `@monoscope prove it` to prove again</sub>