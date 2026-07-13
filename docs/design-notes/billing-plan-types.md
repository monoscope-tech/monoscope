# Design note: type-driven billing/subscription refactor

_Scoped 2026-07-12 from a two-agent map of the billing model + blast radius. Recommendation: do the **bounded** version (A + B below, ~3–4d, reviewed PR with CI green). Do NOT do the naive "change `paymentPlan` to an enum" version._

## Current model (source of truth)

`Project` record, `src/Models/Projects/Projects.hs:242-267`:
- `paymentPlan :: Text` (252) — **open-valued** plan name, NOT an enum. DB `payment_plan TEXT NOT NULL DEFAULT 'Free'`.
- `subId`/`firstSubItemId`/`orderId`/`customerId :: Maybe Text` (257-266), `billingDay :: Maybe UTCTime` (261), `usageLastReported :: UTCTime` (260).
- No stored trial/cancelled/paused flag. Duplicated on `ProjectListItem` (281), `CreateProject` (351); `ProjectCache` (329) carries `paymentPlan` only.
- Provider is **guessed** from `subId` shape: `billingProvider :: Maybe Text -> BillingProvider` (`:950`) — `"sub_"` prefix → Stripe, all-digits → LemonSqueezy, else NoBilling.
- Gating choke points already exist: `isFreeTier` (`:928`), `isOnboarding` (`:934`) — both lower-case before comparing.

## Two constraints that kill the naive refactor

1. **`paymentPlan` is open-valued.** LemonSqueezy webhooks write the LS product name verbatim (`Settings.hs:1048/1089`), so paid-plan strings are unbounded. A closed `Plan` enum is impossible; the paid dimension must keep a `Text` payload.
2. **The DB collapses states.** Persisted truth is 3-way: `ONBOARDING` | `Free` | `<paid name>`. Paused/cancelled/expired all become `Free` with ids preserved (`Settings.hs:1099/1588/1637`); trialing/past-due are never stored — fetched live from Stripe (`StripeSubDetails`), dunning is notify-only. A sum type *on the record* can't represent trial/cancelled/paused; that data isn't there.

→ The win is NOT changing the column. It's (A) storing the provider instead of guessing, and (B) a typed `Plan` *view* over the unchanged column.

## Recommended target

### A. Store the provider (clearest real bug)
- `data BillingProvider = Stripe | LemonSqueezy | NoBilling` via `WrappedEnumSC` (type already exists; add DB/JSON instances, byte-canonical strings).
- New nullable `billing_provider TEXT` column; migration backfills from the existing shape logic (same `sub_%` / all-digits check as migration `0071`).
- Webhooks write the KNOWN provider (each handler knows which one fired): LS `webhookPostH` (`Settings.hs:1029`), Stripe `stripeWebhookPostH` (`:1508`).
- Reads use the stored column; keep shape-based `billingProvider` as backfill fallback only.
- Reads to swap: `BackgroundJobs.hs:458/510/544`, `Settings.hs:1184/1558`, `Pages/Projects.hs:1144`.
- Blast radius: ~6 writes + ~5 reads. Encoding-preserving, ghcid-verifiable.

### B. Typed `Plan` view over the unchanged column (parse, don't validate)
```haskell
newtype PaidPlanName = PaidPlanName Text
data Plan = Onboarding | Free | Paid PaidPlanName
plan :: Project -> Plan   -- single parse point; folds isFreeTier/isOnboarding (preserve lowercasing!)
```
- Storage stays `paymentPlan :: Text`. `Plan` is a derived view; gating pattern-matches exhaustively.
- Replaces scattered string gates with one total function: `Utils.hs:585` (`checkFreeTierStatus`), `ProcessMessage.hs:175`, `OtlpServer.hs:1005/1111/1313/1670`, `Pages/{Reports,Monitors,Endpoints,Anomalies,Dashboards,LogExplorer/Log}.hs`, `BodyWrapper.hs:718`.
- `ProjectCache` keeps decoding `Text`; add a `plan` accessor derived on read (NO cache wire-format change → no invalidation).
- Blast radius: ~20 read/gating sites → `case plan p of …`. `isFreeTier`/`isOnboarding` eventually deletable. The 99 *write* sites are untouched because fields stay `Text`.

### C. (Optional) Normalize empty-string Maybes
`subId`/`firstSubItemId`/`orderId` treat `Just ""` as absent (guards like `first_sub_item_id IS NULL OR = ''`, `Projects.hs:538`). Normalize to `Nothing` at parse. Only if already in the file.

## What NOT to do
- Change `paymentPlan` to an enum column — blocked by open-valued LS names; lossy + risky data migration on the revenue path for no gating benefit the `Plan` view doesn't already give.
- Model trial/cancelled/paused on the record — not persisted; would be fiction. Leave provider-live + notify-only as-is.

## Phasing (~3–4d, reviewed PR, CI green REQUIRED)
1. `BillingProvider` type + instances + column + backfill migration (~1d).
2. Write provider in both webhooks; swap the ~5 reads (~0.5d) — verify `BusinessFlowsSpec`.
3. `Plan` view + `PaidPlanName`; route ~20 gating sites (~1.5d).
4. (optional) empty-string `Maybe` normalization (~0.5d).

## Risks
- **Encoding preservation** — `BillingProvider` field round-trips backfilled column exactly; `Plan` view must reproduce `isFreeTier`'s lowercasing (`Projects.hs:929`) or legacy `"FREE"`/paused rows mis-gate.
- **Cache hot path** — `ProjectCache` decode stays `Text`; derive `Plan` on read.
- **Revenue path** — the only billing test (`test/integration/Pages/BusinessFlowsSpec.hs`) runs in CI only (`live-test-dev` GHCi linker bug). Reviewed PR, not autonomous.
- **Three duplicated records** move in lockstep — the `Plan`-view approach avoids this (fields stay `Text`).

## Blast-radius reference (by file, all billing fields)
Settings.hs 79 · Projects.hs 40 · Pages/Projects.hs 33 · BackgroundJobs.hs 30 · Dashboards.hs 14 · Components.hs 8 · BusinessFlowsSpec 7 · Onboarding 6 · Monitors 5 · Reports/OtlpServer 4 · Utils/Mail/Log/Endpoints 2 · ProcessMessage/ApiTypes/ApiHandlers/BodyWrapper/Anomalies 1. ~95–105 distinct logical sites total — but A+B touch only ~30 of them because storage stays Text.
