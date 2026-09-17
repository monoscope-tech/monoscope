# Activate metered billing at a subscription boundary

Use this runbook only after the subscription owner authorizes charging for metric
datapoints and session replays. The application already sends usage for the two
Stripe meters below. A meter with no attached subscription price records usage
but does not bill it.

Do not attach a price in the middle of a billing period unless the owner has
explicitly accepted the first invoice including usage already accrued in that
period. Do not backfill the dormant period.

## Scope and rate card

| Dimension | Stripe meter event | Rate | Application evidence |
| --- | --- | --- | --- |
| Events | `events_usage` | $1 per 1,000,000 events | Existing subscribed overage |
| Metric datapoints | `metrics_usage` | $1 per 10,000,000 datapoints | `MeterKind MetricDatapoints` |
| Session replays | `session_replays_usage` | $1 per 1,000 replays | `MeterKind SessionReplays` |

The source of truth for meter names and quantities is
`Models.Projects.Projects.stripeMeterEventName` and `meterQuantity`. Do not
create replacement meters with similar names.

## Preconditions

1. Record the owner approval, planned first charge date, affected subscription
   IDs, and the two approved Stripe price IDs in the change record.
2. Confirm every affected subscription's current-period end. Schedule the
   change at that boundary, not merely on the same calendar day.
3. Confirm each subscription still has its existing base and `events_usage`
   items. This change adds the approved metric and replay prices; it does not
   replace or edit the events item.
4. Check pending rows in `projects.usage_report_submissions`. Keep their
   meter kind, window, quantity, and status with the change record. They are
   evidence for a later invoice reconciliation, not a backlog to submit.
5. Freeze pricing-copy deployment until the provider change is scheduled. The
   product must not advertise an overage that is not billable yet.

## Provider change

At each recorded period boundary, use the Stripe Dashboard or an approved
operator tool to add exactly these recurring metered prices to the existing
subscription:

| Add | Must meter | Must not change |
| --- | --- | --- |
| Approved metric-datapoints price | `metrics_usage` | Base item and `events_usage` item |
| Approved session-replays price | `session_replays_usage` | Base item and `events_usage` item |

Record the resulting Stripe subscription-item IDs, price IDs, effective time,
operator, and before/after item lists. Do not delete a live price to undo a
mistake: remove only the newly added subscription item at the next permitted
boundary, then investigate the invoice impact with Finance.

## Verification and reconciliation

1. After the next usage-report cycle, select one low-volume Stripe project and
   verify that Stripe has an event for each attached meter. Compare its values
   with the matching `projects.usage_report_submissions` rows by `meter_kind`
   and billing window.
2. At invoice finalization, compare the invoice quantities and prices with the
   saved submission rows. Record any provider delay, retry, or discrepancy;
   never resend a submitted row to make a dashboard look current.
3. Deploy the pricing-copy update only after the first provider-side attachment
   is confirmed. The app, onboarding, and landing-site copy must name the same
   three dimensions and rates in this runbook.
4. Keep LemonSqueezy reconciliation separate. Its per-meter subscription-item
   IDs live in `projects.billing_meter_items`; a stale LemonSqueezy credential
   must not be used as evidence that Stripe metering failed.

## Completion record

Attach to the change record:

- owner approval and boundary timestamps;
- subscription, price, and new subscription-item IDs;
- before/after subscription item lists;
- one successful event-to-submission comparison per newly attached meter;
- first invoice comparison; and
- the pricing-copy deploy revision.

Without all six, metered billing is not complete.
