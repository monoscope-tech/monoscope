# 05 — Log-item detail: show the whole identity (session, user, tenant)

## What it shows today

The detail panel's header (`Pages/LogExplorer/LogItem.hs:332`) renders
`summaryForDetailView (generateSummary item)`. `generateSummary`
(`Telemetry.hs:2431-2433`) contributes exactly three identity elements, and two of them
fight each other:

```haskell
, tag "session"    "right-badge-neutral" <$> atMapText "session.id"  attrsM
, tag "user email" "right-badge-neutral" <$> atMapText "user.email"  attrsM
    <|> tag "user name" "right-badge-neutral" <$> atMapText "user.id" attrsM
, tag "user name"  "right-badge-neutral" <$> (atMapText "user.full_name" attrsM <|> atMapText "user.name" attrsM)
```

Consequences:

- **`user.id` is only ever shown when `user.email` is absent** — and when it *is* shown it
  is mislabelled `user name`. That is the exact confusion reported.
- `user.full_name` and `user.name` collapse into one slot, so whichever loses is invisible.
- `user.hash` and `session.previous.id` are promoted columns but are never surfaced.
- **Tenant has no representation at all.** There is no tenant concept anywhere in the
  schema, and OTel has no tenant convention, so tenant ids arrive as free-form attributes
  (`tenant.id`, `organization.id`, `account.id`, `workspace.id`, …) and end up buried in the
  Attributes tab's JSON tree.

The values *are* all reachable — Attributes tab, Raw tab — which is why this reads as a
presentation bug rather than a data bug. The identity of the request is the first thing an
on-call reader looks for and it should not require opening a JSON tree.

## Design

A dedicated **Identity** block under the header summary, rendered only when at least one
field is present, as a compact two-column definition list (label → value), one row per
present field, **nothing collapsed and nothing dropped**.

Field set, in reading order:

| Group | Keys |
|---|---|
| Session | `session.id`, `session.previous.id` |
| User | `user.id`, `user.email`, `user.name`, `user.full_name`, `user.hash`, `enduser.id`, `enduser.role`, `enduser.scope` |
| Tenant | `tenant.id`, `tenant.name`, `organization.id`, `org.id`, `account.id`, `workspace.id`, `customer.id` |

The tenant list is a convention we are choosing, so it is documented in the field help and
kept in one exported list — the same list the facets use, so a key that is displayable is
also filterable.

Every value is a `spanBadge`, which already carries the filter/copy menu — so "show me
everything from this tenant" is one click from the panel, which is the point of surfacing
them at all.

`generateSummary`'s identity elements shrink to the one-line row identity (email or name,
plus the session button) — the log **list** genuinely cannot afford eight badges per row.
The full set lives in the detail panel where there is room for it. The mislabelled
`user.id`-as-`user name` fallback goes away in both.

## Plan

1. `Telemetry.identityFields :: [(Text, Text)]` — key → label, the single source shared by
   the detail block and the `FGUserSession` facet list (`Log.hs:442-446`, which is currently
   a separate hand-written five-entry list that has already drifted from the schema).
2. Fix the `generateSummary` identity elements: label `user.id` as *user id*, stop making it
   an `<|>` alternative of email.
3. Identity block in `expandedItemView`'s header, driven by `identityFields`.
4. Extend the `FGUserSession` facets from the same list so tenant/enduser keys become
   filterable.
5. Doctest `identityFields` extraction: a span carrying every key yields every row; a span
   carrying none yields no block.
