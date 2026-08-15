# Log Explorer facet loading plan

## Goal

Show the Common facets as part of the initial Log Explorer page. Keep the
collapsed facet groups lazy so they do not add work to page load.

For the reported project, Common facets should no longer wait for HTMX to load,
initialize, observe the sidebar, make another request, and swap the response.

## What is slow today

The page initially renders a facet skeleton. This element later starts the
facet request:

```haskell
hxGet_ $ "/p/" <> page.pid.toText <> "/log_explorer/facets"
hxTrigger_ "intersect once"
```

That makes an above-the-fold control depend on client-side lazy loading. In the
reported session, the document took about 1.2 seconds and the page settled in
about 4.3 seconds. Several long browser frames occurred while the page was
settling.

Facet values are already precomputed. The current read is a primary-key lookup:

```sql
SELECT doc
FROM apis.schema_summary
WHERE project_id = $1;
```

`project_id` is the table's primary key, so the serving query already has the
right index. The former `apis.facet_summaries` table was dropped in migration
0090. This change does not need a new table, another index, or an application
cache.

## Implementation

### 1. Confirm the existing lookup is cheap

Before changing the page, time one production request to the facet endpoint and
run `EXPLAIN (ANALYZE, BUFFERS)` for the query above.

Expected result: an index scan using the `schema_summary` primary key and a
small database execution time. Record the endpoint duration and response size
for the reported project so the final result can be compared against it.

If the lookup is unexpectedly slow, stop and diagnose that result. Do not add a
cache speculatively.

### 2. Render Common facets in the initial page

`Pages.BodyWrapper.mkPageCtx` already reads the project's summary to build the
environment selector. Make that summary available to the Log Explorer page
renderer instead of reading it again.

In `Pages/LogExplorer/Log.hs`:

- replace the initial facet skeleton with `renderFacetGroup True FGCommon`;
- render the existing missing-summary state if no summary is available;
- preserve the current facet markup and checkbox/query behavior;
- remove the `intersect once` request for Common facets;
- do not add another `getSummary` query to the page request.

The initial page should include only the Common values, not every facet value.

### 3. Keep other groups lazy

Continue rendering HTTP, Severity, Resource, User & Session, Database, and
Errors as collapsed group shells.

On the first expansion, keep using:

```text
/p/:project/log_explorer/facets?group=<group>
```

Each group should:

- load only when first opened;
- replace only its own loading body;
- remain in the DOM after it has loaded;
- not issue another request when reopened;
- keep its accessible disclosure and loading behavior.

The existing field-level “show more” request can remain unchanged.

### 4. Verify the result

Test the reported project before and after the change.

Confirm that:

- Common facets are present in the initial HTML;
- they appear with the page instead of several seconds later;
- the page performs no follow-up request for Common facets;
- collapsed groups perform no requests until opened;
- each opened group loads only once;
- page response size and server time do not regress materially;
- expanding a group does not create a browser task longer than 50 ms;
- facet checkboxes still update and follow the query correctly.

## Tests

Add focused coverage to `test/integration/FacetsSpec.hs` and the Log Explorer
page tests:

1. The initial page contains Common facet values.
2. The initial page does not contain values from collapsed groups.
3. The group endpoint renders only the requested group.
4. The page keeps the existing empty/missing-summary state.
5. Checkbox/query synchronization still works after a group is loaded.

Run the existing facet and Log Explorer tests. No migration or backfill tests
are needed because this plan does not change persistence.

## Not in scope

- an in-memory facet cache;
- a new facet-summary table or column;
- new PostgreSQL indexes;
- dual writes, backfills, or feature flags;
- pagination, virtualization, or client-side chunked rendering;
- redesigning the facet sidebar.

Only revisit storage or caching if the before/after measurement shows that the
primary-key summary read or JSON decoding remains a meaningful part of the
visible delay after Common facets are rendered with the page.

## Done when

Common facets arrive in the initial Log Explorer response, collapsed groups
still load on demand, existing behavior passes its tests, and the reported page
no longer shows a delayed facet skeleton.
