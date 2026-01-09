# Refactor: Move Placeholder Expansion to Query Execution Time

## Problem Statement

Currently, we have two fields on the Widget type:
- `query` / `sql`: Contains expanded queries where `{{const-...}}` placeholders are replaced with actual values
- `rawQuery` / `rawSql`: Contains original queries with placeholders intact for Monaco display

This duplication exists because:
1. The backend pre-expands placeholders during `processWidget` (page render time)
2. The frontend sends the expanded query to `/chart_data` for execution
3. Monaco needs the original query to show users the template structure

This is suboptimal because:
- Two fields for essentially the same data
- Easy to use the wrong field in the wrong context
- Adds complexity to Widget type and processing logic

## Proposed Solution

Move placeholder expansion from `processWidget` (page render time) to `/chart_data` (query execution time).

### Benefits
1. Single `query` field containing the original template
2. Remove `rawQuery` / `rawSql` fields entirely
3. Clearer separation: Widget stores template, execution expands it
4. Monaco automatically shows the right thing (the only `query` field)

### Trade-offs
1. Constants must be passed to `/chart_data` endpoint
2. Slightly more data in chart data requests
3. Need to ensure constant values are available at execution time

## Current Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ Page Load (Dashboards.hs)                                       │
├─────────────────────────────────────────────────────────────────┤
│ 1. Load dashboard YAML                                          │
│ 2. processConstantsAndExtendParams() - execute constant queries │
│ 3. processWidget() - expand {{const-...}} into query field      │
│    - rawQuery = original                                        │
│    - query = expanded                                           │
│ 4. Render page with widget (sends expanded query to frontend)   │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Frontend (widgets.ts)                                           │
├─────────────────────────────────────────────────────────────────┤
│ 1. Receive expanded query from backend                          │
│ 2. Call /chart_data?query=<expanded>&...                        │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Chart Data Endpoint (Charts.hs)                                 │
├─────────────────────────────────────────────────────────────────┤
│ 1. Receive already-expanded query                               │
│ 2. Parse and execute query                                      │
└─────────────────────────────────────────────────────────────────┘
```

## Proposed Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ Page Load (Dashboards.hs)                                       │
├─────────────────────────────────────────────────────────────────┤
│ 1. Load dashboard YAML                                          │
│ 2. processConstantsAndExtendParams() - execute constant queries │
│ 3. processWidget() - NO expansion, keep original query          │
│ 4. Render page with:                                            │
│    - Original query (with {{const-...}} placeholders)           │
│    - Constant values as data attributes or script vars          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Frontend (widgets.ts)                                           │
├─────────────────────────────────────────────────────────────────┤
│ 1. Receive original query + constant values                     │
│ 2. Call /chart_data?query=<original>&const-X=<value>&...        │
│    OR pass constants in request body                            │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ Chart Data Endpoint (Charts.hs)                                 │
├─────────────────────────────────────────────────────────────────┤
│ 1. Receive original query + constant values                     │
│ 2. Expand {{const-...}} placeholders using provided values      │
│ 3. Parse and execute expanded query                             │
└─────────────────────────────────────────────────────────────────┘
```

## Implementation Plan

### Phase 1: Pass Constants to Frontend

**File: `src/Pages/Dashboards.hs`**

1. In `dashboardPage_`, add a script tag with constant values:
   ```haskell
   script_ $ "window.dashboardConstants = " <> constantsJSON <> ";"
   ```
   Where `constantsJSON` is a JSON object like:
   ```json
   {
     "const-top_resources": "(\"api/users\", \"api/orders\")",
     "const-top_resources-kql": "(\"api/users\", \"api/orders\")"
   }
   ```

2. Modify `processWidget` to NOT expand placeholders:
   ```haskell
   -- REMOVE these lines:
   -- & #sql . _Just %~ replacePlaceholdersSQL
   -- & #query %~ fmap replacePlaceholdersKQL
   ```

3. Keep processing other placeholders like `{{project_id}}`, `{{time_filter}}` etc.
   Only skip `{{const-...}}` placeholders.

**File: `src/Pkg/Components/Widget.hs`**

1. Remove `rawQuery` and `rawSql` fields from Widget type
2. Remove references to these fields in `renderChart`
3. Add `constants` field or pass constants separately

### Phase 2: Update Frontend to Pass Constants

**File: `web-components/src/widgets.ts`**

1. In `updateChartData`, include constants in the request:
   ```typescript
   // Get constants from global variable set by backend
   const constants = (window as any).dashboardConstants || {};

   // Add each constant to params
   Object.entries(constants).forEach(([key, value]) => {
     params.set(key, value as string);
   });
   ```

2. Update `WidGetData` type - no changes needed since we're using URL params

### Phase 3: Expand Placeholders in Chart Data Endpoint

**File: `src/Pages/Charts.hs`**

1. In `chartDataGetH` (or wherever queries are processed), expand placeholders:
   ```haskell
   -- Extract const-* params from request
   let constParams = filter (("const-" `T.isPrefixOf`) . fst) allParams
       expandedQuery = replacePlaceholders (Map.fromList constParams) originalQuery
   ```

2. Use `expandedQuery` for execution instead of the original

### Phase 4: Cleanup

**File: `src/Pkg/Components/Widget.hs`**

1. Remove `rawQuery` and `rawSql` fields entirely
2. Update all ToJSON/FromJSON derivations

**File: `src/Pages/Dashboards.hs`**

1. Remove all references to `rawQuery` and `rawSql`
2. Remove the code that sets these fields in `processWidget`
3. Update `widgetViewerEditor_` - just use `query` directly (it now has the original)

**File: `web-components/src/widgets.ts`**

1. Remove any references to `rawQuery` / `rawQuerySQL` from config

## Files to Modify

| File | Changes |
|------|---------|
| `src/Pkg/Components/Widget.hs` | Remove `rawQuery`, `rawSql` fields; update `renderChart` |
| `src/Pages/Dashboards.hs` | Stop expanding const placeholders in `processWidget`; add constants to page; update `widgetViewerEditor_` |
| `src/Pages/Charts.hs` | Expand const placeholders before query execution |
| `web-components/src/widgets.ts` | Pass constants in `/chart_data` requests |
| `src/Pkg/DashboardUtils.hs` | May need helper to extract const-* params |

## Edge Cases to Handle

1. **Nested widgets**: Ensure constants are available for child widgets
2. **Tab lazy loading**: When tabs load via HTMX, ensure constants are passed
3. **Widget preview**: The widget editor preview needs constants too
4. **Empty constants**: Already handled with sentinel value `("__EMPTY_CONST__")`
5. **Variable dependencies**: Some constants may depend on variables - ensure correct order

## Testing Plan

1. Create dashboard with constants
2. Verify Monaco shows original query with `{{const-...}}`
3. Verify chart loads data correctly (placeholders expanded at execution)
4. Test with empty constant results (should use sentinel value)
5. Test widget expansion drawer
6. Test widget preview in editor
7. Test tab lazy loading with constants

## Rollback Plan

If issues arise, revert to the dual-field approach:
1. Restore `rawQuery` / `rawSql` fields
2. Restore expansion in `processWidget`
3. The changes are isolated to the files listed above

## Future Considerations

1. **Caching**: Constants could be cached per-dashboard to avoid re-execution
2. **Real-time constants**: Some constants might need periodic refresh
3. **Constant dependencies**: A constant's query might reference another constant
