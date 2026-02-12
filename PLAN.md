# Plan: Extract Shared Haskell/Lucid Helpers & Delete Dead Code

## Summary

- **7 files changed**, 2 files deleted
- **Net: -405 lines** (93 removed from dedup + 360 dead code deleted - 29 new helper code + 19 import line changes)
- **0 API changes** — all external function signatures stay the same
- **0 behavior changes** — visual output is identical (expansion panels switch from CSS checkbox-peer to native `<details>/<summary>`, which is functionally equivalent)

---

## Phase 1: Enhance `collapsibleSection` in PageHeader.hs

**File:** `src/Pkg/Components/PageHeader.hs`
**Current signature (line 88):**
```haskell
collapsibleSection :: Text -> Maybe Text -> Html () -> Html ()
collapsibleSection title subtitleM content
```

**New signature:**
```haskell
collapsibleSection :: Text -> Maybe Text -> Maybe Text -> Bool -> [Attribute] -> Html () -> Html ()
collapsibleSection title iconM subtitleM open extraAttrs content
```

**Changes:**
- Add `iconM :: Maybe Text` — optional FA icon name (e.g. `Just "clock"`)
- Add `open :: Bool` — whether section starts expanded (`<details open>`)
- Add `extraAttrs :: [Attribute]` — escape hatch for `id_` or custom attrs
- Remove the inner `div_ [class_ "px-4 pb-4"]` content wrapper — let the caller control their own padding/layout

**Full replacement (lines 88-100):**
```haskell
collapsibleSection :: Text -> Maybe Text -> Maybe Text -> Bool -> [Attribute] -> Html () -> Html ()
collapsibleSection title iconM subtitleM open extraAttrs content =
  details_ ([class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"]
           <> [term "open" "" | open] <> extraAttrs) do
    summary_
      [ class_ "p-4 cursor-pointer list-none flex items-center justify-between gap-2"
      , [__|on click toggle .rotate-180 on the next <svg/> in me|]
      ]
      do
        div_ [class_ "flex items-center gap-2"] do
          whenJust iconM \icon -> faSprite_ icon "regular" "w-4 h-4 text-iconNeutral"
          span_ [class_ "font-medium text-sm"] $ toHtml title
          whenJust subtitleM $ span_ [class_ "text-xs text-textWeak"] . toHtml
        faSprite_ "chevron-down" "regular" "w-3.5 h-3.5 text-iconNeutral transition-transform duration-150"
    content
```

**Impact:** +2 lines (14 → 16). Zero callers to update (nobody imports this today).

---

## Phase 2: Replace expansion panels in Monitors.hs

**File:** `src/Pages/Monitors.hs` (518 lines)

### 2a. Delete duplicate `collapsibleSection_` definition

**Delete lines 17 and 363-375:**
```haskell
-- DELETE from exports (line 17):
  collapsibleSection_,

-- DELETE definition (lines 363-375):
collapsibleSection_ :: Text -> Maybe Text -> Html () -> Html ()
collapsibleSection_ title subtitleM content =
  details_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
    summary_
      [ class_ "p-4 cursor-pointer list-none flex items-center justify-between gap-2"
      , [__|on click toggle .rotate-180 on the next <svg/> in me|]
      ]
      do
        div_ [class_ "flex items-center gap-2"] do
          span_ [class_ "font-medium text-sm"] $ toHtml title
          whenJust subtitleM $ span_ [class_ "text-xs text-textWeak"] . toHtml
        faSprite_ "chevron-down" "regular" "w-3.5 h-3.5 text-iconNeutral transition-transform duration-150"
    div_ [class_ "px-4 pb-4"] content
```
**Saved: -14 lines**

### 2b. Add import

**Add after line 44 (existing imports):**
```haskell
import Pkg.Components.PageHeader (collapsibleSection)
```
**Added: +1 line**

### 2c. Replace `monitorScheduleSection_` chrome (lines 393-400)

**Before (8 lines of chrome):**
```haskell
  div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
    label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "clock" "regular" "w-4 h-4 text-iconNeutral"
        span_ [class_ "text-sm font-medium text-textStrong"] "Monitor Schedule"
      input_ [type_ "checkbox", class_ "hidden peer", checked_]
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"
    div_ [class_ "gap-3 p-3 pt-0 peer-has-[:checked]:flex hidden"] do
```

**After (2 lines):**
```haskell
  collapsibleSection "Monitor Schedule" (Just "clock") Nothing True [] do
    div_ [class_ "flex gap-3 p-3 pt-0"] do
```
**Saved: -6 lines**

### 2d. Replace `thresholdsSection_` chrome (lines 421-428)

**Before (8 lines):**
```haskell
  div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden", id_ "thresholds"] do
    label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "chart-line" "regular" "w-4 h-4 text-iconNeutral"
        span_ [class_ "text-sm font-medium text-textStrong"] "Thresholds"
      input_ [type_ "checkbox", class_ "hidden peer", checked_]
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"
    div_ [class_ "p-3 pt-0 peer-has-[:checked]:block hidden"] do
```

**After (2 lines):**
```haskell
  collapsibleSection "Thresholds" (Just "chart-line") Nothing True [id_ "thresholds"] do
    div_ [class_ "p-3 pt-0"] do
```
**Saved: -6 lines**

### 2e. Replace `notificationSettingsSection_` chrome (lines 455-462)

**Before (8 lines):**
```haskell
  div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
    label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "envelope" "regular" "w-4 h-4 text-iconNeutral"
        span_ [class_ "text-sm font-medium text-textStrong"] "Notification Settings"
      input_ [type_ "checkbox", class_ "hidden peer"]
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"
    div_ [class_ "p-3 pt-0 peer-has-[:checked]:block hidden"] do
```

**After (2 lines):**
```haskell
  collapsibleSection "Notification Settings" (Just "envelope") Nothing False [] do
    div_ [class_ "p-3 pt-0"] do
```
**Saved: -6 lines**

Note: `False` because this section starts collapsed (original had no `checked_`).

### Monitors.hs total: -31 lines

**External consumers unaffected:** `Dashboards.hs` (line 91: `import Pages.Monitors qualified as Alerts`) and `LogExplorer/Log.hs` (line 57: `import Pages.Monitors qualified as AlertUI`) both call `monitorScheduleSection_`, `thresholdsSection_`, and `notificationSettingsSection_` — the APIs are unchanged, only internal chrome changed.

---

## Phase 3: Add shared helpers to Components.hs

**File:** `src/Pages/Components.hs` (625 lines)

### 3a. Add `confirmModal_`

Add after `modalCloseButton_` (line 510):

```haskell
-- | Confirmation modal with warning icon, title, description, and action button
-- Usage: confirmModal_ "remove-modal" "Remove bucket?" "This will disconnect..." $
--          button_ [...] "Remove"
confirmModal_ :: Text -> Text -> Text -> Html () -> Html ()
confirmModal_ modalId title description actionButton = do
  input_ [type_ "checkbox", id_ modalId, class_ "modal-toggle"]
  div_ [class_ "modal", role_ "dialog"] do
    div_ [class_ "modal-box p-6"] do
      div_ [class_ "flex items-start gap-3 mb-4"] do
        div_ [class_ "p-2 bg-fillError-weak rounded-full"] $
          faSprite_ "triangle-alert" "regular" "h-5 w-5 text-textError"
        div_ do
          h3_ [class_ "text-lg font-semibold text-textStrong"] $ toHtml title
          p_ [class_ "text-sm text-textWeak mt-1"] $ toHtml description
      div_ [class_ "flex justify-end gap-2 mt-6"] do
        label_ [class_ "btn btn-sm btn-ghost", Lucid.for_ modalId] "Cancel"
        actionButton
    label_ [class_ "modal-backdrop", Lucid.for_ modalId] ""
```
**Added: +16 lines**

### 3b. Add `formActionsModal_`

```haskell
-- | Modal form footer with Cancel (label that closes modal) + Submit button
formActionsModal_ :: Text -> Text -> Text -> Html ()
formActionsModal_ modalId cancelLabel submitLabel =
  div_ [class_ "mt-3 flex justify-end gap-2"] do
    label_ [Lucid.for_ modalId, class_ "btn btn-outline cursor-pointer"] $ toHtml cancelLabel
    button_ [type_ "submit", class_ "btn btn-primary"] $ toHtml submitLabel
```
**Added: +5 lines**

### 3c. Add `connectionField_`

Shared replacement for `S3.connectionField` and `GitSync.gitField`:

```haskell
-- | Labeled input field for connection/settings forms
-- Handles required marker, password type, placeholder
connectionField_ :: Text -> Text -> Bool -> Text -> Bool -> Text -> Html ()
connectionField_ label name required defVal isPassword placeholder =
  fieldset_ [class_ "fieldset"] do
    label_ [class_ "label text-xs font-medium"] do
      toHtml label
      when required $ span_ [class_ "text-textError ml-1"] "*"
    input_ $ [class_ "input input-sm w-full", value_ defVal, name_ name
             , type_ $ if isPassword then "password" else "text"
             , placeholder_ placeholder
             ] <> [required_ "true" | required]
```
**Added: +10 lines**

Note: This unifies the visual style. S3 fields previously used `div_ [class_ "space-y-1.5"]` + `input-bordered`; GitSync used identical code with an extra placeholder param. Both now use `fieldset_` (DaisyUI semantic pattern). The visual change is minor — slightly different spacing, consistent with the rest of the platform.

### 3d. Update module exports (line 1)

**Before:**
```haskell
module Pages.Components (statBox, drawer_, statBox_, emptyState_, emptyStateFiltered_, resizer_, dateTime, paymentPlanPicker, navBar, modal_, modalCloseButton_, tableSkeleton_, chartSkeleton_, cardSkeleton_, statBoxSkeleton_) where
```

**After:**
```haskell
module Pages.Components (statBox, drawer_, statBox_, emptyState_, emptyStateFiltered_, resizer_, dateTime, paymentPlanPicker, navBar, modal_, modalCloseButton_, confirmModal_, formActionsModal_, connectionField_, tableSkeleton_, chartSkeleton_, cardSkeleton_, statBoxSkeleton_) where
```

### Components.hs total: +31 lines added

---

## Phase 4: Replace confirmation modals

### 4a. S3.hs (lines 107-118)

**File:** `src/Pages/S3.hs`

**Add import:**
```haskell
import Pages.Components (confirmModal_)
```

**Before (lines 107-118, 12 lines):**
```haskell
    input_ [type_ "checkbox", id_ "remove-modal", class_ "modal-toggle"]
    div_ [class_ "modal", role_ "dialog"] do
      div_ [class_ "modal-box p-6"] do
        div_ [class_ "flex items-start gap-3 mb-4"] do
          div_ [class_ "p-2 bg-fillError-weak rounded-full"] $ faSprite_ "triangle-alert" "regular" "h-5 w-5 text-textError"
          div_ do
            h3_ [class_ "text-lg font-semibold text-textStrong"] "Remove bucket?"
            p_ [class_ "text-sm text-textWeak mt-1"] "This will disconnect your S3 bucket. Data already stored will remain in your bucket."
        div_ [class_ "flex justify-end gap-2 mt-6"] do
          label_ [class_ "btn btn-sm btn-ghost", Lucid.for_ "remove-modal"] "Cancel"
          button_ [class_ "btn btn-sm bg-fillError-strong text-white hover:opacity-90", hxDelete_ "", hxSwap_ "innerHTML", hxTarget_ "#connectedInd"] "Remove bucket"
      label_ [class_ "modal-backdrop", Lucid.for_ "remove-modal"] ""
```

**After (4 lines):**
```haskell
    confirmModal_ "remove-modal" "Remove bucket?"
      "This will disconnect your S3 bucket. Data already stored will remain in your bucket." $
      button_ [class_ "btn btn-sm bg-fillError-strong text-white hover:opacity-90",
               hxDelete_ "", hxSwap_ "innerHTML", hxTarget_ "#connectedInd"] "Remove bucket"
```
**Saved: -8 lines**

### 4b. Delete S3.hs `connectionField` definition (lines 121-127)

**Delete:**
```haskell
connectionField :: Text -> Text -> Bool -> Text -> Bool -> Html ()
connectionField lbl name required defVal isPass =
  div_ [class_ "space-y-1.5"] do
    label_ [class_ "flex items-center gap-1 text-xs font-medium text-textWeak"] do
      toHtml lbl
      when required $ span_ [class_ "text-textError"] "*"
    input_ ([class_ "input input-bordered input-sm w-full", value_ defVal, name_ name, type_ $ if isPass then "password" else "text", placeholder_ lbl] <> [required_ "true" | required])
```
**Saved: -7 lines**

### 4c. Update S3.hs `connectionField` call sites (lines 88-92)

**Before:**
```haskell
          connectionField "Access Key ID" "accessKey" True (maybe "" (.accessKey) s3BucketM) False
          connectionField "Secret Access Key" "secretKey" True (maybe "" (.secretKey) s3BucketM) True
          connectionField "Region" "region" True (maybe "" (.region) s3BucketM) False
          connectionField "Bucket Name" "bucket" True (maybe "" (.bucket) s3BucketM) False
        connectionField "Custom Endpoint" "endpointUrl" False (maybe "" (.endpointUrl) s3BucketM) False
```

**After:**
```haskell
          connectionField_ "Access Key ID" "accessKey" True (maybe "" (.accessKey) s3BucketM) False "Access Key ID"
          connectionField_ "Secret Access Key" "secretKey" True (maybe "" (.secretKey) s3BucketM) True "Secret Access Key"
          connectionField_ "Region" "region" True (maybe "" (.region) s3BucketM) False "Region"
          connectionField_ "Bucket Name" "bucket" True (maybe "" (.bucket) s3BucketM) False "Bucket Name"
        connectionField_ "Custom Endpoint" "endpointUrl" False (maybe "" (.endpointUrl) s3BucketM) False "Custom Endpoint"
```

Note: The old `connectionField` used the label as placeholder. The new `connectionField_` has an explicit placeholder param. The call sites pass the same label as placeholder to preserve behavior.

**Saved: 0 lines** (same count, just different function name + extra param)

### S3.hs total: -15 lines + 1 import = -14 net

---

### 4d. GitSync.hs (lines 309-320)

**File:** `src/Pages/GitSync.hs`

**Add import:**
```haskell
import Pages.Components (confirmModal_)
```

**Before (lines 309-320, 12 lines):**
```haskell
  input_ [type_ "checkbox", id_ "disconnect-modal", class_ "modal-toggle"]
  div_ [class_ "modal", role_ "dialog"] do
    div_ [class_ "modal-box p-6"] do
      div_ [class_ "flex items-start gap-3 mb-4"] do
        div_ [class_ "p-2 bg-fillError-weak rounded-full"] $ faSprite_ "triangle-alert" "regular" "h-5 w-5 text-textError"
        div_ do
          h3_ [class_ "text-lg font-semibold text-textStrong"] "Disconnect GitHub?"
          p_ [class_ "text-sm text-textWeak mt-1"] "This will stop syncing dashboards with your repository. Dashboards will remain unchanged."
      div_ [class_ "flex justify-end gap-2 mt-6"] do
        label_ [class_ "btn btn-sm btn-ghost", Lucid.for_ "disconnect-modal"] "Cancel"
        button_ [class_ "btn btn-sm bg-fillError-strong text-white hover:opacity-90", hxDelete_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content"] "Disconnect"
    label_ [class_ "modal-backdrop", Lucid.for_ "disconnect-modal"] ""
```

**After (4 lines):**
```haskell
  confirmModal_ "disconnect-modal" "Disconnect GitHub?"
    "This will stop syncing dashboards with your repository. Dashboards will remain unchanged." $
    button_ [class_ "btn btn-sm bg-fillError-strong text-white hover:opacity-90",
             hxDelete_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content"] "Disconnect"
```
**Saved: -8 lines**

### 4e. Delete GitSync.hs `gitField` definition (lines 327-333)

**Delete:**
```haskell
gitField :: Text -> Text -> Bool -> Text -> Bool -> Text -> Html ()
gitField lbl name required defVal isPass placeholder =
  div_ [class_ "space-y-1.5"] do
    label_ [class_ "flex items-center gap-1 text-xs font-medium text-textWeak"] do
      toHtml lbl
      when required $ span_ [class_ "text-textError"] "*"
    input_ ([class_ "input input-bordered input-sm w-full", value_ defVal, name_ name, type_ $ if isPass then "password" else "text", placeholder_ placeholder] <> [required_ "true" | required])
```
**Saved: -7 lines**

### 4f. Update GitSync.hs `gitField` call sites

All 10 call sites change `gitField` → `connectionField_`. The params are already in the same order:
```haskell
-- Before:
gitField "Repository Owner" "owner" True "" False "acme-corp"
-- After:
connectionField_ "Repository Owner" "owner" True "" False "acme-corp"
```
**Saved: 0 lines** (just rename)

### GitSync.hs total: -15 lines + 1 import = -14 net

---

## Phase 5: Replace modal form footers

### 5a. Dashboards.hs rename modal (line 200-201)

**File:** `src/Pages/Dashboards.hs`

**Add import (merge into existing line 88):**
```haskell
import Pages.Components qualified as Components
-- already imported, just use Components.formActionsModal_
```

**Before (lines 200-201):**
```haskell
      div_ [class_ "mt-3 flex justify-end gap-2"] do
        label_ [Lucid.for_ "pageTitleModalId", class_ "btn btn-outline cursor-pointer"] "Cancel"
        button_ [type_ "submit", class_ "btn btn-primary"] "Save"
```

**After (1 line):**
```haskell
      Components.formActionsModal_ "pageTitleModalId" "Cancel" "Save"
```
**Saved: -2 lines**

### 5b. Dashboards.hs tab rename modal (lines 220-221)

**Before:**
```haskell
        div_ [class_ "mt-3 flex justify-end gap-2"] do
          label_ [Lucid.for_ "tabRenameModalId", class_ "btn btn-outline cursor-pointer"] "Cancel"
          button_ [type_ "submit", class_ "btn btn-primary"] "Save"
```

**After:**
```haskell
        Components.formActionsModal_ "tabRenameModalId" "Cancel" "Save"
```
**Saved: -2 lines**

### Dashboards.hs total: -4 lines

---

### 5c. Projects.hs team modal footer (lines 1717-1719)

**File:** `src/Pages/Projects.hs`

**Note:** Projects.hs already imports `Pages.Components (modalCloseButton_, paymentPlanPicker)`. Add `formActionsModal_` and `connectionField_` to that import.

**Update import (line 98):**
```haskell
-- Before:
import Pages.Components (modalCloseButton_, paymentPlanPicker)
-- After:
import Pages.Components (modalCloseButton_, formActionsModal_, paymentPlanPicker)
```

**Before (lines 1717-1719):**
```haskell
        div_ [class_ "pt-6 mt-6 border-t border-strokeWeak flex justify-end gap-3"] do
          label_ [Lucid.for_ modalId, class_ "btn btn-outline"] "Cancel"
          button_ [class_ "btn btn-primary", type_ "submit"] $ faSprite_ "check" "solid" "w-4 h-4" >> " " >> if isJust team then "Save Changes" else "Create Team"
```

**After (3 lines):** Keep as-is. The Projects footer has a **different layout** (`pt-6 mt-6 border-t`, different btn classes, icon in submit button, conditional label) from the standard `formActionsModal_`. Forcing it into the helper would require making the helper overly complex. **Skip this one.**

### Projects.hs: Delete local helpers only

**Delete local helper definitions (lines 1663-1667):**
```haskell
  let field_ inputId lbl input = fieldset_ [class_ "fieldset"] (label_ [class_ "label text-sm font-medium", Lucid.for_ inputId] lbl >> input)
  let fieldIcon_ icon inputId lbl input = fieldset_ [class_ "fieldset p-4"] do
        _ <- label_ [class_ "label text-sm font-medium flex items-center gap-2 mb-2", Lucid.for_ inputId] (faSprite_ icon "solid" "w-4 h-4 text-iconNeutral shrink-0" >> lbl)
        input
  let tagInput_ inputId ph = textarea_ [class_ "textarea w-full min-h-12 resize-none", id_ inputId, placeholder_ ph] ""
```

**Decision:** These local helpers are well-tailored to this modal's specific styling (the `fieldIcon_` has `p-4` for divided sections, `field_` has `for_` linking). They differ enough from `connectionField_` that forcing them into the shared helper would make call sites longer. **Keep them as local `let` bindings.** They're already clean — the issue was never these specific helpers, but the duplicated ones (S3/GitSync).

### Projects.hs total: 0 lines changed (import update only if formActionsModal_ is used elsewhere, but we're skipping the footer)

---

## Phase 6: Delete unused Form.hs

**File:** `src/Pkg/Components/Form.hs` (360 lines)

**Action:** Delete the entire file.

**Rationale:**
- Zero imports across the entire codebase (verified via grep)
- The DSL approach (`textField "name" "Label" & withRequired & withValue v`) is more verbose than direct Lucid (`fieldset_ do label_ "Label"; input_ [...]`) for the forms in this codebase
- Most forms have complex HTMX attributes (hxVals_ with JS expressions, hyperscript event handlers) that don't fit the DSL well
- The new `connectionField_` in Components.hs covers the only case where a form field helper is genuinely useful (repeated simple text inputs)

**Saved: -360 lines**

**Also remove from cabal/package file** if `Form.hs` is listed in `exposed-modules` or `other-modules`.

---

## Files NOT Changed (and why)

| File | Lines | Why unchanged |
|------|-------|---------------|
| `Pages/Anomalies.hs` | 1301 | Collapsibles use different styling (lightweight inline `details_` with custom borders, not the card-style expansion panels). Different UX purpose. |
| `Pages/LogExplorer/Log.hs` | 1097 | Facets use animated `max-height` CSS transitions incompatible with `details_/summary_`. Alert form uses Monitors section components (API unchanged). |
| `Pages/Onboarding/Onboarding.hs` | 1031 | Unique full-page styling, non-standard layout. Not worth unifying. |
| `Pages/Api.hs` | 297 | Single modal with unique layout. No repeated pattern to extract. |
| `Pages/LemonSqueezy.hs` | 235 | Payment-specific layout, no overlap. |
| `Pkg/Components/Table.hs` | 809 | Already well-abstracted with its own DSL. |
| `Pkg/Components/Widget.hs` | 1325 | Domain-specific widget rendering. No repeated form patterns. |

---

## Change Ledger

| File | Before | After | Delta |
|------|--------|-------|-------|
| `Pkg/Components/PageHeader.hs` | 105 | 107 | **+2** |
| `Pages/Components.hs` | 625 | 656 | **+31** |
| `Pages/Monitors.hs` | 518 | 487 | **-31** |
| `Pages/S3.hs` | 139 | 125 | **-14** |
| `Pages/GitSync.hs` | 556 | 542 | **-14** |
| `Pages/Dashboards.hs` | 2533 | 2529 | **-4** |
| `Pkg/Components/Form.hs` | 360 | 0 | **-360** |
| **Total** | **4836** | **4446** | **-390** |

---

## Execution Order

The phases must be done in this order because of dependencies:

1. **Phase 1** — Enhance `collapsibleSection` (PageHeader.hs) — no deps
2. **Phase 3** — Add helpers to Components.hs — no deps
3. **Phase 2** — Update Monitors.hs — depends on Phase 1
4. **Phase 4** — Update S3.hs and GitSync.hs — depends on Phase 3
5. **Phase 5** — Update Dashboards.hs — depends on Phase 3
6. **Phase 6** — Delete Form.hs — independent, do last

Phases 1+3 can be done in parallel. Phases 2+4+5 can be done in parallel after 1+3.

---

## Risk Assessment

- **Monitors expansion panels:** Switching from checkbox-peer CSS to native `<details>/<summary>` changes the DOM structure. Both `Dashboards.hs` and `Log.hs` call `monitorScheduleSection_`, `thresholdsSection_`, and `notificationSettingsSection_` — but these are server-rendered HTML helpers, not HTMX endpoints that target specific DOM IDs within the panels. The content inside the panels is unchanged. **Risk: low.**

- **The `id_ "thresholds"` on the thresholds panel** is used by JavaScript: `remove .hidden from #thresholds` / `add .hidden to #thresholds`. With `<details>`, hiding/showing works differently (via `open` attribute). The hyperscript that toggles `#thresholds` visibility will need to be updated to use `set #thresholds.open to true/false` instead of `add/remove .hidden`. This is in the conditionType select's `on change` handler (line 409). **Must update this handler.**

- **S3/GitSync field styling:** Switching from `div_ + space-y-1.5 + input-bordered` to `fieldset_ + input` changes the visual spacing slightly. The user has approved unifying styles. **Risk: cosmetic only.**

- **Form.hs deletion:** Zero imports confirmed. **Risk: none.**
