# Plan: Extract Shared Haskell/Lucid Helpers & Delete Dead Code (v2)

## Summary

- **11 files changed**, 1 file deleted
- **Net: ~-440 lines**
- Core idea: ONE `formSection_` function that works as a static titled section OR a collapsible panel with chevron, plus shared field/modal helpers used everywhere

---

## Phase 1: Add all shared helpers to Components.hs

**File:** `src/Pages/Components.hs` (625 lines)

### 1a. `formSection_` — the unified section helper

Replaces: `collapsibleSection` (PageHeader.hs), `collapsibleSection_` (Monitors.hs), `teamSection_` (Projects.hs local), all inline `details_/summary_` patterns in Anomalies.hs, facet section toggles in Log.hs, `faQ` in Onboarding.hs.

```haskell
-- | Titled section, optionally collapsible.
-- When collapsible: renders as <details>/<summary> with chevron icon.
-- When static: renders as a div with a header row (no chevron, always visible).
formSection_ :: Text -> Maybe Text -> Maybe Text -> Bool -> Bool -> [Attribute] -> Html () -> Html ()
formSection_ title iconM subtitleM collapsible open extraAttrs content
  | collapsible =
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
  | otherwise =
      div_ ([class_ "space-y-3"] <> extraAttrs) do
        h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wider flex items-center gap-2"] do
          whenJust iconM \icon -> faSprite_ icon "regular" "w-4 h-4 text-iconNeutral"
          toHtml title
          whenJust subtitleM $ span_ [class_ "text-xs text-textWeak normal-case font-normal"] . toHtml
        content
```
**+20 lines**

Usage examples:
```haskell
-- Static section (team modal "Details" header):
formSection_ "Details" Nothing Nothing False True [] do
  div_ [class_ "rounded-lg bg-bgRaised border border-strokeWeak p-4 space-y-4"] do ...

-- Collapsible section (Monitors "Monitor Schedule"):
formSection_ "Monitor Schedule" (Just "clock") Nothing True True [] do
  div_ [class_ "flex gap-3 p-3 pt-0"] do ...

-- Collapsible, starts closed (Monitors "Notification Settings"):
formSection_ "Notification Settings" (Just "envelope") Nothing True False [] do ...

-- Collapsible with extra attrs (Monitors "Thresholds" with id):
formSection_ "Thresholds" (Just "chart-line") Nothing True True [id_ "thresholds"] do ...

-- Anomalies AI chat system prompt:
formSection_ "System Prompt" (Just "terminal") Nothing True False [class_ "mb-4"] do ...

-- Onboarding FAQ:
formSection_ question Nothing Nothing True False [] do
  p_ [class_ "text-textWeak font-medium leading-relaxed"] $ toHtml answer
```

### 1b. `confirmModal_`

```haskell
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
**+16 lines**

### 1c. `formActionsModal_`

Takes `Html ()` for submit content (supports icon + conditional text):

```haskell
formActionsModal_ :: Text -> Text -> Html () -> Html ()
formActionsModal_ modalId cancelLabel submitHtml =
  div_ [class_ "pt-6 mt-6 border-t border-strokeWeak flex justify-end gap-3"] do
    label_ [Lucid.for_ modalId, class_ "btn btn-outline cursor-pointer"] $ toHtml cancelLabel
    button_ [type_ "submit", class_ "btn btn-primary"] submitHtml
```
**+5 lines**

### 1d. `connectionField_`

Shared replacement for `S3.connectionField` and `GitSync.gitField`:

```haskell
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
**+10 lines**

### 1e. `formField_` / `formFieldIcon_` / `tagInput_`

Shared from Projects.hs team modal locals + Log.hs alert form:

```haskell
formField_ :: Text -> Text -> Html () -> Html ()
formField_ inputId label inputHtml =
  fieldset_ [class_ "fieldset"] do
    label_ [class_ "label text-sm font-medium", Lucid.for_ inputId] $ toHtml label
    inputHtml

formFieldIcon_ :: Text -> Text -> Text -> Html () -> Html ()
formFieldIcon_ icon inputId label inputHtml =
  fieldset_ [class_ "fieldset p-4"] do
    label_ [class_ "label text-sm font-medium flex items-center gap-2 mb-2", Lucid.for_ inputId] do
      faSprite_ icon "solid" "w-4 h-4 text-iconNeutral shrink-0"
      toHtml label
    inputHtml

tagInput_ :: Text -> Text -> Html ()
tagInput_ inputId placeholder =
  textarea_ [class_ "textarea w-full min-h-12 resize-none", id_ inputId, placeholder_ placeholder] ""
```
**+16 lines**

### 1f. Update module exports

```haskell
module Pages.Components (...existing..., formSection_, confirmModal_, formActionsModal_,
  connectionField_, formField_, formFieldIcon_, tagInput_) where
```

### Components.hs total: +67 lines added

---

## Phase 2: Delete `collapsibleSection` from PageHeader.hs

**File:** `src/Pkg/Components/PageHeader.hs`

Delete `collapsibleSection` (lines 88-100) and its export (line 16). The `card` helper (line 104-105) also has zero imports — delete it too.

```haskell
-- DELETE lines 16-17 from exports:
  collapsibleSection,
  card,

-- DELETE lines 88-105:
collapsibleSection :: Text -> Maybe Text -> Html () -> Html ()
...
card :: Html () -> Html ()
card = div_ [class_ "rounded-lg bg-bgRaised border border-strokeWeak p-4"]
```
**Saved: -20 lines**

---

## Phase 3: Monitors.hs

**File:** `src/Pages/Monitors.hs` (518 lines)

### 3a. Delete duplicate `collapsibleSection_` export + definition (lines 17, 363-375): **-14 lines**

### 3b. Add import

```haskell
import Pages.Components (formSection_)
```
**+1 line**

### 3c. Replace 3 expansion panels

**monitorScheduleSection_ (lines 393-400) — 8 lines → 2:**
```haskell
-- Before:
  div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
    label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "clock" "regular" "w-4 h-4 text-iconNeutral"
        span_ [class_ "text-sm font-medium text-textStrong"] "Monitor Schedule"
      input_ [type_ "checkbox", class_ "hidden peer", checked_]
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"
    div_ [class_ "gap-3 p-3 pt-0 peer-has-[:checked]:flex hidden"] do

-- After:
  formSection_ "Monitor Schedule" (Just "clock") Nothing True True [] do
    div_ [class_ "flex gap-3 p-3 pt-0"] do
```
**-6 lines**

**thresholdsSection_ (lines 421-428) — 8 → 2:** **-6 lines**
```haskell
  formSection_ "Thresholds" (Just "chart-line") Nothing True True [id_ "thresholds"] do
    div_ [class_ "p-3 pt-0"] do
```

**notificationSettingsSection_ (lines 455-462) — 8 → 2:** **-6 lines**
```haskell
  formSection_ "Notification Settings" (Just "envelope") Nothing True False [] do
    div_ [class_ "p-3 pt-0"] do
```

### 3d. Update `#thresholds` visibility toggle (line 409)

The conditionType select's hyperscript toggles `#thresholds` via `add/remove .hidden`. With `<details>`, change to:
```haskell
-- Before:
[__|on change if my value == 'threshold_exceeded' then remove .hidden from #thresholds else add .hidden to #thresholds end|]
-- After:
[__|on change if my value == 'threshold_exceeded' then set #thresholds.open to true else set #thresholds.open to false end|]
```

### Monitors.hs total: -31 lines

---

## Phase 4: S3.hs

**File:** `src/Pages/S3.hs` (139 lines)

| Change | Lines |
|--------|:-----:|
| Add import `Pages.Components (confirmModal_, connectionField_)` | +1 |
| Replace confirm modal (107-118): 12 → 4 | -8 |
| Delete `connectionField` definition (121-127) | -7 |
| Update 5 call sites: `connectionField` → `connectionField_` (add placeholder param) | 0 |
| **Total** | **-14** |

---

## Phase 5: GitSync.hs

**File:** `src/Pages/GitSync.hs` (556 lines)

| Change | Lines |
|--------|:-----:|
| Add import `Pages.Components (confirmModal_, connectionField_)` | +1 |
| Replace confirm modal (309-320): 12 → 4 | -8 |
| Delete `gitField` definition (327-333) | -7 |
| Update 10 call sites: `gitField` → `connectionField_` | 0 |
| **Total** | **-14** |

---

## Phase 6: Projects.hs

**File:** `src/Pages/Projects.hs` (1756 lines)

### 6a. Update import (line 98)

```haskell
-- Before:
import Pages.Components (modalCloseButton_, paymentPlanPicker)
-- After:
import Pages.Components (modalCloseButton_, paymentPlanPicker, formSection_, formActionsModal_, formField_, formFieldIcon_, tagInput_)
```

### 6b. Delete 4 local helper definitions (lines 1659-1667)

```haskell
-- DELETE teamSection_ (lines 1659-1661):
  let teamSection_ title content = div_ [class_ "space-y-4"] do
        _ <- h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wider"] title
        content

-- DELETE field_, fieldIcon_, tagInput_ (lines 1663-1667):
  let field_ inputId lbl input = ...
  let fieldIcon_ icon inputId lbl input = ...
  let tagInput_ inputId ph = ...
```
**-9 lines**

### 6c. Update `teamSection_` call sites → `formSection_`

```haskell
-- Before (line 1681):
          teamSection_ "Details" do
-- After:
          formSection_ "Details" Nothing Nothing False True [] do

-- Before (line 1688):
          unless isEveryoneTeam $ teamSection_ "Members" do
-- After:
          unless isEveryoneTeam $ formSection_ "Members" Nothing Nothing False True [] do

-- Before (line 1699):
          teamSection_ "Notifications" do
-- After:
          formSection_ "Notifications" Nothing Nothing False True [] do
```
**0 net lines** (same line count, different function)

### 6d. Update `field_` call sites → `formField_`

```haskell
-- Before:
                field_ (mkId "team-name") "Team Name" $ input_ [...]
-- After:
                formField_ (mkId "team-name") "Team Name" $ input_ [...]
```
**0 net lines** (just renamed, 4 call sites)

### 6e. Update `fieldIcon_` call sites → `formFieldIcon_`

```haskell
-- Before:
              fieldIcon_ "envelope" (mkId "notif-emails-input") "Email" $ tagInput_ (mkId "notif-emails-input") "Add email addresses..."
-- After:
              formFieldIcon_ "envelope" (mkId "notif-emails-input") "Email" $ tagInput_ (mkId "notif-emails-input") "Add email addresses..."
```
**0 net lines** (just renamed, 4 call sites)

### 6f. Replace footer (lines 1717-1719) → `formActionsModal_`

```haskell
-- Before:
        div_ [class_ "pt-6 mt-6 border-t border-strokeWeak flex justify-end gap-3"] do
          label_ [Lucid.for_ modalId, class_ "btn btn-outline"] "Cancel"
          button_ [class_ "btn btn-primary", type_ "submit"] $ faSprite_ "check" "solid" "w-4 h-4" >> " " >> if isJust team then "Save Changes" else "Create Team"

-- After:
        formActionsModal_ modalId "Cancel" $ faSprite_ "check" "solid" "w-4 h-4" >> " " >> if isJust team then "Save Changes" else "Create Team"
```
**-2 lines**

### Projects.hs total: -11 lines

---

## Phase 7: Dashboards.hs

**File:** `src/Pages/Dashboards.hs` (2533 lines)

### 7a. Replace rename modal footer (lines 200-202) → `formActionsModal_`

```haskell
-- Before:
      div_ [class_ "mt-3 flex justify-end gap-2"] do
        label_ [Lucid.for_ "pageTitleModalId", class_ "btn btn-outline cursor-pointer"] "Cancel"
        button_ [type_ "submit", class_ "btn btn-primary"] "Save"

-- After:
      formActionsModal_ "pageTitleModalId" "Cancel" "Save"
```
**-2 lines**

### 7b. Replace tab rename modal footer (lines 220-222)

Same pattern. **-2 lines**

### Dashboards.hs total: -4 lines

---

## Phase 8: Anomalies.hs

**File:** `src/Pages/Anomalies.hs` (1301 lines)

### 8a. Add import

```haskell
import Pages.Components (formSection_)
```
**+1 line**

### 8b. AI Chat — System Prompt (lines 634-638)

```haskell
-- Before (5 lines):
      details_ [class_ "mb-4 border border-strokeWeak rounded-lg"] do
        summary_ [class_ "cursor-pointer px-4 py-2 text-sm text-textWeak hover:bg-fillWeaker list-none flex items-center gap-2"] do
          faSprite_ "terminal" "regular" "w-4 h-4"
          "System Prompt"
        div_ [class_ "px-4 py-3 border-t border-strokeWeak bg-fillWeaker/50 text-xs font-mono whitespace-pre-wrap text-textWeak max-h-96 overflow-y-auto"] $ toHtml systemPrompt

-- After (3 lines):
      formSection_ "System Prompt" (Just "terminal") Nothing True False [class_ "mb-4"] do
        div_ [class_ "px-4 pb-4 text-xs font-mono whitespace-pre-wrap text-textWeak max-h-96 overflow-y-auto"] $
          toHtml systemPrompt
```
**-2 lines**. Visual change: gains card-style appearance (bg-bgBase, rounded-xl) instead of lightweight border.

### 8c. AI Chat — Tool Calls (lines 642-646)

```haskell
-- Before (5 lines):
        $ details_ [class_ "mb-4 border border-strokeWeak rounded-lg"] do
          summary_ [class_ "cursor-pointer px-4 py-2 text-sm text-textWeak hover:bg-fillWeaker list-none flex items-center gap-2"] do
            faSprite_ "magnifying-glass-chart" "regular" "w-4 h-4"
            toHtml $ "Behind the scenes: " <> show (length toolCalls) <> " tool calls"
          div_ [class_ "px-4 py-3 border-t border-strokeWeak bg-fillWeaker/50"] $ forM_ toolCalls toolCallView_

-- After (3 lines):
        $ formSection_ ("Behind the scenes: " <> show (length toolCalls) <> " tool calls") (Just "magnifying-glass-chart") Nothing True False [class_ "mb-4"] do
          div_ [class_ "px-4 pb-4"] $ forM_ toolCalls toolCallView_
```
**-2 lines**

### 8d. Chat History — System Prompt (lines 720-724)

```haskell
-- Before (5 lines):
    details_ [class_ ""] do
      summary_ [class_ "cursor-pointer flex items-center gap-2 text-sm font-medium text-textStrong hover:text-textBrand transition-colors"] do
        faSprite_ "file-text" "regular" "w-4 h-4 text-iconInformation"
        "System Prompt"
      div_ [class_ "mt-3 text-xs font-mono whitespace-pre-wrap text-textWeak max-h-96 overflow-y-auto border border-strokeWeak rounded-lg p-4 bg-fillWeaker/50"] $ toHtml systemPrompt

-- After (3 lines):
    formSection_ "System Prompt" (Just "file-text") Nothing True False [] do
      div_ [class_ "px-4 pb-4 text-xs font-mono whitespace-pre-wrap text-textWeak max-h-96 overflow-y-auto"] $
        toHtml systemPrompt
```
**-2 lines**

### 8e. Payload Changes (lines 1045-1049)

```haskell
-- Before (5 lines):
        details_ [class_ "group mb-4"] do
          summary_ [class_ "inline-flex items-center cursor-pointer whitespace-nowrap text-sm font-medium transition-all rounded-md gap-1.5 text-textBrand hover:text-textBrand/80 list-none"] do
            faSprite_ "chevron-right" "regular" "h-4 w-4 mr-1 transition-transform group-open:rotate-90"
            "View detailed payload changes"
          div_ [class_ "mt-4 border border-strokeWeak rounded-lg overflow-hidden bg-bgRaised"] do

-- After (3 lines):
        formSection_ "View detailed payload changes" Nothing Nothing True False [class_ "mb-4"] do
          div_ [class_ "p-4"] do
```
**-2 lines**. Visual change: gains card-style panel instead of branded inline link.

### 8f. Stack Trace (lines 1026-1031)

```haskell
-- Before (6 lines):
        div_ [class_ "border border-strokeError-weak rounded-lg group/er mb-4"] do
          label_ [class_ "text-sm text-textWeak font semibold rounded-lg p-2 flex gap-2 items-center"] do
            faSprite_ "chevron-right" "regular" "h-3 w-3 group-has-[.err-input:checked]/er:rotate-90"
            "Stack trace"
            input_ [class_ "err-input w-0 h-0 opacity-0", type_ "checkbox"]
          div_ [class_ "bg-fillError-weak p-4 overflow-x-scroll hidden group-has-[.err-input:checked]/er:block text-sm monospace text-fillError-strong"] $ pre_ [class_ "whitespace-pre-wrap "] $ toHtml exceptionData.stackTrace

-- After (3 lines):
        formSection_ "Stack trace" Nothing Nothing True False [class_ "mb-4"] do
          div_ [class_ "p-4 overflow-x-scroll text-sm monospace"] $
            pre_ [class_ "whitespace-pre-wrap"] $ toHtml exceptionData.stackTrace
```
**-3 lines**. Visual change: gains standard card-style, loses error-themed colors.

### Anomalies.hs total: -10 lines

---

## Phase 9: LogExplorer/Log.hs

**File:** `src/Pages/LogExplorer/Log.hs` (1097 lines)

### 9a. Add import

```haskell
import Pages.Components (formSection_, formField_)
```
**+1 line**

### 9b. Facet section toggle (lines 230-240)

```haskell
-- Before (6 lines of chrome):
    div_ [class_ "facet-section-group group/section block contain-[layout_style]"] do
      input_ $ [type_ "checkbox", class_ "hidden peer", id_ $ "toggle-" <> T.replace " " "-" sectionName] ++ [checked_ | not collapsed]
      label_ [class_ "p-2 bg-fillWeak rounded-lg cursor-pointer flex gap-2 items-center peer-checked:[&>svg]:rotate-0", Lucid.for_ $ "toggle-" <> T.replace " " "-" sectionName] do
        faSprite_ "chevron-down" "regular" "w-3 h-3 transition-transform -rotate-90"
        span_ [class_ "font-medium text-sm"] (toHtml sectionName)
      div_ [class_ "facets-container mt-1 max-h-0 overflow-hidden peer-checked:max-h-[2000px] transition-[max-height] duration-300"] do

-- After (2 lines):
    formSection_ sectionName Nothing Nothing True (not collapsed) [class_ "facet-section-group"] do
      div_ [class_ "facets-container mt-1"] do
```
**-4 lines**. Note: loses the smooth max-height animation (300ms). `<details>` toggle is instant. This is a UX change, but unifies the pattern.

### 9c. Alert form fieldset (lines 967-976)

```haskell
-- Before (4 lines):
          fieldset_ [class_ "fieldset"] do
            label_ [class_ "label text-xs font-medium text-textStrong mb-1"] "Alert name"
            input_ [type_ "text", name_ "title", value_ $ maybe "" (\x -> x.alertConfig.title) alertM,
                    placeholder_ "e.g. High error rate on checkout API", class_ "input input-sm w-full", required_ ""]

-- After (3 lines):
          formField_ "alert-title" "Alert name" $
            input_ [type_ "text", name_ "title", id_ "alert-title", value_ $ maybe "" (\x -> x.alertConfig.title) alertM,
                    placeholder_ "e.g. High error rate on checkout API", class_ "input input-sm w-full", required_ ""]
```
**-1 line**

### Log.hs total: -4 lines

---

## Phase 10: Onboarding.hs

**File:** `src/Pages/Onboarding/Onboarding.hs` (1031 lines)

### 10a. Add import

```haskell
import Pages.Components (connectionField_, formSection_, formField_)
```
**+1 line**

### 10b. Delete `formField` helper (lines 782-789)

```haskell
-- DELETE:
formField :: Text -> Text -> Text -> Text -> Text -> Html ()
formField labelText inputType inputName inputId inputValue = do
  div_ [class_ "flex flex-col gap-2"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ "text-textStrong lowercase first-letter:uppercase"] $ toHtml labelText
    if inputType == "textarea"
      then textarea_ [class_ "textarea w-full rounded-lg border border-strokeStrong", type_ "text", name_ inputName, id_ inputId] ""
      else input_ [class_ "input w-full h-12", type_ inputType, name_ inputName, id_ inputId, value_ inputValue]
```
**-8 lines**

### 10c. Replace `formField` call sites (lines 813-814)

```haskell
-- Before:
              formField "Notify phone number" "text" "phoneNumber" "phone" phone
              formField "Notify the following email address" "textarea" "emails" "emails_input" ""

-- After:
              connectionField_ "Notify phone number" "phoneNumber" False phone False "Phone number"
              formField_ "emails_input" "Notify the following email addresses" $
                textarea_ [class_ "textarea w-full min-h-12 resize-none", name_ "emails", id_ "emails_input"] ""
```
**0 net lines** (same count). Visual change: gets standard DaisyUI fieldset styling.

### 10d. Restyle `createInputField` (lines 958-964)

```haskell
-- Before (7 lines):
createInputField :: (Text, Text) -> Html ()
createInputField (labelText, value) = do
  div_ [class_ "flex flex-col gap-1 w-full"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ " text-textStrong lowercase first-letter:uppercase"] (toHtml labelText)
      span_ [class_ " text-textWeak"] "*"
    input_ [class_ "input w-full h-12", type_ "text", name_ $ T.replace " " "" labelText, required_ "required", value_ value]

-- After (2 lines):
createInputField :: (Text, Text) -> Html ()
createInputField (labelText, value) =
  connectionField_ labelText (T.replace " " "" labelText) True value False labelText
```
**-5 lines**. Visual change: standard DaisyUI fieldset, standard input size.

### 10e. Restyle `createSelectField` (lines 967-975)

```haskell
-- Before (9 lines):
createSelectField :: Text -> Text -> V.Vector (Text, Text) -> Html ()
createSelectField val labelText options = do
  div_ [class_ "flex flex-col gap-1 w-full"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ " text-textStrong lowercase first-letter:uppercase"] $ toHtml labelText
      span_ [class_ " text-textWeak"] "*"
    select_ [class_ "select w-full h-12", name_ $ T.replace " " "" labelText, required_ "required"] do
      option_ [value_ ""] ""
      forM_ options $ \(key, value) -> option_ (value_ key : [selected_ val | val == key]) $ toHtml value

-- After (5 lines):
createSelectField :: Text -> Text -> V.Vector (Text, Text) -> Html ()
createSelectField val labelText options =
  formField_ (T.replace " " "" labelText) labelText $
    select_ [class_ "select w-full", name_ $ T.replace " " "" labelText, required_ "required"] do
      option_ [value_ ""] ""
      forM_ options $ \(key, value) -> option_ (value_ key : [selected_ val | val == key]) $ toHtml value
```
**-4 lines**

### 10f. Replace `faQ` (lines 1004-1010)

```haskell
-- Before (7 lines):
faQ :: Text -> Text -> Html ()
faQ question answer =
  div_ [class_ "w-full py-5 flex flex-col group/faq"] $ do
    button_ [class_ "text-textStrong  flex w-full justify-between items-center hover:text-textStrong cursor-pointer", [__|on click toggle .hidden on the next <div/> then toggle .rotate-180 on <svg/> in me|]] do
      span_ [class_ "text-left pr-4 leading-normal"] $ toHtml question
      faSprite_ "chevron-down" "regular" "h-4 w-4 text-textWeak group-hover/faq:text-textStrong transition-transform duration-200"
    div_ [class_ "text-textWeak font-medium w-full hidden pt-4 leading-relaxed"] $ toHtml answer

-- After (3 lines):
faQ :: Text -> Text -> Html ()
faQ question answer =
  formSection_ question Nothing Nothing True False [] $
    p_ [class_ "px-4 pb-4 text-textWeak font-medium leading-relaxed"] $ toHtml answer
```
**-4 lines**. Visual change: FAQs get card-style appearance with border.

### Onboarding.hs total: -20 lines

---

## Phase 11: Delete unused Form.hs

**File:** `src/Pkg/Components/Form.hs` (360 lines)

Delete the entire file. Zero imports across the entire codebase. Also remove from cabal/package file if listed.

**-360 lines**

---

## Change Ledger

| File | Before | Delta | After |
|------|:------:|:-----:|:-----:|
| `Pages/Components.hs` | 625 | **+67** | 692 |
| `Pkg/Components/PageHeader.hs` | 105 | **-20** | 85 |
| `Pages/Monitors.hs` | 518 | **-31** | 487 |
| `Pages/S3.hs` | 139 | **-14** | 125 |
| `Pages/GitSync.hs` | 556 | **-14** | 542 |
| `Pages/Projects.hs` | 1756 | **-11** | 1745 |
| `Pages/Dashboards.hs` | 2533 | **-4** | 2529 |
| `Pages/Anomalies.hs` | 1301 | **-10** | 1291 |
| `Pages/LogExplorer/Log.hs` | 1097 | **-4** | 1093 |
| `Pages/Onboarding/Onboarding.hs` | 1031 | **-20** | 1011 |
| `Pkg/Components/Form.hs` | 360 | **-360** | 0 |
| **Total** | **10021** | **-421** | **9600** |

---

## What this consolidates

| Before | Count | Becomes |
|--------|:-----:|---------|
| `collapsibleSection` (PageHeader.hs) | 1 def, 0 uses | deleted, replaced by `formSection_` |
| `collapsibleSection_` (Monitors.hs duplicate) | 1 def, 0 external uses | deleted |
| `teamSection_` (Projects.hs local) | 1 def, 3 uses | → `formSection_ ... False` |
| Monitors checkbox-peer panels | 3 instances | → `formSection_ ... True` |
| Anomalies `details_/summary_` blocks | 5 instances | → `formSection_ ... True` |
| Log.hs facet section toggle | 1 instance | → `formSection_ ... True` |
| Onboarding `faQ` | 1 def, 3 uses | → `formSection_ ... True` |
| `connectionField` (S3.hs) | 1 def, 5 uses | → `connectionField_` |
| `gitField` (GitSync.hs) | 1 def, 10 uses | → `connectionField_` |
| `field_` (Projects.hs local) | 1 def, 4 uses | → `formField_` |
| `fieldIcon_` (Projects.hs local) | 1 def, 4 uses | → `formFieldIcon_` |
| `tagInput_` (Projects.hs local) | 1 def, 5 uses | → `tagInput_` (shared) |
| `formField` (Onboarding.hs) | 1 def, 2 uses | → `connectionField_` |
| `createInputField` styling | 1 def | → restyled via `connectionField_` |
| `createSelectField` styling | 1 def | → restyled via `formField_` |
| S3 confirm modal | 12 lines | → `confirmModal_` |
| GitSync confirm modal | 12 lines | → `confirmModal_` |
| Dashboards modal footers | 2 instances | → `formActionsModal_` |
| Projects modal footer | 1 instance | → `formActionsModal_` |
| `Pkg/Components/Form.hs` | 360 lines, 0 uses | deleted entirely |

**13 local/duplicate definitions eliminated. 1 dead module deleted. 6 shared helpers in one file.**

---

## Execution Order

1. **Phase 1** — Add helpers to Components.hs (no deps)
2. **Phase 2** — Delete from PageHeader.hs (no deps)
3. **Phases 3-10** — All page updates (depend on Phase 1, independent of each other — parallelizable)
4. **Phase 11** — Delete Form.hs (independent, do last)

---

## Risk Assessment

- **Monitors `#thresholds` hyperscript**: Must update `add/remove .hidden` → `set .open to true/false`. Exact line identified (409).
- **Log.hs facet animation**: Loses 300ms smooth `max-height` transition. `<details>` toggle is instant. This is the biggest UX change.
- **Anomalies visual changes**: AI chat disclosures and stack trace get card-style appearance. Previously lightweight. User approved unification.
- **Onboarding field sizing**: Inputs go from `h-12` (tall) to standard DaisyUI size. Labels go from custom `text-textStrong lowercase first-letter:uppercase` to standard `label text-xs font-medium`. User approved unification.
- **FAQ appearance**: Accordion items get card borders. Previously borderless text toggles.
- **Form.hs deletion**: Zero imports confirmed. Zero risk.
