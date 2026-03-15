# Command Palette Specification

## Overview

A global command palette (Cmd+K) for Monoscope — inspired by Raycast — that lets users search across all entities (pages, issues, logs, monitors, dashboards, members, settings), execute quick actions, and ask AI-powered natural language questions. Built with HTMX + Hyperscript, matching the existing variable picker modal pattern.

## Trigger & Activation

- **Keyboard shortcut**: `Cmd+K` (macOS) / `Ctrl+K` (Linux/Windows)
- **Clickable icon**: Persistent search/command icon in the top nav bar
- **Shortcut renders**: A global Hyperscript listener on `<body>` intercepts the keydown and opens the palette via HTMX fetch

### Non-Project Pages

The palette is **only available when a project is active** (i.e., `BWConfig.currProject` is `Just project`). On pages without a project context (e.g., `/projects` list, onboarding), the `Cmd+K` listener is not rendered and the nav icon is hidden.

## Scope

- **Project-scoped**: All searches and actions operate within the current active project
- The palette URL is rendered server-side into the nav bar button's `hx-get` attribute with the `projectId` already baked in — no client-side `projectId` resolution needed
- The `Cmd+K` Hyperscript listener reads the URL from the nav button's `hx-get` rather than constructing it client-side

## UI Structure

Follows the same pattern as `variablePickerModal_` in `Dashboards.hs`:

```
┌─────────────────────────────────────────────┐
│  ⌘  Monoscope Command Palette               │
├─────────────────────────────────────────────┤
│  🔍 Search pages, issues, actions…          │
├─────────────────────────────────────────────┤
│                                             │
│  RECENT                                     │
│  ○ Log Explorer                      Page   │
│  ○ AUTH-1234: Login timeout          Issue   │
│  ○ Latency > 500ms                   Monitor │
│                                             │
│  ─── (after typing) ───                     │
│                                             │
│  PAGES                                      │
│  ○ Log Explorer                             │
│  ○ Dashboards > API Overview                │
│                                             │
│  ISSUES                                     │
│  ○ AUTH-1234: Login timeout error           │
│  ○ AUTH-1201: Session expiry bug            │
│                                             │
│  MONITORS                                   │
│  ○ High Error Rate (production)             │
│                                             │
│  ACTIONS                                    │
│  ○ Create new monitor                       │
│  ○ Switch project                           │
│                                             │
│  AI                                         │
│  ○ Ask: "what caused the spike at 2pm?"     │
│                                             │
├─────────────────────────────────────────────┤
│  ↑↓ Navigate   ↵ Select   esc Close        │
└─────────────────────────────────────────────┘
```

## Search Behavior

### Data Loading Strategy

All searchable data is loaded **server-side when the palette opens** and filtered **client-side** via Hyperscript. This avoids per-keystroke round-trips and provides instant filtering.

When the palette is opened (`GET /p/{pid}/command-palette`), the server renders the full modal HTML including:
- Recent items (from DB, up to 10)
- All static pages and actions
- Up to 50 most recent **issues** (from `apis.issues` — uses `title` column)
- Up to 50 most recent **monitors** (from `monitors.query_monitors` — uses `alert_config->>'title'` from the JSONB `alert_config` column, since monitors have no top-level `name` field)
- All **dashboards** (from `projects.dashboards` — uses `title` column; typically a small set per project)

All items are rendered as DOM elements with a `data-search` attribute containing the searchable text (lowercased). Hyperscript filters by checking `dataset.search.contains(query)` rather than `textContent`, so badges and category labels don't interfere with search matching.

For projects with more than 50 issues/monitors, a "Show all →" link navigates to the full list page with the search query pre-filled as a URL parameter.

### Categorized Sections

Results are grouped under category headers, displayed in this order:

1. **Recent** (shown on empty query, hidden when typing)
2. **Pages** — static navigation targets (Log Explorer, Dashboards list, Monitors, Settings, etc.)
3. **Dashboards** — individual dashboards by name
4. **Issues** — search by title
5. **Monitors** — search by alert title
6. **Logs** — always-visible shortcut: "Search logs for: {query}" → navigates to Log Explorer with query pre-filled
7. **Actions** — quick actions (see below)
8. **AI** — always shown as last item when query length > 15 characters, labeled "Ask AI: {query}"

Each section shows up to **5 visible results** with a "Show all →" link if more match.

### Client-Side Filtering

All filtering happens client-side via Hyperscript on the search input, following the same pattern as `variablePickerModal_` in `Dashboards.hs` (line 641):

```hyperscript
on input
  set :q to my value.toLowerCase()
  -- Hide recents section when typing, show when empty
  if :q.length > 0 then hide <.cmd-palette-recents/> else show <.cmd-palette-recents/> end
  -- Filter all result items by data-search attribute
  show <.cmd-item/> in closest .cmd-palette when its dataset.search contains :q
  -- Update active highlight (same pattern as variable picker)
  for item in <a.cmd-item/> in closest .cmd-palette remove .active from item end
  set :first to the first <a.cmd-item:not([style*='display: none'])/> in closest .cmd-palette
  if :first then add .active to :first end
  -- Show/hide section headers based on whether they have visible children
  for section in <.cmd-section/> in closest .cmd-palette
    set :visible to <.cmd-item:not([style*='display: none'])/> in section
    if :visible.length === 0 then hide section else show section end
  end
  -- Show/hide AI row based on query length
  if :q.length > 15 then show <.cmd-ai-row/> else hide <.cmd-ai-row/> end
  -- Update AI row text dynamically
  set :aiLabel to the first <.cmd-ai-label/>
  if :aiLabel then put `Ask AI: "${my value}"` into :aiLabel end
  -- Update Log Explorer shortcut text
  set :logLabel to the first <.cmd-log-label/>
  if :logLabel then put `Search logs for: "${my value}"` into :logLabel end
  -- Update empty state
  set :allVisible to <.cmd-item:not([style*='display: none'])/> in closest .cmd-palette
  set :empty to the first <.cmd-palette-empty/> in closest .cmd-palette
  if :allVisible.length === 0 and :q.length > 0 then show :empty else hide :empty end
end
```

### AI Suggestion

- Always shown as the last item when query length exceeds 15 characters
- Labeled "Ask AI: {query}" — no intent detection heuristics
- Selecting it sends a `POST` to the existing `aiSearchH` endpoint (`/p/{pid}/log_explorer/ai_search`) with `{"input": query}` as JSON body
- The response contains `{ query, visualization_type, commentary, time_range }` — the palette JS should navigate to Log Explorer with the returned `query` as the filter parameter

**Note**: `aiSearchH` is a `POST` endpoint that takes `ReqBody '[JSON] AE.Value`, not a GET. The AI result row will need a small inline `<script>` or Hyperscript `fetch` to POST the JSON and redirect based on the response.

### Recents

- **Storage**: Database table `apis.command_palette_recents` (in the `apis` schema, following convention of project-scoped operational data)
  - Columns: `id`, `project_id`, `user_id`, `item_type`, `label`, `url`, `created_at`
  - `UNIQUE (project_id, user_id, url)` for upsert dedup
  - Max 20 entries per user per project (oldest pruned on insert via `DELETE` subquery)
- **Display**: On empty query, show last 10 items sorted by recency
- **Tracking**: Each palette result link includes `hx-post` + `hx-vals` for fire-and-forget recording:
  ```html
  <a class="cmd-item" href="/p/{pid}/issues/123"
     hx-post="/p/{pid}/command-palette/recents"
     hx-vals='{"label":"AUTH-1234: Login timeout","url":"/p/{pid}/issues/123","item_type":"issue"}'
     hx-trigger="click"
     hx-swap="none">
  ```
- **Cleanup**: On palette open, the server returns recents from DB — no client-side state needed

## Quick Actions

Static actions available in the palette:

| Action | Description | Target |
|--------|-------------|--------|
| Create monitor | Navigate to new monitor form | `/p/{pid}/monitors/new` |
| Create dashboard | Navigate to new dashboard form | `/p/{pid}/dashboards/new` |
| Go to Log Explorer | Open log explorer | `/p/{pid}/log_explorer` |
| Go to Settings | Open project settings | `/p/{pid}/settings` |
| Copy current URL | Copy page URL to clipboard | JS clipboard API |
| Switch project | Show project switcher | `/projects` |
| Manage team | Open team settings | `/p/{pid}/settings/team` |
| View API keys | Open API keys page | `/p/{pid}/settings/api-keys` |

Actions are rendered server-side and filtered client-side by Hyperscript (same as all other items).

## Keyboard Navigation

Matches the variable picker pattern (see `Dashboards.hs` lines 655-681):

| Key | Action |
|-----|--------|
| `↑` / `↓` | Move through visible results (skip hidden items, skip section headers). Uses `nextElementSibling`/`previousElementSibling` with `style.display === 'none'` skip loop, matching the variable picker exactly. |
| `Enter` | Activate the highlighted result — calls `.click()` on the `.active` element |
| `Escape` | Close the palette — removes the backdrop element |
| `Cmd+K` / `Ctrl+K` | Toggle palette open/close |

The keyboard nav Hyperscript goes on the `<input>` element (not the backdrop), matching the variable picker pattern where the input owns all keyboard handlers.

## Architecture

### Backend (Haskell)

**New module**: `src/Pages/CommandPalette.hs`

```haskell
module Pages.CommandPalette where

-- | Form type for recording recent selections
data RecentForm = RecentForm
  { label :: Text
  , url :: Text
  , itemType :: Text  -- "page", "issue", "monitor", "dashboard", "action"
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)

-- | GET /p/{pid}/command-palette
-- Renders the full palette modal with all searchable data pre-loaded.
-- Access current user via: authCtx <- ask @AuthContext; let userId = authCtx.session.persistentSession.user.getUser.id
commandPaletteH :: ProjectId -> ATAuthCtx (RespHeaders (Html ()))
commandPaletteH pid = do
  authCtx <- ask @AuthContext
  let userId = authCtx.session.persistentSession.user.getUser.id
  -- In parallel (using Async or Effectful concurrency):
  -- 1. SELECT from apis.command_palette_recents WHERE project_id = pid AND user_id = userId ORDER BY created_at DESC LIMIT 10
  -- 2. SELECT id, title, seq_num FROM apis.issues WHERE project_id = pid AND archived_at IS NULL ORDER BY created_at DESC LIMIT 50
  -- 3. SELECT id, alert_config->>'title' as title FROM monitors.query_monitors WHERE project_id = pid AND deleted_at IS NULL ORDER BY created_at DESC LIMIT 50
  -- 4. SELECT id, title FROM projects.dashboards WHERE project_id = pid ORDER BY title
  -- Render full modal HTML
  addRespHeaders $ renderPalette pid recents issues monitors dashboards

-- | POST /p/{pid}/command-palette/recents
-- Records a recent selection. Upserts by (project_id, user_id, url).
commandPaletteRecentPost :: ProjectId -> RecentForm -> ATAuthCtx (RespHeaders NoContent)
commandPaletteRecentPost pid form = do
  authCtx <- ask @AuthContext
  let userId = authCtx.session.persistentSession.user.getUser.id
  -- INSERT INTO apis.command_palette_recents (project_id, user_id, item_type, label, url)
  -- VALUES (pid, userId, form.itemType, form.label, form.url)
  -- ON CONFLICT (project_id, user_id, url) DO UPDATE SET created_at = now(), label = EXCLUDED.label
  -- Then prune: DELETE FROM apis.command_palette_recents WHERE id NOT IN (SELECT id FROM ... ORDER BY created_at DESC LIMIT 20)
  addRespHeaders NoContent
```

**Key codebase patterns to follow**:
- Use `ATAuthCtx` monad (same as all handlers in the codebase)
- Get auth context via `ask @AuthContext` (see `LogExplorer/Log.hs:1060`)
- Return `RespHeaders (Html ())` for HTML endpoints, `RespHeaders NoContent` for fire-and-forget
- Use `addRespHeaders` to wrap the response (standard pattern)
- Use `Entity` deriving for DB model types with `GenericEntity` (see `Models/Apis/Issues.hs:263`)

### Frontend (Hyperscript + HTMX)

**Global listener** (added to `src/Pages/BodyWrapper.hs`, in the `body_` block after `sideNav'` when `bcfg.currProject` is `Just`):

Rather than adding a `data-project-id` to `<body>`, the Cmd+K listener reads the palette URL directly from the nav bar button:

```hyperscript
on keydown[key=='k' and (metaKey or ctrlKey)] from window
  halt the event
  if <.cmd-palette-backdrop/> exists
    remove <.cmd-palette-backdrop/>
  else
    set :btn to the first <#cmd-palette-trigger/>
    if :btn
      fetch :btn.getAttribute('data-palette-url') then put it afterbegin of the body
    end
  end
end
```

This approach avoids adding a `data-project-id` to `<body>` and works because the nav button already has the correct project-scoped URL rendered server-side.

**Nav bar button** (added to `navbar` function in `BodyWrapper.hs`):
```haskell
button_
  [ id_ "cmd-palette-trigger"
  , class_ "btn btn-ghost btn-sm gap-2"
  , data_ "palette-url" ("/p/" <> pid.toText <> "/command-palette")
  , hxGet_ ("/p/" <> pid.toText <> "/command-palette")
  , hxTarget_ "body"
  , hxSwap_ "afterbegin"
  ] do
    faSprite_ "magnifying-glass" "regular" "w-3.5 h-3.5"
    kbd_ [class_ "kbd kbd-xs"] "⌘K"
```

**Auto-close on navigation**: The palette backdrop removes itself on any HTMX request:
```hyperscript
on htmx:beforeRequest from <body/>
  remove me
end
```

**Palette HTML structure** (rendered by `commandPaletteH`):
```html
<div class="cmd-palette-backdrop fixed inset-0 flex flex-col items-center pt-[12vh] bg-black/40"
     style="z-index:99999"
     _="on click if event.target is me remove me
        on htmx:beforeRequest from <body/> remove me">

  <div class="cmd-palette w-full max-w-2xl bg-base-100 rounded-xl shadow-2xl border border-base-300 overflow-hidden"
       _="on click halt the event's bubbling">

    <!-- Search input -->
    <div class="px-3 border-b border-base-300">
      <input type="text"
             class="w-full py-3 bg-transparent outline-none text-sm"
             placeholder="Search pages, issues, actions..."
             autofocus
             _="...filtering + keyboard nav hyperscript...">
    </div>

    <!-- Results container -->
    <div id="cmd-palette-results" class="max-h-96 overflow-y-auto p-1">

      <!-- Recents section (hidden when typing) -->
      <div class="cmd-palette-recents cmd-section">
        <div class="text-2xs font-medium uppercase tracking-wider text-base-content/40 px-3 pt-2 pb-1">Recent</div>
        <a class="cmd-item active flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
           data-search="log explorer" href="/p/{pid}/log_explorer"
           hx-post="/p/{pid}/command-palette/recents" hx-vals='...' hx-trigger="click" hx-swap="none">
          <span class="truncate flex-1">Log Explorer</span>
          <span class="badge badge-ghost badge-xs">Page</span>
        </a>
        <!-- ...more recent items... -->
      </div>

      <!-- Pages section -->
      <div class="cmd-section">
        <div class="text-2xs font-medium uppercase tracking-wider text-base-content/40 px-3 pt-2 pb-1">Pages</div>
        <a class="cmd-item ..." data-search="log explorer" href="/p/{pid}/log_explorer">...</a>
        <a class="cmd-item ..." data-search="dashboards" href="/p/{pid}/dashboards">...</a>
        <!-- ...etc... -->
      </div>

      <!-- Issues section -->
      <div class="cmd-section">
        <div class="text-2xs font-medium uppercase tracking-wider text-base-content/40 px-3 pt-2 pb-1">Issues</div>
        <a class="cmd-item ..." data-search="auth-1234 login timeout error" href="/p/{pid}/issues/...">
          <span class="truncate flex-1">AUTH-1234: Login timeout error</span>
          <span class="badge badge-ghost badge-xs">Issue</span>
        </a>
        <!-- ...up to 50 issues, only first 5 visible initially... -->
      </div>

      <!-- ...Monitors, Actions sections... -->

      <!-- AI row (hidden initially, shown when query > 15 chars) -->
      <div class="cmd-ai-row cmd-section" style="display:none">
        <a class="cmd-item ..." _="on click ...POST to aiSearchH and redirect...">
          <span class="cmd-ai-label truncate flex-1">Ask AI: ""</span>
          <span class="badge badge-ghost badge-xs">AI</span>
        </a>
      </div>

      <!-- Empty state -->
      <div class="cmd-palette-empty px-3 py-8 text-center text-sm text-base-content/40" style="display:none">
        No matching results
      </div>
    </div>

    <!-- Keyboard hints -->
    <div class="flex items-center gap-6 px-3 py-2 border-t border-base-300 text-xs text-base-content/40">
      <div class="flex items-center gap-1.5">Navigate <kbd class="kbd kbd-xs">↑</kbd><kbd class="kbd kbd-xs">↓</kbd></div>
      <div class="flex items-center gap-1.5">Select <kbd class="kbd kbd-xs">↵</kbd></div>
      <div class="flex items-center gap-1.5">Close <kbd class="kbd kbd-xs">esc</kbd></div>
    </div>
  </div>
</div>
```

### Routing

Add fields to `CookieProtectedRoutes` in `src/Web/Routes.hs` (before the sub-route groups section around line 266):

```haskell
  , -- Command palette
    commandPaletteGet :: mode :- "p" :> ProjectId :> "command-palette" :> Get '[HTML] (RespHeaders (Html ()))
  , commandPaletteRecentPost :: mode :- "p" :> ProjectId :> "command-palette" :> "recents" :> ReqBody '[FormUrlEncoded] CommandPalette.RecentForm :> Post '[HTML] (RespHeaders NoContent)
```

Wire handlers in the server function (wherever `CookieProtectedRoutes` handlers are assigned — follow the pattern of existing route wiring).

### Database

**New migration**: `static/migrations/0053_command_palette_recents.sql`

```sql
BEGIN;

CREATE TABLE IF NOT EXISTS apis.command_palette_recents (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  project_id UUID NOT NULL REFERENCES projects.projects(id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users.users(id) ON DELETE CASCADE,
  item_type TEXT NOT NULL,
  label TEXT NOT NULL,
  url TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (project_id, user_id, url)
);

CREATE INDEX IF NOT EXISTS idx_cmd_palette_recents_lookup
  ON apis.command_palette_recents (project_id, user_id, created_at DESC);

COMMIT;
```

Uses `apis` schema (following the pattern of `apis.notification_test_history` and other project-scoped operational tables). References `projects.projects(id)` and `users.users(id)` with full schema-qualified names (required by the existing migration conventions).

## Styling

- **Backdrop**: `fixed inset-0 bg-black/40 z-[99999]` — same as variable picker
- **Panel**: `w-full max-w-2xl bg-base-100 rounded-xl shadow-2xl border border-base-300`
- **Positioned**: `pt-[12vh]` from top, centered horizontally (variable picker uses `pt-[15vh]`, palette is slightly higher since it has more content)
- **Results**: `max-h-96 overflow-y-auto`
- **Active item**: Highlighted with `bg-base-200` (`.active` class toggled by Hyperscript)
- **Category headers**: `text-2xs font-medium uppercase tracking-wider text-base-content/40`
- **Category badges**: Small `badge badge-ghost badge-xs` on each result row
- **Dark mode**: Inherits DaisyUI theme automatically
- **Backdrop dismiss**: Click on backdrop (not panel) closes palette — `on click if event.target is me remove me` (same pattern as variable picker line 630)
- **Bubble stop**: Panel uses `on click halt the event's bubbling` to prevent backdrop dismiss when clicking inside (same as variable picker line 634)

## Implementation Phases

### Phase 1 — Core Shell
- New module `src/Pages/CommandPalette.hs` with `commandPaletteH`
- Route registration in `CookieProtectedRoutes`
- Nav bar button with search icon + `⌘K` badge
- `Cmd+K` global Hyperscript listener in `BodyWrapper.hs` (only when `currProject` is `Just`)
- Palette modal rendering with static pages + actions
- Client-side filtering via Hyperscript
- Keyboard navigation (↑↓, Enter, Esc)
- Auto-close on HTMX navigation
- Styling matching variable picker
- Empty state ("No matching results")

### Phase 2 — Dynamic Data
- Query issues (`apis.issues` — `title`), monitors (`monitors.query_monitors` — `alert_config->>'title'`), dashboards (`projects.dashboards` — `title`) on palette open
- Render up to 50 per category as DOM elements
- Client-side filtering across all categories
- Section show/hide based on visible children count
- "Show all →" overflow links (navigate to list page with search pre-filled)
- "Search logs for: {query}" shortcut row (navigates to Log Explorer)
- AI suggestion row (visible when query > 15 chars)
- AI result handling: POST to `aiSearchH`, redirect to Log Explorer with returned query

### Phase 3 — Recents
- Migration `0053_command_palette_recents.sql`
- `RecentForm` type with `FromForm` instance
- `commandPaletteRecentPost` handler (upsert + prune)
- Recent items section on palette open (empty query state)
- `hx-post` fire-and-forget on each result click

### Phase 4 — Polish
- Animations (fade in/out, slide)
- Result count per section
- Keyboard shortcut hints in nav bar tooltip
- Consider: fuzzy matching (swap `contains` for a simple fuzzy scorer in Hyperscript if exact substring matching feels too strict)
