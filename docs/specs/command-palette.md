# Command Palette Specification

## Overview

A global command palette (Cmd+K) for Monoscope — inspired by Raycast — that lets users search across all entities (pages, issues, logs, monitors, dashboards, members, settings), execute quick actions, and ask AI-powered natural language questions. Built with HTMX + Hyperscript, matching the existing variable picker modal pattern.

## Trigger & Activation

- **Keyboard shortcut**: `Cmd+K` (macOS) / `Ctrl+K` (Linux/Windows)
- **Clickable icon**: Persistent search/command icon in the top nav bar
- **Shortcut renders**: A global Hyperscript listener on `<body>` intercepts the keydown and injects/shows the palette modal

## Scope

- **Project-scoped**: All searches and actions operate within the current active project (`projectId` from URL context)
- The palette knows the current `projectId` and passes it to all search endpoints

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
│  Tab: switch category   ?: AI mode          │
└─────────────────────────────────────────────┘
```

## Search Behavior

### Categorized Sections

Results are grouped under category headers, displayed in this order:

1. **Recent** (shown on empty query from localStorage)
2. **Pages** — static navigation targets (Log Explorer, Dashboards list, Monitors, Settings, etc.)
3. **Dashboards** — individual dashboards by name
4. **Issues / Anomalies** — search by title, error message, group key
5. **Monitors** — search by monitor name
6. **Logs** — jump to log explorer with the query pre-filled
7. **Actions** — quick actions (see below)
8. **AI** — natural language intent detected, shows "Ask AI: {query}" option

Each section shows up to **5 results** with a "Show all →" link if more exist.

### Client-Side Filtering (Pages & Actions)

Pages and actions are a static known list rendered into the HTML. Hyperscript filters them client-side on input (same as variable picker).

### Server-Side Search (Issues, Monitors, Dashboards, Logs)

- Debounced HTMX requests (300ms) triggered on input via `hx-trigger="input changed delay:300ms"`
- Endpoint: `GET /p/{projectId}/command-palette/search?q={query}`
- Server returns pre-rendered HTML fragments grouped by category
- Results replace the dynamic results container via `hx-target`

### AI Intent Auto-Detection

The server-side search endpoint analyzes the query:
- If it looks like a **natural language question** (starts with who/what/why/how, contains "?", longer phrase), include an AI suggestion row: _"Ask AI: {query}"_
- Selecting it routes to the existing `aiSearchH` endpoint and redirects to Log Explorer with the generated query

### Recents & Frequently Used

- **Storage**: `localStorage` key `monoscope:cmd-palette:recents:{projectId}`
- **Structure**: Array of `{ type, label, url, icon, timestamp }` objects, max 20 entries
- **Display**: On empty query, show last 10 items sorted by recency
- **Tracking**: Every time a palette result is selected, push to recents via Hyperscript

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

Actions are rendered server-side as hidden `<a>` or `<button>` elements and filtered client-side by Hyperscript.

## Keyboard Navigation

Matches the variable picker exactly:

| Key | Action |
|-----|--------|
| `↑` / `↓` | Move through visible results (skip hidden, skip section headers) |
| `Enter` | Activate the highlighted result (navigate or execute action) |
| `Escape` | Close the palette |
| `Cmd+K` / `Ctrl+K` | Toggle palette open/close |
| `Tab` | Cycle focus between category sections |

## Architecture

### Backend (Haskell)

**New module**: `src/Pages/CommandPalette.hs`

```haskell
-- Endpoints:
-- GET  /p/{pid}/command-palette          → Render the palette modal HTML
-- GET  /p/{pid}/command-palette/search   → Search results fragment (HTMX partial)

commandPaletteH :: ProjectId -> ATAuthCtx (Html ())
commandPaletteSearchH :: ProjectId -> Maybe Text -> ATAuthCtx (Html ())
```

**Search endpoint logic** (`commandPaletteSearchH`):
1. Receive query `q`
2. In parallel, query:
   - `Anomalies` table (title/message ILIKE)
   - `Monitors` table (name ILIKE)
   - `Dashboards` table (title ILIKE)
3. Client-side: Pages and Actions are already in the DOM, filtered by Hyperscript
4. Check if query looks like natural language → include AI suggestion row
5. Return HTML fragment with categorized sections

### Frontend (Hyperscript + HTMX)

**Global listener** (added to base layout / `src/View/Layout.hs`):

```hyperscript
on keydown[key=='k' and (metaKey or ctrlKey)] from window
  halt the event
  if <#command-palette/> exists
    remove <.cmd-palette-backdrop/>
  else
    fetch /p/{pid}/command-palette then put it into the body
  end
end
```

**Nav bar icon** (in top nav):
```html
<button class="btn btn-ghost btn-sm gap-2"
        hx-get="/p/{pid}/command-palette"
        hx-target="body" hx-swap="beforeend">
  <svg><!-- search icon --></svg>
  <kbd class="kbd kbd-xs">⌘K</kbd>
</button>
```

**Search input** (inside palette modal):
```html
<input type="text"
       hx-get="/p/{pid}/command-palette/search"
       hx-trigger="input changed delay:300ms, search"
       hx-target="#cmd-palette-results"
       hx-swap="innerHTML"
       name="q"
       ...hyperscript for keyboard nav, client-side filtering of pages/actions...>
```

### Routing

Add to `src/Web/Routes.hs`:

```haskell
type CommandPaletteAPI =
  "command-palette" :> Get '[HTML] (Html ())
  :<|> "command-palette" :> "search" :> QueryParam "q" Text :> Get '[HTML] (Html ())
```

Nested under the existing `ProjectRoutes` so it inherits `/p/{projectId}/` prefix.

## Styling

- **Backdrop**: `fixed inset-0 bg-black/40 z-[99999]` — same as variable picker
- **Panel**: `w-full max-w-2xl bg-base-100 rounded-xl shadow-2xl border border-base-300`
- **Positioned**: `pt-[12vh]` from top, centered horizontally
- **Results**: `max-h-96 overflow-y-auto`
- **Active item**: Highlighted with `bg-base-200` (`.active` class toggled by Hyperscript)
- **Category headers**: `text-2xs font-medium uppercase tracking-wider text-base-content/40`
- **Category badges**: Small `badge badge-ghost badge-xs` on each result row
- **Dark mode**: Inherits DaisyUI theme automatically

## Implementation Phases

### Phase 1 — Core Shell
- Palette modal with open/close (Cmd+K + nav icon)
- Static page navigation (client-side filtered)
- Static quick actions
- Keyboard navigation (↑↓, Enter, Esc)
- Styling matching variable picker

### Phase 2 — Server Search
- `/command-palette/search` endpoint
- Search issues, monitors, dashboards
- Debounced HTMX requests
- Categorized result sections

### Phase 3 — Recents & AI
- localStorage recents tracking
- Recent items shown on empty query
- AI intent auto-detection
- Integration with existing `aiSearchH` endpoint
- "Ask AI" result row

### Phase 4 — Polish
- Animations (fade in/out, slide)
- Result count per category
- "Show all →" overflow links
- Frequency tracking for sorting
- Keyboard shortcut hints in nav bar tooltip
