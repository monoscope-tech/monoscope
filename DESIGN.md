---
name: Monoscope
description: OpenTelemetry-native observability — dense, calm, incident-first. Light-first with full dark parity.
colors:
  brand: "oklch(56.7% 0.239 261)"
  brand-content: "oklch(100% 0 0)"
  bg-base: "oklch(99.1% 0.002 247)"
  bg-sunken: "oklch(97.7% 0.006 247)"
  bg-alternate: "oklch(97.7% 0.006 247)"
  bg-overlay: "oklch(99.1% 0.002 247)"
  bg-inverse: "oklch(16.4% 0.012 263)"
  text-strong: "oklch(14.4% 0.069 263 / 0.9)"
  text-weak: "oklch(16.7% 0.086 263 / 0.65)"
  text-disabled: "oklch(21.2% 0.117 264 / 0.45)"
  stroke-weak: "oklch(25.2% 0.146 264 / 0.1)"
  stroke-strong: "oklch(21.2% 0.117 264 / 0.45)"
  fill-weak: "oklch(29.2% 0.174 264 / 0.04)"
  fill-hover: "oklch(29.2% 0.174 264 / 0.04)"
  error: "oklch(51.3% 0.197 21)"
  warning: "oklch(54.7% 0.126 69)"
  success: "oklch(47.8% 0.130 162)"
  information: "oklch(49.3% 0.088 241)"
typography:
  display:
    fontFamily: "Inter, ui-sans-serif, system-ui, sans-serif"
    fontSize: "1.5rem"
    fontWeight: 600
    lineHeight: "2rem"
    letterSpacing: "-0.01em"
  headline:
    fontFamily: "Inter, ui-sans-serif, system-ui, sans-serif"
    fontSize: "1.125rem"
    fontWeight: 600
    lineHeight: "1.75rem"
    letterSpacing: "normal"
  title:
    fontFamily: "Inter, ui-sans-serif, system-ui, sans-serif"
    fontSize: "1rem"
    fontWeight: 600
    lineHeight: "1.5rem"
    letterSpacing: "normal"
  body:
    fontFamily: "Inter, ui-sans-serif, system-ui, sans-serif"
    fontSize: "0.875rem"
    fontWeight: 400
    lineHeight: "1.25rem"
    letterSpacing: "normal"
    fontFeature: "'liga' 1, 'calt' 1, 'ss01' 1, 'ss02' 1, 'cv01' 1, 'cv02' 1"
  label:
    fontFamily: "Inter, ui-sans-serif, system-ui, sans-serif"
    fontSize: "0.75rem"
    fontWeight: 500
    lineHeight: "1rem"
    letterSpacing: "normal"
  caption:
    fontFamily: "Inter, ui-sans-serif, system-ui, sans-serif"
    fontSize: "0.625rem"
    fontWeight: 500
    lineHeight: "0.875rem"
    letterSpacing: "normal"
  mono:
    fontFamily: "JetBrains Mono, monospace"
    fontSize: "0.875rem"
    fontWeight: 400
    lineHeight: "1.25rem"
    letterSpacing: "normal"
rounded:
  xs: "3px"
  sm: "4px"
  field: "0.4rem"
  md: "8px"
  box: "1rem"
spacing:
  row: "4px"
  tight: "8px"
  base: "16px"
  group: "24px"
components:
  button-primary:
    backgroundColor: "{colors.brand}"
    textColor: "{colors.brand-content}"
    rounded: "{rounded.field}"
    padding: "0.5rem 1rem"
  button-primary-hover:
    backgroundColor: "{colors.brand}"
    textColor: "{colors.brand-content}"
  button-primary-active:
    backgroundColor: "{colors.brand}"
    textColor: "{colors.brand-content}"
  input:
    backgroundColor: "{colors.bg-base}"
    textColor: "{colors.text-strong}"
    rounded: "{rounded.field}"
    padding: "0.375rem 0.625rem"
  badge:
    backgroundColor: "{colors.fill-weak}"
    textColor: "{colors.text-strong}"
    rounded: "{rounded.md}"
    padding: "0.125rem 0.5rem"
---

# Design System: Monoscope

## 1. Overview

**Creative North Star: "The Instrument Panel"**

Monoscope is read like a cockpit, not browsed like a website. Its user is an on-call engineer at 3 AM following a shared trace link with an error clock ticking — so the interface behaves like an instrument panel: every readout carries meaning, every color is a gauge rather than paint, and density is a form of respect. The visual system exists to get someone from an alert to a root cause without a single glance being wasted. The emotional target is relief — the feeling of the system finally telling you what happened — never spectacle.

Density is the load-bearing value. Experienced users scan; the waterfall, the log stream, and the field list are packed tight within rows and spaced generously between logical groups so structure reads at a glance. Color is strictly signal: brand blue means clickable, red means error, green means success, service and status hues identify. Where a color has no meaning, no color is used. The reference point is Datadog APM — information-rich, cross-linked, color-as-meaning — but stripped of its glossier edges. Progressive disclosure earns its keep during incidents: summary counts up front, detail one click away, never the exception type buried two clicks deep in a 29-error trace.

The system is light-first with **full dark parity** — every semantic token is defined in both themes and verified in both. Shared views are first-class: a link may open on a stranger's phone mid-panic, so every surface must read standalone. This system explicitly rejects playful illustrations and mascots, decorative icons above headings, neon-on-dark, glassmorphism as decoration, gradient text, and cartoony empty states.

**Key Characteristics:**
- Instrument-panel density: tight within rows, generous between groups.
- Color is signal, never decoration — brand blue for action, semantic hues for state.
- OKLCH semantic-token system with strict light/dark parity.
- Inter for UI and data; JetBrains Mono for code, IDs, and raw telemetry.
- Tonal surfaces + 1px strokes carry depth; shadows are reserved for floating layers.

## 2. Colors

An OKLCH semantic-token palette organized by role (fill / text / stroke / icon / bg), anchored by a single confident brand blue, with a tightly-controlled semantic set and a categorical series palette for data viz.

### Primary
- **Signal Blue** (`oklch(56.7% 0.239 261)`): The brand color and the one interactive accent. Used for primary buttons, current selection, focus rings, links, and "clickable" affordances only. In dark mode it softens to `oklch(60% 0.18 265)` to avoid glare. Available as `fillBrand-strong` and a 5%-alpha `fillBrand-weak` for tinted backgrounds.

### Secondary
- **Semantic Error Red** (`oklch(51.3% 0.197 21)`): Errors, failed spans, 5xx. Paired always with a non-color signal (icon, position, text).
- **Semantic Warning Amber** (`oklch(54.7% 0.126 69)`): Degraded state, 4xx, elevated latency.
- **Semantic Success Green** (`oklch(47.8% 0.130 162)`): Healthy, resolved, 2xx.
- **Semantic Information Blue** (`oklch(49.3% 0.088 241)`): Informational callouts and the default chart line. **Reconciliation note:** this currently sits at hue 241, a different blue from the brand's 261 — unintentional drift, not a designed second blue. The target is to migrate `fillInformation`/`chart-default` onto the brand hue (261) at an info-weight lightness, so the system carries one blue hue. Until then, treat this hue as legacy.

Each semantic color ships `-strong` (text/icon/border) and `-weak` (~5% alpha fill) variants, and each lifts in lightness for dark mode (e.g. error → `oklch(77.2% 0.126 21)`).

### Neutral
- **Ink** (`oklch(14.4% 0.069 263 / 0.9)`): `textStrong` — primary body and heading text.
- **Muted Ink** (`oklch(16.7% 0.086 263 / 0.65)`): `textWeak` — secondary text, still comfortably above 4.5:1 on base surfaces.
- **Base Surface** (`oklch(99.1% 0.002 247)`): `bgBase`, the near-white content plane, faintly cooled toward the brand hue (never warmed).
- **Sunken / Alternate** (`oklch(97.7% 0.006 247)`): recessed panels, table zebra, toolbars — the second neutral layer.
- **Strokes**: `strokeWeak` (`oklch(25.2% 0.146 264 / 0.1)`) for dividers and field borders; `strokeStrong` for emphasis. Borders are hairline (1px) and tinted toward ink, never pure black.

Dark mode swaps the neutral base to a subtle warm charcoal (`bgBase: oklch(12.5% 0.004 260)`) with clear surface steps (`bgSunken` → `bgBase` → `bgRaised` → `bgOverlay`).

### Named Rules
**The Signal Rule.** Color is signal, not decoration. Brand blue means clickable; red means error; green means success; a service or status hue means identity. If an element carries no meaning, it carries no color — it is neutral ink on a neutral surface.

**The One-Blue Rule.** There is a single blue hue in the system: the brand's 261. It carries interaction at full strength (`fillBrand`) and informational state at a lighter weight (`fillInformation`) — the *same* hue, never a second one. Semantic distinction between "clickable" and "informational" comes from strength and context, not from a rival blue hue. Any new blue that isn't hue 261 is drift; fix the token, don't add a rule for it.

## 3. Typography

**Display / Body Font:** Inter Variable (with `ui-sans-serif, system-ui, sans-serif` fallback)
**Mono Font:** JetBrains Mono (with `monospace` fallback)

**Character:** One humanist sans carries the entire UI — headings, labels, body, and dense data — tuned with Inter's OpenType features (`ss01`, `ss02`, `cv01`, `cv02`, contextual ligatures) for a precise, engineered read. JetBrains Mono is the counter-voice: it appears anywhere the content is machine truth — code, trace/span IDs, raw log lines, JSON, KQL. The pairing contrasts on the humanist-sans / monospace axis, never on two similar sans families.

### The Type Scale
Type steps come from a **fixed rem scale** — Tailwind's default ladder extended with two custom small steps — never a fluid `clamp()`. Product UI is viewed at consistent DPI, so a heading that reflows in a sidebar reads worse, not better. Roles map onto scale steps; the ratio stays tight (~1.125–1.14) because a dense tool has many type elements and exaggerated contrast is noise.

| Step | Size / line-height | Role |
|---|---|---|
| `text-2xl` | 1.5rem / 2rem | **Display** — page titles (weight 600, -0.01em) |
| `text-xl` | 1.25rem / 1.75rem | Large headings, empty-state titles |
| `text-lg` | 1.125rem / 1.75rem | **Headline** — panel/section headers (600) |
| `text-base` | 1rem / 1.5rem | **Title** — card and group headers (600) |
| `text-sm` | 0.875rem / 1.25rem | **Body** — the workhorse: labels, table cells, prose (400) |
| `text-xs` | 0.75rem / 1rem | **Label** — field labels, badges, meta, dense data (500) |
| `text-2xs` | 0.625rem / 0.875rem | **Caption** — micro-labels, chart ticks |
| `text-3xl`–`5xl` | 1.875–3rem | Reserved — rare hero moments only, not app chrome |

**Body is `text-sm` (0.875rem), not `text-base`.** The everyday UI runs one step below the web default because density is the point; `text-base` is for the occasional emphasized lead. Prose caps at 65–75ch; dense tables and data may run to 120ch+.

### Named Rules
**The Machine-Truth Rule.** If the content is something a machine emitted verbatim — a span ID, a status code, a log line, a query — it is set in JetBrains Mono at the same `text-sm`/`text-xs` steps as its neighbours. Human-authored chrome is Inter. The font tells the reader which is which.

**The Fixed-Scale Rule.** Every type size is a named step on the scale above. No fluid `clamp()` headings, no one-off rem values, no separate display typeface. Weight and a chosen scale step create hierarchy; `text-3xl` and larger are reserved for rare hero moments and never appear in app chrome.

## 4. Elevation

Depth is **tonal-first**. Surfaces step through a tonal ramp — `bgSunken` (recessed) → `bgBase` (content) → `bgRaised` → `bgOverlay` — reinforced by hairline 1px strokes, and that tonal layering does most of the structural work. Shadows are reserved for elements that genuinely float above the plane: dropdowns, modals, toasts, popovers. This keeps flat surfaces flat and makes a shadow itself a signal that something is transient and layered.

### Shadow Vocabulary
- **shadow-xs** (`0 1px 2px -1px rgba(0 0 0 / 0.06)`): Badges, subtle chips.
- **shadow-sm** (`0 2px 8px -2px rgba(0 0 0 / 0.08), 0 4px 12px -4px rgba(0 0 0 / 0.04)`): Dropdowns, popovers, raised cards.
- **shadow-md** (`0 4px 12px -2px rgba(0 0 0 / 0.12), 0 8px 20px -4px rgba(0 0 0 / 0.08)`): Modals, command palette.
- **shadow-lg** (`0 8px 25px -5px rgba(0 0 0 / 0.1), 0 4px 10px -5px rgba(0 0 0 / 0.04)`): Highest floating layers.
- **shadow-toast** (`0 4px 12px rgba(0 0 0 / 0.12)`): Toast notifications.

Dark mode deepens every step (up to `rgba(0 0 0 / 0.6)`) so float still reads against charcoal surfaces.

### Named Rules
**The Float-Only Shadow Rule.** A shadow means "this floats above the page." Resting surfaces earn depth from tone and stroke, never from a drop shadow. If a card at rest has a shadow, remove it.

## 5. Components

Components feel **tactile and confident**: standard affordances tuned well, with crisp state feedback and no decoration. Every interactive component defines default, hover, focus, active, disabled, and (where relevant) loading and error. Radii come from the token scale — fields/selectors at `0.4rem`, boxes at `1rem`.

### Buttons
- **Shape:** Field radius (`0.4rem`).
- **Primary:** Signal-blue fill, white content, `0.5rem 1rem` padding. In dark mode, hover drops to `opacity: 0.9` with a `strokeBrand-weak` ring rather than a lighter fill.
- **Press feedback:** All buttons scale to `0.98` on `:active` over 75ms — the tactile signature.
- **Loading:** During HTMX requests the button drops to `0.7` opacity, disables pointer events, and appends a spinning `currentColor` ring — no layout shift, no separate spinner.
- **Secondary / Ghost:** Neutral fill (`fillWeak`) or transparent with a hairline stroke; text in `textStrong` / `textWeak`.

### Badges
- **Solid** (`.badge`): `fillStrong`-family background, inverse text, `shadow-xs`, `rounded-md`, `text-xs font-medium`.
- **Outline / Ghost:** 1px stroke or no fill with `textWeak`. Used for status labels and counts; never as decoration.

### Inputs / Fields
- **Style:** `bgBase` surface, 1px `strokeWeak` border, field radius, `0.375rem 0.625rem` padding.
- **Focus:** Border shifts to `strokeFocus` (Signal Blue) with a matching focus ring — the same blue used for selection, so focus reads as "active."
- **Error / Disabled:** Error uses `strokeError` plus a non-color signal; disabled drops to `fillDisabled` with `textDisabled`.

### Tabs / Navigation
- **Style:** Inactive tabs use `textWeak` on transparent; active state is handled by HTMX morph swaps (no imperative class toggling). Nav tabs preload on hover.
- **Behavior:** Presentational state (open/expand, chevron rotation, selection) is expressed with Tailwind state variants (`peer`/`group-has-[:checked]`), which survive HTMX morph swaps, not hyperscript toggles.

### Data Visualization (signature)
Monoscope's charts are the product's heroes, so color mapping is codified, not ad-hoc:
- **Categorical series:** a fixed 20-step palette of `-400`-weight hues (blue, red, green, amber, purple, teal, orange, sky, rose, lime, …) assigned in order so series stay stable across renders.
- **HTTP status → hue:** 2xx = blues/greens, 3xx = cyans/teals, 4xx = yellows/oranges, 5xx = reds/pinks. Status color is semantic and consistent everywhere a status appears.
- **Percentile → performance gradient:** p50 green → p90 amber → p95 orange → p99/p100 harsh red. Latency severity reads as temperature.

### Motion
State-conveying only, 75–250ms, exponential ease-out (`--ease-out-expo`, `--ease-out-quint`) — no bounce, no elastic, no page-load choreography. Legitimate motions: button press (75ms), modal fade-scale-in (150ms), drawer slide (250ms), and staggered list reveals (`.stagger-fade`, 50ms steps). Every animation collapses to near-instant under `prefers-reduced-motion: reduce`, which is wired globally.

## 6. Do's and Don'ts

### Do:
- **Do** treat color as signal — brand blue for interaction, semantic hues for state, service/status hues for identity. No meaning, no color.
- **Do** pack density where power users scan: tight within rows, generous between logical groups.
- **Do** keep both Signal Blue (hue 261, clickable) and Information Blue (hue 241, data/info) distinct; never let "info" read as "clickable."
- **Do** set machine-emitted content (IDs, status codes, logs, queries) in JetBrains Mono; human chrome in Inter.
- **Do** convey depth with tonal surface steps and 1px strokes; reserve shadows for floating layers.
- **Do** verify every token and every state in both light and dark — dark parity is required, not optional.
- **Do** give every error state a second signal beyond color (icon, position, shape, text). WCAG AA is the floor.
- **Do** express presentational state with Tailwind variants (`peer` / `group-has-[:checked]`) so it survives HTMX morphs.

### Don't:
- **Don't** use playful illustrations, mascots, or decorative icons above headings.
- **Don't** ship neon-on-dark, glassmorphism as decoration, gradient text, or cartoony empty states.
- **Don't** add `border-left` / `border-right` greater than 1px as a colored accent stripe.
- **Don't** warm the neutral base toward cream/sand — the base is faintly cooled toward the brand hue, never warmed.
- **Don't** introduce a display font, fluid `clamp()` heading, or one-off rem size; every type size is a named step on the scale, and weight + step carry hierarchy.
- **Don't** add a second blue hue. The system has one blue (brand hue 261); "info" is that hue at a lighter weight, not a rival hue — if you reach for another blue, you're drifting.
- **Don't** put a resting shadow on a card that isn't floating.
- **Don't** reinvent standard affordances (custom scrollbars, non-standard modals, weird form controls) for flavor.
- **Don't** reach for a modal as the first thought — exhaust inline and progressive-disclosure alternatives first.
