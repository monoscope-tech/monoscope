import './index.css';
import './local-time';
import './main';
// Publishes window.flameGraphChart / window.waterFallGraphChart, which the trace
// page's inline initTraceCharts calls. Must be eager: that script runs as soon as
// the trace HTML is swapped in, so a lazy import would lose the race and the
// waterfall would render its rows with no bars.
import './charts';
// Page chrome (tabs, cookies, tooltips, toasts, HTMX progress). Needed on every page, and
// publishes globals that inline Lucid attributes call, so it must be eager too.
import './page-chrome';

const components: Array<[string, () => Promise<unknown>]> = [
  ['[data-chart-widget], [data-widget]', () => import('./widgets')],
  ['log-list', () => import('./log-list')],
  ['live-tail', () => import('./live-tail')],
  // Only the instances that play something on arrival: the anomaly/share pages carry
  // `initialSession`, and a ?session_replay= deep link auto-opens the log explorer's
  // floating player. The log explorer's own idle player is handled below.
  [
    new URLSearchParams(window.location.search).has('session_replay')
      ? 'session-replay'
      : 'session-replay[initialSession]',
    () => import('./session-replay'),
  ],
  ['[data-service-map]', () => import('./service-map').then(m => m.hydrateServiceMaps())],
];

// rrweb's replayer is ~84KB and the log explorer renders a (hidden) <session-replay> on
// every page view, so it used to load for everyone who never opened a recording. Hold the
// opening event, load the module, then re-dispatch so the now-upgraded element sees it.
window.addEventListener(
  'loadSessionReplay',
  e => {
    const { detail } = e as CustomEvent<{ sessionId: string }>;
    void import('./session-replay').then(() =>
      window.dispatchEvent(new CustomEvent('loadSessionReplay', { detail }))
    );
  },
  { once: true }
);

// Monaco is ~1.1MB gzipped and dominates the initial payload of every page carrying a
// search bar. Load it only when the user reaches for the editor. The
// server renders a matching skeleton inside the element (see LogQueryBox.queryEditorSkeleton_)
// so the box looks identical for the ~1 frame before Lit replaces it.
let queryEditorLoad: Promise<unknown> | null = null;
// Memoised, not just relying on import() caching: initializeDefaultSchema() must run once, and
// the still-armed {once:true} pointerdown listeners call this again after a shim-driven load.
const loadQueryEditor = () =>
  (queryEditorLoad ??= (async () => {
    await import('./query-editor/query-editor');
    // Schema list, default schema and the nested-field resolver. These feed Monaco's completion
    // provider and nothing else, so they belong here rather than at DOMContentLoaded — which is
    // where index.html's harness script used to call them, dragging all of Monaco in with it.
    const { initializeDefaultSchema } = await import('./query-editor/query-editor-config');
    initializeDefaultSchema();
    await import('./query-editor/query-builder');
  })());

const deferredComponents: Array<[string, () => Promise<unknown>]> = [
  ['query-editor, query-builder', loadQueryEditor],
  ['yaml-editor', async () => {
    await import('./query-editor/query-editor');
    await import('./yaml-editor');
  }],
];

// Inline handlers all over the app (facet checkboxes, saved-query rows, the field-action menu,
// viz tabs) call methods on <query-editor>. Monaco is loaded lazily, so until the user touches
// the search box that element is still un-upgraded and those calls threw "toggleSubQuery is not
// a function" — the click silently did nothing. Route them all through here: load the chunk if
// needed, wait for firstUpdated to create the Monaco instance every one of those methods bails
// without, then invoke.
(window as any).queryEditorCall = async (method: string, ...args: unknown[]) => {
  const el = document.getElementById('filterElement') as (HTMLElement & Record<string, any>) | null;
  if (!el) return; // no query editor on this page (e.g. shared/standalone item views)
  if (typeof el[method] !== 'function') await loadQueryEditor();
  await el.updateComplete;
  el[method]?.(...args);
};

// The trace page's inline initTraceCharts and the map's filter input can call these before
// the lazy module has loaded, so forward through shims; service-map.ts replaces them on load.
(window as any).serviceMapChart = (...args: unknown[]) =>
  void import('./service-map').then(m => (m.serviceMapChart as (...a: any[]) => void)(...args));
(window as any).serviceMapFilter = (...args: unknown[]) =>
  void import('./service-map').then(m => (m.serviceMapFilter as (...a: any[]) => void)(...args));

// Elements already wired for deferred loading. Survives htmx swaps: a re-swapped editor is a
// new node, so the WeakSet lets it be wired again while the old one is collected.
const armed = new WeakSet<Element>();

const loadDeferredComponents = () => deferredComponents.forEach(([selector, load]) => {
  const elements = Array.from(document.querySelectorAll(selector)).filter(el => !armed.has(el));
  if (elements.length === 0) return;

  let started = false;
  const start = (focusAfter: boolean) => {
    if (started) return;
    started = true;
    void load().then(() => {
      // An interaction-triggered load means the user was already reaching for the editor;
      // land them in it rather than making them click a second time.
      if (focusAfter) (elements[0] as { focusEditor?: () => void }).focusEditor?.();
    });
  };

  elements.forEach(el => {
    armed.add(el);
    ['pointerdown', 'focusin'].forEach(evt => el.addEventListener(evt, () => start(true), { once: true }));
  });
  // The editor's own "/" shortcut only exists once Monaco is in; cover the first press.
  document.addEventListener('keydown', e => {
    const target = e.target as HTMLElement | null;
    if (e.key !== '/' || e.ctrlKey || e.metaKey || e.altKey) return;
    if (target && (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA' || target.isContentEditable)) return;
    start(true);
  }, { once: true });
});

// Components that render their own <query-editor> after this module's initial scan (live-tail)
// announce it here. They must not import Monaco themselves: that loads ~1MB eagerly and defeats
// the deferral for every page carrying that component.
document.addEventListener('arm-deferred-components', () => loadDeferredComponents());

const loadComponents = () => {
  components.forEach(([selector, load]) => {
    if (document.querySelector(selector)) void load();
  });
  loadDeferredComponents();
};

loadComponents();
document.addEventListener('htmx:after:swap', loadComponents);
