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
import { installStaleChunkReload } from './stale-chunk-reload';

// Must be armed before the first loadComponents() below, or the very import that
// races a deploy is the one we miss.
installStaleChunkReload();

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

// The small query editor loads as soon as its host exists. The server textarea
// remains editable during loading; the component adopts its latest text on upgrade.
let queryEditorLoad: Promise<unknown> | null = null;
// Initialize shared defaults once, even when several hosts mount together.
const loadQueryEditor = () =>
  (queryEditorLoad ??= (async () => {
    await import('./query-editor/query-editor');
    const { initializeDefaultSchema } = await import('./query-editor/query-editor-config');
    initializeDefaultSchema();
    await import('./query-editor/query-builder');
  })());

const deferredComponents: Array<[string, () => Promise<unknown>]> = [
  ['query-editor, query-builder', loadQueryEditor],
  ['yaml-editor', async () => {
    await import('./yaml-editor');
  }],
];

// Facets, saved queries and chart controls can act before the component upgrades.
// Resolve the module and first render before invoking its public methods.
const queryEditorCallFor = async (el: (HTMLElement & Record<string, any>) | null, method: string, ...args: unknown[]) => {
  if (!el) return; // no query editor on this page (e.g. shared/standalone item views)
  if (typeof el[method] !== 'function') await loadQueryEditor();
  await el.updateComplete;
  el[method]?.(...args);
};
(window as any).queryEditorCallFor = queryEditorCallFor;
(window as any).queryEditorCall = (method: string, ...args: unknown[]) =>
  queryEditorCallFor(document.getElementById('filterElement') as HTMLElement & Record<string, any>, method, ...args);

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
  let focusTarget: Element | null = null;
  const start = (focusAfter: boolean, target = elements[0]) => {
    if (focusAfter) focusTarget = target;
    if (started) return;
    started = true;
    void load().then(() => {
      // An interaction-triggered load means the user was already reaching for the editor;
      // land them in it rather than making them click a second time.
      if (focusTarget?.isConnected && (document.activeElement === document.body || focusTarget.contains(document.activeElement))) {
        (focusTarget as { focusEditor?: () => void }).focusEditor?.();
      }
    });
  };

  elements.forEach(el => {
    armed.add(el);
    ['pointerdown', 'focusin'].forEach(evt => el.addEventListener(evt, () => start(true, el), { once: true }));
  });
  if (selector.includes('query-editor')) void load();

});

// Hosts created after the initial scan (such as live tail) announce themselves here.
document.addEventListener('arm-deferred-components', () => loadDeferredComponents());

const loadComponents = () => {
  components.forEach(([selector, load]) => {
    if (document.querySelector(selector)) void load();
  });
  loadDeferredComponents();
};

loadComponents();
document.addEventListener('htmx:after:swap', loadComponents);
