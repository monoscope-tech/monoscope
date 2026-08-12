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
  ['query-editor, query-builder', async () => {
    await import('./query-editor/query-editor');
    await import('./query-editor/query-builder');
  }],
  ['yaml-editor', async () => {
    await import('./query-editor/query-editor');
    await import('./yaml-editor');
  }],
  ['live-tail', () => import('./live-tail')],
  ['session-replay', () => import('./session-replay')],
  ['[data-service-map]', () => import('./service-map').then(m => m.hydrateServiceMaps())],
];

// The trace page's inline initTraceCharts and the map's filter input can call these before
// the lazy module has loaded, so forward through shims; service-map.ts replaces them on load.
(window as any).serviceMapChart = (...args: unknown[]) =>
  void import('./service-map').then(m => (m.serviceMapChart as (...a: any[]) => void)(...args));
(window as any).serviceMapFilter = (...args: unknown[]) =>
  void import('./service-map').then(m => (m.serviceMapFilter as (...a: any[]) => void)(...args));

const loadComponents = () => components.forEach(([selector, load]) => {
  if (document.querySelector(selector)) void load();
});

loadComponents();
document.addEventListener('htmx:after:swap', loadComponents);
