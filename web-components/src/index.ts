import './index.css';
import './local-time';
import './main';
// Publishes window.flameGraphChart / window.waterFallGraphChart, which the trace
// page's inline initTraceCharts calls. Must be eager: that script runs as soon as
// the trace HTML is swapped in, so a lazy import would lose the race and the
// waterfall would render its rows with no bars.
import './charts';

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
  ['session-replay', () => import('./session-replay')],
];

const loadComponents = () => components.forEach(([selector, load]) => {
  if (document.querySelector(selector)) void load();
});

loadComponents();
document.addEventListener('htmx:afterSettle', loadComponents);
