import './index.css';
import './local-time';
import './main';

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
