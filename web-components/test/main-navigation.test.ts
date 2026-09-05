import { beforeEach, describe, expect, test } from 'vitest';
import { syncMainNavigation } from '../src/main-navigation';

const project = '/p/project-1';
const links = [
  ['Dashboards', '/dashboards', ''],
  ['Explorador', '/log_explorer?since=1H#results', '/live_tail /metrics /service_map /traces'],
  ['Issues', '/issues', ''],
  ['RUM', '/rum', ''],
  ['Infrastructure', '/infrastructure/hosts', '/infrastructure /containers'],
  ['API Catalog', '/api_catalog', '/endpoints'],
  ['Monitors', '/monitors', ''],
  ['Reports', '/reports', ''],
  ['Settings', '/settings', '/apis /manage_members /manage_billing'],
];
const activeLabels = () => [...document.querySelectorAll('#main-sidenav .active')].map((link) => link.textContent);
const navigate = (path: string, event = 'htmx:after:history:update') => {
  window.history.replaceState({}, '', `${project}${path}`);
  if (event === 'popstate') window.dispatchEvent(new PopStateEvent(event));
  else document.dispatchEvent(new Event(event));
};

beforeEach(() => {
  document.body.innerHTML = `<nav id="main-sidenav">${links.map(([label, path, aliases]) =>
    `<a class="main-nav-link active" href="${project}${path}" data-match="${aliases.split(' ').filter(Boolean).map((alias) => project + alias).join(' ')}" aria-current="page">${label}</a>`
  ).join('')}<a class="main-nav-link" href="https://docs.example.com/p/project-1/metrics">Docs</a></nav>`;
});

describe('sidebar section matching', () => {
  test.each([
    ['/log_explorer', 'Explorador'], ['/live_tail', 'Explorador'], ['/metrics', 'Explorador'],
    ['/metrics/details/http_requests?since=1H', 'Explorador'], ['/service_map', 'Explorador'], ['/traces/abc', 'Explorador'],
    ['/infrastructure/hosts/node-1', 'Infrastructure'], ['/infrastructure/containers', 'Infrastructure'],
    ['/infrastructure/images', 'Infrastructure'], ['/infrastructure/kubernetes', 'Infrastructure'],
    ['/infrastructure/host-map', 'Infrastructure'], ['/containers', 'Infrastructure'],
    ['/endpoints/details?var-host=example.com', 'API Catalog'], ['/api_catalog', 'API Catalog'],
    ['/apis', 'Settings'], ['/manage_members', 'Settings'], ['/manage_billing', 'Settings'], ['/settings/integrations', 'Settings'],
    ['/dashboards/dashboard-1', 'Dashboards'], ['/issues/issue-1', 'Issues'], ['/rum?tab=sessions', 'RUM'],
    ['/monitors/monitor-1/overview', 'Monitors'], ['/reports/report-1', 'Reports'],
  ])('%s keeps %s active', (path, label) => {
    navigate(path);
    expect(activeLabels()).toEqual([label]);
    expect(document.querySelectorAll('[aria-current="page"]')).toHaveLength(1);
    expect(document.querySelector('[aria-current="page"]')?.textContent).toBe(label);
  });

  test('initial load, sidebar swaps and browser history all resync the active section', () => {
    window.history.replaceState({}, '', `${project}/metrics`);
    syncMainNavigation();
    expect(activeLabels()).toEqual(['Explorador']);
    document.querySelector('.active')!.classList.remove('active');
    navigate('/metrics', 'htmx:after:swap');
    expect(activeLabels()).toEqual(['Explorador']);
    navigate('/manage_members', 'popstate');
    expect(activeLabels()).toEqual(['Settings']);
    navigate('/live_tail/', 'popstate');
    expect(activeLabels()).toEqual(['Explorador']);
  });

  test('does not match a similar prefix, another project or an external URL', () => {
    navigate('/reports-old');
    expect(activeLabels()).toEqual([]);
    window.history.replaceState({}, '', '/p/project-2/metrics');
    syncMainNavigation();
    expect(activeLabels()).toEqual([]);
    expect(document.querySelector('[aria-current]')).toBeNull();
  });
});
