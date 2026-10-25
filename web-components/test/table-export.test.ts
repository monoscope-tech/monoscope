import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest';
import '../src/main';

let exported: Blob;

beforeEach(() => {
  document.body.innerHTML = '';
  localStorage.clear();
  Object.defineProperty(URL, 'createObjectURL', {
    configurable: true,
    value: vi.fn((blob: Blob) => {
      exported = blob;
      return 'blob:export';
    }),
  });
  Object.defineProperty(URL, 'revokeObjectURL', { configurable: true, value: vi.fn() });
});

test('column choices survive swaps and column insertion, stay scoped, and reset', () => {
  const markup = (extra = '') => `<div data-table-columns="hostsForm">
    <input type="checkbox" data-column-toggle="CPU" checked><button>Reset</button>
    <table><tr>${extra}<th data-column-key="CPU">CPU</th></tr>
    <tr>${extra}<td data-column-key="CPU">25%</td></tr></table></div>
    <div data-table-columns="imagesForm"><table><tr><td data-column-key="CPU">50%</td></tr></table></div>`;
  document.body.innerHTML = markup();
  const input = document.querySelector<HTMLInputElement>('input')!;
  input.checked = false;
  window.setTableColumn(input);
  document.body.innerHTML = markup('<td data-column-key="Configuration">Europe</td>');
  document.dispatchEvent(new CustomEvent('htmx:after:swap'));

  const hosts = document.querySelector<HTMLElement>('[data-table-columns="hostsForm"]')!;
  expect(Array.from(hosts.querySelectorAll<HTMLElement>('[data-column-key="CPU"]')).every(cell => cell.style.display === 'none')).toBe(true);
  expect(hosts.querySelector<HTMLInputElement>('input')!.checked).toBe(false);
  expect(document.querySelector<HTMLElement>('[data-table-columns="imagesForm"] td')!.style.display).toBe('');
  expect(hosts.querySelector<HTMLElement>('[data-column-key="Configuration"]')!.style.display).toBe('');
  window.resetTableColumns(hosts.querySelector('button')!);
  expect(hosts.querySelector<HTMLElement>('th')!.style.display).toBe('');
  expect(hosts.querySelector<HTMLInputElement>('input')!.checked).toBe(true);
  expect(localStorage.getItem('table-columns:hostsForm')).toBeNull();
});

test('Customize reflects responsive visibility and can reveal a hidden column', () => {
  document.body.innerHTML = `<style>.responsive-column { display: none }</style>
    <div data-table-columns="hostsForm"><input type="checkbox" data-column-toggle="System" checked>
    <table><tr><td class="responsive-column" data-column-key="System">Linux</td></tr></table></div>`;
  document.dispatchEvent(new CustomEvent('htmx:after:swap'));
  const input = document.querySelector<HTMLInputElement>('input')!;
  expect(input.checked).toBe(false);
  input.checked = true;
  window.setTableColumn(input);
  expect(getComputedStyle(document.querySelector('td')!).display).toBe('table-cell');
});

afterEach(() => vi.restoreAllMocks());

describe('table CSV export', () => {
  test('downloads escaped visible cells with the requested filename', async () => {
    document.body.innerHTML = `
      <table id="inventory">
        <tr><th>Host</th><th>Note</th><th style="display:none">Secret</th></tr>
        <tr><td>api-1</td><td>ready, "primary"</td><td style="display:none">token</td></tr>
        <tr style="display:none"><td>hidden</td><td>row</td></tr>
      </table>`;
    document.querySelectorAll<HTMLElement>('th,td').forEach((cell) => (cell.innerText = cell.textContent ?? ''));
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => undefined);

    window.exportTableCsv('#inventory', 'hosts.csv');

    expect(click).toHaveBeenCalledOnce();
    expect(click.mock.instances[0].download).toBe('hosts.csv');
    expect(await exported.text()).toBe('"Host","Note"\n"api-1","ready, ""primary"""');
    expect(URL.revokeObjectURL).toHaveBeenCalledWith('blob:export');
  });

  test('missing tables do not start a download', () => {
    const click = vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => undefined);

    window.exportTableCsv('#missing', 'missing.csv');

    expect(click).not.toHaveBeenCalled();
    expect(URL.createObjectURL).not.toHaveBeenCalled();
  });

  test('grouped exports contain one header and only visible data rows', async () => {
    document.body.innerHTML = `<table id="inventory">
      <thead><tr><th>Host</th><th>CPU</th></tr></thead>
      <tbody><tr><td colspan="2"><details><summary>Europe: 2 hosts</summary><table>
        <thead class="sr-only"><tr><th>Host</th><th>CPU</th></tr></thead>
        <tbody><tr class="itemsListItem"><td>api-1</td><td>25%</td></tr>
        <tr class="itemsListItem" style="display:none"><td>api-2</td><td>50%</td></tr></tbody>
      </table></details></td></tr></tbody></table>`;
    document.querySelectorAll<HTMLElement>('th,td').forEach(cell => cell.innerText = cell.textContent ?? '');
    vi.spyOn(HTMLAnchorElement.prototype, 'click').mockImplementation(() => undefined);

    window.exportTableCsv('#inventory', 'hosts.csv');

    expect(await exported.text()).toBe('"Host","CPU"\n"api-1","25%"');
  });
});
