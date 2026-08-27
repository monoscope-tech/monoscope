import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest';
import '../src/main';

let exported: Blob;

beforeEach(() => {
  document.body.innerHTML = '';
  Object.defineProperty(URL, 'createObjectURL', {
    configurable: true,
    value: vi.fn((blob: Blob) => {
      exported = blob;
      return 'blob:export';
    }),
  });
  Object.defineProperty(URL, 'revokeObjectURL', { configurable: true, value: vi.fn() });
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
});
