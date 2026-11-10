import { describe, test, expect, beforeAll, afterEach, vi } from 'vitest';

// The browser SDK files every console.error as a critical runtime exception. A variable whose
// query the server rejects (a 4xx, or an `error` payload) is the dashboard author's mistake,
// not a fault: it must mark the picker instead of filing "Variable options request failed: 400".
const picker = () => {
  document.body.innerHTML = `<fieldset title="Courier integration"><input class="dash-variable-input" name="courier" data-project-id="pid" data-tagify-query-sql="select 1"></fieldset>`;
  const input = document.querySelector<HTMLElement>('input')!;
  (input as any)._tagifyInstance = { settings: { whitelist: [] }, value: [], state: {} };
  return input;
};
const respond = (status: number, body: unknown) =>
  vi.stubGlobal('fetch', vi.fn(async () => new Response(typeof body === 'string' ? body : JSON.stringify(body), { status })));

describe('dashboard variable option refresh', () => {
  beforeAll(async () => {
    (window as any).htmx = { registerExtension: () => {} };
    await import('../src/main');
  });
  afterEach(() => vi.unstubAllGlobals());

  test.each([
    ['a 400 rejection', 400, 'Query must filter by project_id', 'Query must filter by project_id'],
    ['an error payload', 200, { error: 'Unknown field "x"' }, 'Unknown field "x"'],
  ])('%s marks the picker without console.error', async (_n, status, body, message) => {
    const input = picker();
    const fieldset = input.closest('fieldset')!;
    const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
    respond(status, body);
    await (window as any).reloadVarWhitelist(input);
    expect(consoleError).not.toHaveBeenCalled();
    expect(fieldset.title).toBe(message);
    expect(input.hasAttribute('data-invalid')).toBe(true);

    respond(200, { data_text: [['a']] });
    await (window as any).reloadVarWhitelist(input);
    expect(fieldset.title).toBe('Courier integration');
    expect(input.hasAttribute('data-invalid')).toBe(false);
    consoleError.mockRestore();
  });

  // A 5xx used to be console.error'd while the picker showed nothing: filed as an issue, invisible to the reader.
  test('a 5xx marks the picker with its status instead of filing an issue', async () => {
    const input = picker();
    const consoleError = vi.spyOn(console, 'error').mockImplementation(() => {});
    respond(503, '<html>unavailable</html>');
    await (window as any).reloadVarWhitelist(input);
    expect(consoleError).not.toHaveBeenCalled();
    expect(input.closest('fieldset')!.title).toBe("Couldn't load options (HTTP 503). Retry later.");
    consoleError.mockRestore();
  });
});
