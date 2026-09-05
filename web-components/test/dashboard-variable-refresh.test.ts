import { afterEach, expect, test, vi } from 'vitest';
import '../src/main';

const mount = () => {
  document.body.innerHTML = '<input name="service" class="dash-variable-input" data-tagify-query="| distinct service" data-tagify-reload-on-change="true">';
  const input = document.querySelector('input')!;
  const tagify = { settings: { whitelist: ['all', { value: 'checkout', name: 'Checkout' }] }, value: [{ value: 'checkout' }], loading: vi.fn() };
  (input as any)._tagifyInstance = tagify;
  return { input, tagify };
};
const reload = (input: HTMLElement): Promise<void> => (window as any).reloadVarWhitelist(input);
const response = (rows: unknown[]) => ({ ok: true, json: async () => ({ data_text: rows }) }) as Response;

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  document.body.innerHTML = '';
  history.replaceState({}, '', '/');
});

test('ticks keep options and selection usable, coalesce requests and append only new values', async () => {
  const { input, tagify } = mount();
  const pending = Promise.withResolvers<Response>();
  const fetch = vi.fn(() => pending.promise);
  vi.stubGlobal('fetch', fetch);
  const original = tagify.settings.whitelist;
  const selected = tagify.value;
  window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'auto-refresh' } }));
  const done = reload(input);
  expect(fetch).toHaveBeenCalledTimes(1);
  expect(tagify.settings.whitelist).toBe(original);
  expect(tagify.loading).not.toHaveBeenCalled();
  pending.resolve(response([['checkout', 'Changed label'], ['payments', 'Payments'], ['payments', 'Duplicate']]));
  await done;
  expect(tagify.settings.whitelist).toEqual([...original, { value: 'payments', name: 'Payments' }]);
  expect(tagify.value).toBe(selected);
  expect(tagify.loading).not.toHaveBeenCalled();

  fetch.mockResolvedValue(response([]));
  const merged = tagify.settings.whitelist;
  await reload(input);
  expect(tagify.settings.whitelist).toBe(merged);
});

test.each(['offline', 'http', 'query'])('%s failure preserves options and permits a later retry', async (failure) => {
  const { input, tagify } = mount();
  vi.spyOn(console, 'error').mockImplementation(() => {});
  const original = tagify.settings.whitelist;
  const fetch = vi.fn();
  vi.stubGlobal('fetch', fetch);
  if (failure === 'offline') fetch.mockRejectedValue(new Error('offline'));
  else fetch.mockResolvedValue(failure === 'http' ? { ok: false, status: 502 } : { ok: true, json: async () => ({ error: 'query failed' }) });
  await reload(input);
  expect(tagify.settings.whitelist).toBe(original);
  expect(tagify.loading).not.toHaveBeenCalled();
  fetch.mockResolvedValue(response([['new-service']]));
  await reload(input);
  expect(tagify.settings.whitelist).toEqual([...original, 'new-service']);
});

test('a response for superseded variable parameters cannot append outdated suggestions', async () => {
  const { input, tagify } = mount();
  const old = Promise.withResolvers<Response>();
  vi.stubGlobal('fetch', vi.fn().mockReturnValueOnce(old.promise).mockResolvedValue(response([['current']])));
  const stale = reload(input);
  history.replaceState({}, '', '/?var-environment=production');
  await reload(input);
  old.resolve(response([['obsolete']]));
  await stale;
  expect(tagify.settings.whitelist).toEqual(['all', { value: 'checkout', name: 'Checkout' }, 'current']);
});

test.each([true, false])('paces background requests for five minutes after success=%s, while parameter changes stay immediate', async (ok) => {
  const { input } = mount();
  const now = vi.spyOn(Date, 'now').mockReturnValue(0);
  vi.spyOn(console, 'error').mockImplementation(() => {});
  const fetch = vi.fn().mockResolvedValue(ok ? response([]) : { ok: false, status: 503 });
  vi.stubGlobal('fetch', fetch);
  const tick = () => window.dispatchEvent(new CustomEvent('update-query', { detail: { source: 'auto-refresh' } }));
  await reload(input);
  for (let seconds = 15; seconds < 300; seconds += 15) {
    now.mockReturnValue(seconds * 1000);
    tick();
  }
  expect(fetch).toHaveBeenCalledTimes(1);
  now.mockReturnValue(300_000);
  tick();
  await reload(input); // join the in-flight refresh
  expect(fetch).toHaveBeenCalledTimes(2);

  now.mockReturnValue(315_000);
  history.replaceState({}, '', '/?var-environment=production');
  window.dispatchEvent(new Event('update-query'));
  await reload(input);
  expect(fetch).toHaveBeenCalledTimes(3);
  tick();
  expect(fetch).toHaveBeenCalledTimes(3);
});
