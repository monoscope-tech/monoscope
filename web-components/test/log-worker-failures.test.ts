import { afterEach, describe, expect, test, vi } from 'vitest';

// Regression for issue "Failed to execute 'json' on 'Response': Unexpected end of JSON input":
// an empty body threw from response.json() and was re-thrown in the list as an untyped Error.
describe('log worker fetch failures', () => {
  afterEach(() => vi.unstubAllGlobals());

  test.each([
    ['an empty 200 body', () => new Response('', { status: 200, headers: { 'content-type': 'application/json' } }), /incomplete response \(HTTP 200\)/],
    ['a server-reported error', () => Response.json({ error: 'Unknown field "ur"' }), /^Unknown field "ur"$/],
    ['a dropped connection', () => Promise.reject(new TypeError('Failed to fetch')), /^Failed to fetch$/],
  ])('%s is posted as an expected failure', async (_, respond, message) => {
    vi.stubGlobal('fetch', vi.fn(async () => respond()));
    const posted = vi.spyOn(self, 'postMessage').mockImplementation(() => {});
    await import('../src/log-worker');
    await (self.onmessage as any)({ data: { type: 'fetch', url: '/x', id: 7 } });
    expect(posted).toHaveBeenCalledWith(expect.objectContaining({ type: 'error', expected: true, id: 7, error: expect.stringMatching(message) }));
  });
});

// Regression: a lapsed session answers 401 {error: "unauthenticated"}, which fetchJson passed
// through as a query error, so the query box said "unauthenticated" instead of asking to refresh.
test('a lapsed session reads as session expiry, not a query error', async () => {
  vi.stubGlobal('fetch', vi.fn(async () => Response.json({ error: 'unauthenticated', login: '/login' }, { status: 401 })));
  const { fetchJson } = await import('../src/fetch-failures');
  await expect(fetchJson('/x')).rejects.toThrow('Session expired, please refresh');
  vi.unstubAllGlobals();
});
