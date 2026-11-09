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
    vi.stubGlobal('fetch', vi.fn(respond));
    const posted = vi.spyOn(self, 'postMessage').mockImplementation(() => {});
    await import('../src/log-worker');
    await (self.onmessage as any)({ data: { type: 'fetch', url: '/x', id: 7 } });
    expect(posted).toHaveBeenCalledWith(expect.objectContaining({ type: 'error', expected: true, id: 7, error: expect.stringMatching(message) }));
  });
});
