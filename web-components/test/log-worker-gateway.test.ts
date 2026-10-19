import { test, expect, vi } from 'vitest';

// Shipbubble 2026-10-19: a slow search hit the 60s gateway timeout, and the HTML 504 page
// surfaced as a JSON parse error instead of saying the query timed out.
test('logWorker_gatewayTimeout_saysTimedOutNotParseError', async () => {
  vi.stubGlobal('fetch', vi.fn(async () => new Response('<html>504 Gateway Time-out</html>', { status: 504 })));
  const posted: any[] = [];
  vi.spyOn(self, 'postMessage').mockImplementation(((m: any) => posted.push(m)) as any);
  await import('../src/log-worker');
  await (self.onmessage as any)({ data: { type: 'fetch', url: '/p/x/log_explorer/data', id: 1 } });

  expect(posted).toEqual([expect.objectContaining({ type: 'error', id: 1, error: expect.stringContaining('timed out') })]);
  vi.unstubAllGlobals();
});
