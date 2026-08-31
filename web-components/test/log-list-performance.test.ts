import { describe, expect, test } from 'vitest';
import { LogList } from '../src/log-list';
import { logPage, serverTransport } from './log-list-harness';
import { MAX_RETAINED_ROWS, RETENTION_LIMIT } from '../src/log-list';


describe('LogList production-sized pagination', () => {
  test('merges 10,000 overlapping rows with bounded retained state', async () => {
    const pageSize = 100;
    const pageCount = 100;
    const pages = Array.from({ length: pageCount }, (_, pageIndex) => {
      const first = pageIndex * (pageSize - 1);
      const ids = Array.from({ length: pageSize }, (_, rowIndex) => String(first + rowIndex));
      return logPage(ids, { nextUrl: `/page/${pageIndex + 1}`, hasMore: pageIndex < pageCount - 1 });
    });
    // Keep the component disconnected: this benchmark exercises the production
    // worker/group/merge path without asking a DOM emulator to run lit-virtualizer.
    const list = new LogList();
    list.transport = serverTransport(...pages);

    const started = performance.now();
    await list.fetchData('/page/0', false, false, false);
    for (let page = 1; page < pageCount; page += 1) {
      await list.fetchData(`/page/${page}`, false, false, true);
    }
    const elapsedMs = performance.now() - started;

    // State as well as DOM is bounded: older rows are discarded past the retention window.
    expect((list as any).spanListTree.length).toBeLessThanOrEqual(RETENTION_LIMIT);
    expect((list as any).spanListTree.length).toBeGreaterThanOrEqual(MAX_RETAINED_ROWS);
    expect((list as any).seenIds.size).toBe((list as any).spanListTree.length);
    // Generous CI guardrail: catches accidental quadratic merging without timing normal noise.
    expect(elapsedMs).toBeLessThan(10_000);
  }, 15_000);
});
