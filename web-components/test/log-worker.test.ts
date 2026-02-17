import { describe, test, expect } from 'vitest';
import { groupSpans, flattenSpanTree } from '../src/log-worker-functions';

describe('Log Worker Functions', () => {
  const createSampleColIdxMap = () => ({
    trace_id: 0,
    latency_breakdown: 1,
    parent_id: 2,
    timestamp: 3,
    duration: 4,
    start_time_ns: 5,
    errors: 6,
    kind: 7,
    id: 8,
  });

  const createSampleSpan = (overrides: Partial<any> = {}) => {
    const defaults = {
      0: 'trace-123', // trace_id
      1: 'span-123', // latency_breakdown
      2: null, // parent_id
      3: '2024-01-01T00:00:00Z', // timestamp
      4: 1000000, // duration (nanoseconds)
      5: 1000000000, // start_time_ns
      6: false, // errors
      7: 'span', // kind
      8: 'span-id-123', // id
    };
    return Object.assign([], defaults, overrides);
  };

  describe('groupSpans', () => {
    test('should process a single span with server traces', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [createSampleSpan()];
      const serverTraces = [
        { trace_id: 'trace-123', start_time: 1000000000, duration: 1000000, trace_start_time: '2024-01-01T00:00:00Z', root: 'span-123', children: {} },
      ];

      const result = groupSpans(data, colIdxMap, {}, false, serverTraces);

      expect(result).toBeDefined();
      expect(result.length).toBe(1);
      expect(result[0].depth).toBe(0);
    });

    test('should link parent-child from server traces', () => {
      const colIdxMap = createSampleColIdxMap();
      const parent = createSampleSpan({ 0: 'trace-1', 1: 'span-1', 2: null, 8: 'span-1' });
      const child = createSampleSpan({ 0: 'trace-1', 1: 'span-2', 2: 'span-1', 8: 'span-2', 5: 2000000000 });
      const serverTraces = [
        { trace_id: 'trace-1', start_time: 1000000000, duration: 1000000000, trace_start_time: '2024-01-01T00:00:00Z', root: 'span-1', children: { 'span-1': ['span-2'] } },
      ];

      const result = groupSpans([parent, child], colIdxMap, {}, false, serverTraces);

      expect(result.length).toBe(2);
      expect(result[0].depth).toBe(0);
      expect(result[1].depth).toBe(1);
    });

    test('should handle multiple roots from same trace', () => {
      const colIdxMap = createSampleColIdxMap();
      const span1 = createSampleSpan({ 0: 'trace-1', 1: 'span-1', 2: null, 8: 'span-1', 5: 1000000000 });
      const span2 = createSampleSpan({ 0: 'trace-1', 1: 'span-2', 2: null, 8: 'span-2', 5: 2000000000 });
      const serverTraces = [
        { trace_id: 'trace-1', start_time: 1000000000, duration: 100, trace_start_time: null, root: 'span-1', children: {} },
        { trace_id: 'trace-1', start_time: 2000000000, duration: 100, trace_start_time: null, root: 'span-2', children: {} },
      ];

      const result = groupSpans([span1, span2], colIdxMap, {}, false, serverTraces);

      expect(result.length).toBe(2);
    });

    test('should process log entries (kind=log)', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [createSampleSpan({ 7: 'log', 8: 'log-id-1' })];
      const serverTraces = [
        { trace_id: 'trace-123', start_time: 1000000000, duration: 0, trace_start_time: '2024-01-01T00:00:00Z', root: 'log-id-1', children: {} },
      ];

      const result = groupSpans(data, colIdxMap, {}, false, serverTraces);

      expect(result).toBeDefined();
      expect(result.length).toBe(1);
      expect(result[0].type).toBe('log');
    });

    test('should sort traces by startTime descending by default', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [
        createSampleSpan({ 0: 'trace-1', 1: 'span-1', 8: 'span-1', 5: 1000000000 }),
        createSampleSpan({ 0: 'trace-2', 1: 'span-2', 8: 'span-2', 5: 2000000000 }),
      ];
      const serverTraces = [
        { trace_id: 'trace-1', start_time: 1000000000, duration: 1000000, trace_start_time: null, root: 'span-1', children: {} },
        { trace_id: 'trace-2', start_time: 2000000000, duration: 1000000, trace_start_time: null, root: 'span-2', children: {} },
      ];

      const result = groupSpans(data, colIdxMap, {}, false, serverTraces);

      expect(result.length).toBe(2);
      expect(result[0].startNs).toBeGreaterThanOrEqual(result[1].startNs);
    });

    test('should sort traces ascending when flipDirection=true', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [
        createSampleSpan({ 0: 'trace-1', 1: 'span-1', 8: 'span-1', 5: 1000000000 }),
        createSampleSpan({ 0: 'trace-2', 1: 'span-2', 8: 'span-2', 5: 2000000000 }),
      ];
      const serverTraces = [
        { trace_id: 'trace-1', start_time: 1000000000, duration: 1000000, trace_start_time: null, root: 'span-1', children: {} },
        { trace_id: 'trace-2', start_time: 2000000000, duration: 1000000, trace_start_time: null, root: 'span-2', children: {} },
      ];

      const result = groupSpans(data, colIdxMap, {}, true, serverTraces);

      expect(result.length).toBe(2);
      expect(result[0].startNs).toBeLessThanOrEqual(result[1].startNs);
    });

    test('should handle missing colIdxMap fields gracefully', () => {
      const partialColIdxMap = { trace_id: 0, timestamp: 3, kind: 7, id: 8 };
      const data = [createSampleSpan()];
      const serverTraces = [
        { trace_id: 'trace-123', start_time: 0, duration: 0, trace_start_time: null, root: 'span-id-123', children: {} },
      ];

      expect(() => groupSpans(data, partialColIdxMap, {}, false, serverTraces)).not.toThrow();
    });

    test('should handle empty serverTraces gracefully', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [createSampleSpan()];

      const result = groupSpans(data, colIdxMap, {}, false, []);

      expect(result).toEqual([]);
    });
  });

  describe('Integration test with realistic data', () => {
    test('should process a batch of 2540 items like in production', () => {
      const colIdxMap = createSampleColIdxMap();
      const data: any[] = [];
      const serverTraces: any[] = [];

      for (let i = 0; i < 254; i++) {
        const traceId = `trace-${i}`;
        const childrenMap: Record<string, string[]> = {};
        for (let j = 0; j < 10; j++) {
          data.push(
            createSampleSpan({
              0: traceId,
              1: `${traceId}-span-${j}`,
              2: j > 0 ? `${traceId}-span-${j - 1}` : null,
              8: `${traceId}-span-${j}`,
              5: 1000000000 + i * 1000000 + j * 100000,
            })
          );
          if (j > 0) {
            const parentId = `${traceId}-span-${j - 1}`;
            if (!childrenMap[parentId]) childrenMap[parentId] = [];
            childrenMap[parentId].push(`${traceId}-span-${j}`);
          }
        }
        serverTraces.push({
          trace_id: traceId,
          start_time: 1000000000 + i * 1000000,
          duration: 900000,
          trace_start_time: '2024-01-01T00:00:00Z',
          root: `${traceId}-span-0`,
          children: childrenMap,
        });
      }

      const result = groupSpans(data, colIdxMap, {}, false, serverTraces);

      console.log(`Processed ${data.length} items, got ${result.length} results`);
      expect(result.length).toBe(254 * 10); // One EventLine per span (root + 9 children each)
    });
  });
});
