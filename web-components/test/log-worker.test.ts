import { describe, test, expect } from 'vitest';
import { groupSpans, flattenSpanTree } from '../src/log-worker-functions';

describe('Log Worker Functions', () => {
  // Create sample data matching the server's format
  const createSampleColIdxMap = () => ({
    trace_id: 0,
    latency_breakdown: 1,
    parent_span_id: 2,
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
      2: null, // parent_span_id
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
    test('should process a single span', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [createSampleSpan()];
      const expandedTraces = {};
      const flipDirection = false;

      const result = groupSpans(data, colIdxMap, expandedTraces, flipDirection);

      expect(result).toBeDefined();
      expect(result.length).toBeGreaterThan(0);
      console.log('Single span result:', result);
    });

    test('should process multiple spans in same trace', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [
        createSampleSpan({ 0: 'trace-1', 1: 'span-1', 2: null, 8: 'span-1' }),
        createSampleSpan({ 0: 'trace-1', 1: 'span-2', 2: 'span-1', 8: 'span-2', 5: 1000001000 }),
      ];
      const expandedTraces = {};
      const flipDirection = false;

      const result = groupSpans(data, colIdxMap, expandedTraces, flipDirection);

      expect(result).toBeDefined();
      expect(result.length).toBeGreaterThan(0);
      console.log('Multiple spans result:', result);
    });

    test('should process log entries (kind=log)', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [createSampleSpan({ 7: 'log', 8: 'log-id-1' })];
      const expandedTraces = {};
      const flipDirection = false;

      const result = groupSpans(data, colIdxMap, expandedTraces, flipDirection);

      expect(result).toBeDefined();
      expect(result.length).toBeGreaterThan(0);
      console.log('Log entry result:', result);
    });

    test('should handle flipDirection=true', () => {
      const colIdxMap = createSampleColIdxMap();
      const data = [
        createSampleSpan({ 0: 'trace-1', 5: 1000000000 }),
        createSampleSpan({ 0: 'trace-2', 5: 2000000000 }),
      ];
      const expandedTraces = {};
      const flipDirection = true;

      const result = groupSpans(data, colIdxMap, expandedTraces, flipDirection);

      expect(result).toBeDefined();
      expect(result.length).toBeGreaterThan(0);
      // With flip direction, earlier timestamps should come first
      if (result.length >= 2) {
        expect(result[0].startNs).toBeLessThanOrEqual(result[1].startNs);
      }
    });

    test('should handle missing colIdxMap fields gracefully', () => {
      const partialColIdxMap = {
        trace_id: 0,
        timestamp: 3,
        kind: 7,
        id: 8,
      };
      const data = [createSampleSpan()];
      const expandedTraces = {};
      const flipDirection = false;

      // Should not throw
      expect(() => groupSpans(data, partialColIdxMap, expandedTraces, flipDirection)).not.toThrow();
    });
  });

  describe('Integration test with realistic data', () => {
    test('should process a batch of 2540 items like in production', () => {
      const colIdxMap = createSampleColIdxMap();
      // Create 2540 spans across multiple traces
      const data: any[] = [];
      for (let i = 0; i < 254; i++) {
        // 254 traces with 10 spans each = 2540 spans
        const traceId = `trace-${i}`;
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
        }
      }

      const expandedTraces = {};
      const flipDirection = false;

      const result = groupSpans(data, colIdxMap, expandedTraces, flipDirection);

      console.log(`Processed ${data.length} items, got ${result.length} results`);
      expect(result.length).toBeGreaterThan(0);
      expect(result.length).toBe(254 * 10); // Should have one EventLine per span
    });
  });
});
