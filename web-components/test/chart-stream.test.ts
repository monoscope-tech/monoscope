import { describe, expect, test, vi } from 'vitest';
import { readChartResponse } from '../src/chart-stream';

const response = (body: ReadableStream<Uint8Array>) => new Response(body, { headers: { 'content-type': 'application/x-ndjson' } });
const bytes = (value: unknown) => new TextEncoder().encode(JSON.stringify(value) + '\n');

describe('progressive chart responses', () => {
  test('delivers partial data before completion across byte and Unicode boundaries', async () => {
    let source!: ReadableStreamDefaultController<Uint8Array>;
    const partial = vi.fn();
    const result = readChartResponse(response(new ReadableStream({ start(c) { source = c; } })), partial);
    const first = { headers: ['timestamp', '日本語'], dataset: [[1, 2]] };
    for (const byte of bytes({ type: 'partial', data: first })) source.enqueue(Uint8Array.of(byte));
    await vi.waitFor(() => expect(partial).toHaveBeenCalledWith(first));
    const last = { ...first, dataset: [[1, 2], [2, 3]] };
    source.enqueue(bytes({ type: 'complete', data: last }));
    await expect(result).resolves.toEqual(last);
  });

  test.each([
    ['premature EOF', [{ type: 'partial', data: { dataset: [[1, 2]] } }]],
    ['server error', [{ type: 'partial', data: {} }, { type: 'error', error: 'Query timed out' }]],
    ['unknown frame', [{ type: 'unexpected' }]],
  ])('rejects %s instead of accepting incomplete results', async (_, frames) => {
    const body = new ReadableStream<Uint8Array>({ start(c) { frames.forEach(frame => c.enqueue(bytes(frame))); c.close(); } });
    await expect(readChartResponse(response(body), () => {})).rejects.toThrow();
  });

  test('cancels the body after an invalid frame and accepts ordinary JSON', async () => {
    const cancel = vi.fn();
    const body = new ReadableStream<Uint8Array>({ start(c) { c.enqueue(new TextEncoder().encode('bad json\n')); }, cancel });
    await expect(readChartResponse(response(body), () => {})).rejects.toThrow();
    expect(cancel).toHaveBeenCalledOnce();
    await expect(readChartResponse(Response.json({ dataset: [] }), () => {})).resolves.toEqual({ dataset: [] });
  });
});
