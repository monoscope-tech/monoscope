/** A query failure already sanitized by the chart endpoint. */
export class ChartQueryError extends Error {}

/** A stream always ends with complete or error; EOF alone is not success. */
export async function readChartResponse<T>(response: Response, partial: (data: T) => void): Promise<T> {
  if (!response.headers?.get('content-type')?.includes('application/x-ndjson')) return response.json();
  if (!response.body) throw new Error('Chart response has no body');
  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let pending = '';
  try {
    for (;;) {
      const { value, done } = await reader.read();
      pending += decoder.decode(value, { stream: !done });
      const lines = pending.split('\n');
      pending = lines.pop()!;
      if (done && pending.trim()) lines.push(pending);
      for (const line of lines) {
        if (!line.trim()) continue;
        const frame = JSON.parse(line);
        if (frame.type === 'error') throw new ChartQueryError(typeof frame.error === 'string' ? frame.error : 'Chart query failed');
        if ((frame.type === 'partial' || frame.type === 'complete') && frame.data && typeof frame.data === 'object') {
          if (frame.type === 'complete') {
            return frame.data;
          }
          partial(frame.data);
        } else throw new Error('Invalid chart stream frame');
      }
      if (done) throw new Error('Chart response ended before completion');
      // Bound malformed responses that never delimit a frame.
      if (pending.length > 32 * 1024 * 1024) throw new Error('Chart stream frame is too large');
    }
  } finally {
    // Releases the server query on errors and after the terminal frame.
    await reader.cancel().catch(() => {});
    reader.releaseLock();
  }
}
