'use strict';
import { toPage } from './log-worker-functions';
import { fetchJson } from './fetch-failures';

self.onmessage = async (e: MessageEvent) => {
  const { type, url, expandedTraces, flipDirection, id } = e.data;

  if (type !== 'fetch') return;

  // Every failure up to a parsed body is the server's or the network's, shown to the reader
  // and flagged `expected` so the list does not console.error it.
  const fail = (error: string, queryError = false) => self.postMessage({ type: 'error', error, queryError, expected: true, id });
  let data;
  try {
    data = await fetchJson<any>(url);
  } catch (error) {
    return fail(error instanceof Error ? error.message : 'Network error');
  }
  try {
    // `error` is the only failure field LogResult carries — reading `message`
    // first silently swallowed every server message into the generic fallback.
    if (data.error) return fail(data.error, true);

    self.postMessage({ type: 'success', ...toPage(data, expandedTraces, flipDirection), id });
  } catch (error) {
    console.error('[Worker] Error:', error);
    self.postMessage({
      type: 'error',
      error: error instanceof Error ? error.message : 'Network error',
      id,
    });
  }
};
