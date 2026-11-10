'use strict';
import { groupSpans } from './log-worker-functions';
import { fetchJson } from './fetch-failures';

self.onmessage = async (e: MessageEvent) => {
  const { type, url, colIdxMap, expandedTraces, flipDirection, id } = e.data;

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

    const { logsData, serviceColors, nextUrl, recentUrl, cols, count, traces, hasMore, queryResultCount, viz } = data;

    if (!Array.isArray(logsData) || logsData.length === 0) {
      self.postMessage({
        type: 'success',
        tree: [],
        meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap: data.colIdxMap, count, traces: traces || [], hasMore: hasMore ?? false, queryResultCount: 0, viz },
        id,
      });
      return;
    }

    // Use the colIdxMap from the server response, not the one from the message
    const tree = groupSpans(logsData, data.colIdxMap, expandedTraces, flipDirection, traces || []);

    if (tree.length === 0) {
      console.error('[Worker] Tree is empty after processing', logsData.length, 'items');
    }

    self.postMessage({
      type: 'success',
      tree,
      meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap: data.colIdxMap, count, traces: traces || [], hasMore: hasMore ?? true, queryResultCount: queryResultCount ?? logsData.length, viz },
      id,
    });
  } catch (error) {
    console.error('[Worker] Error:', error);
    self.postMessage({
      type: 'error',
      error: error instanceof Error ? error.message : 'Network error',
      id,
    });
  }
};
