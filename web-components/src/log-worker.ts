'use strict';
import { groupSpans } from './log-worker-functions';

self.onmessage = async (e: MessageEvent) => {
  const { type, url, colIdxMap, expandedTraces, flipDirection, id } = e.data;

  if (type !== 'fetch') return;

  try {
    const response = await fetch(url, {
      method: 'GET',
      headers: { Accept: 'application/json' },
      credentials: 'include',
    });

    const data = await response.json();

    // `error` is the only failure field LogResult carries — reading `message`
    // first silently swallowed every server message into the generic fallback.
    if (data.error) {
      self.postMessage({ type: 'error', error: data.error, queryError: true, id });
      return;
    }

    const { logsData, serviceColors, nextUrl, recentUrl, cols, count, traces, hasMore, queryResultCount } = data;

    if (!Array.isArray(logsData) || logsData.length === 0) {
      self.postMessage({
        type: 'success',
        tree: [],
        meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap: data.colIdxMap, count, traces: traces || [], hasMore: hasMore ?? false, queryResultCount: 0 },
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
      meta: { serviceColors, nextUrl, recentUrl, cols, colIdxMap: data.colIdxMap, count, traces: traces || [], hasMore: hasMore ?? true, queryResultCount: queryResultCount ?? logsData.length },
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
