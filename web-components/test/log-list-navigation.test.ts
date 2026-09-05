import { readFileSync } from 'node:fs';
import { join } from 'node:path';
import { describe, test, expect, beforeAll, afterEach, vi } from 'vitest';
import { LogList } from '../src/log-list';
import { logPage, serverTransport, ids } from './log-list-harness';

// Run the shipped HTMX morph algorithm: mocked DOM events miss its extension-only
// morph hooks and cannot reproduce the loss of Lit's light-DOM part markers.
const htmxSource = readFileSync(join(__dirname, '../../static/public/assets/deps/htmx/htmx-4.0.0-beta6.min.js'), 'utf8');
let htmx: any;
beforeAll(async () => {
  // HTMX installs its transition stylesheet; jsdom does not implement adoption.
  Object.defineProperty(document, 'adoptedStyleSheets', { value: [], writable: true, configurable: true });
  CSSStyleSheet.prototype.replaceSync = () => {};
  // Browsers default XPath's result type to ANY_TYPE; jsdom requires it explicitly.
  const evaluate = XPathExpression.prototype.evaluate;
  XPathExpression.prototype.evaluate = function (node, type = 0, result = null) {
    return evaluate.call(this, node, type, result);
  };
  htmx = new Function(`${htmxSource}; return htmx;`)();
  (window as any).htmx = htmx;
  await import('../src/main');
});
afterEach(() => {
  document.body.replaceChildren();
  vi.restoreAllMocks();
});

const shell = (embedded: boolean) => `
  <main id="main-content">
    <section id="${embedded ? 'issue' : 'apiLogsPage'}">
      <div ${embedded ? 'hidden' : ''}>
        <log-list id="resultTable" projectId="proj-1" ${embedded ? 'initialFetchUrl="/p/proj-1/log_explorer/data?query=issue"' : ''}></log-list>
      </div>
      <p id="sibling">${embedded ? 'Issue' : 'Explorer'}</p>
    </section>
  </main>`;
const morph = async (html: string) => {
  const target = document.getElementById('main-content')!;
  await htmx.swap({ text: html, target, sourceElement: target, swap: 'outerMorph' });
  const list = document.querySelector('log-list') as LogList;
  await list.updateComplete;
  return list;
};

describe('log list page navigation', () => {
  test('replaces a hidden issue list with a working explorer and supports navigation back', async () => {
    const initialFetch = vi.spyOn(LogList.prototype, 'fetchInitialData').mockImplementation(async function (this: LogList) {
      this.transport = serverTransport(logPage(['explorer-row']));
      await this.refetchLogs();
    });
    document.body.innerHTML = shell(true);
    const embedded = document.querySelector('log-list') as LogList;
    await embedded.updateComplete;
    expect(initialFetch).not.toHaveBeenCalled();

    window.history.replaceState({}, '', '/p/proj-1/log_explorer?query=explorer');
    const explorer = await morph(shell(false));
    await vi.waitFor(() => expect(ids(explorer)).toEqual(['explorer-row']));
    expect(explorer).not.toBe(embedded);
    expect(embedded.isConnected).toBe(false);
    expect(explorer.initialFetchUrl).toBe('');
    expect(explorer.querySelector('lit-virtualizer')).not.toBeNull();
    expect(document.getElementById('sibling')?.textContent).toBe('Explorer');

    // A subsequent render must still own valid Lit markers.
    explorer.transport = serverTransport(logPage(['refreshed-row']));
    await explorer.refetchLogs();
    await explorer.updateComplete;
    expect(ids(explorer)).toEqual(['refreshed-row']);

    initialFetch.mockClear();
    const returned = await morph(shell(true));
    expect(returned).not.toBe(explorer);
    expect(returned.initialFetchUrl).toContain('query=issue');
    expect(initialFetch.mock.contexts).not.toContain(returned);
    (globalThis as any).triggerIntersection(returned);
    await vi.waitFor(() => expect(initialFetch.mock.contexts.filter((list) => list === returned)).toHaveLength(1));
    expect(document.getElementById('sibling')?.textContent).toBe('Issue');
  });
});
