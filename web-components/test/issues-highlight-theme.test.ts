import { describe, expect, it, vi } from 'vitest';
import { readFileSync } from 'fs';
import { join } from 'path';

const source = readFileSync(join(__dirname, '../../src/Pages/Issues.hs'), 'utf8');
const then = source.match(/loadDeps \[HighlightJs\]\n\s*<> """\n([\s\S]*?)\n\s*"""/)?.[1];

if (!then) throw new Error('could not find the Issues highlight.js script');

describe('Issues highlight.js theme observer', () => {
  it('observes the theme once per document, however often a link re-runs the page script', async () => {
    const observe = vi.fn();
    const addEventListener = vi.fn();
    const body = document.createElement('body');
    const fakeDocument = { body, addEventListener, getElementById: () => ({ disabled: false }), querySelectorAll: () => [] };
    class FakeMutationObserver {
      constructor(_callback: MutationCallback) {}
      observe = observe;
    }
    const fakeWindow = { loadDeps: () => Promise.resolve() } as Record<string, unknown>;
    // eslint-disable-next-line no-new-func
    const run = () => new Function('document', 'MutationObserver', 'hljs', 'window', `return window.loadDeps('HighlightJs')${then.trim()}`)(fakeDocument, FakeMutationObserver, { highlightElement: vi.fn() }, fakeWindow);
    await run();
    await run();
    expect(observe).toHaveBeenCalledTimes(1);
    expect(observe).toHaveBeenCalledWith(body, { attributeFilter: ['data-theme'] });
    expect(addEventListener).toHaveBeenCalledTimes(1);
  });
});
