import { describe, expect, it, vi } from 'vitest';
import { readFileSync } from 'fs';
import { join } from 'path';

const source = readFileSync(join(__dirname, '../../src/Pages/Anomalies.hs'), 'utf8');
const script = source.match(/function setHljsTheme\(\) \{[\s\S]*?(?=\n    """)/)?.[0];

if (!script) throw new Error('could not find the Issues highlight.js script');

describe('Issues highlight.js theme observer', () => {
  it('waits for body before observing its theme attribute', () => {
    const listeners = new Map<string, () => void>();
    const observe = vi.fn((node: unknown) => {
      if (!(node instanceof Node)) throw new TypeError('parameter 1 is not of type Node');
    });
    const body = document.createElement('body');
    const fakeDocument = {
      body: null as HTMLBodyElement | null,
      addEventListener: (event: string, listener: () => void) => void listeners.set(event, listener),
      getElementById: () => ({ disabled: false }),
      querySelectorAll: () => [],
    };
    class FakeMutationObserver {
      constructor(_callback: MutationCallback) {}
      observe = observe;
    }

    // eslint-disable-next-line no-new-func
    expect(() => new Function('document', 'MutationObserver', 'hljs', script)(fakeDocument, FakeMutationObserver, { highlightElement: vi.fn() })).not.toThrow();
    fakeDocument.body = body;
    listeners.get('DOMContentLoaded')?.();
    expect(observe).toHaveBeenCalledWith(body, { attributeFilter: ['data-theme'] });
  });
});
