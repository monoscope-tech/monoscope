// Regression guard for "document.getElementById(...)?.toggleSubQuery is not a function":
// clicking a facet on a fresh page load reached the still-un-upgraded <query-editor>, threw,
// and dropped the filter. queryEditorCall must load the lazy chunk first and only then invoke.
import { describe, test, expect, beforeEach, vi } from 'vitest';

const spy = vi.fn();
// Stands in for the lazily loaded chunk: the element gains its methods only once this resolves.
vi.mock('../src/query-editor/query-editor', () => {
  (document.getElementById('filterElement') as any).toggleSubQuery = spy;
  return {};
});
vi.mock('../src/query-editor/query-editor-config', () => ({ initializeDefaultSchema: () => {} }));
vi.mock('../src/query-editor/query-builder', () => ({}));

const mountEditor = () => {
  const el = document.createElement('query-editor');
  el.id = 'filterElement';
  // Lit resolves updateComplete after firstUpdated, which is what creates the Monaco instance.
  Object.assign(el, { updateComplete: Promise.resolve(true) });
  document.body.appendChild(el);
};

const call = (...args: unknown[]) => (window as any).queryEditorCall(...args);

beforeEach(() => {
  document.body.innerHTML = '';
  spy.mockClear();
});

describe('queryEditorCall', () => {
  test('loads the lazy editor chunk before calling a method that is not there yet', async () => {
    mountEditor();
    await import('../src/index');

    await call('toggleSubQuery', 'level == "error"');

    expect(spy).toHaveBeenCalledWith('level == "error"');
  });

  test('is a no-op on pages with no query editor, and when the method stays undefined', async () => {
    await import('../src/index');
    await expect(call('toggleSubQuery', 'a == "b"')).resolves.toBeUndefined();

    mountEditor();
    await expect(call('noSuchMethod', 'q')).resolves.toBeUndefined();
  });
});
