import { describe, test, expect, beforeAll, afterEach, vi } from 'vitest';
import { QueryEditorComponent, schemaManager, unclosedQuote, verdictToError, type Verdict } from '../src/query-editor/query-editor';

vi.stubGlobal('Worker', undefined);

// Field-name and grammar rules are the server's (Pkg.Parser.Stats, doctested
// there) and reach the editor through /log_explorer/validate. What's tested here
// is the client half: the one local check, the translation of a verdict into a
// marker, and the wiring — caching, staleness, and failing safe.

describe('unclosedQuote — the only rule the client owns', () => {
  test.each([
    ['kind == "log"', null],
    ["kind == 'log'", null],
    ['kind == "log', 'Unclosed double quote'],
    ["kind == 'log", 'Unclosed single quote'],
    ['kind == "a" and name == "b', 'Unclosed double quote'],
    ['kind == "say \\" hi"', null],
    ['', null],
  ])('%j', (query, expected) => {
    expect(unclosedQuote(query)?.message ?? null).toBe(expected);
  });

  test('underlines from the opening quote to the end of the query', () => {
    const err = unclosedQuote('kind == "log');
    expect(err).toMatchObject({ startColumn: 9, endColumn: 13, line: 1 });
  });
});

describe('verdictToError translates the server verdict', () => {
  test('a valid verdict marks nothing', () => {
    expect(verdictToError({ valid: true })).toBeNull();
  });

  test('an invalid verdict underlines exactly the reported span', () => {
    const v: Verdict = { valid: false, message: 'Unknown field "attribut". Did you mean "attributes"?', column: 17, width: 8 };
    expect(verdictToError(v)).toEqual({
      message: 'Unknown field "attribut". Did you mean "attributes"?',
      startColumn: 17,
      endColumn: 25,
      line: 1,
    });
  });

  test('a message with no position still marks something rather than nothing', () => {
    expect(verdictToError({ valid: false, message: 'boom' })).toMatchObject({ startColumn: 1, endColumn: 2 });
  });

  test('an invalid verdict with no message is not an error', () => {
    expect(verdictToError({ valid: false })).toBeNull();
  });
});

describe('the AI search trigger', () => {
  test('keeps Ask AI out of the Monaco editor except in standalone mode', async () => {
    const mount = async (standalone = false) => {
      const el = new QueryEditorComponent();
      el.setAttribute('project-id', '00000000-0000-0000-0000-000000000000');
      if (standalone) el.setAttribute('standalone-ai-search', '');
      document.body.appendChild(el);
      await el.updateComplete;
      await new Promise((resolve) => setTimeout(resolve));
      return el;
    };
    const editor = await mount();
    const standalone = await mount(true);

    try {
      expect(editor.querySelector('label[for="ai-search-chkbox"]')).toBeNull();
      const trigger = standalone.querySelector('[aria-label="Ask AI"]')!;
      expect(trigger.textContent).toContain('Ask AI');
      expect(trigger.classList).toContain('px-2');
    } finally {
      editor.remove();
      standalone.remove();
    }
  });
});

describe('the editor asks the server and marks the answer', () => {
  const INVALID: Verdict = { valid: false, message: 'Unknown field "attribut". Did you mean "attributes"?', column: 1, width: 8 };
  let fetchMock: ReturnType<typeof vi.fn>;

  beforeAll(() => {
    schemaManager.setSchemaData('spans', { fields: { kind: { type: 'string', examples: [] } } } as any);
    schemaManager.setDefaultSchema('spans');
  });

  const stubVerdicts = (...queue: (Verdict | 'boom')[]) => {
    fetchMock = vi.fn(async () => {
      const next = queue.length > 1 ? queue.shift()! : queue[0];
      if (next === 'boom') throw new Error('offline');
      return { ok: true, json: async () => next } as Response;
    });
    (globalThis as any).fetch = fetchMock;
  };

  const mount = async () => {
    const el = new QueryEditorComponent();
    el.setAttribute('project-id', '00000000-0000-0000-0000-000000000000');
    document.body.appendChild(el);
    await el.updateComplete;
    await new Promise((r) => setTimeout(r, 0));
    return el;
  };

  const validate = async (el: QueryEditorComponent, query: string) => {
    el.setValue(query);
    await (el as any).validateAndMark(query);
  };

  const errorShown = () => {
    const seen: string[] = [];
    const on = (e: Event) => seen.push((e as CustomEvent).detail);
    document.body.addEventListener('showParseError', on);
    return { seen, stop: () => document.body.removeEventListener('showParseError', on) };
  };

  afterEach(() => {
    (globalThis as any).fetch = undefined;
  });

  test('an invalid query surfaces the server message', async () => {
    stubVerdicts(INVALID);
    const el = await mount();
    const watch = errorShown();
    try {
      await validate(el, 'attribut contains "x"');
      expect(watch.seen).toEqual([INVALID.message]);
    } finally {
      watch.stop();
      el.remove();
    }
  });

  test('a valid query clears the message', async () => {
    stubVerdicts({ valid: true });
    const el = await mount();
    const cleared = vi.fn();
    (window as any).clearQueryParseError = cleared;
    try {
      await validate(el, 'kind == "log"');
      expect(cleared).toHaveBeenCalled();
    } finally {
      el.remove();
    }
  });

  test('an unterminated quote is reported without a round trip', async () => {
    stubVerdicts({ valid: true });
    const el = await mount();
    const watch = errorShown();
    try {
      await validate(el, 'kind == "log');
      expect(watch.seen).toEqual(['Unclosed double quote']);
      expect(fetchMock).not.toHaveBeenCalled();
    } finally {
      watch.stop();
      el.remove();
    }
  });

  test('repeating a query costs no second request', async () => {
    stubVerdicts(INVALID);
    const el = await mount();
    try {
      await validate(el, 'attribut contains "x"');
      await validate(el, 'attribut contains "x"');
      expect(fetchMock).toHaveBeenCalledTimes(1);
    } finally {
      el.remove();
    }
  });

  test('a stale answer cannot overwrite a newer one', async () => {
    // First call resolves last; its verdict must not land.
    let releaseFirst: (v: any) => void = () => {};
    const calls: string[] = [];
    (globalThis as any).fetch = vi.fn(async (url: string) => {
      calls.push(url);
      if (calls.length === 1) return new Promise((r) => (releaseFirst = () => r({ ok: true, json: async () => INVALID } as Response)));
      return { ok: true, json: async () => ({ valid: true }) } as Response;
    });

    const el = await mount();
    const watch = errorShown();
    try {

      const stale = (el as any).validateAndMark('attribut contains "x"');
      const fresh = (el as any).validateAndMark('kind == "log"');
      await fresh;
      releaseFirst(undefined);
      await stale;
      expect(watch.seen).toEqual([]); // the newer, valid answer won
    } finally {
      watch.stop();
      el.remove();
    }
  });

  test('a failed request leaves the query unmarked rather than calling it invalid', async () => {
    stubVerdicts('boom');
    const el = await mount();
    const watch = errorShown();
    try {
      await validate(el, 'kind == "log"');
      expect(watch.seen).toEqual([]);
    } finally {
      watch.stop();
      el.remove();
    }
  });

  test('without a project id it does not fetch at all', async () => {
    stubVerdicts(INVALID);
    const el = new QueryEditorComponent();
    document.body.appendChild(el);
    await el.updateComplete;
    await new Promise((r) => setTimeout(r, 0));
    try {
      await validate(el, 'attribut contains "x"');
      expect(fetchMock).not.toHaveBeenCalled();
    } finally {
      el.remove();
    }
  });

  // The placeholder covers the whole editor while the query is empty. If it
  // takes pointer events, the click that should focus Monaco lands on a
  // non-focusable div instead and the box never accepts a keystroke.
  test('the empty-query placeholder does not swallow the click that focuses the editor', async () => {
    stubVerdicts({ valid: true });
    const el = await mount();
    try {
      const overlay = el.querySelector('.cm-placeholder')!;
      expect(overlay.closest('[contenteditable="true"]')).not.toBeNull();
    } finally {
      el.remove();
    }
  });
});



test('a response arriving during the debounce window cannot mark newer text', async () => {
  let release!: (response: unknown) => void;
  vi.stubGlobal('fetch', vi.fn(() => new Promise(resolve => { release = resolve; })));
  const el = new QueryEditorComponent(); el.setAttribute('project-id', 'test'); document.body.append(el); await el.updateComplete;
  const messages: string[] = [];
  const listener = (e: Event) => messages.push((e as CustomEvent).detail);
  document.body.addEventListener('showParseError', listener);
  try {
    el.setValue('old'); const stale = (el as any).validateAndMark('old');
    el.setValue('new');
    release({ ok: true, json: async () => ({ valid: false, message: 'old error' }) });
    await stale; expect(messages).toEqual([]);
  } finally { el.remove(); document.body.removeEventListener('showParseError', listener); }
});

test.each([['name == "ends in slash\\\\"', null], ['level == "ok"\nname == "oops', 2]])('quote escaping and line positions: %s', (text, expected) => {
  expect(unclosedQuote(text)?.line ?? null).toBe(expected);
});
