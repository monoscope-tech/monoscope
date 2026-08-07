import { describe, test, expect, beforeAll, afterEach, vi } from 'vitest';
import { QueryEditorComponent, schemaManager, unclosedQuote, verdictToError, type Verdict } from '../src/query-editor/query-editor';

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
    const model = (el as any).editor.getModel();
    model.setValue(query);
    await (el as any).validateAndMark(query, model);
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
      const model = (el as any).editor.getModel();
      const stale = (el as any).validateAndMark('attribut contains "x"', model);
      const fresh = (el as any).validateAndMark('kind == "log"', model);
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
});

// The dropdown used to mirror Monaco's suggest controller through private APIs
// and ignored a zero-item result, so a stale list stayed on screen. These drive
// the real component against a real Monaco model.
describe('the editor is wired to the completion module', () => {
  const mount = async () => {
    const el = new QueryEditorComponent();
    document.body.appendChild(el);
    await el.updateComplete;
    await new Promise((r) => setTimeout(r, 0));
    return el;
  };

  const suggestFor = async (el: QueryEditorComponent, query: string) => {
    const editor = (el as any).editor;
    editor.getModel().setValue(query);
    editor.setPosition({ lineNumber: 1, column: query.length + 1 });
    await (el as any).refreshSuggestions();
    return (el as any).completionItems.map((i: any) => i.label);
  };

  beforeAll(() => {
    schemaManager.setSchemaData('spans', {
      fields: {
        kind: { type: 'string', examples: ['span'] },
        status_code: { type: 'string', examples: ['OK', 'ERROR'] },
        attributes: { type: 'object', examples: [] },
        'attributes.user_id': { type: 'string', examples: [] },
      },
    } as any);
    schemaManager.setDefaultSchema('spans');
  });

  test('typing a field name offers fields, not just table names', async () => {
    const el = await mount();
    try {
      expect(await suggestFor(el, 'attribut')).toContain('attributes');
    } finally {
      el.remove();
    }
  });

  test('a position with nothing to offer clears the previous list', async () => {
    const el = await mount();
    try {
      expect((await suggestFor(el, '')).length).toBeGreaterThan(0);
      // No such field, so no values exist to suggest — the stale list must go.
      expect(await suggestFor(el, 'nosuchfield == ')).toEqual([]);
    } finally {
      el.remove();
    }
  });

  test('the list follows the cursor through a query', async () => {
    const el = await mount();
    try {
      expect(await suggestFor(el, 'status_code ')).toContain('==');
      expect(await suggestFor(el, 'status_code == ')).toEqual(['OK', 'ERROR']);
      expect(await suggestFor(el, 'attributes.')).toEqual(['user_id']);
    } finally {
      el.remove();
    }
  });

  test('a superseded keystroke cannot overwrite a newer list', async () => {
    const el = await mount();
    try {
      const editor = (el as any).editor;
      editor.getModel().setValue('status_code == ');
      editor.setPosition({ lineNumber: 1, column: 16 });
      const stale = (el as any).refreshSuggestions();

      editor.getModel().setValue('kind ');
      editor.setPosition({ lineNumber: 1, column: 6 });
      const fresh = (el as any).refreshSuggestions();

      await Promise.all([stale, fresh]);
      expect((el as any).completionItems.map((i: any) => i.label)).toContain('==');
    } finally {
      el.remove();
    }
  });
});

// setSchemaData announces itself so a query typed before the (lazily fetched)
// schema lands gets re-checked instead of staying unvalidated.
describe('schema arrival', () => {
  test('announces the table it loaded', () => {
    const seen: string[] = [];
    const on = (e: Event) => seen.push((e as CustomEvent).detail);
    document.body.addEventListener('schema-loaded', on);
    try {
      schemaManager.setSchemaData('spans', { fields: { kind: { type: 'string', examples: [] } } } as any);
      expect(seen).toEqual(['spans']);
    } finally {
      document.body.removeEventListener('schema-loaded', on);
    }
  });

  test('does not cache a lookup made before the schema arrived', async () => {
    schemaManager.setSchemaData('spans', { fields: {} } as any);
    expect(await schemaManager.resolveNested('spans', '')).toEqual([]);
    schemaManager.setSchemaData('spans', { fields: { kind: { type: 'string', examples: [] } } } as any);
    expect((await schemaManager.resolveNested('spans', '')).map((f) => f.name)).toEqual(['kind']);
  });
});


// Presentation state and screen-reader semantics: the page derives the error look
// from one attribute, and the input announces itself as a combobox.
describe('accessibility and state wiring', () => {
  const mount = async () => {
    const el = new QueryEditorComponent();
    document.body.appendChild(el);
    await el.updateComplete;
    await new Promise((r) => setTimeout(r, 0));
    return el;
  };

  test('the input is a combobox pointing at the suggestion list', async () => {
    const el = await mount();
    try {
      const input = (el as any).editor.getDomNode().querySelector('textarea');
      expect(input.getAttribute('role')).toBe('combobox');
      expect(input.getAttribute('aria-controls')).toBe('query-suggestions');
      expect(input.getAttribute('aria-autocomplete')).toBe('list');
    } finally {
      el.remove();
    }
  });

  test('aria-expanded follows the dropdown, and the active option is announced', async () => {
    const el = await mount();
    try {
      const input = () => (el as any).editor.getDomNode().querySelector('textarea');
      expect(input().getAttribute('aria-expanded')).toBe('false');
      expect(input().hasAttribute('aria-activedescendant')).toBe(false);

      (el as any).showSuggestions = true;
      (el as any).selectedIndex = 2;
      await el.updateComplete;

      expect(input().getAttribute('aria-expanded')).toBe('true');
      expect(input().getAttribute('aria-activedescendant')).toBe('query-suggestion-2');

      (el as any).showSuggestions = false;
      await el.updateComplete;
      expect(input().getAttribute('aria-expanded')).toBe('false');
    } finally {
      el.remove();
    }
  });

  test('the editor no longer paints its own error border — the page owns that state', async () => {
    const el = await mount();
    try {
      const model = (el as any).editor.getModel();
      (el as any).showError(model, { message: 'boom', startColumn: 1, endColumn: 2, line: 1 });
      expect(el.querySelector('.\\!border-strokeError-strong')).toBeNull();
    } finally {
      el.remove();
    }
  });
});
