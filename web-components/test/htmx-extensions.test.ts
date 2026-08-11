import { describe, test, expect, beforeAll } from 'vitest';

// The htmx-4 port of json-enc / forward-page-params read `detail.request`, but htmx 4 hands
// every request hook `{ctx}` — so both extensions silently did nothing on every page. For
// json-enc that meant the AI-search POST went out form-encoded with htmx's hardcoded
// `Accept: text/html`, and Servant answered 406 (it negotiates Accept before Content-Type, so
// the 406 masked the body encoding too). These tests drive the hooks through the real htmx
// call shape, so a wrong-shape read fails instead of no-op'ing green.
const hooks: Record<string, any> = {};

// Mirrors htmx's #X: hx-vals land in a URLSearchParams body, headers are a plain object
// carrying the hardcoded Accept, and the mutable request hangs off detail.ctx.
const requestCtx = (action = '/p/pid/log_explorer/ai_search', method = 'post') => ({
  request: {
    method,
    action,
    body: new URLSearchParams({ input: 'errors last hour', timezone: 'UTC' }),
    headers: { 'HX-Request': 'true', Accept: 'text/html' } as Record<string, string>,
  },
});

const fire = (hook: string, html: string, ctx: ReturnType<typeof requestCtx>) => {
  document.body.innerHTML = html;
  hooks[hook.split('.')[0]][hook.split('.')[1]](document.getElementById('el')!, { ctx });
  return ctx.request;
};

describe('htmx 4 extension port', () => {
  beforeAll(async () => {
    (window as any).htmx = { registerExtension: (name: string, h: any) => (hooks[name] = h) };
    await import('../src/main');
  });

  test.each([
    ['data-hx-ext', '<input id="el" data-hx-ext="json-enc">'],
    ['data-hx-ext, comma list', '<form data-hx-ext="json-enc,forward-page-params"><input id="el"></form>'],
    ['hx-ext', '<input id="el" hx-ext="json-enc">'],
  ])('json-enc sends a JSON body and a JSON-preferring Accept when opted in via %s', (_n, html) => {
    const req = fire('json-enc.htmx_before_request', html, requestCtx());
    expect(JSON.parse(req.body as unknown as string)).toEqual({ input: 'errors last hour', timezone: 'UTC' });
    expect(req.headers['Content-Type']).toBe('application/json');
    // text/html stays acceptable at lower q: /widget and manage_teams are `Post '[HTML]`.
    expect(req.headers['Accept']).toBe('application/json, text/html;q=0.9');
  });

  test('json-enc restores nested hx-vals that htmx flattened into the form body', () => {
    // What `hx-vals="js:{...widgetJSON}"` produces: htmx set()s the object into FormData
    // (→ "[object Object]") but keeps the original on ctx.vals.
    const ctx = requestCtx('/p/pid/widget');
    ctx.request.body.set('widget', String({}));
    (ctx as any).vals = { widget: { type: 'timeseries', query: 'x' }, teams: ['a', 'b'] };
    const req = fire('json-enc.htmx_before_request', '<input id="el" data-hx-ext="json-enc">', ctx);
    expect(JSON.parse(req.body as unknown as string)).toMatchObject({
      widget: { type: 'timeseries', query: 'x' },
      teams: ['a', 'b'],
    });
  });

  test('json-enc leaves the request alone when not opted in', () => {
    const req = fire('json-enc.htmx_before_request', '<input id="el" data-hx-ext="forward-page-params">', requestCtx());
    expect(req.body).toBeInstanceOf(URLSearchParams);
    expect(req.headers['Content-Type']).toBeUndefined();
    expect(req.headers['Accept']).toBe('text/html');
  });

  test('forward-page-params merges page params and dashboard constants into the action', () => {
    window.history.replaceState({}, '', '/x?since=1H&from=abc');
    const req = fire(
      'forward-page-params.htmx_config_request',
      '<div data-constants=\'{"env":"prod","since":"24H"}\'><div data-hx-ext="forward-page-params"><input id="el"></div></div>',
      requestCtx('/p/pid/widget?since=5M')
    );
    const q = new URL(req.action, location.origin).searchParams;
    expect(q.get('since')).toBe('5M'); // already on the action: untouched
    expect(q.get('from')).toBe('abc'); // from the page URL
    expect(q.get('env')).toBe('prod'); // from data-constants
  });
});
