// <ai-browser-call data-tool data-args data-call data-answer>: a call the AI assistant made against
// this tab. It runs once on arrival and posts its answer; the server then resumes the paused run.
// The answer is plain text the model reads, so the page is described as one control per line.

const CONTROLS = 'a[href], button, input:not([type=hidden]), select, textarea, summary, [role=button], [role=tab], [role=menuitem], [role=option], [role=checkbox], [role=switch]';
// The assistant's own chrome is not part of the page it is asked about.
const OUTSIDE = '#ai-dock, #cmd-palette-backdrop';
const LIMIT = 8000;

const visible = (el: Element) => !el.closest(OUTSIDE) && el.getClientRects().length > 0;
const label = (el: HTMLElement) =>
  (el.getAttribute('aria-label') || el.innerText || el.getAttribute('placeholder') || el.title || el.getAttribute('name') || '')
    .replace(/\s+/g, ' ')
    .trim()
    .slice(0, 80);

export function snapshot(): string {
  document.querySelectorAll('[data-ai-ref]').forEach(el => el.removeAttribute('data-ai-ref'));
  const lines = [`URL: ${location.pathname}${location.search}`, `Title: ${document.title}`];
  document.querySelectorAll<HTMLElement>('h1, h2, h3').forEach(h => visible(h) && label(h) && lines.push(`${h.tagName.toLowerCase()} ${label(h)}`));
  let n = 0;
  document.querySelectorAll<HTMLElement>(CONTROLS).forEach(el => {
    if (!visible(el) || (el as HTMLButtonElement).disabled) return;
    const ref = `r${++n}`;
    el.dataset.aiRef = ref;
    const input = el instanceof HTMLInputElement || el instanceof HTMLTextAreaElement || el instanceof HTMLSelectElement ? el : null;
    const kind = el.getAttribute('role') || (el instanceof HTMLInputElement ? `input ${el.type}` : el.tagName.toLowerCase());
    const state =
      el instanceof HTMLInputElement && ['checkbox', 'radio'].includes(el.type) ? (el.checked ? ' checked' : ' unchecked')
      : input?.value ? ` value="${input.value.slice(0, 60)}"`
      : '';
    const href = el instanceof HTMLAnchorElement ? ` -> ${el.getAttribute('href')}` : '';
    lines.push(`[${ref}] ${kind} "${label(el)}"${state}${href}`);
  });
  const text = lines.join('\n');
  return text.length > LIMIT ? `${text.slice(0, LIMIT)}\n(truncated)` : text;
}

const sleep = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

// An action may start htmx requests; answer once none is in flight, within five seconds.
async function settle() {
  const end = Date.now() + 5000;
  await sleep(300);
  while (document.querySelector('.htmx-request') && Date.now() < end) await sleep(100);
  await sleep(150);
}

function target(ref: string): HTMLElement {
  const el = document.querySelector<HTMLElement>(`[data-ai-ref="${CSS.escape(ref)}"]`);
  if (!el) throw new Error(`No control ${ref} on the page now; call read_page again.`);
  return el;
}

const tools: Record<string, (args: Record<string, string>) => Promise<string>> = {
  read_page: async () => snapshot(),
  click: async ({ ref }) => {
    target(ref).click();
    await settle();
    return `Clicked ${ref}.\n\n${snapshot()}`;
  },
  fill: async ({ ref, value }) => {
    const el = target(ref) as HTMLInputElement;
    // The prototype's setter, so frameworks that track the value see the change.
    Object.getOwnPropertyDescriptor(Object.getPrototypeOf(el), 'value')?.set?.call(el, value);
    el.dispatchEvent(new Event('input', { bubbles: true }));
    el.dispatchEvent(new Event('change', { bubbles: true }));
    await settle();
    return `Filled ${ref}.\n\n${snapshot()}`;
  },
};

// A reload re-renders a call that is still pending; it must not click twice.
const seen = (call: string) => {
  try {
    const done = sessionStorage.getItem(`ai-browser-call:${call}`) !== null;
    sessionStorage.setItem(`ai-browser-call:${call}`, '');
    return done;
  } catch {
    return false;
  }
};

class AiBrowserCall extends HTMLElement {
  async connectedCallback() {
    const { tool = '', args = '{}', call = '', answer = '' } = this.dataset;
    if (!call || seen(call)) return;
    let result: string;
    try {
      const run = tools[tool];
      if (!run) throw new Error(`Unknown browser tool ${tool}`);
      result = await run(JSON.parse(args));
    } catch (e) {
      result = `Error: ${e instanceof Error ? e.message : String(e)}`;
    }
    await fetch(answer, { method: 'POST', body: new URLSearchParams({ call, result }) });
  }
}

if (!customElements.get('ai-browser-call')) customElements.define('ai-browser-call', AiBrowserCall);
