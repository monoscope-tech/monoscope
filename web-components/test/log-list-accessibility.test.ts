import { html, render } from 'lit';
import { afterEach, describe, expect, test, vi } from 'vitest';
import { mountList } from './log-list-harness';

afterEach(() => {
  document.body.replaceChildren();
  history.replaceState({}, '', '/');
  vi.restoreAllMocks();
});

describe('log table accessibility', () => {
  test('column headings are headers and closed popovers leave the accessibility tree', async () => {
    const el = await mountList();
    const host = document.createElement('table');
    document.body.appendChild(host);
    render(html`<thead><tr>${(el as any).logTableHeading('timestamp')}</tr></thead>`, host);

    const heading = host.querySelector('th');
    expect(heading?.getAttribute('scope')).toBe('col');
    expect(heading?.getAttribute('role')).toBe('columnheader');
    expect(el.querySelector('style')?.textContent).toMatch(/\.column-popover:not\(:popover-open\)[^{]*{[^}]*display:\s*none/s);
    expect(el.querySelector('style')?.textContent).toMatch(/\.column-popover:popover-open[^{]*{[^}]*display:\s*flex/s);
  });

  test('the trace fullscreen action remains focusable before hover', async () => {
    const el = await mountList();
    (el as any).colIdxMap = { timestamp: 0, trace_id: 1, kind: 2 };
    const host = document.createElement('div');
    render((el as any).logItemCol({ data: ['2024-01-01T00:00:00Z', 'trace-1', 'server'] }, 'timestamp'), host);

    const button = host.querySelector<HTMLButtonElement>('button[data-tippy-content="Open trace fullscreen"]');
    expect(button).not.toBeNull();
    expect(button?.classList.contains('hidden')).toBe(false);
    expect(button?.className).toContain('focus-visible:opacity-100');
  });

  test('the selected view has a strong non-fill boundary', async () => {
    const el = await mountList();
    const selected = el.querySelector<HTMLButtonElement>('button[aria-label="Tree view"]');
    const group = selected?.parentElement;

    expect(group?.className).toContain('border-strokeStrong');
    expect(selected?.className).toContain('ring-strokeStrong');
  });

  test('new-row feedback is static rather than a recurring animation', async () => {
    const el = await mountList();
    const styles = el.querySelector('style')?.textContent ?? '';

    expect(styles).not.toContain('@keyframes fadeBg');
    expect(styles).not.toContain('@keyframes pulseIndicator');
  });
});
