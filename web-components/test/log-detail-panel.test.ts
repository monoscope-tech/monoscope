// The detail panel used to strand users on a frozen three-dot loader: htmx's default sync
// strategy ("queue first") silently dropped a click made while another detail request was in
// flight, so the newly clicked row never loaded — and because the indicator was flagged on
// click but only ever cleared by a request that ran, the loader spun until a page reload.
// #log_details_container now carries hx-sync="this:replace" (server-side, see
// Pages.LogExplorer.Log.detailsPanel); these guard the client half of the contract.
import { describe, expect, test, vi, beforeEach } from 'vitest';
import { LogList } from '../src/log-list';

const mountPanelDom = () => {
  document.body.innerHTML = `
    <div id="log_details_container" hx-sync="this:replace">
      <span id="details_indicator" class="htmx-indicator"></span>
    </div>
    <div id="resizer-details_width-wrapper" class="hidden opacity-0 pointer-events-none"></div>
    <table><tbody><tr id="row"><td></td></tr></tbody></table>`;
};

const clickRow = (el: LogList, id: string) => {
  const tr = document.getElementById('row')!;
  (el as any).toggleLogRow({ currentTarget: tr }, [id, '2024-01-01T00:00:00Z', 'logs'], 'p1');
};

const loaderOn = () => document.querySelector('#details_indicator')!.classList.contains('htmx-request');

describe('detail panel loading indicator', () => {
  beforeEach(() => {
    mountPanelDom();
    (window as any).updateUrlState = () => {};
  });

  test('a rejected request still hands the loader back', async () => {
    // htmx rejects for a dropped/aborted request and on network failure. Any of those leaving
    // the class behind is the frozen-loader bug.
    (window as any).htmx = { ajax: () => Promise.reject(new Error('aborted')) };
    const el = new LogList();
    clickRow(el, 'a');
    expect(loaderOn()).toBe(true);
    await new Promise((r) => setTimeout(r, 0));
    expect(loaderOn()).toBe(false);
  });

  test('a superseded request does not clear the loader for the click that replaced it', async () => {
    // hx-sync="this:replace" aborts the in-flight request, so the loser settles *after* the
    // winner started. If the loser cleared the indicator, the winner would load with no loader.
    let rejectFirst: (e: Error) => void = () => {};
    const first = new Promise((_, rej) => (rejectFirst = rej));
    let call = 0;
    (window as any).htmx = { ajax: () => (call++ === 0 ? first : new Promise(() => {})) };

    const el = new LogList();
    clickRow(el, 'a');
    clickRow(el, 'b'); // supersedes
    rejectFirst(new Error('aborted'));
    await new Promise((r) => setTimeout(r, 0));

    expect(loaderOn()).toBe(true); // still loading 'b'
  });

  test('the indicator cleared is the one currently mounted, not a captured stale node', async () => {
    // The indicator is rendered inside the container this request innerHTML-swaps, so the
    // response replaces the node. Clearing a ref captured on click would leave the live one on.
    let resolveIt: (v: unknown) => void = () => {};
    (window as any).htmx = {
      ajax: () =>
        new Promise((res) => {
          resolveIt = res;
        }),
    };
    const el = new LogList();
    clickRow(el, 'a');
    expect(loaderOn()).toBe(true);

    // Simulate the swap: the container's children (indicator included) are replaced.
    document.querySelector('#log_details_container')!.innerHTML = '<span id="details_indicator" class="htmx-indicator htmx-request"></span>';
    resolveIt(undefined);
    await new Promise((r) => setTimeout(r, 0));

    expect(loaderOn()).toBe(false);
  });
});
