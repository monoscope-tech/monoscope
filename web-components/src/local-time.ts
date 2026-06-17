import { format, isValid } from 'date-fns';

// <local-time datetime="2026-06-16T03:38:47.176Z">Jun 16, 03:38:47</local-time>
//
// Server pages render timestamps in UTC (Haskell only knows the instant, not the
// viewer's zone). This element reformats the ISO `datetime` into the browser's
// local timezone — on insertion and on attribute change, so it survives HTMX
// swaps/morphs — so detail views match the virtual log list (which formats
// client-side via date-fns). The pre-rendered UTC text is the no-JS fallback.
// Optional `format` attribute overrides the pattern.
class LocalTime extends HTMLElement {
  static observedAttributes = ['datetime', 'format'];

  render() {
    const iso = this.getAttribute('datetime');
    if (!iso) return;
    const date = new Date(iso);
    if (isValid(date)) this.textContent = format(date, this.getAttribute('format') || 'MMM dd, HH:mm:ss');
  }

  connectedCallback() {
    this.render();
  }
  attributeChangedCallback(_name: string, oldValue: string | null, newValue: string | null) {
    if (this.isConnected && oldValue !== newValue) this.render();
  }
}

if (!customElements.get('local-time')) customElements.define('local-time', LocalTime);
