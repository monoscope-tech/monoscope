// Behaviour for Components.resizer_: a separator with data-resize-target sets that element's
// width by pointer drag or arrow keys, clamped to [data-min, data-max] (px, or vw) and to its
// parent's width. A finished resize persists to localStorage as `resizer-<data-resize-key>`
// and, when data-url-param is present, to the URL. Delegated, so swapped-in panels need no init.
const len = (v: string | undefined, fallback: number) =>
  !v ? fallback : v.endsWith('vw') ? (innerWidth * parseFloat(v)) / 100 : parseFloat(v);

const separatorOf = (e: Event) => (e.target as Element | null)?.closest?.<HTMLElement>('[data-resize-target]') ?? null;

function resize(sep: HTMLElement, width?: number, persist = false) {
  const target = document.getElementById(sep.dataset.resizeTarget ?? '');
  if (!target) return;
  // `||`: a display:contents parent (the log details wrapper) reports 0.
  const max = Math.min(target.parentElement?.clientWidth || Infinity, len(sep.dataset.max, Infinity));
  const w = Math.round(Math.max(len(sep.dataset.min, 0), Math.min(width ?? target.offsetWidth, max)));
  if (max < Infinity) sep.setAttribute('aria-valuemax', String(Math.round(max)));
  sep.setAttribute('aria-valuenow', String(w));
  sep.setAttribute('aria-valuetext', `${w} pixels`);
  if (width === undefined) return;
  target.style.width = `${w}px`;
  dispatchEvent(new Event('loglist-resize'));
  if (!persist) return;
  localStorage.setItem(`resizer-${sep.dataset.resizeKey}`, `${w}px`);
  if (sep.dataset.urlParam) updateUrlState(sep.dataset.urlParam, String(w));
}

const sign = (sep: HTMLElement) => (sep.dataset.resizeDirection === 'decrease' ? -1 : 1);

document.addEventListener('focusin', (e) => {
  const sep = separatorOf(e);
  if (sep) resize(sep);
});

document.addEventListener('keydown', (e) => {
  const sep = separatorOf(e);
  if (!sep || (e.key !== 'ArrowLeft' && e.key !== 'ArrowRight')) return;
  e.preventDefault();
  const target = document.getElementById(sep.dataset.resizeTarget ?? '');
  if (target) resize(sep, target.offsetWidth + (e.key === 'ArrowLeft' ? -10 : 10) * sign(sep), true);
});

document.addEventListener('pointerdown', (e) => {
  const sep = separatorOf(e);
  const target = sep && document.getElementById(sep.dataset.resizeTarget ?? '');
  if (!sep || !target) return;
  const [x0, w0] = [e.clientX, target.offsetWidth];
  const move = (m: PointerEvent) => resize(sep, w0 + (m.clientX - x0) * sign(sep));
  sep.setPointerCapture(e.pointerId);
  document.body.classList.add('select-none');
  sep.addEventListener('pointermove', move);
  sep.addEventListener(
    'lostpointercapture',
    () => {
      sep.removeEventListener('pointermove', move);
      document.body.classList.remove('select-none');
      resize(sep, target.offsetWidth, true);
    },
    { once: true }
  );
});
