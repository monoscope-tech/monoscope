// A deploy replaces every content-hashed chunk: `emptyOutDir` removes the previous
// build's files, and `hashedAssetMiddleware` deliberately 404s a hash a replica does
// not have (serving mismatched bytes would poison the CDN for a year). So a tab left
// open across a deploy — which is how people use a dashboard — 404s the moment it
// lazily imports a chunk, and the page silently loses whatever that chunk powered.
//
// Reload once to pick up HTML referencing the current build.

const KEY = 'monoscope:stale-chunk-reload';

/**
 * True if we should reload now, recording the attempt. False while a previous reload
 * is still within `cooldownMs`.
 *
 * The cooldown is a timestamp rather than a "already reloaded" flag on purpose: we
 * deploy several times a day, and a flag would leave a long-lived tab permanently
 * unguarded after its first reload. A timestamp still bounds a genuinely dead chunk
 * to one reload per cooldown, and that case self-terminates anyway — the reloaded
 * HTML references chunks that exist.
 */
export function shouldReloadForStaleChunk(
  now: number,
  storage: Pick<Storage, 'getItem' | 'setItem'>,
  cooldownMs = 60_000
): boolean {
  let last: number | null = null;
  try {
    last = Number(storage.getItem(KEY)) || null;
  } catch {
    return false; // storage disabled (private mode, blocked cookies): never loop
  }
  if (last !== null && now - last < cooldownMs) return false;
  try {
    storage.setItem(KEY, String(now));
  } catch {
    return false;
  }
  return true;
}

export function installStaleChunkReload(): void {
  window.addEventListener('vite:preloadError', event => {
    // Without this Vite rethrows, and the 404 surfaces as an unhandled rejection —
    // which is how these reached the issue list in the first place.
    event.preventDefault();
    if (shouldReloadForStaleChunk(Date.now(), sessionStorage)) window.location.reload();
  });
}
