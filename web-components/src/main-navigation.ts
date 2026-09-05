// A section owns its main URL, sibling routes in data-match, and their children.
// Match URL paths rather than labels so translations, query strings and fragments
// cannot clear the active section. Require a path boundary (e.g. /reports-old is
// not a child of /reports).
export function syncMainNavigation() {
  const current = window.location;
  document.querySelectorAll<HTMLAnchorElement>('#main-sidenav a.main-nav-link').forEach((link) => {
    const routes = [link.href, ...(link.dataset.match?.split(/\s+/).filter(Boolean) ?? [])];
    const active = routes.some((route) => {
      const url = new URL(route, current.origin);
      const path = url.pathname.replace(/\/$/, '');
      return url.origin === current.origin && (current.pathname === path || current.pathname.startsWith(`${path}/`));
    });
    link.classList.toggle('active', active);
    if (active) link.setAttribute('aria-current', 'page');
    else link.removeAttribute('aria-current');
  });
}

if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', syncMainNavigation, { once: true });
else syncMainNavigation();

// History updates precede the swap; repeat afterward because an OOB sidebar
// morph can overwrite the classes. Popstate covers browser back/forward too.
document.addEventListener('htmx:after:history:update', syncMainNavigation);
document.addEventListener('htmx:after:swap', syncMainNavigation);
window.addEventListener('popstate', syncMainNavigation);
