/**
 * Minified by jsDelivr using Terser v5.37.0.
 * Original file: /npm/htmx-ext-preload@2.1.0/preload.js
 *
 * Do NOT use SRI with dynamically generated files! More information: https://www.jsdelivr.com/using-sri-with-dynamic-files
 */
!(function () {
  function e(n) {
    if (void 0 !== n.preloadState) return
    if (
      !(function (e) {
        const t = ['href', 'hx-get', 'data-hx-get'],
          n = e => t.some(t => e.hasAttribute(t)) || 'get' === e.method,
          o = e.form instanceof HTMLFormElement && n(e.form) && u(e)
        if (!n(e) && !o) return !1
        if (e instanceof HTMLInputElement && e.closest('label')) return !1
        return !0
      })(n)
    )
      return
    if (n instanceof HTMLFormElement) {
      const t = n
      if (!((t.hasAttribute('method') && 'get' === t.method) || t.hasAttribute('hx-get') || t.hasAttribute('hx-data-get'))) return
      for (let n = 0; n < t.elements.length; n++) {
        const o = t.elements.item(n)
        e(o), o.labels.forEach(e)
      }
      return
    }
    let o = l(n, 'preload')
    ;(n.preloadAlways = o && o.includes('always')), n.preloadAlways && (o = o.replace('always', '').trim())
    let r = o || 'mousedown'
    const a = 'mouseover' === r
    n.addEventListener(r, t(n, a)),
      ('mousedown' !== r && 'mouseover' !== r) || n.addEventListener('touchstart', t(n)),
      'mouseover' === r &&
        n.addEventListener('mouseout', function (e) {
          e.target === n && 'TIMEOUT' === n.preloadState && (n.preloadState = 'READY')
        }),
      (n.preloadState = 'READY'),
      htmx.trigger(n, 'preload:init')
  }
  function t(e, t = !1) {
    return function () {
      if ('READY' === e.preloadState)
        if (t) {
          e.preloadState = 'TIMEOUT'
          const t = 100
          window.setTimeout(function () {
            'TIMEOUT' === e.preloadState && ((e.preloadState = 'READY'), n(e))
          }, t)
        } else n(e)
    }
  }
  function n(e) {
    if ('READY' !== e.preloadState) return
    e.preloadState = 'LOADING'
    const t = e.getAttribute('hx-get') || e.getAttribute('data-hx-get')
    if (t) return void r(t, e)
    const n = 'true' === l(e, 'hx-boost')
    if (e.hasAttribute('href')) {
      const t = e.getAttribute('href')
      n ? r(t, e) : a(t, e)
    } else if (u(e)) {
      const t = e.form.getAttribute('action') || e.form.getAttribute('hx-get') || e.form.getAttribute('data-hx-get'),
        i = htmx.values(e.form),
        l = !(e.form.getAttribute('hx-get') || e.form.getAttribute('data-hx-get') || n) ? a : r
      if ('submit' === e.type) return void l(t, e.form, i)
      const u = e.name || e.control.name
      if ('SELECT' === e.tagName)
        return void Array.from(e.options).forEach(n => {
          if (n.selected) return
          i.set(u, n.value)
          const r = o(e.form, i)
          l(t, e.form, r)
        })
      const s = e.getAttribute('type') || e.control.getAttribute('type'),
        d = e.value || e.control?.value
      if ('radio' === s) i.set(u, d)
      else if ('checkbox' === s) {
        const e = i.getAll(u)
        e.includes(d) ? (i[u] = e.filter(e => e !== d)) : i.append(u, d)
      }
      const f = o(e.form, i)
      l(t, e.form, f)
    } else;
  }
  function o(e, t) {
    const n = e.elements,
      o = new FormData()
    for (let e = 0; e < n.length; e++) {
      const r = n.item(e)
      t.has(r.name) && 'SELECT' === r.tagName
        ? o.append(r.name, t.get(r.name))
        : t.has(r.name) && t.getAll(r.name).includes(r.value) && o.append(r.name, r.value)
    }
    return o
  }
  function r(e, t, n = void 0) {
    htmx.ajax('GET', e, { source: t, values: n, headers: { 'HX-Preloaded': 'true' } })
  }
  function a(e, t, n = void 0) {
    const o = new XMLHttpRequest()
    n && (e += '?' + new URLSearchParams(n.entries()).toString()),
      o.open('GET', e),
      o.setRequestHeader('HX-Preloaded', 'true'),
      (o.onload = function () {
        i(t, o.responseText)
      }),
      o.send()
  }
  function i(e, t) {
    ;(e.preloadState = e.preloadAlways ? 'READY' : 'DONE'), 'true' === l(e, 'preload-images') && (document.createElement('div').innerHTML = t)
  }
  function l(e, t) {
    if (null != e) return e.getAttribute(t) || e.getAttribute('data-' + t) || l(e.parentElement, t)
  }
  function u(e) {
    if (e instanceof HTMLInputElement || e instanceof HTMLButtonElement) {
      const t = e.getAttribute('type')
      return ['checkbox', 'radio', 'submit'].includes(t)
    }
    return e instanceof HTMLLabelElement ? e.control && u(e.control) : e instanceof HTMLSelectElement
  }
  htmx.defineExtension('preload', {
    onEvent: function (t, n) {
      if ('htmx:afterProcessNode' !== t) {
        if ('htmx:beforeRequest' === t) {
          const e = n.detail.requestConfig.headers
          if (!('HX-Preloaded' in e) || 'true' !== e['HX-Preloaded']) return
          n.preventDefault()
          const t = n.detail.xhr
          ;(t.onload = function () {
            i(n.detail.elt, t.responseText)
          }),
            (t.onerror = null),
            (t.onabort = null),
            (t.ontimeout = null),
            t.send()
        }
      } else {
        const t = n.target || n.detail.elt
        ;[...(t.hasAttribute('preload') ? [t] : []), ...t.querySelectorAll('[preload]')].forEach(function (t) {
          e(t), t.querySelectorAll('[href],[hx-get],[data-hx-get]').forEach(e)
        })
      }
    },
  })
})()
//# sourceMappingURL=/sm/e3ca72d32241e17ac871246864a944b2d08fe8dee06bb8651bddc2de6d0e7df9.map
