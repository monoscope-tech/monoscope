/**
 * Original file: /npm/htmx-ext-preload@2.1.1/preload.js
 *
 * Do NOT use SRI with dynamically generated files! More information: https://www.jsdelivr.com/using-sri-with-dynamic-files
 */
;(function () {
  htmx.defineExtension('preload', {
    onEvent: function (e, t) {
      if (e === 'htmx:afterProcessNode') {
        const n = t.target || t.detail.elt
        const r = [...(n.hasAttribute('preload') ? [n] : []), ...n.querySelectorAll('[preload]')]
        r.forEach(function (e) {
          i(e)
          e.querySelectorAll('[href],[hx-get],[data-hx-get]').forEach(i)
        })
        return
      }
      if (e === 'htmx:beforeRequest') {
        const o = t.detail.requestConfig.headers
        if (!('HX-Preloaded' in o && o['HX-Preloaded'] === 'true')) {
          return
        }
        t.preventDefault()
        const a = t.detail.xhr
        a.onload = function () {
          s(t.detail.elt, a.responseText)
        }
        a.onerror = null
        a.onabort = null
        a.ontimeout = null
        a.send()
      }
    },
  })
  function i(t) {
    if (t.preloadState !== undefined) {
      return
    }
    if (!l(t)) {
      return
    }
    if (t instanceof HTMLFormElement) {
      const o = t
      if (!((o.hasAttribute('method') && o.method === 'get') || o.hasAttribute('hx-get') || o.hasAttribute('hx-data-get'))) {
        return
      }
      for (let e = 0; e < o.elements.length; e++) {
        const a = o.elements.item(e)
        i(a)
        a.labels.forEach(i)
      }
      return
    }
    let e = g(t, 'preload')
    t.preloadAlways = e && e.includes('always')
    if (t.preloadAlways) {
      e = e.replace('always', '').trim()
    }
    let n = e || 'mousedown'
    const r = n === 'mouseover'
    t.addEventListener(n, u(t, r))
    if (n === 'mousedown' || n === 'mouseover') {
      t.addEventListener('touchstart', u(t))
    }
    if (n === 'mouseover') {
      t.addEventListener('mouseout', function (e) {
        if (e.target === t && t.preloadState === 'TIMEOUT') {
          t.preloadState = 'READY'
        }
      })
    }
    t.preloadState = 'READY'
    htmx.trigger(t, 'preload:init')
  }
  function u(t, n = false) {
    return function () {
      if (t.preloadState !== 'READY') {
        return
      }
      if (n) {
        t.preloadState = 'TIMEOUT'
        const e = 100
        window.setTimeout(function () {
          if (t.preloadState === 'TIMEOUT') {
            t.preloadState = 'READY'
            r(t)
          }
        }, e)
        return
      }
      r(t)
    }
  }
  function r(n) {
    if (n.preloadState !== 'READY') {
      return
    }
    n.preloadState = 'LOADING'
    const e = n.getAttribute('hx-get') || n.getAttribute('data-hx-get')
    if (e) {
      m(e, n)
      return
    }
    const t = g(n, 'hx-boost') === 'true'
    if (n.hasAttribute('href')) {
      const r = n.getAttribute('href')
      if (t) {
        m(r, n)
      } else {
        h(r, n)
      }
      return
    }
    if (p(n)) {
      const r = n.form.getAttribute('action') || n.form.getAttribute('hx-get') || n.form.getAttribute('data-hx-get')
      const o = htmx.values(n.form)
      const a = !(n.form.getAttribute('hx-get') || n.form.getAttribute('data-hx-get') || t)
      const i = a ? h : m
      if (n.type === 'submit') {
        i(r, n.form, o)
        return
      }
      const u = n.name || n.control.name
      if (n.tagName === 'SELECT') {
        Array.from(n.options).forEach(e => {
          if (e.selected) return
          o.set(u, e.value)
          const t = d(n.form, o)
          i(r, n.form, t)
        })
        return
      }
      const s = n.getAttribute('type') || n.control.getAttribute('type')
      const l = n.value || n.control?.value
      if (s === 'radio') {
        o.set(u, l)
      } else if (s === 'checkbox') {
        const c = o.getAll(u)
        if (c.includes(l)) {
          o[u] = c.filter(e => e !== l)
        } else {
          o.append(u, l)
        }
      }
      const f = d(n.form, o)
      i(r, n.form, f)
      return
    }
  }
  function d(e, t) {
    const n = e.elements
    const r = new FormData()
    for (let e = 0; e < n.length; e++) {
      const o = n.item(e)
      if (t.has(o.name) && o.tagName === 'SELECT') {
        r.append(o.name, t.get(o.name))
        continue
      }
      if (t.has(o.name) && t.getAll(o.name).includes(o.value)) {
        r.append(o.name, o.value)
      }
    }
    return r
  }
  function m(e, t, n = undefined) {
    htmx.ajax('GET', e, { source: t, values: n, headers: { 'HX-Preloaded': 'true' } })
  }
  function h(e, t, n = undefined) {
    const r = new XMLHttpRequest()
    if (n) {
      e += '?' + new URLSearchParams(n.entries()).toString()
    }
    r.open('GET', e)
    r.setRequestHeader('HX-Preloaded', 'true')
    r.onload = function () {
      s(t, r.responseText)
    }
    r.send()
  }
  function s(e, t) {
    e.preloadState = e.preloadAlways ? 'READY' : 'DONE'
    if (g(e, 'preload-images') === 'true') {
      document.createElement('div').innerHTML = t
    }
  }
  function g(e, t) {
    if (e == undefined) {
      return undefined
    }
    return e.getAttribute(t) || e.getAttribute('data-' + t) || g(e.parentElement, t)
  }
  function l(e) {
    const n = ['href', 'hx-get', 'data-hx-get']
    const t = t => n.some(e => t.hasAttribute(e)) || t.method === 'get'
    const r = e.form instanceof HTMLFormElement && t(e.form) && p(e)
    if (!t(e) && !r) {
      return false
    }
    if (e instanceof HTMLInputElement && e.closest('label')) {
      return false
    }
    return true
  }
  function p(e) {
    if (e instanceof HTMLInputElement || e instanceof HTMLButtonElement) {
      const t = e.getAttribute('type')
      return ['checkbox', 'radio', 'submit'].includes(t)
    }
    if (e instanceof HTMLLabelElement) {
      return e.control && p(e.control)
    }
    return e instanceof HTMLSelectElement
  }
})()
