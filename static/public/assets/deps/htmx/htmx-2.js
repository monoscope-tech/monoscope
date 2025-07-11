var htmx = (function () {
  'use strict'
  const Q = {
    onLoad: null,
    process: null,
    on: null,
    off: null,
    trigger: null,
    ajax: null,
    find: null,
    findAll: null,
    closest: null,
    values: function (e, t) {
      const n = cn(e, t || 'post')
      return n.values
    },
    remove: null,
    addClass: null,
    removeClass: null,
    toggleClass: null,
    takeClass: null,
    swap: null,
    defineExtension: null,
    removeExtension: null,
    logAll: null,
    logNone: null,
    logger: null,
    config: {
      historyEnabled: true,
      historyCacheSize: 10,
      refreshOnHistoryMiss: false,
      defaultSwapStyle: 'innerHTML',
      defaultSwapDelay: 0,
      defaultSettleDelay: 20,
      includeIndicatorStyles: true,
      indicatorClass: 'htmx-indicator',
      requestClass: 'htmx-request',
      addedClass: 'htmx-added',
      settlingClass: 'htmx-settling',
      swappingClass: 'htmx-swapping',
      allowEval: true,
      allowScriptTags: true,
      inlineScriptNonce: '',
      inlineStyleNonce: '',
      attributesToSettle: ['class', 'style', 'width', 'height'],
      withCredentials: false,
      timeout: 0,
      wsReconnectDelay: 'full-jitter',
      wsBinaryType: 'blob',
      disableSelector: '[hx-disable], [data-hx-disable]',
      scrollBehavior: 'instant',
      defaultFocusScroll: false,
      getCacheBusterParam: false,
      globalViewTransitions: false,
      methodsThatUseUrlParams: ['get', 'delete'],
      selfRequestsOnly: true,
      ignoreTitle: false,
      scrollIntoViewOnBoost: true,
      triggerSpecsCache: null,
      disableInheritance: false,
      responseHandling: [
        { code: '204', swap: false },
        { code: '[23]..', swap: true },
        { code: '[45]..', swap: false, error: true },
      ],
      allowNestedOobSwaps: true,
    },
    parseInterval: null,
    _: null,
    version: '2.0.4',
  }
  Q.onLoad = j
  Q.process = kt
  Q.on = ye
  Q.off = be
  Q.trigger = he
  Q.ajax = Rn
  Q.find = u
  Q.findAll = x
  Q.closest = g
  Q.remove = z
  Q.addClass = K
  Q.removeClass = G
  Q.toggleClass = W
  Q.takeClass = Z
  Q.swap = $e
  Q.defineExtension = Fn
  Q.removeExtension = Bn
  Q.logAll = V
  Q.logNone = _
  Q.parseInterval = d
  Q._ = e
  const n = {
    addTriggerHandler: St,
    bodyContains: le,
    canAccessLocalStorage: B,
    findThisElement: Se,
    filterValues: hn,
    swap: $e,
    hasAttribute: s,
    getAttributeValue: te,
    getClosestAttributeValue: re,
    getClosestMatch: o,
    getExpressionVars: En,
    getHeaders: fn,
    getInputValues: cn,
    getInternalData: ie,
    getSwapSpecification: gn,
    getTriggerSpecs: st,
    getTarget: Ee,
    makeFragment: P,
    mergeObjects: ce,
    makeSettleInfo: xn,
    oobSwap: He,
    querySelectorExt: ae,
    settleImmediately: Kt,
    shouldCancel: ht,
    triggerEvent: he,
    triggerErrorEvent: fe,
    withExtensions: Ft,
  }
  const r = ['get', 'post', 'put', 'delete', 'patch']
  const H = r
    .map(function (e) {
      return '[hx-' + e + '], [data-hx-' + e + ']'
    })
    .join(', ')
  function d(e) {
    if (e == undefined) {
      return undefined
    }
    let t = NaN
    if (e.slice(-2) == 'ms') {
      t = parseFloat(e.slice(0, -2))
    } else if (e.slice(-1) == 's') {
      t = parseFloat(e.slice(0, -1)) * 1e3
    } else if (e.slice(-1) == 'm') {
      t = parseFloat(e.slice(0, -1)) * 1e3 * 60
    } else {
      t = parseFloat(e)
    }
    return isNaN(t) ? undefined : t
  }
  function ee(e, t) {
    return e instanceof Element && e.getAttribute(t)
  }
  function s(e, t) {
    return !!e.hasAttribute && (e.hasAttribute(t) || e.hasAttribute('data-' + t))
  }
  function te(e, t) {
    return ee(e, t) || ee(e, 'data-' + t)
  }
  function c(e) {
    const t = e.parentElement
    if (!t && e.parentNode instanceof ShadowRoot) return e.parentNode
    return t
  }
  function ne() {
    return document
  }
  function m(e, t) {
    return e.getRootNode ? e.getRootNode({ composed: t }) : ne()
  }
  function o(e, t) {
    while (e && !t(e)) {
      e = c(e)
    }
    return e || null
  }
  function i(e, t, n) {
    const r = te(t, n)
    const o = te(t, 'hx-disinherit')
    var i = te(t, 'hx-inherit')
    if (e !== t) {
      if (Q.config.disableInheritance) {
        if (i && (i === '*' || i.split(' ').indexOf(n) >= 0)) {
          return r
        } else {
          return null
        }
      }
      if (o && (o === '*' || o.split(' ').indexOf(n) >= 0)) {
        return 'unset'
      }
    }
    return r
  }
  function re(t, n) {
    let r = null
    o(t, function (e) {
      return !!(r = i(t, ue(e), n))
    })
    if (r !== 'unset') {
      return r
    }
  }
  function h(e, t) {
    const n =
      e instanceof Element && (e.matches || e.matchesSelector || e.msMatchesSelector || e.mozMatchesSelector || e.webkitMatchesSelector || e.oMatchesSelector)
    return !!n && n.call(e, t)
  }
  function T(e) {
    const t = /<([a-z][^\/\0>\x20\t\r\n\f]*)/i
    const n = t.exec(e)
    if (n) {
      return n[1].toLowerCase()
    } else {
      return ''
    }
  }
  function q(e) {
    const t = new DOMParser()
    return t.parseFromString(e, 'text/html')
  }
  function L(e, t) {
    while (t.childNodes.length > 0) {
      e.append(t.childNodes[0])
    }
  }
  function A(e) {
    const t = ne().createElement('script')
    se(e.attributes, function (e) {
      t.setAttribute(e.name, e.value)
    })
    t.textContent = e.textContent
    t.async = false
    if (Q.config.inlineScriptNonce) {
      t.nonce = Q.config.inlineScriptNonce
    }
    return t
  }
  function N(e) {
    return e.matches('script') && (e.type === 'text/javascript' || e.type === 'module' || e.type === '')
  }
  function I(e) {
    Array.from(e.querySelectorAll('script')).forEach(e => {
      if (N(e)) {
        const t = A(e)
        const n = e.parentNode
        try {
          n.insertBefore(t, e)
        } catch (e) {
          O(e)
        } finally {
          e.remove()
        }
      }
    })
  }
  function P(e) {
    const t = e.replace(/<head(\s[^>]*)?>[\s\S]*?<\/head>/i, '')
    const n = T(t)
    let r
    if (n === 'html') {
      r = new DocumentFragment()
      const i = q(e)
      L(r, i.body)
      r.title = i.title
    } else if (n === 'body') {
      r = new DocumentFragment()
      const i = q(t)
      L(r, i.body)
      r.title = i.title
    } else {
      const i = q('<body><template class="internal-htmx-wrapper">' + t + '</template></body>')
      r = i.querySelector('template').content
      r.title = i.title
      var o = r.querySelector('title')
      if (o && o.parentNode === r) {
        o.remove()
        r.title = o.innerText
      }
    }
    if (r) {
      if (Q.config.allowScriptTags) {
        I(r)
      } else {
        r.querySelectorAll('script').forEach(e => e.remove())
      }
    }
    return r
  }
  function oe(e) {
    if (e) {
      e()
    }
  }
  function t(e, t) {
    return Object.prototype.toString.call(e) === '[object ' + t + ']'
  }
  function k(e) {
    return typeof e === 'function'
  }
  function D(e) {
    return t(e, 'Object')
  }
  function ie(e) {
    const t = 'htmx-internal-data'
    let n = e[t]
    if (!n) {
      n = e[t] = {}
    }
    return n
  }
  function M(t) {
    const n = []
    if (t) {
      for (let e = 0; e < t.length; e++) {
        n.push(t[e])
      }
    }
    return n
  }
  function se(t, n) {
    if (t) {
      for (let e = 0; e < t.length; e++) {
        n(t[e])
      }
    }
  }
  function X(e) {
    const t = e.getBoundingClientRect()
    const n = t.top
    const r = t.bottom
    return n < window.innerHeight && r >= 0
  }
  function le(e) {
    return e.getRootNode({ composed: true }) === document
  }
  function F(e) {
    return e.trim().split(/\s+/)
  }
  function ce(e, t) {
    for (const n in t) {
      if (t.hasOwnProperty(n)) {
        e[n] = t[n]
      }
    }
    return e
  }
  function S(e) {
    try {
      return JSON.parse(e)
    } catch (e) {
      O(e)
      return null
    }
  }
  function B() {
    const e = 'htmx:localStorageTest'
    try {
      localStorage.setItem(e, e)
      localStorage.removeItem(e)
      return true
    } catch (e) {
      return false
    }
  }
  function U(t) {
    try {
      const e = new URL(t)
      if (e) {
        t = e.pathname + e.search
      }
      if (!/^\/$/.test(t)) {
        t = t.replace(/\/+$/, '')
      }
      return t
    } catch (e) {
      return t
    }
  }
  function e(e) {
    return vn(ne().body, function () {
      return eval(e)
    })
  }
  function j(t) {
    const e = Q.on('htmx:load', function (e) {
      t(e.detail.elt)
    })
    return e
  }
  function V() {
    Q.logger = function (e, t, n) {
      if (console) {
        console.log(t, e, n)
      }
    }
  }
  function _() {
    Q.logger = null
  }
  function u(e, t) {
    if (typeof e !== 'string') {
      return e.querySelector(t)
    } else {
      return u(ne(), e)
    }
  }
  function x(e, t) {
    if (typeof e !== 'string') {
      return e.querySelectorAll(t)
    } else {
      return x(ne(), e)
    }
  }
  function E() {
    return window
  }
  function z(e, t) {
    e = y(e)
    if (t) {
      E().setTimeout(function () {
        z(e)
        e = null
      }, t)
    } else {
      c(e).removeChild(e)
    }
  }
  function ue(e) {
    return e instanceof Element ? e : null
  }
  function $(e) {
    return e instanceof HTMLElement ? e : null
  }
  function J(e) {
    return typeof e === 'string' ? e : null
  }
  function f(e) {
    return e instanceof Element || e instanceof Document || e instanceof DocumentFragment ? e : null
  }
  function K(e, t, n) {
    e = ue(y(e))
    if (!e) {
      return
    }
    if (n) {
      E().setTimeout(function () {
        K(e, t)
        e = null
      }, n)
    } else {
      e.classList && e.classList.add(t)
    }
  }
  function G(e, t, n) {
    let r = ue(y(e))
    if (!r) {
      return
    }
    if (n) {
      E().setTimeout(function () {
        G(r, t)
        r = null
      }, n)
    } else {
      if (r.classList) {
        r.classList.remove(t)
        if (r.classList.length === 0) {
          r.removeAttribute('class')
        }
      }
    }
  }
  function W(e, t) {
    e = y(e)
    e.classList.toggle(t)
  }
  function Z(e, t) {
    e = y(e)
    se(e.parentElement.children, function (e) {
      G(e, t)
    })
    K(ue(e), t)
  }
  function g(e, t) {
    e = ue(y(e))
    if (e && e.closest) {
      return e.closest(t)
    } else {
      do {
        if (e == null || h(e, t)) {
          return e
        }
      } while ((e = e && ue(c(e))))
      return null
    }
  }
  function l(e, t) {
    return e.substring(0, t.length) === t
  }
  function Y(e, t) {
    return e.substring(e.length - t.length) === t
  }
  function ge(e) {
    const t = e.trim()
    if (l(t, '<') && Y(t, '/>')) {
      return t.substring(1, t.length - 2)
    } else {
      return t
    }
  }
  function p(t, r, n) {
    if (r.indexOf('global ') === 0) {
      return p(t, r.slice(7), true)
    }
    t = y(t)
    const o = []
    {
      let t = 0
      let n = 0
      for (let e = 0; e < r.length; e++) {
        const l = r[e]
        if (l === ',' && t === 0) {
          o.push(r.substring(n, e))
          n = e + 1
          continue
        }
        if (l === '<') {
          t++
        } else if (l === '/' && e < r.length - 1 && r[e + 1] === '>') {
          t--
        }
      }
      if (n < r.length) {
        o.push(r.substring(n))
      }
    }
    const i = []
    const s = []
    while (o.length > 0) {
      const r = ge(o.shift())
      let e
      if (r.indexOf('closest ') === 0) {
        e = g(ue(t), ge(r.substr(8)))
      } else if (r.indexOf('find ') === 0) {
        e = u(f(t), ge(r.substr(5)))
      } else if (r === 'next' || r === 'nextElementSibling') {
        e = ue(t).nextElementSibling
      } else if (r.indexOf('next ') === 0) {
        e = pe(t, ge(r.substr(5)), !!n)
      } else if (r === 'previous' || r === 'previousElementSibling') {
        e = ue(t).previousElementSibling
      } else if (r.indexOf('previous ') === 0) {
        e = me(t, ge(r.substr(9)), !!n)
      } else if (r === 'document') {
        e = document
      } else if (r === 'window') {
        e = window
      } else if (r === 'body') {
        e = document.body
      } else if (r === 'root') {
        e = m(t, !!n)
      } else if (r === 'host') {
        e = t.getRootNode().host
      } else {
        s.push(r)
      }
      if (e) {
        i.push(e)
      }
    }
    if (s.length > 0) {
      const e = s.join(',')
      const c = f(m(t, !!n))
      i.push(...M(c.querySelectorAll(e)))
    }
    return i
  }
  var pe = function (t, e, n) {
    const r = f(m(t, n)).querySelectorAll(e)
    for (let e = 0; e < r.length; e++) {
      const o = r[e]
      if (o.compareDocumentPosition(t) === Node.DOCUMENT_POSITION_PRECEDING) {
        return o
      }
    }
  }
  var me = function (t, e, n) {
    const r = f(m(t, n)).querySelectorAll(e)
    for (let e = r.length - 1; e >= 0; e--) {
      const o = r[e]
      if (o.compareDocumentPosition(t) === Node.DOCUMENT_POSITION_FOLLOWING) {
        return o
      }
    }
  }
  function ae(e, t) {
    if (typeof e !== 'string') {
      return p(e, t)[0]
    } else {
      return p(ne().body, e)[0]
    }
  }
  function y(e, t) {
    if (typeof e === 'string') {
      return u(f(t) || document, e)
    } else {
      return e
    }
  }
  function xe(e, t, n, r) {
    if (k(t)) {
      return { target: ne().body, event: J(e), listener: t, options: n }
    } else {
      return { target: y(e), event: J(t), listener: n, options: r }
    }
  }
  function ye(t, n, r, o) {
    Vn(function () {
      const e = xe(t, n, r, o)
      e.target.addEventListener(e.event, e.listener, e.options)
    })
    const e = k(n)
    return e ? n : r
  }
  function be(t, n, r) {
    Vn(function () {
      const e = xe(t, n, r)
      e.target.removeEventListener(e.event, e.listener)
    })
    return k(n) ? n : r
  }
  const ve = ne().createElement('output')
  function we(e, t) {
    const n = re(e, t)
    if (n) {
      if (n === 'this') {
        return [Se(e, t)]
      } else {
        const r = p(e, n)
        if (r.length === 0) {
          O('The selector "' + n + '" on ' + t + ' returned no matches!')
          return [ve]
        } else {
          return r
        }
      }
    }
  }
  function Se(e, t) {
    return ue(
      o(e, function (e) {
        return te(ue(e), t) != null
      }),
    )
  }
  function Ee(e) {
    const t = re(e, 'hx-target')
    if (t) {
      if (t === 'this') {
        return Se(e, 'hx-target')
      } else {
        return ae(e, t)
      }
    } else {
      const n = ie(e)
      if (n.boosted) {
        return ne().body
      } else {
        return e
      }
    }
  }
  function Ce(t) {
    const n = Q.config.attributesToSettle
    for (let e = 0; e < n.length; e++) {
      if (t === n[e]) {
        return true
      }
    }
    return false
  }
  function Oe(t, n) {
    se(t.attributes, function (e) {
      if (!n.hasAttribute(e.name) && Ce(e.name)) {
        t.removeAttribute(e.name)
      }
    })
    se(n.attributes, function (e) {
      if (Ce(e.name)) {
        t.setAttribute(e.name, e.value)
      }
    })
  }
  function Re(t, e) {
    const n = Un(e)
    for (let e = 0; e < n.length; e++) {
      const r = n[e]
      try {
        if (r.isInlineSwap(t)) {
          return true
        }
      } catch (e) {
        O(e)
      }
    }
    return t === 'outerHTML'
  }
  function He(e, o, i, t) {
    t = t || ne()
    let n = '#' + ee(o, 'id')
    let s = 'outerHTML'
    if (e === 'true') {
    } else if (e.indexOf(':') > 0) {
      s = e.substring(0, e.indexOf(':'))
      n = e.substring(e.indexOf(':') + 1)
    } else {
      s = e
    }
    o.removeAttribute('hx-swap-oob')
    o.removeAttribute('data-hx-swap-oob')
    const r = p(t, n, false)
    if (r) {
      se(r, function (e) {
        let t
        const n = o.cloneNode(true)
        t = ne().createDocumentFragment()
        t.appendChild(n)
        if (!Re(s, e)) {
          t = f(n)
        }
        const r = { shouldSwap: true, target: e, fragment: t }
        if (!he(e, 'htmx:oobBeforeSwap', r)) return
        e = r.target
        if (r.shouldSwap) {
          qe(t)
          _e(s, e, e, t, i)
          Te()
        }
        se(i.elts, function (e) {
          he(e, 'htmx:oobAfterSwap', r)
        })
      })
      o.parentNode.removeChild(o)
    } else {
      o.parentNode.removeChild(o)
      fe(ne().body, 'htmx:oobErrorNoTarget', { content: o })
    }
    return e
  }
  function Te() {
    const e = u('#--htmx-preserve-pantry--')
    if (e) {
      for (const t of [...e.children]) {
        const n = u('#' + t.id)
        n.parentNode.moveBefore(t, n)
        n.remove()
      }
      e.remove()
    }
  }
  function qe(e) {
    se(x(e, '[hx-preserve], [data-hx-preserve]'), function (e) {
      const t = te(e, 'id')
      const n = ne().getElementById(t)
      if (n != null) {
        if (e.moveBefore) {
          let e = u('#--htmx-preserve-pantry--')
          if (e == null) {
            ne().body.insertAdjacentHTML('afterend', "<div id='--htmx-preserve-pantry--'></div>")
            e = u('#--htmx-preserve-pantry--')
          }
          e.moveBefore(n, null)
        } else {
          e.parentNode.replaceChild(n, e)
        }
      }
    })
  }
  function Le(l, e, c) {
    se(e.querySelectorAll('[id]'), function (t) {
      const n = ee(t, 'id')
      if (n && n.length > 0) {
        const r = n.replace("'", "\\'")
        const o = t.tagName.replace(':', '\\:')
        const e = f(l)
        const i = e && e.querySelector(o + "[id='" + r + "']")
        if (i && i !== e) {
          const s = t.cloneNode()
          Oe(t, i)
          c.tasks.push(function () {
            Oe(t, s)
          })
        }
      }
    })
  }
  function Ae(e) {
    return function () {
      G(e, Q.config.addedClass)
      kt(ue(e))
      Ne(f(e))
      he(e, 'htmx:load')
    }
  }
  function Ne(e) {
    const t = '[autofocus]'
    const n = $(h(e, t) ? e : e.querySelector(t))
    if (n != null) {
      n.focus()
    }
  }
  function a(e, t, n, r) {
    Le(e, n, r)
    while (n.childNodes.length > 0) {
      const o = n.firstChild
      K(ue(o), Q.config.addedClass)
      e.insertBefore(o, t)
      if (o.nodeType !== Node.TEXT_NODE && o.nodeType !== Node.COMMENT_NODE) {
        r.tasks.push(Ae(o))
      }
    }
  }
  function Ie(e, t) {
    let n = 0
    while (n < e.length) {
      t = ((t << 5) - t + e.charCodeAt(n++)) | 0
    }
    return t
  }
  function Pe(t) {
    let n = 0
    if (t.attributes) {
      for (let e = 0; e < t.attributes.length; e++) {
        const r = t.attributes[e]
        if (r.value) {
          n = Ie(r.name, n)
          n = Ie(r.value, n)
        }
      }
    }
    return n
  }
  function ke(t) {
    const n = ie(t)
    if (n.onHandlers) {
      for (let e = 0; e < n.onHandlers.length; e++) {
        const r = n.onHandlers[e]
        be(t, r.event, r.listener)
      }
      delete n.onHandlers
    }
  }
  function De(e) {
    const t = ie(e)
    if (t.timeout) {
      clearTimeout(t.timeout)
    }
    if (t.listenerInfos) {
      se(t.listenerInfos, function (e) {
        if (e.on) {
          be(e.on, e.trigger, e.listener)
        }
      })
    }
    ke(e)
    se(Object.keys(t), function (e) {
      if (e !== 'firstInitCompleted') delete t[e]
    })
  }
  function b(e) {
    he(e, 'htmx:beforeCleanupElement')
    De(e)
    if (e.children) {
      se(e.children, function (e) {
        b(e)
      })
    }
  }
  function Me(t, e, n) {
    if (t instanceof Element && t.tagName === 'BODY') {
      return Ve(t, e, n)
    }
    let r
    const o = t.previousSibling
    const i = c(t)
    if (!i) {
      return
    }
    a(i, t, e, n)
    if (o == null) {
      r = i.firstChild
    } else {
      r = o.nextSibling
    }
    n.elts = n.elts.filter(function (e) {
      return e !== t
    })
    while (r && r !== t) {
      if (r instanceof Element) {
        n.elts.push(r)
      }
      r = r.nextSibling
    }
    b(t)
    if (t instanceof Element) {
      t.remove()
    } else {
      t.parentNode.removeChild(t)
    }
  }
  function Xe(e, t, n) {
    return a(e, e.firstChild, t, n)
  }
  function Fe(e, t, n) {
    return a(c(e), e, t, n)
  }
  function Be(e, t, n) {
    return a(e, null, t, n)
  }
  function Ue(e, t, n) {
    return a(c(e), e.nextSibling, t, n)
  }
  function je(e) {
    b(e)
    const t = c(e)
    if (t) {
      return t.removeChild(e)
    }
  }
  function Ve(e, t, n) {
    const r = e.firstChild
    a(e, r, t, n)
    if (r) {
      while (r.nextSibling) {
        b(r.nextSibling)
        e.removeChild(r.nextSibling)
      }
      b(r)
      e.removeChild(r)
    }
  }
  function _e(t, e, n, r, o) {
    switch (t) {
      case 'none':
        return
      case 'outerHTML':
        Me(n, r, o)
        return
      case 'afterbegin':
        Xe(n, r, o)
        return
      case 'beforebegin':
        Fe(n, r, o)
        return
      case 'beforeend':
        Be(n, r, o)
        return
      case 'afterend':
        Ue(n, r, o)
        return
      case 'delete':
        je(n)
        return
      default:
        var i = Un(e)
        for (let e = 0; e < i.length; e++) {
          const s = i[e]
          try {
            const l = s.handleSwap(t, n, r, o)
            if (l) {
              if (Array.isArray(l)) {
                for (let e = 0; e < l.length; e++) {
                  const c = l[e]
                  if (c.nodeType !== Node.TEXT_NODE && c.nodeType !== Node.COMMENT_NODE) {
                    o.tasks.push(Ae(c))
                  }
                }
              }
              return
            }
          } catch (e) {
            O(e)
          }
        }
        if (t === 'innerHTML') {
          Ve(n, r, o)
        } else {
          _e(Q.config.defaultSwapStyle, e, n, r, o)
        }
    }
  }
  function ze(e, n, r) {
    var t = x(e, '[hx-swap-oob], [data-hx-swap-oob]')
    se(t, function (e) {
      if (Q.config.allowNestedOobSwaps || e.parentElement === null) {
        const t = te(e, 'hx-swap-oob')
        if (t != null) {
          He(t, e, n, r)
        }
      } else {
        e.removeAttribute('hx-swap-oob')
        e.removeAttribute('data-hx-swap-oob')
      }
    })
    return t.length > 0
  }
  function $e(e, t, r, o) {
    if (!o) {
      o = {}
    }
    e = y(e)
    const i = o.contextElement ? m(o.contextElement, false) : ne()
    const n = document.activeElement
    let s = {}
    try {
      s = { elt: n, start: n ? n.selectionStart : null, end: n ? n.selectionEnd : null }
    } catch (e) {}
    const l = xn(e)
    if (r.swapStyle === 'textContent') {
      e.textContent = t
    } else {
      let n = P(t)
      l.title = n.title
      if (o.selectOOB) {
        const u = o.selectOOB.split(',')
        for (let t = 0; t < u.length; t++) {
          const a = u[t].split(':', 2)
          let e = a[0].trim()
          if (e.indexOf('#') === 0) {
            e = e.substring(1)
          }
          const f = a[1] || 'true'
          const h = n.querySelector('#' + e)
          if (h) {
            He(f, h, l, i)
          }
        }
      }
      ze(n, l, i)
      se(x(n, 'template'), function (e) {
        if (e.content && ze(e.content, l, i)) {
          e.remove()
        }
      })
      if (o.select) {
        const d = ne().createDocumentFragment()
        se(n.querySelectorAll(o.select), function (e) {
          d.appendChild(e)
        })
        n = d
      }
      qe(n)
      _e(r.swapStyle, o.contextElement, e, n, l)
      Te()
    }
    if (s.elt && !le(s.elt) && ee(s.elt, 'id')) {
      const g = document.getElementById(ee(s.elt, 'id'))
      const p = { preventScroll: r.focusScroll !== undefined ? !r.focusScroll : !Q.config.defaultFocusScroll }
      if (g) {
        if (s.start && g.setSelectionRange) {
          try {
            g.setSelectionRange(s.start, s.end)
          } catch (e) {}
        }
        g.focus(p)
      }
    }
    e.classList.remove(Q.config.swappingClass)
    se(l.elts, function (e) {
      if (e.classList) {
        e.classList.add(Q.config.settlingClass)
      }
      he(e, 'htmx:afterSwap', o.eventInfo)
    })
    if (o.afterSwapCallback) {
      o.afterSwapCallback()
    }
    if (!r.ignoreTitle) {
      kn(l.title)
    }
    const c = function () {
      se(l.tasks, function (e) {
        e.call()
      })
      se(l.elts, function (e) {
        if (e.classList) {
          e.classList.remove(Q.config.settlingClass)
        }
        he(e, 'htmx:afterSettle', o.eventInfo)
      })
      if (o.anchor) {
        const e = ue(y('#' + o.anchor))
        if (e) {
          e.scrollIntoView({ block: 'start', behavior: 'auto' })
        }
      }
      yn(l.elts, r)
      if (o.afterSettleCallback) {
        o.afterSettleCallback()
      }
    }
    if (r.settleDelay > 0) {
      E().setTimeout(c, r.settleDelay)
    } else {
      c()
    }
  }
  function Je(e, t, n) {
    const r = e.getResponseHeader(t)
    if (r.indexOf('{') === 0) {
      const o = S(r)
      for (const i in o) {
        if (o.hasOwnProperty(i)) {
          let e = o[i]
          if (D(e)) {
            n = e.target !== undefined ? e.target : n
          } else {
            e = { value: e }
          }
          he(n, i, e)
        }
      }
    } else {
      const s = r.split(',')
      for (let e = 0; e < s.length; e++) {
        he(n, s[e].trim(), [])
      }
    }
  }
  const Ke = /\s/
  const v = /[\s,]/
  const Ge = /[_$a-zA-Z]/
  const We = /[_$a-zA-Z0-9]/
  const Ze = ['"', "'", '/']
  const w = /[^\s]/
  const Ye = /[{(]/
  const Qe = /[})]/
  function et(e) {
    const t = []
    let n = 0
    while (n < e.length) {
      if (Ge.exec(e.charAt(n))) {
        var r = n
        while (We.exec(e.charAt(n + 1))) {
          n++
        }
        t.push(e.substring(r, n + 1))
      } else if (Ze.indexOf(e.charAt(n)) !== -1) {
        const o = e.charAt(n)
        var r = n
        n++
        while (n < e.length && e.charAt(n) !== o) {
          if (e.charAt(n) === '\\') {
            n++
          }
          n++
        }
        t.push(e.substring(r, n + 1))
      } else {
        const i = e.charAt(n)
        t.push(i)
      }
      n++
    }
    return t
  }
  function tt(e, t, n) {
    return Ge.exec(e.charAt(0)) && e !== 'true' && e !== 'false' && e !== 'this' && e !== n && t !== '.'
  }
  function nt(r, o, i) {
    if (o[0] === '[') {
      o.shift()
      let e = 1
      let t = ' return (function(' + i + '){ return ('
      let n = null
      while (o.length > 0) {
        const s = o[0]
        if (s === ']') {
          e--
          if (e === 0) {
            if (n === null) {
              t = t + 'true'
            }
            o.shift()
            t += ')})'
            try {
              const l = vn(
                r,
                function () {
                  return Function(t)()
                },
                function () {
                  return true
                },
              )
              l.source = t
              return l
            } catch (e) {
              fe(ne().body, 'htmx:syntax:error', { error: e, source: t })
              return null
            }
          }
        } else if (s === '[') {
          e++
        }
        if (tt(s, n, i)) {
          t += '((' + i + '.' + s + ') ? (' + i + '.' + s + ') : (window.' + s + '))'
        } else {
          t = t + s
        }
        n = o.shift()
      }
    }
  }
  function C(e, t) {
    let n = ''
    while (e.length > 0 && !t.test(e[0])) {
      n += e.shift()
    }
    return n
  }
  function rt(e) {
    let t
    if (e.length > 0 && Ye.test(e[0])) {
      e.shift()
      t = C(e, Qe).trim()
      e.shift()
    } else {
      t = C(e, v)
    }
    return t
  }
  const ot = 'input, textarea, select'
  function it(e, t, n) {
    const r = []
    const o = et(t)
    do {
      C(o, w)
      const l = o.length
      const c = C(o, /[,\[\s]/)
      if (c !== '') {
        if (c === 'every') {
          const u = { trigger: 'every' }
          C(o, w)
          u.pollInterval = d(C(o, /[,\[\s]/))
          C(o, w)
          var i = nt(e, o, 'event')
          if (i) {
            u.eventFilter = i
          }
          r.push(u)
        } else {
          const a = { trigger: c }
          var i = nt(e, o, 'event')
          if (i) {
            a.eventFilter = i
          }
          C(o, w)
          while (o.length > 0 && o[0] !== ',') {
            const f = o.shift()
            if (f === 'changed') {
              a.changed = true
            } else if (f === 'once') {
              a.once = true
            } else if (f === 'consume') {
              a.consume = true
            } else if (f === 'delay' && o[0] === ':') {
              o.shift()
              a.delay = d(C(o, v))
            } else if (f === 'from' && o[0] === ':') {
              o.shift()
              if (Ye.test(o[0])) {
                var s = rt(o)
              } else {
                var s = C(o, v)
                if (s === 'closest' || s === 'find' || s === 'next' || s === 'previous') {
                  o.shift()
                  const h = rt(o)
                  if (h.length > 0) {
                    s += ' ' + h
                  }
                }
              }
              a.from = s
            } else if (f === 'target' && o[0] === ':') {
              o.shift()
              a.target = rt(o)
            } else if (f === 'throttle' && o[0] === ':') {
              o.shift()
              a.throttle = d(C(o, v))
            } else if (f === 'queue' && o[0] === ':') {
              o.shift()
              a.queue = C(o, v)
            } else if (f === 'root' && o[0] === ':') {
              o.shift()
              a[f] = rt(o)
            } else if (f === 'threshold' && o[0] === ':') {
              o.shift()
              a[f] = C(o, v)
            } else {
              fe(e, 'htmx:syntax:error', { token: o.shift() })
            }
            C(o, w)
          }
          r.push(a)
        }
      }
      if (o.length === l) {
        fe(e, 'htmx:syntax:error', { token: o.shift() })
      }
      C(o, w)
    } while (o[0] === ',' && o.shift())
    if (n) {
      n[t] = r
    }
    return r
  }
  function st(e) {
    const t = te(e, 'hx-trigger')
    let n = []
    if (t) {
      const r = Q.config.triggerSpecsCache
      n = (r && r[t]) || it(e, t, r)
    }
    if (n.length > 0) {
      return n
    } else if (h(e, 'form')) {
      return [{ trigger: 'submit' }]
    } else if (h(e, 'input[type="button"], input[type="submit"]')) {
      return [{ trigger: 'click' }]
    } else if (h(e, ot)) {
      return [{ trigger: 'change' }]
    } else {
      return [{ trigger: 'click' }]
    }
  }
  function lt(e) {
    ie(e).cancelled = true
  }
  function ct(e, t, n) {
    const r = ie(e)
    r.timeout = E().setTimeout(function () {
      if (le(e) && r.cancelled !== true) {
        if (!gt(n, e, Mt('hx:poll:trigger', { triggerSpec: n, target: e }))) {
          t(e)
        }
        ct(e, t, n)
      }
    }, n.pollInterval)
  }
  function ut(e) {
    return location.hostname === e.hostname && ee(e, 'href') && ee(e, 'href').indexOf('#') !== 0
  }
  function at(e) {
    return g(e, Q.config.disableSelector)
  }
  function ft(t, n, e) {
    if (
      (t instanceof HTMLAnchorElement && ut(t) && (t.target === '' || t.target === '_self')) ||
      (t.tagName === 'FORM' && String(ee(t, 'method')).toLowerCase() !== 'dialog')
    ) {
      n.boosted = true
      let r, o
      if (t.tagName === 'A') {
        r = 'get'
        o = ee(t, 'href')
      } else {
        const i = ee(t, 'method')
        r = i ? i.toLowerCase() : 'get'
        o = ee(t, 'action')
        if (o == null || o === '') {
          o = ne().location.href
        }
        if (r === 'get' && o.includes('?')) {
          o = o.replace(/\?[^#]+/, '')
        }
      }
      e.forEach(function (e) {
        pt(
          t,
          function (e, t) {
            const n = ue(e)
            if (at(n)) {
              b(n)
              return
            }
            de(r, o, n, t)
          },
          n,
          e,
          true,
        )
      })
    }
  }
  function ht(e, t) {
    const n = ue(t)
    if (!n) {
      return false
    }
    if (e.type === 'submit' || e.type === 'click') {
      if (n.tagName === 'FORM') {
        return true
      }
      if (h(n, 'input[type="submit"], button') && (h(n, '[form]') || g(n, 'form') !== null)) {
        return true
      }
      if (n instanceof HTMLAnchorElement && n.href && (n.getAttribute('href') === '#' || n.getAttribute('href').indexOf('#') !== 0)) {
        return true
      }
    }
    return false
  }
  function dt(e, t) {
    return ie(e).boosted && e instanceof HTMLAnchorElement && t.type === 'click' && (t.ctrlKey || t.metaKey)
  }
  function gt(e, t, n) {
    const r = e.eventFilter
    if (r) {
      try {
        return r.call(t, n) !== true
      } catch (e) {
        const o = r.source
        fe(ne().body, 'htmx:eventFilter:error', { error: e, source: o })
        return true
      }
    }
    return false
  }
  function pt(l, c, e, u, a) {
    const f = ie(l)
    let t
    if (u.from) {
      t = p(l, u.from)
    } else {
      t = [l]
    }
    if (u.changed) {
      if (!('lastValue' in f)) {
        f.lastValue = new WeakMap()
      }
      t.forEach(function (e) {
        if (!f.lastValue.has(u)) {
          f.lastValue.set(u, new WeakMap())
        }
        f.lastValue.get(u).set(e, e.value)
      })
    }
    se(t, function (i) {
      const s = function (e) {
        if (!le(l)) {
          i.removeEventListener(u.trigger, s)
          return
        }
        if (dt(l, e)) {
          return
        }
        if (a || ht(e, l)) {
          e.preventDefault()
        }
        if (gt(u, l, e)) {
          return
        }
        const t = ie(e)
        t.triggerSpec = u
        if (t.handledFor == null) {
          t.handledFor = []
        }
        if (t.handledFor.indexOf(l) < 0) {
          t.handledFor.push(l)
          if (u.consume) {
            e.stopPropagation()
          }
          if (u.target && e.target) {
            if (!h(ue(e.target), u.target)) {
              return
            }
          }
          if (u.once) {
            if (f.triggeredOnce) {
              return
            } else {
              f.triggeredOnce = true
            }
          }
          if (u.changed) {
            const n = event.target
            const r = n.value
            const o = f.lastValue.get(u)
            if (o.has(n) && o.get(n) === r) {
              return
            }
            o.set(n, r)
          }
          if (f.delayed) {
            clearTimeout(f.delayed)
          }
          if (f.throttle) {
            return
          }
          if (u.throttle > 0) {
            if (!f.throttle) {
              he(l, 'htmx:trigger')
              c(l, e)
              f.throttle = E().setTimeout(function () {
                f.throttle = null
              }, u.throttle)
            }
          } else if (u.delay > 0) {
            f.delayed = E().setTimeout(function () {
              he(l, 'htmx:trigger')
              c(l, e)
            }, u.delay)
          } else {
            he(l, 'htmx:trigger')
            c(l, e)
          }
        }
      }
      if (e.listenerInfos == null) {
        e.listenerInfos = []
      }
      e.listenerInfos.push({ trigger: u.trigger, listener: s, on: i })
      i.addEventListener(u.trigger, s)
    })
  }
  let mt = false
  let xt = null
  function yt() {
    if (!xt) {
      xt = function () {
        mt = true
      }
      window.addEventListener('scroll', xt)
      window.addEventListener('resize', xt)
      setInterval(function () {
        if (mt) {
          mt = false
          se(ne().querySelectorAll("[hx-trigger*='revealed'],[data-hx-trigger*='revealed']"), function (e) {
            bt(e)
          })
        }
      }, 200)
    }
  }
  function bt(e) {
    if (!s(e, 'data-hx-revealed') && X(e)) {
      e.setAttribute('data-hx-revealed', 'true')
      const t = ie(e)
      if (t.initHash) {
        he(e, 'revealed')
      } else {
        e.addEventListener(
          'htmx:afterProcessNode',
          function () {
            he(e, 'revealed')
          },
          { once: true },
        )
      }
    }
  }
  function vt(e, t, n, r) {
    const o = function () {
      if (!n.loaded) {
        n.loaded = true
        he(e, 'htmx:trigger')
        t(e)
      }
    }
    if (r > 0) {
      E().setTimeout(o, r)
    } else {
      o()
    }
  }
  function wt(t, n, e) {
    let i = false
    se(r, function (r) {
      if (s(t, 'hx-' + r)) {
        const o = te(t, 'hx-' + r)
        i = true
        n.path = o
        n.verb = r
        e.forEach(function (e) {
          St(t, e, n, function (e, t) {
            const n = ue(e)
            if (g(n, Q.config.disableSelector)) {
              b(n)
              return
            }
            de(r, o, n, t)
          })
        })
      }
    })
    return i
  }
  function St(r, e, t, n) {
    if (e.trigger === 'revealed') {
      yt()
      pt(r, n, t, e)
      bt(ue(r))
    } else if (e.trigger === 'intersect') {
      const o = {}
      if (e.root) {
        o.root = ae(r, e.root)
      }
      if (e.threshold) {
        o.threshold = parseFloat(e.threshold)
      }
      const i = new IntersectionObserver(function (t) {
        for (let e = 0; e < t.length; e++) {
          const n = t[e]
          if (n.isIntersecting) {
            he(r, 'intersect')
            break
          }
        }
      }, o)
      i.observe(ue(r))
      pt(ue(r), n, t, e)
    } else if (!t.firstInitCompleted && e.trigger === 'load') {
      if (!gt(e, r, Mt('load', { elt: r }))) {
        vt(ue(r), n, t, e.delay)
      }
    } else if (e.pollInterval > 0) {
      t.polling = true
      ct(ue(r), n, e)
    } else {
      pt(r, n, t, e)
    }
  }
  function Et(e) {
    const t = ue(e)
    if (!t) {
      return false
    }
    const n = t.attributes
    for (let e = 0; e < n.length; e++) {
      const r = n[e].name
      if (l(r, 'hx-on:') || l(r, 'data-hx-on:') || l(r, 'hx-on-') || l(r, 'data-hx-on-')) {
        return true
      }
    }
    return false
  }
  const Ct = new XPathEvaluator().createExpression(
    './/*[@*[ starts-with(name(), "hx-on:") or starts-with(name(), "data-hx-on:") or' +
      ' starts-with(name(), "hx-on-") or starts-with(name(), "data-hx-on-") ]]',
  )
  function Ot(e, t) {
    if (Et(e)) {
      t.push(ue(e))
    }
    const n = Ct.evaluate(e)
    let r = null
    while ((r = n.iterateNext())) t.push(ue(r))
  }
  function Rt(e) {
    const t = []
    if (e instanceof DocumentFragment) {
      for (const n of e.childNodes) {
        Ot(n, t)
      }
    } else {
      Ot(e, t)
    }
    return t
  }
  function Ht(e) {
    if (e.querySelectorAll) {
      const n = ', [hx-boost] a, [data-hx-boost] a, a[hx-boost], a[data-hx-boost]'
      const r = []
      for (const i in Mn) {
        const s = Mn[i]
        if (s.getSelectors) {
          var t = s.getSelectors()
          if (t) {
            r.push(t)
          }
        }
      }
      const o = e.querySelectorAll(
        H +
          n +
          ", form, [type='submit']," +
          ' [hx-ext], [data-hx-ext], [hx-trigger], [data-hx-trigger]' +
          r
            .flat()
            .map(e => ', ' + e)
            .join(''),
      )
      return o
    } else {
      return []
    }
  }
  function Tt(e) {
    const t = g(ue(e.target), "button, input[type='submit']")
    const n = Lt(e)
    if (n) {
      n.lastButtonClicked = t
    }
  }
  function qt(e) {
    const t = Lt(e)
    if (t) {
      t.lastButtonClicked = null
    }
  }
  function Lt(e) {
    const t = g(ue(e.target), "button, input[type='submit']")
    if (!t) {
      return
    }
    const n = y('#' + ee(t, 'form'), t.getRootNode()) || g(t, 'form')
    if (!n) {
      return
    }
    return ie(n)
  }
  function At(e) {
    e.addEventListener('click', Tt)
    e.addEventListener('focusin', Tt)
    e.addEventListener('focusout', qt)
  }
  function Nt(t, e, n) {
    const r = ie(t)
    if (!Array.isArray(r.onHandlers)) {
      r.onHandlers = []
    }
    let o
    const i = function (e) {
      vn(t, function () {
        if (at(t)) {
          return
        }
        if (!o) {
          o = new Function('event', n)
        }
        o.call(t, e)
      })
    }
    t.addEventListener(e, i)
    r.onHandlers.push({ event: e, listener: i })
  }
  function It(t) {
    ke(t)
    for (let e = 0; e < t.attributes.length; e++) {
      const n = t.attributes[e].name
      const r = t.attributes[e].value
      if (l(n, 'hx-on') || l(n, 'data-hx-on')) {
        const o = n.indexOf('-on') + 3
        const i = n.slice(o, o + 1)
        if (i === '-' || i === ':') {
          let e = n.slice(o + 1)
          if (l(e, ':')) {
            e = 'htmx' + e
          } else if (l(e, '-')) {
            e = 'htmx:' + e.slice(1)
          } else if (l(e, 'htmx-')) {
            e = 'htmx:' + e.slice(5)
          }
          Nt(t, e, r)
        }
      }
    }
  }
  function Pt(t) {
    if (g(t, Q.config.disableSelector)) {
      b(t)
      return
    }
    const n = ie(t)
    const e = Pe(t)
    if (n.initHash !== e) {
      De(t)
      n.initHash = e
      he(t, 'htmx:beforeProcessNode')
      const r = st(t)
      const o = wt(t, n, r)
      if (!o) {
        if (re(t, 'hx-boost') === 'true') {
          ft(t, n, r)
        } else if (s(t, 'hx-trigger')) {
          r.forEach(function (e) {
            St(t, e, n, function () {})
          })
        }
      }
      if (t.tagName === 'FORM' || (ee(t, 'type') === 'submit' && s(t, 'form'))) {
        At(t)
      }
      n.firstInitCompleted = true
      he(t, 'htmx:afterProcessNode')
    }
  }
  function kt(e) {
    e = y(e)
    if (g(e, Q.config.disableSelector)) {
      b(e)
      return
    }
    Pt(e)
    se(Ht(e), function (e) {
      Pt(e)
    })
    se(Rt(e), It)
  }
  function Dt(e) {
    return e.replace(/([a-z0-9])([A-Z])/g, '$1-$2').toLowerCase()
  }
  function Mt(e, t) {
    let n
    if (window.CustomEvent && typeof window.CustomEvent === 'function') {
      n = new CustomEvent(e, { bubbles: true, cancelable: true, composed: true, detail: t })
    } else {
      n = ne().createEvent('CustomEvent')
      n.initCustomEvent(e, true, true, t)
    }
    return n
  }
  function fe(e, t, n) {
    he(e, t, ce({ error: t }, n))
  }
  function Xt(e) {
    return e === 'htmx:afterProcessNode'
  }
  function Ft(e, t) {
    se(Un(e), function (e) {
      try {
        t(e)
      } catch (e) {
        O(e)
      }
    })
  }
  function O(e) {
    if (console.error) {
      console.error(e)
    } else if (console.log) {
      console.log('ERROR: ', e)
    }
  }
  function he(e, t, n) {
    e = y(e)
    if (n == null) {
      n = {}
    }
    n.elt = e
    const r = Mt(t, n)
    if (Q.logger && !Xt(t)) {
      Q.logger(e, t, n)
    }
    if (n.error) {
      O(n.error)
      he(e, 'htmx:error', { errorInfo: n })
    }
    let o = e.dispatchEvent(r)
    const i = Dt(t)
    if (o && i !== t) {
      const s = Mt(i, r.detail)
      o = o && e.dispatchEvent(s)
    }
    Ft(ue(e), function (e) {
      o = o && e.onEvent(t, r) !== false && !r.defaultPrevented
    })
    return o
  }
  let Bt = location.pathname + location.search
  function Ut() {
    const e = ne().querySelector('[hx-history-elt],[data-hx-history-elt]')
    return e || ne().body
  }
  function jt(t, e) {
    if (!B()) {
      return
    }
    const n = _t(e)
    const r = ne().title
    const o = window.scrollY
    if (Q.config.historyCacheSize <= 0) {
      localStorage.removeItem('htmx-history-cache')
      return
    }
    t = U(t)
    const i = S(localStorage.getItem('htmx-history-cache')) || []
    for (let e = 0; e < i.length; e++) {
      if (i[e].url === t) {
        i.splice(e, 1)
        break
      }
    }
    const s = { url: t, content: n, title: r, scroll: o }
    he(ne().body, 'htmx:historyItemCreated', { item: s, cache: i })
    i.push(s)
    while (i.length > Q.config.historyCacheSize) {
      i.shift()
    }
    while (i.length > 0) {
      try {
        localStorage.setItem('htmx-history-cache', JSON.stringify(i))
        break
      } catch (e) {
        fe(ne().body, 'htmx:historyCacheError', { cause: e, cache: i })
        i.shift()
      }
    }
  }
  function Vt(t) {
    if (!B()) {
      return null
    }
    t = U(t)
    const n = S(localStorage.getItem('htmx-history-cache')) || []
    for (let e = 0; e < n.length; e++) {
      if (n[e].url === t) {
        return n[e]
      }
    }
    return null
  }
  function _t(e) {
    const t = Q.config.requestClass
    const n = e.cloneNode(true)
    se(x(n, '.' + t), function (e) {
      G(e, t)
    })
    se(x(n, '[data-disabled-by-htmx]'), function (e) {
      e.removeAttribute('disabled')
    })
    return n.innerHTML
  }
  function zt() {
    const e = Ut()
    const t = Bt || location.pathname + location.search
    let n
    try {
      n = ne().querySelector('[hx-history="false" i],[data-hx-history="false" i]')
    } catch (e) {
      n = ne().querySelector('[hx-history="false"],[data-hx-history="false"]')
    }
    if (!n) {
      he(ne().body, 'htmx:beforeHistorySave', { path: t, historyElt: e })
      jt(t, e)
    }
    if (Q.config.historyEnabled) history.replaceState({ htmx: true }, ne().title, window.location.href)
  }
  function $t(e) {
    if (Q.config.getCacheBusterParam) {
      e = e.replace(/org\.htmx\.cache-buster=[^&]*&?/, '')
      if (Y(e, '&') || Y(e, '?')) {
        e = e.slice(0, -1)
      }
    }
    if (Q.config.historyEnabled) {
      history.pushState({ htmx: true }, '', e)
    }
    Bt = e
  }
  function Jt(e) {
    if (Q.config.historyEnabled) history.replaceState({ htmx: true }, '', e)
    Bt = e
  }
  function Kt(e) {
    se(e, function (e) {
      e.call(undefined)
    })
  }
  function Gt(o) {
    const e = new XMLHttpRequest()
    const i = { path: o, xhr: e }
    he(ne().body, 'htmx:historyCacheMiss', i)
    e.open('GET', o, true)
    e.setRequestHeader('HX-Request', 'true')
    e.setRequestHeader('HX-History-Restore-Request', 'true')
    e.setRequestHeader('HX-Current-URL', ne().location.href)
    e.onload = function () {
      if (this.status >= 200 && this.status < 400) {
        he(ne().body, 'htmx:historyCacheMissLoad', i)
        const e = P(this.response)
        const t = e.querySelector('[hx-history-elt],[data-hx-history-elt]') || e
        const n = Ut()
        const r = xn(n)
        kn(e.title)
        qe(e)
        Ve(n, t, r)
        Te()
        Kt(r.tasks)
        Bt = o
        he(ne().body, 'htmx:historyRestore', { path: o, cacheMiss: true, serverResponse: this.response })
      } else {
        fe(ne().body, 'htmx:historyCacheMissLoadError', i)
      }
    }
    e.send()
  }
  function Wt(e) {
    zt()
    e = e || location.pathname + location.search
    const t = Vt(e)
    if (t) {
      const n = P(t.content)
      const r = Ut()
      const o = xn(r)
      kn(t.title)
      qe(n)
      Ve(r, n, o)
      Te()
      Kt(o.tasks)
      E().setTimeout(function () {
        window.scrollTo(0, t.scroll)
      }, 0)
      Bt = e
      he(ne().body, 'htmx:historyRestore', { path: e, item: t })
    } else {
      if (Q.config.refreshOnHistoryMiss) {
        window.location.reload(true)
      } else {
        Gt(e)
      }
    }
  }
  function Zt(e) {
    let t = we(e, 'hx-indicator')
    if (t == null) {
      t = [e]
    }
    se(t, function (e) {
      const t = ie(e)
      t.requestCount = (t.requestCount || 0) + 1
      e.classList.add.call(e.classList, Q.config.requestClass)
    })
    return t
  }
  function Yt(e) {
    let t = we(e, 'hx-disabled-elt')
    if (t == null) {
      t = []
    }
    se(t, function (e) {
      const t = ie(e)
      t.requestCount = (t.requestCount || 0) + 1
      e.setAttribute('disabled', '')
      e.setAttribute('data-disabled-by-htmx', '')
    })
    return t
  }
  function Qt(e, t) {
    se(e.concat(t), function (e) {
      const t = ie(e)
      t.requestCount = (t.requestCount || 1) - 1
    })
    se(e, function (e) {
      const t = ie(e)
      if (t.requestCount === 0) {
        e.classList.remove.call(e.classList, Q.config.requestClass)
      }
    })
    se(t, function (e) {
      const t = ie(e)
      if (t.requestCount === 0) {
        e.removeAttribute('disabled')
        e.removeAttribute('data-disabled-by-htmx')
      }
    })
  }
  function en(t, n) {
    for (let e = 0; e < t.length; e++) {
      const r = t[e]
      if (r.isSameNode(n)) {
        return true
      }
    }
    return false
  }
  function tn(e) {
    const t = e
    if (t.name === '' || t.name == null || t.disabled || g(t, 'fieldset[disabled]')) {
      return false
    }
    if (t.type === 'button' || t.type === 'submit' || t.tagName === 'image' || t.tagName === 'reset' || t.tagName === 'file') {
      return false
    }
    if (t.type === 'checkbox' || t.type === 'radio') {
      return t.checked
    }
    return true
  }
  function nn(t, e, n) {
    if (t != null && e != null) {
      if (Array.isArray(e)) {
        e.forEach(function (e) {
          n.append(t, e)
        })
      } else {
        n.append(t, e)
      }
    }
  }
  function rn(t, n, r) {
    if (t != null && n != null) {
      let e = r.getAll(t)
      if (Array.isArray(n)) {
        e = e.filter(e => n.indexOf(e) < 0)
      } else {
        e = e.filter(e => e !== n)
      }
      r.delete(t)
      se(e, e => r.append(t, e))
    }
  }
  function on(t, n, r, o, i) {
    if (o == null || en(t, o)) {
      return
    } else {
      t.push(o)
    }
    if (tn(o)) {
      const s = ee(o, 'name')
      let e = o.value
      if (o instanceof HTMLSelectElement && o.multiple) {
        e = M(o.querySelectorAll('option:checked')).map(function (e) {
          return e.value
        })
      }
      if (o instanceof HTMLInputElement && o.files) {
        e = M(o.files)
      }
      nn(s, e, n)
      if (i) {
        sn(o, r)
      }
    }
    if (o instanceof HTMLFormElement) {
      se(o.elements, function (e) {
        if (t.indexOf(e) >= 0) {
          rn(e.name, e.value, n)
        } else {
          t.push(e)
        }
        if (i) {
          sn(e, r)
        }
      })
      new FormData(o).forEach(function (e, t) {
        if (e instanceof File && e.name === '') {
          return
        }
        nn(t, e, n)
      })
    }
  }
  function sn(e, t) {
    const n = e
    if (n.willValidate) {
      he(n, 'htmx:validation:validate')
      if (!n.checkValidity()) {
        t.push({ elt: n, message: n.validationMessage, validity: n.validity })
        he(n, 'htmx:validation:failed', { message: n.validationMessage, validity: n.validity })
      }
    }
  }
  function ln(n, e) {
    for (const t of e.keys()) {
      n.delete(t)
    }
    e.forEach(function (e, t) {
      n.append(t, e)
    })
    return n
  }
  function cn(e, t) {
    const n = []
    const r = new FormData()
    const o = new FormData()
    const i = []
    const s = ie(e)
    if (s.lastButtonClicked && !le(s.lastButtonClicked)) {
      s.lastButtonClicked = null
    }
    let l = (e instanceof HTMLFormElement && e.noValidate !== true) || te(e, 'hx-validate') === 'true'
    if (s.lastButtonClicked) {
      l = l && s.lastButtonClicked.formNoValidate !== true
    }
    if (t !== 'get') {
      on(n, o, i, g(e, 'form'), l)
    }
    on(n, r, i, e, l)
    if (s.lastButtonClicked || e.tagName === 'BUTTON' || (e.tagName === 'INPUT' && ee(e, 'type') === 'submit')) {
      const u = s.lastButtonClicked || e
      const a = ee(u, 'name')
      nn(a, u.value, o)
    }
    const c = we(e, 'hx-include')
    se(c, function (e) {
      on(n, r, i, ue(e), l)
      if (!h(e, 'form')) {
        se(f(e).querySelectorAll(ot), function (e) {
          on(n, r, i, e, l)
        })
      }
    })
    ln(r, o)
    return { errors: i, formData: r, values: An(r) }
  }
  function un(e, t, n) {
    if (e !== '') {
      e += '&'
    }
    if (String(n) === '[object Object]') {
      n = JSON.stringify(n)
    }
    const r = encodeURIComponent(n)
    e += encodeURIComponent(t) + '=' + r
    return e
  }
  function an(e) {
    e = qn(e)
    let n = ''
    e.forEach(function (e, t) {
      n = un(n, t, e)
    })
    return n
  }
  function fn(e, t, n) {
    const r = {
      'HX-Request': 'true',
      'HX-Trigger': ee(e, 'id'),
      'HX-Trigger-Name': ee(e, 'name'),
      'HX-Target': te(t, 'id'),
      'HX-Current-URL': ne().location.href,
    }
    bn(e, 'hx-headers', false, r)
    if (n !== undefined) {
      r['HX-Prompt'] = n
    }
    if (ie(e).boosted) {
      r['HX-Boosted'] = 'true'
    }
    return r
  }
  function hn(n, e) {
    const t = re(e, 'hx-params')
    if (t) {
      if (t === 'none') {
        return new FormData()
      } else if (t === '*') {
        return n
      } else if (t.indexOf('not ') === 0) {
        se(t.slice(4).split(','), function (e) {
          e = e.trim()
          n.delete(e)
        })
        return n
      } else {
        const r = new FormData()
        se(t.split(','), function (t) {
          t = t.trim()
          if (n.has(t)) {
            n.getAll(t).forEach(function (e) {
              r.append(t, e)
            })
          }
        })
        return r
      }
    } else {
      return n
    }
  }
  function dn(e) {
    return !!ee(e, 'href') && ee(e, 'href').indexOf('#') >= 0
  }
  function gn(e, t) {
    const n = t || re(e, 'hx-swap')
    const r = {
      swapStyle: ie(e).boosted ? 'innerHTML' : Q.config.defaultSwapStyle,
      swapDelay: Q.config.defaultSwapDelay,
      settleDelay: Q.config.defaultSettleDelay,
    }
    if (Q.config.scrollIntoViewOnBoost && ie(e).boosted && !dn(e)) {
      r.show = 'top'
    }
    if (n) {
      const s = F(n)
      if (s.length > 0) {
        for (let e = 0; e < s.length; e++) {
          const l = s[e]
          if (l.indexOf('swap:') === 0) {
            r.swapDelay = d(l.slice(5))
          } else if (l.indexOf('settle:') === 0) {
            r.settleDelay = d(l.slice(7))
          } else if (l.indexOf('transition:') === 0) {
            r.transition = l.slice(11) === 'true'
          } else if (l.indexOf('ignoreTitle:') === 0) {
            r.ignoreTitle = l.slice(12) === 'true'
          } else if (l.indexOf('scroll:') === 0) {
            const c = l.slice(7)
            var o = c.split(':')
            const u = o.pop()
            var i = o.length > 0 ? o.join(':') : null
            r.scroll = u
            r.scrollTarget = i
          } else if (l.indexOf('show:') === 0) {
            const a = l.slice(5)
            var o = a.split(':')
            const f = o.pop()
            var i = o.length > 0 ? o.join(':') : null
            r.show = f
            r.showTarget = i
          } else if (l.indexOf('focus-scroll:') === 0) {
            const h = l.slice('focus-scroll:'.length)
            r.focusScroll = h == 'true'
          } else if (e == 0) {
            r.swapStyle = l
          } else {
            O('Unknown modifier in hx-swap: ' + l)
          }
        }
      }
    }
    return r
  }
  function pn(e) {
    return re(e, 'hx-encoding') === 'multipart/form-data' || (h(e, 'form') && ee(e, 'enctype') === 'multipart/form-data')
  }
  function mn(t, n, r) {
    let o = null
    Ft(n, function (e) {
      if (o == null) {
        o = e.encodeParameters(t, r, n)
      }
    })
    if (o != null) {
      return o
    } else {
      if (pn(n)) {
        return ln(new FormData(), qn(r))
      } else {
        return an(r)
      }
    }
  }
  function xn(e) {
    return { tasks: [], elts: [e] }
  }
  function yn(e, t) {
    const n = e[0]
    const r = e[e.length - 1]
    if (t.scroll) {
      var o = null
      if (t.scrollTarget) {
        o = ue(ae(n, t.scrollTarget))
      }
      if (t.scroll === 'top' && (n || o)) {
        o = o || n
        o.scrollTop = 0
      }
      if (t.scroll === 'bottom' && (r || o)) {
        o = o || r
        o.scrollTop = o.scrollHeight
      }
    }
    if (t.show) {
      var o = null
      if (t.showTarget) {
        let e = t.showTarget
        if (t.showTarget === 'window') {
          e = 'body'
        }
        o = ue(ae(n, e))
      }
      if (t.show === 'top' && (n || o)) {
        o = o || n
        o.scrollIntoView({ block: 'start', behavior: Q.config.scrollBehavior })
      }
      if (t.show === 'bottom' && (r || o)) {
        o = o || r
        o.scrollIntoView({ block: 'end', behavior: Q.config.scrollBehavior })
      }
    }
  }
  function bn(r, e, o, i) {
    if (i == null) {
      i = {}
    }
    if (r == null) {
      return i
    }
    const s = te(r, e)
    if (s) {
      let e = s.trim()
      let t = o
      if (e === 'unset') {
        return null
      }
      if (e.indexOf('javascript:') === 0) {
        e = e.slice(11)
        t = true
      } else if (e.indexOf('js:') === 0) {
        e = e.slice(3)
        t = true
      }
      if (e.indexOf('{') !== 0) {
        e = '{' + e + '}'
      }
      let n
      if (t) {
        n = vn(
          r,
          function () {
            return Function('return (' + e + ')')()
          },
          {},
        )
      } else {
        n = S(e)
      }
      for (const l in n) {
        if (n.hasOwnProperty(l)) {
          if (i[l] == null) {
            i[l] = n[l]
          }
        }
      }
    }
    return bn(ue(c(r)), e, o, i)
  }
  function vn(e, t, n) {
    if (Q.config.allowEval) {
      return t()
    } else {
      fe(e, 'htmx:evalDisallowedError')
      return n
    }
  }
  function wn(e, t) {
    return bn(e, 'hx-vars', true, t)
  }
  function Sn(e, t) {
    return bn(e, 'hx-vals', false, t)
  }
  function En(e) {
    return ce(wn(e), Sn(e))
  }
  function Cn(t, n, r) {
    if (r !== null) {
      try {
        t.setRequestHeader(n, r)
      } catch (e) {
        t.setRequestHeader(n, encodeURIComponent(r))
        t.setRequestHeader(n + '-URI-AutoEncoded', 'true')
      }
    }
  }
  function On(t) {
    if (t.responseURL && typeof URL !== 'undefined') {
      try {
        const e = new URL(t.responseURL)
        return e.pathname + e.search
      } catch (e) {
        fe(ne().body, 'htmx:badResponseUrl', { url: t.responseURL })
      }
    }
  }
  function R(e, t) {
    return t.test(e.getAllResponseHeaders())
  }
  function Rn(t, n, r) {
    t = t.toLowerCase()
    if (r) {
      if (r instanceof Element || typeof r === 'string') {
        return de(t, n, null, null, { targetOverride: y(r) || ve, returnPromise: true })
      } else {
        let e = y(r.target)
        if ((r.target && !e) || (r.source && !e && !y(r.source))) {
          e = ve
        }
        return de(t, n, y(r.source), r.event, {
          handler: r.handler,
          headers: r.headers,
          values: r.values,
          targetOverride: e,
          swapOverride: r.swap,
          select: r.select,
          returnPromise: true,
        })
      }
    } else {
      return de(t, n, null, null, { returnPromise: true })
    }
  }
  function Hn(e) {
    const t = []
    while (e) {
      t.push(e)
      e = e.parentElement
    }
    return t
  }
  function Tn(e, t, n) {
    let r
    let o
    if (typeof URL === 'function') {
      o = new URL(t, document.location.href)
      const i = document.location.origin
      r = i === o.origin
    } else {
      o = t
      r = l(t, document.location.origin)
    }
    if (Q.config.selfRequestsOnly) {
      if (!r) {
        return false
      }
    }
    return he(e, 'htmx:validateUrl', ce({ url: o, sameHost: r }, n))
  }
  function qn(e) {
    if (e instanceof FormData) return e
    const t = new FormData()
    for (const n in e) {
      if (e.hasOwnProperty(n)) {
        if (e[n] && typeof e[n].forEach === 'function') {
          e[n].forEach(function (e) {
            t.append(n, e)
          })
        } else if (typeof e[n] === 'object' && !(e[n] instanceof Blob)) {
          t.append(n, JSON.stringify(e[n]))
        } else {
          t.append(n, e[n])
        }
      }
    }
    return t
  }
  function Ln(r, o, e) {
    return new Proxy(e, {
      get: function (t, e) {
        if (typeof e === 'number') return t[e]
        if (e === 'length') return t.length
        if (e === 'push') {
          return function (e) {
            t.push(e)
            r.append(o, e)
          }
        }
        if (typeof t[e] === 'function') {
          return function () {
            t[e].apply(t, arguments)
            r.delete(o)
            t.forEach(function (e) {
              r.append(o, e)
            })
          }
        }
        if (t[e] && t[e].length === 1) {
          return t[e][0]
        } else {
          return t[e]
        }
      },
      set: function (e, t, n) {
        e[t] = n
        r.delete(o)
        e.forEach(function (e) {
          r.append(o, e)
        })
        return true
      },
    })
  }
  function An(o) {
    return new Proxy(o, {
      get: function (e, t) {
        if (typeof t === 'symbol') {
          const r = Reflect.get(e, t)
          if (typeof r === 'function') {
            return function () {
              return r.apply(o, arguments)
            }
          } else {
            return r
          }
        }
        if (t === 'toJSON') {
          return () => Object.fromEntries(o)
        }
        if (t in e) {
          if (typeof e[t] === 'function') {
            return function () {
              return o[t].apply(o, arguments)
            }
          } else {
            return e[t]
          }
        }
        const n = o.getAll(t)
        if (n.length === 0) {
          return undefined
        } else if (n.length === 1) {
          return n[0]
        } else {
          return Ln(e, t, n)
        }
      },
      set: function (t, n, e) {
        if (typeof n !== 'string') {
          return false
        }
        t.delete(n)
        if (e && typeof e.forEach === 'function') {
          e.forEach(function (e) {
            t.append(n, e)
          })
        } else if (typeof e === 'object' && !(e instanceof Blob)) {
          t.append(n, JSON.stringify(e))
        } else {
          t.append(n, e)
        }
        return true
      },
      deleteProperty: function (e, t) {
        if (typeof t === 'string') {
          e.delete(t)
        }
        return true
      },
      ownKeys: function (e) {
        return Reflect.ownKeys(Object.fromEntries(e))
      },
      getOwnPropertyDescriptor: function (e, t) {
        return Reflect.getOwnPropertyDescriptor(Object.fromEntries(e), t)
      },
    })
  }
  function de(t, n, r, o, i, D) {
    let s = null
    let l = null
    i = i != null ? i : {}
    if (i.returnPromise && typeof Promise !== 'undefined') {
      var e = new Promise(function (e, t) {
        s = e
        l = t
      })
    }
    if (r == null) {
      r = ne().body
    }
    const M = i.handler || Dn
    const X = i.select || null
    if (!le(r)) {
      oe(s)
      return e
    }
    const c = i.targetOverride || ue(Ee(r))
    if (c == null || c == ve) {
      fe(r, 'htmx:targetError', { target: te(r, 'hx-target') })
      oe(l)
      return e
    }
    let u = ie(r)
    const a = u.lastButtonClicked
    if (a) {
      const L = ee(a, 'formaction')
      if (L != null) {
        n = L
      }
      const A = ee(a, 'formmethod')
      if (A != null) {
        if (A.toLowerCase() !== 'dialog') {
          t = A
        }
      }
    }
    const f = re(r, 'hx-confirm')
    if (D === undefined) {
      const K = function (e) {
        return de(t, n, r, o, i, !!e)
      }
      const G = { target: c, elt: r, path: n, verb: t, triggeringEvent: o, etc: i, issueRequest: K, question: f }
      if (he(r, 'htmx:confirm', G) === false) {
        oe(s)
        return e
      }
    }
    let h = r
    let d = re(r, 'hx-sync')
    let g = null
    let F = false
    if (d) {
      const N = d.split(':')
      const I = N[0].trim()
      if (I === 'this') {
        h = Se(r, 'hx-sync')
      } else {
        h = ue(ae(r, I))
      }
      d = (N[1] || 'drop').trim()
      u = ie(h)
      if (d === 'drop' && u.xhr && u.abortable !== true) {
        oe(s)
        return e
      } else if (d === 'abort') {
        if (u.xhr) {
          oe(s)
          return e
        } else {
          F = true
        }
      } else if (d === 'replace') {
        he(h, 'htmx:abort')
      } else if (d.indexOf('queue') === 0) {
        const W = d.split(' ')
        g = (W[1] || 'last').trim()
      }
    }
    if (u.xhr) {
      if (u.abortable) {
        he(h, 'htmx:abort')
      } else {
        if (g == null) {
          if (o) {
            const P = ie(o)
            if (P && P.triggerSpec && P.triggerSpec.queue) {
              g = P.triggerSpec.queue
            }
          }
          if (g == null) {
            g = 'last'
          }
        }
        if (u.queuedRequests == null) {
          u.queuedRequests = []
        }
        if (g === 'first' && u.queuedRequests.length === 0) {
          u.queuedRequests.push(function () {
            de(t, n, r, o, i)
          })
        } else if (g === 'all') {
          u.queuedRequests.push(function () {
            de(t, n, r, o, i)
          })
        } else if (g === 'last') {
          u.queuedRequests = []
          u.queuedRequests.push(function () {
            de(t, n, r, o, i)
          })
        }
        oe(s)
        return e
      }
    }
    const p = new XMLHttpRequest()
    u.xhr = p
    u.abortable = F
    const m = function () {
      u.xhr = null
      u.abortable = false
      if (u.queuedRequests != null && u.queuedRequests.length > 0) {
        const e = u.queuedRequests.shift()
        e()
      }
    }
    const B = re(r, 'hx-prompt')
    if (B) {
      var x = prompt(B)
      if (x === null || !he(r, 'htmx:prompt', { prompt: x, target: c })) {
        oe(s)
        m()
        return e
      }
    }
    if (f && !D) {
      if (!confirm(f)) {
        oe(s)
        m()
        return e
      }
    }
    let y = fn(r, c, x)
    if (t !== 'get' && !pn(r)) {
      y['Content-Type'] = 'application/x-www-form-urlencoded'
    }
    if (i.headers) {
      y = ce(y, i.headers)
    }
    const U = cn(r, t)
    let b = U.errors
    const j = U.formData
    if (i.values) {
      ln(j, qn(i.values))
    }
    const V = qn(En(r))
    const v = ln(j, V)
    let w = hn(v, r)
    if (Q.config.getCacheBusterParam && t === 'get') {
      w.set('org.htmx.cache-buster', ee(c, 'id') || 'true')
    }
    if (n == null || n === '') {
      n = ne().location.href
    }
    const S = bn(r, 'hx-request')
    const _ = ie(r).boosted
    let E = Q.config.methodsThatUseUrlParams.indexOf(t) >= 0
    const C = {
      boosted: _,
      useUrlParams: E,
      formData: w,
      parameters: An(w),
      unfilteredFormData: v,
      unfilteredParameters: An(v),
      headers: y,
      target: c,
      verb: t,
      errors: b,
      withCredentials: i.credentials || S.credentials || Q.config.withCredentials,
      timeout: i.timeout || S.timeout || Q.config.timeout,
      path: n,
      triggeringEvent: o,
    }
    if (!he(r, 'htmx:configRequest', C)) {
      oe(s)
      m()
      return e
    }
    n = C.path
    t = C.verb
    y = C.headers
    w = qn(C.parameters)
    b = C.errors
    E = C.useUrlParams
    if (b && b.length > 0) {
      he(r, 'htmx:validation:halted', C)
      oe(s)
      m()
      return e
    }
    const z = n.split('#')
    const $ = z[0]
    const O = z[1]
    let R = n
    if (E) {
      R = $
      const Z = !w.keys().next().done
      if (Z) {
        if (R.indexOf('?') < 0) {
          R += '?'
        } else {
          R += '&'
        }
        R += an(w)
        if (O) {
          R += '#' + O
        }
      }
    }
    if (!Tn(r, R, C)) {
      fe(r, 'htmx:invalidPath', C)
      oe(l)
      return e
    }
    p.open(t.toUpperCase(), R, true)
    p.overrideMimeType('text/html')
    p.withCredentials = C.withCredentials
    p.timeout = C.timeout
    if (S.noHeaders) {
    } else {
      for (const k in y) {
        if (y.hasOwnProperty(k)) {
          const Y = y[k]
          Cn(p, k, Y)
        }
      }
    }
    const H = {
      xhr: p,
      target: c,
      requestConfig: C,
      etc: i,
      boosted: _,
      select: X,
      pathInfo: { requestPath: n, finalRequestPath: R, responsePath: null, anchor: O },
    }
    p.onload = function () {
      try {
        const t = Hn(r)
        H.pathInfo.responsePath = On(p)
        M(r, H)
        if (H.keepIndicators !== true) {
          Qt(T, q)
        }
        he(r, 'htmx:afterRequest', H)
        he(r, 'htmx:afterOnLoad', H)
        if (!le(r)) {
          let e = null
          while (t.length > 0 && e == null) {
            const n = t.shift()
            if (le(n)) {
              e = n
            }
          }
          if (e) {
            he(e, 'htmx:afterRequest', H)
            he(e, 'htmx:afterOnLoad', H)
          }
        }
        oe(s)
        m()
      } catch (e) {
        fe(r, 'htmx:onLoadError', ce({ error: e }, H))
        throw e
      }
    }
    p.onerror = function () {
      Qt(T, q)
      fe(r, 'htmx:afterRequest', H)
      fe(r, 'htmx:sendError', H)
      oe(l)
      m()
    }
    p.onabort = function () {
      Qt(T, q)
      fe(r, 'htmx:afterRequest', H)
      fe(r, 'htmx:sendAbort', H)
      oe(l)
      m()
    }
    p.ontimeout = function () {
      Qt(T, q)
      fe(r, 'htmx:afterRequest', H)
      fe(r, 'htmx:timeout', H)
      oe(l)
      m()
    }
    if (!he(r, 'htmx:beforeRequest', H)) {
      oe(s)
      m()
      return e
    }
    var T = Zt(r)
    var q = Yt(r)
    se(['loadstart', 'loadend', 'progress', 'abort'], function (t) {
      se([p, p.upload], function (e) {
        e.addEventListener(t, function (e) {
          he(r, 'htmx:xhr:' + t, { lengthComputable: e.lengthComputable, loaded: e.loaded, total: e.total })
        })
      })
    })
    he(r, 'htmx:beforeSend', H)
    const J = E ? null : mn(p, r, w)
    p.send(J)
    return e
  }
  function Nn(e, t) {
    const n = t.xhr
    let r = null
    let o = null
    if (R(n, /HX-Push:/i)) {
      r = n.getResponseHeader('HX-Push')
      o = 'push'
    } else if (R(n, /HX-Push-Url:/i)) {
      r = n.getResponseHeader('HX-Push-Url')
      o = 'push'
    } else if (R(n, /HX-Replace-Url:/i)) {
      r = n.getResponseHeader('HX-Replace-Url')
      o = 'replace'
    }
    if (r) {
      if (r === 'false') {
        return {}
      } else {
        return { type: o, path: r }
      }
    }
    const i = t.pathInfo.finalRequestPath
    const s = t.pathInfo.responsePath
    const l = re(e, 'hx-push-url')
    const c = re(e, 'hx-replace-url')
    const u = ie(e).boosted
    let a = null
    let f = null
    if (l) {
      a = 'push'
      f = l
    } else if (c) {
      a = 'replace'
      f = c
    } else if (u) {
      a = 'push'
      f = s || i
    }
    if (f) {
      if (f === 'false') {
        return {}
      }
      if (f === 'true') {
        f = s || i
      }
      if (t.pathInfo.anchor && f.indexOf('#') === -1) {
        f = f + '#' + t.pathInfo.anchor
      }
      return { type: a, path: f }
    } else {
      return {}
    }
  }
  function In(e, t) {
    var n = new RegExp(e.code)
    return n.test(t.toString(10))
  }
  function Pn(e) {
    for (var t = 0; t < Q.config.responseHandling.length; t++) {
      var n = Q.config.responseHandling[t]
      if (In(n, e.status)) {
        return n
      }
    }
    return { swap: false }
  }
  function kn(e) {
    if (e) {
      const t = u('title')
      if (t) {
        t.innerHTML = e
      } else {
        window.document.title = e
      }
    }
  }
  function Dn(o, i) {
    const s = i.xhr
    let l = i.target
    const e = i.etc
    const c = i.select
    if (!he(o, 'htmx:beforeOnLoad', i)) return
    if (R(s, /HX-Trigger:/i)) {
      Je(s, 'HX-Trigger', o)
    }
    if (R(s, /HX-Location:/i)) {
      zt()
      let e = s.getResponseHeader('HX-Location')
      var t
      if (e.indexOf('{') === 0) {
        t = S(e)
        e = t.path
        delete t.path
      }
      Rn('get', e, t).then(function () {
        $t(e)
      })
      return
    }
    const n = R(s, /HX-Refresh:/i) && s.getResponseHeader('HX-Refresh') === 'true'
    if (R(s, /HX-Redirect:/i)) {
      i.keepIndicators = true
      location.href = s.getResponseHeader('HX-Redirect')
      n && location.reload()
      return
    }
    if (n) {
      i.keepIndicators = true
      location.reload()
      return
    }
    if (R(s, /HX-Retarget:/i)) {
      if (s.getResponseHeader('HX-Retarget') === 'this') {
        i.target = o
      } else {
        i.target = ue(ae(o, s.getResponseHeader('HX-Retarget')))
      }
    }
    const u = Nn(o, i)
    const r = Pn(s)
    const a = r.swap
    let f = !!r.error
    let h = Q.config.ignoreTitle || r.ignoreTitle
    let d = r.select
    if (r.target) {
      i.target = ue(ae(o, r.target))
    }
    var g = e.swapOverride
    if (g == null && r.swapOverride) {
      g = r.swapOverride
    }
    if (R(s, /HX-Retarget:/i)) {
      if (s.getResponseHeader('HX-Retarget') === 'this') {
        i.target = o
      } else {
        i.target = ue(ae(o, s.getResponseHeader('HX-Retarget')))
      }
    }
    if (R(s, /HX-Reswap:/i)) {
      g = s.getResponseHeader('HX-Reswap')
    }
    var p = s.response
    var m = ce({ shouldSwap: a, serverResponse: p, isError: f, ignoreTitle: h, selectOverride: d, swapOverride: g }, i)
    if (r.event && !he(l, r.event, m)) return
    if (!he(l, 'htmx:beforeSwap', m)) return
    l = m.target
    p = m.serverResponse
    f = m.isError
    h = m.ignoreTitle
    d = m.selectOverride
    g = m.swapOverride
    i.target = l
    i.failed = f
    i.successful = !f
    if (m.shouldSwap) {
      if (s.status === 286) {
        lt(o)
      }
      Ft(o, function (e) {
        p = e.transformResponse(p, s, o)
      })
      if (u.type) {
        zt()
      }
      var x = gn(o, g)
      if (!x.hasOwnProperty('ignoreTitle')) {
        x.ignoreTitle = h
      }
      l.classList.add(Q.config.swappingClass)
      let n = null
      let r = null
      if (c) {
        d = c
      }
      if (R(s, /HX-Reselect:/i)) {
        d = s.getResponseHeader('HX-Reselect')
      }
      const y = re(o, 'hx-select-oob')
      const b = re(o, 'hx-select')
      let e = function () {
        try {
          if (u.type) {
            he(ne().body, 'htmx:beforeHistoryUpdate', ce({ history: u }, i))
            if (u.type === 'push') {
              $t(u.path)
              he(ne().body, 'htmx:pushedIntoHistory', { path: u.path })
            } else {
              Jt(u.path)
              he(ne().body, 'htmx:replacedInHistory', { path: u.path })
            }
          }
          $e(l, p, x, {
            select: d || b,
            selectOOB: y,
            eventInfo: i,
            anchor: i.pathInfo.anchor,
            contextElement: o,
            afterSwapCallback: function () {
              if (R(s, /HX-Trigger-After-Swap:/i)) {
                let e = o
                if (!le(o)) {
                  e = ne().body
                }
                Je(s, 'HX-Trigger-After-Swap', e)
              }
            },
            afterSettleCallback: function () {
              if (R(s, /HX-Trigger-After-Settle:/i)) {
                let e = o
                if (!le(o)) {
                  e = ne().body
                }
                Je(s, 'HX-Trigger-After-Settle', e)
              }
              oe(n)
            },
          })
        } catch (e) {
          fe(o, 'htmx:swapError', i)
          oe(r)
          throw e
        }
      }
      let t = Q.config.globalViewTransitions
      if (x.hasOwnProperty('transition')) {
        t = x.transition
      }
      if (t && he(o, 'htmx:beforeTransition', i) && typeof Promise !== 'undefined' && document.startViewTransition) {
        const v = new Promise(function (e, t) {
          n = e
          r = t
        })
        const w = e
        e = function () {
          document.startViewTransition(function () {
            w()
            return v
          })
        }
      }
      if (x.swapDelay > 0) {
        E().setTimeout(e, x.swapDelay)
      } else {
        e()
      }
    }
    if (f) {
      fe(o, 'htmx:responseError', ce({ error: 'Response Status Error Code ' + s.status + ' from ' + i.pathInfo.requestPath }, i))
    }
  }
  const Mn = {}
  function Xn() {
    return {
      init: function (e) {
        return null
      },
      getSelectors: function () {
        return null
      },
      onEvent: function (e, t) {
        return true
      },
      transformResponse: function (e, t, n) {
        return e
      },
      isInlineSwap: function (e) {
        return false
      },
      handleSwap: function (e, t, n, r) {
        return false
      },
      encodeParameters: function (e, t, n) {
        return null
      },
    }
  }
  function Fn(e, t) {
    if (t.init) {
      t.init(n)
    }
    Mn[e] = ce(Xn(), t)
  }
  function Bn(e) {
    delete Mn[e]
  }
  function Un(e, n, r) {
    if (n == undefined) {
      n = []
    }
    if (e == undefined) {
      return n
    }
    if (r == undefined) {
      r = []
    }
    const t = te(e, 'hx-ext')
    if (t) {
      se(t.split(','), function (e) {
        e = e.replace(/ /g, '')
        if (e.slice(0, 7) == 'ignore:') {
          r.push(e.slice(7))
          return
        }
        if (r.indexOf(e) < 0) {
          const t = Mn[e]
          if (t && n.indexOf(t) < 0) {
            n.push(t)
          }
        }
      })
    }
    return Un(ue(c(e)), n, r)
  }
  var jn = false
  ne().addEventListener('DOMContentLoaded', function () {
    jn = true
  })
  function Vn(e) {
    if (jn || ne().readyState === 'complete') {
      e()
    } else {
      ne().addEventListener('DOMContentLoaded', e)
    }
  }
  function _n() {
    if (Q.config.includeIndicatorStyles !== false) {
      const e = Q.config.inlineStyleNonce ? ` nonce="${Q.config.inlineStyleNonce}"` : ''
      ne().head.insertAdjacentHTML(
        'beforeend',
        '<style' +
          e +
          '>      .' +
          Q.config.indicatorClass +
          '{opacity:0}      .' +
          Q.config.requestClass +
          ' .' +
          Q.config.indicatorClass +
          '{opacity:1; transition: opacity 200ms ease-in;}      .' +
          Q.config.requestClass +
          '.' +
          Q.config.indicatorClass +
          '{opacity:1; transition: opacity 200ms ease-in;}      </style>',
      )
    }
  }
  function zn() {
    const e = ne().querySelector('meta[name="htmx-config"]')
    if (e) {
      return S(e.content)
    } else {
      return null
    }
  }
  function $n() {
    const e = zn()
    if (e) {
      Q.config = ce(Q.config, e)
    }
  }
  Vn(function () {
    $n()
    _n()
    let e = ne().body
    kt(e)
    const t = ne().querySelectorAll("[hx-trigger='restored'],[data-hx-trigger='restored']")
    e.addEventListener('htmx:abort', function (e) {
      const t = e.target
      const n = ie(t)
      if (n && n.xhr) {
        n.xhr.abort()
      }
    })
    const n = window.onpopstate ? window.onpopstate.bind(window) : null
    window.onpopstate = function (e) {
      if (e.state && e.state.htmx) {
        Wt()
        se(t, function (e) {
          he(e, 'htmx:restored', { document: ne(), triggerEvent: he })
        })
      } else {
        if (n) {
          n(e)
        }
      }
    }
    E().setTimeout(function () {
      he(e, 'htmx:load', {})
      e = null
    }, 0)
  })
  return Q
})()
