/**
 * Bundled by jsDelivr using Rollup v2.79.2 and Terser v5.37.0.
 * Original file: /npm/@lit-labs/virtualizer@2.1.0/lit-virtualizer.js
 *
 * Do NOT use SRI with dynamically generated files! More information: https://www.jsdelivr.com/using-sri-with-dynamic-files
 */
import { __decorate as t } from '/npm/tslib@2.8.1/+esm'
import { noChange as e, html as i, LitElement as s } from './lit.js'
import { property as l } from './lit.js'
import { directive as r, PartType as n } from './lit.js'
import { AsyncDirective as o } from './lit.js'
import { repeat as h } from './lit.js'
/**
 * @license
 * Copyright 2021 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */ class a extends Event {
  constructor(t) {
    super(a.eventName, { bubbles: !1 }), (this.first = t.first), (this.last = t.last)
  }
}
a.eventName = 'rangeChanged'
class c extends Event {
  constructor(t) {
    super(c.eventName, { bubbles: !1 }), (this.first = t.first), (this.last = t.last)
  }
}
c.eventName = 'visibilityChanged'
class _ extends Event {
  constructor() {
    super(_.eventName, { bubbles: !1 })
  }
}
_.eventName = 'unpinned'
/**
 * @license
 * Copyright 2021 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
class d {
  constructor(t) {
    this._element = null
    const e = t ?? window
    ;(this._node = e), t && (this._element = t)
  }
  get element() {
    return this._element || document.scrollingElement || document.documentElement
  }
  get scrollTop() {
    return this.element.scrollTop || window.scrollY
  }
  get scrollLeft() {
    return this.element.scrollLeft || window.scrollX
  }
  get scrollHeight() {
    return this.element.scrollHeight
  }
  get scrollWidth() {
    return this.element.scrollWidth
  }
  get viewportHeight() {
    return this._element ? this._element.getBoundingClientRect().height : window.innerHeight
  }
  get viewportWidth() {
    return this._element ? this._element.getBoundingClientRect().width : window.innerWidth
  }
  get maxScrollTop() {
    return this.scrollHeight - this.viewportHeight
  }
  get maxScrollLeft() {
    return this.scrollWidth - this.viewportWidth
  }
}
class u extends d {
  constructor(t, e) {
    super(e),
      (this._clients = new Set()),
      (this._retarget = null),
      (this._end = null),
      (this.__destination = null),
      (this.correctingScrollError = !1),
      (this._checkForArrival = this._checkForArrival.bind(this)),
      (this._updateManagedScrollTo = this._updateManagedScrollTo.bind(this)),
      (this.scrollTo = this.scrollTo.bind(this)),
      (this.scrollBy = this.scrollBy.bind(this))
    const i = this._node
    ;(this._originalScrollTo = i.scrollTo), (this._originalScrollBy = i.scrollBy), (this._originalScroll = i.scroll), this._attach(t)
  }
  get _destination() {
    return this.__destination
  }
  get scrolling() {
    return null !== this._destination
  }
  scrollTo(t, e) {
    const i = 'number' == typeof t && 'number' == typeof e ? { left: t, top: e } : t
    this._scrollTo(i)
  }
  scrollBy(t, e) {
    const i = 'number' == typeof t && 'number' == typeof e ? { left: t, top: e } : t
    void 0 !== i.top && (i.top += this.scrollTop), void 0 !== i.left && (i.left += this.scrollLeft), this._scrollTo(i)
  }
  _nativeScrollTo(t) {
    this._originalScrollTo.bind(this._element || window)(t)
  }
  _scrollTo(t, e = null, i = null) {
    null !== this._end && this._end(),
      'smooth' === t.behavior ? (this._setDestination(t), (this._retarget = e), (this._end = i)) : this._resetScrollState(),
      this._nativeScrollTo(t)
  }
  _setDestination(t) {
    let { top: e, left: i } = t
    return (
      (e = void 0 === e ? void 0 : Math.max(0, Math.min(e, this.maxScrollTop))),
      (i = void 0 === i ? void 0 : Math.max(0, Math.min(i, this.maxScrollLeft))),
      (null === this._destination || i !== this._destination.left || e !== this._destination.top) &&
        ((this.__destination = { top: e, left: i, behavior: 'smooth' }), !0)
    )
  }
  _resetScrollState() {
    ;(this.__destination = null), (this._retarget = null), (this._end = null)
  }
  _updateManagedScrollTo(t) {
    this._destination && this._setDestination(t) && this._nativeScrollTo(this._destination)
  }
  managedScrollTo(t, e, i) {
    return this._scrollTo(t, e, i), this._updateManagedScrollTo
  }
  correctScrollError(t) {
    ;(this.correctingScrollError = !0),
      requestAnimationFrame(() => requestAnimationFrame(() => (this.correctingScrollError = !1))),
      this._nativeScrollTo(t),
      this._retarget && this._setDestination(this._retarget()),
      this._destination && this._nativeScrollTo(this._destination)
  }
  _checkForArrival() {
    if (null !== this._destination) {
      const { scrollTop: t, scrollLeft: e } = this
      let { top: i, left: s } = this._destination
      ;(i = Math.min(i || 0, this.maxScrollTop)), (s = Math.min(s || 0, this.maxScrollLeft))
      const l = Math.abs(i - t),
        r = Math.abs(s - e)
      l < 1 && r < 1 && (this._end && this._end(), this._resetScrollState())
    }
  }
  detach(t) {
    return (
      this._clients.delete(t),
      0 === this._clients.size &&
        ((this._node.scrollTo = this._originalScrollTo),
        (this._node.scrollBy = this._originalScrollBy),
        (this._node.scroll = this._originalScroll),
        this._node.removeEventListener('scroll', this._checkForArrival)),
      null
    )
  }
  _attach(t) {
    this._clients.add(t),
      1 === this._clients.size &&
        ((this._node.scrollTo = this.scrollTo),
        (this._node.scrollBy = this.scrollBy),
        (this._node.scroll = this.scrollTo),
        this._node.addEventListener('scroll', this._checkForArrival))
  }
}
let m = 'undefined' != typeof window ? window.ResizeObserver : void 0
const p = Symbol('virtualizerRef'),
  y = 'virtualizer-sizer'
let f
class g {
  constructor(t) {
    if (
      ((this._benchmarkStart = null),
      (this._layout = null),
      (this._clippingAncestors = []),
      (this._scrollSize = null),
      (this._scrollError = null),
      (this._childrenPos = null),
      (this._childMeasurements = null),
      (this._toBeMeasured = new Map()),
      (this._rangeChanged = !0),
      (this._itemsChanged = !0),
      (this._visibilityChanged = !0),
      (this._scrollerController = null),
      (this._isScroller = !1),
      (this._sizer = null),
      (this._hostElementRO = null),
      (this._childrenRO = null),
      (this._mutationObserver = null),
      (this._scrollEventListeners = []),
      (this._scrollEventListenerOptions = { passive: !0 }),
      (this._loadListener = this._childLoaded.bind(this)),
      (this._scrollIntoViewTarget = null),
      (this._updateScrollIntoViewCoordinates = null),
      (this._items = []),
      (this._first = -1),
      (this._last = -1),
      (this._firstVisible = -1),
      (this._lastVisible = -1),
      (this._scheduled = new WeakSet()),
      (this._measureCallback = null),
      (this._measureChildOverride = null),
      (this._layoutCompletePromise = null),
      (this._layoutCompleteResolver = null),
      (this._layoutCompleteRejecter = null),
      (this._pendingLayoutComplete = null),
      (this._layoutInitialized = null),
      (this._connected = !1),
      !t)
    )
      throw new Error('Virtualizer constructor requires a configuration object')
    if (!t.hostElement) throw new Error('Virtualizer configuration requires the "hostElement" property')
    this._init(t)
  }
  set items(t) {
    Array.isArray(t) && t !== this._items && ((this._itemsChanged = !0), (this._items = t), this._schedule(this._updateLayout))
  }
  _init(t) {
    ;(this._isScroller = !!t.scroller), this._initHostElement(t)
    const e = t.layout || {}
    this._layoutInitialized = this._initLayout(e)
  }
  _initObservers() {
    ;(this._mutationObserver = new MutationObserver(this._finishDOMUpdate.bind(this))),
      (this._hostElementRO = new m(() => this._hostElementSizeChanged())),
      (this._childrenRO = new m(this._childrenSizeChanged.bind(this)))
  }
  _initHostElement(t) {
    const e = (this._hostElement = t.hostElement)
    this._applyVirtualizerStyles(), (e[p] = this)
  }
  connected() {
    this._initObservers()
    const t = this._isScroller
    ;(this._clippingAncestors = (function (t, e = !1) {
      let i = !1
      return (function (t, e = !1) {
        const i = []
        let s = e ? t : w(t)
        for (; null !== s; ) i.push(s), (s = w(s))
        return i
      })(t, e).filter(t => {
        if (i) return !1
        const e = getComputedStyle(t)
        return (i = 'fixed' === e.position), 'visible' !== e.overflow
      })
    })(
      /**
       * @license
       * Copyright 2021 Google LLC
       * SPDX-License-Identifier: BSD-3-Clause
       */ this._hostElement,
      t,
    )),
      (this._scrollerController = new u(this, this._clippingAncestors[0])),
      this._schedule(this._updateLayout),
      this._observeAndListen(),
      (this._connected = !0)
  }
  _observeAndListen() {
    this._mutationObserver.observe(this._hostElement, { childList: !0 }),
      this._hostElementRO.observe(this._hostElement),
      this._scrollEventListeners.push(window),
      window.addEventListener('scroll', this, this._scrollEventListenerOptions),
      this._clippingAncestors.forEach(t => {
        t.addEventListener('scroll', this, this._scrollEventListenerOptions), this._scrollEventListeners.push(t), this._hostElementRO.observe(t)
      }),
      this._hostElementRO.observe(this._scrollerController.element),
      this._children.forEach(t => this._childrenRO.observe(t)),
      this._scrollEventListeners.forEach(t => t.addEventListener('scroll', this, this._scrollEventListenerOptions))
  }
  disconnected() {
    this._scrollEventListeners.forEach(t => t.removeEventListener('scroll', this, this._scrollEventListenerOptions)),
      (this._scrollEventListeners = []),
      (this._clippingAncestors = []),
      this._scrollerController?.detach(this),
      (this._scrollerController = null),
      this._mutationObserver?.disconnect(),
      (this._mutationObserver = null),
      this._hostElementRO?.disconnect(),
      (this._hostElementRO = null),
      this._childrenRO?.disconnect(),
      (this._childrenRO = null),
      this._rejectLayoutCompletePromise('disconnected'),
      (this._connected = !1)
  }
  _applyVirtualizerStyles() {
    const t = this._hostElement.style
    ;(t.display = t.display || 'block'),
      (t.position = t.position || 'relative'),
      (t.contain = t.contain || 'size layout'),
      this._isScroller && ((t.overflow = t.overflow || 'auto'), (t.minHeight = t.minHeight || '150px'))
  }
  _getSizer() {
    const t = this._hostElement
    if (!this._sizer) {
      let e = t.querySelector(`[${y}]`)
      e || ((e = document.createElement('div')), e.setAttribute(y, ''), t.appendChild(e)),
        Object.assign(e.style, { position: 'absolute', margin: '-2px 0 0 0', padding: 0, visibility: 'hidden', fontSize: '2px' }),
        (e.textContent = '&nbsp;'),
        e.setAttribute(y, ''),
        (this._sizer = e)
    }
    return this._sizer
  }
  async updateLayoutConfig(t) {
    await this._layoutInitialized
    const e = t.type || f
    if ('function' == typeof e && this._layout instanceof e) {
      const e = { ...t }
      return delete e.type, (this._layout.config = e), !0
    }
    return !1
  }
  async _initLayout(t) {
    let e, i
    if ('function' == typeof t.type) {
      i = t.type
      const s = { ...t }
      delete s.type, (e = s)
    } else e = t
    void 0 === i && (f = i = (await import('/npm/@lit-labs/virtualizer@2.1.0/layouts/flow.js/+esm')).FlowLayout),
      (this._layout = new i(t => this._handleLayoutMessage(t), e)),
      this._layout.measureChildren &&
        'function' == typeof this._layout.updateItemSizes &&
        ('function' == typeof this._layout.measureChildren && (this._measureChildOverride = this._layout.measureChildren),
        (this._measureCallback = this._layout.updateItemSizes.bind(this._layout))),
      this._layout.listenForChildLoadEvents && this._hostElement.addEventListener('load', this._loadListener, !0),
      this._schedule(this._updateLayout)
  }
  startBenchmarking() {
    null === this._benchmarkStart && (this._benchmarkStart = window.performance.now())
  }
  stopBenchmarking() {
    if (null !== this._benchmarkStart) {
      const t = window.performance.now(),
        e = t - this._benchmarkStart,
        i = performance
          .getEntriesByName('uv-virtualizing', 'measure')
          .filter(e => e.startTime >= this._benchmarkStart && e.startTime < t)
          .reduce((t, e) => t + e.duration, 0)
      return (this._benchmarkStart = null), { timeElapsed: e, virtualizationTime: i }
    }
    return null
  }
  _measureChildren() {
    const t = {},
      e = this._children,
      i = this._measureChildOverride || this._measureChild
    for (let s = 0; s < e.length; s++) {
      const l = e[s],
        r = this._first + s
      ;(this._itemsChanged || this._toBeMeasured.has(l)) && (t[r] = i.call(this, l, this._items[r]))
    }
    ;(this._childMeasurements = t), this._schedule(this._updateLayout), this._toBeMeasured.clear()
  }
  _measureChild(t) {
    const { width: e, height: i } = t.getBoundingClientRect()
    return Object.assign(
      { width: e, height: i },
      (function (t) {
        const e = window.getComputedStyle(t)
        return { marginTop: v(e.marginTop), marginRight: v(e.marginRight), marginBottom: v(e.marginBottom), marginLeft: v(e.marginLeft) }
      })(t),
    )
  }
  async _schedule(t) {
    this._scheduled.has(t) || (this._scheduled.add(t), await Promise.resolve(), this._scheduled.delete(t), t.call(this))
  }
  async _updateDOM(t) {
    ;(this._scrollSize = t.scrollSize), this._adjustRange(t.range), (this._childrenPos = t.childPositions), (this._scrollError = t.scrollError || null)
    const { _rangeChanged: e, _itemsChanged: i } = this
    this._visibilityChanged && (this._notifyVisibility(), (this._visibilityChanged = !1)),
      (e || i) && (this._notifyRange(), (this._rangeChanged = !1)),
      this._finishDOMUpdate()
  }
  _finishDOMUpdate() {
    this._connected &&
      (this._children.forEach(t => this._childrenRO.observe(t)),
      this._checkScrollIntoViewTarget(this._childrenPos),
      this._positionChildren(this._childrenPos),
      this._sizeHostElement(this._scrollSize),
      this._correctScrollError(),
      this._benchmarkStart && 'mark' in window.performance && window.performance.mark('uv-end'))
  }
  _updateLayout() {
    this._layout &&
      this._connected &&
      ((this._layout.items = this._items),
      this._updateView(),
      null !== this._childMeasurements && (this._measureCallback && this._measureCallback(this._childMeasurements), (this._childMeasurements = null)),
      this._layout.reflowIfNeeded(),
      this._benchmarkStart && 'mark' in window.performance && window.performance.mark('uv-end'))
  }
  _handleScrollEvent() {
    if (this._benchmarkStart && 'mark' in window.performance) {
      try {
        window.performance.measure('uv-virtualizing', 'uv-start', 'uv-end')
      } catch (t) {
        console.warn('Error measuring performance data: ', t)
      }
      window.performance.mark('uv-start')
    }
    !1 === this._scrollerController.correctingScrollError && this._layout?.unpin(), this._schedule(this._updateLayout)
  }
  handleEvent(t) {
    if ('scroll' === t.type) (t.currentTarget === window || this._clippingAncestors.includes(t.currentTarget)) && this._handleScrollEvent()
    else console.warn('event not handled', t)
  }
  _handleLayoutMessage(t) {
    'stateChanged' === t.type
      ? this._updateDOM(t)
      : 'visibilityChanged' === t.type
      ? ((this._firstVisible = t.firstVisible), (this._lastVisible = t.lastVisible), this._notifyVisibility())
      : 'unpinned' === t.type && this._hostElement.dispatchEvent(new _())
  }
  get _children() {
    const t = []
    let e = this._hostElement.firstElementChild
    for (; e; ) e.hasAttribute(y) || t.push(e), (e = e.nextElementSibling)
    return t
  }
  _updateView() {
    const t = this._hostElement,
      e = this._scrollerController?.element,
      i = this._layout
    if (t && e && i) {
      let s, l, r, n
      const o = t.getBoundingClientRect()
      ;(s = 0), (l = 0), (r = window.innerHeight), (n = window.innerWidth)
      const h = this._clippingAncestors.map(t => t.getBoundingClientRect())
      h.unshift(o)
      for (const t of h) (s = Math.max(s, t.top)), (l = Math.max(l, t.left)), (r = Math.min(r, t.bottom)), (n = Math.min(n, t.right))
      const a = e.getBoundingClientRect(),
        c = { left: o.left - a.left, top: o.top - a.top },
        _ = { width: e.scrollWidth, height: e.scrollHeight },
        d = s - o.top + t.scrollTop,
        u = l - o.left + t.scrollLeft,
        m = Math.max(0, r - s),
        p = Math.max(0, n - l)
      ;(i.viewportSize = { width: p, height: m }), (i.viewportScroll = { top: d, left: u }), (i.totalScrollSize = _), (i.offsetWithinScroller = c)
    }
  }
  _sizeHostElement(t) {
    const e = 82e5,
      i = t && null !== t.width ? Math.min(e, t.width) : 0,
      s = t && null !== t.height ? Math.min(e, t.height) : 0
    if (this._isScroller) this._getSizer().style.transform = `translate(${i}px, ${s}px)`
    else {
      const t = this._hostElement.style
      ;(t.minWidth = i ? `${i}px` : '100%'), (t.minHeight = s ? `${s}px` : '100%')
    }
  }
  _positionChildren(t) {
    t &&
      t.forEach(({ top: t, left: e, width: i, height: s, xOffset: l, yOffset: r }, n) => {
        const o = this._children[n - this._first]
        o &&
          ((o.style.position = 'absolute'),
          (o.style.boxSizing = 'border-box'),
          (o.style.transform = `translate(${e}px, ${t}px)`),
          void 0 !== i && (o.style.width = i + 'px'),
          void 0 !== s && (o.style.height = s + 'px'),
          (o.style.left = void 0 === l ? null : l + 'px'),
          (o.style.top = void 0 === r ? null : r + 'px'))
      })
  }
  async _adjustRange(t) {
    const { _first: e, _last: i, _firstVisible: s, _lastVisible: l } = this
    ;(this._first = t.first),
      (this._last = t.last),
      (this._firstVisible = t.firstVisible),
      (this._lastVisible = t.lastVisible),
      (this._rangeChanged = this._rangeChanged || this._first !== e || this._last !== i),
      (this._visibilityChanged = this._visibilityChanged || this._firstVisible !== s || this._lastVisible !== l)
  }
  _correctScrollError() {
    if (this._scrollError) {
      const { scrollTop: t, scrollLeft: e } = this._scrollerController,
        { top: i, left: s } = this._scrollError
      ;(this._scrollError = null), this._scrollerController.correctScrollError({ top: t - i, left: e - s })
    }
  }
  element(t) {
    return (
      t === 1 / 0 && (t = this._items.length - 1),
      void 0 === this._items?.[t] ? void 0 : { scrollIntoView: (e = {}) => this._scrollElementIntoView({ ...e, index: t }) }
    )
  }
  _scrollElementIntoView(t) {
    if (t.index >= this._first && t.index <= this._last) this._children[t.index - this._first].scrollIntoView(t)
    else if (((t.index = Math.min(t.index, this._items.length - 1)), 'smooth' === t.behavior)) {
      const e = this._layout.getScrollIntoViewCoordinates(t),
        { behavior: i } = t
      ;(this._updateScrollIntoViewCoordinates = this._scrollerController.managedScrollTo(
        Object.assign(e, { behavior: i }),
        () => this._layout.getScrollIntoViewCoordinates(t),
        () => (this._scrollIntoViewTarget = null),
      )),
        (this._scrollIntoViewTarget = t)
    } else this._layout.pin = t
  }
  _checkScrollIntoViewTarget(t) {
    const { index: e } = this._scrollIntoViewTarget || {}
    e && t?.has(e) && this._updateScrollIntoViewCoordinates(this._layout.getScrollIntoViewCoordinates(this._scrollIntoViewTarget))
  }
  _notifyRange() {
    this._hostElement.dispatchEvent(new a({ first: this._first, last: this._last }))
  }
  _notifyVisibility() {
    this._hostElement.dispatchEvent(new c({ first: this._firstVisible, last: this._lastVisible }))
  }
  get layoutComplete() {
    return (
      this._layoutCompletePromise ||
        (this._layoutCompletePromise = new Promise((t, e) => {
          ;(this._layoutCompleteResolver = t), (this._layoutCompleteRejecter = e)
        })),
      this._layoutCompletePromise
    )
  }
  _rejectLayoutCompletePromise(t) {
    null !== this._layoutCompleteRejecter && this._layoutCompleteRejecter(t), this._resetLayoutCompleteState()
  }
  _scheduleLayoutComplete() {
    this._layoutCompletePromise &&
      null === this._pendingLayoutComplete &&
      (this._pendingLayoutComplete = requestAnimationFrame(() => requestAnimationFrame(() => this._resolveLayoutCompletePromise())))
  }
  _resolveLayoutCompletePromise() {
    null !== this._layoutCompleteResolver && this._layoutCompleteResolver(), this._resetLayoutCompleteState()
  }
  _resetLayoutCompleteState() {
    ;(this._layoutCompletePromise = null), (this._layoutCompleteResolver = null), (this._layoutCompleteRejecter = null), (this._pendingLayoutComplete = null)
  }
  _hostElementSizeChanged() {
    this._schedule(this._updateLayout)
  }
  _childLoaded() {}
  _childrenSizeChanged(t) {
    if (this._layout?.measureChildren) {
      for (const e of t) this._toBeMeasured.set(e.target, e.contentRect)
      this._measureChildren()
    }
    this._scheduleLayoutComplete(), (this._itemsChanged = !1), (this._rangeChanged = !1)
  }
}
function v(t) {
  const e = t ? parseFloat(t) : NaN
  return Number.isNaN(e) ? 0 : e
}
function w(t) {
  if (null !== t.assignedSlot) return t.assignedSlot
  if (null !== t.parentElement) return t.parentElement
  const e = t.parentNode
  return (e && e.nodeType === Node.DOCUMENT_FRAGMENT_NODE && e.host) || null
}
const b = t => t,
  C = (t, e) => i`${e}: ${JSON.stringify(t, null, 2)}`
const E = r(
  class extends o {
    constructor(t) {
      if (
        (super(t),
        (this._virtualizer = null),
        (this._first = 0),
        (this._last = -1),
        (this._renderItem = (t, e) => C(t, e + this._first)),
        (this._keyFunction = (t, e) => b(t, this._first)),
        (this._items = []),
        t.type !== n.CHILD)
      )
        throw new Error('The virtualize directive can only be used in child expressions')
    }
    render(t) {
      t && this._setFunctions(t)
      const e = []
      if (this._first >= 0 && this._last >= this._first) for (let t = this._first; t <= this._last; t++) e.push(this._items[t])
      return h(e, this._keyFunction, this._renderItem)
    }
    update(t, [i]) {
      this._setFunctions(i)
      const s = this._items !== i.items
      return (this._items = i.items || []), this._virtualizer ? this._updateVirtualizerConfig(t, i) : this._initialize(t, i), s ? e : this.render()
    }
    async _updateVirtualizerConfig(t, e) {
      if (!(await this._virtualizer.updateLayoutConfig(e.layout || {}))) {
        const i = t.parentNode
        this._makeVirtualizer(i, e)
      }
      this._virtualizer.items = this._items
    }
    _setFunctions(t) {
      const { renderItem: e, keyFunction: i } = t
      e && (this._renderItem = (t, i) => e(t, i + this._first)), i && (this._keyFunction = (t, e) => i(t, e + this._first))
    }
    _makeVirtualizer(t, e) {
      this._virtualizer && this._virtualizer.disconnected()
      const { layout: i, scroller: s, items: l } = e
      ;(this._virtualizer = new g({ hostElement: t, layout: i, scroller: s })), (this._virtualizer.items = l), this._virtualizer.connected()
    }
    _initialize(t, e) {
      const i = t.parentNode
      i &&
        1 === i.nodeType &&
        (i.addEventListener('rangeChanged', t => {
          ;(this._first = t.first), (this._last = t.last), this.setValue(this.render())
        }),
        this._makeVirtualizer(i, e))
    }
    disconnected() {
      this._virtualizer?.disconnected()
    }
    reconnected() {
      this._virtualizer?.connected()
    }
  },
)
/**
 * @license
 * Copyright 2021 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */ class S extends s {
  constructor() {
    super(...arguments), (this.items = []), (this.renderItem = C), (this.keyFunction = b), (this.layout = {}), (this.scroller = !1)
  }
  createRenderRoot() {
    return this
  }
  render() {
    const { items: t, renderItem: e, keyFunction: s, layout: l, scroller: r } = this
    return i`${E({ items: t, renderItem: e, keyFunction: s, layout: l, scroller: r })}`
  }
  element(t) {
    return this[p]?.element(t)
  }
  get layoutComplete() {
    return this[p]?.layoutComplete
  }
  scrollToIndex(t, e = 'start') {
    this.element(t)?.scrollIntoView({ block: e })
  }
}
t([l({ attribute: !1 })], S.prototype, 'items', void 0),
  t([l()], S.prototype, 'renderItem', void 0),
  t([l()], S.prototype, 'keyFunction', void 0),
  t([l({ attribute: !1 })], S.prototype, 'layout', void 0),
  t([l({ reflect: !0, type: Boolean })], S.prototype, 'scroller', void 0),
  /**
   * @license
   * Copyright 2021 Google LLC
   * SPDX-License-Identifier: BSD-3-Clause
   */
  customElements.define('lit-virtualizer', S)
export { S as LitVirtualizer, a as RangeChangedEvent, c as VisibilityChangedEvent }
export default null
//# sourceMappingURL=/sm/586dc6f1d1595b20986ca432c09ab00ef16eb415aee52eb8338bc49ceaef3562.map
