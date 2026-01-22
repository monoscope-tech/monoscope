// Utility functions for Monoscope

/**
 * Debounce function to limit how often a function can be called
 * @param {Function} func - The function to debounce
 * @param {number} wait - The debounce delay in milliseconds
 * @returns {Function} - The debounced function
 */
function debounce(func, wait) {
  let timeout
  return function (...args) {
    if (timeout) {
      clearTimeout(timeout)
    }
    timeout = setTimeout(() => func(...args), wait)
  }
}

/**
 * Recursively binds functions in an object to a root object
 * @param {Object} rootObj - The object to bind functions to
 * @param {Object} obj - The object containing functions to bind
 * @returns {Object} - The object with bound functions
 */
function bindFunctionsToObjects(rootObj, obj) {
  if (!obj || typeof obj !== 'object') return

  Object.keys(obj).forEach(function (key) {
    const value = obj[key]
    if (typeof value === 'function') {
      obj[key] = value.bind(rootObj)
    } else if (value && typeof value === 'object') {
      bindFunctionsToObjects(rootObj, value)
    }
  })

  return obj
}

// Export the functions
window.debounce = debounce
window.bindFunctionsToObjects = bindFunctionsToObjects

// Define htmx debug extension
htmx.defineExtension('debug', {
  onEvent: function (name, evt) {
    if (console.debug) {
      console.debug(name, evt)
    } else if (console) {
      console.log('DEBUG:', name, evt)
    } else {
      throw new Error('NO CONSOLE SUPPORTED')
    }
  },
})

function getUTCOffset() {
  const now = new Date()
  const offset = now.getTimezoneOffset()
  const sign = offset > 0 ? '-' : '+'
  const absOffset = Math.abs(offset)
  const hours = String(Math.floor(absOffset / 60)).padStart(2, '0')
  const minutes = absOffset % 60
  return minutes > 0 ? `UTC${sign}${hours}:${String(minutes).padStart(2, '0')}` : `UTC${sign}${hours}`
}
window.getUTCOffset = getUTCOffset

// Query editor access function
window.getQueryFromEditor = () =>
  [document.activeElement?.closest('form')?.querySelector('query-editor'), document.getElementById('filterElement'), document.querySelector('query-editor')]
    .find(el => el && el.editor)
    ?.editor.getValue() || ''

// Visualization type getter - returns the current viz type from URL or selected tab
window.getVizType = () => {
  const urlParams = new URLSearchParams(window.location.search)
  const urlVizType = urlParams.get('viz_type')
  if (urlVizType) return urlVizType

  const checkedRadio = document.querySelector('#visualizationTabs input[type="radio"]:checked')
  if (checkedRadio) {
    const value = checkedRadio.value
    // Map viz-* prefix to the actual type name for storage
    return value.startsWith('viz-') ? value.substring(4) : value
  }
  return 'timeseries' // default
}

// Time range getter from UI
window.getTimeRange = () => {
  const customRange = document.getElementById('custom_range_input')?.value
  return customRange
    ? { since: customRange, from: '', to: '' }
    : {
        since: '',
        from: document.querySelector('input[name="from"]')?.value || '',
        to: document.querySelector('input[name="to"]')?.value || '',
      }
}

// URL parameters helper
window.params = () => {
  const params = Object.fromEntries(new URL(location.href).searchParams)
  params.cols = params.cols || ''
  return params
}

window.updateGroupByButtonText = (_e, self) => {
  const el = self,
    ed = document.querySelector('#filterElement')?.editor,
    v = ed?.getValue().toLowerCase() || '',
    field = el.dataset.field || el.closest('[data-field-path]')?.dataset.fieldPath,
    span = el.querySelector('span')

  if (span && field && ed) {
    const isGrouped = ['summarize', 'by', field.toLowerCase()].every(s => v.includes(s))
    span.textContent = isGrouped ? 'Remove group by' : 'Group by'
  }
}

/**
 * Animate a stat value from its current value to a new value
 * @param {HTMLElement} el - The element containing the number
 * @param {number} newValue - The target value
 * @param {number} duration - Animation duration in ms (default 500)
 */
window.animateStatValue = (el, newValue, duration = 500) => {
  if (!el) return
  if (el._animationFrameId) cancelAnimationFrame(el._animationFrameId)
  const currentText = el.textContent.replace(/[^0-9.-]/g, '')
  const startValue = parseFloat(currentText) || 0
  const startTime = performance.now()

  // Add pulse animation class
  el.classList.add('stat-updated')
  setTimeout(() => el.classList.remove('stat-updated'), 1000)

  const animate = (currentTime) => {
    const elapsed = currentTime - startTime
    const progress = Math.min(elapsed / duration, 1)
    // Ease out cubic
    const eased = 1 - Math.pow(1 - progress, 3)
    const currentValue = startValue + (newValue - startValue) * eased

    // Format with commas
    el.textContent = Math.round(currentValue).toLocaleString()

    if (progress < 1) el._animationFrameId = requestAnimationFrame(animate)
    else el._animationFrameId = null
  }

  el._animationFrameId = requestAnimationFrame(animate)
}

/**
 * Add entrance animation to dynamically loaded content
 * @param {HTMLElement} container - The container to animate children of
 */
window.animateContentEntrance = (container) => {
  if (!container) return
  container.classList.add('animate-fadeIn')
  container.addEventListener('animationend', () => container.classList.remove('animate-fadeIn'), { once: true })
}

/**
 * Update URL query parameter without page reload
 * @param {string} key - Parameter key
 * @param {string} value - Parameter value
 */
window.updateUrlState = (key, value) => {
  const url = new URL(window.location.href)
  if (value === null || value === undefined || value === '') {
    url.searchParams.delete(key)
  } else {
    url.searchParams.set(key, value)
  }
  history.replaceState(null, '', url.toString())
}

/**
 * Create a focus trap within an element (for modals/drawers)
 * @param {HTMLElement} container - The element to trap focus within
 * @returns {Function} - Cleanup function to remove the trap
 */
window.createFocusTrap = (container) => {
  const focusableSelectors = 'button, [href], input:not([type="hidden"]), select, textarea, [tabindex]:not([tabindex="-1"])'

  const handleKeydown = (e) => {
    if (e.key !== 'Tab') return

    const focusables = [...container.querySelectorAll(focusableSelectors)].filter(el => !el.disabled && el.offsetParent !== null)
    if (focusables.length === 0) return

    const first = focusables[0]
    const last = focusables[focusables.length - 1]

    if (e.shiftKey && document.activeElement === first) {
      e.preventDefault()
      last.focus()
    } else if (!e.shiftKey && document.activeElement === last) {
      e.preventDefault()
      first.focus()
    }
  }

  container.addEventListener('keydown', handleKeydown)
  return () => container.removeEventListener('keydown', handleKeydown)
}

/**
 * Show a success checkmark animation at element position
 * @param {HTMLElement} el - Element to show success near
 */
window.showSuccessAt = (el) => {
  if (!el) return
  const rect = el.getBoundingClientRect()
  const check = document.createElement('span')
  check.textContent = '✓'
  check.className = 'fixed text-fillSuccess-strong text-xl success-pop pointer-events-none z-[99999]'
  check.style.cssText = `left: ${rect.right + 8}px; top: ${rect.top + rect.height / 2 - 10}px;`
  document.body.appendChild(check)
  setTimeout(() => check.remove(), 1000)
}

/**
 * Highlight an element briefly (for drawing attention)
 * @param {HTMLElement} el - Element to highlight
 */
window.highlightElement = (el) => {
  if (!el) return
  el.classList.add('stat-updated')
  setTimeout(() => el.classList.remove('stat-updated'), 1500)
}

/**
 * Smoothly scroll element into view with offset for fixed headers
 * @param {HTMLElement} el - Element to scroll to
 * @param {number} offset - Offset from top (default 80 for navbar)
 */
window.scrollToElement = (el, offset = 80) => {
  if (!el) return
  const top = el.getBoundingClientRect().top + window.scrollY - offset
  window.scrollTo({ top, behavior: 'smooth' })
}

/**
 * Copy text to clipboard with visual feedback
 * @param {string} text - Text to copy
 * @param {HTMLElement} triggerEl - Element that triggered the copy (for feedback)
 */
window.copyToClipboard = async (text, triggerEl) => {
  try {
    await navigator.clipboard.writeText(text)
    if (triggerEl) {
      triggerEl.classList.add('copy-success')
      setTimeout(() => triggerEl.classList.remove('copy-success'), 1500)
    }
    return true
  } catch (err) {
    console.error('Copy failed:', err)
    if (triggerEl) {
      triggerEl.classList.add('copy-failed')
      setTimeout(() => triggerEl.classList.remove('copy-failed'), 1500)
    }
    return false
  }
}

/**
 * Setup sticky header detection using IntersectionObserver
 * @param {string} sentinelId - ID of the sentinel element
 * @param {string} targetId - ID of the container to add/remove 'stuck' class
 * @param {string} stuckClass - Class to toggle when stuck (default: 'widget-drawer-stuck')
 */
window.setupStickyObserver = (sentinelId, targetId, stuckClass = 'widget-drawer-stuck') => {
  const sentinel = document.getElementById(sentinelId)
  const target = document.getElementById(targetId)
  if (!sentinel || !target) return
  if (target._stickyObserver) target._stickyObserver.disconnect()

  const observer = new IntersectionObserver(
    ([entry]) => target.classList.toggle(stuckClass, !entry.isIntersecting),
    { root: target.closest('.overflow-y-scroll'), threshold: 0 }
  )
  observer.observe(sentinel)
  target._stickyObserver = observer
}

