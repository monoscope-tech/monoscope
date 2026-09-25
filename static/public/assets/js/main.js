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

function getUTCOffset(timeZone = document.documentElement.dataset.timezone) {
  const now = new Date(Math.floor(Date.now() / 1000) * 1000)
  let minutesEast = -now.getTimezoneOffset()

  // The project timezone is authoritative across Monoscope. Derive its offset for the
  // current instant so daylight-saving transitions (for example Berlin UTC+1/UTC+2)
  // are reflected even when the browser process itself runs in UTC.
  if (timeZone) {
    try {
      const values = Object.fromEntries(
        new Intl.DateTimeFormat('en-US', {
          timeZone,
          year: 'numeric',
          month: '2-digit',
          day: '2-digit',
          hour: '2-digit',
          minute: '2-digit',
          second: '2-digit',
          hourCycle: 'h23',
        })
          .formatToParts(now)
          .filter(({ type }) => type !== 'literal')
          .map(({ type, value }) => [type, value])
      )
      const inZoneAsUTC = Date.UTC(
        Number(values.year),
        Number(values.month) - 1,
        Number(values.day),
        Number(values.hour),
        Number(values.minute),
        Number(values.second)
      )
      minutesEast = Math.round((inZoneAsUTC - now.getTime()) / 60000)
    } catch {
      // Invalid or unavailable IANA data: retain the browser offset as a safe fallback.
    }
  }

  const sign = minutesEast < 0 ? '-' : '+'
  const absOffset = Math.abs(minutesEast)
  const hours = Math.floor(absOffset / 60)
  const minutes = absOffset % 60
  return minutes > 0 ? `UTC${sign}${hours}:${String(minutes).padStart(2, '0')}` : `UTC${sign}${hours}`
}
window.getUTCOffset = getUTCOffset

function formatTimeRange(start, end, timeZone = document.documentElement.dataset.timezone) {
  const startDate = new Date(start)
  const endDate = new Date(end)
  if (Number.isNaN(startDate.getTime()) || Number.isNaN(endDate.getTime())) return `${start} – ${end}`

  const zone = timeZone ? { timeZone } : {}
  const dateKey = (date) =>
    new Intl.DateTimeFormat('en-CA', { ...zone, year: 'numeric', month: '2-digit', day: '2-digit' })
      .formatToParts(date)
      .filter(({ type }) => type !== 'literal')
      .map(({ value }) => value)
      .join('-')
  const year = (date) => new Intl.DateTimeFormat('en', { ...zone, year: 'numeric' }).format(date)
  const currentYear = year(new Date())
  const spansYears = year(startDate) !== year(endDate)
  const dateLabel = (date) => {
    const parts = Object.fromEntries(
      new Intl.DateTimeFormat('en', {
        ...zone,
        day: 'numeric',
        month: 'short',
        ...(spansYears || year(date) !== currentYear ? { year: 'numeric' } : {}),
      })
        .formatToParts(date)
        .filter(({ type }) => type !== 'literal')
        .map(({ type, value }) => [type, value])
    )
    return `${parts.day} ${parts.month}${parts.year ? ` ${parts.year}` : ''}`
  }
  const timeLabel = (date) =>
    new Intl.DateTimeFormat('en', {
      ...zone,
      hour: '2-digit',
      minute: '2-digit',
      hourCycle: 'h23',
    }).format(date)

  return dateKey(startDate) === dateKey(endDate)
    ? `${dateLabel(startDate)}, ${timeLabel(startDate)}–${timeLabel(endDate)}`
    : `${dateLabel(startDate)}, ${timeLabel(startDate)} – ${dateLabel(endDate)}, ${timeLabel(endDate)}`
}
window.formatTimeRange = formatTimeRange
window.dispatchEvent(new CustomEvent('monoscope:time-format-ready'))

// Query editor access function
window.getQueryFromEditor = () => {
  const el = [document.activeElement?.closest('form')?.querySelector('query-editor'), document.getElementById('filterElement'), document.querySelector('query-editor')].find(Boolean)
  return el?.getValue?.() ?? el?.querySelector('textarea[data-query-input]')?.value ?? el?.getAttribute('default-value') ?? ''
}

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
    ed = document.querySelector('#filterElement'),
    v = ed?.getValue?.().toLowerCase() || '',
    field = el.dataset.field || el.closest('[data-field-path]')?.dataset.fieldPath,
    // Only the verb span — the field key (.ctx-key) is a sibling and must be preserved.
    span = el.querySelector('.gb-verb') || el.querySelector('span')

  if (span && field && ed) {
    const isGrouped = ['summarize', 'by', field.toLowerCase()].every(s => v.includes(s))
    span.textContent = isGrouped ? 'Remove group by ' : 'Group by '
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

/** Label a drawer from the first heading in its loaded content. */
window.labelDrawer = (drawer) => {
  if (!drawer) return
  const title = drawer.querySelector('[data-drawer-title], h1, h2, h3')
  if (!title) {
    drawer.removeAttribute('aria-labelledby')
    drawer.setAttribute('aria-label', 'Details')
    return
  }
  if (!title.id) title.id = `${drawer.id}-title`
  drawer.setAttribute('aria-labelledby', title.id)
  drawer.removeAttribute('aria-label')
}

document.addEventListener('htmx:after:swap', () => {
  document.querySelectorAll('.drawer-toggle:checked').forEach(toggle => {
    window.labelDrawer(toggle.closest('.drawer')?.querySelector('[role="dialog"]'))
  })
})

/**
 * Isolate focus and screen-reader navigation within a modal drawer.
 * @param {HTMLElement} container - The drawer's dialog panel
 * @returns {Function} - Cleanup function that restores the page and trigger focus
 */
window.createFocusTrap = (container) => {
  const focusableSelectors = 'button, [href], input:not([type="hidden"]), select, textarea, [tabindex]:not([tabindex="-1"])'
  const drawer = container.closest('.drawer')
  const toggle = drawer.querySelector('.drawer-toggle')
  const previousFocus = toggle._returnFocus || document.activeElement
  const inerted = []
  const drawerSide = container.closest('.drawer-side')

  window.labelDrawer(container)
  container.setAttribute('aria-modal', 'true')
  for (let node = drawerSide; node && node !== document.body; node = node.parentElement) {
    const parent = node.parentElement
    if (!parent) break
    for (const sibling of parent.children) {
      if (sibling === node || sibling.classList.contains('drawer-toggle') || sibling.inert) continue
      sibling.inert = true
      inerted.push(sibling)
    }
  }

  const handleKeydown = (e) => {
    if (e.key === 'Escape') {
      e.preventDefault()
      toggle.checked = false
      toggle.dispatchEvent(new Event('change', { bubbles: true }))
      return
    }
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
  return () => {
    container.removeAttribute('aria-modal')
    container.removeEventListener('keydown', handleKeydown)
    inerted.forEach(element => { element.inert = false })
    if (previousFocus?.isConnected) previousFocus.focus()
  }
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
