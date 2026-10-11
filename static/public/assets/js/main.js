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

// Classic script, so `init` hyperscript (e.g. params().fullscreen) can read it before the deferred bundle redefines it.
window.params = () => Object.fromEntries(new URLSearchParams(location.search))

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
