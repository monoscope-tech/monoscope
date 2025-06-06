// Utility functions for APIToolkit

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
  const minutes = String(absOffset % 60).padStart(2, '0')
  return `UTC${sign}${hours}:${minutes}`
}
window.getUTCOffset = getUTCOffset

