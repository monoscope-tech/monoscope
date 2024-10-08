'use strict'

function getEvent(eventName, value) {
  const event = new CustomEvent(eventName, {
    detail: value,
    bubbles: true,
    composed: true,
  })
  return event
}

const METHODS = ['GET', 'POST', 'PATCH', 'PUT', 'DELETE', 'HEAD', 'OPTIONS']
const ASSERTS = ['exists', 'number', 'string', 'array', 'date', 'boolean', 'ok', 'empty', 'notEmpty', 'null']

function triggerToastEvent(event) {
  document.querySelector('body').dispatchEvent(event)
}

function mergeErrors(errors, errs) {
  if (errs === undefined) {
    return errors
  }
  if (errors === undefined) {
    errors = {}
  }
  return { ...errors, ...errs }
}

function isValidStep(step) {
  let errors
  errors = mergeErrors(errors, getMethodAndUrlErrors(step))
  if (step.asserts) {
    errors = mergeErrors(errors, getAssertErrors(step.asserts))
  }
  if (step.headers) {
    errors = mergeErrors(errors, getHeaderErrors(step.headers))
  }

  return errors
}

function getHeaderErrors(headers) {
  // Check if headers is an object
  if (typeof headers !== 'object' || headers === null || Array.isArray(headers)) {
    return { headers: 'Header must be key value object pair' }
  }
  const errors = []
  for (const [key, value] of Object.entries(headers)) {
    const err = undefined
    if (typeof key !== 'string' || key.trim() === '') {
      if (!err) {
        err.key = 'Header key must be a non empty string'
      }
    }
    if (typeof value !== 'string') {
      if (!err) {
        err.value = 'Header value must be a non empty string'
      }
    }
    errors.push(err)
  }
  if (errors.filter((e) => e !== undefined).length > 0) {
    return { headers: errors }
  }
  return undefined
}

function getMethodAndUrlErrors(obj) {
  if (typeof obj !== 'object' || obj === null || Array.isArray(obj)) {
    return { method: 'No valid http method found' }
  }
  const httpMethodKeys = Object.keys(obj).filter((key) => METHODS.includes(key))
  if (httpMethodKeys.length === 0) {
    return { method: 'No valid http method found' }
  }
  const httpMethodValue = obj[httpMethodKeys[0]]
  if (typeof httpMethodValue !== 'string') {
    return { url: 'No valid url found' }
  }
  return undefined
}

function getAssertErrors(asserts) {
  if (!Array.isArray(asserts)) {
    return { asserts: 'Asserts must be an array' }
  }

  let errors = []
  for (const item of asserts) {
    let err = undefined
    if (typeof item !== 'object' || item === null || Array.isArray(item)) {
      err.value = 'Assert must be an object'
    }
    const keys = Object.keys(item)

    if (!ASSERTS.includes(keys[0])) {
      if (!err) {
        err = {}
      }
      err.key = 'Invalid assert key'
    }
    if (typeof item[keys[0]] !== 'string') {
      if (!err) {
        err = {}
      }

      err.value = 'Invalid assert value'
    }
    errors.push(err)
  }
  if (errors.filter((e) => e !== undefined).length > 0) {
    return { asserts: errors }
  }
  return undefined
}

function validateYaml(data) {
  try {
    if (!Array.isArray(data)) {
      triggerToastEvent(getEvent('errorToast', { value: ['Array of steps expected'] }))
      return undefined
    }
    let errors = []
    for (let [_, step] of data.entries()) {
      errors.push(isValidStep(step))
    }
    window.updateStepsWithErrors(errors)
    if (errors.filter((err) => err !== undefined).length > 0) {
      triggerToastEvent(getEvent('errorToast', { value: ['Saving failed. Please fix the errors and try again.'] }))
      return
    }
    data.map((step) => {
      if (step.json) {
        step.json = typeof step.json === 'string' ? step.json : JSON.stringify(step.json)
      }
    })
    return data
  } catch (error) {
    const event = getEvent('errorToast', { value: ['Invalid yaml'] })
    triggerToastEvent(event)
    return undefined
  }
}

function convertCollectionStepsToTestkitFormat(collectionSteps) {
  const testkitSteps = []
  if (Array.isArray(collectionSteps)) {
    collectionSteps.forEach((step) => {
      const assertions = []
      step._assertions?.forEach((assertion) => {
        assertions.push(convertToTestkitAssertion(assertion))
      })
      const testkitStep = {
        title: step.title || '',
        [step._method || 'GET']: step._url,
        headers: step.headers || {},
        exports: step._exports || {},
        assertions: assertions || [],
      }
      testkitSteps.push(testkitStep)
    })
  }
  return testkitSteps
}

function convertToTestkitAssertion(assertion) {
  let jsonpath = ''
  let operation = 'ok'
  let evalOperation = '=='

  if (assertion.type === 'header') {
    jsonpath = `$.resp.headers.${assertion.headerName}`
  } else if (assertion.type === 'body') {
    jsonpath = `$.resp.json.${assertion.jsonpath.substring(2)}`
  } else if (assertion.type === 'statusCode') {
    jsonpath = `$.resp.status`
  } else if (assertion.type === 'responseTime') {
    jsonpath = `$.resp.duration_ms`
  }

  if (assertion.operation === 'equals') {
    evalOperation = '=='
  } else if (assertion.operation === 'notEquals') {
    evalOperation = '!='
  } else if (assertion.operation === 'greaterThan') {
    evalOperation = '>'
  } else if (assertion.operation === 'lessThan') {
    evalOperation = '<'
  } else if (assertion.operation === 'greaterThanOrEqual') {
    evalOperation = '>='
  } else if (assertion.operation === 'lessThanOrEqual') {
    evalOperation = '<='
  }
  return {
    [operation]: `${jsonpath} ${evalOperation} ${assertion.value}`,
  }
}
