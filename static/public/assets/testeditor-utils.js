'use strict'

function getEvent(eventName, value) {
  const event = new CustomEvent(eventName, {
    detail: value,
    bubbles: true,
    composed: true,
  })
  return event
}

const METHODS = ['GET', 'POST', 'PATCH', 'PUT', 'DELETE', 'HEAD', 'OPTIONS', 'CONNECT', 'TRACE']
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
  data = convertCollectionStepsToTestkitFormat(data)
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

function getStepMethod(step) {
  const httpMethodKeys = Object.keys(step).filter((key) => METHODS.includes(key))
  if (httpMethodKeys.length === 0) {
    return 'GET'
  }
  return httpMethodKeys[0]
}

function convertTestkitToCollectionSteps(testkitSteps) {
  const collectionSteps = []
  if (Array.isArray(testkitSteps)) {
    testkitSteps.forEach((step) => {
      const assertions = []
      if (step.asserts && Array.isArray(step.asserts)) {
        step.asserts.forEach((assertion) => {
          const tka = convertTestkitAssertions(assertion)
          if (tka) {
            assertions.push(tka)
          }
        })
      }
      const method = getStepMethod(step)
      const collectionStep = {
        title: step.title || '',
        _method: method,
        _url: step[method],
        headers: step.headers || {},
        params: step.params || {},
        _assertions: assertions,
        exports: step.exports || {},
        _json: step.json,
        _requestBody: step.requestBody,
        followRedirects: step.followRedirects || true,
        allowRedirects: step.allowRedirects || true,
        ignoreSSLErrors: step.ignoreSSLErrors || false,
        timeout: step.timeout || 60,
        httpVersion: step.httpVersion || 'http1',
      }
      collectionSteps.push(collectionStep)
    })
  }
  return collectionSteps
}

function convertTestkitAssertions(assertion) {
  const keys = Object.keys(assertion)
  if (keys.length > 0) {
    const value = assertion[keys[0]]
    if (keys[0] === 'ok') {
      const [jsonpath, evalOperation, val] = value.split(' ')
      const operation = getTextOperation(evalOperation)
      if (jsonpath.startsWith('$.resp.json')) {
        return {
          type: 'body',
          operation: 'jsonpath',
          jsonpath: jsonpath.replace('.resp.json', ''),
          subOperation: operation,
          value: val,
        }
      } else if (jsonpath.startsWith('$.resp.headers')) {
        return {
          type: 'header',
          operation: operation,
          headerName: jsonpath.replace('$.resp.headers.', ''),
          value: val,
        }
      }
      return {
        type: 'statusCode',
        operation: operation,
        value: val,
      }
    } else {
      const operation = keys[0]
      const jsonpath = value
      if (jsonpath.startsWith('$.resp.json')) {
        return {
          type: 'body',
          operation: 'jsonpath',
          jsonpath: jsonpath.replace('.resp.json', ''),
          subOperation: operation,
          value: '',
        }
      } else if (jsonpath.startsWith('$.resp.headers')) {
        return {
          type: 'header',
          operation: operation,
          headerName: jsonpath.replace('$.resp.headers.', ''),
          value: '',
        }
      }
      return {
        type: 'statusCode',
        operation: operation,
        value: '',
      }
    }
  }
  return undefined
}

function getTextOperation(operation) {
  if (operation === '==') {
    return 'equals'
  } else if (operation === '!=') {
    return 'notEquals'
  } else if (operation === '>') {
    return 'greaterThan'
  } else if (operation === '<') {
    return 'lessThan'
  } else if (operation === '>=') {
    return 'greaterThanOrEqual'
  } else if (operation === '<=') {
    return 'lessThanOrEqual'
  }
}

function getOperationFromText(operation) {
  if (operation === 'equals') {
    return '=='
  } else if (operation === 'notEquals') {
    return '!='
  } else if (operation === 'greaterThan') {
    return '>'
  } else if (operation === 'lessThan') {
    return '<'
  } else if (operation === 'greaterThanOrEqual') {
    return '>='
  } else if (operation === 'lessThanOrEqual') {
    return '<='
  }
  return '=='
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
        [step._method || 'GET']: step._url || '',
        headers: step.headers || {},
        exports: step.exports || {},
        asserts: assertions || [],
        params: step.params || {},
      }
      if (!step._requestType) {
        step._requestType = 'application/json'
      }
      if (step._requestType === 'application/json') {
        testkitStep.json = step._json
      } else if (step._requestType === 'application/x-www-form-urlencoded') {
        testkitStep.requestBody = step._requestBody || {}
      }
      if (step.followRedirects) {
        testkitStep.followRedirects = true
      }
      if (step.ignoreSSLErrors) {
        testkitStep.ignoreSSLErrors = true
      }
      if (step.timeout && step.timeout !== 60) {
        testkitStep.timeout = step.timeout
      }
      if (step.httpVersion && step.httpVersion !== 'http2-http1') {
        testkitStep.httpVersion = step.httpVersion
      }
      testkitSteps.push(testkitStep)
    })
  }
  return testkitSteps
}

function convertToTestkitAssertion(assertion) {
  let jsonpath = ''
  let operation = 'ok'

  if (assertion.type === 'header') {
    jsonpath = `$.resp.headers.${assertion.headerName}`
  } else if (assertion.type === 'body' && assertion.jsonpath) {
    jsonpath = `$.resp.json.${assertion.jsonpath.substring(2)}`
  } else if (assertion.type === 'statusCode') {
    jsonpath = `$.resp.status`
  } else if (assertion.type === 'responseTime') {
    jsonpath = `$.resp.duration_ms`
  }

  if (['equals', 'notEquals', 'greaterThan', 'lessThan', 'greaterThanOrEqual', 'lessThanOrEqual'].includes(assertion.operation === 'jsonpath' ? assertion.subOperation : assertion.operation)) {
    const evalOperation = getOperationFromText(assertion.operation === 'jsonpath' ? assertion.subOperation : assertion.operation)
    return {
      [operation]: `${jsonpath} ${evalOperation} ${assertion.value}`,
    }
  } else {
    const operation = assertion.operation
    return {
      [operation === 'jsonpath' ? assertion.subOperation : operation]: `${jsonpath}`,
    }
  }
}

function replaceVariables(expression) {
  // regex to match {{VALUE}}
  const regex = /{{(.*?)}}/g
  const matches = expression.match(regex)
  const variables = window.testVariables || []
  if (matches) {
    for (const match of matches) {
      const variable = match.replace('{{', '').replace('}}', '')
      if (variables && Array.isArray(variables)) {
        const variableValue = variables.find((v) => v.variableName === variable)
        if (variableValue) {
          expression = expression.replace(match, variableValue.variableValue)
        }
      }
    }
  }
  return expression
}
