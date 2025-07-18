'use strict'
function saveSwagger() {
  try {
    parsePaths()
  } catch (error) {
    const errorEvent = new CustomEvent('errorToast', {
      detail: { value: ['Something went wrong'] },
      bubbles: true,
      composed: true,
    })
    document.querySelector('body').dispatchEvent(errorEvent)
  }
}
function parsePaths() {
  if (window.diffEditor) {
    const originalValue = diffEditor.getModel().original.getValue()
    const modifiedValue = diffEditor.getModel().modified.getValue()
    const originalObject = jsyaml.load(originalValue)
    const modifiedObject = jsyaml.load(modifiedValue)
    const catOriginal = groupByFieldCategories(originalObject)
    const catModified = groupByFieldCategories(modifiedObject)
    const idTarget = document.querySelector('#save_swagger_input_id')
    const swagger_id = idTarget ? idTarget.value : ''

    const shapes = []
    const endpoints = []

    for (const [key, originalVal] of Object.entries(catOriginal)) {
      const modifiedVal = catModified[key]
      if (!modifiedVal) continue
      const operations = []
      const method = modifiedVal.method
      const url = modifiedVal.url
      let shapeChanged = false

      if (originalVal.description !== modifiedVal.description) {
        endpoints.push({
          endpointUrl: url,
          endpointMethod: method,
          endpointDescription: modifiedVal.description,
          endpointHost: getHostFromUrl(modifiedObject.servers),
        })
      }
      const requestHeadersKeyPaths = modifiedVal.requestHeadersKeyPaths.map(v => {
        return fieldMap(v, 'request_header')
      })

      const queryParamsKeyPaths = modifiedVal.queryParamsKeyPaths.map(v => {
        return fieldMap(v, 'query_param')
      })

      // request headers
      let info = getFieldsToOperate(
        originalVal.requestHeadersKeyPaths,
        modifiedVal.requestHeadersKeyPaths,
        originalVal.method,
        originalVal.url,
        'request_header',
      )
      shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
      operations.push(...info.ops)
      // end request headers

      // query params
      info = getFieldsToOperate(originalVal.queryParamsKeyPaths, modifiedVal.queryParamsKeyPaths, originalVal.method, originalVal.url, 'query_param')
      shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
      operations.push(...info.ops)
      // end query params

      // compare response bodies
      for (const [status, mdVal] of Object.entries(modifiedVal.response)) {
        let ogVal = originalVal.response[status]
        if (!ogVal) {
          ogVal = { responseBodyKeyPaths: [], responseHeadersKeyPaths: [] }
        }

        // response headers
        const responseHeadersKeyPaths = mdVal.responseHeadersKeyPaths.map(v => {
          return fieldMap(v, 'response_body')
        })

        info = getFieldsToOperate(ogVal.responseHeadersKeyPaths, mdVal.responseHeadersKeyPaths, modifiedVal.method, modifiedVal.url, 'response_header')
        shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
        operations.push(...info.ops)
        // end response headers

        const shapeSoFar = {
          opShapeChanged: shapeChanged,
          opQueryParamsKeyPaths: queryParamsKeyPaths,
          opRequestHeadersKeyPaths: requestHeadersKeyPaths,
          opResponseHeadersKeyPaths: responseHeadersKeyPaths,
          opMethod: method,
          opUrl: url,
          opStatus: status,
          opHost: getHostFromUrl(modifiedObject.servers),
          opResponseBodyKeyPaths: [],
          resDescription: mdVal.respDescription,
          reqDescription: modifiedVal.request.description,
        }
        // response body keypaths
        if (mdVal.responseBodyKeyPaths.length === 0) {
          shapeSoFar.opOperations = operations
          shapes.push(...finalizeShapeWithRequestBody(modifiedVal.request.requestBodyKeypaths, shapeSoFar))
        } else {
          mdVal.responseBodyKeyPaths.forEach((responseKeyPaths, ind) => {
            const responseBodyKeyPaths = responseKeyPaths.map(v => {
              return fieldMap(v, 'response_body')
            })

            info = getFieldsToOperate(ogVal.responseBodyKeyPaths[ind] || [], responseKeyPaths, modifiedVal.method, modifiedVal.url, 'response_body')
            shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
            operations.push(...info.ops)
            // end response body key paths
            shapeSoFar.opResponseBodyKeyPaths = responseBodyKeyPaths
            shapeSoFar.opShapeChanged = shapeChanged
            shapeSoFar.opOperations = operations
            shapes.push(...finalizeShapeWithRequestBody(modifiedVal.request.requestBodyKeypaths, shapeSoFar))
          })
        }
      }
    }

    for (const [key, val] of Object.entries(catModified)) {
      if (catOriginal[key]) continue
      // we have new endpoint

      const method = val.method
      const url = val.url
      endpoints.push({
        endpointUrl: url,
        endpointMethod: method,
        endpointDescription: val.description || '',
        endpointHost: getHostFromUrl(modifiedObject.servers),
      })

      const operations = []
      const requestHeadersKeyPaths = val.requestHeadersKeyPaths.map(v => {
        return fieldMap(v, 'request_header')
      })
      const queryParamsKeyPaths = val.queryParamsKeyPaths.map(v => {
        return fieldMap(v, 'query_param')
      })

      // Since it's a new endpoint, they operations will all be inserts
      // request headers
      let info = getFieldsToOperate([], val.requestHeadersKeyPaths, val.method, val.url, 'request_header')
      operations.push(...info.ops)
      // query params
      info = getFieldsToOperate([], val.queryParamsKeyPaths, val.method, val.url, 'query_param')
      operations.push(...info.ops)

      for (const [status, respVal] of Object.entries(val.response)) {
        const responseHeadersKeyPaths = respVal.responseHeadersKeyPaths.map(v => {
          return fieldMap(v, 'response_header')
        })

        info = getFieldsToOperate([], respVal.responseHeadersKeyPaths, method, url, 'response_header')
        operations.push(...info.ops)
        const shapeSoFar = {
          opShapeChanged: true,
          opQueryParamsKeyPaths: queryParamsKeyPaths,
          opRequestHeadersKeyPaths: requestHeadersKeyPaths,
          opResponseHeadersKeyPaths: responseHeadersKeyPaths,
          opMethod: method,
          opUrl: url,
          opHost: getHostFromUrl(modifiedObject.servers),
          opStatus: status,
          opResponseBodyKeyPaths: [],
          resDescription: respVal.description,
          reqDescription: val.request.description,
        }
        if (respVal.responseBodyKeyPaths.length === 0) {
          shapeSoFar.opOperations = operations
          shapes.push(...finalizeShapeWithRequestBody(val.request.requestBodyKeypaths, shapeSoFar))
        } else {
          respVal.responseBodyKeyPaths.forEach(resKeyPath => {
            const responseBodyKeyPaths = resKeyPath.map(v => {
              return fieldMap(v, 'response_body')
            })

            info = getFieldsToOperate([], resKeyPath, method, url, 'response_body')
            operations.push(...info.ops)
            shapeSoFar.opResponseBodyKeyPaths = responseBodyKeyPaths
            shapeSoFar.opOperations = operations
            shapes.push(...finalizeShapeWithRequestBody(val.request.requestBodyKeypaths, shapeSoFar))
          })
        }
      }
    }
    saveData(swagger_id, modifiedObject, shapes, endpoints)
  }
}

function getHostFromUrl(servers) {
  const url = servers.length > 0 ? servers[0].url : ''
  try {
    const urlObject = new URL(url)
    return urlObject.host
  } catch (error) {
    return ''
  }
}

function finalizeShapeWithRequestBody(requestBodyKeyPaths2D, shapeSoFar) {
  const shapes = []
  requestBodyKeyPaths2D.forEach(reqBodyKeyPath => {
    const requestBodyKeyPaths = reqBodyKeyPath.map(v => {
      return fieldMap(v, 'request_body')
    })
    let info = getFieldsToOperate([], reqBodyKeyPath, shapeSoFar.opMethod, shapeSoFar.opUrl, 'request_body')
    const shapeChanged = shapeSoFar.opShapeChanged ? true : info.updatesShape

    shapes.push({
      ...shapeSoFar,
      opShapeChanged: shapeChanged,
      opOperations: [...shapeSoFar.opOperations, ...info.ops],
      opRequestBodyKeyPaths: requestBodyKeyPaths,
    })
  })
  return shapes
}

function fieldMap(v, category) {
  return {
    fkKeyPath: v.keypath,
    fkType: getTypeAndFormat(v.type, v.format).type,
    fkCategory: category,
  }
}

function transFormURL(url) {
  return url.replaceAll('{', ':').replaceAll('}', '')
}

async function saveData(swaggerId, modifiedObject, shapes, endpoints) {
  const errorEvent = new CustomEvent('errorToast', {
    detail: { value: ['Something went wrong'] },
    bubbles: true,
    composed: true,
  })

  const url = window.location.pathname + '/save'
  shapes.forEach(shape => {
    shape.opOperations = shape.opOperations.map(op => {
      op.host = getHostFromUrl(modifiedObject.servers)
      return op
    })
    return shape
  })
  const data = {
    swagger_id: swaggerId,
    updated_swagger: JSON.stringify(modifiedObject),
    endpoints,
    diffsInfo: shapes.filter(shape => shape.opShapeChanged || shape.opOperations.length > 0),
  }
  try {
    document.querySelector('#save_swagger_loader').classList.remove('hidden')
    const response = await fetch(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data),
    })

    if (response.ok) {
      const modal = document.getElementById('swaggerModal')
      if (modal) {
        modal.style.display = 'none'
      }
      if (window.monacoEditor) {
        monacoEditor.setTheme('nightOwl')
      }
      setTimeout(() => {
        const event = new CustomEvent('successToast', {
          detail: { value: ['Swagger updated successfully'] },
          bubbles: true,
          composed: true,
        })
        document.querySelector('body').dispatchEvent(event)
      }, 100)
    } else {
      document.querySelector('body').dispatchEvent(errorEvent)
    }
  } catch (error) {
    document.querySelector('body').dispatchEvent(errorEvent)
  } finally {
    document.querySelector('#save_swagger_loader').classList.add('hidden')
  }
}

function getFieldsToOperate(ogPaths, mdPaths, method, url, category) {
  let ops = []
  let hasDeleted = false
  let updatesShape = false

  ogPaths.forEach(path => {
    const t = mdPaths.find(v => v.keypath === path.keypath)
    if (!t) {
      hasDeleted = true
      updatesShape = true
      ops.push({
        action: 'delete',
        keypath: path.keypath,
        description: path.description,
        category: category,
        url: url,
        method: method,
        ftype: path.type,
        format: path.format,
        examples: path.examples,
        isEnum: path.isEnum,
        isRequired: path.isRequired,
      })
    } else {
      if (keyPathModified(path, t)) {
        if (path.type !== t.type) {
          updatesShape = true
          ops.push({
            action: 'insert',
            keypath: t.keypath,
            description: t.description,
            category: category,
            url: url,
            method: method,
            ftype: path.type,
            format: t.format,
            examples: t.examples,
            isEnum: path.isEnum,
            isRequired: path.isRequired,
          })
        } else {
          ops.push({
            action: 'update',
            keypath: t.keypath,
            description: t.description,
            category: category,
            url: url,
            method: method,
            ftype: path.type,
            format: t.format,
            examples: t.examples,
            isEnum: path.isEnum,
            isRequired: path.isRequired,
          })
        }
      }
    }
  })

  if (mdPaths > ogPaths || hasDeleted) {
    mdPaths.forEach(path => {
      const t = ogPaths.find(v => v.keypath === path.keypath)
      if (!t) {
        updatesShape = true

        ops.push({
          action: 'insert',
          keypath: path.keypath,
          description: path.description,
          category: category,
          url: url,
          method: method,
          ftype: path.type,
          format: path.format,
          examples: path.examples,
          isEnum: path.isEnum,
          isRequired: path.isRequired,
        })
      }
    })
  }
  return { updatesShape, ops }
}

function keyPathModified(v1, v2) {
  if (v1.description !== v2.description || v1.type !== v2.type || v1.format !== v2.format || v1.example !== v2.example) {
    return true
  }
  return false
}

function groupByFieldCategories(swagger) {
  const paths = swagger.paths
  const components = swagger.components
  let hashMap = {}
  for (let [key, value] of Object.entries(paths)) {
    for (let [method, v] of Object.entries(value)) {
      let ob = {
        url: transFormURL(key),
        method,
        description: v.description || '',
      }
      const headersAndParams = parseHeadersAndParams(v.headers, v.parameters, components)
      ob.request = parseRequestBody(v.requestBody, components)
      ob.response = parseResponses(v.responses, components)
      ob = { ...ob, ...headersAndParams }
      hashMap[`${key}_${method}`] = ob
    }
  }
  return hashMap
}

function parseResponses(responses, components) {
  if (!responses) {
    return {
      responseBodyKeyPaths: [],
      responseHeadersKeyPaths: [],
      respDescription: '',
    }
  }
  responses = resolveRefs(responses, components)
  responses = resolveAllOf(responses)
  let ob = {}

  //key is status code
  for (const [key, value] of Object.entries(responses)) {
    if (!value.content) {
      ob[key] = {
        responseBodyKeyPaths: [],
        responseHeadersKeyPaths: [],
        respDescription: value.description || '',
      }
    } else {
      let headers = value.headers ? (value.headers.content ? value.headers.content.schema : undefined) : undefined
      if (!headers) {
        headers = value.headers
      }
      for (let [header, value] of Object.entries(headers || {})) {
        headers[header] = mapHeaders(value)
      }
      const headersKeypaths = getKeyPaths({
        properties: { ...headers },
        type: 'object',
      })
      for (const [contentType, v] of Object.entries(value.content)) {
        let schema = v.schema
        if (hasOneOfOrAnyOf(schema)) {
          const allValues = getAnyOfOrOneOfValues(schema)
          ob[key] = {
            responseBodyKeyPaths: allValues.map(val => getKeyPaths(val)),
            responseHeadersKeyPaths: headersKeypaths,
            respDescription: value.description || '',
          }
          break
        } else {
          ob[key] = {
            responseBodyKeyPaths: [getKeyPaths(schema)],
            responseHeadersKeyPaths: headersKeypaths,
            respDescription: value.description || '',
          }
        }
        break
      }
    }
  }
  return ob
}

function parseRequestBody(body, components) {
  if (!body) {
    return {
      requestBodyKeypaths: [],
      description: '',
    }
  }
  body = resolveRefs(body, components)
  body = resolveAllOf(body)

  if (body.content) {
    const content = body.content
    // _ is content type
    for (let [_, value] of Object.entries(content)) {
      if (value && value.schema) {
        if (hasOneOfOrAnyOf(value)) {
          const allValues = getAnyOfOrOneOfValues(value.schema)
          return allValues.map(val => getKeyPaths(val))
        } else {
          let schema = value.schema
          return {
            requestBodyKeypaths: [getKeyPaths(schema)],
            description: body.description || '',
          }
        }
      } else {
        return {
          requestBodyKeypaths: [],
          description: body.description || '',
        }
      }
    }
  }
  return {
    requestBodyKeypaths: [],
    description: body.description || '',
  }
}

function parseHeadersAndParams(headers, parameters, components) {
  let ob = {}
  headers = resolveRefs(headers, components)
  headers = headers?.content?.schema ? headers.content.schema : headers

  for (let [header, value] of Object.entries(headers || {})) {
    headers[header] = mapHeaders(value)
  }

  ob.requestHeadersKeyPaths = getKeyPaths({
    properties: { ...headers },
    type: 'object',
  })
  ob.queryParamsKeyPaths = []
  ob.pathParamsKeyPaths = []

  if (!parameters || !Array.isArray(parameters)) return ob
  parameters = resolveRefs(parameters, components)
  parameters = resolveAllOf(parameters)
  parameters.forEach(param => {
    const { type, format } = getTypeAndFormat(param.schema?.type, param.schema?.format)
    const v = {
      example: param.schema.example || '',
      type,
      format,
      description: param.description || '',
      keypath: '.' + param.name,
    }
    if (!param.name) return
    if (param.in === 'query') {
      ob.queryParamsKeyPaths.push(v)
    } else if (param.in === 'path') {
      ob.pathParamsKeyPaths.push(v)
    } else {
      ob.requestHeadersKeyPaths.push(v)
    }
  })
  return ob
}

function getTypeAndFormat(type, format) {
  if (type === 'boolean') {
    type = 'bool'
  }
  if (type === 'integer') {
    type = 'number'
  }

  if (type && format) {
    return { type, format }
  }

  if (!type && !format) {
    return { type: 'string', format: 'text' }
  }
  if (!type) {
    if (format.includes('int')) {
      return { type: 'number', format }
    }

    if (format.includes('bool')) {
      return { type: 'boolean', format }
    }

    return { type: 'string', format }
  }
  if (type === 'number' && !format) {
    return { type: 'number', format: 'int64' }
  }
  if (type === 'bool' && !format) {
    return { type, format: 'boolean' }
  }
  return { type: 'string', format: 'text' }
}

function resolveRefs(data, components) {
  if (Array.isArray(data)) {
    return data.map(item => resolveRefs(item, components))
  } else if (typeof data === 'object' && data !== null) {
    if ('$ref' in data) {
      const refPath = data.$ref.replace('#/components/', '')
      const refComponents = refPath.split('/')
      let referencedData = components
      for (const refComponent of refComponents) {
        referencedData = referencedData[refComponent]
      }
      return resolveRefs(referencedData, components)
    } else {
      const resolvedData = {}
      for (const key in data) {
        resolvedData[key] = resolveRefs(data[key], components)
      }

      return resolvedData
    }
  } else {
    return data
  }
}

function getKeyPaths(value) {
  if (!value) {
    return []
  }
  return getKeyPathsHelper(value, '', value.required || [])
}

function getKeyPathsHelper(value, path, requiredFields) {
  if (value.type === 'object') {
    if (!value.properties) return []
    let paths = []
    for (const [key, val] of Object.entries(value.properties)) {
      paths.push(...getKeyPathsHelper(val, `${path}.${key}`, requiredFields))
    }
    return paths
  } else if (value.type === 'array') {
    return getKeyPathsHelper({ ...value.items, description: value.description || '' }, `${path}[*]`, requiredFields)
  }
  if (path === '') {
    return []
  }
  const { type, format } = getTypeAndFormat(value.type, value.format)
  let examples = []
  let isEnum = !!value.enum
  let isRequired = !!value.required
  if (value.enum && Array.isArray(value.enum)) {
    examples = value.enum
  } else if (value.example) {
    examples = [value.example]
  }
  if (requiredFields.length > 0 && isRequired == false) {
    const keys = path.split('.')
    // requred from request body schema only supports top level properties
    // eg .name, .age etc
    if (keys.length === 2) {
      isRequired = requiredFields.includes(keys[1])
    }
  }
  return [
    {
      isEnum,
      isRequired,
      type,
      description: value.description || '',
      format,
      examples: examples,
      keypath: path,
    },
  ]
}

function resolveAllOf(data) {
  if (typeof data === 'object' && data !== null) {
    if ('allOf' in data && Array.isArray(data['allOf'])) {
      let mergedData = {}
      data.allOf.forEach(obj => {
        const resolvedObj = resolveAllOf(obj)
        if (resolvedObj.properties) {
          mergedData = {
            ...mergedData,
            properties: { ...mergedData.properties, ...resolvedObj.properties },
          }
          delete resolvedObj.properties
        }
        Object.assign(mergedData, resolvedObj)
      })
      return mergedData
    } else {
      let resolvedData = {}
      if (Array.isArray(data)) {
        resolvedData = data.map(val => resolveAllOf(val))
      } else {
        for (const key in data) {
          resolvedData[key] = resolveAllOf(data[key])
        }
      }
      return resolvedData
    }
  } else {
    return data
  }
}

function hasOneOfOrAnyOf(content) {
  if (typeof content === 'object' && content !== null) {
    if ('oneOf' in content || 'anyOf' in content) {
      return true
    } else if ('schema' in content) {
      return hasOneOfOrAnyOf(content.schema)
    } else if ('properties' in content) {
      return hasOneOfOrAnyOf(content.properties)
    } else if ('items' in content) {
      return hasOneOfOrAnyOf(content.items)
    }
  }
  return false
}

function mapHeaders(header) {
  if (!header.hasOwnProperty('content')) return header.schema ? header.schema : header
  for (let [contentType, value] of Object.entries(header.content)) {
    return value.schema
  }
}

function getAnyOfOrOneOfValues(value) {
  const properties = value.properties || {}
  const items = value.items || {}

  if (value.anyOf) {
    return value.anyOf
  }

  if (value.oneOf) {
    return value.oneOf
  }

  if (properties.anyOf) {
    return properties.anyOf.map(val => ({ type: 'object', properties: val }))
  }

  if (properties.oneOf) {
    return properties.oneOf.map(val => ({ type: 'object', properties: val }))
  }

  if (items.anyOf) {
    return items.anyOf.map(val => ({ type: 'array', items: val }))
  }

  if (items.oneOf) {
    return items.oneOf.map(val => ({ type: 'array', items: val }))
  }

  return []
}

// exports.default = {
//   resolveRefs,
//   parseHeadersAndParams,
//   parseRequestBody,
//   parseResponses,
//   resolveAllOf,
//   hasOneOfOrAnyOf,
//   getAnyOfOrOneOfValues,
// };
