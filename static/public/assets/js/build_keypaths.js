"use strict"
function parsePaths() {
    if (window.diffEditor) {
        const originalValue = diffEditor.getModel().original.getValue();
        const modifiedValue = diffEditor.getModel().modified.getValue();
        const originalObject = jsyaml.load(originalValue);
        const modifiedObject = jsyaml.load(modifiedValue)
        const catOriginal = groupByFieldCategories(originalObject)
        const catModified = groupByFieldCategories(modifiedObject)
        const idTarget = document.querySelector("#save_swagger_input_id")
        const swagger_id = idTarget ? idTarget.value : ""

        const shapes = []
        const endpoints = []

        for (const [key, originalVal] of Object.entries(catOriginal)) {
            const modifiedVal = catModified[key]
            if (!modifiedVal) continue

            const operations = []
            const requestBodyKeyPaths = modifiedVal.requestBodyKeyPaths.map(v => {
                return fieldMap(v, "request_body")
            })

            const requestHeadersKeyPaths = modifiedVal.requestHeadersKeyPaths.map(v => {
                return fieldMap(v, "request_header")
            })

            const queryParamsKeyPaths = modifiedVal.queryParamsKeyPaths.map(v => {
                return fieldMap(v, "query_param")
            })

            const method = modifiedVal.method
            const url = modifiedVal.url
            let shapeChanged = false
            // compare request body
            let info = getFieldsToOperate(originalVal.requestBodyKeyPaths, modifiedVal.requestBodyKeyPaths, originalVal.method, originalVal.url, "request_body")
            shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
            operations.push(...info.ops)
            // request headers
            info = getFieldsToOperate(originalVal.requestHeadersKeyPaths, modifiedVal.requestHeadersKeyPaths, originalVal.method, originalVal.url, "request_header")
            shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
            operations.push(...info.ops)
            // query params
            info = getFieldsToOperate(originalVal.queryParamsKeyPaths, modifiedVal.queryParamsKeyPaths, originalVal.method, originalVal.url, "query_param")
            shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
            operations.push(...info.ops)

            // compare response bodies
            for (const [status, mdVal] of Object.entries(modifiedVal.response)) {
                let ogVal = originalVal.response[status]
                if (!ogVal) {
                    ogVal = { responseBodyKeyPaths: [], responseHeadersKeyPaths: [] }
                }
                const responseBodyKeyPaths = mdVal.responseBodyKeyPaths.map(v => {
                    return fieldMap(v, "response_body")
                })
                const responseHeadersKeyPaths = mdVal.responseHeadersKeyPaths.map(v => {
                    return fieldMap(v, "response_header")
                })

                info = getFieldsToOperate(ogVal.responseBodyKeyPaths, mdVal.responseBodyKeyPaths, modifiedVal.method, modifiedVal.url, "response_body")
                shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
                operations.push(...info.ops)

                info = getFieldsToOperate(ogVal.responseHeadersKeyPaths, mdVal.responseHeadersKeyPaths, modifiedVal.method, modifiedVal.url, "response_header")
                shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
                operations.push(...info.ops)
                shapes.push({
                    opShapeChanged: shapeChanged,
                    opRequestBodyKeyPaths: requestBodyKeyPaths,
                    opQueryParamsKeyPaths: queryParamsKeyPaths,
                    opRequestHeadersKeyPaths: requestHeadersKeyPaths,
                    opResponseBodyKeyPaths: responseBodyKeyPaths,
                    opResponseHeadersKeyPaths: responseHeadersKeyPaths,
                    opMethod: method,
                    opUrl: url,
                    opStatus: status,
                    opOperations: operations
                })
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
                endpointHost: modifiedObject.servers.length > 0 ? modifiedObject.servers[0].url : "example.com"
            })

            const operations = []
            const requestBodyKeyPaths = val.requestBodyKeyPaths.map(v => {
                return fieldMap(v, "request_body")
            })
            const requestHeadersKeyPaths = val.requestHeadersKeyPaths.map(v => {
                return fieldMap(v, "request_header")
            })
            const queryParamsKeyPaths = val.queryParamsKeyPaths.map(v => {
                return fieldMap(v, "query_param")
            })
            // Since it's a new endpoint, they operations will all be inserts
            let info = getFieldsToOperate([], val.requestBodyKeyPaths, val.method, val.url, "request_body")
            operations.push(...info.ops)
            // request headers
            info = getFieldsToOperate([], val.requestHeadersKeyPaths, val.method, val.url, "request_header")
            operations.push(...info.ops)
            // query params
            info = getFieldsToOperate([], val.queryParamsKeyPaths, val.method, val.url, "query_param")
            operations.push(...info.ops)

            for (const [status, respVal] of Object.entries(val.response)) {
                const responseBodyKeyPaths = respVal.responseBodyKeyPaths.map(v => {
                    return fieldMap(v, "response_body")
                })
                const responseHeadersKeyPaths = respVal.responseHeadersKeyPaths.map(v => {
                    return fieldMap(v, "response_header")
                })

                info = getFieldsToOperate([], respVal.responseBodyKeyPaths, method, url, "response_body")
                operations.push(...info.ops)

                info = getFieldsToOperate([], respVal.responseHeadersKeyPaths, method, url, "response_header")
                operations.push(...info.ops)

                shapes.push({
                    opShapeChanged: true,
                    opRequestBodyKeyPaths: requestBodyKeyPaths,
                    opQueryParamsKeyPaths: queryParamsKeyPaths,
                    opRequestHeadersKeyPaths: requestHeadersKeyPaths,
                    opResponseBodyKeyPaths: responseBodyKeyPaths,
                    opResponseHeadersKeyPaths: responseHeadersKeyPaths,
                    opMethod: method,
                    opUrl: url,
                    opStatus: status,
                    opOperations: operations
                })
            }
        }
        saveData(swagger_id, modifiedObject, shapes, endpoints)
    }
}

function fieldMap(v, category) {
    return {
        fkKeyPath: v.keypath,
        fkType: v.type || "text",
        fkCategory: category
    }
}

function transFormURL(url) {
    return url.replaceAll("{", ":").replaceAll("}", "")
}

async function saveData(swaggerId, modifiedObject, shapes, endpoints) {
    const url = window.location.pathname + '/save';
    const data = {
        swagger_id: swaggerId,
        updated_swagger: JSON.stringify(modifiedObject),
        endpoints,
        diffsInfo: shapes.filter(shape => shape.opShapeChanged || shape.opOperations.length > 0)
    };
    try {
        const response = await fetch(url, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(data)
        });

        if (response.ok) {
            const modal = document.getElementById('swaggerModal')
            if (modal) {
                modal.style.display = 'none';
            }
            if (window.monacoEditor) {
                monacoEditor.setTheme('nightOwl')
            }
            setTimeout(() => {
                alert("Swagger updated successfully")
            }, 100);
        } else {
            alert("Something went wrong")
        }
    } catch (error) {
        alert("Something went wrong")
    }
}

function getFieldsToOperate(ogPaths, mdPaths, method, url, category) {
    let ops = []
    let hasDeleted = false
    let updatesShape = false

    ogPaths.forEach(path => {
        const t = mdPaths.find((v) => v.keypath === path.keypath)
        if (!t) {
            hasDeleted = true
            updatesShape = true
            ops.push({
                action: "delete", keypath: path.keypath, description: path.description, category: category,
                url: url, method: method, ftype: path.type, format: path.format, example: path.example
            })
        } else {
            if (keyPathModified(path, t)) {
                if (path.type !== t.type) {
                    updatesShape = true
                    ops.push({
                        action: "insert", keypath: t.keypath, description: t.description, category: category, url: url,
                        method: method, ftype: path.type, format: t.format, example: t.example,
                    })
                } else {
                    ops.push({
                        action: "update", keypath: t.keypath, description: t.description, category: category, url: url,
                        method: method, ftype: path.type, format: t.format, example: t.example,
                    })
                }
            }
        }
    })

    if (mdPaths > ogPaths || hasDeleted) {
        mdPaths.forEach(path => {
            const t = ogPaths.find((v) => v.keypath === path.keypath)
            if (!t) {
                updatesShape = true
                ops.push({
                    action: "insert", keypath: path.keypath, description: path.description, category: category,
                    url: url, method: method, ftype: path.type, format: path.format, example: path.example
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
    let hash = {}
    for (let [key, value] of Object.entries(paths)) {
        for (let [method, v] of Object.entries(value)) {
            let ob = { url: transFormURL(key), method }
            const headersAndParams = parseHeadersAndParams(v.headers, v.parameters, components)
            ob.requestBodyKeyPaths = parseRequestBody(v.requestBody, components)
            ob.response = parseResponses(v.responses, components)
            ob = { ...ob, ...headersAndParams }
            hash[`${key}_${method}`] = ob
        }
    }
    return hash
}

function parseResponses(responses, components) {
    if (!responses) {
        return undefined
    }
    let ob = {}
    for (const [key, value] of Object.entries(responses)) {
        if (!value.content) continue
        for (const [contentType, v] of Object.entries(value.content)) {
            let headers = value.headers ? value.headers.content ? value.headers.content.schema : {} : {}
            let schema = v.schema
            if (v.schema["$ref"]) {
                schema = getComponent(v.schema["$ref"], components)
            } else if (v.schema.type === "array" && v.schema.items["$ref"]) {
                schema = { items: getComponent(v.schema.items["$ref"], components), type: "array", description: v.schema.description }
            }
            ob[key] = {
                responseBodyKeyPaths: getKeyPaths(schema),
                responseHeadersKeyPaths: getKeyPaths(headers)
            }
            break;
        }
    }
    return ob
}

function parseRequestBody(body, components) {
    if (!body) {
        return []
    }
    if (body["$ref"]) {
        const ref = body["$ref"]
        const refValue = getComponent(ref, components)
        if (!refValue) {
            return []
        } else {
            return getKeyPaths(refValue)
        }
    }
    if (body.content) {
        const content = body.content
        for (let [_, value] of Object.entries(content)) {
            if (value && value.schema) {
                let schema = value.schema
                if (schema["$ref"]) {
                    schema = getComponent(schema["$ref"], components)
                } else if (schema.type === "array" && v.schema.items["$ref"]) {
                    schema = { items: getComponent(v.schema.items["$ref"], components), type: "array", description: schema.description }
                }
                return getKeyPaths(schema)
            } else {
                return []
            }
        }
    }
    return []
}

function getComponent(ref, components) {
    const path = ref.split("/").slice(2)
    let value = components
    for (const segment of path) {
        if (value && value.hasOwnProperty(segment)) {
            value = value[segment];
        } else {
            value = undefined;
            break;
        }
    }
    return value;
}

function parseHeadersAndParams(headers, parameters, components) {
    let ob = {}
    ob.requestHeadersKeyPaths = getKeyPaths(headers?.content?.schema || { properties: { ...headers }, type: "object" })
    ob.queryParamsKeyPaths = []
    ob.pathParamsKeyPaths = []

    if (!parameters || !Array.isArray(parameters)) return ob

    parameters.forEach(param => {
        const { type, format } = getTypeAndFormat(param.schema?.type, param.schema?.format)
        const v = { example: param.schema.example || "", type, format, description: param.description || "", keypath: "." + param.name }
        if (!param.name) return
        if (param.in === "query") {
            ob.queryParamsKeyPaths.push(v)
        } else if (param.in === "path") {
            ob.pathParamsKeyPaths.push(v)
        } else {
            ob.requestHeadersKeyPaths.push(v)
        }
    })
    return ob
}

function getTypeAndFormat(type, format) {
    if (type && format) {
        return { type, format };
    }
    if (type === "boolean") {
        type = "bool";
    }
    if (!type && !format) {
        return { type: "string", format: "text" };
    }
    if (!type) {
        if (format.includes("int")) {
            return { type: "number", format };
        }

        if (format.includes("bool")) {
            return { type: "boolean", format };
        }

        return { type: "string", format };
    }
    if (type.includes("int")) {
        return { type: "number", format: "int64" };
    }
    if (type === "bool") {
        return { type, format: "boolean" };
    }
    return { type: "string", format: "text" };
}


function getKeyPaths(value) {
    if (!value) {
        return []
    }
    return getKeyPathsHelper(value, "")
}

function getKeyPathsHelper(value, path) {
    if (value.type === "object") {
        if (!value.properties) return []
        let paths = []
        for (const [key, val] of Object.entries(value.properties)) {
            paths.push(...getKeyPathsHelper(val, `${path}.${key}`))
        }
        return paths
    } else if (value.type === "array") {
        return getKeyPathsHelper({ ...value.items, description: value.description || "" }, `${path}[*]`)
    }
    if (path === "") {
        return []
    }
    const { type, format } = getTypeAndFormat(value.type, value.format)
    return [{ type, description: value.description || "", format, example: value.example || "", keypath: path }]
}