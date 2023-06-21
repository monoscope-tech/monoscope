"use strict"
function parsePaths() {
    if (window.diffEditor) {
        const originalValue = diffEditor.getModel().original.getValue();
        const modifiedValue = diffEditor.getModel().modified.getValue();
        const originalObject = jsyaml.load(originalValue);
        const modifiedObject = jsyaml.load(modifiedValue)
        const catOriginal = groupByFieldCategories(originalObject.paths)
        const catModified = groupByFieldCategories(modifiedObject.paths)
        const idTarget = document.querySelector("#save_swagger_input_id")
        const swagger_id = idTarget ? idTarget.value : ""

        let updateDBOb = {
            endpoints: [],
            fields: [],
        }
        const shapes = []

        console.log(catModified)

        for (const [key, originalVal] of Object.entries(catOriginal)) {
            const modifiedVal = catModified[key]
            if (!modifiedVal) continue

            const operations = []
            const requestBodyKeyPaths = modifiedVal.requestBodyKeyPaths.map(v => {
                return { fkKeyPath: v.keypath, fkType: v.type, fkCategory: "request_body" }
            })

            const requestHeadersKeyPaths = modifiedVal.requestHeadersKeyPaths.map(v => {
                return { fkKeyPath: v.keypath, fkType: v.type, fkCategory: "request_header" }
            })

            const queryParamsKeyPaths = modifiedVal.queryParamsKeyPaths.map(v => {
                return { fkKeyPath: v.keypath, fkType: v.type, fkCategory: "query_param" }
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
            for (const [status, ogVal] of Object.entries(originalVal.response)) {
                const mdVal = modifiedVal.response[status]
                if (!mdVal) continue
                const responseBodyKeyPaths = mdVal.responseBodyKeyPaths.map(v => {
                    return { fkKeyPath: v.keypath, fkType: v.type, fkCategory: "response_body" }
                })
                const responseHeadersKeyPaths = mdVal.responseHeadersKeyPaths.map(v => {
                    return { fkKeyPath: v.keypath, fkType: v.type, fkCategory: "response_header" }
                })

                info = getFieldsToOperate(ogVal.responseBodyKeyPaths, mdVal.responseBodyKeyPaths, originalVal.method, originalVal.url, "response_body")
                shapeChanged = shapeChanged ? shapeChanged : info.updatesShape
                operations.push(...info.ops)

                info = getFieldsToOperate(ogVal.responseHeadersKeyPaths, mdVal.responseHeadersKeyPaths, originalVal.method, originalVal.url, "response_header")
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

        saveData(swagger_id, modifiedObject, shapes)
    }
}


async function saveData(swaggerId, modifiedObject, shapes) {
    const url = window.location.pathname + '/save';
    const data = {
        swagger_id: swaggerId,
        updated_swagger: JSON.stringify(modifiedObject),
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
            console.log('Data sent successfully to the backend.');
        } else {
            console.error('Error sending data to the backend:', response);
        }
    } catch (error) {
        console.error('Error sending data to the backend:', error);
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
                url: url, method: method, ftype: path.type === "boolean" ? "bool" : path.type, format: path.format, example: path.example
            })
        } else {
            if (keyPathModified(path, t)) {
                if (path.type !== t.type) {
                    updatesShape = true
                    ops.push({
                        action: "insert", keypath: t.keypath, description: t.description, category: category, url: url,
                        method: method, ftype: path.type === "boolean" ? "bool" : path.type, format: t.format, example: t.example,
                    })
                } else {
                    ops.push({
                        action: "update", keypath: t.keypath, description: t.description, category: category, url: url,
                        method: method, ftype: path.type === "boolean" ? "bool" : path.type, format: t.format, example: t.example,
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
                    url: url, method: method, ftype: path.type === "boolean" ? "bool" : path.type, format: path.format, example: path.example
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

function groupByFieldCategories(paths) {
    let hash = {}
    for (let [key, value] of Object.entries(paths)) {
        for (let [method, v] of Object.entries(value)) {
            let ob = { url: key, method }
            const headersAndParams = parseHeadersAndParams(v.headers, v.parameters)
            ob.requestBodyKeyPaths = parseRequestBody(v.requestBody)
            ob.response = parseResponses(v.responses)
            ob = { ...ob, ...headersAndParams }
            hash[`${key}_${method}`] = ob
        }
    }
    return hash
}

function parseResponses(responses) {
    if (!responses) {
        return undefined
    }
    let ob = {}
    for (const [key, value] of Object.entries(responses)) {
        for (const [k, v] of Object.entries(value.content)) {
            ob[key] = {
                responseBodyKeyPaths: getKeyPaths(v.schema),
                responseHeadersKeyPaths: getKeyPaths(value.headers.content.schema)
            }
            break;
        }
    }
    return ob
}

function parseRequestBody(body) {
    if (!body || !body.content) {
        return []
    }
    const content = body.content
    for (let [_, value] of Object.entries(content)) {
        if (value && value.schema) {
            return getKeyPaths(value.schema)
        } else {
            return []
        }
    }
}

function parseHeadersAndParams(headers, parameters) {
    let ob = {}
    ob.requestHeadersKeyPaths = getKeyPaths(headers?.content?.schema || undefined)
    ob.queryParamsKeyPaths = []
    ob.pathParamsKeyPaths = []

    if (!parameters || !Array.isArray(parameters)) return ob

    parameters.forEach(param => {
        const v = param.schema
        v.description = param.description
        const key = param.name + ".[]"
        v.keypath = key
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

function getKeyPaths(value) {
    if (!value) {
        return []
    }
    return getKeyPathsHelper(value, "")
}

function getKeyPathsHelper(value, path) {
    if (value.type === "object") {
        let paths = []
        for (const [key, val] of Object.entries(value.properties)) {
            paths.push(...getKeyPathsHelper(val, `${path}.${key}`))
        }
        return paths
    } else if (value.type === "array") {
        return getKeyPathsHelper({ ...value.items, description: value.description || "" }, `${path}[*]`)
    }
    return [{ ...value, keypath: path }]
}