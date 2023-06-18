function parsePaths() {
    if (window.diffEditor) {
        const originalValue = diffEditor.getModel().original.getValue();
        const modifiedValue = diffEditor.getModel().modified.getValue();
        const originalObject = jsyaml.load(originalValue);
        const modifiedObject = jsyaml.load(modifiedValue)
        const catOriginal = groupByFieldCategories(originalObject.paths)
        const catModified = groupByFieldCategories(modifiedObject.paths)
        console.log(catOriginal, catModified)
    }
}

function groupByFieldCategories(paths) {
    let arr = []
    for (let [key, value] of Object.entries(paths)) {
        for (let [method, v] of Object.entries(value)) {
            let ob = { url: key, method }
            // const headersAndParams = parseHeadersAndParams(v.headers, v.parameters)
            ob.requestBodyKeyPaths = parseRequestBody(v.requestBody)
            ob.response = parseResponses(v.responses)
            arr.push(ob)
        }
    }

    return arr
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
        return getKeyPathsHelper(value.items, `${path}[*]`)
    }
    let ob = {}
    ob[path] = value
    return [ob]
}

// function parseHeadersAndParams(headers, parameters) {

// }