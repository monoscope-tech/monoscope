class SwaggerEndPointsUI {
    constructor(json) {
        this.json = json;
        this.paths = {};
        this.schemas = {};
    }

    parsePaths() {
        if (this.json.paths) {
            const paths = this.json.paths;
            for (const path in paths) {
                const startWord = path.split('/')[1];
                if (startWord) {
                    if (!this.paths[startWord]) {
                        this.paths[startWord] = [];
                    }
                    const methods = paths[path];
                    for (const method in methods) {
                        this.paths[startWord].push({ path: path, method: method });
                    }
                }
            }
        }
    }

    parseSchemas() {
        if (this.json.components && this.json.components.schemas) {
            this.schemas = this.json.components.schemas;
        }
    }

    elt(type, props, ...children) {
        let dom = document.createElement(type);
        if (props) {
            for (let prop in props) {
                if (prop === 'class') {
                    dom.className = props[prop];
                } else if (prop.startsWith('on') && typeof props[prop] === 'function') {
                    const eventName = prop.substring(2).toLowerCase();
                    dom.addEventListener(eventName, props[prop]);
                } else {
                    dom.setAttribute(prop, props[prop]);
                }
            }
        }
        for (let child of children) {
            if (typeof child != "string") dom.appendChild(child);
            else dom.appendChild(document.createTextNode(child));
        }
        return dom;
    }

    //operations-tag-{val}

    renderPathsUI() {
        const container = document.getElementById("endpoint_paths_container");
        for (const key in this.paths) {
            const pathsArray = this.paths[key];
            const headerContainer = this.elt(
                "div",
                {
                    class: "subpaths-header",
                    onclick: (event) => { event.target.parentNode.classList.toggle("endpoint-path-collapse") }
                },
                this.elt("h4", { style: "font-size:16px; text-transform: capitalize;" }, key),
                this.elt("img", { class: "collpase-icon", src: "/assets/svgs/up_chevron.svg" })
            )

            const article = this.elt("article", {}, headerContainer)
            const pathsContainer = this.elt("div", { class: "subpaths-container" })
            for (const pathObj of pathsArray) {
                const method = this.elt("div", { class: `path-method-${pathObj.method} text-sm font-bold`, style: "text-transform: uppercase; width: 70px" }, pathObj.method)
                const endpoint = this.elt("div", { class: `flex gap-4 items-center` }, method, this.elt("p", {}, pathObj.path))
                pathsContainer.appendChild(endpoint);
            }
            article.appendChild(pathsContainer)
            container.appendChild(article);
        }
    }

    renderUI() {
        // Implement your UI rendering logic here
        console.log("Rendering UI...");
        this.renderPathsUI()
        console.log("Schemas:", this.schemas);
        // Update the UI based on the parsed paths and schemas
    }

    initialize() {
        this.parsePaths();
        this.parseSchemas();
        this.renderUI();
    }
}