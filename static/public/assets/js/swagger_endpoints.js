"use strict";

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
        const startWord = path.split("/")[1];
        if (startWord) {
          if (!this.paths[startWord]) {
            this.paths[startWord] = [];
          }
          const methods = paths[path];
          for (const method in methods) {
            let opId = methods[method].operationId;
            let modifiedPath = path.replaceAll(/[^a-zA-Z0-9]/g, "_");
            let targetId = opId
              ? `operations-${startWord}-${opId}`
              : `operations-default-${method}${modifiedPath}`;
            this.paths[startWord].push({
              path: path,
              method: method,
              operationId: targetId,
            });
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

  searchListener() {
    const searchBar = document.getElementById("endpoints-search");
    searchBar.oninput = (event) => {
      const searchValue = event.target.value;
      if (searchValue) {
        const searchPaths = {};
        const searchSchemas = {};
        for (const key in this.paths) {
          for (const path of this.paths[key]) {
            if (
              path.path.toLowerCase().includes(searchValue.toLowerCase()) ||
              path.method
                .toLocaleLowerCase()
                .includes(searchValue.toLocaleLowerCase())
            ) {
              if (!searchPaths[key]) {
                searchPaths[key] = [];
              }
              searchPaths[key].push(path);
            }
          }
        }
        for (const key in this.schemas) {
          if (
            key.toLocaleLowerCase().includes(searchValue.toLocaleLowerCase())
          ) {
            searchSchemas[key] = "";
          }
        }
        this.renderUI(searchPaths, searchSchemas);
      } else {
        this.renderUI(this.paths, this.schemas);
      }
    };
  }

  elt(type, props, ...children) {
    let dom = document.createElement(type);
    if (props) {
      for (let prop in props) {
        if (prop === "class") {
          dom.className = props[prop];
        } else if (prop.startsWith("on") && typeof props[prop] === "function") {
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

  renderPathsUI(paths) {
    const container = document.getElementById("endpoint_paths_container");
    for (const key in paths) {
      const pathsArray = paths[key];
      const headerContainer = this.elt(
        "div",
        {
          class: "subpaths-header",
          onclick: (event) => {
            event.target.parentNode.classList.toggle("endpoint-path-collapse");
            const targetHeader = document.getElementById(
              `operations-tag-${key}`
            );
            if (targetHeader) {
              const parentNode = targetHeader.parentNode;
              parentNode.classList.add("is-open");
              targetHeader.setAttribute("data-is-open", "true");
              targetHeader.scrollIntoView({
                behavior: "smooth",
                block: "nearest",
                inline: "nearest",
              });
            }
          },
        },
        this.elt(
          "h4",
          {
            style:
              "font-size: 14px; font-weight: 600; color: rgb(75 85 99); text-transform: capitalize;",
          },
          key
        ),
        this.elt("img", {
          class: "collpase-icon",
          src: "/assets/svgs/up_chevron.svg",
        })
      );

      const article = this.elt("article", {}, headerContainer);
      const pathsContainer = this.elt("div", { class: "subpaths-container" });
      for (const pathObj of pathsArray) {
        const method = this.elt(
          "div",
          {
            class: `text-sm font-bold`,
            style: "text-transform: uppercase; width: 50px",
          },
          pathObj.method
        );
        const endpoint = this.elt(
          "div",
          {
            class: `path-method-${pathObj.method} flex gap-4 items-center`,
            onclick: (event) => {
              event.stopPropagation();
              for (const child of Array.from(
                document.querySelectorAll('[class^="path-method-"]')
              )) {
                child.classList.remove("endpoint_active");
              }
              event.currentTarget.classList.add("endpoint_active");
              const target = document.getElementById(pathObj.operationId);
              if (target) {
                if (
                  target.firstElementChild &&
                  target.firstElementChild.firstElementChild
                ) {
                  target.firstElementChild.firstElementChild.click();
                }
                target.scrollIntoView({
                  behavior: "smooth",
                  block: "nearest",
                  inline: "nearest",
                });
              }
              const model = window.editor.getModel();
              const startPos = model.findNextMatch(
                pathObj.path + ":",
                0,
                false,
                false,
                "",
                false
              );
              const position = model.findNextMatch(
                pathObj.method + ":",
                startPos.range.getStartPosition(),
                false,
                false,
                "",
                false
              );
              if (position) {
                editor.revealPositionInCenter(
                  position.range.getStartPosition()
                );
                editor.setPosition(position.range.getStartPosition());
              }
            },
          },
          method,
          this.elt("p", { class: "text-gray-700" }, pathObj.path)
        );
        pathsContainer.appendChild(endpoint);
      }
      article.appendChild(pathsContainer);
      container.appendChild(article);
    }
  }

  renderSchemaUI(schemas) {
    const container = document.getElementById("endpoint_paths_container");
    const headerContainer = this.elt(
      "div",
      {
        class: "subpaths-header",
        onclick: (event) => {
          event.target.parentNode.classList.toggle("endpoint-path-collapse");
          const targetHeader = document.getElementById(`operations-tag-${key}`);
          if (targetHeader) {
            const parentNode = targetHeader.parentNode;
            parentNode.classList.add("is-open");
            targetHeader.setAttribute("data-is-open", "true");
            targetHeader.scrollIntoView({
              behavior: "smooth",
              block: "nearest",
              inline: "nearest",
            });
          }
        },
      },
      this.elt(
        "h4",
        {
          style:
            "font-size: 16px; font-weight: bold; text-transform: capitalize;",
        },
        "Schemas"
      ),
      this.elt("img", {
        class: "collpase-icon",
        src: "/assets/svgs/up_chevron.svg",
      })
    );

    const article = this.elt("div", {}, headerContainer);
    const pathsContainer = this.elt("div", { class: "subpaths-container" });

    for (const key in schemas) {
      const method = this.elt(
        "div",
        {
          class: `endpoint_schema text-sm font-bold`,
          style: "text-transform: uppercase; width: 70px",
        },
        "Schema"
      );
      const endpoint = this.elt(
        "div",
        {
          class: `flex gap-4 items-center`,
          onclick: () => {
            const target = document.getElementById(`model-${key}`);
            if (target) {
              const clickTarget = target.querySelector(".model-box");
              if (clickTarget && clickTarget.firstElementChild) {
                clickTarget.firstElementChild.click();
              }
              target.scrollIntoView({
                behavior: "smooth",
                block: "nearest",
                inline: "nearest",
              });
              const model = window.editor.getModel();
              const startPos = model.findNextMatch(
                "schemas:",
                0,
                false,
                false,
                "",
                false
              );
              const position = model.findNextMatch(
                key + ":",
                startPos.range.getStartPosition(),
                false,
                false,
                "",
                false
              );
              if (position) {
                editor.revealPositionInCenter(
                  position.range.getStartPosition()
                );
                editor.setPosition(position.range.getStartPosition());
              }
            }
          },
        },
        method,
        this.elt("p", {}, key)
      );
      pathsContainer.appendChild(endpoint);
    }
    article.appendChild(pathsContainer);
    container.appendChild(article);
  }

  infoTagsServer() {
    const container = document.querySelector("#info_tags_container");
    if (this.json.info) {
      container.appendChild(
        this.elt(
          "div",
          {
            class:
              "px-2 py-1 font-bold text-lg cursor-pointer w-full hover:bg-gray-500",
            onclick: () => {
              const infoArea = document.querySelector(".info");
              if (infoArea) {
                infoArea.scrollIntoView({
                  behavior: "smooth",
                  block: "nearest",
                  inline: "nearest",
                });
              }
            },
          },
          "Info"
        )
      );
    }

    if (this.json.servers) {
      container.appendChild(
        this.elt(
          "div",
          {
            class:
              "px-2 py-1 font-bold text-lg cursor-pointer w-full hover:bg-gray-500",
            id: "apt-servers",
            onclick: () => {
              const infoArea = document.querySelector(".scheme-container");
              infoArea.scrollIntoView({
                behavior: "smooth",
                block: "nearest",
                inline: "nearest",
              });
            },
          },
          "Servers"
        )
      );
      const servers_container = document.querySelector("#apt-servers");
      if (Array.isArray(this.json.servers)) {
        this.json.servers.forEach((server) => {
          servers_container.appendChild(
            this.elt(
              "div",
              { class: "text-sm flex items-center gap-2" },
              this.elt("span", { class: "text-gray-800" }, server.description),
              this.elt("span", { class: "text-gray-600" }, server.url)
            )
          );
        });
      }
    }
    // if (this.json.tags) {
    //   container.appendChild(
    //     this.elt(
    //       "div",
    //       {
    //         class:
    //           "px-4 py-3 font-bold text-lg cursor-pointer w-full hover:bg-gray-500",
    //         onclick: () => {
    //           const infoArea = document.querySelector(".info");
    //           infoArea.scrollIntoView({
    //             behavior: "smooth",
    //             block: "nearest",
    //             inline: "nearest",
    //           });
    //         },
    //       },
    //       "Tags"
    //     )
    //   );
    // }
  }

  renderUI(paths, schemas) {
    const container = document.getElementById("endpoint_paths_container");
    while (container.firstChild) {
      container.removeChild(container.firstChild);
    }
    const infoTagsServerContainer = document.getElementById(
      "info_tags_container"
    );
    while (infoTagsServerContainer.firstChild) {
      infoTagsServerContainer.removeChild(infoTagsServerContainer.firstChild);
    }
    this.infoTagsServer();
    this.renderPathsUI(paths);
    this.renderSchemaUI(schemas);
  }

  updateData(data) {
    try {
      JSON.stringify(data);
      this.json = data;
    } catch (error) {
      this.json = {};
    }
    this.paths = {};
    this.schemas = {};
    this.initialize();
  }

  initialize() {
    this.searchListener();
    this.parsePaths();
    this.parseSchemas();
    this.renderUI(this.paths, this.schemas);
  }
}
