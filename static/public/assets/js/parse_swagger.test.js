const { resolveRefs, parseHeadersAndParams, parseRequestBody, parseResponses } =
  require("./parse_swagger.js").default;

it("Should resolve all refs", () => {
  const components = {
    parameters: {
      offsetParam: {
        in: "query",
        name: "offset",
        schema: {
          type: "integer",
          minimum: 0,
        },
        description:
          "The number of items to skip before starting to collect the result set.",
      },
      limitParam: {
        in: "query",
        name: "limit",
        required: false,
        schema: {
          type: "integer",
          example: 20,
          default: 20,
        },
      },
    },
  };

  const data = {
    get: {
      summary: "Gets a list of users.",
      parameters: [
        {
          $ref: "#/components/parameters/offsetParam",
        },
        {
          $ref: "#/components/parameters/limitParam",
        },
      ],
    },
  };
  const expectedData = {
    get: {
      summary: "Gets a list of users.",
      parameters: [
        {
          in: "query",
          name: "offset",
          schema: {
            type: "integer",
            minimum: 0,
          },
          description:
            "The number of items to skip before starting to collect the result set.",
        },
        {
          in: "query",
          name: "limit",
          required: false,
          schema: {
            type: "integer",
            default: 20,
          },
        },
      ],
    },
  };

  const expectedQueryParamsData = [
    {
      keypath: ".offset",
      type: "number",
      format: "int64",
      example: "",
      description:
        "The number of items to skip before starting to collect the result set.",
    },
    {
      keypath: ".limit",
      type: "number",
      example: 20,
      format: "int64",
      description: "",
    },
  ];
  expect(resolveRefs(data, components)).toMatchObject(expectedData);
  expect(
    parseHeadersAndParams({}, data.get.parameters, components)
      .queryParamsKeyPaths
  ).toMatchObject(expectedQueryParamsData);
});

it("should recursively resolve all refs", () => {
  const data = {
    paths: {
      "/users": {
        get: {
          summary: "Retrieves a list of users.",
          responses: {
            200: {
              description: "A list of users.",
              content: {
                "application/json": {
                  schema: {
                    $ref: "#/components/schemas/UserList",
                  },
                },
              },
            },
          },
        },
      },
    },
  };
  const components = {
    schemas: {
      UserList: {
        type: "object",
        properties: {
          users: {
            type: "array",
            items: {
              $ref: "#/components/schemas/User",
            },
          },
        },
      },
      User: {
        name: { type: "string", format: "text", example: "Jon Doe" },
        age: {
          type: "number",
          format: "int64",
          example: 25,
          description: "the users age",
        },
      },
    },
  };
  const expectedData = {
    paths: {
      "/users": {
        get: {
          summary: "Retrieves a list of users.",
          responses: {
            200: {
              description: "A list of users.",
              content: {
                "application/json": {
                  schema: {
                    type: "object",
                    properties: {
                      users: {
                        type: "array",
                        items: {
                          name: {
                            type: "string",
                            format: "text",
                            example: "Jon Doe",
                          },
                          age: {
                            type: "number",
                            format: "int64",
                            example: 25,
                            description: "the users age",
                          },
                        },
                      },
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
  };
  expect(resolveRefs(data, components)).toMatchObject(expectedData);
});

it("should parse requestBodies", () => {
  const data = {
    content: {
      "application/json": {
        schema: {
          $ref: "#/components/schemas/Cat",
        },
      },
    },
  };
  const component = {
    schemas: {
      Cat: {
        type: "object",
        properties: {
          hunts: {
            type: "boolean",
          },
          age: {
            type: "integer",
          },
          tags: {
            type: "array",
            items: {
              type: "object",
              properties: {
                breed: {
                  type: "string",
                  example: "bob cat",
                  description: "what?",
                  format: "bandicut",
                },
                category: {
                  type: "string",
                  example: "big cats",
                },
              },
            },
          },
        },
      },
    },
  };
  const expectedData = [
    {
      type: "bool",
      description: "",
      format: "boolean",
      example: "",
      keypath: ".hunts",
    },
    {
      type: "number",
      format: "int64",
      description: "",
      example: "",
      keypath: ".age",
    },
    {
      type: "string",
      format: "bandicut",
      description: "what?",
      example: "bob cat",
      keypath: ".tags[*].breed",
    },
    {
      type: "string",
      format: "text",
      description: "",
      keypath: ".tags[*].category",
    },
  ];
  expect(parseRequestBody(data, component)).toMatchObject(expectedData);
});

it("should parse responses", () => {
  const response = {
    200: {
      headers: {
        "Access-Control-Allow-Headers": {
          content: {
            "text/plain": {
              schema: {
                type: "string",
                example: "X-Requested-With,content-type,authorization",
              },
              example: "X-Requested-With,content-type,authorization",
            },
          },
        },

        "Content-Length": {
          content: {
            "text/plain": {
              schema: {
                type: "string",
                example: "11083",
              },
              example: "11083",
            },
          },
        },
      },
      content: {
        "application/json": {
          schema: {
            $ref: "#/components/schemas/GetAuthbyid",
          },
        },
      },
      description: "OK",
    },
  };
  const component = {
    schemas: {
      GetAuthbyid: {
        type: "object",
        properties: {
          status: {
            type: "string",
          },
          data: {
            $ref: "#/components/schemas/Data",
          },
        },
      },
      Data: {
        title: "Data",
        type: "object",
        properties: {
          pagination: {
            $ref: "#/components/schemas/Pagination",
          },
          auths: {
            description: "",
            type: "array",
            items: {
              $ref: "#/components/schemas/Auth",
            },
          },
        },
      },
      Pagination: {
        type: "object",
        properties: {
          limit: {
            format: "int32",
            type: "integer",
          },
        },
      },
      Auth: {
        type: "object",
        properties: {
          validated: {
            type: "boolean",
          },
          id: {
            type: "string",
          },
        },
      },
    },
  };
  // {
  //   type,
  //   description: value.description || "",
  //   format,
  //   example: value.example || "",
  //   keypath: path,
  // },

  expectedData = {
    responseBodyKeyPaths: [
      {
        keypath: ".status",
        type: "string",
        format: "text",
        example: "",
        description: "",
      },
      {
        keypath: ".data.pagination.limit",
        type: "number",
        format: "int32",
        example: "",
        description: "",
      },
      {
        keypath: ".data.auths[*].validated",
        type: "bool",
        format: "boolean",
        example: "",
        description: "",
      },
      {
        keypath: ".data.auths[*].id",
        type: "string",
        format: "text",
        example: "",
        description: "",
      },
    ],
    responseHeadersKeyPaths: [
      {
        keypath: ".Access-Control-Allow-Headers",
        type: "string",
        format: "text",
        example: "X-Requested-With,content-type,authorization",
        description: "",
      },
      {
        keypath: ".Content-Length",
        type: "string",
        format: "text",
        example: "11083",
        description: "",
      },
    ],
  };
  expect(parseResponses(response, component)).toMatchObject({
    200: expectedData,
  });
});
