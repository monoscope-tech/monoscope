const { resolveRefs, parseHeadersAndParams } =
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
