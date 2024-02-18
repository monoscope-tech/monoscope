const { resolveRefs } = require("./parse_swagger.js").default;

it("Should resolve all refs", () => {
  const components = {
    parameters: {
      offsetParam: {
        in: "query",
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
      responses: {
        200: {
          description: "OK",
        },
      },
    },
  };
  const expectedData = {
    get: {
      summary: "Gets a list of users.",
      parameters: [
        {
          in: "query",
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
      responses: {
        200: {
          description: "OK",
        },
      },
    },
  };
  expect(resolveRefs(data, components)).toMatchObject(expectedData);
});
