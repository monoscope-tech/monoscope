module Pages.IntegrationDemos.Javascript (javascriptGuide, expressGuide, fastifyGuide, nextJsGuide) where

import Lucid
import NeatInterpolation (text)
import Pkg.Components
import Relude


javascriptGuide :: Text -> Html ()
javascriptGuide apikey = do
  section_ [class_ "flex flex-col gap-4 lang-guide", id_ "js_main"] do
    span_ [class_ "text-2xl font-semibold  text-textStrong"] "Integrate Javascript SDK"
    -- div_ [class_ "flex items-center gap-2"] do
    --   mapM_ featureItem features
    div_ [class_ "flex flex-col gap-2"] do
      span_ [class_ " text-textStrong font-semibold"] "Select framework"
      div_ [class_ "flex items-center gap-2"] do
        mapM_ (frameworkItem "js") frameworks
    div_ [class_ "w-full border-b"] pass
    expressGuide apikey
    fastifyGuide apikey
    nextJsGuide apikey



frameworks :: [Text]
frameworks = ["Express", "Fastify", "NextJS"]


expressGuide :: Text -> Html ()
expressGuide apikey = do
  section_ [class_ "flex flex-col gap-10 js-guide", id_ "Express"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Installation"
      p_ [class_ "text-gray-600 font-medium"] "Run the command below to install the APIToolkit express sdk and Open telemetery API, SDK, and auto instrumentation tools."
      bashCommand "npm install --save apitoolkit-express @opentelemetry/api @opentelemetry/auto-instrumentations-node"

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Open Telemetry SDK Configuration"
      p_ [class_ "text-gray-600 font-medium"] "Set the neccessary environment variables to configure the SDK"
      codeExample $ otelConfig apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize APItoolkit Express SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming http request, errors and outgoing request."
      codeExample $ initCode

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts the following optional fields to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample $ configOptions False


fastifyGuide :: Text -> Html ()
fastifyGuide apikey = do
  section_ [class_ "flex flex-col gap-10 js-guide hidden", id_ "Fastify"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Installation"
      p_ [class_ "text-gray-600 font-medium"] "Run the command below to install the APIToolkit Fastify sdk and Open telemetery API, SDK, and auto instrumentation tools."
      bashCommand "npm install --save apitoolkit-fastify @opentelemetry/api @opentelemetry/auto-instrumentations-node"

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Open Telemetry SDK Configuration"
      p_ [class_ "text-gray-600 font-medium"] "Set the neccessary environment variables to configure the SDK"
      codeExample $ otelConfig apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize APItoolkit Fastify SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming http request, errors and outgoing request."
      codeExample initCodeFastify

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts the following optional fields to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample $ configOptions True


nextJsGuide :: Text -> Html ()
nextJsGuide apikey = do
  section_ [class_ "flex flex-col gap-10 js-guide hidden", id_ "NextJS"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Installation"
      p_ [class_ "text-gray-600 font-medium"] "Run the command below to install the APIToolkit NextJs sdk and Open telemetery API, SDK, and auto instrumentation tools."
      bashCommand "npm install --save apitoolkit-next @opentelemetry/api @vercel/otel"

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Open Telemetry SDK Configuration"
      p_ [class_ "text-gray-600 font-medium"] "Set the neccessary environment variables to configure the SDK"
      codeExample $
        [text|
OTEL_EXPORTER_OTLP_ENDPOINT="http://otelcol.apitoolkit.io:4318"
OTEL_SERVICE_NAME="my-service" # Specifies the name of the service.
OTEL_RESOURCE_ATTRIBUTES="at-project-key=$apikey"
OTEL_EXPORTER_OTLP_PROTOCOL="http/protobuf"
|]

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize APItoolkit NextJs SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming http request, errors and outgoing request."
      h4_ [class_ "text-xl font-medium"] "Register the SDK"
      p_ [class_ "text-gray-600 font-medium"] do
        "Register the opentelemetry sdk in your "
        codeEmphasis "intrumentation.ts|js"
        " file"
      codeExample
        [text|
import { registerOTel } from "@vercel/otel";

export function register() {
  registerOTel("");
}      |]
      p_ [] "Wrap your handlers like so"
      codeExample
        [text|
import { withAPItoolkitAppRouter } from "apitoolkit-next";
import { NextRequest, NextResponse } from "next/server";
async function handleRequest(req: NextRequest) {
  return NextResponse.json({ message: "hello world" });
}

// Optional configuration
const config = {
  captureResponseBody: true,
  serviceName: "my-service",
}
export const GET = withAPItoolkitAppRouter(handleRequest, config);
      |]

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts the following optional fields to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample $ configOptions False


initCode :: Text
initCode =
  [text|
import * as express from "express";
import { APIToolkit } from "apitoolkit-express";
import axios from "axios";

const app = express();
const apitoolkitClient = APIToolkit.NewClient({
  serviceName: "my-service",
  monitorAxios: axios, // Optional: Use this to monitor Axios requests
});

// Add middleware for request monitoring
app.use(apitoolkitClient.middleware);

app.get("/", async (req, res) => {
  // This axios request get's monitored and appears in the  APIToolkit explorer
  const response = await axios.get(
    "https://jsonplaceholder.typicode.com/todos/1"
  );
  res.json(response.data);
});

// automatically report unhandled errors along with the request data
app.use(apitoolkitClient.errorMiddleware);

app.listen(3000, () => {
  console.log("Example app listening on port 3000!");
});
|]


otelConfig :: Text -> Text
otelConfig apiKey =
  [text|
# Specifies the endpoint URL for the OpenTelemetry collector.
export OTEL_EXPORTER_OTLP_ENDPOINT="http://otelcol.apitoolkit.io:4317"
# Specifies the name of the service.
export OTEL_SERVICE_NAME=""
# Adds your API KEY to the resource.
export OTEL_RESOURCE_ATTRIBUTES="at-project-key=$apiKey"
# Specifies the protocol to use for the OpenTelemetry exporter.
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc"

export NODE_OPTIONS="--require @opentelemetry/auto-instrumentations-node/register"
# Start your server
node server.js|]


configOptions :: Bool -> Text
configOptions isFastify =
  let fastify = if isFastify then "fastify: <YOUR FASTIFY INSTANCE>  // required" else ""
   in [text|
{
  $fastify
  // List of request and response headers to be redacted
  redactHeaders: ["Authorization"],
  // Json path list of response body fields to be redacted
  redactResponseBody: ["$.user.email", "$.user.age"],
  // Json path list of request body fields to be redacted
  redactRequestBody: ["$.password", "$.credit_card", "$.ccv"],
  // Set debug to true to help in troubleshooting issues
  debug: false,
  // Set the service name
  serviceName: "my-service",
  // Enable caputuring request body
  captureRequestBody: true,
  // Enable caputuring response body
  captureResponseBody: true,
  // Tags
  tags: ["prod", "eu"],
  // Service version
  serviceVersion: "1.0.0",
}
|]


initCodeFastify :: Text
initCodeFastify =
  [text|
import fastify from "fastify";
import { APIToolkit } from "apitoolkit-fastify";
import axios from "axios";

const fastifyServer = fastify({});
const apitoolkitClient = APIToolkit.NewClient({
  fastify: fastifyServer, // Required: The Fastify server instance
  monitorAxios: axios, // Optional: Use this to monitor Axios requests
});

apitoolkitClient.initializeHooks();

fastifyServer.get("/", async (request, reply) => {
  const response = await axios.get("https://api.github.com/users/octocat");
  return response.data;
});

fastifyServer.listen({ port: 3000 });
|]
