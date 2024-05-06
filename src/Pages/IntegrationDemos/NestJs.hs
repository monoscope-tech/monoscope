module Pages.IntegrationDemos.NestJs (nestGuide) where

import Data.Text
import Data.Text qualified as T
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


nestGuide :: Text -> Html ()
nestGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      h6_ [class_ "mt-2 text-lg font-medium"] "For ExpressJs engine"
      p_ [class_ "text-gray-600 font-medium"] "Install the APItoolkit Express SDK using npm/bun/pnpm"
      bashCommand "npm install apitoolkit-express"

      h6_ [class_ "mt-2 text-lg font-medium"] "For FastifyJs engine"
      p_ [class_ "text-gray-600 font-medium"] "Install the APItoolkit Fastify SDK using npm/bun/pnpm"
      bashCommand "npm install apitoolkit-fastify"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK (Express Platform or Default)"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your server"
      codeExample
        $ [text|
import { NestFactory } from '@nestjs/core';
import { APIToolkit } from 'apitoolkit-express';
import { AppModule } from './app.module';

async function bootstrap() {
  const apiToolkitClient = APIToolkit.NewClient({ apikey: "$apikey" });
  const app = await NestFactory.create(AppModule);
  app.use(apiToolkitClient.expressMiddleware);
  await app.listen(3000);
}

bootstrap();
|]

      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK (Fastify Platform)"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your server"
      codeExample
        $ [text|
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { FastifyAdapter, NestFastifyApplication} from '@nestjs/platform-fastify';
import fastify from 'fastify';
import APIToolkit from 'apitoolkit-fastify';

async function bootstrap() {
  const fastifyInstance = fastify();
  const app = await NestFactory.create(
    AppModule,
    new FastifyAdapter(fastifyInstance)
  );
  const apiToolkitClient = await APIToolkit.NewClient({
    apiKey: "$apikey", // required
    fastify: fastifyInstance, // required
  });
  apiToolkitClient.init();
  await app.listen(3000);
}
bootstrap();
|]

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting (Express Platform)"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample $ errorReportingCode apikey

      h3_ [class_ "text-2xl font-semibold"] "Error Reporting (Fastify Platform)"
      codeExample
        $ [text|
import APIToolkit, { ReportError } from "apitoolkit-fastify";
import Fastify from 'fastify';
const fastify = Fastify();

fastify.get('/', async (request, reply) => {
    try {
      const val  = 1/0
      reply.send({ message: val });
    } catch (error) {
      ReportError(error);
      reply.send({ message: "Something went wrong" });
    }
});   

// Your boostrap function ...
      |]

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring (Express Platform)"
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them, you can also monitor request in a background job or outside request context. To achieve this, wrap your axios instance with APItoolkit's observeAxios function."
      codeExample $ outgoingRequest apikey

      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring (Fastify Platform)"
      codeExample
        $ [text|
import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { FastifyAdapter, NestFastifyApplication} from '@nestjs/platform-fastify';
import APIToolkit, { observeAxios } from "apitoolkit-fastify";
import axios from "axios"
import Fastify from 'fastify';
const fastify = Fastify();

const apittoolkitClient = APIToolkit.NewClient({ apiKey: "$apikey", fastify: fastify });
apitoolkitClient.init();

fastify.get('/', async (request, reply) => {
    try {
        // The http request is monitored and will appear in the log explorer
        const res = await observeAxios(axios).get("/hello");
        reply.send({ data: res.data });
    } catch (err) {
      reply.send({error: "Something went wreong"})
    }
});

// Your boostrap function goes here ...
      
      |]


configOptions :: Text
configOptions =
  [text|
 {
     // List of request and response headers to be redacted,
     redactHeaders: ["Authorization"],
     // Json path list of response body fields to be redacted,
     redactResponseBody: ["$.user.email", "$.user.age"],
     // Json path list of request body fields to be redacted,
     redactRequestBody: ["$.password", "$.credit_card", "$.ccv"],
     // Set debug to true to help in troubleshooting issues,
     debug: true,
  }
|]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  T.unlines
    [ "import { APIToolkit, ReportError } from \"apitoolkit-express\";"
    , "import express from \"express\";"
    , "import axios from \"axios\";"
    , ""
    , "const app = express();"
    , ""
    , "app.get(\"/\", (req, res) => {"
    , "  try {"
    , "    let inf = 1/0;"
    , "    res.send(\"The impossible number is: \" + inf);"
    , "  } catch (error) {"
    , "    // Manually report errors to APItoolkit"
    , "    ReportError(error);"
    , "    res.send(\"Something went wrong\");"
    , "  }"
    , "});"
    , "// Automatically report unhandled errors"
    , "// Error handler must be before any other error middleware and after all controllers"
    , "app.use(apitoolkitClient.errorHandler);"
    , "//... your boostrap function"
    ]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  [text|
import { NestFactory } from '@nestjs/core';
import { APIToolkit } from 'apitoolkit-express';
import { AppModule } from './app.module';
import axios from 'axios';
import { observeAxios } from 'apitoolkit-express';

app.get('/', (req, res) => {
    const response = await observeAxios(axios).get("https://jsonplaceholder.typicode.com/posts/1");
    res.send(response.data);
});
|]
