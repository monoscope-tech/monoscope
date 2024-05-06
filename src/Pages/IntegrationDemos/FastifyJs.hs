module Pages.IntegrationDemos.FastifyJs (fastifyGuide) where

import Data.Text
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


fastifyGuide :: Text -> Html ()
fastifyGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] "Install the APItoolkit fastify SDK using npm/bun/pnpm"
      bashCommand "npm install apitoolkit-fastify"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your server"
      codeExample
        $ [text|
import Fastify from 'fastify';
import APIToolkit from 'apitoolkit-fastify';

const fastify = Fastify();

// Create and initialize an instance of the APIToolkit
const apittoolkitClient = APIToolkit.NewClient({
  apiKey: "$apikey", // required
  fastify: fastify, // required
});
apitoolkitClient.init();

//Rest of your app
fastify.get('/hello', function (request, reply) {
  reply.send({ message: 'Hello world' });
});

fastify.listen({ port: 3000 }, function (err, address) {
  if (err) {
    fastify.log.error(err);
    process.exit(1);
  }
});
      |]

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample
        $ [text|
{
    // List of request and response headers to be redacted
    redactHeaders: [\"Authorization\"],
    // Json path list of response body fields to be redacted
    redactResponseBody: [\"$.user.email\", \"$.user.age\"],
    // Json path list of request body fields to be redacted
    redactRequestBody: [\"$.password\", \"$.credit_card\", \"$.ccv\"],
    // Set debug to true to help in troubleshooting issues
    debug: true
}
      |]

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample
        $ [text|
import Fastify from 'fastify';
import axios from "axios";
import APIToolkit, { ReportError } from "apitoolkit-fastify";
const fastify = Fastify();

const apittoolkitClient = APIToolkit.NewClient({apiKey: "$apikey", fastify: fastify});
apitoolkitClient.init();

fastify.get('/', async (request, reply) => {
    try {
      const val  = 1/0
      reply.send({ message: val });
    } catch (error) {
      ReportError(error);
      reply.send({ message: "Something went wrong" });
    }
});
      |]

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them. To achieve this, wrap your axios instance with APItoolkit's observeAxios function."
      codeExample
        $ [text|
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
      |]
