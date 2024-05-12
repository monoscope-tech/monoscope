module Pages.IntegrationDemos.NestJs (nestGuide) where

import Data.Text
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
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      p_ [] do
        "After setting up the SDK in your "
        codeEmphasis "main.ts"
        "file you can monitor requests like so"
      h3_ [class_ "text-2xl font-semibold mt-2"] "Error Reporting (Express Platform)"
      codeExample $ errorReportingCode apikey

      h3_ [class_ "text-2xl font-semibold mt-2"] "Error Reporting (Fastify Platform)"
      codeExample
        $ [text|
import { Controller, Get } from '@nestjs/common';
import { AppService } from './app.service';
import { ReportError } from 'apitoolkit-fastify';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(): string {
    try {
      throw new Error('something went wrong');
    } catch (error) {
      // Report error to APItoolkit
      ReportError(error);
    }
    return this.appService.getHello();
  }
}
      |]

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them, you can also monitor request in a background job or outside request context. To achieve this, wrap your axios instance with APItoolkit's observeAxios function."
      p_ [] do
        "After setting up the SDK in your "
        codeEmphasis "main.ts"
        "file you can monitor requests like so"

      h3_ [class_ "text-2xl font-semibold mt-2"] "Outgoing Request Monitoring (Express Platform)"
      codeExample $ outgoingRequest apikey

      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring (Fastify Platform)"
      codeExample
        $ [text|
import { Controller, Get } from '@nestjs/common';
import { AppService } from './app.service';
import { observeAxios } from 'apitoolkit-fastify';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(): string {
    const response = await observeAxios(axios).get("https://jsonplaceholder.typicode.com/posts/1")
    return response.data;
  }
}
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
  [text|
import { Controller, Get } from '@nestjs/common';
import { AppService } from './app.service';
import { ReportError } from 'apitoolkit-express';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(): string {
    try {
      throw new Error('something went wrong');
    } catch (error) {
      // Report error to APItoolkit
      ReportError(error);
    }
    return this.appService.getHello();
  }
}
|]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  [text|
import { Controller, Get } from '@nestjs/common';
import { AppService } from './app.service';
import { observeAxios } from 'apitoolkit-express';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(): string {
    const response = await observeAxios(axios).get("https://jsonplaceholder.typicode.com/posts/1")
    return response.data;
  }
}
|]
