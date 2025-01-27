module Pages.IntegrationDemos.Csharp (csharpGuide) where

import Data.Text qualified as T
import Lucid
import NeatInterpolation (text)
import Pkg.Components
import Relude


csharpGuide :: Text -> Html ()
csharpGuide apikey =
  section_ [class_ "flex flex-col gap-4 lang-guide hidden", id_ "cs_main"] do
    span_ [class_ "text-2xl font-semibold  text-textStrong"] "Integrate .NET SDK"
    -- div_ [class_ "flex items-center gap-2"] do
    --   mapM_ featureItem features
    div_ [class_ "flex flex-col gap-2"] do
      csharpGuideTemplate apikey


csharpGuideTemplate :: Text -> Html ()
csharpGuideTemplate apikey = do
  section_ [class_ "flex flex-col gap-10 csharp-guide "] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Installation"
      p_ [class_ "text-gray-600 font-medium"] $ toHtml "Run the command below to install the APItoolkit .NET sdk"
      bashCommand "dotnet add package ApiToolkit.NET"
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Open Telemetry SDK Configuration"
      p_ [] "To instrument a .NET application automatically, download and run the installer script for Linux/Mac"
      codeExample
        [text|
# Download the bash script
curl -sSfL https://github.com/open-telemetry/opentelemetry-dotnet-instrumentation/releases/latest/download/otel-dotnet-auto-install.sh -O

# Install core files
sh ./otel-dotnet-auto-install.sh

# Enable execution for the instrumentation script
chmod +x $$HOME/.otel-dotnet-auto/instrument.sh

# Setup the instrumentation for the current shell session
. $$HOME/.otel-dotnet-auto/instrument.sh
|]
      a_ [href_ "https://opentelemetry.io/docs/zero-code/net/#windows-powershell", class_ "text-brand"] "See windows auto intrumentation guide"
      p_ [class_ "text-gray-600 font-medium"] "Set the neccessary environment variables to configure the SDK"
      codeExample $ otelConfig apikey
      p_ [class_ "text-gray-600 font-medium"] "After setting the environment variables, build and run your application and you should see the logs, traces and metrics in the APIToolkit dashboard."
    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] $ toHtml $ "Configure & Initialize APItoolkit .NET Core sdk"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming http request, errors and outgoing request."
      codeExample initCode
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts the following optional fields to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions


otelConfig :: Text -> Text
otelConfig apikey =
  [text|
export OTEL_EXPORTER_OTLP_ENDPOINT="http://otelcol.apitoolkit.io:4317" # Specifies the endpoint to send the traces to.
export OTEL_DOTNET_AUTO_TRACES_ADDITIONAL_SOURCES="APItoolkit.HTTPInstrumentation" # The apitoolkit instrumentation  activity resource.
export OTEL_SERVICE_NAME="my-service" # Specifies the name of the service.
export OTEL_RESOURCE_ATTRIBUTES="at-project-key=$apikey" # Adds your API KEY to the resource.
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc" #
|]


initCode :: Text
initCode =
  [text|
using ApiToolkit.Net;

var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

// Initialize the APItoolkit client
var config = new Config{};
var client = APIToolkit.NewClient(config);

// Register APItoolkit's middleware
app.Use(async (context, next) =>
{
  var apiToolkit = new APIToolkit(next, client);
  await apiToolkit.InvokeAsync(context);
});

app.MapGet("/hello", async context =>
{
  await context.Response.WriteAsync("Hello, world!");
});

app.Run();
|]


configOptions :: Text
configOptions =
  [text|
var config = new Config
{
    // Set to true to enable debug mode for additional logging and troubleshooting.
    Debug = false,
    // A list of tags for grouping and filtering data on the dashboard.
    Tags = new List<string> { "environment: production", "region: us-east-1" },
    // The defined version of your application for debugging purposes.
    ServiceVersion = "v2.0",
    // The name of the service to distinguish it on the dashboard.
    ServiceName = "MyService",
    // A list of HTTP header keys to redact for privacy.
    RedactHeaders = new List<string> { "Authorization", "X-API-Key" },
    // A list of JSONPaths from the request body to redact for privacy.
    RedactRequestBody = new List<string> { "$.password", "$.creditCard.number" },
    // A list of JSONPaths from the response body to redact for privacy.
    RedactResponseBody = new List<string> { "$.user.token", "$.transaction.details" },
    // Set to true to capture the request body.
    CaptureRequestBody = true,
    // Set to true to capture the response body.
    CaptureResponseBody = true,
};
|]
