module Pages.IntegrationDemos.DotNet (dotNetGuide) where

import Data.Text
import Data.Text qualified as T
import Lucid
import Pkg.Components
import Relude

dotNetGuide :: Text -> Html ()
dotNetGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium flex items-center gap-1"] do
        "Install the APItoolkit .NET SDK using "
        span_ [class_ "text-red-500"] "dotnet add:"
      bashCommand "dotnet add package ApiToolkit.Net"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your .NET server"
      codeExample $ initCode apikey

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production. Aside reporting errors manually, we also automatically report unhandled exceptions that occured during a request."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] do
        "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them. You can achieve this by calling the "
        span_ [class_ "text-red-500"] " APIToolkit.ObservingHandler "
        "method of the apitoolkit client class and passing that to an HttpClient instance"
      codeExample $ outgoingRequest apikey

initCode :: Text -> Text
initCode apiKey =
  T.unlines
    [ "using ApiToolkit.Net;",
      "",
      "var builder = WebApplication.CreateBuilder(args);",
      "var app = builder.Build();",
      "",
      "var config = new Config",
      "{",
      "    ApiKey = \"" <> apiKey <> "\"",
      "};",
      "var client = await APIToolkit.NewClientAsync(config);",
      "",
      "app.Use(async (context, next) =>",
      "{",
      "    var apiToolkit = new APIToolkit(next, client);",
      "    await apiToolkit.InvokeAsync(context);",
      "});",
      "",
      "app.MapGet(\"/\", async (context) =>",
      "{",
      "    await context.Response.WriteAsync(\"Hello World!\");",
      "});",
      "",
      "app.Run();"
    ]

configOptions :: Text
configOptions =
  T.unlines
    [ "var config = new Config",
      "{",
      "    // Your api key, this is the only required field",
      "    ApiKey = <YOUR_API_KEY>",
      "    // Your api key, this is the only required field",
      "    ApiKey = \"YOUR_API_KEY\",",
      "    // List of headers to redact (i.e not send to APItoolkit)",
      "    RedactHeaders = new List<string> { \"Authorization\", \"X-User-Token\" },",
      "    // List of jsonpath of fields to redact in request body",
      "    RedactRequestBody = new List<string> { \"$.user.email\", \"$.user.password\" },",
      "    // List of jsonpath of fields to redact in response body",
      "    RedactResponseBody = new List<string> { \"$.user.account_number\", \"$.user.cvv\" },",
      "    // Set to true to enable debug mode",
      "    Debug = false,",
      "    // Version of your service, this allows your to track different versions of your app",
      "    ServiceVersion = \"1.0.0\",",
      "    // Tags for the service you're integrating with",
      "    Tags = new List<string> {\"hermis\"}",
      "};"
    ]

errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  T.unlines
    [ "using ApiToolkit.Net;",
      "",
      "var builder = WebApplication.CreateBuilder(args);",
      "var app = builder.Build();",
      "",
      "var config = new Config",
      "{",
      "    ApiKey = \"" <> apiKey <> "\"",
      "};",
      "var client = await APIToolkit.NewClientAsync(config);",
      "",
      "app.Use(async (context, next) =>",
      "{",
      "    var apiToolkit = new APIToolkit(next, client);",
      "    await apiToolkit.InvokeAsync(context);",
      "});",
      "",
      "app.MapGet(\"/error-tracking\", async context =>",
      "{",
      "    try",
      "    {",
      "        // Attempt to open a non-existing file (just an example)",
      "        using (var fileStream = System.IO.File.OpenRead(\"nonexistingfile.txt\"))",
      "        {",
      "            // File opened successfully, do something if needed",
      "        }",
      "        await context.Response.WriteAsync(\"Hello world!\");",
      "    }",
      "    catch (Exception ex)",
      "    {",
      "        // Report error to APItoolkit",
      "        client.ReportError(context, ex);",
      "        await context.Response.WriteAsync(\"Error reported!\");",
      "    }",
      "});",
      "",
      "app.Run();"
    ]

outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  T.unlines
    [ "using ApiToolkit.Net;",
      "",
      "var builder = WebApplication.CreateBuilder(args);",
      "var app = builder.Build();",
      "",
      "var config = new Config",
      "{",
      "    ApiKey = \"" <> apiKey <> "\"",
      "};",
      "var client = await APIToolkit.NewClientAsync(config);",
      "",
      "app.Use(async (context, next) =>",
      "{",
      "    var apiToolkit = new APIToolkit(next, client);",
      "    await apiToolkit.InvokeAsync(context);",
      "});",
      "",
      "app.MapGet(\"/\", async (context) =>",
      "{",
      "    var observingHandlerOptions = new ATOptions",
      "    {",
      "        // For paths with patterns",
      "        PathWildCard = \"/posts/{id}/{name}\",",
      "        // Headers to redact",
      "        RedactHeaders = [\"User-Agent\"],",
      "        // Request body fields to redact",
      "        RedactRequestBody = [\"$.user.password\"],",
      "        // Response body fields to redact",
      "        RedactResponseBody = [\"$.user.data.email\"]",
      "    };",
      "    using var httpClient = new HttpClient(client.APIToolkitObservingHandler(context, new ATOptions { PathWildCard = \"/todos/{id}\" }));",
      "    var request = new HttpRequestMessage(HttpMethod.Get, \"https://jsonplaceholder.typicode.com/todos/1?vood=dooo\");",
      "    var response = await httpClient.SendAsync(request);",
      "    var body = await response.Content.ReadAsStringAsync();",
      "    Console.WriteLine(body);",
      "    await context.Response.WriteAsync(\"Hello World!\");",
      "});",
      "",
      "app.Run();"
    ]
