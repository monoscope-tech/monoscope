module Pages.IntegrationDemos.Gin (ginGuide) where

import Data.Text
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx
import Pkg.Components
import Relude

ginGuide :: Text -> Maybe Text -> Maybe Text -> Html ()
ginGuide apikey errorReportingM integrationsM = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-bold"] "Install"
      p_ [class_ "text-gray-600 font-medium flex items-center gap-1"] do
        "Install the APIToolkit gin SDK using "
        span_ [class_ "text-red-500"] "go get:"
      bashCommand "go get github.com/apitoolkit/apitoolkit-go"

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-bold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your gin server"
      codeExample $ initCode apikey

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK has accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-bold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APIToolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production. Aside reporting errors manually, we also automatically report unrecovered panics that occured during a request."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-bold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APIToolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them, you can also monitor request in a background job or outside request context. To monitor outgoing HTTP requests from your Go application, you can replace the default HTTP client transport with a custom APIToolkit roundtripper."
      codeExample $ outgoingRequest apikey

initCode :: Text -> Text
initCode apiKey =
  T.unlines
    [ "package main",
      "",
      "import (",
      "\tcontext",
      "",
      "\tapitoolkit \"github.com/apitoolkit/apitoolkit-go\"",
      "\tgithub.com/gin-gonic/gin\"",
      ")",
      "",
      "func main() {",
      "\tapitoolkitCfg := apitoolkit.Config{",
      "\t\tAPIKey: \"" <> apiKey <> "\",",
      "\t}",
      "",
      "\t// Initialize the client using your apitoolkit generated apikey",
      "\tapitoolkitClient, _ := apitoolkit.NewClient(context.Background(), apitoolkitCfg)",
      "\trouter := gin.New()",
      "",
      "\t// Register with the corresponding middleware of your choice. For Gin router, we use the GinMiddleware method.",
      "\trouter.Use(apitoolkitClient.GinMiddleware)",
      "",
      "\t// Register your handlers and run the gin server as usual.",
      "\trouter.POST(\"/:slug/test\", func(c *gin.Context) {",
      "\t\tc.JSON(200, gin.H{\"message\": \"Hello world\"})",
      "\t})",
      "\trouter.Run(\":8080\")",
      "}"
    ]

configOptions :: Text
configOptions =
  T.unlines
    [ "apitoolkit.Config{",
      "\t// Your apikey, the only required field",
      "\tAPIKey: \"<API_KEY>\"",
      "\t// Redacting both request and response headers",
      "\tRedactHeaders: []string{\"Content-Type\", \"Authorization\", \"Cookies\"},",
      "\t// A jsonpath list of request body fields to redact",
      "\tRedactRequestBody: []string{\"$.user.password\", \"$.user.creditCard.number\"},",
      "\t// A jsonpath list of response body fields to redact",
      "\tRedactResponseBody: []string{\"$.message.error\"},",
      "\t// Set to true to enable debug mode",
      "\tDebug: false",
      "\t// The current version of your api service",
      "\tServiceVersion: \"1.0.1\",",
      "\t// Allows you to add tags for this service",
      "\tTags: []",
      "}"
    ]

errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  T.unlines
    [ "package main",
      "",
      "import (",
      "\t\"github.com/gin-gonic/gin\"",
      "\t\"context\"",
      "\tapitoolkit \"github.com/apitoolkit/apitoolkit-go\"",
      ")",
      "",
      "func main() {",
      "\tr := gin.Default()",
      "\tapitoolkitClient, err := apitoolkit.NewClient(context.Background(), apitoolkit.Config{APIKey: \"" <> apiKey <> "\"})",
      "\tif err != nil {",
      "\t\tpanic(err)",
      "\t}",
      "",
      "\tr.Use(apitoolkitClient.GinMiddleware)",
      "",
      "\tr.GET(\"/\", func(c *gin.Context) {",
      "\t\tfile, err := os.Open(\"non-existing-file.txt\")",
      "\t\tif err != nil {",
      "\t\t\t// Report an error to apitoolkit",
      "\t\t\tapitoolkit.ReportError(c.Request.Context(), err)",
      "\t\t}",
      "\t\tc.String(http.StatusOK, \"Hello, World!\")",
      "\t})",
      "",
      "\tr.Run(\":8080\")",
      "}"
    ]

outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  T.unlines
    [ "package main",
      "",
      "import (",
      "\t\"context\"",
      "\t\"net/http\"",
      "",
      "\tapitoolkit \"github.com/apitoolkit/apitoolkit-go\"",
      "\t\"github.com/gin-gonic/gin\"",
      ")",
      "",
      "func main() {",
      "",
      "\tapitoolkitClient, err := apitoolkit.NewClient(context.Background(), apitoolkit.Config{APIKey: \"" <> apiKey <> "\"})",
      "\tif err != nil {",
      "\t\tpanic(err)",
      "\t}",
      "",
      "\trouter := gin.New()",
      "",
      "\t// Register with the corresponding middleware of your choice. For Gin router, we use the GinMiddleware method.",
      "\trouter.Use(apitoolkitClient.GinMiddleware)",
      "",
      "\trouter.POST(\"/:slug/test\", func(c *gin.Context) (err error) {",
      "\t\t// Create a new HTTP client",
      "\t\tHTTPClient := http.DefaultClient",
      "",
      "\t\t// Replace the transport with the custom roundtripper",
      "\t\tHTTPClient.Transport = client.WrapRoundTripper(",
      "\t\t\tc.Request().Context(),",
      "\t\t\tHTTPClient.Transport,",
      "\t\t\tWithRedactHeaders([]string{})",
      "\t\t)",
      "",
      "\t\t// Make an outgoing HTTP request using the modified HTTPClient",
      "\t\t_, _ = HTTPClient.Get(\"https://jsonplaceholder.typicode.com/posts/1\")",
      "",
      "\t\t// Respond to the request",
      "\t\tc.JSON(http.StatusOK, gin.H{\"message\": \"outgoing request monitored\"})",
      "\t})",
      "}"
    ]