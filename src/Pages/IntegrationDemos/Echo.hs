module Pages.IntegrationDemos.Echo (echoGuide) where

import Data.Text
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


echoGuide :: Text -> Html ()
echoGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium flex items-center gap-1"] do
        "Install the APItoolkit echo SDK using "
        codeEmphasis "go get:"
      bashCommand "go get github.com/apitoolkit/apitoolkit-go"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your echo server"
      codeExample $ initCode apikey

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production. Aside reporting errors manually, we also automatically report unrecovered panics that occured during a request."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them, you can also monitor request in a background job or outside request context. To monitor outgoing HTTP requests from your Go application, you can replace the default HTTP client transport with a custom APItoolkit roundtripper."
      codeExample $ outgoingRequest apikey


initCode :: Text -> Text
initCode apiKey =
  [text|
package main

import (
	"context"
	"github.com/labstack/echo/v4"
	apitoolkit "github.com/apitoolkit/apitoolkit-go"
)

func main() {
	ctx := context.Background()

	// Initialize the client using your apitoolkit.io generated apikey
	apitoolkitClient, err := apitoolkit.NewClient(ctx, apitoolkit.Config{APIKey: "$apiKey"})
	if err != nil {
		panic(err)
	}

	e := echo.New()

	// Register with the corresponding middleware of your choice.
	// Assuming apitoolkit provides an EchoMiddleware function for the echo framework.
	e.Use(apitoolkitClient.EchoMiddleware)

	e.POST("/:slug/test", func(c echo.Context) error {
		return c.String(http.StatusOK, "ok")
	})

	e.Start(":8080")
}

|]


configOptions :: Text
configOptions =
  [text|
apitoolkitCfg := apitoolkit.Config{
    RedactHeaders: []string{"Content-Type", "Authorization", "Cookies"}, // Redacting both request and response headers
    RedactRequestBody: []string{"$.user.password", "$.user.creditCard.number"}, // Redact request body using jsonpath
    RedactResponseBody: []string{"$.message.error"}, // Redact response body using jsonpath
    Tags: []string{"prod", "eu"} // Tags for the current service
    ServiceVersion: "1.0.0" // Your service's version
}
|]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  [text|
package main

import (
	"context"
	"github.com/labstack/echo/v4"
	apitoolkit "github.com/apitoolkit/apitoolkit-go"
)
func main() {
	e := echo.New()
	ctx := context.Background()

	apitoolkitClient, err := apitoolkit.NewClient(ctx, apitoolkit.Config{APIKey: "$apiKey"})
	if err != nil {
		panic(err)
	}

	e.Use(apitoolkitClient.EchoMiddleware)

	e.GET("/", hello)

	e.Logger.Fatal(e.Start(":1323"))
}

func hello(c echo.Context) error {
	file, err := os.Open("non-existing-file.txt")
	if err != nil {
		apitoolkit.ReportError(c.Request().Context(), err)
	}
	return c.String(http.StatusOK, "Hello, World!")
}

|]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  [text|
package main

import (
    "github.com/labstack/echo/v4"
    "net/http"
    apitoolkit "github.com/apitoolkit/apitoolkit-go"
)

func main() {
    router := echo.New()

    apitoolkitClient, err := apitoolkit.NewClient(ctx, apitoolkit.Config{APIKey: "$apiKey"})
	if err != nil {
		panic(err)
	}

    router.Use(apitoolkitClient.EchoMiddleware)

    router.POST("/:slug/test", func(c echo.Context) (err error) {
        // Create a new HTTP client
        HTTPClient := http.DefaultClient

        // Replace the transport with the custom roundtripper
        HTTPClient.Transport = client.WrapRoundTripper(
            c.Request().Context(),
            HTTPClient.Transport,
            WithRedactHeaders([]string{}),
        )

        // Make an outgoing HTTP request using the modified HTTPClient
        _, _ = HTTPClient.Get("http://localhost:3000/monitored-outgoing-request")

        // Respond to the request
        c.String(http.StatusOK, "ok")
    })
}
|]
