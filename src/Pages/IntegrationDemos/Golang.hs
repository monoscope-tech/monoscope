module Pages.IntegrationDemos.Golang (golangGuide) where

import Data.Text qualified as T
import Lucid
import NeatInterpolation (text)
import Pkg.Components
import Relude

golangGuide :: Text -> Html ()
golangGuide apikey =
  section_ [class_ "flex flex-col gap-4 lang-guide hidden", id_ "go_main"] do
    span_ [class_ "text-2xl font-semibold text-strong"] "Integrate Golang SDK"
    -- div_ [class_ "flex items-center gap-2"] do
    --   mapM_ featureItem features
    div_ [class_ "flex flex-col gap-2"] do
      span_ [class_ "text-strong font-semibold"] "Select framework"
      div_ [class_ "flex items-center gap-2"] do
        mapM_ (frameworkItem "go") frameworks
    div_ [class_ "w-full border-b"] pass
    goGuideTemplate apikey "Gin" ginCode
    goGuideTemplate apikey "Echo" echoCode
    goGuideTemplate apikey "Fiber" fiberCode
    goGuideTemplate apikey "Gorilla" gorillaMuxCode
    goGuideTemplate apikey "Native" nativeCode
    goGuideTemplate apikey "Chi" chiCode


frameworks :: [Text]
frameworks = ["Gin", "Echo", "Fiber", "Gorilla", "Native", "Chi"]

goGuideTemplate :: Text -> Text -> Text -> Html ()
goGuideTemplate apikey framework initCode = do
  section_ [class_ $ "flex flex-col gap-10 go-guide " <> if framework == "Gin" then "" else "hidden", id_ framework] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Installation"
      p_ [class_ "text-gray-600 font-medium"] $ toHtml $ "Run the command below to install the APItoolkit " <> framework <> " sdk"
      bashCommand $ "go get github.com/apitoolkit/apitoolkit-go/" <> T.toLower framework
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Open Telemetry SDK Configuration"
      p_ [class_ "text-gray-600 font-medium"] "Set the neccessary environment variables to configure the SDK"
      codeExample $ otelConfig apikey
    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] $ toHtml $ "Configure & Initialize APItoolkit " <> framework <> " sdk"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming http request, errors and outgoing request."
      codeExample initCode
      p_ [class_ "text-gray-600 font-medium"] do
         "In the demo code abovec, the "
         codeEmphasis "shutdown, err := apitoolkit.ConfigureOpenTelemetry()"
         " Is only neccesary if you don't have OpenTelemetry configured in your application already."
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts the following optional fields to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample $ configOptions


otelConfig :: Text -> Text
otelConfig apiKey =
  [text|
OTEL_RESOURCE_ATTRIBUTES="at-project-key=$apiKey" # Your apitoolkit API key (required)
OTEL_SERVICE_NAME="apitoolkit-otel-go-demo" # Service name for your the service you're integrating in (optional)
OTEL_SERVICE_VERSION="0.0.1" # Your application's service version (optional)
# OTHER OTEL ENVIRONMENT VARIABLES
|]


configOptions :: Text
configOptions = [text|
apitoolkit.Config{
	RedactHeaders:       []string{"Authorization", "X-Api-Key"},
	RedactRequestBody:   []string{"password", "credit_card"},
	RedactResponseBody:  []string{"password", "credit_card"},
	Debug:               false,
	ServiceName:         "my-service",
	ServiceVersion:      "1.0.0",
	CaptureRequestBody:  true,
	CaptureResponseBody: true,
	Tags:                []string{"prod", "eu"},
}|]



fiberCode :: Text
fiberCode = [text|
package main

import (
	"log"

	apitoolkit "github.com/apitoolkit/apitoolkit-go/fiber"
	"github.com/gofiber/fiber/v2"
	_ "github.com/joho/godotenv/autoload" // autoload .env file for otel configuration
)

func main() {
	// Configure OpenTelemetry
	shutdown, err := apitoolkit.ConfigureOpenTelemetry()
	if err != nil {
		log.Printf("error configuring openTelemetry: %v", err)
	}
	defer shutdown()

	app := fiber.New()
	// Register APItoolkit's middleware
	app.Use(apitoolkit.Middleware(apitoolkit.Config{}))

	// Define a route for Hello World
	app.Get("/", func(c *fiber.Ctx) error {
		return c.JSON(fiber.Map{
			"message": "Hello, World!",
		})
	})

	app.Listen(":3000")
}
|]

chiCode :: Text
chiCode = [text|
package main

import (
	"log"
	"net/http"

	apitoolkit "github.com/apitoolkit/apitoolkit-go/chi"
	"github.com/go-chi/chi/v5"
	_ "github.com/joho/godotenv/autoload" // autoload .env file for otel configuration
)

func main() {
	// Configure OpenTelemetry
	shutdown, err := apitoolkit.ConfigureOpenTelemetry()
	if err != nil {
		log.Printf("error configuring openTelemetry: %v", err)
	}
	defer shutdown()

	r := chi.NewRouter()
	// Add the apitoolkit chi middleware to monitor http requests
	// And report errors to apitoolkit
	r.Use(apitoolkit.Middleware(apitoolkit.Config{}))

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("Hello, world!"))
	})

	http.ListenAndServe(":8000", r)
}|]

gorillaMuxCode :: Text
gorillaMuxCode = [text|
package main

import (
	"log"
	"net/http"

	apitoolkit "github.com/apitoolkit/apitoolkit-go/gorilla"
	"github.com/gorilla/mux"
	_ "github.com/joho/godotenv/autoload" // autoload .env file for otel configuration
)

func main() {
	shutdown, err := apitoolkit.ConfigureOpenTelemetry()
	if err != nil {
		log.Printf("error configuring openTelemetry: %v", err)
	}
	defer shutdown()

	router := mux.NewRouter()
	// Register APItoolkit's middleware
	router.Use(apitoolkit.Middleware(apitoolkit.Config{}))

	router.HandleFunc("/test", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("ok"))
	})
	http.Handle("/", router)
	http.ListenAndServe(":8000", router)

}
|]

nativeCode :: Text
nativeCode = [text|
package main

import (
	"log"
	"net/http"

	apitoolkit "github.com/apitoolkit/apitoolkit-go/native"
	_ "github.com/joho/godotenv/autoload" // autoload .env file for otel configuration
)

func main() {
	// configure openTelemetry
	shutdown, err := apitoolkit.ConfigureOpenTelemetry()
	if err != nil {
		log.Printf("error configuring openTelemetry: %v", err)
	}
	defer shutdown()

	handler := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("Hello, World!"))
	})
	// configure apitoolkit middleware
	nativeMiddleware := apitoolkit.Middleware(apitoolkit.Config{
		RedactHeaders:      []string{"Authorization", "X-Api-Key"},
		RedactRequestBody:  []string{"password", "credit_card"},
		RedactResponseBody: []string{"password", "credit_card"},
	})
	// Wrap handler with middleware for monitoring requests and reporting errors
	http.Handle("/", nativeMiddleware(handler))
	if err := http.ListenAndServe(":8000", nil); err != nil {
		log.Fatal(err)
	}
}
|]

ginCode :: Text
ginCode = [text|
package main

import (
	"log"

	apitoolkit "github.com/apitoolkit/apitoolkit-go/gin"
	"github.com/gin-gonic/gin"
  _ "github.com/joho/godotenv/autoload" // autoload .env file for otel configuration

)

func main() {
	// Configure openTelemetry, if you don't have it configured already.
	shutdown, err := apitoolkit.ConfigureOpenTelemetry()
	if err != nil {
		log.Printf("error configuring openTelemetry: %v", err)
	}
	defer shutdown()

	r := gin.Default()
	// Add the apitoolkit gin middleware to monitor http requests and error reporting
	r.Use(apitoolkit.Middleware(apitoolkit.Config{}))

	r.GET("/greet/:name", func(c *gin.Context) {
		c.JSON(http.StatusOK, gin.H{"message": "Hello " + c.Param("name")})
	})

	r.Run(":8000")
}
|]

echoCode :: Text
echoCode = [text|
package main

import (
	"log"

	apitoolkit "github.com/apitoolkit/apitoolkit-go/echo"
	"github.com/labstack/echo/v4"
  _ "github.com/joho/godotenv/autoload" // autoload .env file for otel configuration

)

func main() {
  // Configure OpenTelemetry
	shutdown, err := apitoolkit.ConfigureOpenTelemetry()
	if err != nil {
		log.Printf("error configuring openTelemetry: %v", err)
	}
	defer shutdown()

	router := echo.New()
	// Register APItoolkit's middleware
	router.Use(apitoolkit.Middleware(apitoolkit.Config{}))

	router.GET("/:slug/test", func(c echo.Context) error {
		return c.String(http.StatusOK, "Ok, success!")
	})

	router.Start(":8000")
}
|]
