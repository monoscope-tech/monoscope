module Pages.IntegrationDemos.GorillaMux (gorillaGuide) where

import Data.Text
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


gorillaGuide :: Text -> Html ()
gorillaGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium flex items-center gap-1"] do
        "Install the APItoolkit gorilla mux SDK using "
        codeEmphasis "go get:"
      bashCommand "go get github.com/apitoolkit/apitoolkit-go"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your gorilla mux server"
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
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them, you can also monitor request in a background job or outside request context. To monitor outgoing HTTP requests from your Go application, you can replace the default HTTP client transport with a custom APIToolkit roundtripper."
      codeExample $ outgoingRequest apikey


initCode :: Text -> Text
initCode apiKey =
  [text|
package main

import (
  "context"
  "net/http"

  "github.com/gorilla/mux"
  apitoolkit "github.com/apitoolkit/apitoolkit-go/gorilla"
)

func main() {
  ctx := context.Background()

  // Initialize the APItoolkit client
  apitoolkitClient, err := apitoolkit.NewClient(
    ctx,
    apitoolkit.Config{
      APIKey: "$apiKey",
    },
  )
  if err != nil {
    panic(err)
  }

  router := mux.NewRouter()

  // Register APItoolkit's middleware
  router.Use(apitoolkit.GorillaMuxMiddleware(apitoolkitClient))

  // router.Use(...)
  // Other middleware

  router.HandleFunc("/test", func(w http.ResponseWriter, r *http.Request) {
    w.WriteHeader(http.StatusOK)
    w.Write([]byte("ok"))
  })

  http.Handle("/", router)
  http.ListenAndServe(":8080", router)
}|]


configOptions :: Text
configOptions =
  [text|apitoolkit.Config{
      // Your apikey, the only required field
      APIKey: "<API_KEY>",
      // Redacting both request and response headers"
      RedactHeaders: []string{"Content-Type", "Authorization", "Cookies"},
      // A jsonpath list of request body fields to redact"
      RedactRequestBody: []string{"$.user.password", "$.user.creditCard.number"},
      // A jsonpath list of response body fields to redact
      RedactResponseBody: []string{"$.message.error"},
      // Set to true to enable debug mode"
      Debug: false,
      // The current version of your api service"
      ServiceVersion: "1.0.1",
      // Allows you to add tags for this service"
      Tags: [],
      }
    |]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  [text|
package main

import (
  "context"
  "fmt"
  "net/http"
  "os"
  "log"

  "github.com/gorilla/mux"
  apitoolkit "github.com/apitoolkit/apitoolkit-go/gorilla"
)

func main() {
  ctx := context.Background()
  apitoolkitClient, err := apitoolkit.NewClient(
    ctx,
    apitoolkit.Config{APIKey: "$apiKey"},
  )
  if err != nil {
    panic(err)
  }

  router := mux.NewRouter()
  router.Use(apitoolkit.GorillaMuxMiddleware(apitoolkitClient))

  router.HandleFunc("/", hello)

  http.Handle("/", router)
  log.Fatal(http.ListenAndServe(":8080", nil))
}

func hello(w http.ResponseWriter, r *http.Request) {
  // Attempt to open a non-existing file
  _, err := os.Open("non-existing-file.txt")
  if err != nil {
    // Report the error to APItoolkit
    apitoolkit.ReportError(r.Context(), err)
  }
  fmt.Fprintln(w, "Hello, World!")
}|]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  [text|
package main

import (
  "context"
  "log"
  "net/http"

  "github.com/gorilla/mux"
  apitoolkit "github.com/apitoolkit/apitoolkit-go/gorilla"
)

func main() {
  ctx := context.Background()

  apitoolkitClient, err := apitoolkit.NewClient(
    ctx,
    apitoolkit.Config{APIKey: "$apiKey"},
  )
  if err != nil {
    panic(err)
  }

  router := mux.NewRouter()
  router.Use(apitoolkit.GorillaMuxMiddleware(apitoolkitClient))

  router.HandleFunc("/{slug}/test", func(w http.ResponseWriter, r *http.Request) {
    // Create a new HTTP client
    HTTPClient := apitoolkit.HTTPClient(
      r.Context(),
      apitoolkit.WithRedactHeaders("content-type", "Authorization", "HOST"),
      apitoolkit.WithRedactRequestBody("$.user.email", "$.user.addresses"),
      apitoolkit.WithRedactResponseBody("$.users[*].email", "$.users[*].credit_card"),
    )

    // Make an outgoing HTTP request using the modified HTTPClient
    _, _ = HTTPClient.Get("https://jsonplaceholder.typicode.com/posts/1")

    // Respond to the request
    w.WriteHeader(http.StatusOK)
    w.Write([]byte("Ok, success!"))
  }).Methods(http.MethodPost)

  log.Fatal(http.ListenAndServe(":8080", router))
}
|]
