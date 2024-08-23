module Pages.IntegrationDemos.Phoenix (phoenixGuide) where

import Data.Text
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


phoenixGuide :: Text -> Html ()
phoenixGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] do
        "Install the SDK by adding"
        codeEmphasis " apitoolkit_phoenix "
        "to the dependencies in"
        codeEmphasis " mix.exs: "
      codeExample
        [text|def deps do
  [
    {:apitoolkit_phoenix, "~> 0.1.1"}
  ]
end|]

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] do
        "Import and initialize the"
        codeEmphasis " ApitoolkitPhoenix "
        "Plug in your"
        codeEmphasis " router.ex "
        "file to start monitoring incoming requests."
      codeExample
        [text|
defmodule HelloWeb.Router do
  use HelloWeb, :router
  use Plug.ErrorHandler
  import ApitoolkitPhoenix

  pipeline :api do
    plug :accepts, ["json"]
    # Other plugs
    plug ApitoolkitPhoenix,
      config: %{
        api_key: "$apikey",
      }
  end
end      
      |]

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize it. Redacting sensitive fields, debug mode etc."
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      h6_ [class_ "text-lg font-medium"] "Automatic error handling"
      codeExample
        [text|
@impl Plug.ErrorHandler
def handle_errors(conn, err) do
  conn = report_error(conn, err)
  json(conn, %{message: "Something went wrong"})
end
      |]
      h6_ [class_ "text-lg font-medium"] "Report errors manually to APItoolkit"
      codeExample
        [text|
defmodule HelloWeb.PageController do
  use HelloWeb, :controller
  import ApitoolkitPhoenix

  def home(conn, _params) do
    try do
      raise("Oops, something went wrong")
    rescue
      err ->
        report_error(conn, err, __STACKTRACE__)
    end

    json(conn, %{message: "Hello, world!"})
  end
end
      |]


configOptions :: Text
configOptions =
  [text|
config: %{
  redact_headers: ["accept-language", "cookie", "x-csrf-token"] # list of headers to be redacted
  redact_request_body: [".user.password", ".user.credit_card"] # list of json paths to redact from request body
  redact_response_body: [".users[*].email"] # list of json paths to redact from response body
  tags: ["prod", "eu"] # list of tags for the service
  service_version: "1.0.0" # current version of the service
}
|]
