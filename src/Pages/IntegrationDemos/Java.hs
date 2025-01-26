module Pages.IntegrationDemos.Java (javaGuide) where

import Data.Text qualified as T
import Lucid
import NeatInterpolation (text)
import Pkg.Components
import Relude


javaGuide :: Text -> Html ()
javaGuide apikey =
  section_ [class_ "flex flex-col gap-4 lang-guide hidden", id_ "java_main"] do
    span_ [class_ "text-2xl font-semibold text-strong"] "Integrate Java SDK"
    -- div_ [class_ "flex items-center gap-2"] do
    --   mapM_ featureItem features
    div_ [class_ "flex flex-col gap-2"] do
      csharpGuideTemplate apikey


csharpGuideTemplate :: Text -> Html ()
csharpGuideTemplate apikey = do
  section_ [class_ "flex flex-col gap-10 java-guide "] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Installation"
      p_ [class_ "text-gray-600 font-medium"] do
        "To install the SDK, kindly add the following dependency to your "
        codeEmphasis "pom.xml"
        " file within the "
        codeEmphasis "<dependencies>"
        " section, like so:"
      codeExample
        [text|
<dependency>
  <groupId>io.apitoolkit.springboot</groupId>
  <artifactId>apitoolkit-springboot</artifactId>
  <version>2.0.9</version>
</dependency>
      |]
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Open Telemetry SDK Configuration"
      p_ [] "To instrument a Springboot application automatically, download the java agent and run the following command"
      bashCommand "curl -L -O https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases/latest/download/opentelemetry-javaagent.jar"
      p_ [class_ "text-gray-600 font-medium"] "Set the neccessary environment variables to configure the SDK"
      codeExample $ otelConfig apikey
      p_ [class_ "text-gray-600 font-medium"] "After setting the environment variables, run your application using the command below and you should see the logs, traces and metrics in the APIToolkit dashboard."
      bashCommand "mvn spring-boot:run -Dspring-boot.run.jvmArguments=\"-javaagent:/opentelemetry-javaagent.jar\""
    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] $ toHtml $ "Configure & Initialize APItoolkit Springboot SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming http request, errors and outgoing request."
      codeExample initCode
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] do
        "The sdk can be configured using the following optional properties in your"
        codeEmphasis " resource/application.properties "
        "file:"
      codeExample configOptions


otelConfig :: Text -> Text
otelConfig apikey =
  [text|
export OTEL_EXPORTER_OTLP_ENDPOINT="http://otelcol.apitoolkit.io:4317" # Specifies the endpoint to send the traces to.
export OTEL_SERVICE_NAME="my-service" # Specifies the name of the service.
export OTEL_RESOURCE_ATTRIBUTES="at-project-key=$apikey" # Adds your API KEY to the resource.
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc" #
|]


initCode :: Text
initCode =
  [text|
package com.example.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
// Import APItoolkit annotation
import io.apitoolkit.springboot.annotations.EnableAPIToolkit;
import org.springframework.web.bind.annotation.*;

@SpringBootApplication
// Add APIToolkit custom annotation
@EnableAPIToolkit
@RestController
public class DemoApplication {

  public static void main(String[] args) {
    SpringApplication.run(DemoApplication.class, args);
  }

  @GetMapping("/greet/{name}")
  public String getUser(@PathVariable String name) {
    return "Hello, " + name;
  }
}|]


configOptions :: Text
configOptions =
  [text|
apitoolkit.captureRequestBody=true # Capture request body.
apitoolkit.captureResponseBody=true # Capture response body.
apitoolkit.serviceName=my-service # Service name.
apitoolkit.serviceVersion="2.0" # Service version.
apitoolkit.tags = "value1,value2" # Comma-separated list of tags.
apitoolkit.redactHeaders = "Authorizations, X-Api-Key" # Comma-separated list of headers to redact.
apitoolkit.redactRequestBody= "$.password,$.creditCardNumber" # Comma-separated list of JSON paths to redact.
apitoolkit.redactResponseBody= "$.password,$.creditCardNumber" # Comma-separated list of JSON paths to redact.
apitoolkit.debug=false # Enable debug mode.
|]
