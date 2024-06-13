module Pages.IntegrationDemos.Springboot (springGuide) where

import Data.Text
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


springGuide :: Text -> Html ()
springGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] do
        "Add the dependency to your"
        codeEmphasis " pom.xml "
        "file"
      codeExample
        [text|
<dependency>
    <groupId>io.apitoolkit.springboot</groupId>
    <artifactId>apitoolkit-springboot</artifactId>
    <version>1.0.5</version>
</dependency>
      |]

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] do
        "Add this to your"
        codeEmphasis " application.properties "
        "file"
      codeExample $ configOptions apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] do
        "To initialize the the sdk add the " <> codeEmphasis " @EnableAPIToolkit " <> "custom annotation to your application's main class."
      codeExample $ initCode

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them."
      codeExample $ outgoingRequest apikey


initCode :: Text
initCode =
  [text|
package com.example.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.*;

// Import APItoolkit annotation
import io.apitoolkit.springboot.annotations.EnableAPIToolkit;

@SpringBootApplication
@EnableAPIToolkit
@RestController
public class DemoApplication {

	public static void main(String[] args) {
		SpringApplication.run(DemoApplication.class, args);
	}

	@GetMapping("/user/{name}")
	public User getUser(@PathVariable String name) {
		return new User("Jon Doe", "[email protected]");
	}
}
|]


configOptions :: Text -> Text
configOptions apikey =
  [text|
# Your apikey (required)
apitoolkit.apikey=$apikey;
# A list of requesta nd response headers to be redacted.
apitoolkit.redactHeaders=content-type,Authorization,HOST
# A list of request body fields (jsonpaths) to be redacted
apitoolkit.redactRequestBody=$.user.email,$.user.addresses
# A list of response body fields (jsonpaths) to be redacted. 
apitoolkit.redactResponseBody=$.users[*].email,$.users[*].credit_card
# Set to true to enable debug mode
apitoolkit.apikey={ENTER_YOUR_API_KEY_HERE}
|]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  [text|
package com.example.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.*;
import jakarta.servlet.http.HttpServletRequest;

import io.apitoolkit.springboot.APErrors;
import io.apitoolkit.springboot.annotations.EnableAPIToolkit;

@EnableAPIToolkit
@SpringBootApplication
public class DemoApplication {

    public static void main(String[] args) {
        SpringApplication.run(DemoApplication.class, args);
    }

    @GetMapping("/hello")
    public String hello(@RequestParam(value = "name", defaultValue = "World") String name, HttpServletRequest request) {
        try {
            System.out.print(1 / 0); // This will throw an ArithmeticException
        } catch (Exception e) {
            // Report the error to APItoolkit
            APErrors.reportError(request, e);
        }
        return String.format("Hello %s!", name);
    }
}
|]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  [text|
package com.example.demo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import javax.servlet.http.HttpServletRequest;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.util.EntityUtils;

@SpringBootApplication
@EnableAPIToolkit
@RestController
public class DemoApplication {

    public static void main(String[] args) {
        SpringApplication.run(DemoApplication.class, args);
    }

    // Create an Instance of ObserveRequest
    private ObserveRequest observingClient = new ObserveRequest(
        List.of("cookies", "authorization", "x-api-key"),
        List.of("$.title", "$.id"),
        List.of("$.body")
    );

    @GetMapping("/hello")
    public String hello(HttpServletRequest request) {
        // Use observingClient to Create an HTTP Client
        CloseableHttpClient httpClient = observingClient.createHttpClient(request, "/posts/{post_id}");
        try {
            HttpGet httpGet = new HttpGet("https://jsonplaceholder.typicode.com/posts/1");
            CloseableHttpResponse response = httpClient.execute(httpGet);
            String responseStr = EntityUtils.toString(response.getEntity());
            return responseStr;
        } catch (Exception e) {
            e.printStackTrace();
            return "Error occurred while processing the request";
        }
    }
}
|]
