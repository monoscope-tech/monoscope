module Pages.IntegrationDemos.Symfony (symfonyGuide) where

import Data.Text
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


symfonyGuide :: Text -> Html ()
symfonyGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] "Run the following command to install the Symfony package:"
      bashCommand "composer require apitoolkit/apitoolkit-symfony"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "SET API KEY"
      p_ [class_ "text-gray-600 font-medium"] do
        "Set the APITOOLKIT_KEY environment variable to your API key in you"
        codeEmphasis " .env "
        "file, should look like this:"
      codeExample $ apiKeyCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] do
        "Register the APItoolkit listener in your"
        codeEmphasis " service.yaml "
        "file"
      codeExample initCode

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] do
        "The SDK accepts other options alongside apikey to allow you to customize the sdk. Here's an example of what your"
        codeEmphasis " service.yaml "
        "file could look like, with the other options configured"
      codeExample configOptions


apiKeyCode :: Text -> Text
apiKeyCode apiKey =
  [text|
APITOOLKIT_KEY=$apiKey
|]


initCode :: Text
initCode =
  [text|
services:
  APIToolkit\EventSubscriber\APIToolkitService:
    arguments:
      $$apiKey: '%env(APITOOLKIT_KEY)%'
    # Optional:  if you want to cache login result add this cache poll instance via setter injection
    calls:
      - setCachePool: ['@PutYourCachePoolServiceHere']
    tags:
      - { name: 'kernel.event_subscriber' }
|]


configOptions :: Text
configOptions =
  [text|
services:

  APIToolkit\EventSubscriber\APIToolkitService:
    arguments:
      $$apiKey: '%env(APITOOLKIT_KEY)%'
      $$redactedHeaders: 
        - 'HOST' # Note that you don't need json path syntax for headers
        - 'CONTENT-TYPE'
      $$redactRequestBody:
        - `$.password`
        - `$.payment.credit_cards[*].cvv`
        - `$.user.addresses[*]`
      $$redactResponseBody:
        - `$.title`
        - `$.store.books[*].author`
|]
