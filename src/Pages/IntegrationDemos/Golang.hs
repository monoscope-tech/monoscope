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
    ginGuide apikey
    echoGuide apikey


frameworks :: [Text]
frameworks = ["Gin", "Echo", "Fiber", "GorillaMux", "Native", "Chi"]


ginGuide :: Text -> Html ()
ginGuide apikey = do
  section_ [class_ "flex flex-col gap-10 go-guide", id_ "Gin"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium flex items-center gap-1"] do
        "Install the APItoolkit gin SDK using "
        codeEmphasis "go get:"
      bashCommand "go get github.com/apitoolkit/apitoolkit-go"


echoGuide :: Text -> Html ()
echoGuide apikey = do
  section_ [class_ "flex flex-col gap-10 go-guide hidden", id_ "Echo"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium flex items-center gap-1"] do
        "Install the APItoolkit gin SDK using "
        codeEmphasis "go get:"
      bashCommand "go get github.com/apitoolkit/apitoolkit-go"
