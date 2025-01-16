module Pages.IntegrationDemos.Javascript (javascriptGuide) where

import Data.Text qualified as T
import Lucid
import NeatInterpolation (text)
import Pages.IntegrationDemos.ExpressJs (expressGuide, fastifyGuide, nextJsGuide)
import Pkg.Components
import Relude


javascriptGuide :: Text -> Html ()
javascriptGuide apikey = do
  section_ [class_ "flex flex-col gap-4"] do
    span_ [class_ "text-2xl font-semibold text-strong"] "Integrate Javascript SDK"
    div_ [class_ "flex items-center gap-2"] do
      mapM_ featureItem features
    div_ [class_ "flex flex-col gap-2"] do
      span_ [class_ "text-strong font-semibold"] "Select framework"
      div_ [class_ "flex items-center gap-2"] do
        mapM_ frameworkItem frameworks
    div_ [class_ "w-full border-b"] pass
    expressGuide apikey
    fastifyGuide apikey
    nextJsGuide apikey


features :: [Text]
features = ["API Tracking", "Logs", "Traces", "Error Reporting", "Outgoing Request Monitoring"]


frameworks :: [Text]
frameworks = ["Express", "Fastify", "NestJS", "NextJS"]


featureItem :: Text -> Html ()
featureItem title =
  div_ [class_ "h-8 px-3 rounded-lg flex justify-center items-center gap-2 stroke-strong"] $ do
    let featureId = T.replace " " "" title
    input_ [type_ "checkbox", class_ "checkbox checkbox-sm shrink-0", style_ "--chkbg:#000626E5", id_ featureId]
    label_ [class_ "text-center text-[#000833]/60 text-sm font-semibold", Lucid.for_ featureId] $ toHtml title


frameworkItem :: Text -> Html ()
frameworkItem title =
  button_ [class_ "h-8 px-3 rounded-lg flex justify-center items-center gap-2 stroke-strong", term "_" [text|on click add .hidden to <.js-guide/> then remove .hidden from $title|]] $ do
    input_ [type_ "radio", class_ "radio radio-sm hrink-0", name_ "frameworks", style_ "--chkbg:#000626E5", id_ title]
    label_ [class_ "text-center text-[#000833]/60 text-sm font-semibold", Lucid.for_ title] $ toHtml title
