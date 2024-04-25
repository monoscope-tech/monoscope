module Pages.IntegrationGuides (getH) where

import Data.Default
import Data.Text
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper
  ( BWConfig (currProject, pageTitle, sessM),
    bodyWrapper,
  )
import Pages.IntegrationDemos.ExpressJs
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext)
import System.Types (ATAuthCtx)

getH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (Html ())
getH pid sdkM errReportM reqMonM = do
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession
  (apiKey, project) <- dbtToEff $ do
    apiKey <- ProjectApiKeys.projectApiKeysByProjectId pid
    project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
    let key = if V.length apiKey > 0 then let defKey = V.head apiKey in defKey.keyPrefix else "<API_KEY>"
    pure (key, project)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Integrations"
          }
  pure $ bodyWrapper bwconf $ integrationsPage pid (fromMaybe "express" sdkM) apiKey errReportM reqMonM

integrationsPage :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Maybe Text -> Html ()
integrationsPage pid sdk apiKey errReportM reqMonM = do
  main_ [class_ "w-full h-full overflow-y-scroll", id_ "main"] do
    div_ [class_ "flex flex-col gap-6 border-b  m-8 pb-4"] do
      div_ [class_ "flex justify-between items-center"] do
        h3_ [class_ "text-3xl font-medium capitalize"] $ toHtml $ "Configure " <> sdk <> " SDK"
        div_ [class_ "flex items-center gap-4"] do
          button_ [class_ "rounded-lg border px-4 py-2"] "See Full Documentation"
      div_ [class_ "flex items-center gap-4"] do
        select_ [name_ "sdk", hxGet_ $ "/p/" <> pid.toText <> "integration_guides", hxTarget_ "#main", class_ "select select-primary select-bordered select-sm w-38 text-md"] do
          option_ [] $ toHtml $ getTitle sdk
          option_ [value_ "gin"] "Go Gin"
          option_ [value_ "express"] "ExpressJs"
          option_ [value_ "pyramid"] "Python Pyramid"
        label_ [class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium text-sm hover:bg-gray-100"] do
          input_ [class_ "check-box", type_ "checkbox"]
          span_ "Request monitoring"
        label_ [class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium text-sm hover:bg-gray-100"] do
          input_ [class_ "check-box", type_ "checkbox"]
          span_ "Error Reporting"
        label_ [class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium text-sm hover:bg-gray-100"] do
          input_ [class_ "check-box", type_ "checkbox"]
          span_ "Outgoing request monitoring"
    div_ [class_ "px-8 mb-10"] do
      expressGuide apiKey errReportM reqMonM
    script_
      [text|
hljs.highlightAll();
 |]

getTitle :: Text -> Text
getTitle "gin" = "GO Gin"
getTitle "pyramid" = "Python Pyramid"
getTitle _ = "Express Js"
