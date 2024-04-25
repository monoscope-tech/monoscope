module Pages.IntegrationGuides (getH) where

import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects
import Pages.BodyWrapper
  ( BWConfig (currProject, pageTitle, sessM),
    bodyWrapper,
  )

getH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> AuthContext (Html ())
getH pid sdkM errReportM reqMonM = do
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession
  (apiKey, project) <- dbtToEff $ do
    apiKey <- ProjectApiKeys.getProjectApiKey pid
    project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
    pure (fromMaybe "" apiKey, project)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Integrations"
          }
  pure $ bodyWrapper bwconf $ integrationsPage pid sdk apiKey errReportM reqMonM

integrationsPage :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Maybe Text -> Html ()
integrationsPage pid sdk apiKey errReportM reqMonM = do
  main_ [class_ "w-full"] do
    div_ [] do
      div_ [] do
        h3_ [] $ "Configure " <> sdk <> " SDK"
        div_ [] do
          button_ [] "Back to Dashboard"
          button_ [] "Full documentation"
      div_ [] do
        button_ [] "Select SDK"
        button_ [] "Request monitoring"
        button_ [] "Outgoing request monitoring"
        button_ [] "Error Reporting"
