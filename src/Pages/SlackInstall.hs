module Pages.SlackInstall (getH) where

import Config
import Data.Aeson
import Data.Text
import Lucid
import Relude
import Servant


slackOAuthAPI :: String
slackOAuthAPI = "https://slack.com/api/oauth.v2.access"


getH :: Maybe Text -> DashboardM (Html ())
getH sid = do
    pure $ slackPage (fromMaybe "" sid)


slackPage :: Text -> Html ()
slackPage sid = do
    div_ [] do
        h1_ [class_ "text-2xl font-bold"] "Slack Install"
        p_ [class_ "mt-2 text-sm text-gray-600"] $ toHtml sid

-- Handle the response and extract the access token
