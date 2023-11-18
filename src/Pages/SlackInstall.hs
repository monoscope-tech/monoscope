{-# LANGUAGE OverloadedStrings #-}

module Pages.SlackInstall (getH) where

import Config
import Control.Lens ((.~), (?~), (^.))
import Data.Aeson
import Data.Aeson.QQ
import Data.Text
import Lucid
import Network.Wreq
import Relude


data TokenResponse = TokenResponse
    { ok :: Bool
    , tokenType :: String
    , accessToken :: String
    }
    deriving stock (Show, Generic)


instance FromJSON TokenResponse where
    parseJSON = withObject "TokenResponse" $ \v ->
        TokenResponse
            <$> v
            .: "ok"
            <*> v
            .: "token_type"
            <*> v
            .: "access_token"


exchangeCodeForToken :: Text -> Text -> Text -> Text -> IO (Maybe TokenResponse)
exchangeCodeForToken clientId clientSecret redirectUri code = do
    let formData :: [FormParam]
        formData =
            [ "client_id" := clientId
            , "client_secret" := clientSecret
            , "code" := code
            ]

    let hds = header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=utf-8"]
    response <- postWith (defaults & hds) "https://slack.com/api/oauth.v2.access" formData
    let responseBdy = response ^. responseBody
    traceShowM responseBdy
    case decode responseBdy of
        Just token -> return $ Just token
        Nothing -> return Nothing


getH :: Maybe Text -> DashboardM (Html ())
getH slack_code = do
    envCfg <- asks env
    let client_id = envCfg.slackClientId
    let client_secret = envCfg.slackClientSecret
    let redirect_uri = envCfg.slackRedirectUri
    token <- liftIO $ exchangeCodeForToken client_id client_secret redirect_uri (fromMaybe "" slack_code)

    pure $ slackPage (fromMaybe "" slack_code) token


slackPage :: Text -> Maybe TokenResponse -> Html ()
slackPage sid token = do
    div_ [] do
        h1_ [class_ "text-2xl font-bold"] "Slack Install"
        p_ [class_ "mt-2 text-sm text-gray-600"] $ toHtml sid
        p_ [] $ show token
