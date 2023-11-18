{-# LANGUAGE OverloadedStrings #-}

module Pages.SlackInstall (getH) where

import Config
import Control.Lens ((.~), (^.))
import Data.Aeson
import Data.Default
import Data.Text
import Lucid
import Network.Wreq
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Pkg.Components (navBar)
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
    let bwconf =
            (def :: BWConfig)
                { sessM = Nothing
                , currProject = Nothing
                , pageTitle = "Slack Install"
                }
    pure $ bodyWrapper bwconf (maybe noTokenFound slackPage token)


slackPage :: TokenResponse -> Html ()
slackPage token = do
    navBar
    div_ [] do
        h1_ [class_ "text-2xl font-bold"] "Select a project to link slack with"
        p_ [] $ show token


noTokenFound :: Html ()
noTokenFound = do
    navBar
    section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
        h3_ [class_ "text-5xl font-semibold my-8"] "Token Not Found"
        p_ [class_ "text-2xl"] "No slack access token found, reinstall the APIToolkit slack app to try again."
