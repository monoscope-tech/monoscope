{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.SlackInstall (getH, linkProjectsGetH, postH, LinkProjectsForm, updateWebHook) where

import Config
import Control.Lens ((.~), (^.))
import Data.Aeson
import Data.Default
import Data.Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Fmt (dateDashF, fmt)
import Lucid

import Data.Aeson.QQ
import Database.PostgreSQL.Simple (Only (Only))
import Lucid.Htmx (hxPost_)
import Models.Apis.Slack (SlackData (webhookUrl), insertAccessToken)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Session
import Network.Wreq
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Pkg.Components (navBar)
import Relude
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)
import Web.FormUrlEncoded (FromForm)


data LinkProjectsForm = LinkProjectsForm
    { projects :: [Text]
    , webhookUrl :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromForm)


data IncomingWebhook = IncomingWebhook
    { channel :: String
    , channelId :: String
    , configurationUrl :: String
    , url :: String
    }
    deriving stock (Show, Generic)


instance FromJSON IncomingWebhook where
    parseJSON = withObject "IncomingWebhook" $ \v ->
        IncomingWebhook
            <$> v
            .: "channel"
            <*> v
            .: "channel_id"
            <*> v
            .: "configuration_url"
            <*> v
            .: "url"


data TokenResponse = TokenResponse
    { ok :: Bool
    , incomingWebhook :: IncomingWebhook
    }
    deriving stock (Show, Generic)


instance FromJSON TokenResponse where
    parseJSON = withObject "TokenResponse" $ \v ->
        TokenResponse
            <$> v
            .: "ok"
            <*> v
            .: "incoming_webhook"


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
        Just token -> do
            return $ Just token
        Nothing -> return Nothing


updateWebHook :: Session.PersistentSession -> Projects.ProjectId -> LinkProjectsForm -> DashboardM (Headers '[HXTrigger] (Html ()))
updateWebHook sess pid LinkProjectsForm{projects, webhookUrl} = do
    pool <- asks pool
    _ <- liftIO $ withPool pool $ insertAccessToken [pid.toText] webhookUrl
    let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Webhook url updated successfully"]} |]
    pure $ addHeader hxTriggerData $ span_ [] "Projects linked successfully"


postH :: Session.PersistentSession -> LinkProjectsForm -> DashboardM (Headers '[HXTrigger] (Html ()))
postH sess LinkProjectsForm{projects, webhookUrl} = do
    pool <- asks pool
    let q = [sql| update projects.projects set notifications_channel = 'slack' where id=ANY(?::uuid[])|]
    n <- liftIO $ withPool pool do
        _ <- insertAccessToken projects webhookUrl
        execute Update q (Only $ Vector.fromList projects)
    let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"successToast": ["Slack account linked to project(s),successfully"]} |]
    pure $ addHeader hxTriggerData $ span_ [] "Projects linked successfully"


getH :: Maybe Text -> DashboardM (Html ())
getH slack_code = do
    let bwconf =
            (def :: BWConfig)
                { sessM = Nothing
                , currProject = Nothing
                , pageTitle = "Slack Install"
                }
    pure $ bodyWrapper bwconf (maybe noTokenFound toLinkPage slack_code)


toLinkPage :: Text -> Html ()
toLinkPage code = do
    navBar
    section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col gap-6 items-center mx-auto"] do
        div_ [class_ "mt-10"] do
            h1_ [class_ "text-2xl font-bold"] "Link Slack Account to a project to complete"
        section_ [] do
            div_ [class_ "bg-white flex flex-col items-center sm:rounded-md"] do
                h3_ [class_ "mb-6"] "Make sure you are logged in"
                a_ [href_ $ "http://localhost:8080/slack/link-projects?code=" <> code, class_ "btn btn-primary"] "Link a project(s)"


linkProjectsGetH :: Session.PersistentSession -> Maybe Text -> DashboardM (Html ())
linkProjectsGetH sess slack_code = do
    envCfg <- asks env
    pool <- asks pool
    projects <- liftIO $ withPool pool $ Projects.selectProjectsForUser sess.userId
    let client_id = envCfg.slackClientId
    let client_secret = envCfg.slackClientSecret
    let redirect_uri = envCfg.slackRedirectUri
    token <- liftIO $ exchangeCodeForToken client_id client_secret redirect_uri (fromMaybe "" slack_code)
    let bwconf =
            (def :: BWConfig)
                { sessM = Just sess
                , currProject = Nothing
                , pageTitle = "Link a project"
                }

    pure $ bodyWrapper bwconf (maybe noTokenFound (slackPage projects) token)


slackPage :: Vector Projects.Project' -> TokenResponse -> Html ()
slackPage projects token = do
    main_ [class_ "w-[1000px] flex flex-col mt-8 items-center mx-auto"] do
        h1_ [class_ "text-2xl font-bold"] "Link Slack Account to a project to complete"
        section_ [class_ "w-full"] do
            div_ [class_ "bg-white shadow overflow-hidden sm:rounded-md"] do
                form_ [hxPost_ "/slack/link-projects"] do
                    input_ [type_ "hidden", name_ "webhookUrl", value_ $ toText token.incomingWebhook.url]
                    ul_ [role_ "list", class_ "divide-y divide-gray-200"] do
                        projects & mapM_ \project -> do
                            li_ do
                                label_ [class_ "block", Lucid.for_ project.id.toText] do
                                    div_ [class_ "px-4 py-4 flex items-center sm:px-6"] do
                                        div_ [class_ "min-w-0 flex-1 sm:flex sm:items-center sm:justify-between"] do
                                            div_ [class_ "truncate"] do
                                                div_ [class_ "text-sm"] do
                                                    p_ [class_ "block font-medium text-indigo-600 truncate py-2"] $ toHtml project.title
                                                    p_ [class_ "block flex-shrink-0 font-normal text-gray-500"] $ toHtml project.description
                                                div_ [class_ "mt-2 flex"] do
                                                    div_ [class_ "flex items-center text-sm text-gray-500"] do
                                                        small_ do
                                                            span_ "Created on "
                                                            time_ [datetime_ $ fmt $ dateDashF project.createdAt] $ toHtml @Text $ fmt $ dateDashF project.createdAt
                                        div_ [class_ "ml-5 flex-shrink-0 text-gray-400"] do
                                            input_ [type_ "checkbox", id_ project.id.toText, name_ "projects", value_ project.id.toText]
                        button_ [class_ "mx-2 mb-2 mt-6 btn btn-primary"] "Link Projects"


noTokenFound :: Html ()
noTokenFound = do
    navBar
    section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
        h3_ [class_ "text-5xl font-semibold my-8"] "Token Not Found"
        p_ [class_ "text-2xl"] "No slack access token found, reinstall the APIToolkit slack app to try again."
