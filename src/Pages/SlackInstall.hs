{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.SlackInstall (linkProjectGetH, postH, LinkProjectsForm, updateWebHook, SlackLink) where

import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.Default (Default (def))
import Database.PostgreSQL.Entity.DBT (withPool)
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask, asks)
import Lucid
import Models.Apis.Slack (insertAccessToken)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Network.Wreq (FormParam (..), defaults, header, postWith, responseBody)
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pkg.Components (navBar)
import Pkg.Mail (sendSlackMessage)
import Relude hiding (ask, asks)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import System.Config (AuthContext (env, pool), EnvConfig (slackClientId, slackClientSecret, slackRedirectUri))
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)


data LinkProjectsForm = LinkProjectsForm
  { projects :: [Text]
  , webhookUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


data IncomingWebhook = IncomingWebhook
  { channel :: Text
  , channelId :: Text
  , configurationUrl :: Text
  , url :: Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] IncomingWebhook


data TokenResponse = TokenResponse
  { ok :: Bool
  , incomingWebhook :: IncomingWebhook
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] TokenResponse


exchangeCodeForToken :: Text -> Text -> Text -> Text -> IO (Maybe TokenResponse)
exchangeCodeForToken clientId clientSecret redirectUri code = do
  let formData :: [FormParam]
      formData =
        [ "client_id" := clientId
        , "client_secret" := clientSecret
        , "code" := code
        , "redirect_uri" := redirectUri
        ]

  let hds = header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=utf-8"]
  response <- postWith (defaults & hds) "https://slack.com/api/oauth.v2.access" formData
  let responseBdy = response ^. responseBody
  case AE.decode responseBdy of
    Just token -> do
      return $ Just token
    Nothing -> return Nothing


updateWebHook :: Projects.ProjectId -> LinkProjectsForm -> ATAuthCtx (RespHeaders (Html ()))
updateWebHook pid LinkProjectsForm{projects, webhookUrl} = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession

  _ <- dbtToEff $ insertAccessToken [pid.toText] webhookUrl
  addSuccessToast "Webhook url updated successfully" Nothing
  addRespHeaders $ span_ [] "Projects linked successfully"


postH :: LinkProjectsForm -> ATAuthCtx (RespHeaders (Html ()))
postH LinkProjectsForm{projects, webhookUrl} = do
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession

  _ <- dbtToEff $ insertAccessToken projects webhookUrl
  addSuccessToast "Slack account linked to project(s), successfully" Nothing
  addRespHeaders $ span_ [] "Projects linked successfully"


linkProjectGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] SlackLink)
linkProjectGetH pid slack_code onboardingM = do
  envCfg <- asks env
  pool <- asks pool
  let client_id = envCfg.slackClientId
  let client_secret = envCfg.slackClientSecret
  let redirect_uri = envCfg.slackRedirectUri
  token <- liftIO $ exchangeCodeForToken client_id client_secret (redirect_uri <> pid.toText <> if isJust onboardingM then "?onboarding=true" else "") (fromMaybe "" slack_code)
  let bwconf =
        (def :: BWConfig)
          { sessM = Nothing
          , currProject = Nothing
          , pageTitle = "Slack app installed"
          }
  project <- liftIO $ withPool pool $ Projects.projectById pid
  case (token, project) of
    (Just token', Just project') -> do
      n <- liftIO $ withPool pool do
        insertAccessToken [pid.toText] token'.incomingWebhook.url
      sendSlackMessage pid ("APItoolkit Bot has been linked to your project: " <> project'.title)
      case onboardingM of
        Just _ -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
        Nothing -> pure $ addHeader "" $ SlackLinked $ PageCtx bwconf ()
    (_, _) -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoTokenFound $ PageCtx bwconf ()


data SlackLink
  = SlackLinked (PageCtx ())
  | NoTokenFound (PageCtx ())
  | NoContent (PageCtx ())


instance ToHtml SlackLink where
  toHtml (SlackLinked (PageCtx bwconf ())) = toHtml $ PageCtx bwconf installedSuccess
  toHtml (NoTokenFound (PageCtx bwconf ())) = toHtml $ PageCtx bwconf noTokenFound
  toHtml (NoContent (PageCtx bwconf ())) = toHtml $ PageCtx bwconf ""
  toHtmlRaw = toHtml


noTokenFound :: Html ()
noTokenFound = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    h3_ [class_ "text-5xl font-semibold my-8"] "Token Not Found"
    p_ [class_ "text-2xl"] "No slack access token found, reinstall the APItoolkit slack app to try again."


installedSuccess :: Html ()
installedSuccess = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    div_ [class_ "flex flex-col border px-6 py-16 mt-16 rounded-2xl items-center"] do
      faSprite_ "check" "regular" "h-10 w-10 text-green-500"
      h3_ [class_ "text-3xl font-semibold my-8"] "APItoolkit Slack App Installed"
      p_ [class_ "text-gray-600 text-center max-w-prose"] "APItoolkit Bot Slack app has been connected to your project successfully. You can now recieve notifications on slack."
