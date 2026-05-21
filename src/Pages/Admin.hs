{-# LANGUAGE OverloadedStrings #-}

-- | Instance-wide admin settings (visible to sudo users only).
--
-- Lets the admin configure SMTP credentials and the Telegram bot token
-- from the web UI. Values are persisted in @system.app_config@ and read
-- by the notification dispatcher with env-var fallback.
module Pages.Admin
  ( AdminSettingsGet (..)
  , adminSettingsGetH
  , AdminSmtpForm (..)
  , adminSmtpPostH
  , AdminTelegramForm (..)
  , adminTelegramPostH
  ) where

import Data.Default (def)
import Data.Text qualified as T
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static qualified as Reader
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.System.AppConfig qualified as AppCfg
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (settingsH2_, settingsSection_)
import Relude
import Servant (NoContent (..))
import Servant.Server (ServerError (..), err403)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast, redirectCS)
import Web.FormUrlEncoded (FromForm)
import Web.I18n qualified as I18n


-- * GET /admin/settings


newtype AdminSettingsGet = AdminSettingsGet (PageCtx (Html ()))
  deriving stock (Generic)


instance ToHtml AdminSettingsGet where
  toHtml (AdminSettingsGet pgctx) = toHtml pgctx
  toHtmlRaw = toHtml


adminSettingsGetH :: ATAuthCtx (RespHeaders AdminSettingsGet)
adminSettingsGetH = do
  sess <- Projects.getSession
  unless sess.user.isSudo $ throwError err403{errBody = "Admin only"}
  appCtx <- Reader.ask @AuthContext
  smtp <- AppCfg.getSmtpConfig
  tg <- AppCfg.getTelegramConfig
  let lang = sess.lang
      env = appCtx.env
      bw = (def :: BWConfig){pageTitle = I18n.t lang "admin.settings.title", config = env, sessM = Just sess, hideNavbar = False, isSettingsPage = True}
  addRespHeaders $ AdminSettingsGet $ PageCtx bw $ adminSettingsBody lang env smtp tg


adminSettingsBody :: I18n.Language -> EnvConfig -> Maybe AppCfg.SmtpConfig -> Maybe AppCfg.TelegramConfig -> Html ()
adminSettingsBody lang env smtp tg = settingsSection_ $ div_ [class_ "max-w-3xl mx-auto p-6 space-y-10"] do
  -- Header
  div_ [class_ "space-y-1"] do
    settingsH2_ $ I18n.t lang "admin.settings.title"
    p_ [class_ "text-sm text-textWeak"] $ toHtml $ I18n.t lang "admin.settings.subtitle"

  -- SMTP card
  section_ [class_ "panel border border-strokeWeak rounded-lg p-6 space-y-4 bg-bgRaised"] do
    h3_ [class_ "text-lg font-semibold text-textStrong"] $ toHtml $ I18n.t lang "admin.smtp.title"
    p_ [class_ "text-sm text-textWeak"] $ toHtml $ I18n.t lang "admin.smtp.desc"
    smtpForm lang env smtp

  -- Telegram card
  section_ [class_ "panel border border-strokeWeak rounded-lg p-6 space-y-4 bg-bgRaised"] do
    h3_ [class_ "text-lg font-semibold text-textStrong"] $ toHtml $ I18n.t lang "admin.telegram.title"
    p_ [class_ "text-sm text-textWeak"] $ toHtml $ I18n.t lang "admin.telegram.desc"
    telegramForm lang env tg


smtpForm :: I18n.Language -> EnvConfig -> Maybe AppCfg.SmtpConfig -> Html ()
smtpForm lang env mc = do
  let val sel envFallback = case mc of
        Just c | not (T.null (sel c)) -> sel c
        _ -> envFallback
      _txt = id @Text -- type hint helper, unused-suppressor
  form_ [method_ "post", action_ "/admin/settings/smtp", class_ "grid grid-cols-1 md:grid-cols-2 gap-4"] do
    field "host" (I18n.t lang "admin.smtp.host") "text" (val (.host) env.smtpHost) "smtp.gmail.com"
    field "port" (I18n.t lang "admin.smtp.port") "number" (val (show . (.port)) (show env.smtpPort)) "587"
    selectField "tls" (I18n.t lang "admin.smtp.tls") (val (boolText . (.tls)) (boolText env.smtpTls))
      [("True", "TLS"), ("False", "STARTTLS")]
    field "sender" (I18n.t lang "admin.smtp.sender") "email" (val (.sender) env.smtpSender) "notifications@example.com"
    field "username" (I18n.t lang "admin.smtp.username") "text" (val (.username) env.smtpUsername) "user@example.com"
    passwordField "password" (I18n.t lang "admin.smtp.password") (I18n.t lang "admin.smtp.password_help")
    div_ [class_ "md:col-span-2 flex items-center gap-3"] do
      button_ [type_ "submit", class_ "btn btn-primary"] $ toHtml $ I18n.t lang "common.save"
  where
    boolText :: Bool -> Text
    boolText True = "True"
    boolText False = "False"


telegramForm :: I18n.Language -> EnvConfig -> Maybe AppCfg.TelegramConfig -> Html ()
telegramForm lang env mc = do
  let valTok = case mc of
        Just c | not (T.null c.botToken) -> c.botToken
        _ -> env.telegramBotToken
      valDesc = maybe "" (.description) mc
  form_ [method_ "post", action_ "/admin/settings/telegram", class_ "space-y-4"] do
    field "botToken" (I18n.t lang "admin.telegram.bot_token") "text" valTok "1234567890:ABC-DEF…"
    p_ [class_ "text-xs text-textWeak"] $ toHtml $ I18n.t lang "admin.telegram.bot_token_help"
    field "description" (I18n.t lang "admin.telegram.description") "text" valDesc "Monoscope alerts bot"
    div_ [class_ "flex items-center gap-3"] do
      button_ [type_ "submit", class_ "btn btn-primary"] $ toHtml $ I18n.t lang "common.save"


field :: Text -> Text -> Text -> Text -> Text -> Html ()
field name lbl ty val placeholder = label_ [class_ "form-control w-full"] do
  div_ [class_ "label"] $ span_ [class_ "label-text"] $ toHtml lbl
  input_ [type_ ty, name_ name, value_ val, placeholder_ placeholder, class_ "input input-bordered w-full"]


passwordField :: Text -> Text -> Text -> Html ()
passwordField name lbl helpText = label_ [class_ "form-control w-full"] do
  div_ [class_ "label"] $ span_ [class_ "label-text"] $ toHtml lbl
  input_ [type_ "password", name_ name, autocomplete_ "new-password", placeholder_ "••••••••", class_ "input input-bordered w-full"]
  p_ [class_ "text-xs text-textWeak mt-1"] $ toHtml helpText


selectField :: Text -> Text -> Text -> [(Text, Text)] -> Html ()
selectField name lbl selected opts = label_ [class_ "form-control w-full"] do
  div_ [class_ "label"] $ span_ [class_ "label-text"] $ toHtml lbl
  select_ [name_ name, class_ "select select-bordered w-full"] do
    forM_ opts $ \(v, d) ->
      option_ (value_ v : [selected_ "selected" | v == selected]) (toHtml d)


-- * POST /admin/settings/smtp


data AdminSmtpForm = AdminSmtpForm
  { host :: Text
  , port :: Maybe Int
  , tls :: Maybe Text
  , username :: Text
  , password :: Text
  , sender :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


adminSmtpPostH :: AdminSmtpForm -> ATAuthCtx (RespHeaders NoContent)
adminSmtpPostH form = do
  sess <- Projects.getSession
  unless sess.user.isSudo $ throwError err403{errBody = "Admin only"}
  let lang = sess.lang
  -- Empty password = "don't change it". We re-read what's already stored
  -- so a save without touching the password field doesn't blow it away.
  prev <- AppCfg.getSmtpConfig
  let finalPw
        | not (T.null form.password) = form.password
        | otherwise = maybe "" (.password) prev
      tls = case fromMaybe "" form.tls of
        "True" -> True
        _ -> False
      cfg =
        AppCfg.SmtpConfig
          { host = T.strip form.host
          , port = fromMaybe 587 form.port
          , tls
          , username = T.strip form.username
          , password = finalPw
          , sender = T.strip form.sender
          }
  AppCfg.setSmtpConfig cfg
  addSuccessToast (I18n.t lang "admin.smtp.saved") Nothing
  redirectCS "/admin/settings"
  addRespHeaders NoContent


-- * POST /admin/settings/telegram


data AdminTelegramForm = AdminTelegramForm
  { botToken :: Text
  , description :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


adminTelegramPostH :: AdminTelegramForm -> ATAuthCtx (RespHeaders NoContent)
adminTelegramPostH form = do
  sess <- Projects.getSession
  unless sess.user.isSudo $ throwError err403{errBody = "Admin only"}
  let lang = sess.lang
      cfg = AppCfg.TelegramConfig{botToken = T.strip form.botToken, description = T.strip form.description}
  AppCfg.setTelegramConfig cfg
  addSuccessToast (I18n.t lang "admin.telegram.saved") Nothing
  redirectCS "/admin/settings"
  addRespHeaders NoContent
