module Pkg.EmailTemplates (
  -- * Rendering
  renderEmail,
  emailWrapper,
  emailBody,

  -- * Templates
  projectInviteEmail,
  projectCreatedEmail,
  projectDeletedEmail,
  runtimeErrorsEmail,
  escalatingErrorsEmail,
  regressedErrorsEmail,
  errorSpikesEmail,
  anomalyEndpointEmail,
  issueAssignedEmail,
  weeklyReportEmail,
  WeeklyReportData (..),
  monitorAlertEmail,
  monitorRecoveryEmail,
  freeTierUsageEmail,
  planUpgradedEmail,
  planDowngradedEmail,

  -- * Sample data for previews
  sampleProjectInvite,
  sampleProjectCreated,
  sampleProjectDeleted,
  sampleRuntimeErrors,
  sampleAnomalyEndpoint,
  sampleIssueAssigned,
  sampleWeeklyReport,
) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Time (formatTime)
import Data.Time.Format (defaultTimeLocale)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Lucid
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import Utils (formatWithCommas)


cellpadding_, cellspacing_, align_ :: Text -> Attribute
cellpadding_ = term "cellpadding"
cellspacing_ = term "cellspacing"
align_ = term "align"


-- | Render a template to HTML text
renderEmail :: Text -> Html () -> Text
renderEmail subject content = toStrict $ renderText $ emailWrapper subject content


-- =============================================================================
-- Shared Email Layout
-- =============================================================================

emailCss :: Text
emailCss =
  """
  .monoscope-email { width: 100% !important; height: 100%; margin: 0; -webkit-text-size-adjust: none; color: #24292f; }
  .monoscope-email a { color: #377cfb; text-decoration: none; }
  .monoscope-email a:hover { text-decoration: underline; }
  .monoscope-email a img { border: none; }
  .monoscope-email td { word-break: break-word; }
  .monoscope-email, .monoscope-email td, .monoscope-email th { font-family: Inter, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen, Ubuntu, Cantarell, "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif; }
  .monoscope-email h1 { color: #24292f; font-size: 24px; font-weight: 600; margin: 0 0 16px; letter-spacing: -0.3px; }
  .monoscope-email h2 { color: #24292f; font-size: 20px; font-weight: 600; margin: 32px 0 12px; letter-spacing: -0.2px; }
  .monoscope-email h3 { color: #24292f; font-size: 18px; font-weight: 600; margin: 24px 0 10px; letter-spacing: -0.2px; }
  .monoscope-email td, .monoscope-email th { font-size: 16px; }
  .monoscope-email p { margin: 0 0 12px; font-size: 16px; font-weight: 400; line-height: 24px; color: #24292f; letter-spacing: -0.2px; }
  .monoscope-email p:last-child { margin-bottom: 0; }
  .monoscope-email ul, .monoscope-email ol { color: #24292f; list-style-position: outside; padding-left: 20px; margin: 0 0 16px; }
  .monoscope-email li { padding: 0 0 10px 5px; line-height: 24px; }
  .monoscope-email p.sub { font-size: 13px; color: #99a2af; }
  .monoscope-email .email-wrapper { width: 100%; margin: 0; padding: 0; }
  .monoscope-email .email-content { width: 100%; margin: 0; padding: 0; }
  .monoscope-email .email-masthead { padding: 40px 0; text-align: center; }
  .monoscope-email .email-masthead_logo { width: 160px; height: auto; }
  .monoscope-email .email-body { width: 100%; margin: 0; padding: 0; }
  .monoscope-email .email-body_inner { width: 100%; max-width: 600px; margin: 0 auto; padding: 0; }
  .monoscope-email .content-cell { padding: 0 20px 40px; }
  .monoscope-email img { max-width: 100%; height: auto; }
  .monoscope-email .content-cell img { max-width: 100%; height: auto; display: block; }
  .monoscope-email .content-image { width: 100%; max-width: 100%; height: auto; border-radius: 12px; margin: 16px 0 24px; display: block; }
  .monoscope-email .feature-image { width: 100%; max-width: 100%; height: auto; border-radius: 16px; border: 1px solid #dee2e7; margin: 0 0 24px; display: block; }
  .monoscope-email .email-footer { width: 100%; max-width: 600px; margin: 0 auto; padding: 0; }
  .monoscope-email .email-footer p { font-family: Ubuntu, Helvetica, Arial, sans-serif; color: #99a2af; font-size: 12px; line-height: 16px; text-align: left; margin: 0 0 10px; }
  .monoscope-email .email-footer a { color: #99a2af; text-decoration: none; }
  .monoscope-email .email-footer a:hover { text-decoration: underline; }
  .monoscope-email .footer-links { font-family: Ubuntu, Helvetica, Arial, sans-serif; font-size: 12px; line-height: 20px; text-align: center; color: #99a2af; }
  .monoscope-email .footer-links a { color: #99a2af; text-decoration: none; }
  .monoscope-email .divider { border: none; border-top: 1px solid #dee2e7; margin: 32px 0; }
  .monoscope-email .button { display: inline-block; background-color: #1d1e20; color: #ffffff !important; font-size: 16px; font-weight: 500; text-decoration: none; padding: 14px 28px; border-radius: 12px; border: 1px solid #505967; letter-spacing: -0.2px; }
  .monoscope-email .button:hover { text-decoration: none; background-color: #2d2e30; }
  .monoscope-email .button-wrapper { text-align: center; margin: 32px 0; }
  .monoscope-email .monoscope-code { background-color: #f6f8fa; border: 1px solid #dee2e7; border-radius: 6px; padding: 2px 6px; font-family: ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace; font-size: 14px; }
  .monoscope-email .highlight-box { background-color: #f6f8fa; border-radius: 12px; padding: 20px 24px; margin: 24px 0; }
  .monoscope-email .highlight-box p { margin: 0; color: #57606a; font-size: 15px; }
  .monoscope-email .error-card { border: 1px solid #dee2e7; border-radius: 12px; margin: 16px 0; }
  .monoscope-email .error-card-header { color: #cf222e; font-size: 16px; font-weight: 600; margin: 0 0 4px; word-break: break-word; }
  .monoscope-email .error-card-sub { color: #57606a; font-size: 13px; margin: 0; }
  .monoscope-email .error-card-meta { font-size: 13px; color: #57606a; }
  .monoscope-email .error-card-label { font-weight: 600; color: #24292f; }
  .monoscope-email .error-card-stack { background-color: #1d1e20; color: #e2e8f0; border-radius: 8px; padding: 12px 16px; font-family: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace; font-size: 12px; line-height: 1.5; white-space: pre-wrap; overflow-x: auto; }
  .monoscope-email .social-icons { text-align: center; margin: 20px 0; }
  .monoscope-email .social-icons a { display: inline-block; vertical-align: middle; padding: 0 4px; }
  .monoscope-email .social-icons img { width: 17px; height: 17px; border-radius: 3px; display: block; }
  .monoscope-email .report-table { margin: 20px 0 32px 0 !important; border: 1px solid #dee2e7 !important; border-collapse: collapse !important; width: 100% !important; }
  .monoscope-email .report-table th { background-color: #f6f8fa !important; color: #24292f !important; padding: 12px 15px !important; text-align: left !important; border: 1px solid #dee2e7 !important; font-size: 14px !important; font-weight: 600 !important; }
  .monoscope-email .report-table td { font-weight: 400 !important; color: #24292f !important; padding: 16px 15px !important; text-align: left !important; border: 1px solid #dee2e7 !important; font-size: 14px !important; }
  @media only screen and (max-width: 600px) {
    .monoscope-email .email-body_inner, .monoscope-email .email-footer { width: 100% !important; }
    .monoscope-email .content-cell { padding: 0 20px 30px !important; }
    .monoscope-email .email-masthead { padding: 30px 0 !important; }
    .monoscope-email h1 { font-size: 22px !important; }
    .monoscope-email h2 { font-size: 18px !important; }
    .monoscope-email .footer-row { display: block !important; width: 100% !important; }
    .monoscope-email .footer-col-left, .monoscope-email .footer-col-right { display: block !important; width: 100% !important; text-align: left !important; padding: 0 !important; }
    .monoscope-email .footer-col-right { padding-top: 16px !important; }
    .monoscope-email .footer-icons { float: none !important; }
    .monoscope-email .report-table th:nth-child(n+2), .monoscope-email .report-table td:nth-child(n+2) { display: none !important; }
    .monoscope-email .stat-number { font-size: 22px !important; white-space: nowrap !important; }
  }
  @media (prefers-color-scheme: dark) {
    .monoscope-email p, .monoscope-email ul, .monoscope-email ol, .monoscope-email li, .monoscope-email h1, .monoscope-email h2, .monoscope-email h3 { color: #ffffff !important; }
    .monoscope-email .divider { border-top-color: #333333 !important; }
    .monoscope-email .monoscope-code { background-color: #1a1a1a !important; border-color: #333333 !important; color: #ffffff !important; }
    .monoscope-email .highlight-box { background-color: #1a1a1a !important; }
    .monoscope-email .highlight-box p { color: #aaaaaa !important; }
    .monoscope-email .error-card { border-color: #333333 !important; background-color: transparent !important; }
    .monoscope-email .error-card-header { color: #f87171 !important; }
    .monoscope-email .error-card-sub { color: #aaaaaa !important; }
    .monoscope-email .error-card-meta { color: #aaaaaa !important; }
    .monoscope-email .error-card-label { color: #ffffff !important; }
    .monoscope-email .feature-image { border-color: #333333 !important; }
    .monoscope-email .email-footer p, .monoscope-email .email-footer a { color: #666666 !important; }
    .monoscope-email .report-table th { background-color: #1a1a1a !important; color: #ffffff !important; border-color: #333333 !important; }
    .monoscope-email .report-table td { color: #e0e0e0 !important; border-color: #333333 !important; }
    .monoscope-email .bar-track { background-color: #333333 !important; }
    .monoscope-email .bar-legend { color: #aaaaaa !important; }
    .monoscope-email td { color: #e0e0e0 !important; }
  }
  :root { color-scheme: light dark; supported-color-schemes: light dark; }
  """


emailWrapper :: Text -> Html () -> Html ()
emailWrapper subject content = doctypehtml_ do
  head_ do
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=UTF-8"]
    meta_ [name_ "color-scheme", content_ "light dark"]
    meta_ [name_ "supported-color-schemes", content_ "light dark"]
    title_ $ toHtml subject
    style_ emailCss
    toHtmlRaw @Text "<!--[if mso]><style type=\"text/css\">.monoscope-email,.monoscope-email td,.monoscope-email th{font-family:Arial,sans-serif !important;}</style><![endif]-->"
  body_ [class_ "monoscope-email"] do
    table_ [class_ "email-wrapper", width_ "100%", cellpadding_ "0", cellspacing_ "0", role_ "presentation"]
      $ tr_
      $ td_ [align_ "center"]
      $ table_ [class_ "email-content", width_ "100%", cellpadding_ "0", cellspacing_ "0", role_ "presentation"] do
        -- Masthead
        tr_
          $ td_ [class_ "email-masthead", align_ "center"]
          $ a_ [href_ "https://monoscope.tech?utm_source=transac_emails"]
          $ img_ [class_ "email-masthead_logo", src_ "https://monoscope.tech/assets/email/full_logo_l.png", alt_ "Monoscope", width_ "160", style_ "width: 160px; height: auto;"]
        -- Body
        tr_ $ td_ [class_ "email-body", width_ "100%"] content
        -- Footer
        tr_ $ td_ $ table_ [class_ "email-footer", align_ "center", width_ "600", cellpadding_ "0", cellspacing_ "0", role_ "presentation"] do
          -- Divider
          tr_ $ td_ [style_ "padding: 0 20px;"] $ p_ [style_ "border-top: 1px solid #dee2e7; font-size: 1px; margin: 0 0 19px; width: 100%;"] ""
          -- Description + social icons
          tr_
            $ td_ [style_ "padding: 0 20px 20px;"]
            $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0", role_ "presentation"]
            $ tr_ [class_ "footer-row"] do
              td_ [class_ "footer-col-left", style_ "vertical-align: top; width: 65%;"] do
                p_ [style_ "font-family: Ubuntu, Helvetica, Arial, sans-serif; font-size: 12px; line-height: 16px; text-align: left; color: #99a2af; margin: 0 0 10px;"] "Monoscope \8212 monitoring and observability, built to know what\8217s happening the moment it happens. Logs, metrics, traces, and API payload monitoring with unlimited retention."
                p_ [style_ "font-family: Ubuntu, Helvetica, Arial, sans-serif; font-size: 12px; line-height: 16px; text-align: left; color: #99a2af; margin: 0;"] "\169 2026 Monoscope."
              td_ [class_ "footer-col-right", style_ "vertical-align: top; text-align: right; width: 35%;"]
                $ table_ [class_ "footer-icons", align_ "right", cellpadding_ "0", cellspacing_ "0", role_ "presentation"]
                $ tr_ do
                  td_ [style_ "vertical-align: middle; padding: 0;"]
                    $ a_ [href_ "https://x.com/monoscope_tech", target_ "_blank"]
                    $ img_ [alt_ "X", width_ "17", height_ "17", src_ "https://userimg-assets.customeriomail.com/images/client-env-146107/1745317233071_x_01JSEG70G3MSFPMCXP6XF0G06X.png", style_ "display: block; border-radius: 3px; border: 0;"]
                  td_ [style_ "vertical-align: middle; padding: 0 0 0 8px;"]
                    $ a_ [href_ "https://www.linkedin.com/company/89803535/", target_ "_blank"]
                    $ img_ [alt_ "LinkedIn", width_ "17", height_ "17", src_ "https://userimg-assets.customeriomail.com/images/client-env-145828/1713246315947_LinkedinIcon_01HVJQ09FBW9AHPM72CRERJ7F8.png", style_ "display: block; border-radius: 3px; border: 0;"]
          -- Footer links
          tr_
            $ td_ [align_ "center", style_ "padding: 0 20px 19px;"]
            $ p_ [style_ "font-family: Ubuntu, Helvetica, Arial, sans-serif; font-size: 12px; line-height: 20px; text-align: center; color: #99a2af; margin: 0;"] do
              a_ [href_ "https://monoscope.tech/changelog?utm_source=transac_emails", style_ "color: #99a2af; text-decoration: none;"] "Changelog"
              toHtmlRaw @Text " &nbsp;\183&nbsp; "
              a_ [href_ "https://monoscope.tech/docs?utm_source=transac_emails", style_ "color: #99a2af; text-decoration: none;"] "Docs"
              toHtmlRaw @Text " &nbsp;\183&nbsp; "
              a_ [href_ "https://monoscope.tech/legal/privacy?utm_source=transac_emails", style_ "color: #99a2af; text-decoration: none;"] "Privacy"


emailBody :: Html () -> Html ()
emailBody content =
  table_ [class_ "email-body_inner", align_ "center", width_ "600", cellpadding_ "0", cellspacing_ "0", role_ "presentation"]
    $ tr_
    $ td_ [class_ "content-cell"] content


emailButton :: Text -> Text -> Html ()
emailButton url label =
  div_ [class_ "button-wrapper"] $ a_ [href_ url, class_ "button", target_ "_blank"] $ toHtml label


emailDivider :: Html ()
emailDivider = hr_ [class_ "divider"]


metaCell :: Text -> Text -> Html ()
metaCell label val = td_ [width_ "50%", style_ "padding-bottom: 10px;"]
  $ span_ [class_ "error-card-meta"] do
    b_ [class_ "error-card-label"] $ toHtml @Text label
    " "
    toHtml @Text val


emailHelpLinks :: Html ()
emailHelpLinks = p_ do
  "Need help getting started? Check out our "
  a_ [href_ "https://monoscope.tech/docs/onboarding?utm_source=transac_emails"] "onboarding guide"
  " and other "
  a_ [href_ "https://monoscope.tech/docs?utm_source=transac_emails"] "documentation"
  " resources. You can also "
  a_ [href_ "https://monoscope.tech/demo?utm_source=transac_emails"] "schedule a call"
  " with our team; we're always ready to help!"


emailSignoff :: Html ()
emailSignoff = p_ [style_ "color: #57606a;"] "\8212 Monoscope"


emailFallbackUrl :: Text -> Html ()
emailFallbackUrl url = do
  emailDivider
  p_ [class_ "sub"] "If you're having trouble with the button above, copy and paste this URL into your browser:"
  p_ [class_ "sub"] $ toHtml url


emailStatRow :: [(Text, Text, Maybe Text)] -> Html ()
emailStatRow cols = div_ [class_ "highlight-box"]
  $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"]
  $ tr_
  $ forM_ (zip [0 :: Int ..] cols) \(i, (label, val, colorM)) ->
    td_ [width_ "33%", style_ $ "text-align: center; padding: 8px 0;" <> if i > 0 then " border-left: 1px solid #dee2e7;" else ""] do
      p_ [style_ "margin: 0 0 4px; font-size: 13px; color: #57606a;"] $ toHtml label
      p_ [style_ $ "margin: 0; font-size: 22px; font-weight: 700;" <> maybe "" ("; color: " <>) colorM] $ toHtml val


emailGreeting :: Maybe Text -> Html ()
emailGreeting = maybe (p_ "Hi there!") \name -> p_ do "Hi "; b_ (toHtml name); "!"


-- =============================================================================
-- Simple Project Templates
-- =============================================================================

data ProjectNotifKind = PNInvite | PNCreated | PNDeleted


projectNotifEmail :: ProjectNotifKind -> Maybe Text -> Text -> Maybe (Text, Text) -> (Text, Html ())
projectNotifEmail kind userNameM projectName ctaM =
  ( subject
  , emailBody do
      emailGreeting userNameM
      p_ $ toHtmlRaw @Text message
      whenJust ctaM $ uncurry emailButton
      emailHelpLinks
      br_ []
      emailSignoff
      whenJust ctaM \(url, _) -> emailFallbackUrl url
  )
  where
    (subject, message) = case kind of
      PNInvite -> ("[···] Project Invitation", "<b>" <> fromMaybe "Someone" userNameM <> "</b> has invited you to the <b>" <> projectName <> "</b> project on Monoscope. We're excited to have you on board! All you need to do next is <a href=\"https://monoscope.tech/docs/sdks?utm_source=transac_emails\">integrate one of our SDKs</a> into your application so we can begin monitoring your API.")
      PNCreated -> ("[···] New Project Created", "You have created a new <b>" <> projectName <> "</b> project on Monoscope.")
      PNDeleted -> ("[···] Project Deleted", "You have successfully deleted the <b>" <> projectName <> "</b> project.")


projectInviteEmail :: Text -> Text -> Text -> (Text, Html ())
projectInviteEmail userName projectName projectUrl =
  projectNotifEmail PNInvite (Just userName) projectName (Just (projectUrl, "Access Project"))


projectCreatedEmail :: Text -> Text -> Text -> (Text, Html ())
projectCreatedEmail userName projectName projectUrl =
  projectNotifEmail PNCreated (Just userName) projectName (Just (projectUrl, "Access Project"))


projectDeletedEmail :: Text -> Text -> (Text, Html ())
projectDeletedEmail userName projectName =
  projectNotifEmail PNDeleted (Just userName) projectName (Just ("https://app.monoscope.tech/p/new", "Create a New Project"))


-- =============================================================================
-- Runtime Errors Template
-- =============================================================================

runtimeErrorsEmail :: Text -> Text -> [ErrorPatterns.ATError] -> (Text, Html ())
runtimeErrorsEmail projectName errorsUrl errors =
  runtimeErrorVariantEmail
    "New Runtime Error(s)"
    "[···] New Runtime Exception(s) Detected - "
    projectName
    errorsUrl
    errors
    "We've detected a new runtime error in your "


escalatingErrorsEmail :: Text -> Text -> [ErrorPatterns.ATError] -> (Text, Html ())
escalatingErrorsEmail projectName errorsUrl errors =
  runtimeErrorVariantEmail
    "Escalating Runtime Error(s)"
    "[···] Escalating Runtime Error(s) Detected - "
    projectName
    errorsUrl
    errors
    "We've detected escalating runtime errors in your "


regressedErrorsEmail :: Text -> Text -> [ErrorPatterns.ATError] -> (Text, Html ())
regressedErrorsEmail projectName errorsUrl errors =
  runtimeErrorVariantEmail
    "Regressed Runtime Error(s)"
    "[···] Regressed Runtime Error(s) Detected - "
    projectName
    errorsUrl
    errors
    "We've detected regressed runtime errors in your "


errorSpikesEmail :: Text -> Text -> [ErrorPatterns.ATError] -> (Text, Html ())
errorSpikesEmail projectName errorsUrl errors =
  runtimeErrorVariantEmail
    "Runtime Error Spike(s)"
    "[···] Runtime Error Spike(s) Detected - "
    projectName
    errorsUrl
    errors
    "We've detected a runtime error spike in your "


runtimeErrorVariantEmail :: Text -> Text -> Text -> Text -> [ErrorPatterns.ATError] -> Text -> (Text, Html ())
runtimeErrorVariantEmail heading subjectPrefix projectName errorsUrl errors intro =
  ( subjectPrefix <> projectName
  , emailBody do
      h1_ $ toHtml heading
      p_ do
        toHtml intro
        b_ $ toHtml projectName
        "."
      emailDivider
      forM_ (take maxErrorCards errors) (errorCard errorsUrl)
      when (length errors > maxErrorCards)
        $ p_ [style_ "text-align: center; color: #57606a; font-size: 14px; margin: 16px 0;"]
        $ toHtml
        $ "and "
        <> show (length errors - maxErrorCards)
        <> " more error(s)…"
      emailButton errorsUrl "View all errors"
  )
  where
    maxErrorCards = 5


errorCard :: Text -> ErrorPatterns.ATError -> Html ()
errorCard errorsUrl e =
  table_ [class_ "error-card", width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
    tr_ $ td_ [style_ "padding: 15px 20px 10px 20px;"] do
      p_ [class_ "error-card-header"] $ toHtml $ truncateText 120 e.errorType
      p_ [class_ "error-card-sub", style_ "word-break: break-word;"] $ toHtml $ truncateText 200 e.message
      when hasDistinctRootCause
        $ p_ [style_ "margin: 6px 0 0; font-size: 13px; color: #24292f; word-break: break-word;"]
        $ do
          b_ [style_ "color: #57606a;"] "Root cause: "
          toHtml $ truncateText 200 (e.rootErrorType <> ": " <> e.rootErrorMessage)
    tr_
      $ td_ [style_ "padding: 0 20px 12px 20px;"]
      $ p_ [class_ "error-card-meta", style_ "margin: 0; line-height: 1.6;"]
      $ do
        let meta =
              filter
                ((/= "") . snd)
                [ ("", fromMaybe "" e.requestMethod <> " " <> fromMaybe "" e.requestPath)
                , ("", fromMaybe "" e.serviceName)
                , ("", fromMaybe "" e.environment)
                , ("", toText $ formatTime defaultTimeLocale "%b %-e, %Y, %-l:%M %p" e.when)
                ]
        forM_ (zip [0 :: Int ..] meta) \(i, (_, val)) -> do
          when (i > 0) $ span_ [style_ "color: #c0c5cc; padding: 0 6px;"] "\183"
          toHtml val
    when (e.stackTrace /= "") $ tr_ $ td_ [style_ "padding: 0 20px 15px 20px;"] do
      let traceLines = T.lines e.stackTrace
          lastLines = takeEnd 2 traceLines
          hasMore = length traceLines > 2
      div_ [class_ "error-card-stack"] $ toHtml $ T.intercalate "\n" lastLines
      when hasMore
        $ p_ [style_ "margin: 8px 0 0; font-size: 12px;"]
        $ a_ [href_ (errorsUrl <> "by_hash/" <> e.hash), style_ "color: #377cfb; text-decoration: none;"]
        $ toHtml @Text ("View full trace (" <> show (length traceLines) <> " lines) \8594")
  where
    hasDistinctRootCause = e.rootErrorType /= e.errorType || e.rootErrorMessage /= e.message
    takeEnd n xs = drop (length xs - n) xs


truncateText :: Int -> Text -> Text
truncateText n t = if T.length t > n then T.take n t <> "…" else t


-- =============================================================================
-- Anomaly Endpoint Template
-- =============================================================================

anomalyEndpointEmail :: Text -> Text -> Text -> [Text] -> (Text, Html ())
anomalyEndpointEmail userName projectName anomalyUrl endpointNames =
  ( "[···] New Endpoint(s) Detected for Your \"" <> projectName <> "\" Project"
  , emailBody do
      emailGreeting (Just userName)
      p_ do
        "We detected new endpoints on your "
        b_ $ toHtml projectName
        " project:"
      div_ [class_ "highlight-box"]
        $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
          tr_ $ td_ [style_ "padding-bottom: 8px; font-weight: 600; font-size: 15px;"] "New Endpoints:"
          forM_ endpointNames
            $ tr_
            . td_ [style_ "padding: 3px 0;"]
            . span_ [class_ "monoscope-code"]
            . toHtml
      emailButton anomalyUrl "Explore the Endpoint"
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl anomalyUrl
  )


-- =============================================================================
-- Issue Assigned Template
-- =============================================================================

issueAssignedEmail :: Text -> Text -> Text -> Text -> Text -> Text -> (Text, Html ())
issueAssignedEmail userName projectName issueTitle issueUrl errorType errorMessage =
  ( "[···] Issue Assigned: " <> issueTitle
  , emailBody do
      emailGreeting (Just userName)
      p_ do
        "You have been assigned to an issue in the "
        b_ $ toHtml projectName
        " project."
      table_ [class_ "error-card", width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
        tr_ $ td_ [style_ "padding: 15px 20px 5px 20px;"] do
          p_ [class_ "error-card-header"] $ toHtml errorType
          p_ [class_ "error-card-sub"] $ toHtml errorMessage
        tr_
          $ td_ [style_ "padding: 10px 20px 20px 20px;"]
          $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"]
          $ tr_ do
            metaCell "Issue:" issueTitle
            metaCell "Project:" projectName
      emailButton issueUrl "View Issue"
      emailDivider
      emailSignoff
      emailFallbackUrl issueUrl
  )


-- =============================================================================
-- Weekly Report Template
-- =============================================================================

data WeeklyReportData = WeeklyReportData
  { userName :: Text
  , projectName :: Text
  , reportUrl :: Text
  , projectUrl :: Text
  , startDate :: Text
  , endDate :: Text
  , eventsChartUrl :: Text
  , errorsChartUrl :: Text
  , totalEvents :: Int
  , totalErrors :: Int
  , eventsChangePct :: Double
  , errorsChangePct :: Double
  , runtimeErrorsCount :: Int
  , apiChangesCount :: Int
  , alertsCount :: Int
  , logPatternCount :: Int
  , rateChangeCount :: Int
  , anomalies :: V.Vector Issues.IssueSummary
  , performance :: V.Vector (Text, Text, Text, Int, Double, Int, Double)
  , slowQueries :: V.Vector (Text, Int, Int)
  , topPatterns :: V.Vector (Text, Int64, Text)
  , freeTierExceeded :: Bool
  }
  deriving stock (Generic)


weeklyReportEmail :: WeeklyReportData -> (Text, Html ())
weeklyReportEmail d =
  ( "[···] Weekly Report for " <> d.projectName
  , emailBody do
      emailGreeting (Just d.userName)
      h1_ [style_ "margin: 0 0 4px;"] $ toHtml d.projectName
      p_ [style_ "color: #57606a; margin: 0 0 16px;"] $ toHtml $ d.startDate <> " \8212 " <> d.endDate
      when d.freeTierExceeded $ p_ [style_ "color: #cf222e; margin: 0 0 16px;"] "Free tier limit exceeded \8212 report is incomplete."

      -- Stats: two-column numbers with change indicators
      table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"] $ tr_ do
        td_ [width_ "50%", style_ "padding: 16px 0;"] do
          p_ [style_ "margin: 0 0 4px; font-size: 14px; color: #57606a;"] "Events"
          p_ [class_ "stat-number", style_ "margin: 0; font-size: 28px; font-weight: 700; white-space: nowrap;"] $ toHtml $ formatWithCommas (fromIntegral d.totalEvents)
          changeIndicator d.eventsChangePct False
        td_ [width_ "50%", style_ "padding: 16px 0; border-left: 1px solid #dee2e7; padding-left: 20px;"] do
          p_ [style_ "margin: 0 0 4px; font-size: 14px; color: #57606a;"] "Errors"
          p_ [class_ "stat-number", style_ "margin: 0; font-size: 28px; font-weight: 700; white-space: nowrap; color: #cf222e;"] $ toHtml $ formatWithCommas (fromIntegral d.totalErrors)
          changeIndicator d.errorsChangePct True

      -- Charts: full-width
      chartBlock "Events" d.eventsChartUrl
      chartBlock "Errors" d.errorsChartUrl

      -- Issues breakdown bar with counts in legend (only non-zero categories)
      let categories =
            filter
              (\(_, c, _) -> c > 0)
              [ ("Runtime Errors", d.runtimeErrorsCount, "#cf222e")
              , ("API Changes", d.apiChangesCount, "#377cfb")
              , ("Monitor Alerts", d.alertsCount, "#bf8700")
              , ("Log Patterns", d.logPatternCount, "#0969da")
              , ("Rate Changes", d.rateChangeCount, "#e36209")
              ]
          totalCats = sum $ map (\(_, c, _) -> c) categories
          pctOf n = if totalCats == 0 then 0 else (fromIntegral n / fromIntegral totalCats) * 99 :: Double
      let totalIssues = V.length d.anomalies
      when (totalIssues > 0 && not (null categories))
        $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"]
        $ tr_
        $ td_ [style_ "padding: 20px 0;"] do
          h3_ [style_ "margin: 0 0 12px; font-size: 18px;"] $ toHtml $ "Issues breakdown (" <> show totalIssues <> ")"
          table_ [class_ "bar-track", width_ "100%", cellpadding_ "0", cellspacing_ "0", style_ "background-color: #dee2e7; border-radius: 8px; overflow: hidden;"] $ tr_ do
            forM_ categories \(_, cnt, color) ->
              td_ [style_ $ "height: 12px; background-color: " <> color <> "; padding: 0;", width_ $ show (pctOf cnt) <> "%"] ""
          table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0", style_ "margin-top: 10px;"] $ tr_ do
            forM_ categories \(label, cnt, color) -> barLegendWithCount color label cnt

      -- Issues table: top 10 with type badges, sparkline, linked to issue page
      unless (V.null d.anomalies) do
        let maxIssues = 10
            shown = V.take maxIssues d.anomalies
            remaining = totalIssues - V.length shown
            issueUrl iid = d.projectUrl <> "/issues/" <> iid.toText
        reportTable ("Issues (" <> show totalIssues <> ")") ["Trend"]
          $ V.toList
            ( shown <&> \iss -> tr_ do
                td_ [style_ "vertical-align: middle;"] do
                  issueTypeBadge iss.issueType iss.critical
                  a_ [href_ (issueUrl iss.id), style_ "color: inherit; text-decoration: none;"] $ toHtml $ stripSummaryBadges iss.title
                td_ [width_ "120", style_ "vertical-align: middle; text-align: right;"]
                  $ sparklineImg (fromMaybe [] iss.activityBuckets)
            )
          <> [ tr_
                 $ td_ [colspan_ "2", style_ "text-align: center; color: #57606a; font-size: 14px;"]
                 $ a_ [href_ (d.projectUrl <> "/issues"), style_ "color: #377cfb; text-decoration: none;"]
                 $ toHtml @Text ("and " <> show remaining <> " more\8230")
             | remaining > 0
             ]

      -- Performance table (hidden when empty)
      unless (V.null d.performance)
        $ reportTable ("HTTP Endpoints (" <> show (V.length d.performance) <> ")") ["Avg Latency", "Change"]
        $ V.toList
        $ V.take 10 d.performance
        <&> \(host, method, urlPath, dur, durChange, _, _) ->
          tr_ do
            td_ do toHtml host; " "; span_ [class_ "monoscope-code"] $ toHtml method; " "; span_ [class_ "monoscope-code"] $ toHtml urlPath
            td_ $ toHtml $ show dur <> "ms"
            td_ [style_ if durChange > 0 then "color: #cf222e;" else "color: #1a7f37;"]
              $ toHtml
              $ (if durChange > 0 then "+" else "")
              <> show durChange
              <> "%"

      -- Slow queries table (already hidden when empty)
      unless (V.null d.slowQueries)
        $ reportTable ("Slow DB Queries (" <> show (V.length d.slowQueries) <> ")") ["Avg Latency", "Events"]
        $ V.toList
        $ d.slowQueries
        <&> \(statement, total, latency) ->
          tr_ do
            td_ $ span_ [class_ "monoscope-code"] $ toHtml statement
            td_ $ toHtml $ show latency <> "ms"
            td_ $ toHtml $ show total

      -- Top log patterns
      unless (V.null d.topPatterns)
        $ reportTable ("Top Log Patterns (" <> show (V.length d.topPatterns) <> ")") ["Source", "Count"]
        $ V.toList
        $ d.topPatterns
        <&> \(pat, cnt, srcLabel) ->
          tr_ do
            td_ $ span_ [class_ "monoscope-code"] $ toHtml $ stripSummaryBadges pat
            td_ $ toHtml srcLabel
            td_ $ toHtml $ formatWithCommas (fromIntegral cnt)

      emailButton d.reportUrl "View Full Report"
  )


chartBlock :: Text -> Text -> Html ()
chartBlock label url = do
  p_ [style_ "margin: 20px 0 8px; font-size: 14px; font-weight: 600; color: #57606a;"] $ toHtml label
  img_ [src_ url, alt_ $ label <> " chart", width_ "600", style_ "max-width: 100%; height: auto; display: block; border: 1px solid #dee2e7; border-radius: 8px;"]


barLegendWithCount :: Text -> Text -> Int -> Html ()
barLegendWithCount color label count = td_ [class_ "bar-legend", style_ "font-size: 12px; color: #57606a; padding: 0;"] do
  span_ [style_ $ "width: 8px; height: 8px; background-color: " <> color <> "; display: inline-block; border-radius: 50%; margin-right: 5px; vertical-align: middle;"] ""
  toHtml $ label <> " (" <> show count <> ")"


changeIndicator :: Double -> Bool -> Html ()
changeIndicator pct invertColor
  | pct == 0 = pure ()
  | otherwise =
      let isUp = pct > 0
          arrow = if isUp then "\8593 " else "\8595 "
          color
            | invertColor = if isUp then "#cf222e" else "#1a7f37"
            | otherwise = if isUp then "#1a7f37" else "#cf222e"
       in p_ [style_ $ "margin: 4px 0 0; font-size: 14px; font-weight: 500; color: " <> color <> ";"]
            $ toHtml
            $ arrow
            <> show (abs pct)
            <> "% vs last period"


-- | Strip `field;style⇒value` summary badge tokens to plain text values
stripSummaryBadges :: Text -> Text
stripSummaryBadges = T.unwords . mapMaybe extractValue . T.words
  where
    extractValue token = case T.breakOn "\8658" token of
      (_, "") -> Just token
      (_, rest) -> let v = T.drop 1 rest in if T.null v then Nothing else Just v


issueTypeBadge :: Issues.IssueType -> Bool -> Html ()
issueTypeBadge iType critical =
  let (label, color) = case iType of
        Issues.RuntimeException -> ("Error", "#cf222e")
        Issues.QueryAlert -> ("Alert", "#bf8700")
        Issues.LogPattern -> ("Log Pattern", "#377cfb")
        Issues.LogPatternRateChange -> ("Rate Change", "#bf8700")
        Issues.ApiChange | critical -> ("Breaking", "#cf222e")
        Issues.ApiChange -> ("Incremental", "#377cfb")
   in span_ [style_ $ "display: inline-block; font-size: 11px; font-weight: 600; color: #fff; background-color: " <> color <> "; padding: 2px 8px; border-radius: 10px; margin-right: 8px; vertical-align: middle;"] $ toHtml label


-- | Render an SVG sparkline as an inline data URI image (email-safe, works in dark/light mode)
sparklineImg :: [Int] -> Html ()
sparklineImg buckets
  | null buckets || all (== 0) buckets = pure ()
  | otherwise =
      let peakVal = foldl' max 1 buckets
          peak = fromIntegral @Int @Double peakVal
          n = length buckets
          h = 32 :: Int
          barZone = 28 :: Int
          gap = 2 :: Int
          barW = max 3 (120 `div` n - gap)
          w = n * (barW + gap)
          bars =
            mconcat
              $ zipWith
                ( \i v ->
                    let barH = max 2 (round @Double @Int $ (fromIntegral v / peak) * fromIntegral barZone)
                        x = i * (barW + gap)
                        y = h - barH
                        opacity = if v == 0 then "0.2" else "0.7" :: Text
                     in "<rect x='" <> show x <> "' y='" <> show y <> "' width='" <> show barW <> "' height='" <> show barH <> "' rx='1.5' fill='%23377cfb' opacity='" <> opacity <> "'/>"
                )
                [0 ..]
                buckets
          svg = "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 " <> show w <> " " <> show h <> "' width='" <> show w <> "' height='" <> show h <> "'>" <> bars <> "</svg>"
          dataUri = "data:image/svg+xml," <> svg
       in img_ [src_ dataUri, alt_ "trend", width_ "120", height_ "32", style_ "display: block;"]


reportTable :: Text -> [Text] -> [Html ()] -> Html ()
reportTable title headers rows =
  table_ [class_ "report-table", width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
    tr_ do
      th_ $ toHtml title
      forM_ headers $ th_ . toHtml
    sequence_ rows


-- =============================================================================
-- Sample Data for Previews
-- =============================================================================

sampleProjectInvite :: (Text, Html ())
sampleProjectInvite = projectInviteEmail "Jane Doe" "My API Project" "https://app.monoscope.tech/p/sample-id"


sampleProjectCreated :: (Text, Html ())
sampleProjectCreated = projectCreatedEmail "Jane Doe" "My API Project" "https://app.monoscope.tech/p/sample-id"


sampleProjectDeleted :: (Text, Html ())
sampleProjectDeleted = projectDeletedEmail "Jane Doe" "My API Project"


sampleRuntimeErrors :: (Text, Html ())
sampleRuntimeErrors = runtimeErrorsEmail "My API Project" "https://app.monoscope.tech/p/sample-id/issues/" [sampleError1, sampleError2, sampleError3]
  where
    sampleError1 =
      def
        { ErrorPatterns.errorType = "TypeError"
        , ErrorPatterns.message = "Cannot read property 'map' of undefined"
        , ErrorPatterns.rootErrorType = "TypeError"
        , ErrorPatterns.rootErrorMessage = "Cannot read property 'map' of undefined"
        , ErrorPatterns.stackTrace = "at Array.map (<anonymous>)\n  at processItems (src/handlers/items.js:42:15)\n  at async Router.handle (node_modules/express/lib/router.js:174:12)"
        , ErrorPatterns.hash = "abc123def"
        , ErrorPatterns.requestMethod = Just "GET"
        , ErrorPatterns.requestPath = Just "/api/v1/items"
        , ErrorPatterns.serviceName = Just "api-gateway"
        , ErrorPatterns.environment = Just "production"
        }
    sampleError2 =
      def
        { ErrorPatterns.errorType = "NullPointerException"
        , ErrorPatterns.message = "Attempt to invoke method on null reference"
        , ErrorPatterns.rootErrorType = "NullPointerException"
        , ErrorPatterns.rootErrorMessage = "null reference in UserService.getUser()"
        , ErrorPatterns.stackTrace = "at com.example.UserService.getUser(UserService.java:56)\n  at com.example.ApiController.handleRequest(ApiController.java:123)"
        , ErrorPatterns.hash = "xyz789abc"
        , ErrorPatterns.requestMethod = Just "POST"
        , ErrorPatterns.requestPath = Just "/api/v1/users"
        , ErrorPatterns.serviceName = Just "user-service"
        , ErrorPatterns.environment = Just "production"
        }
    sampleError3 =
      def
        { ErrorPatterns.errorType = "HttpException"
        , ErrorPatterns.message = "HttpExceptionRequest Request { host = \"api.lemonsqueezy.com\" port = 443 secure = True requestHeaders = [(\"Authorization\",\"<REDACTED>\"),(\"User-Agent\",\"haskell wreq-0.5.4.3\")] path = \"/v1/subscriptions/\" queryString = \"\" method = \"GET\" } (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = \"Forbidden\"}}))"
        , ErrorPatterns.rootErrorType = "StatusCodeException"
        , ErrorPatterns.rootErrorMessage = "403 Forbidden from api.lemonsqueezy.com"
        , ErrorPatterns.stackTrace = "at Network.Wreq.getWith (src/Network/Wreq.hs:112:5)\n  at Billing.LemonSqueezy.fetchSubscriptions (src/Billing/LemonSqueezy.hs:89:12)\n  at BackgroundJobs.syncSubscriptions (src/BackgroundJobs.hs:234:8)"
        , ErrorPatterns.hash = "ls403err"
        , ErrorPatterns.requestMethod = Just "GET"
        , ErrorPatterns.requestPath = Just "/v1/subscriptions/"
        , ErrorPatterns.serviceName = Just "billing-worker"
        , ErrorPatterns.environment = Just "production"
        }


sampleAnomalyEndpoint :: (Text, Html ())
sampleAnomalyEndpoint = anomalyEndpointEmail "Jane Doe" "My API Project" "https://app.monoscope.tech/p/sample-id/issues" ["POST /api/v1/orders", "GET /api/v1/orders/:id"]


sampleIssueAssigned :: (Text, Html ())
sampleIssueAssigned = issueAssignedEmail "Jane Doe" "My API Project" "TypeError: Cannot read property 'map' of undefined" "https://app.monoscope.tech/p/sample-id/issues/by_hash/abc123" "TypeError" "Cannot read property 'map' of undefined"


sampleWeeklyReport :: Text -> Text -> (Text, Html ())
sampleWeeklyReport eventsChart errorsChart =
  weeklyReportEmail
    WeeklyReportData
      { userName = "Jane Doe"
      , projectName = "My API Project"
      , reportUrl = "https://app.monoscope.tech/p/sample-id/reports/sample-report"
      , projectUrl = "https://app.monoscope.tech/p/sample-id"
      , startDate = "2025-01-01"
      , endDate = "2025-01-07"
      , eventsChartUrl = eventsChart
      , errorsChartUrl = errorsChart
      , totalEvents = 125000
      , totalErrors = 342
      , eventsChangePct = 12.5
      , errorsChangePct = -8.3
      , runtimeErrorsCount = 3
      , apiChangesCount = 1
      , alertsCount = 1
      , logPatternCount = 1
      , rateChangeCount = 1
      , anomalies =
          V.fromList
            [ Issues.IssueSummary (UUIDId UUID.nil) "TypeError: Cannot read property 'map'" True "critical" Issues.RuntimeException (Just [0, 2, 5, 12, 8, 3, 1])
            , Issues.IssueSummary (UUIDId UUID.nil) "New endpoint detected: POST /api/orders" False "medium" Issues.ApiChange (Just [0, 0, 0, 1, 0, 0, 0])
            , Issues.IssueSummary (UUIDId UUID.nil) "Connection timeout pattern detected" False "medium" Issues.LogPattern (Just [1, 3, 2, 0, 1, 4, 2])
            , Issues.IssueSummary (UUIDId UUID.nil) "Request rate spike on /api/users" False "high" Issues.LogPatternRateChange (Just [0, 1, 1, 5, 12, 3, 0])
            ]
      , performance = V.fromList [("api.example.com", "GET", "/api/v1/users", 245, -12.5, 5000, 8.3), ("api.example.com", "POST", "/api/v1/orders", 890, 45.2, 1200, -3.1)]
      , slowQueries = V.fromList [("SELECT * FROM users WHERE email = $1", 3400, 1250 :: Int)]
      , topPatterns = V.fromList [("GET /api/v1/users/<*>", 4500, "URL path"), ("severity_text;badge-error⇒ERROR Failed to connect to database: connection refused at <*>", 1230, "Event summary"), ("Request timeout after <*> ms for endpoint <*>", 890, "Log body")]
      , freeTierExceeded = False
      }


-- =============================================================================
-- Monitor Alert Templates
-- =============================================================================

monitorAlertEmail :: Text -> Text -> Text -> Double -> Double -> Text -> (Text, Html ())
monitorAlertEmail projectName monitorTitle monitorUrl currentValue threshold direction =
  ( "[···] Monitor Alert: " <> monitorTitle <> " - " <> projectName
  , emailBody do
      h1_ "Monitor Alert Triggered"
      p_ do
        "The monitor "
        b_ $ toHtml monitorTitle
        " in your "
        b_ $ toHtml projectName
        " project has breached its threshold."
      emailDivider
      emailStatRow
        [ ("Current Value", show (round currentValue :: Int), Just "#cf222e")
        , ("Threshold", show (round threshold :: Int), Nothing)
        , ("Direction", direction, Nothing)
        ]
      emailButton monitorUrl "View Monitor"
      emailDivider
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl monitorUrl
  )


monitorRecoveryEmail :: Text -> Text -> Text -> (Text, Html ())
monitorRecoveryEmail projectName monitorTitle monitorUrl =
  ( "[···] Monitor Recovered: " <> monitorTitle <> " - " <> projectName
  , emailBody do
      h1_ [style_ "color: #1a7f37;"] "Monitor Recovered"
      p_ do
        "The monitor "
        b_ $ toHtml monitorTitle
        " in your "
        b_ $ toHtml projectName
        " project has recovered and is back to normal."
      emailButton monitorUrl "View Monitor"
      emailDivider
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl monitorUrl
  )


freeTierUsageEmail :: Text -> Text -> Int -> Int -> Bool -> (Text, Html ())
freeTierUsageEmail projectName billingUrl used limit exceeded =
  let pct = (used * 100) `div` max 1 limit
   in ( "[···] " <> (if exceeded then "Daily event limit reached" else "Approaching daily event limit") <> " - " <> projectName
      , emailBody do
          h1_ $ if exceeded then "Daily Event Limit Reached" else "Approaching Daily Event Limit"
          p_ do
            "Your "
            b_ $ toHtml projectName
            if exceeded
              then " project has hit its daily free tier limit. New events are being dropped."
              else " project is approaching its daily free tier limit."
          emailStatRow
            [ ("Events Today", formatWithCommas (fromIntegral used), if exceeded then Just "#cf222e" else Just "#bf8700")
            , ("Daily Limit", formatWithCommas (fromIntegral limit), Nothing)
            , ("Usage", show pct <> "%", if exceeded then Just "#cf222e" else Just "#bf8700")
            ]
          emailButton billingUrl "Upgrade Plan"
          emailDivider
          emailHelpLinks
          br_ []
          emailSignoff
          emailFallbackUrl billingUrl
      )


planUpgradedEmail :: Text -> Text -> Text -> (Text, Html ())
planUpgradedEmail projectName newPlan billingUrl =
  ( "[···] Plan upgraded to " <> newPlan <> " - " <> projectName
  , emailBody do
      h1_ "Plan Upgraded"
      p_ do
        "Your "
        b_ $ toHtml projectName
        " project has been upgraded to the "
        b_ $ toHtml newPlan
        " plan. Thank you for your support!"
      emailButton billingUrl "View Billing"
      emailDivider
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl billingUrl
  )


planDowngradedEmail :: Text -> Text -> Text -> (Text, Html ())
planDowngradedEmail projectName reason billingUrl =
  ( "[···] Plan downgraded to Free - " <> projectName
  , emailBody do
      h1_ "Plan Downgraded to Free"
      p_ do
        "Your "
        b_ $ toHtml projectName
        " project has been downgraded to the Free plan because your subscription "
        toHtml reason
        "."
      p_ "On the Free plan, daily event limits apply and additional team members will be deactivated. You can re-subscribe at any time to restore full access."
      emailButton billingUrl "Upgrade Plan"
      emailDivider
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl billingUrl
  )
