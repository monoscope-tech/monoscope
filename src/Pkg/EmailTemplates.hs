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
  anomalyEndpointEmail,
  weeklyReportEmail,
  WeeklyReportData (..),
  logPatternEmail,
  logPatternRateChangeEmail,

  -- * Sample data for previews
  sampleProjectInvite,
  sampleProjectCreated,
  sampleProjectDeleted,
  sampleRuntimeErrors,
  sampleAnomalyEndpoint,
  sampleWeeklyReport,
  sampleLogPattern,
  sampleLogPatternRateChange,
) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Time (formatTime)
import Data.Time.Format (defaultTimeLocale)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Lucid
import Models.Apis.Issues qualified as Issues
import Models.Apis.RequestDumps qualified as RequestDumps
import Pkg.DeriveUtils (UUIDId (..))
import Relude


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
  .monoscope-email .error-card { border: 1px solid #dee2e7; border-radius: 12px; border-left: 4px solid #cf222e; margin: 16px 0; }
  .monoscope-email .error-card-header { color: #cf222e; font-size: 16px; font-weight: 600; margin: 0 0 4px; }
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
  }
  @media (prefers-color-scheme: dark) {
    .monoscope-email p, .monoscope-email ul, .monoscope-email ol, .monoscope-email li, .monoscope-email h1, .monoscope-email h2, .monoscope-email h3 { color: #ffffff !important; }
    .monoscope-email .divider { border-top-color: #333333 !important; }
    .monoscope-email .monoscope-code { background-color: #1a1a1a !important; border-color: #333333 !important; color: #ffffff !important; }
    .monoscope-email .highlight-box { background-color: #1a1a1a !important; }
    .monoscope-email .highlight-box p { color: #aaaaaa !important; }
    .monoscope-email .error-card { border-color: #333333 !important; border-left-color: #f87171 !important; background-color: transparent !important; }
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
emailSignoff = p_ do
  "Best regards,"
  br_ []
  "Monoscope Team"


emailFallbackUrl :: Text -> Html ()
emailFallbackUrl url = do
  emailDivider
  p_ [class_ "sub"] "If you're having trouble with the button above, copy and paste this URL into your browser:"
  p_ [class_ "sub"] $ toHtml url


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

runtimeErrorsEmail :: Text -> Text -> [RequestDumps.ATError] -> (Text, Html ())
runtimeErrorsEmail projectName errorsUrl errors =
  ( "[···] New Runtime Exception(s) Detected - " <> projectName
  , emailBody do
      h1_ "New Runtime Error(s)"
      p_ do
        "We've detected a new runtime error in your "
        b_ $ toHtml projectName
        " project. A copy of this email was sent to all members of the project."
      emailDivider
      forM_ errors errorCard
      emailButton errorsUrl "View all errors"
      emailDivider
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl errorsUrl
  )


errorCard :: RequestDumps.ATError -> Html ()
errorCard e =
  table_ [class_ "error-card", width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
    tr_ $ td_ [style_ "padding: 15px 20px 5px 20px;"] do
      p_ [class_ "error-card-header"] $ toHtml $ e.errorType <> ": " <> e.message
      p_ [class_ "error-card-sub"] $ toHtml $ "Root cause: " <> e.rootErrorType <> ": " <> e.rootErrorMessage
    tr_
      $ td_ [style_ "padding: 10px 20px;"]
      $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
        tr_ do
          metaCell "When:" $ toText $ formatTime defaultTimeLocale "%b %-e, %Y, %-l:%M:%S %p" e.when
          metaCell "Technology:" $ maybe "" (toText . show) e.technology
        tr_ do
          metaCell "Hash:" $ fromMaybe "" e.hash
          metaCell "Request:" $ fromMaybe "" e.requestMethod <> " " <> fromMaybe "" e.requestPath
    tr_ $ td_ [style_ "padding: 10px 20px 20px 20px;"] do
      h3_ [style_ "margin: 0 0 10px 0; font-size: 14px; font-weight: 600;"] "Stack Trace"
      div_ [class_ "error-card-stack"] $ toHtml e.stackTrace
  where
    metaCell label val = td_ [width_ "50%", style_ "padding-bottom: 10px;"]
      $ span_ [class_ "error-card-meta"] do
        b_ [class_ "error-card-label"] $ toHtml @Text label
        " "
        toHtml @Text val


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
-- Weekly Report Template
-- =============================================================================

data WeeklyReportData = WeeklyReportData
  { userName :: Text
  , projectName :: Text
  , reportUrl :: Text
  , startDate :: Text
  , endDate :: Text
  , eventsChartUrl :: Text
  , errorsChartUrl :: Text
  , totalEvents :: Int
  , totalErrors :: Int
  , anomaliesCount :: Int
  , runtimeErrorsPct :: Double
  , apiChangesPct :: Double
  , alertsPct :: Double
  , anomalies :: V.Vector (Issues.IssueId, Text, Bool, Text, Issues.IssueType)
  , performance :: V.Vector (Text, Text, Text, Int, Double, Int, Double)
  , slowQueries :: V.Vector (Text, Int, Int)
  , freeTierExceeded :: Bool
  }
  deriving stock (Generic)


weeklyReportEmail :: WeeklyReportData -> (Text, Html ())
weeklyReportEmail d =
  ( "[···] Weekly Report for " <> d.projectName
  , emailBody do
      emailGreeting (Just d.userName)
      p_ do
        "Here's the weekly report for your "
        b_ $ toHtml d.projectName
        " project on Monoscope from "
        b_ $ toHtml d.startDate
        " until "
        b_ $ toHtml d.endDate
        "."
      when d.freeTierExceeded $ p_ [style_ "color: #cf222e;"] "Free tier limit was exceeded, hence the report is incomplete for the week."
      emailDivider

      -- Stats: two-column numbers
      table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"] $ tr_ do
        td_ [width_ "50%", style_ "text-align: center; padding: 16px 0;"] do
          p_ [style_ "margin: 0 0 4px; font-size: 14px; color: #57606a;"] "Total project events"
          p_ [style_ "margin: 0; font-size: 28px; font-weight: 700;"] $ toHtml $ show d.totalEvents
        td_ [width_ "50%", style_ "text-align: center; padding: 16px 0; border-left: 1px solid #dee2e7;"] do
          p_ [style_ "margin: 0 0 4px; font-size: 14px; color: #57606a;"] "Total project errors"
          p_ [style_ "margin: 0; font-size: 28px; font-weight: 700;"] $ toHtml $ show d.totalErrors

      -- Charts: stacked full-width
      chartBlock "Events" d.eventsChartUrl
      chartBlock "Errors" d.errorsChartUrl

      -- Issues breakdown bar
      table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"] $ tr_ $ td_ [style_ "padding: 20px 0;"] do
        h3_ [style_ "margin: 0 0 12px; font-size: 18px;"] "Issues breakdown"
        -- Bar
        table_ [class_ "bar-track", width_ "100%", cellpadding_ "0", cellspacing_ "0", style_ "background-color: #dee2e7; border-radius: 8px; overflow: hidden;"] $ tr_ do
          td_ [style_ "height: 12px; background-color: #f87171; padding: 0;", width_ $ show d.runtimeErrorsPct <> "%"] ""
          td_ [style_ "height: 12px; background-color: #60a5fa; padding: 0;", width_ $ show d.apiChangesPct <> "%"] ""
          td_ [style_ "height: 12px; background-color: #fbbf24; padding: 0;", width_ $ show d.alertsPct <> "%"] ""
        -- Legend
        table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0", style_ "margin-top: 10px;"] $ tr_ do
          barLegend "#f87171" "Runtime errors"
          barLegend "#60a5fa" "Api changes"
          barLegend "#fbbf24" "Monitor alerts"

      -- Anomalies table
      reportTable ("Issues: Changes, Alerts, and Errors {" <> show d.anomaliesCount <> "}") ["First Seen", "Total Requests"]
        $ if V.null d.anomalies
          then [tr_ $ td_ [colspan_ "3"] "No anomalies detected yet."]
          else
            V.toList $ d.anomalies <&> \(_, title, _, _, _) ->
              tr_ do td_ $ toHtml title; td_ "\8212"; td_ "\8212"

      -- Performance table
      reportTable ("HTTP Endpoints {" <> show (V.length d.performance) <> "}") ["Average Latency", "Latency Change"]
        $ if V.null d.performance
          then [tr_ $ td_ [colspan_ "3"] "No performance data yet."]
          else
            V.toList $ V.take 10 d.performance <&> \(host, method, urlPath, dur, durChange, _, _) ->
              tr_ do
                td_ do toHtml host; " "; span_ [class_ "monoscope-code"] $ toHtml method; " "; span_ [class_ "monoscope-code"] $ toHtml urlPath
                td_ $ toHtml $ show dur
                td_ $ toHtml $ show durChange <> "%"

      -- Slow queries table
      reportTable ("Slow db operations {" <> show (V.length d.slowQueries) <> "}") ["Average Latency", "Total events"]
        $ if V.null d.slowQueries
          then [tr_ $ td_ [colspan_ "3"] "No slow queries detected."]
          else
            V.toList $ d.slowQueries <&> \(statement, total, latency) ->
              tr_ do
                td_ $ span_ [class_ "monoscope-code"] $ toHtml statement
                td_ $ toHtml $ show latency
                td_ $ toHtml $ show total

      emailButton d.reportUrl "View the Full Report"
      emailDivider
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl d.reportUrl
  )


chartBlock :: Text -> Text -> Html ()
chartBlock label url = do
  p_ [style_ "margin: 20px 0 8px; font-size: 14px; font-weight: 600; color: #57606a;"] $ toHtml label
  img_ [src_ url, alt_ $ label <> " chart", width_ "600", style_ "max-width: 100%; height: auto; display: block; border: 1px solid #dee2e7; border-radius: 8px;"]


barLegend :: Text -> Text -> Html ()
barLegend color label = td_ [class_ "bar-legend", style_ "font-size: 12px; color: #57606a; padding: 0;"] do
  span_ [style_ $ "width: 8px; height: 8px; background-color: " <> color <> "; display: inline-block; border-radius: 50%; margin-right: 5px; vertical-align: middle;"] ""
  toHtml label


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
sampleRuntimeErrors = runtimeErrorsEmail "My API Project" "https://app.monoscope.tech/p/sample-id/issues/" [sampleError1, sampleError2]
  where
    sampleError1 =
      def
        { RequestDumps.errorType = "TypeError"
        , RequestDumps.message = "Cannot read property 'map' of undefined"
        , RequestDumps.rootErrorType = "TypeError"
        , RequestDumps.rootErrorMessage = "Cannot read property 'map' of undefined"
        , RequestDumps.stackTrace = "at Array.map (<anonymous>)\n  at processItems (src/handlers/items.js:42:15)\n  at async Router.handle (node_modules/express/lib/router.js:174:12)"
        , RequestDumps.hash = Just "abc123def"
        , RequestDumps.requestMethod = Just "GET"
        , RequestDumps.requestPath = Just "/api/v1/items"
        , RequestDumps.technology = Just RequestDumps.JsExpress
        }
    sampleError2 =
      def
        { RequestDumps.errorType = "NullPointerException"
        , RequestDumps.message = "Attempt to invoke method on null reference"
        , RequestDumps.rootErrorType = "NullPointerException"
        , RequestDumps.rootErrorMessage = "null reference in UserService.getUser()"
        , RequestDumps.stackTrace = "at com.example.UserService.getUser(UserService.java:56)\n  at com.example.ApiController.handleRequest(ApiController.java:123)"
        , RequestDumps.hash = Just "xyz789abc"
        , RequestDumps.requestMethod = Just "POST"
        , RequestDumps.requestPath = Just "/api/v1/users"
        }


sampleAnomalyEndpoint :: (Text, Html ())
sampleAnomalyEndpoint = anomalyEndpointEmail "Jane Doe" "My API Project" "https://app.monoscope.tech/p/sample-id/issues" ["POST /api/v1/orders", "GET /api/v1/orders/:id"]


sampleWeeklyReport :: Text -> Text -> (Text, Html ())
sampleWeeklyReport eventsChart errorsChart =
  weeklyReportEmail
    WeeklyReportData
      { userName = "Jane Doe"
      , projectName = "My API Project"
      , reportUrl = "https://app.monoscope.tech/p/sample-id/reports/sample-report"
      , startDate = "2025-01-01"
      , endDate = "2025-01-07"
      , eventsChartUrl = eventsChart
      , errorsChartUrl = errorsChart
      , totalEvents = 125000
      , totalErrors = 342
      , anomaliesCount = 7
      , runtimeErrorsPct = 42.8
      , apiChangesPct = 28.6
      , alertsPct = 28.6
      , anomalies = V.fromList [(UUIDId UUID.nil, "TypeError: Cannot read property 'map'", True, "critical", Issues.RuntimeException), (UUIDId UUID.nil, "New endpoint detected: POST /api/orders", False, "medium", Issues.APIChange)]
      , performance = V.fromList [("api.example.com", "GET", "/api/v1/users", 245, -12.5, 5000, 8.3), ("api.example.com", "POST", "/api/v1/orders", 890, 45.2, 1200, -3.1)]
      , slowQueries = V.fromList [("SELECT * FROM users WHERE email = $1", 3400, 1250 :: Int)]
      , freeTierExceeded = False
      }


sampleLogPattern :: (Text, Html ())
sampleLogPattern =
  logPatternEmail
    "My API Project"
    "https://app.monoscope.tech/p/sample-id/anomalies/sample-issue"
    "Failed to connect to database: connection refused at <*>"
    (Just "Failed to connect to database: connection refused at 10.0.0.1:5432")
    (Just "error")
    (Just "api-server")
    "body"
    42


sampleLogPatternRateChange :: (Text, Html ())
sampleLogPatternRateChange =
  logPatternRateChangeEmail
    "My API Project"
    "https://app.monoscope.tech/p/sample-id/anomalies/sample-issue"
    "Request timeout after <*> ms for endpoint <*>"
    (Just "Request timeout after 30000 ms for endpoint /api/users")
    (Just "warning")
    (Just "api-gateway")
    "spike"
    150.0
    12.0
    1150.0


-- =============================================================================
-- Log Pattern Template
-- =============================================================================

logPatternEmail :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Int -> (Text, Html ())
logPatternEmail projectName issueUrl patternText sampleMessageM logLevelM serviceNameM sourceField occurrenceCount =
  ( "[···] New Log Pattern Detected - " <> projectName
  , emailBody do
      h1_ "New Log Pattern Detected"
      p_ do
        "A new log pattern has been detected in your "
        b_ $ toHtml projectName
        " project."
      emailDivider
      table_ [class_ "error-card", width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
        tr_ $ td_ [style_ "padding: 15px 20px 10px 20px;"] do
          p_ [class_ "error-card-header", style_ "color: #377cfb;"] "Pattern"
          div_ [class_ "error-card-stack"] $ toHtml $ T.take 300 patternText
        whenJust sampleMessageM \msg ->
          tr_ $ td_ [style_ "padding: 10px 20px;"] do
            p_ [style_ "margin: 0 0 4px; font-size: 13px; font-weight: 600; color: #57606a;"] "Sample Message"
            div_ [class_ "error-card-stack"] $ toHtml $ T.take 300 msg
        tr_
          $ td_ [style_ "padding: 10px 20px 15px 20px;"]
          $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
            tr_ do
              metaCell "Level:" $ fromMaybe "—" logLevelM
              metaCell "Service:" $ fromMaybe "—" serviceNameM
            tr_ do
              metaCell "Source:" sourceField
              metaCell "Occurrences:" $ show occurrenceCount
      emailButton issueUrl "View Log Pattern"
      emailDivider
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl issueUrl
  )
  where
    metaCell label val = td_ [width_ "50%", style_ "padding-bottom: 10px;"]
      $ span_ [class_ "error-card-meta"] do
        b_ [class_ "error-card-label"] $ toHtml @Text label
        " "
        toHtml @Text val


-- =============================================================================
-- Log Pattern Rate Change Template
-- =============================================================================

logPatternRateChangeEmail :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Double -> Double -> Double -> (Text, Html ())
logPatternRateChangeEmail projectName issueUrl patternText sampleMessageM logLevelM serviceNameM direction currentRate baselineMean changePercent =
  ( "[···] Log Pattern Volume " <> T.toTitle direction <> " - " <> projectName
  , emailBody do
      h1_ $ toHtml $ "Log Pattern Volume " <> T.toTitle direction
      p_ do
        "A log pattern volume "
        b_ $ toHtml direction
        " has been detected in your "
        b_ $ toHtml projectName
        " project."
      emailDivider
      -- Rate change stats
      div_ [class_ "highlight-box"] do
        table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"] $ tr_ do
          td_ [width_ "33%", style_ "text-align: center; padding: 8px 0;"] do
            p_ [style_ "margin: 0 0 4px; font-size: 13px; color: #57606a;"] "Current Rate"
            p_ [style_ "margin: 0; font-size: 22px; font-weight: 700;"] $ toHtml $ show (round currentRate :: Int) <> "/hr"
          td_ [width_ "33%", style_ "text-align: center; padding: 8px 0; border-left: 1px solid #dee2e7;"] do
            p_ [style_ "margin: 0 0 4px; font-size: 13px; color: #57606a;"] "Baseline"
            p_ [style_ "margin: 0; font-size: 22px; font-weight: 700;"] $ toHtml $ show (round baselineMean :: Int) <> "/hr"
          td_ [width_ "33%", style_ "text-align: center; padding: 8px 0; border-left: 1px solid #dee2e7;"] do
            p_ [style_ "margin: 0 0 4px; font-size: 13px; color: #57606a;"] "Change"
            p_ [style_ $ "margin: 0; font-size: 22px; font-weight: 700; color: " <> if direction == "spike" then "#cf222e" else "#1a7f37"] $ toHtml $ show (round changePercent :: Int) <> "%"
      -- Pattern card
      table_ [class_ "error-card", width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
        tr_ $ td_ [style_ "padding: 15px 20px 10px 20px;"] do
          p_ [class_ "error-card-header", style_ $ "color: " <> if direction == "spike" then "#cf222e" else "#bf8700"] "Pattern"
          div_ [class_ "error-card-stack"] $ toHtml $ T.take 300 patternText
        whenJust sampleMessageM \msg ->
          tr_ $ td_ [style_ "padding: 10px 20px;"] do
            p_ [style_ "margin: 0 0 4px; font-size: 13px; font-weight: 600; color: #57606a;"] "Sample Message"
            div_ [class_ "error-card-stack"] $ toHtml $ T.take 300 msg
        tr_
          $ td_ [style_ "padding: 10px 20px 15px 20px;"]
          $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"]
          $ tr_ do
            metaCell "Level:" $ fromMaybe "—" logLevelM
            metaCell "Service:" $ fromMaybe "—" serviceNameM
      emailButton issueUrl "View Log Pattern"
      emailDivider
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl issueUrl
  )
  where
    metaCell label val = td_ [width_ "50%", style_ "padding-bottom: 10px;"]
      $ span_ [class_ "error-card-meta"] do
        b_ [class_ "error-card-label"] $ toHtml @Text label
        " "
        toHtml @Text val
