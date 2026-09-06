module Pkg.EmailTemplates (
  -- * Rendering
  renderEmail,
  emailWrapper,
  emailBody,

  -- * Shared alert row (rendered by email + Slack + Discord renderers)
  EndpointAlertRow (..),
  endpointContextLabel,
  groupedByContext,

  -- * Templates
  projectInviteEmail,
  projectCreatedEmail,
  projectDeletedEmail,
  runtimeErrorsEmail,
  escalatingErrorsEmail,
  logPatternEmail,
  logPatternRateChangeEmail,
  regressedErrorsEmail,
  errorSpikesEmail,
  digestEmail,
  anomalyEndpointEmail,
  hostsUnarchivedEmail,
  issueAssignedEmail,
  weeklyReportEmail,
  WeeklyReportData (..),
  monitorAlertEmail,
  monitorRecoveryEmail,
  freeTierUsageEmail,
  planUpgradedEmail,
  planDowngradedEmail,
  trialEndingEmail,

  -- * Helpers
  stripSummaryBadges,
  traceExplorerUrl,

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
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, formatTime, fromGregorian)
import Data.Time.Format (defaultTimeLocale)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Lucid
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Report qualified as Report
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import Utils (formatWithCommas, kqlQuoted, showFFloat', toUriStr)


-- | One row in a new-endpoint alert. @label@ is "METHOD /path"; @host@ is the
--   remote hostname (from @server.address@/@http.host@/@url.full@); @service@
--   and @environment@ come from OTel resource attrs. Host is the primary
--   grouping axis — a service may publish traffic on multiple hosts, and paths
--   like @/@ or @/health@ only become identifiable once the host is known.
data EndpointAlertRow = EndpointAlertRow
  { label :: Text
  , host :: Maybe Text
  , service :: Maybe Text
  , environment :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)


-- | A "service · env" caption, or 'Nothing' when neither attribute is set.
--
-- >>> endpointContextLabel (EndpointAlertRow "GET /x" Nothing (Just "auth") (Just "prod"))
-- Just "auth \183 prod"
-- >>> endpointContextLabel (EndpointAlertRow "GET /x" Nothing (Just "auth") Nothing)
-- Just "auth"
-- >>> endpointContextLabel (EndpointAlertRow "GET /x" Nothing Nothing Nothing)
-- Nothing
endpointContextLabel :: EndpointAlertRow -> Maybe Text
endpointContextLabel r = guarded (not . T.null) $ T.intercalate " · " $ catMaybes [r.service, r.environment]


-- | Partition alert rows by @(host, service, environment)@. Each group carries
--   a @host@ header (primary) plus a @service · env@ caption (secondary). A
--   single group ⇒ homogeneous batch; multiple groups ⇒ renderers emit
--   per-group subheaders. Host leads because it's the operational axis — a
--   reader seeing "GET /" needs to know which hostname got the new surface.
--
-- >>> groupedByContext [EndpointAlertRow "GET /a" (Just "api.x") (Just "auth") (Just "prod"), EndpointAlertRow "POST /b" (Just "api.x") (Just "auth") (Just "prod")]
-- [((Just "api.x",Just "auth \183 prod"),["GET /a","POST /b"])]
-- >>> length (groupedByContext [EndpointAlertRow "GET /a" (Just "api.x") Nothing Nothing, EndpointAlertRow "POST /b" (Just "admin.x") Nothing Nothing])
-- 2
-- >>> groupedByContext [EndpointAlertRow "GET /a" Nothing Nothing Nothing]
-- [((Nothing,Nothing),["GET /a"])]
groupedByContext :: Foldable f => f EndpointAlertRow -> [((Maybe Text, Maybe Text), [Text])]
groupedByContext rows =
  NE.groupAllWith (\r -> (r.host, r.service, r.environment)) (toList rows)
    <&> \grp ->
      let r = head grp
       in ((r.host, endpointContextLabel r), (.label) <$> NE.toList grp)


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
  .monoscope-email { width: 100% !important; height: 100%; margin: 0; -webkit-text-size-adjust: 100%; color: #24292f; background-color: #ffffff; }
  .monoscope-email table { mso-table-lspace: 0pt; mso-table-rspace: 0pt; }
  .monoscope-email .system-report { max-width: 720px; }
  .monoscope-email .report-attention a { color: #b42318 !important; }
  .monoscope-email a { color: #1d4ed8; text-decoration: none; }
  .monoscope-email a:hover { text-decoration: underline; }
  .monoscope-email a img { border: none; }
  .monoscope-email .report-muted { color: #57606a; }
  .monoscope-email td { word-break: break-word; }
  .monoscope-email, .monoscope-email td, .monoscope-email th { font-family: Inter, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen, Ubuntu, Cantarell, "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif; }
  .monoscope-email h1 { color: #24292f; font-size: 24px; font-weight: 600; line-height: 1.25; margin: 0 0 16px; letter-spacing: -0.02em; }
  .monoscope-email h2 { color: #24292f; font-size: 20px; font-weight: 600; line-height: 1.3; margin: 32px 0 12px; letter-spacing: -0.01em; }
  .monoscope-email h3 { color: #24292f; font-size: 18px; font-weight: 600; line-height: 1.3; margin: 24px 0 10px; }
  .monoscope-email td, .monoscope-email th { font-size: 16px; }
  .monoscope-email p { margin: 0 0 12px; font-size: 16px; font-weight: 400; line-height: 1.6; color: #24292f; }
  .monoscope-email p:last-child { margin-bottom: 0; }
  .monoscope-email ul, .monoscope-email ol { color: #24292f; list-style-position: outside; padding-left: 20px; margin: 0 0 16px; }
  .monoscope-email li { padding: 0 0 10px 5px; line-height: 1.6; }
  .monoscope-email p.sub { font-size: 13px; color: #57606a; }
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
  .monoscope-email .email-footer a { color: #57606a; text-decoration: none; }
  .monoscope-email .email-footer a:hover { text-decoration: underline; }
  .monoscope-email .footer-links a { color: #57606a; text-decoration: none; }
  .monoscope-email .divider { border: none; border-top: 1px solid #dee2e7; margin: 32px 0; }
  .monoscope-email .button { display: inline-block; background-color: #1d1e20; color: #ffffff !important; font-size: 15px; font-weight: 500; text-decoration: none; padding: 13px 28px; border-radius: 6px; border: 1px solid #505967; }
  .monoscope-email .button:hover { text-decoration: none; background-color: #2d2e30; }
  .monoscope-email .button-wrapper { text-align: center; margin: 32px 0; }
  .monoscope-email .monoscope-code { background-color: #f6f8fa; border: 1px solid #dee2e7; border-radius: 6px; padding: 2px 6px; font-family: ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace; font-size: 14px; }
  .monoscope-email .highlight-box { background-color: #f6f8fa; border-radius: 12px; padding: 20px 24px; margin: 24px 0; }
  .monoscope-email .highlight-box p { margin: 0; color: #57606a; font-size: 15px; }
  .monoscope-email .error-card { border-top: 1px solid #f1f3f5; margin: 0; }
  .monoscope-email .error-card:first-of-type { border-top: none; }
  .monoscope-email .error-card-header { color: #cf222e; font-size: 16px; font-weight: 600; line-height: 1.35; margin: 0 0 5px; word-break: break-word; }
  .monoscope-email .error-card-sub { color: #374151; font-size: 15px; line-height: 1.5; margin: 0; }
  .monoscope-email .error-card-meta { font-size: 13px; color: #57606a; }
  .monoscope-email .error-card-label { font-weight: 600; color: #24292f; }
  .monoscope-email .error-card-stack { background-color: #1d1e20; color: #e2e8f0; border-radius: 8px; padding: 12px 16px; font-family: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace; font-size: 12px; line-height: 1.5; white-space: pre-wrap; overflow-x: auto; }
  .monoscope-email .social-icons { text-align: center; margin: 20px 0; }
  .monoscope-email .social-icons a { display: inline-block; vertical-align: middle; padding: 0 4px; }
  .monoscope-email .social-icons img { width: 17px; height: 17px; border-radius: 3px; display: block; }
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
    .monoscope-email .report-metric { display: inline-block !important; width: 50% !important; box-sizing: border-box; }
  }
  @media (prefers-color-scheme: dark) {
    .monoscope-email { background-color: #111827 !important; color: #f3f4f6 !important; }
    .monoscope-email .report-attention a { color: #fda29b !important; }
    .monoscope-email .report-muted { color: #cbd5e1 !important; }
    .monoscope-email .report-item { border-color: #374151 !important; }
    .monoscope-email .report-notice { background-color: #1f2937 !important; border-color: #374151 !important; }
    .monoscope-email a { color: #93b4ff !important; }
    .monoscope-email p, .monoscope-email ul, .monoscope-email ol, .monoscope-email li, .monoscope-email h1, .monoscope-email h2, .monoscope-email h3 { color: #ffffff !important; }
    .monoscope-email .divider { border-top-color: #333333 !important; }
    .monoscope-email .monoscope-code { background-color: #1a1a1a !important; border-color: #333333 !important; color: #ffffff !important; }
    .monoscope-email .highlight-box { background-color: #1a1a1a !important; }
    .monoscope-email .highlight-box p { color: #aaaaaa !important; }
    .monoscope-email .error-card { border-top-color: #2a2a2a !important; background-color: transparent !important; }
    .monoscope-email .error-card-header { color: #f87171 !important; }
    .monoscope-email .error-card-sub { color: #d1d5db !important; }
    .monoscope-email .error-card-meta { color: #aaaaaa !important; }
    .monoscope-email .error-card-label { color: #ffffff !important; }
    .monoscope-email .feature-image { border-color: #333333 !important; }
    .monoscope-email .email-footer p, .monoscope-email .email-footer a { color: #94a3b8 !important; }
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
    table_ [class_ "email-wrapper", style_ "width:100%;table-layout:fixed;", width_ "100%", cellpadding_ "0", cellspacing_ "0", role_ "presentation"]
      $ tr_
      $ td_ [align_ "center"]
      $ table_ [class_ "email-content", style_ "width:100%;table-layout:fixed;", width_ "100%", cellpadding_ "0", cellspacing_ "0", role_ "presentation"] do
        -- Masthead
        tr_
          $ td_ [class_ "email-masthead", align_ "center"]
          $ a_ [href_ "https://monoscope.tech?utm_source=transac_emails"]
          $ img_ [class_ "email-masthead_logo", src_ "https://monoscope.tech/assets/email/full_logo_l.png", alt_ "Monoscope", width_ "160", style_ "width: 160px; height: auto; background-color:#ffffff; padding:8px; border-radius:4px;"]
        -- Body
        tr_ $ td_ [class_ "email-body", width_ "100%"] content
        -- Footer
        tr_ $ td_ $ table_ [class_ "email-footer", style_ "width:100%;max-width:600px;table-layout:fixed;", align_ "center", width_ "600", cellpadding_ "0", cellspacing_ "0", role_ "presentation"] do
          -- Divider
          tr_ $ td_ [style_ "padding: 0 20px;"] $ p_ [style_ "border-top: 1px solid #dee2e7; font-size: 1px; margin: 0 0 19px; width: 100%;"] ""
          -- Description + social icons
          tr_
            $ td_ [style_ "padding: 0 20px 20px;"]
            $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0", role_ "presentation"]
            $ tr_ [class_ "footer-row"] do
              td_ [class_ "footer-col-left", style_ "vertical-align: top; width: 65%;"] do
                p_ [style_ "font-size: 12px; line-height: 1.5; text-align: left; color: #57606a; margin: 0 0 10px;"] "Monoscope \8212 monitoring and observability, built to know what\8217s happening the moment it happens. Logs, metrics, traces, and API payload monitoring with unlimited retention."
                p_ [style_ "font-size: 12px; line-height: 1.5; text-align: left; color: #57606a; margin: 0;"] "\169 2026 Monoscope."
              td_ [class_ "footer-col-right", style_ "vertical-align: top; text-align: right; width: 35%;"]
                $ table_ [class_ "footer-icons", align_ "right", cellpadding_ "0", cellspacing_ "0", role_ "presentation"]
                $ tr_ do
                  td_ [style_ "vertical-align: middle; padding: 0;"]
                    $ a_ [href_ "https://x.com/monoscope_tech", target_ "_blank"]
                    $ toHtml @Text "X"
                  td_ [style_ "vertical-align: middle; padding: 0 0 0 8px;"]
                    $ a_ [href_ "https://www.linkedin.com/company/89803535/", target_ "_blank"]
                    $ toHtml @Text "LinkedIn"
          -- Footer links
          tr_
            $ td_ [align_ "center", style_ "padding: 0 20px 19px;"]
            $ p_ [style_ "font-size: 12px; line-height: 1.5; text-align: center; color: #57606a; margin: 0;"] do
              a_ [href_ "https://monoscope.tech/changelog?utm_source=transac_emails", style_ "color: #57606a; text-decoration: none;"] "Changelog"
              toHtmlRaw @Text " &nbsp;\183&nbsp; "
              a_ [href_ "https://monoscope.tech/docs?utm_source=transac_emails", style_ "color: #57606a; text-decoration: none;"] "Docs"
              toHtmlRaw @Text " &nbsp;\183&nbsp; "
              a_ [href_ "https://monoscope.tech/legal/privacy?utm_source=transac_emails", style_ "color: #57606a; text-decoration: none;"] "Privacy"


emailBody :: Html () -> Html ()
emailBody content =
  table_ [class_ "email-body_inner", style_ "width:100%;max-width:600px;table-layout:fixed;", align_ "center", width_ "600", cellpadding_ "0", cellspacing_ "0", role_ "presentation"]
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
    b_ [class_ "error-card-label"] $ toHtml label
    " "
    toHtml val


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
emailStatRow cols =
  p_ [style_ "margin: 8px 0 20px; font-size: 13px; color: #57606a; line-height: 1.8;"]
    $ mconcat
    $ intersperse (span_ [style_ "padding: 0 8px; color: #c0c5cc;"] "\183")
    $ map
      ( \(label, val, colorM) -> span_ [] do
          toHtml label
          " "
          b_ [style_ $ "font-weight: 600;" <> maybe "" (\c -> " color: " <> c <> ";") colorM] $ toHtml val
      )
      cols


monoPre :: Text -> Html ()
monoPre = pre_ [style_ "font-family: monospace; font-size: 13px; white-space: pre-wrap; margin: 0 0 16px 0;"] . toHtml


emailGreeting :: Maybe Text -> Html ()
emailGreeting = maybe (p_ "Hi there!") \name -> p_ do "Hi "; b_ (toHtml name); "!"


-- =============================================================================
-- Simple Project Templates
-- =============================================================================

-- | @message@ is raw HTML; the CTA doubles as the fallback URL shown at the foot.
projectNotifEmail :: Text -> Text -> Maybe Text -> (Text, Text) -> (Text, Html ())
projectNotifEmail subject message userNameM (url, ctaLabel) =
  ( subject
  , emailBody do
      emailGreeting userNameM
      p_ $ toHtmlRaw message
      emailButton url ctaLabel
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl url
  )


projectInviteEmail :: Text -> Text -> Text -> (Text, Html ())
projectInviteEmail userName projectName projectUrl =
  projectNotifEmail "[···] Project Invitation" ("<b>" <> userName <> "</b> has invited you to the <b>" <> projectName <> "</b> project on Monoscope. We're excited to have you on board! Click the button below and <b>sign up using this email address</b> (or sign in with Google/GitHub if it's already linked to one) to access the project.") (Just userName) (projectUrl, "Access Project")


projectCreatedEmail :: Text -> Text -> Text -> (Text, Html ())
projectCreatedEmail userName projectName projectUrl =
  projectNotifEmail "[···] New Project Created" ("You have created a new <b>" <> projectName <> "</b> project on Monoscope.") (Just userName) (projectUrl, "Access Project")


projectDeletedEmail :: Text -> Text -> (Text, Html ())
projectDeletedEmail userName projectName =
  projectNotifEmail "[···] Project Deleted" ("You have successfully deleted the <b>" <> projectName <> "</b> project.") (Just userName) ("https://app.monoscope.tech/p/new", "Create a New Project")


-- =============================================================================
-- Runtime Errors Template
-- =============================================================================

runtimeErrorsEmail
  , escalatingErrorsEmail
  , regressedErrorsEmail
  , errorSpikesEmail
    :: Text -> Text -> Text -> [ErrorPatterns.ATError] -> Maybe Text -> Maybe Text -> Maybe Text -> (Text, Html ())
runtimeErrorsEmail = runtimeErrorVariantEmail "New Runtime Error(s)" "[···] New Runtime Exception(s) Detected - " "We've detected a new runtime error in your "
escalatingErrorsEmail = runtimeErrorVariantEmail "Escalating Runtime Error(s)" "[···] Escalating Runtime Error(s) Detected - " "We've detected escalating runtime errors in your "
regressedErrorsEmail = runtimeErrorVariantEmail "Regressed Runtime Error(s)" "[···] Regressed Runtime Error(s) Detected - " "We've detected regressed runtime errors in your "
errorSpikesEmail = runtimeErrorVariantEmail "Runtime Error Spike(s)" "[···] Runtime Error Spike(s) Detected - " "We've detected a runtime error spike in your "


-- | Email variants for log-pattern issues. Slack/Discord/PagerDuty payloads
-- live in Pkg.Mail; email is just subject + html passed to sendRenderedEmail.
logPatternEmail
  :: Text -- projectName
  -> Text -- issueUrl
  -> Text -- patternText
  -> Maybe Text -- sampleMessage
  -> Maybe Text -- logLevel
  -> Maybe Text -- serviceName
  -> Text -- sourceField
  -> Int -- occurrenceCount
  -> Bool -- isError
  -> (Text, Html ())
logPatternEmail projectName issueUrl patternText sampleMessage logLevel serviceName sourceField occurrenceCount isError =
  patternIssueEmail
    ("[···] New " <> kindLabel <> " pattern - " <> projectName)
    ("New " <> kindLabel <> " pattern detected in " <> projectName)
    issueUrl
    [ Just ("Level", fromMaybe "—" logLevel, Nothing)
    , ("Service",,Nothing) <$> serviceName
    , Just ("Occurrences", show occurrenceCount, Nothing)
    , Just ("Source", sourceField, Nothing)
    ]
    ( ("Pattern", stripSummaryBadges patternText)
        : [("Sample", truncateText 400 (stripSummaryBadges s)) | s <- maybeToList sampleMessage]
    )
  where
    kindLabel
      | isError = "error log"
      | any (\l -> T.toLower l `elem` (["warn", "warning"] :: [Text])) logLevel = "warning log"
      | otherwise = "log" :: Text


logPatternRateChangeEmail
  :: Text -- projectName
  -> Text -- issueUrl
  -> Text -- patternText
  -> Maybe Text -- logLevel
  -> Maybe Text -- serviceName
  -> Text -- direction
  -> Double -- currentRate
  -> Double -- baselineMean
  -> Double -- changePercent
  -> (Text, Html ())
logPatternRateChangeEmail projectName issueUrl patternText logLevel serviceName direction currentRate baselineMean changePercent =
  patternIssueEmail
    ("[···] Log pattern " <> direction <> " - " <> projectName)
    ("Log pattern " <> direction <> " detected in " <> projectName)
    issueUrl
    [ Just ("Level", fromMaybe "—" logLevel, Nothing)
    , ("Service",,Nothing) <$> serviceName
    , Just ("Current", show (round currentRate :: Int) <> "/h", Just "#cf222e")
    , Just ("Baseline", show (round baselineMean :: Int) <> "/h", Nothing)
    , Just ("Change", show (round changePercent :: Int) <> "%", Just "#cf222e")
    ]
    [("Pattern", truncateText 400 (stripSummaryBadges patternText))]


-- | Shared skeleton for log-pattern issue emails: heading, stat row, one
-- labelled monospace block per entry, "Open issue" CTA.
patternIssueEmail :: Text -> Text -> Text -> [Maybe (Text, Text, Maybe Text)] -> [(Text, Text)] -> (Text, Html ())
patternIssueEmail subject heading issueUrl stats blocks =
  ( subject
  , emailBody do
      h1_ $ toHtml heading
      emailStatRow $ catMaybes stats
      emailDivider
      forM_ blocks \(label, body) -> do
        p_ [style_ "margin: 0 0 8px; font-weight: 600; color: #24292f;"] $ toHtml label
        monoPre body
      emailButton issueUrl "Open issue"
  )


-- | Summary digest body for the hourly notification flush (rate-limited
-- overflow + low-signal issues). Subject is built at the call site.
digestEmail :: Text -> Text -> Text -> Int -> Html ()
digestEmail projectName inboxUrl summary total = emailBody do
  h1_ "Batched notifications"
  p_ do
    "We batched "
    b_ $ toHtml (show @Text total <> " notifications")
    " for "
    b_ $ toHtml projectName
    " to avoid spam. A sample is below."
  emailDivider
  monoPre $ unlines $ map stripSummaryBadges (lines summary)
  emailButton inboxUrl "Open inbox"


-- | @ongoingForM@ surfaces the "Still firing · 3 hours" banner and rewrites the
-- subject line when we re-notify on an issue we've already alerted about — the
-- signal that matters is *how long* this has been burning, not "new error".
runtimeErrorVariantEmail :: Text -> Text -> Text -> Text -> Text -> Text -> [ErrorPatterns.ATError] -> Maybe Text -> Maybe Text -> Maybe Text -> (Text, Html ())
runtimeErrorVariantEmail heading subjectPrefix intro projectName projectUrl errorsUrl errors chartUrlM occTextM ongoingForM =
  ( subject
  , emailBody do
      h1_ $ toHtml $ maybe heading ("Still firing: " <>) ongoingForM
      p_ do
        toHtml $ maybe intro (const ("This error is still firing in your " :: Text)) ongoingForM
        b_ $ toHtml projectName
        "."
      forM_ (catMaybes [("⏳ Still firing · " <>) <$> ongoingForM, occTextM])
        $ p_ [style_ "margin: 8px 0; font-size: 14px; font-weight: 600; color: #57606a;"]
        . toHtml @Text
      emailDivider
      forM_ (zip [0 :: Int ..] (take maxErrorCards errors)) \(i, err) ->
        errorCard projectUrl errorsUrl (if i == 0 then chartUrlM else Nothing) err
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
    subject = case ongoingForM of
      Just d
        | Just e <- viaNonEmpty head errors ->
            "[···] Still firing: " <> truncateText 80 e.errorType <> " — " <> d <> " · " <> projectName
      _ -> subjectPrefix <> projectName


-- | Explorer URL pinned to a trace id, over a 1h window centred on when the trace happened.
-- Log Explorer falls back to "last 1H" when since/from/to are absent (TimePicker.defaultSince),
-- so a bare query= link silently returns zero rows for anything older — escalating/regressed
-- alerts routinely carry an occurrence from well before that window.
--
-- >>> import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
-- >>> traceExplorerUrl "https://app" "abc" (UTCTime (fromGregorian 2026 8 22) (secondsToDiffTime 43200))
-- "https://app/log_explorer?query=trace_id%20%3D%3D%20%22abc%22&from=2026-08-22T11:30:00Z&to=2026-08-22T12:30:00Z"
traceExplorerUrl :: Text -> Text -> UTCTime -> Text
traceExplorerUrl projectUrl tid when' =
  projectUrl
    <> "/log_explorer?query="
    <> toUriStr ("trace_id == \"" <> tid <> "\"")
    <> "&from="
    <> isoT (addUTCTime (-1800) when')
    <> "&to="
    <> isoT (addUTCTime 1800 when')
  where
    isoT t = toText $ formatTime defaultTimeLocale "%FT%TZ" t


errorCard :: Text -> Text -> Maybe Text -> ErrorPatterns.ATError -> Html ()
errorCard projectUrl errorsUrl chartUrlM e =
  table_ [class_ "error-card", width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
    tr_ $ td_ [style_ "padding: 16px 0 8px 0;"] do
      p_ [class_ "error-card-header"] $ toHtml $ truncateText 120 e.errorType
      p_ [class_ "error-card-sub", style_ "word-break: break-word;"] $ toHtml $ truncateText 200 e.message
      when hasDistinctRootCause
        $ p_ [style_ "margin: 6px 0 0; font-size: 13px; color: #24292f; word-break: break-word;"]
        $ do
          b_ [style_ "color: #57606a;"] "Root cause: "
          toHtml $ truncateText 200 (e.rootErrorType <> ": " <> e.rootErrorMessage)
    tr_
      $ td_ [style_ "padding: 0 0 10px 0;"]
      $ p_ [class_ "error-card-meta", style_ "margin: 0; line-height: 1.6;"]
      $ do
        let routeText = T.strip $ fromMaybe "" e.requestMethod <> " " <> fromMaybe "" e.requestPath
            ctxMeta = filter (/= "") [fromMaybe "" e.serviceName, fromMaybe "" e.environment, toText $ formatTime defaultTimeLocale "%b %-e, %Y, %-l:%M %p" e.when]
        mconcat
          $ intersperse (span_ [style_ "color: #c0c5cc; padding: 0 6px;"] "\183")
          $ [span_ [class_ "monoscope-code", style_ "font-size: 12px;"] $ toHtml routeText | routeText /= ""]
          <> map toHtml ctxMeta
    when (e.stackTrace /= "") $ tr_ $ td_ [style_ "padding: 0 0 12px 0;"] do
      let traceLines = lines e.stackTrace
      div_ [class_ "error-card-stack"] $ toHtml $ T.intercalate "\n" $ drop (length traceLines - 2) traceLines
      when (length traceLines > 2)
        $ p_ [style_ "margin: 8px 0 0; font-size: 12px;"]
        $ a_ [href_ (projectUrl <> "/issues/by_hash/" <> e.hash), style_ linkStyle]
        $ toHtml @Text ("View full stack trace (" <> show (length traceLines) <> " lines) \8594")
    -- Mirrors Slack's "View trace" button: jumps to Log Explorer scoped to this trace id.
    whenJust (e.traceId >>= guarded (not . T.null)) \tid ->
      tr_
        $ td_ [style_ "padding: 0 0 12px 0;"]
        $ p_ [style_ "margin: 0; font-size: 12px;"]
        $ a_ [href_ (traceExplorerUrl projectUrl tid e.when), style_ linkStyle]
        $ toHtml @Text "Open trace in Log Explorer \8594"
    whenJust chartUrlM $ \url ->
      tr_
        $ td_ [style_ "padding: 8px 0 16px 0;"]
        $ img_ [src_ url, alt_ "Error trend", width_ "560", style_ "width:100%;max-width:560px;height:auto;display:block;border-radius:4px;"]
  where
    hasDistinctRootCause = e.rootErrorType /= e.errorType || e.rootErrorMessage /= e.message
    linkStyle = "color: #377cfb; text-decoration: none;"


truncateText :: Int -> Text -> Text
truncateText n t = if T.length t > n then T.take n t <> "…" else t


-- =============================================================================
-- Anomaly Endpoint Template
-- =============================================================================

anomalyEndpointEmail :: Text -> Text -> Text -> [EndpointAlertRow] -> (Text, Html ())
anomalyEndpointEmail userName projectName anomalyUrl endpointRows =
  ctaEmail
    ("[···] New Endpoint(s) Detected for Your \"" <> projectName <> "\" Project")
    True
    ( do
        emailGreeting (Just userName)
        p_ do
          "We detected new endpoints on your "
          b_ $ toHtml projectName
          " project:"
        div_ [class_ "highlight-box"]
          $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"] do
            tr_ $ td_ [style_ "padding-bottom: 8px; font-weight: 600; font-size: 15px;"] "New Endpoints:"
            forM_ (groupedByContext endpointRows) \((hostM, ctxM), labels) -> do
              whenJust hostM \h ->
                tr_ $ td_ [style_ "padding: 10px 0 2px 0; font-weight: 600; font-size: 14px; color: #111827;"] do
                  "🌐 "
                  toHtml h
                  whenJust ctxM (span_ [style_ "color: #6b7280; font-weight: 400; margin-left: 8px; font-size: 13px;"] . toHtml)
              -- When host is absent but a service/env caption is present, still show it on its own line.
              when (isNothing hostM) $ whenJust ctxM (tr_ . td_ [style_ "padding: 10px 0 2px 0; color: #6b7280; font-size: 13px;"] . toHtml)
              forM_ labels (tr_ . td_ [style_ "padding: 3px 0 3px 18px;"] . span_ [class_ "monoscope-code"] . toHtml)
    )
    "Explore the Endpoint"
    anomalyUrl


-- | Digest for hosts the retention sweep just unarchived because traffic returned.
-- One email per project per sweep, however many hosts woke up.
hostsUnarchivedEmail :: Text -> Text -> Text -> [Text] -> (Text, Html ())
hostsUnarchivedEmail userName projectName catalogUrl hosts =
  ctaEmail
    ("[···] Archived hosts are receiving traffic again on \"" <> projectName <> "\"")
    True
    ( do
        emailGreeting (Just userName)
        p_ do
          "These hosts on your "
          b_ $ toHtml projectName
          " project had been auto-archived after 30 days without traffic, but are now receiving events again, so we unarchived them:"
        div_ [class_ "highlight-box"]
          $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0"]
          $ forM_ hosts (tr_ . td_ [style_ "padding: 3px 0;"] . span_ [class_ "monoscope-code"] . toHtml)
    )
    "Open the API Catalog"
    catalogUrl


-- =============================================================================
-- Issue Assigned Template
-- =============================================================================

issueAssignedEmail :: Text -> Text -> Text -> Text -> Text -> Text -> (Text, Html ())
issueAssignedEmail userName projectName issueTitleRaw issueUrl errorType errorMessage =
  let issueTitle = stripSummaryBadges issueTitleRaw
   in ctaEmail
        ("[···] Issue Assigned: " <> issueTitle)
        False
        ( do
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
        )
        "View Issue"
        issueUrl


-- =============================================================================
-- Weekly Report Template
-- =============================================================================

data WeeklyReportData = WeeklyReportData
  { reportType :: Projects.ReportType
  , userName :: Text
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
  , performance :: V.Vector (Text, Text, Text, Int64, Double, Int64, Double)
  , slowQueries :: V.Vector (Text, Int, Int)
  , topPatterns :: V.Vector (Text, Int64, Text)
  , freeTierExceeded :: Bool
  , systemSnapshot :: Maybe Report.ReportSnapshot
  , fullReport :: Bool
  , timeZone :: Text
  , fromTime :: Text
  , toTime :: Text
  }
  deriving stock (Generic)


weeklyReportEmail :: WeeklyReportData -> (Text, Html ())
weeklyReportEmail d =
  ( reportTitle <> " · " <> d.projectName <> " · " <> d.endDate
  , table_ [class_ "email-body_inner system-report", align_ "center", width_ "720", cellpadding_ "0", cellspacing_ "0", role_ "presentation", style_ "width:100%;max-width:720px;table-layout:fixed;margin:0 auto;"]
      $ tr_
      $ td_ [class_ "content-cell", style_ "padding:0 24px 32px;"] do
        div_ [style_ "display:none;max-height:0;overflow:hidden;mso-hide:all;"] $ toHtml $ d.projectName <> ": " <> reportCount events <> " events, " <> reportCount errors <> " error events. Your services, infrastructure and issues for this period."
        h1_ [style_ "margin:0 0 6px;font-size:26px;line-height:1.2;"] $ toHtml d.projectName
        reportNote $ reportTitle <> " · " <> d.startDate <> " – " <> d.endDate <> " · " <> d.timeZone
        p_ [style_ "margin:10px 0 24px;font-size:14px;"] $ a_ [target_ "_top", href_ d.reportUrl] "View full report"
        when d.freeTierExceeded $ reportNotice "Daily ingestion cap reached" "The project reached its daily ingestion cap at report generation. Activity dropped after the cap is not included."
        reportSection (if daily then "This day" else "This week") "Observed activity and the items that need review." do
          p_ [style_ "font-size:16px;line-height:1.5;margin:0 0 14px;"]
            $ toHtml
            $ if events == 0
              then "No telemetry events were observed in this period. Check collection before drawing conclusions about system health."
              else reportCount errors <> " error events across " <> reportCount events <> " telemetry events (" <> reportRatio errors events <> ")."
          reportMetrics
            [ ("Telemetry events", reportCount events, "Logs and spans")
            , ("Error events", reportCount errors, reportRatio errors events <> " of events")
            , ("Services", show serviceCount, "Named services observed")
            , ("Server requests", reportCount requests, "Server spans only")
            ]
          forM_ d.systemSnapshot $ \snapshot -> case snapshot.issues of
            Report.Available issues -> p_ [style_ "margin:12px 0 0;font-size:14px;"] do
              a_ [target_ "_top", href_ $ d.projectUrl <> "/issues"] $ toHtml $ reportCount issues.openIssues <> " unacknowledged issues"
              toHtml $ "; " <> reportCount issues.criticalOpen <> " critical. " <> reportCount issues.newIssues <> " issues created in this period."
            Report.Unavailable -> reportNote "Issue status is unavailable for this report."
        forM_ d.systemSnapshot $ \snapshot -> do
          case snapshot.issues of
            Report.Available issues | issues.criticalOpen > 0 -> reportFinding (d.projectUrl <> "/issues") $ reportCount issues.criticalOpen <> " critical issues need review"
            _ -> pass
          case snapshot.monitors of
            Report.Available monitors | monitors.alerting > 0 -> reportFinding (d.projectUrl <> "/monitors") $ show monitors.alerting <> " monitors are alerting"
            _ -> pass
          case snapshot.infrastructure of
            Report.Available infra | infra.unready > 0 -> reportFinding (d.projectUrl <> "/infrastructure/containers" <> windowQuery) $ show infra.unready <> " infrastructure resources report not ready"
            _ -> pass
        case d.systemSnapshot of
          Nothing -> reportNotice "Historical report" "Service, infrastructure and monitor snapshots were not recorded for this report. The original metrics and issue list are shown below."
          Just snapshot -> do
            reportSection "Services" "Ordered by error events, then activity. Compare with the preceding equal-length period." do
              when (null snapshot.services) $ reportNote "No services observed in either period."
              forM_ (reportRows 8 snapshot.services) $ \s -> do
                let current = s.current
                    previous = s.previous
                    label = fromMaybe "Unnamed service" s.service
                    detail = T.intercalate " · " $ catMaybes [s.environment, Just $ if isNothing current then "No events this period" else if isNothing previous then "First observed in this comparison" else "Compared with previous period"]
                reportItem
                  (serviceUrl s.service s.environment)
                  label
                  detail
                  [ ("Events", maybe "0" (reportCount . (.events)) current, reportChange (fromIntegral . (.events) <$> current) (fromIntegral . (.events) <$> previous))
                  , ("Error events", maybe "0" (reportCount . (.errorEvents)) current, maybe "No observations" (\v -> reportRatio v.errorEvents v.events <> " of events") current)
                  , ("Requests", maybe "0" (reportCount . (.serverRequests)) current, "Server spans")
                  , ("Avg request", maybe "Not measured" reportMs (current >>= (.serverLatencyMs)), reportChange (current >>= (.serverLatencyMs)) (previous >>= (.serverLatencyMs)))
                  ]
              when (not d.fullReport && length snapshot.services > 8) $ reportMore d.reportUrl (length snapshot.services - 8) "service comparisons"
            reportSection "Infrastructure" "Latest resource usage and readiness at the period end." $ case snapshot.infrastructure of
              Report.Unavailable -> reportNote "Infrastructure metrics could not be loaded. Open infrastructure to check the latest observations."
              Report.Available infra -> do
                reportNote $ "Observation window: " <> reportTime infra.observedFrom <> " to " <> reportTime infra.observedUntil <> " UTC."
                reportMetrics [("Hosts", show infra.hosts, "Host metrics observed"), ("Containers", show infra.containers, "Container metrics observed"), ("Pod rollups", show infra.pods, "Pods without container detail")]
                when (null infra.resources) $ reportNote "No infrastructure metrics observed. Configure an OpenTelemetry host, Docker or Kubernetes receiver to include resource usage."
                forM_ (reportRows 4 infra.resources) $ \r ->
                  reportItem
                    (d.projectUrl <> (if r.scope == "Host" then "/infrastructure/hosts" else "/infrastructure/containers") <> "?from=" <> toUriStr (reportISO infra.observedFrom) <> "&to=" <> toUriStr (reportISO infra.observedUntil))
                    r.name
                    (T.intercalate " · " $ r.scope : catMaybes [r.host, r.cluster, r.namespace])
                    [("CPU / capacity", maybe "Not measured" (\x -> reportDecimal (100 * x) <> "%") r.cpuRatio, "Latest sample"), ("Memory / capacity", maybe "Not measured" (\x -> reportDecimal (100 * x) <> "%") r.memoryRatio, "Latest sample"), ("Storage used", maybe "Not measured" (\x -> reportDecimal (100 * x) <> "%") r.storageRatio, "Latest sample"), ("Readiness", maybe "Not reported" (\ready -> if ready then "Ready" else "Not ready") r.ready, maybe "" (\n -> "Restart counter: " <> reportDecimal n) r.restartCounter)]
                let remaining = infra.hosts + infra.containers + infra.pods - length (reportRows 4 infra.resources)
                when (remaining > 0) $ reportMore (d.projectUrl <> "/infrastructure/containers" <> windowQuery) remaining "infrastructure resources"
            reportSection "Issues to review" "Issue state at report generation, with new and archived groups for the period." $ case snapshot.issues of
              Report.Unavailable -> reportNote "Issue data could not be loaded."
              Report.Available issues -> do
                reportMetrics [("New in period", reportCount issues.newIssues, "Issue groups"), ("Unacknowledged", reportCount issues.openIssues, reportCount issues.criticalOpen <> " critical"), ("Acknowledged", reportCount issues.acknowledged, "Not archived"), ("Archived", reportCount issues.archivedInPeriod, "During this period")]
                when (null issues.priorities) $ reportNote "No unacknowledged issues at generation time."
                forM_ (reportRows 6 issues.priorities) $ \i ->
                  reportItem
                    (d.projectUrl <> "/issues/" <> i.id)
                    (reportClip 180 $ stripSummaryBadges i.title)
                    (T.intercalate " · " $ i.severity : maybeToList i.service)
                    [("Category", T.replace "_" " " i.issueType, ""), ("Affected requests", reportCount i.affectedRequests, "Recorded issue total")]
                when (issues.openIssues > fromIntegral (length $ reportRows 6 issues.priorities)) $ reportMore (d.projectUrl <> "/issues") (fromIntegral issues.openIssues - length (reportRows 6 issues.priorities)) "unacknowledged issues"
            reportSection "Monitors" ("Status at " <> reportTime snapshot.generatedAt <> " UTC.") $ case snapshot.monitors of
              Report.Unavailable -> reportNote "Monitor status could not be loaded."
              Report.Available monitors -> do
                reportMetrics [("Alerting", show monitors.alerting, show monitors.warning <> " warning"), ("Normal", show monitors.normal, "Evaluated monitors"), ("Paused", show monitors.paused, "Not evaluating"), ("Not evaluated", show monitors.unevaluated, "No result yet")]
                when (null monitors.observations) $ reportNote "No monitors configured. Create a monitor to evaluate the signals that matter to your system."
                forM_ (reportRows 4 monitors.observations) $ \m ->
                  reportItem
                    (d.projectUrl <> "/monitors/" <> m.id <> "/overview")
                    (reportClip 120 m.title)
                    m.status
                    [("Last value", maybe "Not evaluated" reportDecimal m.value, maybe "No evaluation recorded" (\t -> "Evaluated " <> reportTime t <> " UTC") m.lastEvaluated)]
                when (not d.fullReport && length monitors.observations > 4) $ reportMore (d.projectUrl <> "/monitors") (length monitors.observations - 4) "monitors"
        when (isNothing d.systemSnapshot && not (V.null d.anomalies))
          $ reportSection "Recorded issues" "Issues retained in this historical report."
          $ forM_ (V.take 10 d.anomalies)
          $ \i -> reportItem (d.projectUrl <> "/issues/" <> i.id.toText) (reportClip 180 $ stripSummaryBadges i.title) "" []
        case d.systemSnapshot of
          Just snapshot -> do
            reportSection "HTTP endpoint performance" "Highest-volume server HTTP requests, with the preceding period for comparison."
              $ reportObserved snapshot.performance
              $ \rows -> do
                when (null rows) $ reportNote "No HTTP server spans recorded in this period."
                forM_ (reportRows 6 rows) $ \comparison -> do
                  let e = comparison.current
                  reportItem
                    (queryUrl $ serviceFilter e.service e.environment <> endpointHostFilter e.host <> " and kind == \"server\" and attributes.http.request.method == " <> kqlQuoted e.method <> " and attributes.url.path == " <> kqlQuoted e.path)
                    (e.method <> " " <> reportClip 140 e.path)
                    (T.intercalate " · " $ catMaybes [e.service, e.environment, Just e.host])
                    [("Requests", reportCount e.requests, "Server spans"), ("Avg duration", maybe "Not measured" reportMs e.averageMs, reportChange e.averageMs (comparison.previous >>= (.averageMs)))]
                when (not d.fullReport && length rows > 6) $ reportMore d.reportUrl (length rows - 6) "endpoints"
            reportSection "Slow database operations" "Queries averaging more than 500 ms, ordered by average duration."
              $ reportObserved snapshot.databases
              $ \rows -> do
                when (null rows) $ reportNote "No database queries above this threshold were recorded."
                forM_ (reportRows 4 rows) $ \q ->
                  reportItem
                    (queryUrl $ servicePredicate q.service <> if T.length q.statement <= 512 then " and attributes.db.query.text == " <> kqlQuoted q.statement else " and duration > 500000000")
                    (reportClip 180 q.statement)
                    (fromMaybe "Unnamed service" q.service)
                    [("Avg duration", reportMs q.averageMs, ""), ("Operations", reportCount q.operations, "Recorded spans")]
                when (not d.fullReport && length rows > 4) $ reportMore d.reportUrl (length rows - 4) "slow queries"
            reportSection "Workload composition" "Recorded span kinds separate incoming requests, dependencies, and background work."
              $ reportObserved snapshot.workloads
              $ \rows -> do
                when (null rows) $ reportNote "No spans recorded in this period."
                reportMetrics [("Logs", reportCount $ sum $ map (.logs) currentServices, "Log records"), ("Spans", reportCount $ events - sum (map (.logs) currentServices), "Traced operations")]
                unless (null rows) $ table_ [width_ "100%", cellpadding_ "0", cellspacing_ "0", style_ "font-size:13px;table-layout:fixed;text-align:left;"] do
                  thead_ $ tr_ $ forM_ (["Kind", "Events", "Avg duration"] :: [Text]) $ \label -> th_ [scope_ "col", style_ "padding:8px 0;"] $ toHtml label
                  tbody_ $ forM_ rows $ \w -> tr_ do
                    td_ [style_ "padding:6px 0;"] $ a_ [target_ "_top", href_ $ queryUrl $ "kind == " <> kqlQuoted w.kind] $ toHtml $ T.toTitle w.kind
                    td_ [style_ "padding:6px 0;"] $ toHtml $ reportCount w.events
                    td_ [style_ "padding:6px 0;"] $ toHtml $ maybe "—" reportMs w.averageMs
          Nothing -> do
            reportSection "HTTP endpoint performance" "Highest-volume HTTP operations. Latency is the average recorded span duration." do
              when (V.null d.performance) $ reportNote "No HTTP endpoint spans recorded in this period."
              forM_ (V.take 8 d.performance) $ \(host, method, path, durationNs, change, count, _) ->
                reportItem
                  (d.projectUrl <> "/log_explorer" <> windowQuery)
                  (method <> " " <> reportClip 140 path)
                  host
                  [("Operations", reportCount count, "HTTP spans"), ("Avg duration", reportMs (fromIntegral durationNs / 1000000), ""), ("Change", reportDecimal change <> "%", "Versus previous period")]
            reportSection "Slow database operations" "Queries averaging more than 500 ms, ordered by average duration." do
              when (V.null d.slowQueries) $ reportNote "No database queries above this threshold were recorded."
              forM_ d.slowQueries $ \(statement, durationNs, count) ->
                reportItem
                  (d.projectUrl <> "/log_explorer" <> windowQuery)
                  (reportClip 180 statement)
                  ""
                  [("Avg duration", reportMs (fromIntegral durationNs / 1000000), ""), ("Operations", reportCount $ fromIntegral count, "Recorded spans")]
        forM_ d.systemSnapshot $ \snapshot -> when (snapshot.topPatterns == Report.Unavailable) $ reportNotice "Log patterns unavailable" "This section could not be loaded for the report."
        unless (V.null d.topPatterns)
          $ reportSection "Log patterns" "Most frequent stored patterns. Counts are lifetime totals, not limited to this reporting period."
          $ forM_ (V.take 5 d.topPatterns)
          $ \(patternText, count, source) -> reportItem (d.projectUrl <> "/log_explorer" <> windowQuery) (reportClip 180 $ stripSummaryBadges patternText) source [("Occurrences", reportCount count, "Lifetime total")]
        reportSection "Activity trends" "Charts are supplemental; the measured totals are above." do
          unless (T.null d.eventsChartUrl) $ chartBlock "Telemetry events" d.eventsChartUrl
          unless (T.null d.errorsChartUrl) $ chartBlock "Error events" d.errorsChartUrl
        reportSection "Coverage and next steps" "This report describes the telemetry Monoscope received." do
          reportNote "Missing telemetry does not mean a service was healthy. Server request metrics require server spans; infrastructure usage requires metrics and a reported capacity."
          p_ [style_ "font-size:14px;line-height:1.7;"] do
            a_ [target_ "_top", href_ d.reportUrl] "Open full report"
            " · "
            a_ [target_ "_top", href_ $ d.projectUrl <> "/reports"] "Manage report notifications"
            " · "
            a_ [target_ "_top", href_ "https://monoscope.tech/docs"] "Instrumentation guide"
  )
  where
    daily = d.reportType == Projects.RTDaily
    reportTitle = if daily then "Daily system report" else "Weekly system report"
    reportRows :: Int -> [a] -> [a]
    reportRows n rows = if d.fullReport then rows else take n rows
    currentServices = maybe [] (mapMaybe (.current) . (.services)) d.systemSnapshot
    events = maybe (fromIntegral d.totalEvents) (const $ sum $ map (.events) currentServices) d.systemSnapshot
    errors = maybe (fromIntegral d.totalErrors) (const $ sum $ map (.errorEvents) currentServices) d.systemSnapshot
    requests = sum $ map (.serverRequests) currentServices
    serviceCount = length $ ordNub $ mapMaybe (.service) currentServices
    windowQuery = "?from=" <> toUriStr d.fromTime <> "&to=" <> toUriStr d.toTime
    queryUrl query = if T.length query > 2000 then d.reportUrl else d.projectUrl <> "/log_explorer" <> windowQuery <> "&query=" <> toUriStr query
    serviceUrl service environment = queryUrl $ serviceFilter service environment
    servicePredicate :: Maybe Text -> Text
    servicePredicate = maybe "(service.name == null or service.name == \"\")" (\name -> "service.name == " <> kqlQuoted name)
    serviceFilter service environment = servicePredicate service <> " and " <> maybe "(resource.deployment.environment.name == null or resource.deployment.environment.name == \"\")" (\e -> "resource.deployment.environment.name == " <> kqlQuoted e) environment
    endpointHostFilter host = " and (attributes.server.address == " <> kqlQuoted host <> " or (attributes.server.address == null and " <> (if T.null host then "(service.name == null or service.name == \"\")" else "service.name == " <> kqlQuoted host) <> "))"


reportFinding :: Text -> Text -> Html ()
reportFinding url label = p_ [class_ "report-attention", style_ "font-size:14px;font-weight:600;line-height:1.5;margin:6px 0;color:#b42318;"] $ a_ [target_ "_top", href_ url, style_ "color:#b42318;text-decoration:underline;"] $ toHtml label


reportObserved :: Report.ReportSection a -> (a -> Html ()) -> Html ()
reportObserved section render = case section of
  Report.Available value -> render value
  Report.Unavailable -> reportNote "This section could not be loaded for the report. Open the linked workspace to investigate."


reportISO :: UTCTime -> Text
reportISO = toText . formatTime defaultTimeLocale "%FT%TZ"


reportSection :: Text -> Text -> Html () -> Html ()
reportSection title detail content = do
  h2_ [style_ "font-size:18px;font-weight:600;line-height:1.35;margin:28px 0 6px;"] $ toHtml title
  reportNote detail
  content


reportNote :: Text -> Html ()
reportNote = p_ [class_ "report-muted", style_ "font-size:13px;line-height:1.5;margin:0 0 12px;overflow-wrap:anywhere;"] . toHtml


reportNotice :: Text -> Text -> Html ()
reportNotice title detail = table_ [width_ "100%", role_ "presentation", cellpadding_ "0", cellspacing_ "0", class_ "report-notice", style_ "background:#f6f8fa;border:1px solid #dee2e7;margin:16px 0;"] $ tr_ $ td_ [style_ "padding:12px 16px;"] do
  p_ [style_ "font-size:14px;font-weight:600;margin:0 0 4px;"] $ toHtml title
  reportNote detail


reportMetrics :: [(Text, Text, Text)] -> Html ()
reportMetrics metrics = unless (null metrics)
  $ table_ [width_ "100%", role_ "presentation", cellpadding_ "0", cellspacing_ "0", style_ "table-layout:fixed;"]
  $ tr_
  $ forM_ metrics
  $ \(label, value, context) -> td_ [class_ "report-metric", style_ "vertical-align:top;padding:8px 8px 8px 0;font-size:12px;overflow-wrap:anywhere;"] do
    span_ $ toHtml label
    strong_ [style_ "display:block;font-size:16px;"] $ toHtml value
    unless (T.null context) $ span_ $ toHtml context


reportItem :: Text -> Text -> Text -> [(Text, Text, Text)] -> Html ()
reportItem url title detail metrics = table_ [width_ "100%", role_ "presentation", cellpadding_ "0", cellspacing_ "0", class_ "report-item", style_ "border-bottom:1px solid #dee2e7;"] $ tr_ $ td_ [style_ "padding:12px 0;"] do
  p_ [style_ "font-size:14px;font-weight:600;line-height:1.5;margin:0 0 3px;overflow-wrap:anywhere;word-break:break-word;"] $ a_ [target_ "_top", href_ url] $ toHtml $ reportClip 240 title
  unless (T.null detail) $ reportNote $ reportClip 320 detail
  reportMetrics metrics


reportMore :: Text -> Int -> Text -> Html ()
reportMore url remaining noun = p_ [style_ "font-size:13px;margin:12px 0;"] $ a_ [target_ "_top", href_ url] $ toHtml $ "View " <> show remaining <> " more " <> noun


reportCount :: Int64 -> Text
reportCount = formatWithCommas . fromIntegral


reportDecimal :: Double -> Text
reportDecimal = showFFloat' 1


reportMs :: Double -> Text
reportMs value = reportDecimal value <> " ms"


reportRatio :: Int64 -> Int64 -> Text
reportRatio errorCount eventCount = if eventCount == 0 then "Not measured" else showFFloat' 2 (100 * fromIntegral errorCount / fromIntegral eventCount) <> "%"


reportChange :: Maybe Double -> Maybe Double -> Text
reportChange current previous = case (current, previous) of
  (_, Nothing) -> "No previous data"
  (Nothing, _) -> "No current data"
  (Just _, Just 0) -> "No positive baseline"
  (Just c, Just p) -> let change = 100 * (c - p) / p in (if change > 0 then "+" else "") <> reportDecimal change <> "% vs previous"


reportClip :: Int -> Text -> Text
reportClip n value = if T.length value > n then T.take n value <> "…" else value


reportTime :: UTCTime -> Text
reportTime = toText . formatTime defaultTimeLocale "%d %b %H:%M"


chartBlock :: Text -> Text -> Html ()
chartBlock label url = do
  p_ [style_ "margin: 20px 0 8px; font-size: 14px; font-weight: 600; color: #57606a;"] $ toHtml label
  img_ [src_ url, alt_ $ label <> " chart", width_ "600", style_ "width:100%;max-width:600px;box-sizing:border-box;height:auto;display:block;border:1px solid #dee2e7; border-radius: 8px;"]


-- | Strip `field;style⇒value` summary badge tokens to plain text values
stripSummaryBadges :: Text -> Text
stripSummaryBadges = unwords . mapMaybe extractValue . words
  where
    extractValue token = case T.breakOn "\8658" token of
      (_, "") -> Just token
      (_, rest) -> let v = T.drop 1 rest in if T.null v then Nothing else Just v


-- =============================================================================
-- Sample Data for Previews
-- =============================================================================

sampleProjectInvite :: (Text, Html ())
sampleProjectInvite = projectInviteEmail "Jane Doe" "My API Project" "https://app.monoscope.tech/p/sample-id"


sampleProjectCreated :: (Text, Html ())
sampleProjectCreated = projectCreatedEmail "Jane Doe" "My API Project" "https://app.monoscope.tech/p/sample-id"


sampleProjectDeleted :: (Text, Html ())
sampleProjectDeleted = projectDeletedEmail "Jane Doe" "My API Project"


sampleRuntimeErrors :: Maybe Text -> (Text, Html ())
sampleRuntimeErrors chartUrlM = runtimeErrorsEmail "My API Project" "https://app.monoscope.tech/p/sample-id" "https://app.monoscope.tech/p/sample-id/issues/" [sampleError1, sampleError2, sampleError3] chartUrlM (Just "42 occurrences in last hour") (Just "3 hours")
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
sampleAnomalyEndpoint =
  anomalyEndpointEmail
    "Jane Doe"
    "My API Project"
    "https://app.monoscope.tech/p/sample-id/issues"
    [ EndpointAlertRow "POST /api/v1/orders" (Just "api.example.com") (Just "orders-service") (Just "production")
    , EndpointAlertRow "GET /api/v1/orders/:id" (Just "api.example.com") (Just "orders-service") (Just "production")
    ]


sampleIssueAssigned :: (Text, Html ())
sampleIssueAssigned = issueAssignedEmail "Jane Doe" "My API Project" "TypeError: Cannot read property 'map' of undefined" "https://app.monoscope.tech/p/sample-id/issues/by_hash/abc123" "TypeError" "Cannot read property 'map' of undefined"


sampleWeeklyReport :: Text -> Text -> (Text, Html ())
sampleWeeklyReport eventsChart errorsChart =
  weeklyReportEmail
    WeeklyReportData
      { reportType = Projects.RTWeekly
      , userName = "Jane Doe"
      , projectName = "My API Project"
      , reportUrl = "https://app.monoscope.tech/p/sample-id/reports/sample-report"
      , projectUrl = "https://app.monoscope.tech/p/sample-id"
      , startDate = "2025-01-01"
      , endDate = "2025-01-08"
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
            [ Issues.IssueSummary (UUIDId UUID.nil) "TypeError: Cannot read property 'map'" True Issues.Critical Issues.RuntimeException (Just [0, 2, 5, 12, 8, 3, 1])
            , Issues.IssueSummary (UUIDId UUID.nil) "New endpoint detected: POST /api/orders" False Issues.Warning Issues.ApiChange (Just [0, 0, 0, 1, 0, 0, 0])
            , Issues.IssueSummary (UUIDId UUID.nil) "Connection timeout pattern detected" False Issues.Info Issues.LogPattern (Just [1, 3, 2, 0, 1, 4, 2])
            , Issues.IssueSummary (UUIDId UUID.nil) "Request rate spike on /api/users" False Issues.Warning Issues.LogPatternRateChange (Just [0, 1, 1, 5, 12, 3, 0])
            ]
      , performance = V.fromList [("api.example.com", "GET", "/api/v1/users", 245000000, -12.5, 5000, 8.3), ("api.example.com", "POST", "/api/v1/orders", 890000000, 45.2, 1200, -3.1)]
      , slowQueries = V.fromList [("SELECT * FROM users WHERE email = $1", 1250000000, 3400 :: Int)]
      , topPatterns = V.fromList [("GET /api/v1/users/<*>", 4500, "URL path"), ("severity_text;badge-error⇒ERROR Failed to connect to database: connection refused at <*>", 1230, "Event summary"), ("Request timeout after <*> ms for endpoint <*>", 890, "Log body")]
      , freeTierExceeded = False
      , systemSnapshot = Just sampleSystemSnapshot
      , fullReport = False
      , timeZone = "UTC"
      , fromTime = "2025-01-01T00:00:00Z"
      , toTime = "2025-01-08T00:00:00Z"
      }


-- Synthetic preview evidence; values deliberately exercise different signal families.
sampleSystemSnapshot :: Report.ReportSnapshot
sampleSystemSnapshot =
  Report.ReportSnapshot
    { services =
        Report.compareServices
          [Report.ServiceStats (Just "checkout-api") (Just "production") 100000 280 20000 80000 220 1000 (Just 245), Report.ServiceStats (Just "payment-worker") (Just "production") 20000 50 18000 0 0 500 Nothing, Report.ServiceStats (Just "storefront") (Just "production") 5000 12 0 0 0 700 Nothing]
          [Report.ServiceStats (Just "checkout-api") (Just "production") 85000 320 17000 68000 250 1000 (Just 220), Report.ServiceStats (Just "payment-worker") (Just "production") 18000 30 16000 0 0 300 Nothing]
    , infrastructure =
        Report.Available
          $ Report.InfrastructureStats
            3
            12
            2
            [Report.InfrastructureResource "worker-node-2" "Host" Nothing (Just "production-eu") Nothing (Just 0.87) (Just 0.92) (Just 0.62) Nothing Nothing, Report.InfrastructureResource "payments-7fdc9" "Container" (Just "worker-node-2") (Just "production-eu") (Just "payments") (Just 0.72) (Just 0.84) Nothing (Just False) (Just 4), Report.InfrastructureResource "api-6bcfd" "Container" (Just "worker-node-1") (Just "production-eu") (Just "api") (Just 0.24) (Just 0.48) Nothing (Just True) (Just 0)]
            1
            (addUTCTime (-900) end)
            end
    , monitors =
        Report.Available
          $ Report.MonitorStats
            0
            1
            1
            0
            1
            [Report.MonitorObservation "sample-latency" "Checkout latency" "Alerting" (Just 842) (Just end), Report.MonitorObservation "sample-errors" "Payment error rate" "Warning" (Just 2.4) (Just end), Report.MonitorObservation "sample-queue" "Queue depth" "Not evaluated" Nothing Nothing]
    , issues =
        Report.Available
          $ Report.IssueStats
            7
            12
            2
            8
            4
            [Report.IssueObservation "sample-issue" "Payment authorization failed: upstream timeout" (Just "checkout-api") "critical" "runtime_exception" 220, Report.IssueObservation "sample-db" "Database connection pool exhausted" (Just "payment-worker") "warning" "runtime_exception" 50]
    , generatedAt = end
    , topPatterns = Report.Available []
    , performance = Report.Available [Report.EndpointComparison (Report.EndpointStats (Just "checkout-api") (Just "production") "api.example.com" "POST" "/api/v1/orders" (Just 890) 1200) (Just $ Report.EndpointStats (Just "checkout-api") (Just "production") "api.example.com" "POST" "/api/v1/orders" (Just 613) 1238)]
    , databases = Report.Available [Report.DatabaseStats (Just "checkout-api") "SELECT * FROM users WHERE email = $1" 1250 3400]
    , workloads = Report.Available [Report.WorkloadStats "server" 80000 (Just 245), Report.WorkloadStats "consumer" 2000 (Just 70), Report.WorkloadStats "client" 5000 (Just 80)]
    , ingestionCapped = Just False
    , startTime = addUTCTime (-(7 * 86400)) end
    , endTime = end
    }
  where
    end = UTCTime (fromGregorian 2025 1 8) 0


-- =============================================================================
-- Monitor Alert Templates
-- =============================================================================

monitorAlertEmail :: Text -> Text -> Text -> Double -> Double -> Text -> Maybe Text -> (Text, Html ())
monitorAlertEmail projectName monitorTitle monitorUrl currentValue threshold direction chartUrlM =
  ctaEmail
    ("[···] Monitor Alert: " <> monitorTitle <> " - " <> projectName)
    True
    ( do
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
        whenJust chartUrlM $ chartBlock "Monitor Trend"
    )
    "View Monitor"
    monitorUrl


monitorRecoveryEmail :: Text -> Text -> Text -> (Text, Html ())
monitorRecoveryEmail projectName monitorTitle =
  ctaEmail
    ("[···] Monitor Recovered: " <> monitorTitle <> " - " <> projectName)
    True
    ( do
        h1_ [style_ "color: #1a7f37;"] "Monitor Recovered"
        p_ do
          "The monitor "
          b_ $ toHtml monitorTitle
          " in your "
          b_ $ toHtml projectName
          " project has recovered and is back to normal."
    )
    "View Monitor"


freeTierUsageEmail :: Text -> Text -> Int -> Int -> Bool -> (Text, Html ())
freeTierUsageEmail projectName billingUrl used limit exceeded =
  ctaEmail
    ("[···] " <> (if exceeded then "Daily event limit reached" else "Approaching daily event limit") <> " - " <> projectName)
    True
    ( do
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
          , ("Usage", show ((used * 100) `div` max 1 limit) <> "%", if exceeded then Just "#cf222e" else Just "#bf8700")
          ]
    )
    "Upgrade Plan"
    billingUrl


-- | Shared skeleton for every notification email that ends in a CTA: content,
-- button, then divider / optional help links / signoff / fallback URL.
ctaEmail :: Text -> Bool -> Html () -> Text -> Text -> (Text, Html ())
ctaEmail subject withHelp content ctaLabel url =
  ( subject
  , emailBody do
      content
      emailButton url ctaLabel
      emailDivider
      when withHelp (emailHelpLinks >> br_ [])
      emailSignoff
      emailFallbackUrl url
  )


planUpgradedEmail :: Text -> Text -> Text -> (Text, Html ())
planUpgradedEmail projectName newPlan =
  ctaEmail
    ("[···] Plan upgraded to " <> newPlan <> " - " <> projectName)
    True
    ( do
        h1_ "Plan Upgraded"
        p_ do
          "Your "
          b_ $ toHtml projectName
          " project has been upgraded to the "
          b_ $ toHtml newPlan
          " plan. Thank you for your support!"
    )
    "View Billing"


trialEndingEmail :: Text -> Int -> Text -> (Text, Html ())
trialEndingEmail projectName daysLeft =
  ctaEmail
    ("[···] Your free trial ends in " <> show daysLeft <> " days - " <> projectName)
    True
    ( do
        h1_ $ toHtml $ "Your trial ends in " <> show @Text daysLeft <> " days"
        p_ do
          "The 30-day free trial on "
          b_ $ toHtml projectName
          " ends in "
          b_ $ toHtml (show @Text daysLeft)
          " days. Your subscription will renew automatically and you'll be billed for usage accrued during the trial."
        p_ "If you'd like to cancel before the trial ends, you can do so from the billing page."
    )
    "Manage Billing"


planDowngradedEmail :: Text -> Text -> Text -> (Text, Html ())
planDowngradedEmail projectName reason =
  ctaEmail
    ("[···] Plan downgraded to Free - " <> projectName)
    True
    ( do
        h1_ "Plan Downgraded to Free"
        p_ do
          "Your "
          b_ $ toHtml projectName
          " project has been downgraded to the Free plan because your subscription "
          toHtml reason
          "."
        p_ "On the Free plan, daily event limits apply and additional team members will be deactivated. You can re-subscribe at any time to restore full access."
    )
    "Upgrade Plan"
