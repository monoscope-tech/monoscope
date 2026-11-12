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
  NewLogPattern (..),
  logPatternEmail,
  logPatternRateChangeEmail,
  regressedErrorsEmail,
  errorSpikesEmail,
  digestEmail,
  anomalyEndpointEmail,
  hostsUnarchivedEmail,
  issueAssignedEmail,
  routineReportEmail,
  sampleRoutineReport,
  monitorAlertEmail,
  checkAlertEmail,
  monitorRecoveryEmail,
  freeTierUsageEmail,
  planUpgradedEmail,
  planDowngradedEmail,
  invoiceSettledEmail,
  paymentFailedEmail,
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
) where

import Data.Default (def)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime)
import Lucid
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Projects.Projects qualified as Projects
import Relude
import Utils (fmtDate, formatWithCommas, isoT, nonEmptyT, truncateText)
import Web.Links (LogExplorerRoutes' (..), logExplorerLinks)


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
  .monoscope-email a { color: #1d4ed8; text-decoration: none; }
  .monoscope-email a:hover { text-decoration: underline; }
  .monoscope-email a img { border: none; }
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
  }
  .monoscope-email .email-image-dark { display:none !important; max-height:0; overflow:hidden; mso-hide:all; }
  @media (prefers-color-scheme: dark) {
    .monoscope-email .email-image-light { display:none !important; }
    .monoscope-email .email-image-dark { display:block !important; max-height:none !important; overflow:visible !important; }
    .monoscope-email .report-chart { border-color:#374151 !important; }
    .monoscope-email { background-color: #111827 !important; color: #f3f4f6 !important; }
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
          $ a_ [href_ "https://monoscopehq.com?utm_source=transac_emails"]
          $ themedImages
            "https://app.monoscope.tech/public/assets/email/logo-dark-ink.png"
            "https://app.monoscope.tech/public/assets/email/logo-white-ink.png"
            [class_ "email-masthead_logo", alt_ "Monoscope", width_ "160", style_ "width:160px;height:auto;margin:0 auto;"]
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
              sequence_ $ intersperse (toHtmlRaw @Text " &nbsp;\183&nbsp; ") [a_ [href_ $ "https://monoscopehq.com/" <> path <> "?utm_source=transac_emails", style_ "color: #57606a; text-decoration: none;"] $ toHtml @Text label | (path, label) <- [("changelog", "Changelog"), ("docs", "Docs"), ("legal/privacy", "Privacy")]]


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
  a_ [href_ "https://monoscopehq.com/docs/onboarding?utm_source=transac_emails"] "onboarding guide"
  " and other "
  a_ [href_ "https://monoscopehq.com/docs?utm_source=transac_emails"] "documentation"
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

-- | @message@ is 'richP_' runs; the CTA doubles as the fallback URL shown at the foot.
projectNotifEmail :: Text -> [Text] -> Maybe Text -> (Text, Text) -> (Text, Html ())
projectNotifEmail subject message userNameM (url, ctaLabel) =
  ( subject
  , emailBody do
      emailGreeting userNameM
      richP_ message
      emailButton url ctaLabel
      emailHelpLinks
      br_ []
      emailSignoff
      emailFallbackUrl url
  )


projectInviteEmail :: Text -> Text -> Text -> (Text, Html ())
projectInviteEmail userName projectName projectUrl =
  projectNotifEmail "[···] Project Invitation" ["", userName, " has invited you to the ", projectName, " project on Monoscope. We're excited to have you on board! Click the button below and ", "sign up using this email address", " (or sign in with Google/GitHub if it's already linked to one) to access the project."] (Just userName) (projectUrl, "Access Project")


projectCreatedEmail :: Text -> Text -> Text -> (Text, Html ())
projectCreatedEmail userName projectName projectUrl =
  projectNotifEmail "[···] New Project Created" ["You have created a new ", projectName, " project on Monoscope."] (Just userName) (projectUrl, "Access Project")


projectDeletedEmail :: Text -> Text -> (Text, Html ())
projectDeletedEmail userName projectName =
  projectNotifEmail "[···] Project Deleted" ["You have successfully deleted the ", projectName, " project."] (Just userName) ("https://app.monoscope.tech/p/new", "Create a New Project")


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
data NewLogPattern = NewLogPattern
  { issueUrl :: Text
  , patternText :: Text
  , sampleMessage :: Maybe Text
  , logLevel :: Maybe Text
  , serviceName :: Maybe Text
  , sourceField :: Text
  , occurrenceCount :: Int
  , isError :: Bool
  }


logPatternEmail :: Text -> NewLogPattern -> (Text, Html ())
logPatternEmail projectName NewLogPattern{..} =
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
  richP_ ["We batched ", show total <> " notifications", " for ", projectName, " to avoid spam. A sample is below."]
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
      richP_ [maybe intro (const "This error is still firing in your ") ongoingForM, projectName, "."]
      forM_ (catMaybes [("⏳ Still firing · " <>) <$> ongoingForM, occTextM])
        $ p_ [style_ "margin: 8px 0; font-size: 14px; font-weight: 600; color: #57606a;"]
        . toHtml @Text
      emailDivider
      zipWithM_ (errorCard projectUrl errorsUrl) (chartUrlM : repeat Nothing) (take maxErrorCards errors)
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
-- "https://app/log_explorer?query=trace_id%20%3D%3D%20%22abc%22&from=2026-08-22T11%3A30%3A00Z&to=2026-08-22T12%3A30%3A00Z"
traceExplorerUrl :: Text -> Text -> UTCTime -> Text
traceExplorerUrl projectUrl tid when' = projectUrl <> logExplorerLinks.logExplorerGet (Just $ "trace_id == \"" <> tid <> "\"") Nothing Nothing (Just $ isoT $ addUTCTime (-1800) when') (Just $ isoT $ addUTCTime 1800 when') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


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
            ctxMeta = filter (/= "") [fromMaybe "" e.serviceName, fromMaybe "" e.environment, fmtDate "%b %-e, %Y, %-l:%M %p" e.when]
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
    whenJust (nonEmptyT e.traceId) \tid ->
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
        richP_ ["We detected new endpoints on your ", projectName, " project:"]
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
        richP_ ["These hosts on your ", projectName, " project had been auto-archived after 30 days without traffic, but are now receiving events again, so we unarchived them:"]
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
            richP_ ["You have been assigned to an issue in the ", projectName, " project."]
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
-- Report routine
-- =============================================================================

-- | A report routine's result: headline, then each highlight with its chart and a way in.
-- Highlights are (title, detail, chart PNG, absolute link).
routineReportEmail :: Text -> Text -> Text -> Text -> [(Text, Text, Maybe Text, Maybe Text)] -> (Text, Html ())
routineReportEmail projectTitle period headline reportUrl highlights =
  ( headline <> " · " <> projectTitle
  , emailBody do
      div_ [style_ "display:none;max-height:0;overflow:hidden;mso-hide:all;"] $ toHtml $ T.intercalate " · " $ map (\(title, _, _, _) -> title) highlights
      p_ [class_ "sub", style_ "margin:0 0 8px;"] $ toHtml $ projectTitle <> " · " <> period
      h1_ [style_ "margin:0 0 8px;"] $ toHtml headline
      forM_ highlights \(title, detail, chart, link) -> do
        h2_ [style_ "font-size:18px;margin:32px 0 8px;"] $ toHtml title
        unless (T.null detail) $ p_ $ toHtml detail
        whenJust chart $ div_ [style_ "margin-top:12px;"] . chartImage_ title
        whenJust link \url -> p_ [style_ "margin:10px 0 0;font-size:14px;"] $ a_ [href_ url, target_ "_blank"] "Explore in Monoscope \8594"
      emailButton reportUrl "Open the report"
      p_ [class_ "sub"] "This report is an AI routine. Change its schedule, delivery or instructions from the report page."
  )


sampleRoutineReport :: Maybe Text -> Maybe Text -> (Text, Html ())
sampleRoutineReport errorsChart latencyChart =
  routineReportEmail
    "Acme"
    "9 – 16 Nov 2026"
    "Checkout errors rose 4.8x after Thursday's deploy"
    "https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/ai/routines"
    [ ("Checkout 5xx up 4.8x", "Errors on POST /checkout rose from 412 to 1,980 against last week, all after Thursday 14:05 when payments v2.3.1 rolled out.", errorsChart, Just "https://app.monoscope.tech/p/00000000-0000-0000-0000-000000000000/issues")
    , ("Search p95 recovered", "p95 on GET /search fell from 1.8 s to 420 ms once the index rebuild finished on Tuesday.", latencyChart, Nothing)
    ]


chartBlock :: Text -> Text -> Html ()
chartBlock label url = do
  p_ [style_ "margin: 20px 0 8px; font-size: 14px; font-weight: 600; color: #57606a;"] $ toHtml label
  chartImage_ label url


-- | A chart PNG in both appearances. PNG responses are immutable in the CDN, so a renderer
-- revision needs fresh URLs (@render@).
chartImage_ :: Text -> Text -> Html ()
chartImage_ label url = themedImages (appearance "light") (appearance "dark") [class_ "report-chart", alt_ $ label <> " chart", width_ "600", style_ "width:100%;max-width:600px;box-sizing:border-box;height:auto;display:block;border:1px solid #dee2e7; border-radius: 8px;"]
  where
    appearance mode = url <> (if "?" `T.isInfixOf` url then "&" else "?") <> "appearance=" <> mode <> "&render=2"


-- Separate wrappers keep Outlook's inline fallback and dark-mode CSS predictable.
themedImages :: Text -> Text -> [Attribute] -> Html ()
themedImages lightUrl darkUrl attrs = do
  div_ [class_ "email-image-light"] $ img_ (src_ lightUrl : attrs)
  div_ [class_ "email-image-dark", style_ "display:none;max-height:0;overflow:hidden;mso-hide:all;"] $ img_ (src_ darkUrl : attrs)


-- | Strip `field;style⇒value` summary badge tokens to plain text values
stripSummaryBadges :: Text -> Text
stripSummaryBadges = unwords . mapMaybe extractValue . words
  where
    extractValue token = case T.breakOn "\8658" token of
      (_, "") -> Just token
      (_, rest) -> guarded (not . T.null) (T.drop 1 rest)


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
        richP_ ["The monitor ", monitorTitle, " in your ", projectName, " project has breached its threshold."]
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


-- | An uptime check or cron monitor failing: what failed, and the facts that say how.
checkAlertEmail :: Text -> Text -> [Text] -> Text -> (Text, Html ())
checkAlertEmail projectName headline facts =
  ctaEmail
    ("[···] " <> headline <> " - " <> projectName)
    True
    ( do
        h1_ $ toHtml headline
        forM_ facts $ p_ . toHtml
    )
    "View Issue"


monitorRecoveryEmail :: Text -> Text -> Text -> (Text, Html ())
monitorRecoveryEmail projectName monitorTitle =
  ctaEmail
    ("[···] Monitor Recovered: " <> monitorTitle <> " - " <> projectName)
    True
    ( do
        h1_ [style_ "color: #1a7f37;"] "Monitor Recovered"
        richP_ ["The monitor ", monitorTitle, " in your ", projectName, " project has recovered and is back to normal."]
    )
    "View Monitor"


freeTierUsageEmail :: Text -> Text -> Int -> Int -> Int -> Projects.FreeTierNotice -> (Text, Html ())
freeTierUsageEmail projectName billingUrl seen kept limit notice =
  ctaEmail
    ("[···] " <> subject <> " - " <> projectName)
    True
    ( do
        h1_ $ toHtml $ T.toTitle subject
        richP_ ["Your ", projectName, sentence]
        emailStatRow
          [ ("Events Today", formatWithCommas (fromIntegral seen), Nothing)
          , ("Kept", formatWithCommas (fromIntegral kept), Just colour)
          , ("Daily Budget", formatWithCommas (fromIntegral limit), Nothing)
          ]
    )
    "Upgrade Plan"
    billingUrl
  where
    (subject, sentence, colour) :: (Text, Text, Text) = case notice of
      Projects.NoticeSampling -> ("Events are being sampled", " project is sending more than its free tier budget, so today's events are sampled to spread that budget across the day. Errors are kept first, and a kept trace is kept whole.", "#bf8700")
      Projects.NoticeExceeded -> ("Daily event ceiling reached", " project has hit its daily free tier ceiling. New events are dropped until it resets at 00:00 UTC.", "#cf222e")


-- | Paragraph alternating plain and bold runs: @richP_ ["Your ", project, " is ready."]@.
richP_ :: [Text] -> Html ()
richP_ = p_ . zipWithM_ ($) (cycle [toHtml, b_ . toHtml])


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
planUpgradedEmail newPlan projectName =
  ctaEmail
    ("[···] Plan upgraded to " <> newPlan <> " - " <> projectName)
    True
    ( do
        h1_ "Plan Upgraded"
        richP_ ["Your ", projectName, " project has been upgraded to the ", newPlan, " plan. Thank you for your support!"]
    )
    "View Billing"


trialEndingEmail :: Text -> Int -> Text -> (Text, Html ())
trialEndingEmail projectName daysLeft =
  ctaEmail
    ("[···] Your free trial ends in " <> show daysLeft <> " days - " <> projectName)
    True
    ( do
        h1_ $ toHtml $ "Your trial ends in " <> show @Text daysLeft <> " days"
        richP_ ["The 30-day free trial on ", projectName, " ends in ", show daysLeft, " days. Your subscription will renew automatically and you'll be billed for usage accrued during the trial."]
        p_ "If you'd like to cancel before the trial ends, you can do so from the billing page."
    )
    "Manage Billing"


planDowngradedEmail :: Text -> Text -> Text -> (Text, Html ())
planDowngradedEmail reason projectName =
  ctaEmail
    ("[···] Plan downgraded to Free - " <> projectName)
    True
    ( do
        h1_ "Plan Downgraded to Free"
        richP_ ["Your ", projectName, " project has been downgraded to the Free plan because your subscription " <> reason <> "."]
        p_ $ toHtml $ "On the Free plan, ingestion is budgeted at " <> freeTierCap <> " events per day (resetting at 00:00 UTC); beyond that, events are sampled to fit. Additional team members will be deactivated. You can re-subscribe at any time to restore full access."
    )
    "Upgrade Plan"


-- | A cancelled subscription's leftover invoice was paid outside checkout: the money is in, but
-- nothing reactivated, since a cancelled subscription can't be revived.
invoiceSettledEmail :: Text -> Text -> (Text, Html ())
invoiceSettledEmail projectName =
  ctaEmail
    ("[···] Payment received - " <> projectName)
    True
    ( do
        h1_ "Payment Received"
        richP_ ["Thanks, your outstanding invoice for ", projectName, " is paid. The subscription it belonged to had already ended, so the project is still on the Free plan."]
        p_ "Reactivate from the billing page to restore full access."
    )
    "Reactivate Subscription"


-- | Provider dunning is still retrying, so nothing has changed yet; the downgrade only follows
-- if every retry fails.
paymentFailedEmail :: Text -> Text -> (Text, Html ())
paymentFailedEmail projectName =
  ctaEmail
    ("[···] Payment failed - " <> projectName)
    True
    ( do
        h1_ "Payment Failed"
        richP_ ["We couldn't charge the card for your ", projectName, " project. Your plan is unchanged while we retry over the next few days."]
        p_ $ toHtml $ "Please update your payment method. If every retry fails, the project moves to the Free plan and ingestion is budgeted at " <> freeTierCap <> " events per day."
    )
    "Update Payment Method"


freeTierCap :: Text
freeTierCap = formatWithCommas (fromIntegral Projects.freeTierDailyMaxEvents)
