module Pages.BodyWrapper (bodyWrapper, BWConfig (..), PageCtx (..), NavigationResponse (..), navigationResponse, mkPageCtx, mkAIPageCtx, withSettingsPage, settingsContentTarget, navTabAttrs) where

import Data.CaseInsensitive qualified as CI
import Data.Default (Default, def)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.List (lookup, partition)
import Data.Text qualified as T
import Data.Time (UTCTime, diffDays, diffUTCTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Tuple.Extra (fst3, uncurry3)
import Data.Vector qualified as V
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static qualified as EffReader
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (makeElement)
import Lucid.Htmx (hxConfirm_, hxDelete_, hxGet_, hxIndicator_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.Issues qualified as Issues
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pages.CommandPalette qualified as CommandPalette
import Pages.Components qualified as Components
import Pkg.DeriveUtils (UUIDId, assetUrl, viteAssetFile)
import PyF
import Relude hiding (ask)
import System.Config (AuthContext (..), DeploymentEnv (Dev), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (FieldMenuCtx (..), FreeTierStatus (..), LoadingSize (..), LoadingType (..), explorerTabs, faSprite_, fieldContextMenuItems_, fieldMenuActions, freeTierUsageBanner, infrastructureTabs, loadingIndicatorWith_, navTabAttrs, popoverPanel_, popoverTrigger_)
import Web.I18n qualified as I18n


-- | Page-handler bootstrap. Loads (session, project), reads AuthContext, and seeds
-- a BWConfig with the always-set fields (sessM, currProject, config). Caller updates
-- the returned BWConfig with handler-specific fields via record syntax.
mkPageCtx :: Projects.ProjectId -> ATAuthCtx (Projects.Session, Projects.Project, BWConfig)
mkPageCtx pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- EffReader.ask @AuthContext
  now <- Time.currentTime
  -- One indexed row read of the learned facet values — the same summary the Log Explorer
  -- sidebar renders, so the picker offers exactly the environments this project has
  -- reported. getFacetSummary ignores the time range.
  facetsM <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" now now
  activationProgressM <-
    if V.elem "checklist_dismissed" project.onboardingStepsCompleted
      then pure Nothing
      else Just <$> activationProgress pid (V.elem "Integration" project.onboardingStepsCompleted)
  conversations <- Issues.listConversations pid
  let envOptions = maybe V.empty (facetValues "resource.deployment.environment.name" . (.facetJson)) facetsM
      serviceOptions = maybe V.empty (facetValues "resource.service.name" . (.facetJson)) facetsM
  pure (sess, project, def{sessM = Just sess, currProject = Just project, config = appCtx.config, facetSummaryM = facetsM, activationProgressM, conversations, envOptions, serviceOptions, needsTagify = True, shellNow = now})
  where
    facetValues field (SchemaCatalog.FacetData m) = V.fromList $ sort [v.value | v <- HM.findWithDefault [] field m, not (T.null v.value)]


-- | Minimal authenticated shell for AI threads. The AI execution path loads its own
-- facet context when a prompt is submitted; rendering an existing conversation does
-- not need to transfer and decode that document or query onboarding progress.
-- Conversation navigation is loaded alongside the selected thread by the page handler.
mkAIPageCtx :: Projects.ProjectId -> ATAuthCtx (Projects.Session, Projects.Project, BWConfig)
mkAIPageCtx pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- EffReader.ask @AuthContext
  now <- Time.currentTime
  pure (sess, project, def{sessM = Just sess, currProject = Just project, config = appCtx.config, shellNow = now})


-- | The checklist is a statement of customer value, not of which links someone clicked.
-- Ingestion is verified by the existing Integration step, which is recorded only after an
-- event query succeeds. The remaining milestones are live facts so deleting a dashboard,
-- monitor, or test record cannot leave the shell claiming a project is activated.
data ActivationProgress = ActivationProgress
  { ingestionVerified :: Bool
  , hasDashboard :: Bool
  , hasMonitor :: Bool
  , hasTestNotification :: Bool
  }
  deriving stock (Show)


activationProgress :: (Hasql.Hasql :> es, IOE :> es) => Projects.ProjectId -> Bool -> Eff es ActivationProgress
activationProgress pid ingestionVerified = do
  rows :: [(Bool, Bool, Bool)] <-
    Hasql.interp
      [HI.sql|
        SELECT
          EXISTS (SELECT 1 FROM projects.dashboards WHERE project_id = #{pid}),
          EXISTS (SELECT 1 FROM monitors.query_monitors WHERE project_id = #{pid} AND deleted_at IS NULL),
          EXISTS (SELECT 1 FROM apis.notification_test_history WHERE project_id = #{pid} AND status = 'sent')
      |]
  let (hasDashboard, hasMonitor, hasTestNotification) = fromMaybe (False, False, False) $ listToMaybe rows
  pure ActivationProgress{..}


-- | Shortcut for settings sub-pages: sets pageTitle + isSettingsPage = True.
-- Content builder runs in ATAuthCtx so handlers can do DB/HTTP work for the body.
withSettingsPage :: Projects.ProjectId -> Text -> (Projects.Project -> ATAuthCtx (Html ())) -> ATAuthCtx (RespHeaders (Html ()))
withSettingsPage pid title build = do
  (_, project, bw) <- mkPageCtx pid
  body <- build project
  addRespHeaders $ bodyWrapper bw{pageTitle = title, isSettingsPage = True} body


-- | Right-side tippy tooltip attrs (this file's fixed placement for every tooltip).
tippyRight_ :: Text -> [Attribute]
tippyRight_ content = [term "data-tippy-placement" "right", term "data-tippy-content" content]


menu :: I18n.Language -> Projects.ProjectId -> [(Text, Text, Text)]
menu lang pid =
  [ (I18n.t lang "nav.dashboards", p "/dashboards", "dashboard")
  , (I18n.t lang "nav.explorer", p "/log_explorer", "explore")
  , (I18n.t lang "nav.issues", p "/issues", "bug")
  , ("Real User Monitoring", p "/rum", "web")
  , (I18n.t lang "nav.infrastructure", p "/infrastructure/hosts", "server")
  , (I18n.t lang "nav.api_catalog", p "/api_catalog", "swap")
  , (I18n.t lang "nav.monitors", p "/monitors", "list-check")
  , (I18n.t lang "nav.reports", p "/reports", "chart-simple")
  ]
  where
    p path = "/p/" <> pid.toText <> path


-- | Onboarding checklist widget for the sidenav
onboardingChecklist_ :: Projects.Project -> ActivationProgress -> Html ()
onboardingChecklist_ project progressState = do
  let pid = project.id.toText
      items =
        [ (progressState.ingestionVerified, ("Verify ingestion", "/p/" <> pid <> "/onboarding?step=Integration", "paper-plane"))
        , (progressState.hasDashboard, ("Create a dashboard", "/p/" <> pid <> "/dashboards", "dashboard"))
        , (progressState.hasMonitor, ("Create a monitor", "/p/" <> pid <> "/monitors", "bell"))
        , (progressState.hasTestNotification, ("Send a test notification", "/p/" <> pid <> "/settings/integrations", "envelope"))
        ]
          :: [(Bool, (Text, Text, Text))]
      doneCount = length $ filter fst items
      totalCount = length items
      progress = show doneCount <> "/" <> show totalCount :: Text
  unless (doneCount == totalCount || V.elem "checklist_dismissed" project.onboardingStepsCompleted)
    $ div_ [id_ "onboarding-checklist", class_ "mt-5 pt-3 border-t border-strokeWeak"] do
      -- Collapsed state: rocket icon
      div_ [class_ "flex justify-center group-has-[#sidenav-toggle:checked]/pg:hidden"] do
        a_ ([href_ $ "/p/" <> pid <> "/onboarding", class_ "relative tap-target"] <> tippyRight_ ("Getting Started (" <> progress <> ")")) do
          faSprite_ "rocket" "regular" "w-4 h-4 text-textWeak"
      -- Expanded state: full checklist
      div_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block bg-fillWeaker rounded-lg p-2.5"] do
        div_ [class_ "flex items-center justify-between mb-1.5 pl-1.5"] do
          div_ [class_ "flex items-center gap-2"] do
            faSprite_ "rocket" "regular" "w-2.5 h-2.5 text-textWeak"
            span_ [class_ "text-xs font-medium text-textStrong"] "Getting Started"
          div_ [class_ "flex items-center gap-3"] do
            span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml progress
            button_
              [ class_ "text-textWeak opacity-50 hover:opacity-100 hover:text-textStrong tap-target cursor-pointer"
              , Aria.label_ "Dismiss getting started checklist"
              , hxPost_ $ "/p/" <> pid <> "/onboarding/dismiss-checklist"
              , hxTarget_ "#onboarding-checklist"
              , hxSwap_ "delete"
              ]
              $ faSprite_ "xmark" "regular" "w-2.5 h-2.5"
        div_ [class_ "h-0.5 w-full bg-strokeWeak rounded-full overflow-hidden mb-2"]
          $ div_ [class_ "h-full bg-strokeBrand-strong rounded-full transition-[width]", style_ $ "width:" <> show (doneCount * 100 `div` totalCount) <> "%"] ""
        div_ [class_ "flex flex-col gap-0.5"] do
          forM_ (sortWith (Down . fst) items) \(done, (label, link, icon)) ->
            a_
              [ href_ link
              , class_ $ "flex items-center gap-2 px-2 py-1 rounded-md text-xs transition-colors " <> bool "text-textStrong font-medium hover:bg-fillWeak" "text-textWeak opacity-60" done
              ]
              do
                if done
                  then faSprite_ "circle-check" "solid" "w-3.5 h-3.5 text-textSuccess shrink-0"
                  else faSprite_ icon "regular" "w-3.5 h-3.5 shrink-0"
                span_ [class_ "truncate"] $ toHtml label


data PageCtx a = PageCtx
  { conf :: BWConfig
  , content :: a
  }
  deriving stock (Generic, Show)


instance ToHtml a => ToHtml (PageCtx a) where
  {-# INLINE toHtml #-}
  toHtml (PageCtx bwcfg child) = toHtmlRaw $ bodyWrapper bwcfg (toHtml child)
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw (PageCtx bwcfg child) = toHtmlRaw $ bodyWrapper bwcfg (toHtmlRaw child)


-- | A page URL can remain a normal, shareable document URL while an htmx navigation
-- receives only the part it is going to swap.  The fragment is intentionally supplied
-- by the page, rather than inferred from a CSS selector: callers can include the
-- corresponding OOB navigation/breadcrumb updates in the same response.
--
-- Keeping this contract here prevents every tabbed page from inventing a separate
-- @/content@ endpoint or weakening its full-page fallback.
data NavigationResponse a
  = NavigationFull (PageCtx a)
  | NavigationPartial a (Html ())


instance ToHtml a => ToHtml (NavigationResponse a) where
  toHtml = \case
    NavigationFull page -> toHtml page
    NavigationPartial _ fragment -> toHtmlRaw fragment
  toHtmlRaw = toHtml


navigationResponse :: Maybe Text -> PageCtx a -> Html () -> NavigationResponse a
navigationResponse hxRequest page fragment =
  maybe (NavigationFull page) (const $ NavigationPartial page.content fragment) hxRequest


-- TODO: Rename to pageCtx
data BWConfig = BWConfig
  { sessM :: Maybe Projects.Session
  , currProject :: Maybe Projects.Project
  , prePageTitle :: Maybe Text
  , pageTitle :: Text
  , pageTitleSuffix :: Maybe Text -- Additional breadcrumb after pageTitle (e.g., tab name)
  , pageTitleModalId :: Maybe Text -- Modal ID for renaming page title
  , pageTitleSuffixModalId :: Maybe Text -- Modal ID for renaming suffix (e.g., tab)
  , menuItem :: Maybe Text -- Use PageTitle if menuItem is not set
  , navTabs :: Maybe (Html ())
  , pageActions :: Maybe (Html ())
  , docsLink :: Maybe Text
  , isSettingsPage :: Bool
  , freeTierStatus :: FreeTierStatus
  , hideNavbar :: Bool -- When True, hides the entire navbar
  , needsGridStack :: Bool
  , needsTagify :: Bool
  , headContent :: Maybe (Html ()) -- Optional HTML content to include in the head
  , globalDrawerContent :: Maybe (Html ())
  , config :: EnvConfig -- Environment configuration for telemetry
  , facetSummaryM :: Maybe SchemaCatalog.FacetSummary
  -- ^ The project summary already fetched by 'mkPageCtx'. Page renderers that need
  -- facet values should reuse it instead of issuing the same primary-key read again.
  , envOptions :: V.Vector Text
  -- ^ Deployment environments this project has actually reported, for the app-wide picker.
  -- Seeded by 'mkPageCtx' from the learned facet values, so it is the same set the Log
  -- Explorer's facet sidebar offers and it costs one indexed row read.
  , serviceOptions :: V.Vector Text
  , activationProgressM :: Maybe ActivationProgress
  , conversations :: [Issues.ConversationSummary]
  , activeConversationId :: Maybe (UUIDId "conversation")
  , shellNow :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)


bodyWrapper :: BWConfig -> Html () -> Html ()
bodyWrapper bcfg child = do
  let isProd = bcfg.config.environment /= Dev
      -- One condition for the SDK <script> and the constructor that needs it. They were two
      -- different conditions, and the constructor's omitted `isProd`: any non-prod deploy
      -- carrying a telemetry key rendered `new Monoscope(...)` with nothing having loaded it.
      browserMonitoring = isProd && bcfg.config.enableBrowserMonitoring && bcfg.config.telemetryApiKey /= ""
      initialTheme = maybe "dark" (.theme) bcfg.sessM
      themeColor = bool "#fbfcfd" "#060708" (initialTheme == "dark")
      projectTimeZone = mfilter (not . T.null) $ (.timeZone) <$> bcfg.currProject
  doctype_
  html_ ([lang_ "en"] <> [data_ "timezone" tz | tz <- maybeToList projectTimeZone]) do
    head_ do
      title_ $ toHtml bcfg.pageTitle
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      meta_ [name_ "description", content_ $ "Monoscope — " <> bcfg.pageTitle]
      meta_ [httpEquiv_ "X-UA-Compatible", content_ "ie=edge"]
      -- Keep htmx 4's explicit inheritance instead of letting htmx-2-compat switch the
      -- implicit model back on. Attributes meant to inherit say so (`hx-preload:inherited`
      -- on body / settings nav / dashboard tabs); everything else stays local, which also
      -- stops table containers leaking their own hx-get/hx-trigger onto ~80 descendants.
      -- NB `useExplicitInheritace` is spelled that way in the compat extension.
      meta_ [name_ "htmx-config", content_ "{\"compat\":{\"useExplicitInheritace\":true}}"]
      -- favicon items
      link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/public/apple-touch-icon.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/public/favicon-32x32.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/public/favicon-16x16.png"]
      link_ [rel_ "manifest", href_ "/public/site.webmanifest"]
      link_ [rel_ "mask-icon", href_ "/public/safari-pinned-tab.svg", term "color" "#5bbad5"]
      meta_ [name_ "msapplication-TileColor", content_ "#da532c"]
      meta_ [name_ "theme-color", content_ themeColor, id_ "theme-color-meta"]

      -- Resource hints. Avatars are served from /api/avatar (same origin) and easepick's
      -- CSS is self-hosted now, so only the origins actually contacted are listed — an
      -- unused preconnect costs a real connection the browser then throws away.
      when (isProd && bcfg.config.enableBrowserMonitoring)
        $ link_ [rel_ "preconnect", href_ "https://unpkg.com"]

      -- Preload critical CSS and Inter before their stylesheet declarations are
      -- encountered. Inter uses a block period, so fallback glyphs never paint.
      link_ [rel_ "preload", href_ (assetUrl "/public/assets/css/tailwind.min.css"), term "as" "style"]
      link_ [rel_ "preload", href_ "/public/assets/fonts/InterVariable-Latin.woff2", term "as" "font", type_ "font/woff2", term "crossorigin" "anonymous"]

      -- View Transitions API (Chrome 111+, graceful fallback for others)
      meta_ [name_ "view-transition", content_ "same-origin"]
      style_
        """
        @supports (view-transition-name: root) {
          ::view-transition-old(root) { animation: vt-fade-out 150ms ease-out; }
          ::view-transition-new(root) { animation: vt-fade-in 150ms ease-in; }
        }
        @keyframes vt-fade-out { from { opacity: 1; } to { opacity: 0; } }
        @keyframes vt-fade-in { from { opacity: 0; } to { opacity: 1; } }
        """

      let css href = link_ [rel_ "stylesheet", type_ "text/css", href_ href]
          -- Toasts and the session replayer are post-interaction UI. Tagify is not:
          -- dashboard filters are above the fold, and upgrading them before their CSS
          -- arrives produces an unstyled wrapper followed by a visible resize.
          deferredCss href =
            link_ [rel_ "preload", term "as" "style", href_ href, onload_ "this.onload=null;this.rel='stylesheet'"]
          deferScript src = script_ [src_ src, defer_ "true"] ("" :: Text)
          gridStackScript src = do
            void $ script_ "window.gridStackReady = new Promise(resolve => { window.__resolveGridStack = resolve; });"
            script_ [src_ src, defer_ "true", onload_ "window.__resolveGridStack?.(window.GridStack)"] ("" :: Text)
      deferredCss $ assetUrl "/public/assets/css/thirdparty/rrweb.css"
      mapM_ css
        $ [assetUrl "/public/assets/css/thirdparty/tagify.min.css" | bcfg.needsTagify]
        <> [assetUrl "/public/assets/deps/gridstack/gridstack.min.css" | bcfg.needsGridStack]
        <> [ assetUrl "/public/assets/css/tailwind.min.css"
           , assetUrl "/public/assets/web-components/dist/css/index.css"
           ]

      fold bcfg.headContent

      mapM_
        deferScript
        $ [ assetUrl "/public/assets/deps/htmx/htmx-4.0.0.min.js"
          , -- Must load immediately after htmx: restores implicit attribute inheritance
            -- (v4 requires `:inherited` otherwise) and 4xx/5xx no-swap. The app's own
            -- listeners use v4 event names directly, so the shim's legacy-name replay is
            -- only load-bearing for third-party code (hyperscript binds the legacy load event).
            assetUrl "/public/assets/deps/htmx/htmx-2-compat.js"
          , assetUrl "/public/assets/deps/htmx/hx-preload-4.js"
          , -- Reactive DOM bindings (`:checked="…"`, `:class`, …) so UI state that CSS
            -- can't derive on its own stays declarative instead of scripted.
            assetUrl "/public/assets/deps/htmx/hx-live-4.js"
          , assetUrl "/public/assets/js/main.js"
          , -- Dropped with the htmx 4 upgrade: multi-swap and response-targets had no
            -- users (no `multi:` swaps, no hx-target-4xx/5xx) and no v4 port; idiomorph
            -- is superseded by built-in outerMorph; preload.js and json-enc-2.js call the
            -- removed defineExtension API (v4 preload ships above; json-enc and
            -- forward-page-params are re-registered in main.ts via registerExtension).
            assetUrl "/public/assets/js/thirdparty/_hyperscript_web0_9_93.min.js"
          ]
        <> [assetUrl "/public/assets/deps/tagify/tagify.min.js" | bcfg.needsTagify]
      script_ [src_ (assetUrl "/public/assets/deps/lit/lit-html.js"), type_ "module", defer_ "true"] ("" :: Text)
      when bcfg.needsGridStack $ gridStackScript $ assetUrl "/public/assets/deps/gridstack/gridstack-all.js"
      mapM_
        deferScript
        [ assetUrl "/public/assets/deps/easepick/bundle.min.js"
        , assetUrl "/public/assets/js/thirdparty/luxon.min.js"
        , assetUrl "/public/assets/js/thirdparty/popper2_11_4.min.js"
        , assetUrl "/public/assets/js/thirdparty/tippy6_3_7.umd.min.js"
        ]

      -- Vendored (not unpkg): a third-party CDN must never sit on our critical path — a
      -- slow or unreachable CDN stalled first paint for every visitor. defer keeps even
      -- the local copy off the parser.
      when browserMonitoring $ script_ [src_ $ assetUrl "/public/assets/deps/monoscope/monoscope-0.11.6.min.js", defer_ ""] ("" :: Text)

      -- Hashed URLs for assets the TS bundle references by path (see web-components/src/assets.ts).
      -- Those references can't carry a compile-time hash of their own, and /public/assets/* is
      -- served with a year-long max-age — an un-versioned sprite URL would pin returning users
      -- to a sprite sheet that predates every icon added since.
      let echartsURL = assetUrl "/public/assets/deps/echarts/echarts.min.js" :: Text
          echartsThemeURL = assetUrl "/public/assets/roma-echarts.js" :: Text
          spriteSolidURL = assetUrl "/public/assets/svgs/fa-sprites/solid.svg" :: Text
          spriteRegularURL = assetUrl "/public/assets/svgs/fa-sprites/regular.svg" :: Text
      script_
        [text|window.assetUrls = {
          echarts: "${echartsURL}", echartsTheme: "${echartsThemeURL}",
          spriteSolid: "${spriteSolidURL}", spriteRegular: "${spriteRegularURL}"
        };|]

      -- Flag for widget initialization - set to true after web-components loads
      script_ "window.widgetDepsReady = false;"
      script_ [type_ "module", src_ $(viteAssetFile "index.html")] ("" :: Text)

      when isProd
        $ script_
          [text|
        !function(e,t,n,s,u,a){e.twq||(s=e.twq=function(){s.exe?s.exe.apply(s,arguments):s.queue.push(arguments);},s.version='1.1',s.queue=[],u=t.createElement(n),u.async=!0,u.src='https://static.ads-twitter.com/uwt.js',
        a=t.getElementsByTagName(n)[0],a.parentNode.insertBefore(u,a))}(window,document,'script');
        twq('config','om5gt');
        |]

      -- The service worker was deleted on 2026-08-31. It was a checked-in workbox
      -- bundle regenerated only by a manual `make update-service-worker`, so its
      -- precache manifest pinned content-hashed chunk URLs from whenever someone last
      -- ran it (2026-08-06). 20 of its 104 entries no longer existed, and workbox
      -- fails the whole install if any precache entry 404s — so it had not installed
      -- successfully in three weeks, and could never self-correct. Precaching hashed
      -- chunks is also the wrong strategy here: they are already immutable by hash,
      -- so the SW added no caching benefit and one more way to serve a dead chunk.
      --
      -- Clients that installed it before then still have a live worker, and an
      -- unregistered SW is not removed by deleting the script. Keep this sweep until
      -- ~2026-12 so returning tabs drop theirs.
      script_
        [text|
        if ("serviceWorker" in navigator) {
          navigator.serviceWorker.getRegistrations()
            .then(rs => rs.forEach(r => r.unregister()))
            .catch(() => {});
        }
          |]
      script_
        [type_ "text/hyperscript"]
        [text|
          behavior LogItemMenuable
            on click
              if I match <.with-context-menu/> then
                remove <.log-item-cloned-menu /> then remove .with-context-menu from <.with-context-menu />
              else
                remove <.log-item-cloned-menu /> then remove .with-context-menu from <.with-context-menu /> then
                get #log-item-context-menu-tmpl.innerHTML then put it after me then add .with-context-menu to me then
                _hyperscript.processNode(.log-item-cloned-menu) then htmx.process(next <.log-item-cloned-menu/>) then
                for el in <.log-item-cloned-menu .ctx-key/> set el's textContent to (my @data-field-path or 'field') end then
                for el in <.log-item-cloned-menu .ctx-val/> set el's textContent to (my @data-field-value or 'value') end
              end
            end
          end
          -- Delegated variant for JSON trees: ONE listener on the tree container serves every
          -- field row (a per-leaf menu/listener froze the browser once enough detail panels
          -- had been opened). Clones the same #log-item-context-menu-tmpl into the clicked
          -- .log-item-field-parent, whose data-field-path/value the menu items read via closest.
          behavior FieldMenuDelegate
            on click
              set anchor to event.target.closest('.log-item-field-parent')
              if no anchor or anchor matches .with-context-menu then
                remove <.log-item-cloned-menu /> then remove .with-context-menu from <.with-context-menu />
                exit
              end
              remove <.log-item-cloned-menu /> then remove .with-context-menu from <.with-context-menu /> then
              get #log-item-context-menu-tmpl.innerHTML then put it at the end of anchor then add .with-context-menu to anchor then
              set menu to anchor.querySelector('.log-item-cloned-menu') then
              call _hyperscript.processNode(menu) then call htmx.process(menu) then
              for el in <.ctx-key/> in menu set el's textContent to (anchor's @data-field-path or 'field') end then
              for el in <.ctx-val/> in menu set el's textContent to (anchor's @data-field-value or 'value') end
            end
          end
    |]

    body_ [class_ "h-full w-full bg-bgBase text-textStrong group/pg", term "data-theme" initialTheme, term "hx-preload:inherited" "mouseover"] do
      -- Skip to main content link for keyboard users (accessibility)
      a_ [class_ "sr-only focus:not-sr-only focus:absolute focus:top-4 focus:left-4 focus:z-[100000] focus:bg-bgRaised focus:px-4 focus:py-2 focus:rounded-lg focus:text-textBrand focus:shadow-lg focus:ring-2 focus:ring-strokeFocus", href_ "#main-content"] "Skip to main content"
      -- HTMX progress bar for long operations
      div_ [id_ "htmx-progress", class_ "htmx-progress"] ""
      case bcfg.sessM of
        Nothing ->
          main_ [class_ "flex flex-col grow  h-screen overflow-y-hidden", id_ "main-content"]
            $ section_ [class_ "flex-1 overflow-y-auto"] child
        Just sess -> do
          -- Command palette (shell rendered inline, dynamic items lazy-loaded)
          whenJust bcfg.currProject \p -> CommandPalette.paletteShell_ p.id >> aiComposerShell_ p.id >> aiThreadRenameShell_
          -- Mobile nav toggle (CSS-only sidebar control, only rendered when sidebar exists)
          when (isJust bcfg.currProject)
            $ input_ [type_ "checkbox", class_ "hidden", id_ "mobile-nav-toggle", [__|on load if window.innerWidth < 768 then set #sidenav-toggle.checked to true|]]
          section_ [class_ "flex flex-row grow-0 h-screen overflow-hidden"] do
            foldMap (\project -> sideNav sess project bcfg) bcfg.currProject
            section_ [class_ "h-full overflow-y-hidden grow flex flex-col"] do
              when (sess.persistentSession.user.getUser.email == "hello@monoscope.tech") loginBanner
              -- Empty navbar anchor so OOB morph can remove non-settings navbar
              if bcfg.isSettingsPage || bcfg.hideNavbar
                then nav_ [id_ "main-navbar", class_ "hidden"] ""
                else navbar bcfg (foldMap (\p -> menu sess.lang p.id) bcfg.currProject)
              main_ [id_ "main-content", class_ "overflow-y-auto h-full grow"] do
                whenJust bcfg.currProject (\p -> freeTierUsageBanner p.id.toText bcfg.freeTierStatus)
                if bcfg.isSettingsPage
                  then maybe child (\p -> settingsWrapper p.id bcfg.pageTitle child) bcfg.currProject
                  else child
              div_ [class_ "h-0 shrink"] do
                Components.drawer_ "global-data-drawer" (isJust bcfg.globalDrawerContent) Nothing bcfg.globalDrawerContent ""
                -- Modal for copying widgets to other dashboards
                Components.modal_ "dashboards-modal" "" do
                  input_ [type_ "hidden", id_ "dashboards-modal-widget-id", name_ "widget_id"]
                  input_ [type_ "hidden", id_ "dashboards-modal-source-dashboard-id", name_ "source_dashboard_id"]
                  -- Only set for a widget that lives on no dashboard, whose definition
                  -- exists solely on the client; the picker rows PUT it as the new widget.
                  input_ [type_ "hidden", id_ "dashboards-modal-widget-json", name_ "widget_json"]
                  div_
                    [ id_ "dashboards-modal-content"
                    , class_ "dashboards-list space-y-3 max-h-160 overflow-y-auto"
                    , hxGet_ ("/p/" <> foldMap (.id.toText) bcfg.currProject <> "/dashboards?embedded=true")
                    , hxTrigger_ "loadDashboards"
                    , hxSelect_ "#itemsListPage"
                    , hxSwap_ "innerHTML"
                    , -- The source id is spread in only when set: a widget that lives on no
                      -- dashboard would otherwise send `source_dashboard_id=`, which is not
                      -- a UUID, and the whole request 400s.
                      hxVals_ "js:{copy_widget_id: document.getElementById('dashboards-modal-widget-id').value, ...(document.getElementById('dashboards-modal-source-dashboard-id').value ? {source_dashboard_id: document.getElementById('dashboards-modal-source-dashboard-id').value} : {})}"
                    ]
                    $ replicateM_ 3 (div_ [class_ "skeleton h-16 w-full"] "")

      -- Mobile nav backdrop (at body level, after section, so it paints on top)
      when (isJust bcfg.sessM && isJust bcfg.currProject)
        $ label_ [term "for" "mobile-nav-toggle", class_ "fixed inset-0 bg-black/50 backdrop-blur-xs z-40 hidden group-has-[#mobile-nav-toggle:checked]/pg:max-md:block cursor-default", Aria.label_ "Close menu"] ""
      when isProd $ externalHeadScripts_ bcfg.config
      globalTemplates_
      when isProd $ script_ [async_ "true", src_ "https://www.googletagmanager.com/gtag/js?id=AW-11285541899"] ("" :: Text)
      script_
        [text|
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', 'AW-11285541899');

          // These two stay inline rather than moving to the bundle with the rest of the page
          // chrome: the theme block below calls getCookie while the document is still
          // parsing, so it can apply the saved theme before first paint. The bundle is a
          // deferred module and does not run until parsing finishes, which would both throw
          // here and flash the wrong theme.
          // NB: string concatenation, not JS template literals. This block is a neat-interpolation
          // quasi-quote, whose own dollar-brace syntax would consume them before GHC saw them.
          function setCookie(cname, cvalue, exdays = 365) {
            const d = new Date();
            d.setTime(d.getTime() + (exdays * 24 * 60 * 60 * 1000));
            document.cookie = cname + "=" + cvalue + ";expires=" + d.toUTCString() + ";path=/";
          }

          function getCookie(cname) {
            const name = cname + "=";
            const ca = decodeURIComponent(document.cookie).split(';');
            for (let i = 0; i < ca.length; i++) {
              const c = ca[i].trim();
              if (c.startsWith(name)) return c.substring(name.length);
            }
            return "";
          }

          function syncThemeToggles(theme) {
            ['dark-mode-toggle', 'dark-mode-toggle-navbar'].forEach(id => {
              const el = document.getElementById(id);
              if (el) el.checked = theme === 'dark';
            });
          }

          function applyTheme(theme) {
            document.body.setAttribute('data-theme', theme);
            document.getElementById('theme-color-meta').content = theme === 'dark' ? '#060708' : '#fbfcfd';
          }

          // Suppresses transitions across the swap so the theme change doesn't flash.
          function applyThemeSmooth(theme) {
            document.documentElement.classList.add('no-transition');
            applyTheme(theme);
            requestAnimationFrame(() => document.documentElement.classList.remove('no-transition'));
          }

          function toggleDarkMode() {
            const newTheme = document.body.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
            applyThemeSmooth(newTheme);
            setCookie('theme', newTheme, 365);
            syncThemeToggles(newTheme);
          }

          // Match the server default: dark until the user explicitly chooses light.
          // OS theme changes must not override the dashboard's theme.
          applyTheme(getCookie('theme') === 'light' ? 'light' : 'dark');

          window.addEventListener('DOMContentLoaded', () => syncThemeToggles(document.body.getAttribute('data-theme')));
      |]
      let userM = (.persistentSession.user.getUser) <$> bcfg.sessM
          email = show $ maybe "" (.email) userM
          name = maybe "" (\u -> u.firstName <> " " <> u.lastName) userM
          pidT = foldMap (.id.toText) bcfg.currProject
          pTitle = foldMap (.title) bcfg.currProject
          telemetryApiKey = bcfg.config.telemetryApiKey
          telemetryServiceName = bcfg.config.telemetryServiceName
      script_
        [text| window.addEventListener("load", (event) => {
                  if (typeof posthog !== 'undefined' && posthog && posthog.people && posthog.people.set_once) {
                    posthog.people.set_once({email: ${email}, name: "${name}", projectId: "${pidT}", projectTitle: "${pTitle}"});
                  }
                  // echarts.connect('default');
                });
      |]
      -- Constructed on DOMContentLoaded, not inline: the SDK <script> above is `defer`, so it
      -- executes after the parser finishes — after this inline script would have run. Calling
      -- the constructor directly threw `Monoscope is not defined` on every page load and no
      -- browser telemetry was ever collected from the dashboard. Deferred scripts are
      -- guaranteed to run before DOMContentLoaded fires, so this is the earliest safe moment.
      when browserMonitoring
        $ let enableReplay = bool "false" "true" bcfg.config.enableSessionReplay
           in script_
                [text|
                  window.addEventListener('DOMContentLoaded', () => {
                    window.monoscope = new Monoscope({
                      apiKey: "${telemetryApiKey}",
                      serviceName: "${telemetryServiceName}",
                      debug: undefined,
                      sessionReplay: ${enableReplay},
                      user: {
                        email: ${email},
                        name: "${name}"
                      }
                    });
                  });
              |]
  where
    dialog_ :: [Attribute] -> Html () -> Html ()
    dialog_ = with (makeElement "dialog")

    -- Focused prompt capture shared by the sidebar's chat and routine launchers.
    -- A conversation is created only after submission, so abandoned drafts never pollute
    -- the thread list. The resulting page uses the same history renderer as issue chat.
    aiComposerShell_ :: Projects.ProjectId -> Html ()
    aiComposerShell_ pid =
      dialog_ [id_ "ai-composer-modal", class_ "modal !items-start px-4 pt-[15vh] backdrop:backdrop-blur-sm", term "aria-labelledby" "ai-composer-title"] do
        form_ [method_ "dialog", class_ "modal-backdrop"] $ button_ [Aria.label_ "Close composer"] "close"
        form_
          [ id_ "ai-composer-panel"
          , hxPost_ $ "/p/" <> pid.toText <> "/ai"
          , hxSwap_ "none"
          , term "hx-disabled-elt" "find button[type='submit']"
          , class_ "modal-box group/composer !w-full !max-w-lg !overflow-visible !rounded-none !border-0 !bg-transparent !p-0 !shadow-none flex flex-col"
          ]
          do
            div_ [class_ "mb-2 flex items-center justify-between px-3"] do
              h2_ [id_ "ai-composer-title", class_ "text-xs font-medium uppercase tracking-wider text-white"] do
                span_ [class_ "group-has-[#ai-composer-routine:checked]/composer:hidden"] "New chat"
                span_ [class_ "hidden group-has-[#ai-composer-routine:checked]/composer:inline"] "New routine"
              button_ [type_ "button", Aria.label_ "Close composer", class_ "inline-flex h-8 w-8 cursor-pointer items-center justify-center rounded-lg text-white/70 hover:bg-white/10 hover:text-white focus-visible:outline-2 focus-visible:outline-white active:scale-[0.96] transition-[color,background-color,scale]", onclick_ "this.closest('dialog').close()"]
                $ faSprite_ "xmark" "regular" "h-3.5 w-3.5"
            div_ [class_ "w-full overflow-hidden rounded-lg border border-base-300 bg-bgBase shadow-2xl"] do
              fieldset_ [class_ "m-2 grid w-fit grid-cols-2 rounded-lg bg-fillWeak p-0.5 text-xs", Aria.label_ "Conversation type"] do
                input_ [type_ "radio", name_ "mode", value_ "chat", id_ "ai-composer-chat", class_ "sr-only peer/chat", checked_]
                label_ [Lucid.for_ "ai-composer-chat", class_ "cursor-pointer rounded-md px-3 py-1.5 text-center font-medium text-textWeak peer-checked/chat:bg-bgRaised peer-checked/chat:text-textStrong peer-checked/chat:shadow-xs peer-focus-visible/chat:outline-2 peer-focus-visible/chat:outline-offset-2 transition-[color,background-color,box-shadow] duration-100"] "Chat"
                input_ [type_ "radio", name_ "mode", value_ "routine", id_ "ai-composer-routine", class_ "sr-only peer/routine"]
                label_ [Lucid.for_ "ai-composer-routine", class_ "cursor-pointer rounded-md px-3 py-1.5 text-center font-medium text-textWeak peer-checked/routine:bg-bgRaised peer-checked/routine:text-textStrong peer-checked/routine:shadow-xs peer-focus-visible/routine:outline-2 peer-focus-visible/routine:outline-offset-2 transition-[color,background-color,box-shadow] duration-100"] "Routine"
              div_ [class_ "border-b border-base-300 px-3"] do
                textarea_
                  [ id_ "ai-composer-input"
                  , name_ "query"
                  , rows_ "3"
                  , maxlength_ "4000"
                  , required_ "required"
                  , autofocus_
                  , placeholder_ "Describe what you want Monoscope to investigate…"
                  , Aria.label_ "Instructions"
                  , class_ "no-focus-ring min-h-24 w-full resize-none bg-transparent py-3 text-sm leading-5 text-textStrong outline-none placeholder:text-textDisabled"
                  , [__|on keydown[key=='Enter' and (metaKey or ctrlKey)] halt the event then call my form.requestSubmit()|]
                  ]
                  ""
              div_ [class_ "hidden group-has-[#ai-composer-routine:checked]/composer:flex items-center justify-between gap-3 border-b border-base-300 px-4 py-3"] do
                label_ [Lucid.for_ "ai-routine-interval", class_ "text-xs font-medium text-textStrong"] "Run every"
                select_ [id_ "ai-routine-interval", name_ "intervalMinutes", class_ "select select-sm h-8 min-h-8 border-strokeWeak bg-bgRaised text-xs text-textStrong"] do
                  option_ [value_ "60", class_ "bg-bgRaised text-textStrong"] "Hour"
                  option_ [value_ "1440", selected_ "selected", class_ "bg-bgRaised text-textStrong"] "Day"
                  option_ [value_ "10080", class_ "bg-bgRaised text-textStrong"] "Week"
              div_ [class_ "flex justify-end gap-1.5 p-2"] do
                button_ [type_ "button", class_ "btn btn-sm btn-ghost cursor-pointer shadow-none active:scale-[0.96] transition-transform", onclick_ "this.closest('dialog').close()"] "Cancel"
                button_ [type_ "submit", class_ "btn btn-sm btn-primary cursor-pointer gap-1.5 shadow-none active:scale-[0.96] transition-transform"] do
                  span_ [class_ "group-has-[#ai-composer-routine:checked]/composer:hidden"] "Start chat"
                  span_ [class_ "hidden group-has-[#ai-composer-routine:checked]/composer:inline"] "Start routine"
                  faSprite_ "arrow-right" "regular" "h-3 w-3"
            div_ [class_ "cmd-palette-hints mt-3 flex items-center justify-center gap-6 text-xs text-white drop-shadow dark:text-white/80"] do
              div_ [class_ "flex items-center gap-1.5"] do
                "Start"
                kbd_ [class_ "kbd kbd-xs"] "⌘/Ctrl"
                kbd_ [class_ "kbd kbd-xs"] "↵"
              div_ [class_ "flex items-center gap-1.5"] do
                "Close"
                kbd_ [class_ "kbd kbd-xs"] "esc"

    aiThreadRenameShell_ :: Html ()
    aiThreadRenameShell_ =
      dialog_ [id_ "ai-thread-rename-modal", class_ "modal !items-start px-4 pt-[15vh] backdrop:backdrop-blur-sm", term "aria-labelledby" "ai-thread-rename-title"] do
        form_ [method_ "dialog", class_ "modal-backdrop"] $ button_ [Aria.label_ "Close rename dialog"] "close"
        form_ [id_ "ai-thread-rename-form", hxPost_ "#", hxSwap_ "none", class_ "modal-box !w-full !max-w-lg !overflow-visible !rounded-none !border-0 !bg-transparent !p-0 !shadow-none flex flex-col"] do
          div_ [class_ "mb-2 flex items-center justify-between px-3"] do
            h2_ [id_ "ai-thread-rename-title", class_ "text-xs font-medium uppercase tracking-wider text-white"] "Rename chat"
            button_ [type_ "button", Aria.label_ "Close rename dialog", class_ "inline-flex h-8 w-8 cursor-pointer items-center justify-center rounded-lg text-white/70 hover:bg-white/10 hover:text-white focus-visible:outline-2 focus-visible:outline-white active:scale-[0.96] transition-[color,background-color,scale]", onclick_ "this.closest('dialog').close()"]
              $ faSprite_ "xmark" "regular" "h-3.5 w-3.5"
          div_ [class_ "w-full overflow-hidden rounded-lg border border-base-300 bg-bgBase shadow-2xl"] do
            div_ [class_ "border-b border-base-300 px-4 py-3"]
              $ label_ [Lucid.for_ "ai-thread-rename-input", class_ "flex flex-col gap-1.5 text-xs font-medium text-textStrong"] do
                span_ "Chat title"
                input_ [id_ "ai-thread-rename-input", name_ "conversationTitle", maxlength_ "100", required_ "required", autofocus_, autocomplete_ "off", class_ "no-focus-ring w-full bg-transparent py-2 text-sm font-normal outline-none placeholder:text-textDisabled", placeholder_ "Production changes today"]
            div_ [class_ "flex justify-end gap-1.5 p-2"] do
              button_ [type_ "button", class_ "btn btn-sm btn-ghost cursor-pointer shadow-none active:scale-[0.96] transition-transform", onclick_ "this.closest('dialog').close()"] "Cancel"
              button_ [type_ "submit", class_ "btn btn-sm btn-primary cursor-pointer shadow-none active:scale-[0.96] transition-transform"] "Save"


projectsDropDown :: Projects.Project -> V.Vector Projects.Project -> Html ()
projectsDropDown currProject projects = do
  let pidTxt = currProject.id.toText
  div_
    [ term "data-menu" "true"
    , class_ "origin-top-right z-40 bg-bgRaised p-2 w-[18rem] rounded-xl shadow-lg border border-strokeWeak"
    ]
    do
      when (V.length projects > 1)
        $ div_ [class_ "p-1 pb-2"] do
          div_ [class_ "relative"] do
            div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"] $ faSprite_ "magnifying-glass" "regular" "h-4 w-4 text-textWeak"
            input_
              [ type_ "search"
              , Aria.label_ "Search projects"
              , class_ "pl-10 w-full bg-fillWeak rounded-lg border-0 py-2 px-3 text-sm"
              , placeholder_ "Search..."
              , [__|on input
                  show .project_item in #projectsContainer when its textContent.toLowerCase() contains my value.toLowerCase()
                  then set visibleCount to #projectsContainer.querySelectorAll('.project_item:not([style*="display: none"])').length
                  if visibleCount == 0 remove .hidden from #noProjectsFound
                  else add .hidden to #noProjectsFound end|]
              ]
      div_ [class_ "space-y-0.5 max-h-[50vh] overflow-y-auto", id_ "projectsContainer"] do
        projects & mapM_ \project -> do
          let isActive = currProject.id == project.id
          a_ [class_ $ "flex justify-between items-center py-2 px-2.5 rounded-lg transition-colors duration-100 project_item min-w-0" <> bool " hover:bg-fillHover" " bg-fillWeak font-medium" isActive, href_ $ "/p/" <> project.id.toText] do
            span_ [class_ "truncate"] $ toHtml project.title
            when isActive $ faSprite_ "check" "regular" "h-3.5 w-3.5 text-textBrand shrink-0"
        p_ [class_ "hidden text-textWeak text-sm text-center py-4", id_ "noProjectsFound"] "No matching projects"
      let actionLink attrs icon label = a_ (class_ "flex items-center gap-2 py-2 px-2.5 rounded-lg hover:bg-fillHover cursor-pointer text-sm" : attrs) $ faSprite_ icon "regular" "h-3.5 w-3.5 text-textWeak" >> span_ label
      div_ [class_ "border-t border-strokeWeak mt-1 pt-1"] do
        actionLink [href_ "/"] "grid" "All projects"
        actionLink [href_ "/p/new"] "plus" "New project"
        when (currProject.paymentPlan `elem` ["UsageBased", "GraduatedPricing"])
          $ actionLink [hxGet_ [text| /p/$pidTxt/manage_subscription |]] "dollar-sign" "Manage billing"


sideNav :: Projects.Session -> Projects.Project -> BWConfig -> Html ()
sideNav sess project bcfg = aside_ [class_ "group/nav relative z-40 bg-fillWeaker max-md:bg-bgBase text-sm max-md:fixed max-md:z-50 max-md:w-60 max-md:h-full max-md:-translate-x-full max-md:transition-transform group-has-[#mobile-nav-toggle:checked]/pg:max-md:translate-x-0 md:min-w-13 md:w-13 md:shrink-0 group-has-[#sidenav-toggle:checked]/pg:md:w-60 h-screen md:transition-[width] duration-200 ease-out flex flex-col justify-between", id_ "side-nav-menu"] do
  span_ [Aria.hidden_ "true", class_ "pointer-events-none absolute inset-x-0 bottom-0 h-64 bg-gradient-to-t from-fillBrand-weak/30 via-fillBrand-weak/10 to-transparent"] ""
  -- Right border resize handle (desktop only)
  label_ [term "for" "sidenav-toggle", class_ "max-md:hidden absolute right-0 top-0 bottom-0 w-1 border-r border-strokeWeak cursor-e-resize group-has-[#sidenav-toggle:checked]/pg:cursor-w-resize hover:border-strokeBrand-strong hover:w-1 transition-colors z-10", Aria.label_ "Toggle sidebar"] ""
  div_ [class_ "relative z-[1] px-1 group-has-[#sidenav-toggle:checked]/pg:px-2 flex flex-col grow min-h-0 overflow-hidden"] do
    input_ ([type_ "checkbox", class_ "hidden", id_ "sidenav-toggle", [__|on change call setCookie("isSidebarClosed", `${me.checked}`) then send "toggle-sidebar" to <body/>|]] <> [checked_ | sess.isSidebarClosed])
    -- Project picker + Toggle (context + sidebar control)
    div_ [class_ "pt-2 flex flex-col group-has-[#sidenav-toggle:checked]/pg:flex-row items-center gap-1 group-has-[#sidenav-toggle:checked]/pg:gap-2 group/ctx"] do
      div_ [class_ "block group-has-[#sidenav-toggle:checked]/pg:flex-1 group-has-[#sidenav-toggle:checked]/pg:min-w-0"] do
        button_
          ( [ type_ "button"
            , class_ "flex flex-row w-full text-textStrong hover:bg-fillHover gap-2 items-center rounded-lg cursor-pointer py-1 justify-center group-has-[#sidenav-toggle:checked]/pg:px-2 group-has-[#sidenav-toggle:checked]/pg:bg-fillWeak transition-colors duration-100"
            , Aria.haspopup_ "dialog"
            , Aria.label_ $ "Switch project, current: " <> project.title
            ]
              <> tippyRight_ (project.title <> " — Switch project")
              <> popoverTrigger_ "project-picker-pop"
          )
          do
            span_ [class_ "w-8 h-8 group-has-[#sidenav-toggle:checked]/pg:w-6 group-has-[#sidenav-toggle:checked]/pg:h-6 rounded-lg group-has-[#sidenav-toggle:checked]/pg:rounded-md bg-fillBrand-weak text-textBrand text-sm group-has-[#sidenav-toggle:checked]/pg:text-xs font-semibold flex items-center justify-center shrink-0"] $ toHtml $ T.take 1 project.title
            span_ [class_ "grow hidden group-has-[#sidenav-toggle:checked]/pg:block overflow-x-hidden whitespace-nowrap truncate"] $ toHtml project.title
            span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:flex shrink-0"] $ faSprite_ "angles-up-down" "regular" "w-4 text-textWeak"
        div_ ([class_ "dropdown group-has-[#sidenav-toggle:not(:checked)]/pg:dropdown-right group-has-[#sidenav-toggle:not(:checked)]/pg:ml-2", role_ "dialog", Aria.label_ "Project switcher"] <> popoverPanel_ "project-picker-pop") $ projectsDropDown project (Projects.getProjects $ Projects.projects sess.persistentSession)
      -- Toggle sidebar (desktop: toggles sidenav-toggle, mobile: closes mobile-nav-toggle)
      label_
        ( [ term "for" "sidenav-toggle"
          , role_ "button"
          , tabindex_ "0"
          , class_ "max-md:hidden cursor-pointer text-textWeak hover:text-textStrong flex items-center justify-center group-has-[#sidenav-toggle:checked]/pg:text-strokeStrong transition-colors duration-150 focus-visible:outline-2 focus-visible:outline-offset-2"
          , Aria.label_ "Toggle sidebar"
          , Aria.expanded_ (bool "true" "false" sess.isSidebarClosed)
          , Aria.controls_ "side-nav-menu"
          , [__|on keydown[key=='Enter' or key==' ']
                  halt the event
                  call me.click()
                end
                on change from #sidenav-toggle
                  if #sidenav-toggle.checked
                    set @aria-expanded to 'false'
                  else
                    set @aria-expanded to 'true'
                  end
                end|]
          ]
            <> tippyRight_ "Expand sidebar"
        )
        do
          faSprite_ "side-chevron-left-in-box" "regular" "h-3.5 w-3.5 rotate-180 group-has-[#sidenav-toggle:checked]/pg:rotate-0 group-has-[#sidenav-toggle:checked]/pg:h-5 group-has-[#sidenav-toggle:checked]/pg:w-5"
      label_ [term "for" "mobile-nav-toggle", role_ "button", tabindex_ "0", class_ "md:!hidden max-md:flex cursor-pointer text-strokeStrong min-w-6 min-h-6 items-center focus-visible:outline-2 focus-visible:outline-offset-2", Aria.label_ "Close menu", Components.keyboardActivateAttr_] $ faSprite_ "side-chevron-left-in-box" "regular" "h-5 w-5 pointer-events-none"
    -- Search
    let searchScript = [__|on click send paletteToggle to #cmd-palette-global|]
    div_ [class_ "mt-3 pb-2 flex items-center justify-center"] do
      -- Expanded: search input trigger
      button_ [class_ "border border-strokeWeak hidden group-has-[#sidenav-toggle:checked]/pg:flex items-center gap-2 px-2 py-1 flex-1 rounded-md text-textWeak text-sm hover:bg-fillHover hover:text-textStrong transition-colors duration-100 cursor-pointer", searchScript] do
        faSprite_ "sidebar-search" "regular" "w-3.5 h-3.5 shrink-0"
        span_ [class_ "flex-1 text-left"] "Search..."
        kbd_ [class_ "kbd kbd-xs"] "\x2318K"
      -- Collapsed: search icon
      button_ ([class_ "group-has-[#sidenav-toggle:checked]/pg:hidden flex items-center justify-center p-2 rounded-lg hover:bg-fillHover text-textWeak hover:text-textStrong cursor-pointer transition-colors duration-100", searchScript, Aria.label_ "Search"] <> tippyRight_ "Search (\x2318K)") do
        faSprite_ "sidebar-search" "regular" "w-4 h-4"
    fieldset_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:grid grid-cols-2 rounded-lg bg-fillWeak p-0.5 text-xs"] do
      legend_ [class_ "sr-only"] "Sidebar view"
      input_ [type_ "radio", name_ "nav-view", id_ "nav-view-menu", class_ "sr-only peer/menu", checked_]
      label_ [term "for" "nav-view-menu", class_ "cursor-pointer rounded-md px-2 py-1 text-center text-textWeak peer-checked/menu:bg-bgRaised peer-checked/menu:text-textStrong peer-checked/menu:shadow-xs peer-focus-visible/menu:outline-2 peer-focus-visible/menu:outline-offset-2 transition-[color,background-color,box-shadow] duration-100"] "Navigate"
      input_ [type_ "radio", name_ "nav-view", id_ "nav-view-ai", class_ "sr-only peer/ai"]
      label_ [term "for" "nav-view-ai", class_ "cursor-pointer rounded-md px-2 py-1 text-center text-textWeak peer-checked/ai:bg-bgRaised peer-checked/ai:text-textStrong peer-checked/ai:shadow-xs peer-focus-visible/ai:outline-2 peer-focus-visible/ai:outline-offset-2 transition-[color,background-color,box-shadow] duration-100"] "Assistant"
    nav_ [id_ "main-sidenav", class_ "mt-2 min-h-0 overflow-x-hidden overflow-y-auto flex flex-col gap-1 text-textWeak group-has-[#nav-view-ai:checked]/nav:hidden [&_.main-nav-link.active]:bg-fillBrand-weak [&_.main-nav-link.active]:text-textStrong [&_.main-nav-link.active]:font-medium [&_.main-nav-link.active]:border-l-strokeBrand-strong [&_.main-nav-link.active]:border-y-transparent [&_.main-nav-link.active]:border-r-transparent [&_.main-nav-link.active_.nav-icon]:text-textBrand", [__|on click set #mobile-nav-toggle.checked to false end|]] do
      let pidTxt = project.id.toText
          flyoutLink (linkText, link) =
            a_ ([href_ link, class_ "flex gap-2.5 items-center px-3 py-2 text-sm text-textWeak hover:bg-fillWeak hover:text-textStrong whitespace-nowrap"] <> navTabAttrs)
              $ span_ [] (toHtml linkText)
          renderNavItem mTitle mUrl fIcon = do
            -- Page handlers name the section in English; labels may be translated.
            let sectionTitle = fromMaybe mTitle $ lookup mUrl [(url, title) | (title, url, _) <- menu I18n.En project.id]
                isActive = fromMaybe pageTitle menuItem == sectionTitle
                activeCls = bool "" " active" isActive
                flyoutItems = navFlyoutItems pidTxt sectionTitle
                hasFlyout = not (null flyoutItems)
            ( if hasFlyout
                then
                  div_
                    [ class_ "relative group/flyout"
                    , [__|on mouseenter
                        set flyout to my.querySelector('.nav-flyout')
                        set sidebarBounds to my.closest('aside').getBoundingClientRect()
                        set flyoutLeft to Math.max(8, Math.min(sidebarBounds.right + 4, window.innerWidth - flyout.offsetWidth - 8))
                        set flyoutTop to Math.max(8, Math.min(my.getBoundingClientRect().top, window.innerHeight - flyout.offsetHeight - 8))
                        set flyout.style.left to `${flyoutLeft}px`
                        set flyout.style.top to `${flyoutTop}px`
                      end|]
                    ]
                else id
              )
              do
                a_
                  ( -- The visible label is display:none while the rail is collapsed, which
                    -- takes it out of the accessibility tree too — without this the whole
                    -- sidenav is a column of unnamed links. Same string, so it never
                    -- disagrees with the label shown when expanded.
                    [ href_ mUrl
                    , Aria.label_ mTitle
                    , class_ $ "main-nav-link relative group-has-[#sidenav-toggle:checked]/pg:px-4 gap-3 py-2 flex no-wrap shrink-0 justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start items-center rounded-lg overflow-x-hidden overflow-y-hidden hover:bg-fillWeak hover:text-textStrong transition-colors duration-100" <> activeCls
                    ]
                      <> [term "aria-current" "page" | isActive]
                      <> if hasFlyout then [] else tippyRight_ mTitle <> navTabAttrs
                  )
                  do
                    faSprite_ fIcon "regular" "nav-icon w-4 h-4 shrink-0"
                    span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block whitespace-nowrap truncate"] $ toHtml mTitle
                    when hasFlyout $ span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block ml-auto text-textWeak"] $ faSprite_ "chevron-right" "regular" "w-3 h-3"
                when hasFlyout
                  $ div_ [class_ "nav-flyout max-md:hidden invisible opacity-0 group-hover/flyout:visible group-hover/flyout:opacity-100 fixed top-0 left-0 z-50 min-w-44 bg-bgRaised border border-strokeWeak rounded-lg shadow-md py-1.5 transition-[opacity,visibility] duration-100"]
                  $ mapM_ flyoutLink flyoutItems
      -- Three, not two: the split is positional, so moving Issues up into the first
      -- group without widening it would have put it at the top of the second one.
      let (primary, secondary) = splitAt 3 $ menu sess.lang project.id
      mapM_ (uncurry3 renderNavItem) primary
      div_ [class_ "border-t border-strokeWeak/50 my-1.5 mx-2"] ""
      mapM_ (uncurry3 renderNavItem) secondary
      whenJust activationProgressM $ onboardingChecklist_ project
      div_ [class_ "border-t border-strokeWeak my-2"] ""
      renderNavItem "Settings" ("/p/" <> pidTxt <> "/settings") "gear"
      a_
        ( [ href_ "https://monoscope.tech/docs/"
          , target_ "blank"
          , Aria.label_ "Docs"
          , class_ "main-nav-link relative group-has-[#sidenav-toggle:checked]/pg:px-4 gap-3 py-2 flex no-wrap shrink-0 justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start items-center rounded-lg overflow-x-hidden overflow-y-hidden hover:bg-fillWeak hover:text-textStrong transition-colors duration-100"
          ]
            <> tippyRight_ "Docs"
        )
        do
          faSprite_ "circle-question" "regular" "nav-icon w-4 h-4 shrink-0"
          span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:flex items-center gap-1.5 whitespace-nowrap truncate"] do
            "Docs"
            faSprite_ "arrow-up-right" "regular" "w-3 h-3 text-textWeak"
    nav_ [id_ "ai-conversation-nav", Aria.label_ "AI conversations", class_ "hidden group-has-[#nav-view-ai:checked]/nav:flex mt-1.5 min-h-0 overflow-y-auto flex-col gap-px text-textWeak"] do
      let newAIAction active icon label action = button_
            ([type_ "button", Aria.label_ label, class_ $ "w-full group-has-[#sidenav-toggle:checked]/pg:px-2.5 gap-2 py-1.5 flex items-center justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start rounded-md text-sm font-medium focus-visible:outline-2 focus-visible:outline-offset-2 active:scale-[0.96] transition-[color,background-color,scale] duration-100 cursor-pointer" <> bool " text-textStrong hover:bg-fillWeak" " text-textBrand hover:bg-fillBrand-weak" active, action] <> tippyRight_ label)
            do
              faSprite_ icon "regular" $ bool "w-3 h-3 shrink-0" "w-3.5 h-3.5 shrink-0" active
              span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block"] $ toHtml label
      div_ [class_ "flex flex-col gap-px group-has-[#sidenav-toggle:checked]/pg:px-1"] do
        newAIAction True "plus" "New chat" [__|on click set #ai-composer-input.value to '' then set #ai-composer-chat.checked to true then call #ai-composer-modal.showModal()|]
        newAIAction False "clock-rotate-left" "New routine" [__|on click set #ai-composer-input.value to '' then set #ai-composer-routine.checked to true then call #ai-composer-modal.showModal()|]
      div_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block border-t border-strokeWeak/40 my-1 mx-2"] ""
      when (length conversations > 15) $ div_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block px-1 pb-1"] do
        div_ [class_ "relative"] do
          span_ [class_ "pointer-events-none absolute inset-y-0 start-2 flex items-center text-textWeak"] $ faSprite_ "sidebar-search" "regular" "h-3 w-3"
          input_
            [ type_ "search"
            , Aria.label_ "Filter conversations"
            , placeholder_ "Filter conversations…"
            , class_ "h-7 w-full rounded-md border border-strokeWeak bg-bgBase ps-7 pe-2 text-xs placeholder:text-textWeak focus:border-strokeFocus focus:outline-none"
            , [__|on input
                show .ai-thread-item in #ai-conversation-nav when its dataset.threadTitle.toLowerCase() contains my value.toLowerCase()
                then set visibleCount to #ai-conversation-nav.querySelectorAll('.ai-thread-item:not([style*="display: none"])').length
                if visibleCount == 0 remove .hidden from #ai-thread-empty
                else add .hidden to #ai-thread-empty end|]
            ]
        p_ [id_ "ai-thread-empty", class_ "hidden px-2 py-2 text-xs text-textWeak"] "No matching conversations"
      let (routines, chats) = partition (isJust . (.routineInterval)) conversations
          conversationHref c = "/p/" <> project.id.toText <> "/ai/" <> c.conversationId.toText
          scheduleLabel c
            | isJust c.routineRunningSince = "Running now"
            | not c.routineActive = "Paused"
            | otherwise = maybe "Waiting to be scheduled" (\t -> "Next " <> toText (formatTime defaultTimeLocale "%-d %b, %H:%M UTC" t)) c.routineNextRunAt
          relativeTime t =
            let seconds = max 0 $ floor (diffUTCTime now t) :: Int
             in if
                  | seconds < 60 -> "now"
                  | seconds < 3600 -> show (seconds `div` 60) <> "m"
                  | seconds < 86400 -> show (seconds `div` 3600) <> "h"
                  | seconds < 604800 -> show (seconds `div` 86400) <> "d"
                  | otherwise -> toText $ formatTime defaultTimeLocale "%-d %b" t
          sectionLink :: Text -> Text -> Int -> Html ()
          sectionLink href label count = a_ [href_ href, class_ "hidden group-has-[#sidenav-toggle:checked]/pg:flex items-center justify-between rounded-md px-2 py-1.5 text-2xs font-medium text-textWeak hover:bg-fillWeak hover:text-textStrong focus-visible:outline-2 focus-visible:outline-offset-1 transition-colors"] do
            span_ [] $ toHtml label
            span_ [class_ "text-2xs tabular-nums text-textDisabled"] $ toHtml $ show count
          timeGroupLabel label = div_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block px-2 pb-0.5 pt-1 text-2xs font-medium text-textDisabled"] $ toHtml label
          threadLink c =
            let isActive = activeConversationId == Just c.conversationId
                href = conversationHref c
                actionsId = "ai-thread-actions-" <> c.conversationId.toText
             in div_ [data_ "thread-title" c.title, class_ $ "ai-thread-item group/thread relative flex min-w-0 items-center rounded-md text-textWeak/80 hover:bg-fillWeak/70 hover:text-textStrong transition-[color,background-color] duration-100" <> bool "" " text-textStrong font-medium" isActive] do
                  a_ ([href_ href, Aria.label_ c.title, class_ "flex min-w-0 flex-1 items-center justify-center py-1 group-has-[#sidenav-toggle:checked]/pg:justify-start group-has-[#sidenav-toggle:checked]/pg:ps-2 group-has-[#sidenav-toggle:checked]/pg:pe-1 focus-visible:outline-2 focus-visible:outline-offset-2"] <> [term "aria-current" "page" | isActive] <> tippyRight_ c.title) do
                    faSprite_ "message" "regular" "group-has-[#sidenav-toggle:checked]/pg:hidden w-3.5 h-3.5 shrink-0"
                    span_ [class_ $ "hidden group-has-[#sidenav-toggle:checked]/pg:block me-2 h-1 w-1 shrink-0 rounded-full " <> bool "bg-transparent" "bg-fillBrand-strong" isActive, Aria.hidden_ "true"] ""
                    span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block min-w-0 flex-1 truncate leading-5"] $ toHtml c.title
                  span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block shrink-0 pe-2 text-2xs tabular-nums text-textDisabled group-hover/thread:opacity-0 group-focus-within/thread:opacity-0 transition-opacity duration-100", title_ $ toText $ formatTime defaultTimeLocale "%e %b %Y, %H:%M UTC" c.updatedAt] $ toHtml $ relativeTime c.updatedAt
                  button_
                    ( [ type_ "button"
                      , Aria.label_ $ "Actions for " <> c.title
                      , class_ "absolute end-0.5 hidden h-6 w-6 items-center justify-center rounded text-textWeak opacity-0 hover:bg-fillHover hover:text-textStrong group-has-[#sidenav-toggle:checked]/pg:flex group-hover/thread:opacity-100 group-focus-within/thread:opacity-100 focus-visible:opacity-100 focus-visible:outline-2 focus-visible:outline-offset-1 transition-[color,background-color,opacity] duration-100"
                      ]
                        <> popoverTrigger_ actionsId
                    )
                    $ faSprite_ "ellipsis" "regular" "h-3.5 w-3.5"
                  ul_ ([class_ "dropdown dropdown-end menu menu-sm z-50 w-44 rounded-lg border border-strokeWeak bg-bgRaised p-1 shadow-lg", [__|on click call me.hidePopover()|]] <> popoverPanel_ actionsId) do
                    li_ [] $ a_ [href_ href] "Open chat"
                    li_ []
                      $ button_
                        [ type_ "button"
                        , data_ "thread-title" c.title
                        , data_ "thread-url" href
                        , [__|on click
                            set #ai-thread-rename-input.value to my.dataset.threadTitle
                            set the @hx-post of #ai-thread-rename-form to `${my.dataset.threadUrl}/title`
                            call #ai-thread-rename-modal.showModal()|]
                        ]
                        "Rename"
                    when (c.conversationType == Issues.CTWeb && isNothing c.routineInterval) $ li_ [] $ form_ [class_ "w-full", hxPost_ $ href <> "/routine", hxSwap_ "none", hxConfirm_ "Turn this chat into a daily routine?"] do
                      input_ [type_ "hidden", name_ "intervalMinutes", value_ "1440"]
                      button_ [type_ "submit", class_ "w-full text-start"] "Make daily routine"
                    li_ [] $ button_ [type_ "button", class_ "text-textError", hxDelete_ href, hxSwap_ "none", hxConfirm_ "Delete this chat and its message history? This cannot be undone."] "Delete chat"
          routineCard c = do
            let isActive = activeConversationId == Just c.conversationId
                cadenceLabel = foldMap Issues.routineCadence c.routineInterval
                statusLabel = scheduleLabel c
                stateLabel
                  | isJust c.routineRunningSince = "Running"
                  | c.routineActive = "Active"
                  | otherwise = "Paused"
                nextRunLabel =
                  c.routineNextRunAt <&> \t ->
                    if utctDay t == utctDay now
                      then toText $ formatTime defaultTimeLocale "%H:%M" t
                      else toText $ formatTime defaultTimeLocale "%-d %b" t
                href = conversationHref c
            a_
              ( [ href_ href
                , Aria.label_ $ c.title <> ", " <> cadenceLabel
                , data_ "thread-title" c.title
                , class_ "ai-thread-item group-has-[#sidenav-toggle:checked]/pg:hidden flex items-center justify-center rounded-md py-1.5 text-textBrand hover:bg-fillBrand-weak focus-visible:outline-2 focus-visible:outline-offset-2 transition-colors duration-100"
                ]
                  <> tippyRight_ (c.title <> " — " <> cadenceLabel)
              )
              $ faSprite_ "clock" "regular" "h-3.5 w-3.5"
            a_
              ( [ href_ href
                , data_ "thread-title" c.title
                , Aria.label_ $ c.title <> ", " <> stateLabel <> ", " <> cadenceLabel
                , class_ $ "ai-thread-item hidden min-w-0 items-center rounded-md px-2 py-1.5 hover:bg-fillWeak focus-visible:outline-2 focus-visible:outline-offset-2 group-has-[#sidenav-toggle:checked]/pg:flex " <> bool "" "bg-fillWeak" isActive
                ]
                  <> [term "aria-current" "page" | isActive]
              )
              do
                span_ [class_ "min-w-0 flex-1"] do
                  span_ [class_ "block truncate font-medium text-textStrong"] $ toHtml c.title
                  span_ [class_ "flex min-w-0 items-center gap-1 text-2xs font-normal leading-4 text-textWeak"] do
                    span_
                      [ class_
                          $ "h-1.5 w-1.5 shrink-0 rounded-full "
                          <> if
                            | isJust c.routineRunningSince -> "bg-fillBrand-strong"
                            | c.routineActive -> "bg-fillSuccess-strong"
                            | otherwise -> "bg-fillWeak border border-strokeWeak"
                      , Aria.hidden_ "true"
                      ]
                      ""
                    span_ [] $ toHtml stateLabel
                    span_ [Aria.hidden_ "true"] "·"
                    span_ [class_ "truncate"] $ toHtml cadenceLabel
                    whenJust nextRunLabel $ span_ [class_ "ms-auto shrink-0 tabular-nums text-textDisabled", title_ statusLabel] . toHtml
      sectionLink ("/p/" <> project.id.toText <> "/ai/routines") "Routines" (length routines)
      unless (null routines) $ div_ [class_ "flex flex-col gap-px px-0.5 group-has-[#sidenav-toggle:not(:checked)]/pg:px-0"] $ mapM_ routineCard routines
      sectionLink ("/p/" <> project.id.toText <> "/ai") "Conversations" (length chats)
      unless (null chats) do
        let ageInDays c = diffDays (utctDay now) (utctDay c.updatedAt)
            (today, beforeToday) = partition ((<= 0) . ageInDays) chats
            (thisWeek, older) = partition ((<= 7) . ageInDays) beforeToday
            renderTimeGroup label threads = unless (null threads) do
              timeGroupLabel label
              mapM_ threadLink threads
        renderTimeGroup "Today" today
        renderTimeGroup "This week" thisWeek
        renderTimeGroup "Older" older

  div_ [class_ "relative z-[1] py-2.5 px-2 group-has-[#sidenav-toggle:checked]/pg:px-3 border-t border-strokeWeak flex flex-col gap-1"] do
    let currUser = sess.persistentSession.user.getUser
        userIdentifier = bool (CI.original currUser.email) (currUser.firstName <> " " <> currUser.lastName) (currUser.firstName /= "" || currUser.lastName /= "")
    label_ [class_ "flex group-has-[#sidenav-toggle:not(:checked)]/pg:hidden cursor-pointer gap-2 items-center px-2 py-2 rounded-lg hover:bg-fillWeak transition-colors duration-100", Aria.label_ "Toggle dark mode"] do
      faSprite_ "sun-bright" "regular" "h-4 w-4 shrink-0 text-textWeak"
      input_ [type_ "checkbox", class_ "toggle toggle-sm theme-controller", id_ "dark-mode-toggle", Aria.label_ "Use dark appearance", onclick_ "toggleDarkMode()"]
      faSprite_ "moon-stars" "regular" "h-4 w-4 shrink-0 text-textWeak"
    button_ ([type_ "button", class_ "group-has-[#sidenav-toggle:checked]/pg:hidden flex justify-center items-center py-2 rounded-lg hover:bg-fillWeak cursor-pointer transition-colors duration-100", Aria.label_ "Toggle dark mode", onclick_ "toggleDarkMode()"] <> tippyRight_ "Toggle dark mode") do
      span_ [class_ "dark:hidden"] $ faSprite_ "sun-bright" "regular" "h-4 w-4 text-textWeak"
      span_ [class_ "hidden dark:inline-flex"] $ faSprite_ "moon-stars" "regular" "h-4 w-4 text-textWeak"
    -- User avatar popover
    div_ [class_ "block group/user"] do
      button_
        ( [ type_ "button"
          , class_ "flex items-center gap-2 py-2 px-1 rounded-lg hover:bg-fillWeak cursor-pointer w-full justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start"
          , Aria.haspopup_ "true"
          , Aria.label_ $ userIdentifier <> ", user menu"
          ]
            <> popoverTrigger_ "user-menu-pop"
        )
        do
          img_ ([class_ "w-8 h-8 rounded-full bg-fillPress shrink-0 outline outline-1 outline-black/10 dark:outline-white/10", src_ $ "/api/avatar/" <> currUser.id.toText, alt_ userIdentifier] <> tippyRight_ userIdentifier)
          span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:flex items-center gap-1 overflow-hidden flex-1"] do
            span_ [class_ "truncate text-sm"] $ toHtml userIdentifier
            faSprite_ "chevron-down" "regular" "w-3 h-3 text-textWeak shrink-0 ml-auto transition-transform duration-150 rotate-180 group-focus-within/user:rotate-0"
      ul_ ([class_ "dropdown dropdown-top menu menu-md bg-bgRaised rounded-box shadow-lg border border-strokeWeak w-56 mb-2", role_ "menu"] <> popoverPanel_ "user-menu-pop") do
        div_ [class_ "px-3 py-2 text-sm"] do
          div_ [class_ "font-medium text-textStrong truncate"] $ toHtml userIdentifier
          div_ [class_ "text-textWeak text-xs truncate"] $ toHtml $ CI.original currUser.email
        div_ [class_ "divider my-0"] ""
        li_ [class_ "menu-title px-3 pt-2"] $ toHtml $ I18n.t sess.lang "nav.language"
        -- onclick rewrites the redirect to the current path so the user stays put instead of bouncing to /.
        let langLink code lang labelKey =
              li_ [] $ a_
                [ href_ $ "/set_language/" <> code <> "?redirect_to=/"
                , class_ "flex items-center justify-between"
                , onclick_ $ "this.href='/set_language/" <> code <> "?redirect_to='+encodeURIComponent(location.pathname+location.search);return true;"
                ]
                do
                  toHtml $ I18n.t sess.lang labelKey
                  when (sess.lang == lang) $ faSprite_ "check" "regular" "w-3 h-3"
        langLink "en" I18n.En "nav.language.english"
        langLink "es" I18n.Es "nav.language.spanish"
        div_ [class_ "divider my-0"] ""
        li_ [] $ a_ [href_ "/logout", class_ "flex items-center gap-2 text-textError", [__| on click js posthog.reset(); end |]] do
          faSprite_ "arrow-right-from-bracket" "regular" "w-4 h-4"
          toHtml $ I18n.t sess.lang "nav.logout"
  where
    pageTitle = fromMaybe bcfg.pageTitle bcfg.prePageTitle
    menuItem = if bcfg.isSettingsPage then Just "Settings" else bcfg.menuItem
    activationProgressM = bcfg.activationProgressM
    conversations = bcfg.conversations
    activeConversationId = bcfg.activeConversationId
    now = bcfg.shellNow


navbar :: BWConfig -> [(Text, Text, Text)] -> Html ()
navbar bcfg menuL =
  nav_ [id_ "main-navbar", class_ "grid w-full grid-cols-[minmax(0,1fr)_auto] items-center border-b border-strokeWeak px-4 py-1 max-md:px-2 max-md:py-1"] do
    -- The bar has two intentional anchors: orientation and sibling navigation lead;
    -- scope and time trail. The flexible first column owns all compression so the
    -- operational context never wanders into the middle of a wide viewport.
    div_ [class_ "flex min-w-0 items-center gap-0 overflow-hidden"] do
      div_ [class_ "flex min-w-0 shrink items-center gap-1 overflow-hidden text-textStrong"] do
        when (isJust bcfg.currProject) do
          label_ [term "for" "mobile-nav-toggle", role_ "button", tabindex_ "0", class_ "md:!hidden max-md:flex group-has-[#mobile-nav-toggle:checked]/pg:max-md:!hidden cursor-pointer text-strokeStrong p-2 -m-2 items-center justify-center focus-visible:outline-2 focus-visible:outline-offset-2", Aria.label_ "Open menu", Components.keyboardActivateAttr_] $ faSprite_ "side-chevron-left-in-box" "regular" "h-5 w-5 rotate-180 pointer-events-none"
          div_ [class_ "md:!hidden max-md:block group-has-[#mobile-nav-toggle:checked]/pg:max-md:!hidden w-px h-5 bg-strokeWeak ms-2"] ""
        whenJust bcfg.prePageTitle \pt -> whenJust (find ((== pt) . fst3) menuL) \(_, url, icon) -> do
          a_ ([class_ "max-md:hidden p-1 hover:bg-fillWeak inline-flex items-center justify-center gap-1 rounded-md text-sm", href_ url] <> navTabAttrs) do
            faSprite_ icon "regular" "w-4 h-4 text-strokeStrong"
            toHtml pt
          faSprite_ "chevron-right" "regular" "w-3 h-3 max-md:hidden"
        h1_ [class_ "flex min-w-0 items-center text-textStrong"] do
          let targetPageM = Components.getTargetPage bcfg.pageTitle <* bcfg.pageTitleSuffix
          case targetPageM of
            Just targetPage -> whenJust bcfg.currProject \p -> a_ ([class_ "max-w-48 truncate rounded-md p-1 py-2 text-xl font-semibold leading-none text-textStrong hover:bg-fillWeak max-md:max-w-24 max-md:text-sm", href_ $ "/p/" <> p.id.toText <> targetPage, id_ "pageTitleText"] <> navTabAttrs) $ toHtml bcfg.pageTitle
            Nothing -> case bcfg.pageTitleModalId of
              Just modalId -> label_ [class_ "max-w-48 truncate rounded-md p-1 py-2 text-xl font-semibold leading-none text-textStrong cursor-pointer hover:bg-fillWeak focus-visible:outline-2 focus-visible:outline-offset-2 max-md:max-w-24 max-md:text-sm", Lucid.for_ modalId, id_ "pageTitleText", role_ "button", tabindex_ "0", Aria.label_ $ "Rename " <> bcfg.pageTitle, Components.keyboardActivateAttr_] $ toHtml bcfg.pageTitle
              Nothing -> span_ [class_ "max-w-48 truncate p-1 py-2 text-xl font-semibold leading-none text-textStrong max-md:max-w-24 max-md:text-sm", id_ "pageTitleText"] $ toHtml bcfg.pageTitle
          -- Show tab/suffix in breadcrumbs if present (with ID for htmx out-of-band updates)
          span_ [id_ "pageTitleSuffix", class_ "max-md:hidden flex items-center gap-1"] $ whenJust bcfg.pageTitleSuffix \suffix -> do
            faSprite_ "chevron-right" "regular" "w-3 h-3"
            case bcfg.pageTitleSuffixModalId of
              Just modalId -> label_ [class_ "font-medium text-xl p-1 leading-none text-textWeak cursor-pointer hover:bg-fillWeak rounded-md focus-visible:outline-2 focus-visible:outline-offset-2", Lucid.for_ modalId, id_ "pageTitleSuffixText", role_ "button", tabindex_ "0", Aria.label_ $ "Rename " <> suffix, Components.keyboardActivateAttr_] $ toHtml suffix
              Nothing -> span_ [class_ "font-medium text-xl p-1 leading-none text-textWeak", id_ "pageTitleSuffixText"] $ toHtml suffix
        whenJust bcfg.docsLink \link -> a_ ([class_ "-ms-0.5 max-md:hidden inline-flex h-8 w-8 shrink-0 items-center justify-center rounded-md text-iconBrand hover:bg-fillWeak focus-visible:outline-2 focus-visible:outline-offset-2", href_ link, term "hx-preload" "false", target_ "_blank", rel_ "noopener", Aria.label_ "Open documentation for this page"] <> tippyRight_ "Open documentation for this page") $ faSprite_ "circle-question" "regular" "w-4 h-4"
      when (isJust bcfg.navTabs) headerDivider_
      whenJust bcfg.navTabs $ div_ [class_ "min-w-0 max-w-full overflow-x-auto [scrollbar-width:none] [&::-webkit-scrollbar]:hidden data-[overflowing=true]:[mask-image:linear-gradient(to_right,transparent,black_1rem,black_calc(100%-1rem),transparent)]", data_ "nav-tab-strip" ""]
    div_ [class_ $ "ms-auto flex shrink-0 items-center justify-end gap-0 text-sm" <> bool " max-md:hidden" "" (isJust bcfg.pageActions)] do
      unless (V.null bcfg.envOptions && V.null bcfg.serviceOptions) $ div_ [class_ "flex items-center gap-2", data_ "header-scope" "", role_ "group", Aria.label_ "Global telemetry scope"] do
        envPicker_ (bcfg.sessM >>= (.environment)) bcfg.envOptions
        servicePicker_ (bcfg.sessM >>= (.service)) bcfg.serviceOptions
      when
        (not (V.null bcfg.envOptions && V.null bcfg.serviceOptions) && isJust bcfg.pageActions)
        headerDivider_
      whenJust bcfg.pageActions $ div_ [class_ "flex items-center", data_ "header-actions" ""]
    script_
      [text|
      (() => {
        const navbar = document.currentScript?.closest('#main-navbar');
        const strip = navbar?.querySelector('[data-nav-tab-strip]');
        if (!strip) return;
        const sync = () => {
          strip.dataset.overflowing = String(strip.scrollWidth > strip.clientWidth + 1);
        };
        requestAnimationFrame(() => {
          strip.querySelector('[aria-current="page"]')?.scrollIntoView({block: 'nearest', inline: 'nearest'});
          sync();
        });
        if (window.ResizeObserver) new ResizeObserver(sync).observe(strip);
      })();
    |]
  where
    -- A single divider treatment keeps the title/tabs and scope/actions boundaries
    -- optically consistent. Spacing belongs to the divider so either neighbour can
    -- disappear without leaving a stray gap behind.
    headerDivider_ = div_ [class_ "mx-4 hidden h-4 w-px shrink-0 bg-strokeWeak opacity-70 md:block", Aria.hidden_ "true"] ""


-- | The app-wide environment selector, in the shape Datadog puts in its top bar: pick prod
-- or staging once and every telemetry surface stays scoped to it until you change it.
--
-- The selection lives in the URL so navigation and shared links preserve the active scope.
--
-- Hidden entirely when the project has never reported an environment: a picker whose only
-- option is "All" is furniture, not a control.
envPicker_ :: Maybe Text -> V.Vector Text -> Html ()
envPicker_ selected options =
  unless (V.null options) $ div_ [class_ "relative", data_ "scope-picker" ""] do
    let current = fromMaybe "All environments" selected
    button_
      ( [ class_ "group inline-flex h-8 items-center gap-1.5 rounded-lg border border-strokeWeak bg-bgRaised px-2.5 shadow-xs hover:border-strokeStrong hover:bg-fillWeak focus-visible:border-strokeFocus cursor-pointer aria-busy:opacity-60"
        , type_ "button"
        , id_ "env-picker-trigger"
        , Aria.label_ $ "Deployment environment: " <> current
        , term "data-tippy-content" "Global scope: apply this deployment environment to every telemetry page"
        ]
          <> popoverTrigger_ "env-picker"
      )
      do
        span_ [class_ "group-aria-busy:hidden"] $ faSprite_ "layer-group" "regular" "w-3.5 h-3.5 text-iconNeutral"
        span_ [class_ "hidden group-aria-busy:block"] $ faSprite_ "spinner" "regular" "w-3.5 h-3.5 animate-spin text-iconBrand"
        span_ [class_ "max-w-32 truncate font-normal text-textWeak max-md:hidden", id_ "env-picker-label"] $ toHtml $ fromMaybe "All envs" selected
        faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral"
    ul_ (popoverPanel_ "env-picker" <> [class_ "dropdown menu flex flex-col bg-bgBase border border-strokeWeak w-56 p-1 text-sm rounded-lg shadow"])
      -- Nothing is "all environments" and is always offered: an environment that has gone
      -- quiet must not be able to strand a reader in a view with no data and no way out.
      $ forM_ (Nothing : (Just <$> V.toList options))
      $ scopeOption_ "environment" "All environments" selected


servicePicker_ :: Maybe Text -> V.Vector Text -> Html ()
servicePicker_ selected options =
  unless (V.null options) $ div_ [class_ "relative", data_ "scope-picker" ""] do
    let current = fromMaybe "All services" selected
    button_ ([class_ "group inline-flex h-8 items-center gap-1.5 rounded-lg border border-strokeWeak bg-bgRaised px-2.5 shadow-xs hover:border-strokeStrong hover:bg-fillWeak focus-visible:border-strokeFocus cursor-pointer aria-busy:opacity-60", type_ "button", id_ "service-picker-trigger", Aria.label_ $ "Global service scope: " <> current, term "data-tippy-content" "Global scope: apply this service to every telemetry page"] <> popoverTrigger_ "service-picker") do
      span_ [class_ "group-aria-busy:hidden"] $ faSprite_ "cube" "regular" "w-3.5 h-3.5 text-iconNeutral"
      span_ [class_ "hidden group-aria-busy:block"] $ faSprite_ "spinner" "regular" "w-3.5 h-3.5 animate-spin text-iconBrand"
      span_ [class_ "max-w-32 truncate font-normal text-textWeak max-md:hidden", id_ "service-picker-label"] $ toHtml current
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral"
    ul_ (popoverPanel_ "service-picker" <> [class_ "dropdown menu flex flex-col bg-bgBase border border-strokeWeak w-56 p-1 text-sm rounded-lg shadow"])
      $ forM_ (Nothing : (Just <$> V.toList options))
      $ scopeOption_ "service_scope" "All services" selected


-- The URL is the scope store. The dataset carries arbitrary service/environment names
-- without embedding them in executable text; the one-line handler preserves every other
-- query parameter through the shared URL helper.
scopeOption_ :: Text -> Text -> Maybe Text -> Maybe Text -> Html ()
scopeOption_ scope fallback selected option =
  li_ []
    $ button_
      [ class_ $ "w-full text-left cursor-pointer rounded-md px-2 py-1 hover:bg-fillWeak " <> bool "" "font-semibold text-textBrand" (option == selected)
      , type_ "button"
      , term "aria-pressed" (bool "false" "true" (option == selected))
      , data_ "scope-key" scope
      , data_ "scope-value" (fromMaybe "" option)
      , [__|on click call window.setQueryParamAndReload(my.dataset.scopeKey, my.dataset.scopeValue)|]
      ]
    $ toHtml (fromMaybe fallback option)


globalTemplates_ :: Html ()
globalTemplates_ = do
  template_ [id_ "log-item-context-menu-tmpl"] do
    ul_ [class_ "log-item-cloned-menu dropdown-content z-50 menu p-2 shadow-sm bg-bgRaised rounded-box w-96 max-w-[92vw] absolute", tabindex_ "0"] do
      fieldContextMenuItems_ DynamicField fieldMenuActions
  let toastTmpl :: Text -> Bool -> Text -> Html () -> Html ()
      toastTmpl tmplId isError icon fallback =
        template_ [id_ tmplId]
          $ div_ ([class_ "toast-animate flex items-center gap-3 p-2 ps-4 rounded-box bg-bgRaised text-textStrong shadow-toast max-md:w-full md:w-96"] <> if isError then [role_ "alert"] else [[__|init wait 8s then remove me|]]) do
            span_ [Aria.hidden_ "true", class_ $ "grid size-8 shrink-0 place-items-center rounded-full " <> bool "bg-fillSuccess-weak text-textSuccess" "bg-fillError-weak text-textError" isError] $ faSprite_ icon "regular" "size-4"
            span_ [class_ "title min-w-0 flex-1 break-words text-pretty text-sm leading-normal"] fallback
            button_ [type_ "button", Aria.label_ "Dismiss notification", class_ "grid size-10 max-md:size-11 shrink-0 place-items-center rounded-lg text-iconNeutral hover:bg-fillHover hover:text-textStrong focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-textBrand transition-colors", [__|on click remove closest .toast-animate|]] $ faSprite_ "xmark" "regular" "size-3"
  toastTmpl "successToastTmpl" False "circle-check" "Something succeeded"
  toastTmpl "errorToastTmpl" True "circle-exclamation" "Something failed"
  section_ [class_ "fixed bottom-0 right-0 z-50 p-4 max-md:left-0 space-y-2 pointer-events-none [&>*]:pointer-events-auto", id_ "toastsParent", Aria.live_ "polite", term "aria-relevant" "additions"] ""
  script_
    [type_ "text/javascript"]
    [text|
    document.addEventListener('DOMContentLoaded', function(){
      for (const [eventName, type] of [['triggerToast', null], ['successToast', 'success'], ['errorToast', 'error']]) {
        document.body.addEventListener(eventName, function(e){
          const toasts = type ? e.detail.value.map(message => [type, message]) : e.detail.value;
          toasts.forEach(function(toastEvent){
            const template = document.getElementById(toastEvent[0].toLowerCase()+'ToastTmpl');
            const clone = document.importNode(template.content, true);
            clone.querySelector('.title').textContent = toastEvent[1];
            document.getElementById("toastsParent").appendChild(clone);
            _hyperscript.processNode(document.querySelector("#toastsParent"));
          })
        })
      }
    })
  |]


loginBanner :: Html ()
loginBanner = do
  div_ [class_ "flex items-center justify-between border-b border-strokeWeak bg-fillWeak max-md:px-2 px-4 py-1.5 gap-2 text-sm max-md:text-xs max-md:flex-wrap"] do
    div_ [class_ "flex items-center gap-2"] do
      faSprite_ "flask" "regular" "h-4 w-4 text-iconBrand"
      span_ [class_ "font-medium text-textStrong"] "Demo Project"
      span_ [class_ "hidden sm:inline text-textWeak"] "· Explore Monoscope's features"
    div_ [class_ "flex items-center gap-2 max-md:gap-1.5 max-md:ml-auto"] do
      a_ [class_ "text-textBrand hover:underline underline-offset-2 max-md:hidden", href_ "https://monoscope.tech/docs/onboarding/"] "Docs"
      a_ [class_ "py-1 px-2.5 rounded-lg bg-fillWeak hover:bg-fillHover text-textStrong border border-strokeWeak text-xs font-medium max-md:hidden", href_ "https://calendar.app.google/1a4HG5GZYv1sjjZG6"] "Book Demo"
      a_ [class_ "py-1 px-2.5 rounded-lg bg-fillBrand-strong hover:opacity-90 text-textInverse-strong text-xs font-medium", href_ "/login"] "Start Free Trial"


settingsWrapper :: Projects.ProjectId -> Text -> Html () -> Html ()
settingsWrapper pid current pageHtml =
  section_ [class_ "flex max-md:flex-col h-full w-full"] do
    nav_ [id_ "settings-nav", class_ "md:w-52 shrink-0 md:h-full max-md:px-3 max-md:py-2.5 p-4 md:pt-8 max-md:border-b max-md:border-b-strokeWeak md:border-r md:border-r-strokeWeak max-md:overflow-x-auto max-md:scrollbar-hide", term "hx-preload:inherited" "mouseover"] do
      h1_ [class_ "text-lg pl-3 font-semibold text-textStrong max-md:hidden"] "Settings"
      ul_ [class_ "flex max-md:flex-row max-md:flex-nowrap md:flex-col md:mt-4 gap-0.5 w-full [&_.settings-nav-link]:hover:bg-fillWeak [&_.settings-nav-link]:text-textWeak [&_.settings-nav-link.active]:bg-fillBrand-weak [&_.settings-nav-link.active]:text-textBrand [&_.settings-nav-link.active]:hover:bg-fillBrand-weak"] do
        li_ [class_ "md:hidden shrink-0"]
          $ label_ [term "for" "mobile-nav-toggle", role_ "button", tabindex_ "0", class_ "flex items-center px-2.5 py-2 rounded-lg cursor-pointer text-strokeStrong hover:bg-fillWeak focus-visible:outline-2 focus-visible:outline-offset-2", Aria.label_ "Open menu", Components.keyboardActivateAttr_]
          $ faSprite_ "side-chevron-left-in-box" "regular" "shrink-0 h-4.5 w-4.5 rotate-180"
        mapM_ (renderNavBottomItem current) $ navBottomList pid.toText
    section_ [id_ "settings-content", class_ "relative w-full h-full overflow-y-auto", Aria.label_ current] do
      div_ [id_ settingsLoadingId, class_ "htmx-indicator absolute inset-0 z-10 bg-bgBase/60 flex items-center justify-center"] do
        loadingIndicatorWith_ LdMD LdSpinner "text-textBrand"
      pageHtml


-- | Settings nav entries as @(title, url, icon)@ — the same shape as 'menu', so a
-- reader never has to check which of the three strings is which.
navBottomList :: Text -> [(Text, Text, Text)]
navBottomList pidTxt =
  [ ("Project", "/p/" <> pidTxt <> "/settings", "gear")
  , ("API Keys", "/p/" <> pidTxt <> "/apis", "key")
  , ("Team", "/p/" <> pidTxt <> "/manage_members", "users")
  , ("Integrations", "/p/" <> pidTxt <> "/settings/integrations", "arrows-turn-right")
  , ("Prometheus", "/p/" <> pidTxt <> "/settings/prometheus", "objects-column")
  , ("Billing", "/p/" <> pidTxt <> "/manage_billing", "dollar")
  ]


navFlyoutItems :: Text -> Text -> [(Text, Text)]
navFlyoutItems pidTxt = \case
  "Explorer" -> [(label, p path) | (label, path) <- explorerTabs]
  "Infrastructure" -> [(label, p path) | (label, path) <- infrastructureTabs]
  "Real User Monitoring" -> [("Overview", p "/rum"), ("Sessions", p "/rum?tab=sessions"), ("Performance", p "/rum?tab=performance")]
  -- API Docs carries no host: it is the whole project's learned spec. The
  -- per-host and per-endpoint scopes are reached from the catalog rows.
  "API Catalog" -> [("Incoming", p "/api_catalog?request_type=Incoming"), ("Outgoing", p "/api_catalog?request_type=Outgoing"), ("API Docs", p "/api_catalog/docs?request_type=Incoming")]
  "Issues" -> [("Inbox", p "/issues?filter=Inbox"), ("Acknowledged", p "/issues?filter=Acknowledged"), ("Archived", p "/issues?filter=Archived")]
  "Monitors" -> [("Active", p "/monitors?filter=Active"), ("Inactive", p "/monitors?filter=Inactive"), ("New Monitor", p "/log_explorer#create-alert-toggle")]
  "Settings" -> [(t, l) | (t, l, _) <- navBottomList pidTxt]
  _ -> []
  where
    p path = "/p/" <> pidTxt <> path


settingsContentTarget :: Text
settingsContentTarget = "#settings-content"


settingsLoadingId :: Text
settingsLoadingId = "settings-loading"


renderNavBottomItem :: Text -> (Text, Text, Text) -> Html ()
renderNavBottomItem curr (linkText, link, iconName) =
  li_ [] do
    a_
      ( [ class_ $ "settings-nav-link flex gap-2 md:gap-3 items-center px-2.5 md:px-3 py-2 rounded-lg whitespace-nowrap" <> bool "" " active" (curr == linkText)
        , href_ link
        , hxGet_ link
        , hxTarget_ settingsContentTarget
        , hxSelect_ settingsContentTarget
        , term "hx-select-oob" "#settings-nav:outerMorph"
        , hxSwap_ "outerMorph"
        , hxPushUrl_ "true"
        , hxIndicator_ ("#" <> settingsLoadingId)
        , [__|on click set my.preloadState to 'DONE'|]
        ]
          <> tippyRight_ linkText
      )
      do
        faSprite_ iconName "regular" "shrink-0 h-4 w-4"
        span_ [class_ "text-sm font-medium"] (toHtml linkText)


externalHeadScripts_ :: EnvConfig -> Html ()
externalHeadScripts_ config = do
  -- Google Ads
  whenJust config.googleAdsConversionId $ \conversionId -> do
    script_ [async_ "true", src_ $ "https://www.googletagmanager.com/gtag/js?id=" <> conversionId] ("" :: Text)
    script_
      [fmt|
            window.dataLayer = window.dataLayer || [];
            function gtag(){{dataLayer.push(arguments);}}
            gtag('js', new Date());
            gtag('config', '{conversionId}'); |]

  -- Facebook Pixel Code
  when (isJust config.facebookPixelId1 || isJust config.facebookPixelId2) $ do
    let fbInit pixelId = [fmt|fbq('init', '{pixelId}'); fbq('track', 'PageView');|]
        pixelInitScript = foldMap fbInit $ catMaybes [config.facebookPixelId1, config.facebookPixelId2]
    script_
      [fmt|
          setTimeout(function(){{
      !function(f,b,e,v,n,t,s)
    {{if(f.fbq)return;n=f.fbq=function(){{n.callMethod?
    n.callMethod.apply(n,arguments):n.queue.push(arguments)}};
    if(!f._fbq)f._fbq=n;n.push=n;n.loaded=!0;n.version='2.0';
    n.queue=[];t=b.createElement(e);t.async=!0;
    t.src=v;s=b.getElementsByTagName(e)[0];
    s.parentNode.insertBefore(t,s)}}(window,document,'script',
    'https://connect.facebook.net/en_US/fbevents.js');
      {pixelInitScript}
      }},3000);
      |]
    whenJust config.facebookPixelId2 $ \pixelId ->
      noscript_ $ img_ [height_ "1", width_ "1", src_ $ "https://www.facebook.com/tr?id=" <> pixelId <> "&ev=PageView&noscript=1"]
  -- End Facebook Pixel Code

  -- Google Tag Manager
  whenJust config.googleTagManagerId $ \gtmId -> do
    script_
      [fmt|
    (function(w,d,s,l,i){{w[l]=w[l]||[];w[l].push({{'gtm.start':
    new Date().getTime(),event:'gtm.js'}});var f=d.getElementsByTagName(s)[0],
    j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
    'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
    }})(window,document,'script','dataLayer','{gtmId}');
      |]
    noscript_ $ iframe_ [height_ "0", width_ "0", style_ "display:none;visibility:hidden", src_ $ "https://www.googletagmanager.com/ns.html?id=" <> gtmId] ""
  -- End Google Tag Manager

  -- LinkedIn pixel
  whenJust config.linkedInPartnerId $ \partnerId -> do
    script_
      [fmt|
    _linkedin_partner_id = "{partnerId}"; window._linkedin_data_partner_ids = window._linkedin_data_partner_ids || [];
    window._linkedin_data_partner_ids.push(_linkedin_partner_id);
      |]
    script_
      [raw|
    setTimeout(function(){
    (function(l) { if (!l){window.lintrk = function(a,b){window.lintrk.q.push([a,b])}; window.lintrk.q=[]} var s = document.getElementsByTagName("script")[0]; var b = document.createElement("script"); b.type = "text/javascript";b.async = true; b.src = "https://snap.licdn.com/li.lms-analytics/insight.min.js"; s.parentNode.insertBefore(b, s);})(window.lintrk);
                  },3000);
      |]
    noscript_ $ img_ [height_ "0", width_ "0", style_ "display:none;visibility:hidden", src_ $ "https://px.ads.linkedin.com/collect/?pid=" <> partnerId <> "&fmt=gif"]
  -- End LinkedIn

  -- PostHog
  whenJust config.postHogApiKey $ \apiKey -> do
    let apiHost = fromMaybe "https://eu.i.posthog.com" config.postHogApiHost
    script_
      [fmt|
(function() {{
    !function(t,e){{var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){{function g(t,e){{var o=e.split(".");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){{t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}}}(p=t.createElement("script")).type="text/javascript",p.crossOrigin="anonymous",p.async=!0,p.src=s.api_host.replace(".i.posthog.com","-assets.i.posthog.com")+"/static/array.js",(r=t.getElementsByTagName("script")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a="posthog",u.people=u.people||[],u.toString=function(t){{var e="posthog";return"posthog"!==a&&(e+="."+a),t||(e+=" (stub)"),e}},u.people.toString=function(){{return u.toString(1)+".people (stub)"}},o="init Ce Ls Ns Te As js capture Xe calculateEventProperties qs register register_once register_for_session unregister unregister_for_session Gs getFeatureFlag getFeatureFlagPayload isFeatureEnabled reloadFeatureFlags updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures on onFeatureFlags onSurveysLoaded onSessionId getSurveys getActiveMatchingSurveys renderSurvey canRenderSurvey canRenderSurveyAsync identify setPersonProperties group resetGroups setPersonPropertiesForFlags resetPersonPropertiesForFlags setGroupPropertiesForFlags resetGroupPropertiesForFlags reset get_distinct_id getGroups get_session_id get_session_replay_url alias set_config startSessionRecording stopSessionRecording sessionRecordingStarted captureException loadToolbar get_property getSessionProperty Hs Us createPersonProfile Ws Os Js opt_in_capturing opt_out_capturing has_opted_in_capturing has_opted_out_capturing get_explicit_consent_status is_capturing clear_opt_in_out_capturing zs debug L Bs getPageViewId captureTraceFeedback captureTraceMetric".split(" "),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])}},e.__SV=1)}}(document,window.posthog||[]);
    posthog.init('{apiKey}', {{
        api_host: '{apiHost}',
        defaults: '2025-05-24',
        person_profiles: 'identified_only'
    }})
}})();
        |]
  -- Crisp chat. Unset or empty CRISP_WEBSITE_ID disables the widget.
  whenJust (mfilter (not . T.null) config.crispWebsiteId) $ \websiteId ->
    script_
      [fmt|window.$crisp = []; window.CRISP_WEBSITE_ID = "{websiteId}"; (function () {{ d = document; s = d.createElement("script"); s.src = "https://client.crisp.chat/l.js"; s.async = 1; d.getElementsByTagName("head")[0].appendChild(s); }})();|]
